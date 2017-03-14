      SUBROUTINE CLIP_S( R, NFP, NLP, DEL, CLIP3M, TL, TR, IOS )

!***********************************************************************
!* CLIP Version 2.1 Subroutine
!*
!* Computes the maximum value that the linear interpolation of a time
!* series, typically a chest resultant acceleration filtered to 300 Hz
!* cutoff, meets or exceeds for an interval of at least 3 msec, based on
!* the linearly interpolated time series.
!*
!* Background:
!*  The original CLIP (v.1.0) program did not require the signal
!*  to remain above the CLIP level in the interior of the CLIP
!*  time span.  CLIP v.2.0 implemented a new algorithm that
!*  conforms with our interpretation of the following code
!*  of federal regulations: "The resultant acceleration at the
!*  center of gravity of the upper thorax shall not exceed 60 G's
!*  except for intervals whos cumulative duration is not more
!*  than 3 milliseconds".  CLIP v.2.0 did not allow for a time
!*  step that was not a divisor of 3 msec.  CLIP v.2.1 added
!*  support for any time step and an interpolation algorithm
!*  that found the CLIP value based on the linearly interpolated
!*  input data.
!*
!* Approach:
!*  CLIP v.2.1 is based on the observation that CLIP solutions
!*  are either vertex values with time spans of any size at least
!*  3 msec, or a constant-y line between two interpolated end
!*  points of exactly 3 msec span and with both end segments
!*  sloping upwards towards the center of the span (tail solutions
!*  of 3 msec length are also possible with only the interior
!*  segment end meeting these conditions).
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************


      ! Arguments ______________________________________________________

      INTEGER NFP ! Index of first data point

      INTEGER NLP ! Index of last data point

      REAL R(NFP:NLP) ! Resultant acceleration time series array

      REAL DEL ! Time step (delta-t) (sec) of time series data (>0)

      REAL CLIP3M ! CLIP solution value

      REAL TL ! CLIP solution start time

      REAL TR ! CLIP solution end time

      INTEGER IOS ! Status flag returned


      ! Variables ______________________________________________________

      LOGICAL UP, DN, DO_TWO, DO_AGAIN, NEW_CLIP

      INTEGER I, J, K, IDELC, IMAX, IUP, IDN, ISPAN

      REAL DELC, DELCM, DELMUL, YL, YR, ZL, ZR, Y3M, YI, TF, TFMAX,
     & SPAN

      PARAMETER ( DELC = .003 )


      ! Initialization
      CLIP3M = 0.
      TL = 0.
      TR = 0.
      IF ( DEL .LE. 0. ) THEN ! Nonpositive time step error
        IOS = 1
        RETURN
      END IF
      IF ( ( NLP - NFP ) * DEL .LT. DELC ) THEN ! Not enough data error
        IOS = 2
        RETURN
      END IF
      IOS = 0
      DELCM = DELC * ( 1. - 1.E-7 )
      DELMUL = DELC / DEL
      IDELC = INT( DELMUL * ( 1. + 1.E-7 ) )

      ! Compute max non-vertex CLIP3M
      IF ( IDELC * DEL .LT. DELCM ) THEN ! Use 2 segments on right end
        DO_TWO = .TRUE.
      ELSE
        DO_TWO = .FALSE.
      END IF
      DO I = NFP, NLP - IDELC - 1
        DO_AGAIN = DO_TWO
        YL = R(I)
        YR = R(I+1)
        IF ( YR .GT. MAX( YL, CLIP3M ) ) THEN ! Contender
          J = I + IDELC
  102     ZL = R(J)
          ZR = R(J+1)
          IF ( ( ZL .GT. MAX( ZR, YL ) ) .AND. ( ZR .LT. YR ) )
     &       THEN ! Contender
            TFMAX = J + 1 - I - DELMUL
            TF = ( ZL - YL + ( I - J + DELMUL ) * ( ZR - ZL ) ) /
     &         ( YR - YL - ZR + ZL )
            IF ( ( TF .GT. 0. ) .AND. ( TF .LT. TFMAX ) ) THEN
              Y3M = YL + TF * ( YR - YL )
              IF ( Y3M .GT. CLIP3M ) THEN ! Still contender
                NEW_CLIP = .TRUE.
                K = I + 2
                DO WHILE ( ( NEW_CLIP ) .AND. ( K .LE. J-1 ) )
                  IF ( R(K) .LE. Y3M ) NEW_CLIP = .FALSE.
                  K = K + 1
                END DO
                IF ( NEW_CLIP ) THEN
                  CLIP3M = Y3M
                  TL = ( I + TF ) * DEL
                  TR = TL + DELC
                END IF
              END IF
            END IF
          END IF
          IF ( DO_AGAIN ) THEN
            J = J + 1
            DO_AGAIN = .FALSE.
            IF ( J .LT. NLP ) GO TO 102
          END IF
        END IF
      END DO

      ! Check left tail
      J = NFP + INT( DELC / DEL )
      TF = DELMUL - J
      IF ( J .LT. NLP ) THEN
        ZL = R(J)
        ZR = R(J+1)
        IF ( ZL .GT. MAX( ZR, CLIP3M ) ) THEN ! Contender
          Y3M = ZL + TF * ( ZR - ZL )
          IF ( Y3M .GT. CLIP3M ) THEN ! Still contender
            NEW_CLIP = .TRUE.
            K = NFP
            DO WHILE ( ( NEW_CLIP ) .AND. ( K .LE. J-1 ) )
              IF ( R(K) .LE. Y3M ) NEW_CLIP = .FALSE.
              K = K + 1
            END DO
            IF ( NEW_CLIP ) THEN
              CLIP3M = Y3M
              TL = NFP * DEL
              TR = TL + DELC
            END IF
          END IF
        END IF
      END IF

      ! Check right tail
      J = NLP - INT( DELC / DEL )
      IF ( J .GT. NFP ) THEN
        YL = R(J-1)
        YR = R(J)
        IF ( YR .GT. MAX( YL, CLIP3M ) ) THEN ! Contender
          Y3M = YR + TF * ( YL - YR )
          IF ( Y3M .GT. CLIP3M ) THEN ! Still contender
            NEW_CLIP = .TRUE.
            K = NLP
            DO WHILE ( ( NEW_CLIP ) .AND. ( K .GE. J+1 ) )
              IF ( R(K) .LE. Y3M ) NEW_CLIP = .FALSE.
              K = K - 1
            END DO
            IF ( NEW_CLIP ) THEN
              CLIP3M = Y3M
              TR = NLP * DEL
              TL = TR - DELC
            END IF
          END IF
        END IF
      END IF

      ! Compute max vertex-valued CLIP3M
      IMAX = -1
      IDELC = INT( DELC / DEL ) + 1
      DO I = NFP, NLP
        YI = R(I)
        IF ( YI .GT. CLIP3M ) THEN ! Contender
          DN = .TRUE.
          UP = .TRUE.
          IDN = I
          IUP = I
          ISPAN = 0
          DO WHILE ( ( DN .OR. UP ) .AND. ( ISPAN .LT. IDELC ) )
            IF ( DN ) THEN
              IF ( ( IDN .LE. NFP ) .OR.
     &           ( R(IDN-1) .LT. YI ) ) THEN
                DN = .FALSE.
              ELSE
                IDN = IDN - 1
                ISPAN = ISPAN + 1
              END IF
            END IF
            IF ( UP ) THEN
              IF ( ( IUP .GE. NLP ) .OR.
     &           ( R(IUP+1) .LT. YI ) ) THEN
                UP = .FALSE.
              ELSE
                IUP = IUP + 1
                ISPAN = ISPAN + 1
              END IF
            END IF
          END DO
          IF ( ISPAN * DEL .GE. DELCM ) THEN ! More than 3 msec
            CLIP3M = YI
            IMAX = I
          ELSE IF ( ( ISPAN + 2 ) * DEL .GE. DELCM ) THEN
            ! Check full span
            SPAN = ISPAN
            IF ( IDN .GT. NFP ) THEN
              ! Extend downward to seg intersection
              SPAN = SPAN +
     &           ( ( R(IDN) - YI ) / ( R(IDN) - R(IDN-1) ) )
            END IF
            IF ( IUP .LT. NLP ) THEN
              ! Extend upward to seg intersection
              SPAN = SPAN +
     &           ( ( R(IUP) - YI ) / ( R(IUP) - R(IUP+1) ) )
            END IF
            IF ( SPAN * DEL .GE. DELCM ) THEN ! More than 3 msec
              CLIP3M = YI
              IMAX = I
            END IF
          END IF
        END IF
      END DO

      ! Get span of a vertex solution
      IF ( IMAX .GE. 0 ) THEN ! Vertex solution
        I = IMAX
        YI = CLIP3M
        DN = .TRUE.
        UP = .TRUE.
        IDN = I
        IUP = I
        DO WHILE ( DN .OR. UP )
          IF ( DN ) THEN
            IF ( ( IDN .LE. NFP ) .OR.
     &         ( R(IDN-1) .LT. YI ) ) THEN
              DN = .FALSE.
            ELSE
              IDN = IDN - 1
            END IF
          END IF
          IF ( UP ) THEN
            IF ( ( IUP .GE. NLP ) .OR.
     &         ( R(IUP+1) .LT. YI ) ) THEN
              UP = .FALSE.
            ELSE
              IUP = IUP + 1
            END IF
          END IF
        END DO

        TL = IDN
        IF ( IDN .GT. NFP ) THEN ! Extend downward to seg intersection
          TL = TL - ( ( R(IDN) - YI ) / ( R(IDN) - R(IDN-1) ) )
        END IF
        TR = IUP
        IF ( IUP .LT. NLP ) THEN ! Extend upward to seg intersection
          TR = TR + ( ( R(IUP) - YI ) / ( R(IUP) - R(IUP+1) ) )
        END IF
        TL = TL * DEL
        TR = TR * DEL
      END IF

      ! Scale CLIP span endpoints to msec
      TL = TL * 1000
      TR = TR * 1000

      RETURN
      END
