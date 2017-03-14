!***********************************************************************
!* Interpolation Routines:
!*   INTERP_S - In-place interpolation
!*   INTERP_SN - Out-of-place interpolation
!*   INTERP_XY_S - X-Y interpolation
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!*
!* Notes:
!* . In-place: overwrites Y array, range, and step with interpolated data
!* . Out-of-place: writes Z array, range, and step with interpolated data
!***********************************************************************



      SUBROUTINE INTERP_S( Z, NFP_L, NLP_U, NFP, NLP, DX, DX_NEW, IOS )

!***********************************************************************
!* In-Place Interpolation of an Array to a New Step Value
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!*
!* Notes:
!* . This routine does not worry about aliasing, so the input signal
!*   should be filtered appropriately for the new step prior to
!*   interpolation
!***********************************************************************


      ! Arguments ______________________________________________________

      INTEGER NFP_L ! Data array lower index limit

      INTEGER NLP_U ! Data array upper index limit

      REAL Z(NFP_L:NLP_U) ! Data array -> Interpolated data

      INTEGER NFP ! Index of first data point

      INTEGER NLP ! Index of last data point

      DOUBLE PRECISION DX ! X-axis step (delta-x) of data (>0)

      DOUBLE PRECISION DX_NEW ! New X-axis step (delta-x) desired (>0)

      INTEGER IOS ! Status flag returned


      ! Do in-place interpolation
      CALL INTERP_SN( Z, NFP_L, NLP_U, NFP, NLP, DX, DX_NEW,
     & Z, NFP, NLP, DX, IOS )

      RETURN
      END



      SUBROUTINE INTERP_SN( Z, NFP_L, NLP_U, NFP, NLP, DX, DX_NEW,
     & Z_OUT, NFP_OUT, NLP_OUT, DX_OUT, IOS )

!***********************************************************************
!* Interpolation of an Array to a New Step Value
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!*
!* Notes:
!* . This routine does not worry about aliasing, so the input signal
!*   should be filtered appropriately for the new step prior to
!*   interpolation
!***********************************************************************


      ! Arguments ______________________________________________________

      INTEGER NFP_L ! Data array lower index limit

      INTEGER NLP_U ! Data array upper index limit

      REAL Z(NFP_L:NLP_U) ! Data array

      INTEGER NFP ! Index of first data point

      INTEGER NLP ! Index of last data point

      DOUBLE PRECISION DX ! X-axis step (delta-x) of data (>0)

      DOUBLE PRECISION DX_NEW ! New X-axis step (delta-x) desired (>0)

      REAL Z_OUT(NFP_L:NLP_U) ! Interpolated data array (can send Z)

      INTEGER NFP_OUT ! Index of first interpolated point (can send NFP)

      INTEGER NLP_OUT ! Index of last interpolated point (can send NLP)

      DOUBLE PRECISION DX_OUT ! Interpolated X-axis step (delta-x) (>0)

      INTEGER IOS ! Status flag returned


      ! Variables ______________________________________________________

      INTEGER NFP_NEW, NLP_NEW, ISUB, NSUB, N

      DOUBLE PRECISION STT, FTT


      ! Functions ______________________________________________________

      LOGICAL EQUAL_DP

      EXTERNAL EQUAL_DP


      ! Check for valid step values
      IF ( ( DX .LE. 0.D0 ) .OR. ( DX_NEW .LE. 0.D0 ) ) THEN
        IOS = 1
        RETURN
      END IF
      IOS = 0

      ! Check for subsampling
      NSUB = NINT( DX_NEW / DX )
      IF ( .NOT. EQUAL_DP( DX_NEW, NSUB * DX, 1.D-15 ) ) THEN ! Interp
        ISUB = 0
      ELSE ! Subsample
        ISUB = NSUB
      END IF

      ! Set first point index for output signals
      STT = NFP * DX
      NFP_NEW = NINT( STT / DX_NEW )
      IF ( ( NFP_NEW * DX_NEW .LT. STT ) .AND.
     & ( .NOT. EQUAL_DP( NFP_NEW * DX_NEW, STT, 2.D-7 ) ) ) THEN
        NFP_NEW = NFP_NEW + 1
      END IF
      IF ( NFP_NEW .LT. NFP_L ) THEN
        NFP_NEW = NFP_L
      END IF

      ! Set last point index for output signals
      FTT = NLP * DX
      NLP_NEW = NINT( FTT / DX_NEW )
      IF ( ( NLP_NEW * DX_NEW .GT. FTT ) .AND.
     & ( .NOT. EQUAL_DP( NLP_NEW * DX_NEW, FTT, 2.D-7 ) ) ) THEN
        NLP_NEW = NLP_NEW - 1
      END IF
      IF ( NLP_NEW .GT. NLP_U ) THEN
        NLP_NEW = NLP_U
      END IF

      ! Subsample or interpolate to new step
      IF ( ISUB .GT. 0 ) THEN ! Subsample

        DO N = MAX(0,NFP_NEW), NLP_NEW
          Z_OUT(N) = Z(N*ISUB)
        END DO
        DO N = MIN(-1,NLP_NEW), NFP_NEW, -1
          Z_OUT(N) = Z(N*ISUB)
        END DO

      ELSE ! Interpolate

        IF ( DX_NEW .GT. DX ) THEN ! Outward assign to avoid overwriting

          DO N = MAX(0,NFP_NEW), NLP_NEW
            CALL INTERP_VAL( Z, NFP_L, NLP_U, DX, DX_NEW,
     &         N, Z_OUT(N) )
          END DO
          DO N = MIN(-1,NLP_NEW), NFP_NEW, -1
            CALL INTERP_VAL( Z, NFP_L, NLP_U, DX, DX_NEW,
     &         N, Z_OUT(N) )
          END DO

        ELSE ! Inward assign to avoid overwriting

          DO N = NLP_NEW, MAX(0,NFP_NEW), -1
            CALL INTERP_VAL( Z, NFP_L, NLP_U, DX, DX_NEW,
     &         N, Z_OUT(N) )
          END DO
          DO N = NFP_NEW, MIN(-1,NLP_NEW)
            CALL INTERP_VAL( Z, NFP_L, NLP_U, DX, DX_NEW,
     &         N, Z_OUT(N) )
          END DO

        END IF

      END IF

      ! Set output array index range and x-axis step
      NFP_OUT = NFP_NEW
      NLP_OUT = NLP_NEW
      DX_OUT = DX_NEW

      RETURN
      END



      SUBROUTINE INTERP_VAL( Z, NFP_L, NLP_U, DX, DX_NEW, N, ZN )

!***********************************************************************
!* Sets a Single Interpolated Value
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!*
!* Notes:
!* . Assumes INTERP_VAL calls ordered to avoid overwriting
!* . Assumes N*DX_NEW falls within valid Z range
!***********************************************************************


      ! Arguments ______________________________________________________

      INTEGER NFP_L ! Data array lower index limit

      INTEGER NLP_U ! Data array upper index limit

      REAL Z(NFP_L:NLP_U) ! Data array

      DOUBLE PRECISION DX ! X-axis step (delta-x) of data (>0)

      DOUBLE PRECISION DX_NEW ! New X-axis step (delta-x) desired (>0)

      INTEGER N ! Interpolated data index

      REAL ZN ! Interpolated data value


      ! Variables ______________________________________________________

      INTEGER I

      DOUBLE PRECISION XN


      ! Functions ______________________________________________________

      LOGICAL EQUAL_DP

      EXTERNAL EQUAL_DP


      ! Set interpolated value
      XN = N * DX_NEW
      I = NINT( XN / DX ) ! Nearest index in input array
      IF ( .NOT. EQUAL_DP( I * DX, XN, 1.D-15 ) ) THEN ! Between points
        IF ( N .GT. 0 ) THEN
          I = INT( XN / DX ) ! Downward round for (+) value
        ELSE ! N < 0
          I = INT( XN / DX ) - 1 ! Downward round for (-) value
        END IF
        ZN = REAL( Z(I) + ( ( XN / DX ) - I ) * ( Z(I+1) - Z(I) ) )
      ELSE ! On a point
        ZN = Z(I)
      END IF

      RETURN
      END



      SUBROUTINE INTERP_XY_S( X, Y, NFP_L, NLP_U, NFP, NLP,
     & DX_NEW, Y_OUT, NFP_OUT, NLP_OUT, IOS )

!***********************************************************************
!* Interpolation of X-Y Signal to a Specified Even Step Value
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!*
!* Notes:
!* . This routine does not worry about aliasing, so the input signal
!*   should be filtered appropriately for the new step prior to
!*   interpolation
!* . X array not set to even step
!* . Returns the initial x-monotonic segment if y(x) is multi-valued
!***********************************************************************


      ! Arguments ______________________________________________________

      INTEGER NFP_L ! Data array lower index limit

      INTEGER NLP_U ! Data array upper index limit

      REAL X(NFP_L:NLP_U) ! X-axis data array

      REAL Y(NFP_L:NLP_U) ! Y-axis data array

      INTEGER NFP ! Index of first data point

      INTEGER NLP ! Index of last data point

      DOUBLE PRECISION DX_NEW ! New X-axis step (delta-x) desired (>0)

      REAL Y_OUT(NFP_L:NLP_U) ! Interpolated Y-axis data array

      INTEGER NFP_OUT ! Index of first interpolated data point

      INTEGER NLP_OUT ! Index of last interpolated data point

      INTEGER IOS ! Status flag returned


      ! Variables ______________________________________________________

      LOGICAL FIRST

      INTEGER I, K, INC, NA, NB

      DOUBLE PRECISION XM, XI, XD, XK, YM, YI, YD, FRAC


      ! Functions ______________________________________________________

      LOGICAL EQUAL_DP

      EXTERNAL EQUAL_DP


      ! Check for valid step value
      IF ( ( DX_NEW .LE. 0.D0 ) .OR. ( NFP .GE. NLP ) ) THEN
        IOS = 1
        RETURN
      END IF
      IOS = 0

      ! Set x-axis direction
      IF ( X(NFP) .LE. X(NFP+1) ) THEN ! Increasing
        INC = 1
      ELSE ! Decreasing
        INC = -1
      END IF

      ! Interpolate to even step
      I = NFP
      XI = X(I)
      YI = Y(I)
      FIRST = .TRUE.
      DO WHILE ( ( I .LT. NLP ) .AND. ( X(I+1) * INC .GE. XI * INC ) )
        I = I + 1
        XM = XI
        XI = X(I)
        XD = XI - XM
        NA = MIN( MAX( INT( XM / DX_NEW - INC ), NFP_L ), NLP_U )
        NB = MIN( MAX( INT( XI / DX_NEW + INC ), NFP_L ), NLP_U )
        YM = YI
        YI = Y(I)
        YD = YI - YM
        DO K = NA, NB, INC
          XK = K * DX_NEW
          IF ( ( ( XK .GE. MIN( XM, XI ) ) .AND.
     &       ( XK .LE. MAX( XM, XI ) ) ) .OR.
     &       ( EQUAL_DP( XK, XM, 2.D-7 ) ) .OR.
     &       ( EQUAL_DP( XK, XI, 2.D-7 ) ) ) THEN ! In range
            IF ( XD .NE. 0.D0 ) THEN
              FRAC = ( XK - XM ) / XD
              Y_OUT(K) = REAL( YM + FRAC * YD )
            ELSE
              Y_OUT(K) = REAL( YM )
            END IF
            IF ( FIRST ) THEN
              NFP_OUT = K
              NLP_OUT = K
              FIRST = .FALSE.
            ELSE
              NFP_OUT = MIN( NFP_OUT, K )
              NLP_OUT = MAX( NLP_OUT, K )
            END IF
          END IF
        END DO
      END DO

      IF ( FIRST ) IOS = 2 ! No output

      RETURN
      END
