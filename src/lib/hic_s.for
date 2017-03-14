      SUBROUTINE HIC_S( R, S, NFP, NLP, JL, KL, NAR,
     & WIN, DEL, HIC, T1, T2, IOS )

!***********************************************************************
!* Computes the Head Injury Criterion Value and Time Interval
!*
!* Uses an efficient global branch and bound algorithm
!*
!* Key variables:
!*  FMAX - Incumbent/max value of HIC subfunction F
!*   F = ( S(K) - S(J) ) * ( K - J )**(-.6)
!*   HIC = FMAX**(2.5) * DEL
!*  JMAX - J index of FMAX value (corr. to time T1 at soln.)
!*  KMAX - K index of FMAX value (corr. to time T2 at soln.)
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

      REAL R(0:NLP) ! Resultant time series data array (send R(0))

      REAL S(0:NLP) ! Scratch array (send S(0)) (can send R(0))

      INTEGER NAR ! LJ and KL upper index (suggest NAR=2*NLP_max)

      INTEGER JL(NAR) ! Active region lower J bdy index scratch array

      INTEGER KL(NAR) ! Active region lower K bdy index scratch array

      REAL WIN ! Max HIC time window (msec) (send 0 for unwindowed HIC)

      REAL DEL ! Time step (delta-t) (sec) of time series data (>0)

      REAL HIC ! HIC solution value

      REAL T1 ! HIC solution start time

      REAL T2 ! HIC solution end time

      INTEGER IOS ! Return status (0 => success)


      ! Variables ______________________________________________________

      INTEGER NFP_H, I, JMAX, KMAX, KJL, KJU, NWU, IBR, IER, INC,
     & IBL, IL, ITMP

      REAL RMAX, FMAX

      DOUBLE PRECISION RC, RM, SC, SM


      ! Initializations
      HIC = 0.0
      T1 = 0.0
      T2 = 0.0
      IF ( DEL .LE. 0.0 ) THEN ! Nonpositive time step error
        IOS = 1
        RETURN
      END IF

      ! Compute sum (integral) array using trapezoid rule
      NFP_H = MAX( NFP, 0 ) ! Lower index for HIC search
      RM = ABS( R(NFP_H) )
      RMAX = REAL( RM )
      S(NFP_H) = 0.0
      SM = 0.D0
      DO I = NFP_H+1, NLP
        RC = ABS( R(I) )
        SC = SM + .5D0 * ( RC + RM )
        S(I) = REAL( SC )
        RMAX = MAX( RMAX, REAL(RC) )
        SM = SC
        RM = RC
      END DO

      ! Initialize HIC computation values
      FMAX = 0.0
      JMAX = NFP_H
      KMAX = NFP_H
      JL(1) = -NFP_H
      KL(1) = -( NFP_H + 1 )
      KJL = 1
      IF ( WIN .GT. 0.0 ) THEN ! Windowed HIC
        KJU = INT( WIN / ( 1000 * DEL ) * ( 1.0 + 1.E-7 ) )
      ELSE ! Unwindowed HIC - Suppress window limit
        KJU = NLP + 1
      END IF
      NWU = NLP - NFP_H
      IBR = 1
      IER = 1
      INC = -1
      IBL = NAR
      IL = NAR + 1

      ! Scan list of active regions
      IF ( ( NLP .GT. NFP_H ) .AND. ( RMAX .GT. 0.0 ) ) THEN
        DO WHILE ( IBR * INC .LE. IER * INC ) ! Perform scan pass
          CALL HIC_SCAN( IBR, IER, IL, INC, JL, KL,
     &       KJL, KJU, NWU, S, FMAX, RMAX, JMAX, KMAX )
          ! Set up for next scan pass
          ITMP = IBL
          IBL = IER
          IER = ITMP
          IBR = IL
          INC = -INC
          IL = IBL - INC
          NWU = NWU / 2
        END DO
      END IF

      ! Prepare output fields
      HIC = FMAX**2.5 * DEL
      T1 = JMAX * DEL * 1000
      T2 = KMAX * DEL * 1000

      RETURN
      END



      SUBROUTINE HIC_SCAN( IBR, IER, IL, INC, JL, KL,
     & KJL, KJU, NWU, S, FMAX, RMAX, JMAX, KMAX )

!***********************************************************************
!* Scans Current List of Active Regions
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************


      ! Arguments ______________________________________________________

      INTEGER IBR ! Index of beginning of active region list

      INTEGER IER ! Index of end of active region list

      INTEGER IL ! Index of beginning of new active region list

      INTEGER INC ! Increment (1 or -1) direction for active region list

      INTEGER JL(*) ! Active region lower J boundary index array

      INTEGER KL(*) ! Active region lower K boundary index array

      INTEGER KJL ! Lower limit of (K-J) for contender solution

      INTEGER KJU ! Upper limit of (K-J) from max HIC window

      INTEGER NWU ! Upper width of current active boxes

      REAL S(0:*) ! Summed resultant array (send S(0))

      REAL FMAX ! Current best solution F function value

      REAL RMAX ! Max resultant value

      INTEGER JMAX ! Current best solution left point index

      INTEGER KMAX ! Current best solution right point index


      ! Variables ______________________________________________________

      INTEGER IR, NJ, JLI, JUI, JLB, NK, KLI, KUI, KUB, IFR, KJ,
     & JKJMAX, J

      REAL SUL, F, FR, SKJMAX, SKJ


      ! Scan list of active regions
      DO IR = IBR, IER, INC
        JLI = JL(IR)
        IF ( JLI .GT. 0 ) THEN
          NJ = NWU
        ELSE
          JLI = -JLI
          NJ = NWU - 1
        END IF
        JUI = JLI + NJ
        KLI = KL(IR)
        IF ( KLI .GT. 0 ) THEN
          NK = NWU
        ELSE
          KLI = -KLI
          NK = NWU - 1
        END IF
        KUI = KLI + NK
        IF ( ( KUI-JLI .LT. KJL ) .OR. ( KLI-JUI .GT. KJU ) ) THEN
          ! Infeasible region - Discard
        ELSE IF ( ( NJ .GT. 0 ) .OR. ( NK .GT. 0 ) ) THEN
          ! Active region with feasible points - Bound
          SUL = S(KUI) - S(JLI)
          IF ( SUL * MAX( KLI-JUI, KJL )**(-.6) .GT. FMAX ) THEN
            ! Region is still active - Evaluate and partition
            IF ( KUI-JUI .LE. KJU ) THEN
              JLB = MAX( JLI, KUI-KJU )
              KUB = KUI
            ELSE
              JLB = JUI
              KUB = JUI + KJU
            END IF
            F = ( S(KUB) - S(JLB) ) * ( KUB - JLB )**(-.6)
            IF ( F .GT. FMAX ) THEN
              FMAX = F
              FR = ( FMAX / RMAX )**2.5
              IFR = INT( FR + 1. )
              KJL = MAX( KJL, INT(FR-IFR)+IFR )
              JMAX = JLB
              KMAX = KUB
            END IF
            IF ( ( IL*INC+4 .GT. IR*INC ) .OR. ( NWU .LT. 10 ) )
     &         THEN ! Complete with exhaustive method
              DO KJ = MAX( KLI-JUI, KJL ), MIN( KUI-JLI, KJU )
                SKJMAX = 0.0
                JKJMAX = 0
                DO J = MAX( KLI-KJ, JLI ), MIN( KUI-KJ, JUI )
                  SKJ = S(KJ+J) - S(J)
                  IF ( SKJ .GT. SKJMAX ) THEN
                    SKJMAX = SKJ
                    JKJMAX = J
                  END IF
                END DO
                F = SKJMAX * KJ**(-.6)
                IF ( F .GT. FMAX ) THEN
                  FMAX = F
                  FR = ( FMAX / RMAX )**2.5
                  IFR = INT( FR + 1. )
                  KJL = MAX( KJL, INT(FR-IFR)+IFR )
                  JMAX = JKJMAX
                  KMAX = KJ + JKJMAX
                END IF
              END DO
            ELSE ! Partition
              CALL HIC_PARTITION( JLI, JUI, NJ, NWU-NJ, KLI, KUI,
     &           NK, NWU-NK, IL, INC, JL, KL )
            END IF
          END IF
        ELSE ! Single point - Evaluate
          F = ( S(KUI) - S(JLI) ) * ( KUI - JLI )**(-.6)
          IF ( F .GT. FMAX ) THEN
            FMAX = F
            FR = ( FMAX / RMAX )**2.5
            IFR = INT( FR + 1. )
            KJL = MAX( KJL, INT(FR-IFR)+IFR )
            JMAX = JLI
            KMAX = KUI
          END IF
        END IF
      END DO

      RETURN
      END



      SUBROUTINE HIC_PARTITION( JLI, JUI, NJ, NJD, KLI, KUI,
     & NK, NKD, IL, INC, JL, KL )

!***********************************************************************
!* Performs Partitioning of Possibly Optimal Regions
!* . Positive index used for width=NWU
!* . Nonpositive index used for width=NWU-1
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************


      ! Arguments ______________________________________________________

      INTEGER JLI ! Lower J index of region to partition

      INTEGER JUI ! Upper J index of region to partition

      INTEGER NJ ! J side width of region to partition

      INTEGER NJD ! Upper width - NJ (0 or 1)

      INTEGER KLI ! Lower K index of region to partition

      INTEGER KUI ! Upper K index of region to partition

      INTEGER NK ! K side width of region to partition

      INTEGER NKD ! Upper width - NK (0 or 1)

      INTEGER IL ! Index of beginning of new active region list

      INTEGER INC ! Increment (1 or -1) direction for active region list

      INTEGER JL(*) ! Active region lower J boundary index array

      INTEGER KL(*) ! Active region lower K boundary index array


      ! Variables ______________________________________________________

      INTEGER JMU, NJMOD2, KMU, NKMOD2


      ! Initializations
      JMU = ( JUI - ( NJ / 2 ) )
      NJMOD2 = MOD( NJ, 2 )
      IF ( ( NJMOD2 .NE. 0 ) .AND. ( NJD .NE. 0 ) ) JMU = -JMU
      KMU = ( KUI - ( NK / 2 ) )
      NKMOD2 = MOD( NK, 2 )
      IF ( ( NKMOD2 .NE. 0 ) .AND. ( NKD .NE. 0 ) ) KMU = -KMU

      ! First region
      IL = IL + INC
      JL(IL) = JMU
      KL(IL) = KMU

      ! Second region
      IF ( NJ .GT. 0 ) THEN ! Finite width along J axis
        IF ( ( NJMOD2 .EQ. 0 ) .OR. ( NJD .NE. 0 ) ) JLI = -JLI
        IL = IL + INC
        JL(IL) = JLI
        KL(IL) = KMU
      END IF

      ! Third and fourth regions
      IF ( NK .GT. 0 ) THEN ! Finite width along K axis
        IF ( ( NKMOD2 .EQ. 0 ) .OR. ( NKD .NE. 0 ) ) KLI = -KLI

        ! Third region
        IF ( NJ .GT. 0 ) THEN ! Finite width along J axis
          IL = IL + INC
          JL(IL) = JLI
          KL(IL) = KLI
        END IF

        ! Fourth region
        IL = IL + INC
        JL(IL) = JMU
        KL(IL) = KLI

      END IF

      RETURN
      END
