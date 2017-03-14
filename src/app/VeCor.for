      PROGRAM VECOR

!***********************************************************************
!* Performs assessment and correction of systematic calibration, bias,
!* and time shift errors in one-dimensional VTB and VTV tests.
!*
!* Inputs are the impact direction accelerations and summed barrier
!* force, and available supplementary measurements.
!*
!* The supplementary measurements and kinematic and physical principles
!* are used with a weighted error minimization scheme to estimate the
!* errors in each pulse.
!*
!* The current version of VeCor is based on old source code and does not
!* have a particularly friendly user interface.  A rewrite of VeCor is
!* planned.
!*
!* This program was developed by Stuart G. Mentzer under contract to:
!*
!*   National Highway Traffic Safety Administration
!*   U.S. Department of Transportation
!*   400 7th St., S.W.
!*   Washington, DC  20590
!*   U.S.A.
!*
!* Complete documentation can be obtained from this agency.
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/27
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'

      RECORD /UDS_Y/  U

      INTEGER ITL(0:20), ITU(0:20), ITMIN(0:20),
     & IEA(3,100), IEV(3,100), IED(3,100), IER(4,100),
     & IEVS(3,2), IEM(3), ITCON(3,100)

      INTEGER I, J, JL, M, M1, M2, NEA, NEV, NED, NER, NTC, NGA, NRAW,
     & NFP_V, NLP_V

      ! Array range - can be adjusted as desired
      PARAMETER ( NFP_V = -10000, NLP_V = 100000 )

      REAL A(NFP_V-1000:NLP_V+1000,0:20),
     & V(NFP_V-1000:NLP_V+1000,0:20),
     & D(NFP_V-1000:NLP_V+1000,0:20)

      DOUBLE PRECISION DT, V0(0:20), DTAV, DTVD, DTAD, WT(0:20),
     & CFAC(0:20), ES(2,0:20), EB(2,0:20), ET(2,0:20),
     & EA(3,100), EV(3,100), ED(3,100), ER(3,100), EVS(3,2), EM(2),
     & GI(42,42), RI(42), XMIN(42), ERRAW, ERMIN,
     & C(0:20), B(0:20), T(0:20)

      CHARACTER IDCODE*10, TS_FILE(0:20)*255, DSCR(0:20)*30, OUT*2,
     & OUT_FILE*255

      DATA JL/1/, M/0/, CFAC/21*1.D0/, NEA/0/, NEV/0/, NED/0/,
     & NER/0/, NTC/0/, GI/1764*0.D0/, RI/42*0.D0/,
     & ITMIN/21*0/, (XMIN(I),I=1,41,2)/21*1.D0/,
     & (XMIN(I),I=2,42,2)/21*0.D0/, NRAW/0/


      ! Write VeCor Program header
      WRITE( *, 901 )

      ! Read the VeCor input file
      CALL INPUT( IDCODE, TS_FILE, JL, M, M1, M2,
     & A, DT, V0, DSCR, WT, CFAC, DTAV, DTVD, DTAD,
     & ITL, ITU, ES, EB, ET, EA, IEA, NEA, EV, IEV, NEV,
     & ED, IED, NED, ER, IER, NER, EVS, IEVS, EM, IEM,
     & ITCON, NTC, NGA, OUT, OUT_FILE, NFP_V, NLP_V, U )

      ! Integrate the pulse data
      DO J = JL, M
        CALL INTEG( DTAV, DTVD, DTAD, V0(J), A(NFP_V-1000,J),
     &     V(NFP_V-1000,J), D(NFP_V-1000,J), NFP_V, NLP_V )
      END DO

      ! Compute the error measure for the raw data
      CALL EVAL( JL, M, M1, M2, WT, A, V, D, NFP_V, NLP_V, DT,
     & DTAV, DTVD, DTAD, ITMIN, XMIN, ES, EB, ET, EA,
     & IEA, NEA, EV, IEV, NEV, ED, IED, NED, ER, IER, NER,
     & EVS, IEVS, EM, IEM, ITCON, NRAW, ERRAW )

      ! Compute the invariant contributions to the gradient equations
      CALL GRINVAR( JL, M, M1, M2, WT, V, D, NFP_V, NLP_V,
     & DTAV, DTVD, DTAD, ES, EB, EA, IEA, NEA, EV, IEV, NEV,
     & ED, IED, NED, ER, IER, NER, EVS, IEVS, EM, IEM, GI, RI )

      ! Perform the time shift grid/gradient search for minimum error
      CALL SEARCH( JL, M, M1, M2, WT, A, V, D, NFP_V, NLP_V, DT,
     & DTAV, DTVD, DTAD, GI, RI, ITL, ITU, NGA, ES, EB, ET, EA,
     & IEA, NEA, EV, IEV, NEV, ED, IED, NED, ER, IER, NER,
     & EVS, IEVS, EM, IEM, ITCON, NTC, ITMIN, XMIN, ERMIN )

      ! Report error measure and pulse errors from solution
      CALL REPORT( OUT_FILE, IDCODE, JL, M, DSCR, DT, CFAC,
     & ITMIN, XMIN, ERRAW, ERMIN, C, B, T )

      ! Output corrected UDS files
      IF ( OUT .NE. 'NO' ) THEN
        CALL OUTPUT( IDCODE, JL, M, C, B, T, ITMIN, TS_FILE,
     &     NFP_V, NLP_V, U, A(NFP_V,0) )
      END IF

  901 FORMAT(//1X,6X,67('*')/1X,6X,'*',65X,'*'/1X,6X,'*   VeCor ',
     & '- Vehicle Test Data Assessment and Correction Program   *'/
     & 1X,6X,'*',65X,'*'/1X,6X,67('*')///)

      END



      SUBROUTINE INPUT( IDCODE, TS_FILE, JL, M, M1, M2,
     & A, DT, V0, DSCR, WT, CFAC, DTAV, DTVD, DTAD,
     & ITL, ITU, ES, EB, ET, EA, IEA, NEA, EV, IEV, NEV,
     & ED, IED, NED, ER, IER, NER, EVS, IEVS, EM, IEM,
     & ITCON, NTC, NGA, OUT, OUT_FILE, NFP_V, NLP_V, U )

!***********************************************************************
!* Reads  Input File and Prepares Necessary Arrays
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'

      RECORD /UDS_Y/  U
      RECORD /UDS_CONTROL/  UC

      INTEGER JL, M, M1, M2, ITL(0:20), ITU(0:20),
     & IEA(3,100), NEA, IEV(3,100), NEV, IED(3,100), NED,
     & IER(4,100), NER, IEVS(3,2), IEM(3), ITCON(3,100), NTC, NGA,
     & NFP_V, NLP_V, I, IE, IV, IOS, ITUMAX, IVNO, INP_UNIT,
     & J, J1, J2, JT(21), MV, NFPT, NLPT, NGMAX

      REAL A(NFP_V-1000:NLP_V+1000,0:20), DELT, VEL0, VWT

      DOUBLE PRECISION DT, V0(0:20), AVFAC, VDFAC, ADFAC,
     & DTAV, DTVD, DTAD, ES(2,0:20), EB(2,0:20), ET(2,0:20),
     & TSL(0:20), TSLJ, CFAC(0:20), WT(0:20), VWT1, VWT2,
     & WTOT, TL, TU, VAL, VALR, RMSR, EA(3,100), EV(3,100),
     & ED(3,100), ER(3,100), EVS(3,2), EM(2), EVS1, GFAC, DP_TRAN

      CHARACTER INP_FILE*255, IDCODE*10, TITL*70, PM(0:20),
     & TS_FILE(0:20)*255, DIMSYST*3, TSTNAMT*12, AXIST*2,
     & DSCR(0:20)*30, DEF*2, ETYP*2, OUT*2, OUT_FILE*255,
     & LJUST*255

      INTEGER NUM_ARGS, MAX_ARGS
      PARAMETER ( MAX_ARGS = 1 )

      CHARACTER CL_ARG(0:MAX_ARGS)*255


      ! Initializations
      IVNO = 0
      DELT = 0.0
      VEL0 = 0.0
      VWT = 0.0
      AVFAC = 0.0D0
      VDFAC = 0.0D0
      ADFAC = 0.0D0
      GFAC = 0.0D0

      ! Get input file name and open it
      INP_FILE = ' '
      CALL GET_CL( CL_ARG, NUM_ARGS, MAX_ARGS )
      IF ( NUM_ARGS .GT. 0 ) INP_FILE = CL_ARG(1)
  101 DO WHILE ( INP_FILE .EQ. ' ' ) ! Prompt for input file name
        WRITE( *, 901 )
        READ( *, 911, END=195 ) INP_FILE
      END DO
      CALL OPEN_FS( INP_UNIT, INP_FILE, 'R', IOS )
      IF ( IOS .NE. 0 ) THEN
  102   WRITE( *, 981 )
        INP_FILE = ' '
        GO TO 101
      END IF

      ! Read and report run title and ID code
  103 READ( INP_UNIT, 912, END=194 ) IDCODE, TITL
      WRITE( *, 902 ) TITL, IDCODE

      ! Write pulse summary header
      WRITE( *, 903 )
      WRITE( *, 904 )

      ! Initializations
      VWT1 = 0.D0
      VWT2 = 0.D0

      ! Set up UDS read controls
      CALL UDS_CONTROL_INIT( UC )
      UC.DIMSYS = ' '
      UC.CHANFORM = 'Y'

      ! Read pulse information
  104 READ( INP_UNIT, 913, ERR=106, END=194 ) J
      BACKSPACE INP_UNIT

      ! Read spec's for each signal
      IF ( ( ( ( J .EQ. 0 ) .AND. ( JL .EQ. 1 ) ) .OR.
     & ( J .EQ. M+1 ) ) .AND. ( J .LE. 20 ) ) THEN
        ! Pulse number is acceptable - Read spec's
        M = J
        IF ( J .EQ. 0 ) JL = 0
        READ( INP_UNIT, 914, ERR=191, END=194 )
     &   PM(J), TSL(J), ES(2,J), EB(2,J),
     &   ET(2,J), ES(1,J), EB(1,J), ET(1,J)
        IF ( ES(1,J) .EQ. 0.D0 ) ES(1,J) = 1.D0
        IF ( ES(2,J) .EQ. 0.D0 ) ES(2,J) = .5D0
        ES(2,J) = 1.D0 / ES(2,J)**2
        IF ( ET(2,J) .EQ. 0.D0 ) ET(2,J) = .02D0
        ET(2,J) = 1.D0 / ET(2,J)**2

        READ( INP_UNIT, 911, END=194 ) TS_FILE(J)
        TS_FILE(J) = LJUST( TS_FILE(J) )
        CALL FN_LOCAL( TS_FILE(J) )

        ! Read the pulse UDS file
        CALL UDS_READ( TS_FILE(J), U, UC, IOS )
        IF ( IOS .NE. 0 ) GO TO 193

        ! Check and prepare pulse data
        IF ( ( U.XTYP .NE. 'TIME' ) .OR.
     &   ( U.XUNITS .NE. 'SECONDS' ) ) THEN
          WRITE( *, 982 )
          STOP ' '
        END IF
        IF ( ( U.NFP .LT. NFP_V ) .OR. ( U.NLP .GT. NLP_V ) ) THEN
          WRITE( *, * )
     &     '*** WARNING: Arrays limited to VeCor bounds: ',
     &     NFP_V, ' : ', NLP_V
          U.NFP = MAX( U.NFP, NFP_V )
          U.NLP = MIN( U.NLP, NLP_V )
        END IF
        IF ( U.FCUT .NE. 0. ) WRITE( *, 971 )
        V0(J) = DP_TRAN( U.INIVEL )
        DO I = NFP_V, U.NFP-1
          A(I,J) = 0.
        END DO
        DO I = U.NFP, U.NLP
          A(I,J) = U.Y(I)
        END DO
        DO I = U.NLP+1, NLP_V
          A(I,J) = 0.
        END DO
        IF ( J .EQ. JL ) THEN
          ! This is the first pulse read
          NFPT = U.NFP
          NLPT = U.NLP
          DIMSYST = U.DIMSYS
          UC.DIMSYS = U.DIMSYS
          DT = DP_TRAN( U.DEL )
          TSTNAMT = U.TSTNAM
          AXIST = U.AXIS
          DELT = U.DEL

          IF ( U.DIMSYS .EQ. 'MET' ) THEN ! Metric
            AVFAC = 9.80665D0 * 3600.D0 / 1000.D0
            VDFAC = 1.D6 / 3600.D0
            ADFAC = 9.80665D3
            GFAC = 9.80665D0
          ELSE IF ( U.DIMSYS .EQ. 'SI' ) THEN ! SI
            AVFAC = 1.D0
            VDFAC = 1.D0
            ADFAC = 1.D0
            GFAC = 9.80665D0
          ELSE IF ( U.DIMSYS .EQ. 'ENG' ) THEN ! English
            AVFAC = 21.93675D0
            VDFAC = 17.6D0
            ADFAC = 386.0868D0
            GFAC = 1.D0
          ELSE
            WRITE( *, * ) '*** ERROR: Unsupported DIMSYS: ', U.DIMSYS
            STOP ' '
          END IF

          IF ( J .EQ. 0 ) THEN ! Should be a total external force file
            IF ( U.YTYP .NE. 'FORCE' ) THEN
              WRITE( *, 983 )
              STOP ' '
            END IF
            IF ( ( ( U.DIMSYS .EQ. 'MET' ) .OR.
     &       ( U.DIMSYS .EQ. 'SI' ) ) .AND.
     &       ( U.YUNITS .NE. 'NEWTONS' ) ) THEN
              WRITE( *, * ) '*** ERROR: Force file units not NEWTONS'
              STOP ' '
            ELSE IF ( ( U.DIMSYS .EQ. 'ENG' ) .AND.
     &       ( U.YUNITS .NE. 'POUNDS' ) ) THEN
              WRITE( *, * ) '*** ERROR: Force file units not POUNDS'
              STOP ' '
            END IF
            V0(0) = 0.D0
          ELSE ! Should be an acceleration file
            IF ( ( U.YTYP .NE. 'ACCELERATION' ) .OR.
     &       ( U.YUNITS .NE. 'G''S' ) ) THEN
              WRITE( *, 984 ) J
              STOP ' '
            END IF
            IF ( U.VEHNO .NE. 1 ) THEN
              WRITE( *, 985 )
              STOP ' '
            END IF
            IVNO = U.VEHNO
            VEL0 = U.INIVEL
            VWT = U.VEHTWT
            VWT1 = DP_TRAN( U.VEHTWT )
          END IF
        ELSE ! Not the first pulse read
          IF ( U.TSTNAM .NE. TSTNAMT ) THEN
            WRITE( *, * ) '*** WARNING: Inconsistent TSTNAM value'
          END IF
          IF ( U.AXIS .NE. AXIST ) THEN
            WRITE( *, * ) '*** WARNING: Inconsistent AXIS value'
          END IF
          IF ( U.DIMSYS .NE. DIMSYST ) THEN
            WRITE( *, * ) '*** ERROR: Mixed unit system UDS files'
            STOP ' '
          END IF
          IF ( U.DEL .NE. DELT ) THEN
            WRITE( *, * ) '*** ERROR: Inconsistent DEL value'
            STOP ' '
          END IF
          IF ( ( U.YTYP .NE. 'ACCELERATION' ) .OR.
     &     ( U.YUNITS .NE. 'G''S' ) ) THEN
            WRITE( *, 984 ) J
            STOP ' '
          END IF
          NFPT = MAX( NFPT, U.NFP )
          NLPT = MIN( NLPT, U.NLP )
          IF ( ( JL .EQ. 0 ) .AND. ( J .EQ. 1 ) ) THEN
            ! First acceleration pulse read
            IF ( U.VEHNO .NE. 1 ) THEN
              WRITE( *, 985 )
              STOP ' '
            END IF
            IVNO = U.VEHNO
            VEL0 = U.INIVEL
            VWT = U.VEHTWT
            VWT1 = DP_TRAN( U.VEHTWT )
          ELSE IF ( ( JL .EQ. 1 ) .AND. ( U.VEHNO .EQ. 2 ) .AND.
     &     ( IVNO .EQ. 1 ) ) THEN
            ! This is the first Vehicle 2 acceleration pulse read
            M1 = J-1
            IVNO = 2
            VEL0 = U.INIVEL
            VWT = U.VEHTWT
            VWT2 = DP_TRAN( U.VEHTWT )
          ELSE ! Not the first acceleration pulse for a vehicle
            IF ( U.VEHNO .NE. IVNO ) THEN
              WRITE( *, 985 )
              STOP ' '
            END IF
            IF ( U.VEHTWT .NE. VWT ) THEN
              WRITE( *, 987 )
              STOP ' '
            END IF
            IF ( U.INIVEL .NE. VEL0 ) WRITE( *, 972 )
          END IF
        END IF

        ! Write pulse summary entry for this pulse
        DSCR(J) = U.SENATT
        IF ( U.VEHNO .EQ. 2 ) DSCR(J) = U.SENATT(1:27)//'V 2'
        WRITE( *, 905 ) J, DSCR(J), TS_FILE(J)
      ELSE ! Pulse numbering is wrong
        WRITE( *, 988 )
        STOP ' '
      END IF
      GO TO 104

      ! Report dimensional system
  106 IF ( U.DIMSYS .EQ. 'MET' ) THEN
        WRITE( *, '(/A/)')' Metric dimensional system being used'
      ELSE IF ( U.DIMSYS .EQ. 'SI' ) THEN
        WRITE( *, '(/A/)')' SI dimensional system being used'
      ELSE
        WRITE( *, '(/A/)')' English dimensional system being used'
      END IF

      ! Check time starts at or before zero
      IF ( NFPT .GT. 0 ) THEN
        WRITE( *, * ) '*** ERROR: Initial array index is >0'
        STOP ' '
      END IF

      ! Check time step specified in files
      IF ( DT .EQ. 0.D0 ) THEN
        WRITE( *, * ) '*** ERROR: Time step (DEL) is zero in UDS files'
        STOP ' '
      END IF

      ! Check vehicle weight specified in files
      IF ( VWT1 + VWT2 .EQ. 0.D0 ) THEN
        WRITE( *, * )
     &   '*** ERROR: No vehicle weight (VEHTWT) in UDS files'
        STOP ' '
      END IF

      ! Prepare pulse related values
      IF ( ( JL .EQ. 0 ) .OR. ( IVNO .EQ. 1 ) ) THEN
        ! This is a single vehicle run
        M1 = M
        M2 = 0
        WT(0) = .5D0
        CFAC(0) = -1.D0 / (VWT1*GFAC)
        IF ( PM(0) .EQ. '+' ) CFAC(0) = -CFAC(0)
        DO J = 1, M
          WT(J) = .5D0 / M
          IF ( PM(J) .EQ. '-' ) CFAC(J) = -1.D0
        END DO
      ELSE ! This is a VTV type run (no external force)
        M2 = M-M1
        WTOT = VWT1 + VWT2
        DO J = 1, M1
          WT(J) = VWT1 / ( M1 * WTOT )
          IF ( PM(J) .EQ. '-' ) CFAC(J) = -1.D0
        END DO
        DO J = M1+1, M
          WT(J) = VWT2 / ( M2 * WTOT )
          IF ( PM(J) .NE. '+' ) CFAC(J) = -1.D0
        END DO
      END IF

      ! Perform scaling/inversion of data
      DO J = JL, M
        DO I = NFP_V, NLP_V
          A(I,J) = REAL( CFAC(J) * A(I,J) )
        END DO
        V0(J) = CFAC(J) * V0(J)
        EB(1,J) = CFAC(J) * EB(1,J)
        EB(2,J) = CFAC(J) * EB(2,J)
        IF ( EB(2,J) .EQ. 0.D0 ) EB(2,J) = 5.D0
        EB(2,J) = 1.D0 / EB(2,J)**2
      END DO

      ! Set time step/conversion factors
      DTAV = AVFAC * DT
      DTVD = VDFAC * DT
      DTAD = ADFAC * DT**2

      ! Set time shift limit indices
      ITUMAX = 0
      DO J = JL, M
        TSLJ = ABS( TSL(J) )
        IF ( TSLJ .EQ. 0.D0 ) TSLJ = MIN( .01D0, (NLPT-NFPT)*DT )
        ITL(J) = MAX( INT( -TSLJ / DT ), -1000 )
        ITU(J) = -ITL(J)
        ITUMAX = MAX( ITUMAX, ITU(J) )
      END DO

      ! Set default acceleration errors
      DO J = JL, M
        NEA = NEA + 1
        IEA(1,NEA) = J
        IEA(2,NEA) = MIN( NFPT-ITL(J), 1 )
        IEA(3,NEA) = 0
        EA(1,NEA) = 0.D0
        IF ( IEA(3,NEA) .LT. IEA(2,NEA) ) THEN
          EA(2,NEA) = 0.D0
        ELSE
          CALL RMSMIN( IEA(2,NEA), IEA(3,NEA), A(NFP_V-1000,J),
     &     NFP_V, NLP_V, RMSR )
          EA(2,NEA) = 1.D0 /
     &     ( ( IEA(3,NEA) - IEA(2,NEA) + 1 ) * ( 2.D0 + RMSR )**2 )
        END IF
        EA(3,NEA) = EA(1,NEA) * EA(2,NEA)

        NEA = NEA + 1
        IEA(1,NEA) = J
        IEA(3,NEA) = MAX( NLPT - ITU(J), -1 )
        IEA(2,NEA) = MAX( IEA(3,NEA) - NINT( .03D0 / DT ), 0 )
        EA(1,NEA) = 0.D0
        IF ( IEA(3,NEA) .LT. IEA(2,NEA) ) THEN
          EA(2,NEA) = 0.D0
        ELSE
          CALL RMSMIN( IEA(2,NEA), IEA(3,NEA), A(NFP_V-1000,J),
     &     NFP_V, NLP_V, RMSR )
          EA(2,NEA) = 1.D0 /
     &     ( ( IEA(3,NEA) - IEA(2,NEA) + 1 ) * ( 3.D0 + RMSR )**2 )
        END IF
        EA(3,NEA) = EA(1,NEA) * EA(2,NEA)
      END DO

      ! Set default velocity scatter error(s)
      IEVS(1,1) = 1
      IEVS(3,1) = MAX( NLPT - ITUMAX, -1 )
      IEVS(2,1) = MAX( IEVS(3,1) - NINT( .02D0 / DT ), 0 )
      EVS1 = 0.D0
      DO J = 1, M1
        EVS1 = EVS1 + V0(J)
      END DO
      EVS(1,1) = EVS1 / M1
      IF ( IEVS(3,1) .LT. IEVS(2,1) ) THEN
        EVS(2,1) = 0.D0
      ELSE
        IF ( U.DIMSYS .EQ. 'ENG' ) THEN ! English
          EVS(2,1) = 1.D0 /
     &     ( ( IEVS(3,1) - IEVS(2,1) + 1 ) * 1.D0**2 )
        ELSE IF ( U.DIMSYS .EQ. 'MET' ) THEN ! Metric
          EVS(2,1) = 1.D0 /
     &     ( ( IEVS(3,1) - IEVS(2,1) + 1 ) * 1.609344D0**2 )
        ELSE IF ( U.DIMSYS .EQ. 'SI' ) THEN ! SI
          EVS(2,1) = 1.D0 /
     &     ( ( IEVS(3,1) - IEVS(2,1) + 1 ) * .44704D0**2 )
        END IF
      END IF
      EVS(3,1) = EVS(2,1) / M1

      IF ( M2 .NE. 0 ) THEN
        IEVS(1,2) = M1 + 1
        IEVS(3,2) = IEVS(3,1)
        IEVS(2,2) = IEVS(2,1)
        EVS1 = 0.D0
        DO J = M1+1, M
          EVS1 = EVS1 + V0(J)
        END DO
        EVS(1,2) = EVS1 / M2
        EVS(2,2) = EVS(2,1)
        EVS(3,2) = EVS(2,2) / M2
      END IF

      ! Set default momentum error
      IEM(3) = MAX( NLPT - ITUMAX, -1 )
      IEM(2) = MAX( IEM(3) - NINT( .02D0 / DT ), 0 )
      IF ( ( IEM(3) .LT. IEM(2) ) .OR.
     & ( ( JL .EQ. 1 ) .AND. ( M2 .EQ. 0 ) ) ) THEN
        EM(2) = 0.D0
      ELSE
        IF ( U.DIMSYS .EQ. 'ENG' ) THEN ! English
          EM(2) = 1.D0 /
     &     ( ( IEM(3) - IEM(2) + 1 ) * 1.D0**2 )
        ELSE IF ( U.DIMSYS .EQ. 'MET' ) THEN ! Metric
          EM(2) = 1.D0 /
     &     ( ( IEM(3) - IEM(2) + 1 ) * 1.609344D0**2 )
        ELSE IF ( U.DIMSYS .EQ. 'SI' ) THEN ! SI
          EM(2) = 1.D0 /
     &     ( ( IEM(3) - IEM(2) + 1 ) * .44704D0**2 )
        END IF
      END IF
      EM(1) = EM(2) * DTAV

      ! Read error information
      READ( INP_UNIT, 916, END=194 )
  116 READ( INP_UNIT, 917, END=194 ) DEF, ETYP
      BACKSPACE INP_UNIT
      CALL STR_UP( DEF )
      CALL STR_UP( ETYP )

      IF ( ( DEF .EQ. 'S' ) .OR. ( DEF .EQ. 'SD' ) .OR.
     & ( DEF .EQ. 'SS' ) .OR. ( DEF .EQ. 'SE' ) ) THEN
        ! Suppressed error specification
        READ( INP_UNIT, 916 )
        GO TO 116
      ELSE IF ( ETYP .EQ. 'AC' ) THEN ! Acceleration error specified
        READ( INP_UNIT, 918, ERR=191 ) J, TL, TU, VAL, VALR
        IF ( ( J .LT. JL ) .OR. ( J .GT. M ) ) THEN
          WRITE( *, 989 )
          STOP ' '
        END IF
        VAL = CFAC(J) * VAL
        VALR = CFAC(J) * VALR
        IF ( DEF .EQ. ' ' ) THEN
          IF ( NEA .GE. 100 ) THEN
            WRITE( *, 973 ) 'AC'
            GO TO 116
          END IF
          IF ( VALR .EQ. 0.D0 ) VALR = 2.D0
          NEA = NEA + 1
          IEA(1,NEA) = J
          IEA(2,NEA) = MAX( NINT(TL/DT), NFP_V )
          IEA(3,NEA) = MIN( NINT(TU/DT), NLP_V )
          IE = NEA
        ELSE IF ( ( DEF .EQ. 'DS' ) .OR. ( DEF .EQ. 'DE' ) ) THEN
          IF ( DEF .EQ. 'DS' ) THEN
            IE = 2 * ( J - JL ) + 1
            IF ( VALR .EQ. 0.D0 ) VALR = 2.D0
          ELSE
            IE = 2 * ( J - JL ) + 2
            IF ( VALR .EQ. 0.D0 ) VALR = 3.D0
          END IF
          IF ( ( TL .NE. 0.D0 ) .OR. ( TU .NE. 0.D0 ) ) THEN
            IEA(2,IE) = MAX( NINT( TL / DT ), NFP_V )
            IEA(3,IE) = MIN( NINT( TU / DT ), NLP_V )
          END IF
        ELSE
          GO TO 116
        END IF

        EA(1,IE) = VAL
        IF ( IEA(3,IE) .LT. IEA(2,IE) ) THEN
          EA(2,IE) = 0.D0
        ELSE
          CALL RMSMIN( IEA(2,IE), IEA(3,IE), A(NFP_V-1000,J),
     &     NFP_V, NLP_V, RMSR )
          EA(2,IE) = 1.D0 /
     &     ( ( IEA(3,IE) - IEA(2,IE) + 1 ) * ( VALR + RMSR )**2 )
        END IF
        EA(3,IE) = EA(1,IE) * EA(2,IE)
        GO TO 116
      ELSE IF ( ETYP .EQ. 'VC' ) THEN ! Velocity error specified
        READ( INP_UNIT, 918, ERR=191 ) J, TL, TU, VAL, VALR
        IF ( ( J .LT. 1 ) .OR. ( J .GT. M ) ) THEN
          WRITE( *, 989 )
          STOP ' '
        END IF
        VAL = CFAC(J) * VAL
        VALR = CFAC(J) * VALR
        IF ( VALR .EQ. 0.D0 ) THEN
          IF ( U.DIMSYS .EQ. 'ENG' ) THEN ! English
            VALR = 1.D0
          ELSE IF ( U.DIMSYS .EQ. 'MET' ) THEN ! Metric
            VALR = 1.609344D0
          ELSE IF ( U.DIMSYS .EQ. 'SI' ) THEN ! SI
            VALR = .44704D0
          END IF
        END IF
        IF ( NEV .GE. 100 ) THEN
          WRITE( *, 973 ) 'VC'
          GO TO 116
        END IF
        NEV = NEV + 1
        IEV(1,NEV) = J
        IEV(2,NEV) = MAX( NINT( TL / DT ), NFP_V )
        IEV(3,NEV) = MIN( NINT( TU / DT ), NLP_V )
        EV(1,NEV) = VAL
        IF ( IEV(3,NEV) .LT. IEV(2,NEV) ) THEN
          EV(2,NEV) = 0.D0
        ELSE
          EV(2,NEV) = 1.D0 /
     &     ( ( IEV(3,NEV) - IEV(2,NEV) + 1 ) * VALR**2 )
        END IF
        EV(3,NEV) = EV(2,NEV) * ( EV(1,NEV) - V0(J) )
        GO TO 116
      ELSE IF ( ETYP .EQ. 'DC' ) THEN ! Displacement error specified
        READ( INP_UNIT, 918, ERR=191 ) J, TL, TU, VAL, VALR
        IF ( ( J .LT. 1 ) .OR. ( J .GT. M ) ) THEN
          WRITE( *, 989 )
          STOP ' '
        END IF
        VAL = CFAC(J) * VAL
        VALR = CFAC(J) * VALR
        IF ( VALR .EQ. 0.D0 ) THEN
          IF ( U.DIMSYS .EQ. 'ENG' ) THEN ! English
            VALR = 1.D0
          ELSE IF ( U.DIMSYS .EQ. 'MET' ) THEN ! Metric
            VALR = 25.4D0
          ELSE IF ( U.DIMSYS .EQ. 'SI' ) THEN ! SI
            VALR = .0254D0
          END IF
        END IF
        IF ( NED .GE. 100 ) THEN
          WRITE( *, 973 ) 'DC'
          GO TO 116
        END IF
        NED = NED + 1
        IED(1,NED) = J
        IED(2,NED) = MAX( NINT( TL / DT ), NFP_V )
        IED(3,NED) = MIN( NINT( TU / DT ), NLP_V )
        ED(1,NED) = VAL
        IF ( IED(3,NED) .LT. IED(2,NED) ) THEN
          ED(2,NED) = 0.D0
        ELSE
          ED(2,NED) = 1.D0 /
     &     ( ( IED(3,NED) - IED(2,NED) + 1 ) * VALR**2 )
        END IF
        ED(3,NED) = -.5D0 * ED(2,NED) * DTAD
        GO TO 116
      ELSE IF ( ETYP .EQ. 'DR' ) THEN ! Rel displacement error specified
        READ( INP_UNIT, 919, ERR=191 ) J1, J2, TL, TU, VAL, VALR
        IF ( ( J1 .LT. 1 ) .OR. ( J1 .GT. M ) .OR.
     &   ( J2 .LT. 1 ) .OR. ( J2 .GT. M ) ) THEN
          WRITE( *, 989 )
          STOP ' '
        END IF
        VAL = CFAC(J1) * VAL
        IF ( VALR .EQ. 0.D0 ) THEN
          IF ( U.DIMSYS .EQ. 'ENG' ) THEN ! English
            VALR = 1.D0
          ELSE IF ( U.DIMSYS .EQ. 'MET' ) THEN ! Metric
            VALR = 25.4D0
          ELSE IF ( U.DIMSYS .EQ. 'SI' ) THEN ! SI
            VALR = .0254D0
          END IF
        END IF
        IF ( NER .GE. 100 ) THEN
          WRITE( *, 973 ) 'DR'
          GO TO 116
        END IF
        NER = NER + 1
        IF ( J1 .GT. J2 ) THEN
          IER(1,NER) = J2
          IER(4,NER) = J1
          VAL = -VAL
        ELSE
          IER(1,NER) = J1
          IER(4,NER) = J2
        END IF
        IF ( ( TL .NE. 0.D0 ) .OR. ( TU .NE. 0.D0 ) ) THEN
          IER(2,NER) = MAX( NINT( TL / DT ), NFP_V )
          IER(3,NER) = MIN( NINT( TU / DT ), NLP_V )
        ELSE
          IER(3,NER) = MAX( NLPT - ITUMAX, -1 )
          IER(2,NER) = MAX( IER(3,NER), 0 )
        END IF
        ER(1,NER) = VAL
        IF ( IER(3,NER) .LT. IER(2,NER) ) THEN
          ER(2,NER) = 0.D0
        ELSE
          ER(2,NER) = 1.D0 /
     &     ( ( IER(3,NER) - IER(2,NER) + 1 ) * VALR**2 )
        END IF
        ER(3,NER) = -.5D0 * ER(2,NER) * DTAD
        GO TO 116
      ELSE IF ( ETYP .EQ. 'VS' ) THEN ! Velocity scatter error specified
        READ( INP_UNIT, 918, ERR=191 ) IV, TL, TU, VAL, VALR
        IF ( ( IV .EQ. 0 ) .AND. ( M2 .EQ. 0 ) ) IV = 1
        IF ( ( IV .LT. 1 ) .OR. ( IV .GT. 2 ) .OR.
     &   ( IV .GT. M2 + 1 ) ) THEN
          WRITE( *, 990 )
          STOP ' '
        END IF
        IF ( VALR .EQ. 0.D0 ) THEN
          IF ( U.DIMSYS .EQ. 'ENG' ) THEN ! English
            VALR = 1.D0
          ELSE IF ( U.DIMSYS .EQ. 'MET' ) THEN ! Metric
            VALR = 1.609344D0
          ELSE IF ( U.DIMSYS .EQ. 'SI' ) THEN ! SI
            VALR = .44704D0
          END IF
        END IF
        IF ( IV .EQ. 1 ) THEN
          MV = M1
        ELSE
          MV = M2
        END IF
        IF ( ( TL .NE. 0.D0 ) .OR. ( TU .NE. 0.D0 ) ) THEN
          IEVS(2,IV) = MAX( NINT( TL / DT ), NFP_V )
          IEVS(3,IV) = MIN( NINT( TU / DT ), NLP_V )
        END IF
        IF ( IEVS(3,IV) .LT. IEVS(2,IV) ) THEN
          EVS(2,IV) = 0.D0
        ELSE
          EVS(2,IV) = 1.D0 /
     &     ( ( IEVS(3,IV) - IEVS(2,IV) + 1 ) * VALR**2 )
        END IF
        EVS(3,IV) = EVS(2,IV) / MV
        GO TO 116
      ELSE IF ( ETYP .EQ. 'MO' ) THEN ! Momentum error specified
        READ( INP_UNIT, 918, ERR=191 ) J, TL, TU, VAL, VALR
        IF ( ( JL .EQ. 1 ) .AND. ( M2 .EQ. 0 ) ) WRITE( *, 974 )
        IF ( VALR .EQ. 0.D0 ) THEN
          IF ( U.DIMSYS .EQ. 'ENG' ) THEN ! English
            VALR = 1.D0
          ELSE IF ( U.DIMSYS .EQ. 'MET' ) THEN ! Metric
            VALR = 1.609344D0
          ELSE IF ( U.DIMSYS .EQ. 'SI' ) THEN ! SI
            VALR = .44704D0
          END IF
        END IF
        IF ( ( TL .NE. 0.D0 ) .OR. ( TU .NE. 0.D0 ) ) THEN
          IEM(2) = MAX( NINT( TL / DT ), NFP_V )
          IEM(3) = MIN( NINT( TU / DT ), NLP_V )
        END IF
        IF ( IEM(3) .LT. IEM(2) ) THEN
          EM(2) = 0.D0
        ELSE
          EM(2) = 1.D0 / ( ( IEM(3) - IEM(2) + 1 ) * VALR**2 )
        END IF
        EM(1) = EM(2) * DTAV
        GO TO 116
      ELSE IF ( ETYP .EQ. 'TR' ) THEN ! Rel timing constraint specified
        READ( INP_UNIT, 920, ERR=191 ) ( JT(I), I = 1, 21 )
        DO I = 1, 21
          IF ( ( JT(I) .LT. 0 ) .OR. ( JT(I) .GT. M ) ) THEN
            WRITE( *, 989 )
            STOP ' '
          END IF
          IF ( I .GT. 1 ) THEN
            IF ( JT(I) .NE. 0 ) THEN
              IF ( NTC .GE. 100 ) THEN
                WRITE( *, 973 ) 'TR'
                GO TO 116
              END IF
              NTC = NTC + 1
              ITCON(1,NTC) = JT(I-1)
              ITCON(2,NTC) = JT(I)
            ELSE
              GO TO 116
            END IF
          END IF
        END DO
        GO TO 116
      END IF

      ! Set timing constraints
      CALL TCON( JL, M, A, NFPT, NLPT, NFP_V, NLP_V, DT,
     & ITL, ITU, ITCON, NTC )

      ! Read solution information
      READ( INP_UNIT, 921, ERR=191, END=194 ) NGA
      NGA = ABS( NGA )
      IF ( NGA .EQ. 0 ) THEN
        NGA = NINT( 100.D0**( 1.D0 / ( M - JL + 1 ) ) )
      END IF
      NGMAX = 0
      DO J = JL, M
        NGMAX = MAX( NGMAX, ITU(J) - ITL(J) + 1 )
      END DO
      NGA = MIN( NGA, NGMAX )

      ! Read output information
      READ( INP_UNIT, 922, END=194 ) OUT, OUT_FILE
      CLOSE( INP_UNIT )
      CALL STR_UP( OUT )

      RETURN

      ! Input file read error handler
  191 WRITE( *, 991 )
      STOP ' '

      ! UDS read error handler
  193 WRITE( *, 993 )
      STOP ' '

      ! Early input file termination handler
  194 WRITE( *, 994 )
      STOP ' '

      ! EOF to input file request handler
  195 STOP ' '

  901 FORMAT(/' VeCor input file name?'/' >> ',$)
  902 FORMAT(/1X,80('-')//1X,5X,A70///1X,'Run ID Code : ',A10//)
  903 FORMAT(//1X,33X,'PULSE  SUMMARY'/1X,32X,16('-')/)
  904 FORMAT(/' Pulse',5X,'Description',29X,'File Name'/
     & 1X,5('-'),5X,30('-'),10X,30('-'))
  905 FORMAT(1X,1X,I2,7X,A30,10X,A)

  911 FORMAT(A80)
  912 FORMAT(/A10,A70//)
  913 FORMAT(I5)
  914 FORMAT(9X,A1,7F10.0)
  915 FORMAT(BN,E15.7)
  916 FORMAT()
  917 FORMAT(A2,1X,A2)
  918 FORMAT(5X,I5,4F10.0)
  919 FORMAT(6X,I2,I2,4F10.0)
  920 FORMAT(5X,21I5)
  921 FORMAT(/I5)
  922 FORMAT(/3X,A2,A75)

  971 FORMAT(/' *** WARNING: UDS data has been filtered'/)
  972 FORMAT(/' *** WARNING: Different INIVEL values for same ',
     & 'vehicle'/)
  973 FORMAT(/' *** WARNING: Surplus ',A2,' errors ignored'/)
  974 FORMAT(/' *** WARNING: Momentum error specified for ',
     & 'system with one vehicle and no external force'/)

  981 FORMAT(/' *** ERROR: Input file open failed, try again'/)
  982 FORMAT(/' *** ERROR: UDS file data not in time series form'/)
  983 FORMAT(/' *** ERROR: Pulse 0 is not FORCE'/)
  984 FORMAT(/' *** ERROR: Pulse ',I2,' is not ACCELERATION in G''S'/)
  985 FORMAT(/' *** ERROR: Incorrect or inconsistent VEHNO value'/)
  987 FORMAT(/' *** ERROR: Inconsistent VEHTWT value'/)
  988 FORMAT(/' *** ERROR: Incorrect pulse numbering'/)
  989 FORMAT(/' *** ERROR: Incorrect pulse number in error ',
     & 'specification'/)
  990 FORMAT(/' *** ERROR: Incorrect vehicle number in error ',
     & 'specification'/)
  991 FORMAT(/' *** ERROR: Input file read error occurred'/)
  993 FORMAT(/' *** ERROR: UDS file read failed')
  994 FORMAT(/' *** ERROR: VeCor input file terminated early'/)

      END



      SUBROUTINE RMSMIN( IL, IU, A, NFP_V, NLP_V, RMSR )

!***********************************************************************
!* Computes a Minimum RMS Value Over All Bias Shifts
!***********************************************************************

      INTEGER IL, IU, NFP_V, NLP_V, I

      REAL A(NFP_V-1000:NLP_V+1000)

      DOUBLE PRECISION B, AM, AI, E, EM, EI, RMSR


      RMSR = 0.D0
      IF ( IU .LE. IL) RETURN

      ! Find average value (= min. RMS bias value)
      B = 0.D0
      AM = A(IL)

      DO I = IL+1, IU
        AI = A(I)
        B = B + ( ( AI + AM ) / 2.D0 )
        AM = AI
      END DO
      B = B / ( IU - IL + 1 )

      ! Compute the RMS value
      E = 0.D0
      EM = A(IL)**2

      DO I = IL+1, IU
        EI = A(I)**2
        E = E + ( ( EI + EM ) / 2.D0 )
        EM = EI
      END DO

      RMSR = SQRT( E / ( IU - IL + 1 ) - B**2 ) * 1.5D0

      RETURN
      END



      SUBROUTINE TCON( JL, M, A, NFPT, NLPT, NFP_V, NLP_V, DT,
     & ITL, ITU, ITCON, NTC )

!***********************************************************************
!* Estimates the Initial Response Time of Each Pulse and Sets the
!* Time Shift Constraints
!***********************************************************************

      INTEGER JL, M, NFPT, NLPT, NFP_V, NLP_V, ITL(0:20), ITU(0:20),
     & ITCON(3,100), NTC, I, IE, J, J1, J2, K, ISTR(0:20), NAV, NK

      REAL A(NFP_V-1000:NLP_V+1000,0:20)

      DOUBLE PRECISION DT, VEL, VEL_MIN, VEL_MAX, VEL_K, DELV_MAX


      ! Find the initial response times
      DO J = JL, M

        VEL_MIN = 0.D0
        VEL_MAX = 0.D0
        VEL = 0.D0
        DO I = NFPT, NLPT-1
          VEL = VEL + ( A(I,J) + A(I+1,J) ) / 2.D0
          VEL_MIN = MIN( VEL_MIN, VEL )
          VEL_MAX = MAX( VEL_MAX, VEL )
        END DO
        IF ( ABS( VEL_MAX ) .GT. ABS( VEL_MIN ) ) THEN
          DELV_MAX = VEL_MAX
        ELSE
          DELV_MAX = VEL_MIN
        END IF

        NAV = MAX( NINT( .003D0 / DT ), 1 )
        VEL = 0.D0
        DO I = NFPT, NLPT-1
          NK = 0
          VEL_K = 0.D0
          DO K = MAX( I - NAV, NFPT ), MIN( I + NAV, NLPT )
            VEL_K = VEL_K + A(K,J)
            NK = NK + 1
          END DO
          VEL = VEL + ( VEL_K / NK )
          IF ( ( VEL / DELV_MAX ) .GT. ( 1.D0 / 30.D0 ) ) THEN
            ISTR(J) = I
            GO TO 103
          END IF
        END DO
        ISTR(J) = NLPT
  103   CONTINUE
      END DO

      ! Set time shift limits
      DO J = JL, M
        ITU(J) = MIN( ITU(J), ISTR(J) - NINT( .001D0 / DT ) )
        IF ( ITU(J) .LT. ITL(J) ) THEN
          WRITE( *, 991 ) J
          STOP ' '
        END IF
      END DO

      ! Set relative time shift constraints
      DO IE = 1, NTC
        J1 = ITCON(1,IE)
        J2 = ITCON(2,IE)
        ITCON(3,IE) = ISTR(J1) - ISTR(J2)
        IF ( ITU(J1) - ITL(J2) .LT. ITCON(3,IE) ) THEN
          WRITE( *, 992 ) J1, J2
          STOP ' '
        END IF
      END DO

      RETURN

  991 FORMAT(/' *** ERROR: Pulse ',I2,' time shift limits are ',
     & 'insufficient'/)
  992 FORMAT(/' *** ERROR: Pulses ',I2,' and ',I2,' time shift ',
     & 'limits are insufficient'/)

      END



      SUBROUTINE INTEG( DTAV, DTVD, DTAD, V0, A, V, D, NFP_V, NLP_V )

!***********************************************************************
!* Computes First and Second Integrals of the Input Data
!***********************************************************************

      INTEGER NFP_V, NLP_V, I

      REAL A(NFP_V-1000:NLP_V+1000), V(NFP_V-1000:NLP_V+1000),
     & D(NFP_V-1000:NLP_V+1000)

      DOUBLE PRECISION DTAV, DTVD, DTAD, V0, DTAVH, DTAD6,
     & AI, AM, VI, VM, DI, DM


      DTAVH = DTAV / 2.D0
      DTAD6 = DTAD / 6.D0

      V(0) = REAL( V0 )
      D(0) = 0.D0

      ! Perform post-zero integration
      AM = A(0)
      VM = V0
      DM = 0.D0

      DO I = 1, NLP_V + 1000
        AI = A(I)
        VI = VM + DTAVH * ( AI + AM )
        DI = DM + DTVD * VM + DTAD6 * ( AI + 2.D0 * AM )

        AM = AI
        VM = VI
        DM = DI

        V(I) = REAL( VI )
        D(I) = REAL( DI )
      END DO

      ! Perform pre-zero integration
      AM = A(0)
      VM = V0
      DM = 0.D0

      DO I = -1, NFP_V - 1000, -1
        AI = A(I)
        VI = VM - DTAVH * ( AI + AM )
        DI = DM - DTVD * VM + DTAD6 * ( AI + 2.D0 * AM )

        AM = AI
        VM = VI
        DM = DI

        V(I) = REAL( VI )
        D(I) = REAL( DI )
      END DO

      RETURN
      END



      SUBROUTINE GRINVAR( JL, M, M1, M2, WT, V, D, NFP_V, NLP_V,
     & DTAV, DTVD, DTAD, ES, EB, EA, IEA, NEA, EV, IEV, NEV,
     & ED, IED, NED, ER, IER, NER, EVS, IEVS, EM, IEM, GI, RI )

!***********************************************************************
!* Computes the Invariant Contributions to the G Matrix and R Vector
!***********************************************************************

      INTEGER JL, M, M1, M2, NFP_V, NLP_V,
     & IEA(3,100), NEA, IEV(3,100), NEV, IED(3,100), NED,
     & IER(4,100), NER, IEVS(3,2), IEM(3)

      INTEGER I, I1, I2, IE, II, ISUM, J, J1, J2, K, L, N

      REAL V(NFP_V-1000:NLP_V+1000,0:20),
     & D(NFP_V-1000:NLP_V+1000,0:20)

      DOUBLE PRECISION WT(0:20), DTAV, DTVD, DTAD, ES(2,0:20),
     & EB(2,0:20), EA(3,100), EV(3,100), ED(3,100), ER(3,100),
     & EVS(3,2), EM(2), GI(42,42), RI(42), SQR, CUB, QUAR, GBB, RB,
     & VLSUM, GBBM, WFAC, WTFAC

      ! Scaling and bias error contributions
      DO J = JL, M
        I = 2 * J + 1
        GI(I,I) = GI(I,I) + ES(2,J)
        RI(I) = RI(I) + ES(1,J) * ES(2,J)
        I = I + 1
        GI(I,I) = GI(I,I) + EB(2,J)
        RI(I) = RI(I) + EB(1,J) * EB(2,J)
      END DO

      ! Acceleration error contributions
      DO IE = 1, NEA
        N = MAX( IEA(3,IE) - IEA(2,IE) + 1, 0 )
        I = 2 * IEA(1,IE) + 2
        GI(I,I) = GI(I,I) + EA(2,IE) * N
        RI(I) = RI(I) - EA(3,IE) * N
      END DO

      ! Velocity error contributions
      DO IE = 1, NEV
        ISUM = 0
        SQR = 0.D0
        DO II = IEV(2,IE), IEV(3,IE)
          ISUM = ISUM + II
          SQR = SQR + II**2
        END DO
        I = 2 * IEV(1,IE) + 2
        GI(I,I) = GI(I,I) + EV(2,IE) * DTAV**2 * SQR
        RI(I) = RI(I) - EV(3,IE) * DTAV * ISUM
      END DO

      ! Displacement error contributions
      DO IE = 1, NED
        J = IED(1,IE)
        SQR = 0.D0
        CUB = 0.D0
        QUAR = 0.D0
        DO II = IED(2,IE), IED(3,IE)
          SQR = SQR + II**2
          CUB = CUB + DBLE(II)**3
          QUAR = QUAR + DBLE(II)**4
        END DO
        I = 2 * J + 2
        GI(I,I) = GI(I,I) - DTAD * ED(3,IE) * QUAR / 2.D0
        RI(I) = RI(I) - ED(3,IE) *
     &   ( ( D(0,J) - ED(1,IE) ) * SQR + DTVD * V(0,J) * CUB )
      END DO

      ! Relative displacement error contributions
      DO IE = 1, NER
        J1 = IER(1,IE)
        J2 = IER(4,IE)
        SQR = 0.D0
        CUB = 0.D0
        QUAR = 0.D0
        DO II = IER(2,IE), IER(3,IE)
          SQR = SQR + II**2
          CUB = CUB + DBLE(II)**3
          QUAR = QUAR + DBLE(II)**4
        END DO
        GBB = DTAD * ER(3,IE) * QUAR / 2.D0
        I1 = 2 * J1 + 2
        I2 = 2 * J2 + 2
        GI(I1,I1) = GI(I1,I1) - GBB
        GI(I1,I2) = GI(I1,I2) + GBB
        GI(I2,I2) = GI(I2,I2) - GBB
        RB = ER(3,IE) * ( ( D(0,J1) - D(0,J2) - ER(1,IE) ) * SQR +
     &   DTVD * ( V(0,J1) - V(0,J2) ) * CUB )
        RI(I1) = RI(I1) - RB
        RI(I2) = RI(I2) + RB
      END DO

      ! Velocity scatter error contributions
      VLSUM = EVS(1,1)
      RB = EVS(2,1) * DTAV
      ISUM = 0
      SQR = 0.D0
      DO II = IEVS(2,1), IEVS(3,1)
        ISUM = ISUM + II
        SQR = SQR + II**2
      END DO
      GBB = RB * DTAV * SQR
      GBBM = GBB / M1
      DO J = 1, M1
        I = 2 * J + 2
        GI(I,I) = GI(I,I) + GBB
        DO K = I, 2 * M1 + 2, 2
          GI(I,K) = GI(I,K) - GBBM
        END DO
        RI(I) = RI(I) + RB * ( V(0,J) - VLSUM ) * ISUM
      END DO

      IF ( M2 .EQ. 0) GO TO 115
      VLSUM = EVS(1,2)
      RB = EVS(2,2) * DTAV
      J1 = IEVS(1,2)
      ISUM = 0
      SQR = 0.D0
      DO II = IEVS(2,2), IEVS(3,2)
        ISUM = ISUM + II
        SQR = SQR + II**2
      END DO
      GBB = RB * DTAV * SQR
      GBBM = GBB / M2
      DO J = J1, M
        I = 2 * J + 2
        GI(I,I) = GI(I,I) + GBB
        DO K = I, 2 * M + 2, 2
          GI(I,K) = GI(I,K) - GBBM
        END DO
        RI(I) = RI(I) + RB * ( V(0,J) - VLSUM ) * ISUM
      END DO

      ! Momentum error contributions
  115 SQR = 0.D0
      DO II = IEM(2), IEM(3)
        SQR = SQR + II**2
      END DO
      WFAC = EM(1) * DTAV * SQR
      DO J = JL, M
        WTFAC = WFAC * WT(J)
        I = 2 * J + 2
        DO L = J, M
          K = 2 * L + 2
          GI(I,K) = GI(I,K) + WT(L) * WTFAC
        END DO
      END DO

      RETURN
      END



      SUBROUTINE SEARCH( JL, M, M1, M2, WT, A, V, D, NFP_V, NLP_V,
     & DT, DTAV, DTVD, DTAD, GI, RI, ITL, ITU, NGA, ES, EB, ET, EA,
     & IEA, NEA, EV, IEV, NEV, ED, IED, NED, ER, IER, NER, EVS, IEVS,
     & EM, IEM, ITCON, NTC, ITMIN, XMIN, ERMIN )

!***********************************************************************
!* Performs the Grid/Gradient Search of the Time Shift Domain
!***********************************************************************

      INTEGER JL, M, M1, M2, NFP_V, NLP_V,
     & ITL(0:20), ITU(0:20), NGA,
     & IEA(3,100), NEA, IEV(3,100), NEV, IED(3,100), NED,
     & IER(4,100), NER, IEVS(3,2), IEM(3), ITCON(3,100), NTC,
     & ITMIN(0:20)

      INTEGER I, IE, J, ND, NDL, ITROR(0:20), IND(0:20)

      REAL A(NFP_V-1000:NLP_V+1000,0:20),
     & V(NFP_V-1000:NLP_V+1000,0:20),
     & D(NFP_V-1000:NLP_V+1000,0:20)

      DOUBLE PRECISION WT(0:20), DT, DTAV, DTVD, DTAD, GI(42,42),
     & RI(42), ES(2,0:20), EB(2,0:20), ET(2,0:20),
     & EA(3,100), EV(3,100), ED(3,100), ER(3,100), EVS(3,2),
     & EM(2), TINC(0:20), XMIN(42), ERMIN, XROR(42), ERROR

      SAVE IND

      DATA IND/21*1/


      ! Initializations
      NDL = 2 * JL + 1
      ND = 2 * M + 2

      ! Search from the smallest time shift point
      DO J = JL, M
        ITMIN(J) = MIN( MAX( ITMIN(J), ITL(J) ), ITU(J) )
      END DO

      CALL MOVE( JL, M, M1, M2, WT, NDL, ND, A, V, D, NFP_V, NLP_V,
     & DT, DTAV, DTVD, DTAD, GI, RI, ITL, ITU, ITMIN,
     & ES, EB, ET, EA, IEA, NEA, EV, IEV, NEV, ED, IED, NED,
     & ER, IER, NER, EVS, IEVS, EM, IEM, ITCON, NTC, XMIN, ERMIN )

      IF ( NGA .EQ. 1 ) RETURN

      ! Perform the uniform grid search
      DO J = JL, M
        TINC(J) = MAX( ( ITU(J) - ITL(J) ) / DBLE( NGA + 1 ), DT )
      END DO

  103 DO J = JL, M
        ITROR(J) = NINT( ITL(J) + IND(J) * TINC(J) )
        IF ( ITROR(J) .GT. ITU(J) ) THEN
          IND(J) = NGA
          GO TO 108
        END IF
      END DO

      DO IE = 1, NTC
        IF ( ITROR(ITCON(1,IE)) - ITROR(ITCON(2,IE)) .LT.
     &     ITCON(3,IE) ) GO TO 108
      END DO

      CALL MOVE( JL, M, M1, M2, WT, NDL, ND, A, V, D, NFP_V, NLP_V,
     & DT, DTAV, DTVD, DTAD, GI, RI, ITL, ITU, ITROR,
     & ES, EB, ET, EA, IEA, NEA, EV, IEV, NEV, ED, IED, NED,
     & ER, IER, NER, EVS, IEVS, EM, IEM, ITCON, NTC, XROR, ERROR )

      IF ( ERROR .LT. ERMIN ) THEN
        ERMIN = ERROR
        DO J = JL, M
          ITMIN(J) = ITROR(J)
        END DO
        DO I = NDL, ND
          XMIN(I) = XROR(I)
        END DO
      END IF

      ! Set next grid point using "odometer" logic
  108 J = JL
  109 IF ( IND(J) .LT. NGA ) THEN
        IND(J) = IND(J) + 1
        GO TO 103
      ELSE IF ( J .LT. M ) THEN
        IND(J) = 1
        J = J + 1
        GO TO 109
      END IF

      RETURN
      END



      SUBROUTINE MOVE( JL, M, M1, M2, WT, NDL, ND, A, V, D,
     & NFP_V, NLP_V, DT, DTAV, DTVD, DTAD, GI, RI, ITL, ITU,
     & ITROR, ES, EB, ET, EA, IEA, NEA, EV, IEV, NEV,
     & ED, IED, NED, ER, IER, NER, EVS, IEVS, EM, IEM,
     & ITCON, NTC, XROR, ERROR )

!***********************************************************************
!* Performs Local Gradient Search from a Given Time Shift Point
!***********************************************************************

      INTEGER JL, M, M1, M2, NDL, ND, NFP_V, NLP_V,
     & ITL(0:20), ITU(0:20), ITROR(0:20),
     & IEA(3,100), NEA, IEV(3,100), NEV, IED(3,100), NED,
     & IER(4,100), NER, IEVS(3,2), IEM(3), ITCON(3,100), NTC

      INTEGER I, IFLAG, IMOVE, INC, ITNEW, ITRJ,
     & J, JDIR, JNUM, JSIGN, IGRO(0:20)

      REAL A(NFP_V-1000:NLP_V+1000,0:20),
     & V(NFP_V-1000:NLP_V+1000,0:20),
     & D(NFP_V-1000:NLP_V+1000,0:20)

      DOUBLE PRECISION WT(0:20), DT, DTAV, DTVD, DTAD, GI(42,42),
     & RI(42), ES(2,0:20), EB(2,0:20), ET(2,0:20),
     & EA(3,100), EV(3,100), ED(3,100), ER(3,100), EVS(3,2),
     & EM(2), G(42,42), R(42), XROR(42), ERROR, X(42), ERNEW,
     & GRE(0:20)


      ! Evaluate error and estimate the gradient at the given solution
      CALL GRCOMP( JL, M, M1, M2, WT, NDL, ND, A, V, D,
     & NFP_V, NLP_V, DTAV, DTVD, GI, RI, ITROR,
     & EA, IEA, NEA, EV, IEV, NEV, ED, IED, NED,
     & ER, IER, NER, EVS, IEVS, EM, IEM, G, R )

      CALL SOLVE( NDL, ND, G, R, XROR )

      CALL EVAL( JL, M, M1, M2, WT, A, V, D, NFP_V, NLP_V,
     & DT, DTAV, DTVD, DTAD, ITROR, XROR,
     & ES, EB, ET, EA, IEA, NEA, EV, IEV, NEV, ED, IED, NED,
     & ER, IER, NER, EVS, IEVS, EM, IEM, ITCON, NTC, ERROR )

  101 DO J = JL, M
        ITRJ = ITROR(J)
        IF ( ITRJ .LT. ITU(J) ) THEN
          INC = 1
        ELSE IF ( ITRJ .GT. ITL(J) ) THEN
          INC = -1
        ELSE
          GRE(J) = 0.D0
          GO TO 102
        END IF
        ITROR(J) = ITRJ + INC

        CALL GRCOMP( JL, M, M1, M2, WT, NDL, ND, A, V, D,
     &   NFP_V, NLP_V, DTAV, DTVD, GI, RI, ITROR,
     &   EA, IEA, NEA, EV, IEV, NEV, ED, IED, NED,
     &   ER, IER, NER, EVS, IEVS, EM, IEM, G, R )

        CALL SOLVE( NDL, ND, G, R, X )

        CALL EVAL( JL, M, M1, M2, WT, A, V, D, NFP_V, NLP_V,
     &   DT, DTAV, DTVD, DTAD, ITROR, X,
     &   ES, EB, ET, EA, IEA, NEA, EV, IEV, NEV, ED, IED, NED,
     &   ER, IER, NER, EVS, IEVS, EM, IEM, ITCON, NTC, ERNEW )

        GRE(J) = INC * ( ERNEW - ERROR )

        ITROR(J) = ITRJ
  102   CONTINUE
      END DO

      ! Order the gradient components by magnitude
      DO J = JL, M
        IGRO(J) = J
      END DO

  104 IFLAG = 0
      DO J = JL, M-1
        IF ( ABS( GRE(IGRO(J)) ) .LT. ABS( GRE(IGRO(J+1)) ) ) THEN
          I = IGRO(J)
          IGRO(J) = IGRO(J+1)
          IGRO(J+1) = I
          IFLAG = 1
        END IF
      END DO
      IF ( IFLAG .EQ. 1 ) GO TO 104

      ! Move down the steepest directions until no improvement
      IMOVE = 0
      JNUM = JL
  106 JDIR = IGRO(JNUM)
      JSIGN = INT( SIGN( 1.D0, GRE(JDIR) ) )
  107 ITNEW = ITROR(JDIR) - JSIGN


      IF ( ( ITNEW .GE. ITL(JDIR) ) .AND. ( ITNEW .LE. ITU(JDIR) ) )
     & THEN
        ITROR(JDIR) = ITNEW

        CALL GRCOMP( JL, M, M1, M2, WT, NDL, ND, A, V, D,
     &   NFP_V, NLP_V, DTAV, DTVD, GI, RI, ITROR,
     &   EA, IEA, NEA, EV, IEV, NEV, ED, IED, NED,
     &   ER, IER, NER, EVS, IEVS, EM, IEM, G, R )

        CALL SOLVE( NDL, ND, G, R, X )

        CALL EVAL( JL, M, M1, M2, WT, A, V, D, NFP_V, NLP_V,
     &   DT, DTAV, DTVD, DTAD, ITROR, X,
     &   ES, EB, ET, EA, IEA, NEA, EV, IEV, NEV, ED, IED, NED,
     &   ER, IER, NER, EVS, IEVS, EM, IEM, ITCON, NTC, ERNEW )

        IF ( ERNEW .LT. ERROR ) THEN
          ERROR = ERNEW
          DO I = NDL, ND
            XROR(I) = X(I)
          END DO
          IMOVE = 1
          GO TO 107
        ELSE
          ITROR(JDIR) = ITNEW + JSIGN
        END IF
      END IF

      IF ( JNUM .LT. M ) THEN
        JNUM = JNUM + 1
        GO TO 106
      ELSE IF ( IMOVE .EQ. 1 ) THEN
        GO TO 101
      END IF

      RETURN
      END



      SUBROUTINE GRCOMP( JL, M, M1, M2, WT, NDL, ND, A, V, D,
     & NFP_V, NLP_V, DTAV, DTVD, GI, RI, IT,
     & EA, IEA, NEA, EV, IEV, NEV, ED, IED, NED,
     & ER, IER, NER, EVS, IEVS, EM, IEM, G, R )

!***********************************************************************
!* Computes the Nonconstant Contributions to the G Matrix and R Vector
!***********************************************************************

      INTEGER JL, M, M1, M2, NDL, ND, NFP_V, NLP_V,
     & IT(0:20), IEA(3,100), NEA, IEV(3,100), NEV,
     & IED(3,100), NED, IER(4,100), NER, IEVS(3,2), IEM(3)

      INTEGER I, I1, I2, IE, II, IIL, IISQ, IIU, ITJ, ITJ1, ITJ2,
     & J, J1, J2, K, L

      REAL A(NFP_V-1000:NLP_V+1000,0:20),
     & V(NFP_V-1000:NLP_V+1000,0:20),
     & D(NFP_V-1000:NLP_V+1000,0:20)

      DOUBLE PRECISION WT(0:20), DTAV, DTVD, GI(42,42),
     & RI(42), EA(3,100), EV(3,100), ED(3,100), ER(3,100), EVS(3,2),
     & EM(2), G(42,42), R(42), GSS, GSB, W, RS, VIT, VDIF, V0T, DIT,
     & D0D, DDIF, VIT1, VIT2, DIT1, DIT2, GS1S1, GS2S2, GS1B1, GS1S2,
     & GS2B2, RS1, RS2, D1, D2, D3, WFAC, VLSUM, WM, WDT, WDTM,
     & VITJ(0:20), GSSL(0:20), GSBL(0:20), VDIFL, SFAC, BFAC, WTJ, WTL


      ! Initialize G and R with the invariant contributions
      DO K = NDL, ND
        DO I = NDL, K
          G(I,K) = GI(I,K)
        END DO
        R(K) = RI(K)
      END DO

      ! Set up velocity value array
      DO J = JL, M
        VITJ(J) = V(IT(J),J)
      END DO

      ! Acceleration error contributions
      DO IE = 1, NEA
        J = IEA(1,IE)
        ITJ = IT(J)
        GSS = 0.D0
        GSB = 0.D0
        DO II = IEA(2,IE) + ITJ, IEA(3,IE) + ITJ
          GSS = GSS + A(II,J)**2
          GSB = GSB + A(II,J)
        END DO
        W = EA(2,IE)
        I = 2 * J + 1
        G(I,I) = G(I,I) + W * GSS
        G(I,I+1) = G(I,I+1) - W * GSB
        R(I) = R(I) + EA(3,IE) * GSB
      END DO

      ! Velocity error contributions
      DO IE = 1, NEV
        J = IEV(1,IE)
        ITJ = IT(J)
        VIT = VITJ(J)
        GSS = 0.D0
        GSB = 0.D0
        RS = 0.D0
        DO II = IEV(2,IE), IEV(3,IE)
          VDIF = V(II+ITJ,J) - VIT
          GSS = GSS + VDIF**2
          GSB = GSB + VDIF * II
          RS = RS + VDIF
        END DO
        W = EV(2,IE)
        I = 2 * J + 1
        G(I,I) = G(I,I) + W * GSS
        G(I,I+1) = G(I,I+1) - W * DTAV * GSB
        R(I) = R(I) + EV(3,IE) * RS
      END DO

      ! Displacement error contributions
      DO IE = 1, NED
        J = IED(1,IE)
        ITJ = IT(J)
        VIT = VITJ(J) * DTVD
        V0T = V(0,J) * DTVD
        DIT = D(ITJ,J)
        D0D = D(0,J) - ED(1,IE)
        GSS = 0.D0
        GSB = 0.D0
        RS = 0.D0
        DO II = IED(2,IE), IED(3,IE)
          DDIF = D(II+ITJ,J) - DIT - VIT * II
          GSS = GSS + DDIF**2
          GSB = GSB + DDIF * II**2
          RS = RS + DDIF * ( D0D + V0T * II )
        END DO
        W = ED(2,IE)
        I = 2 * J + 1
        G(I,I) = G(I,I) + W * GSS
        G(I,I+1) = G(I,I+1) + ED(3,IE) * GSB
        R(I) = R(I) - W * RS
      END DO

      ! Relative displacement error contributions
      DO IE = 1, NER
        J1 = IER(1,IE)
        J2 = IER(4,IE)
        ITJ1 = IT(J1)
        ITJ2 = IT(J2)
        VIT1 = VITJ(J1) * DTVD
        VIT2 = VITJ(J2) * DTVD
        V0T = ( V(0,J1) - V(0,J2) ) * DTVD
        DIT1 = D(ITJ1,J1)
        DIT2 = D(ITJ2,J2)
        D0D = D(0,J1) - D(0,J2) - ER(1,IE)
        GS1S1 = 0.D0
        GS2S2 = 0.D0
        GS1B1 = 0.D0
        GS1S2 = 0.D0
        GS2B2 = 0.D0
        RS1 = 0.D0
        RS2 = 0.D0
        DO II = IER(2,IE), IER(3,IE)
          D1 = D(II+ITJ1,J1) - DIT1 - VIT1 * II
          D2 = D(II+ITJ2,J2) - DIT2 - VIT2 * II
          D3 = D0D + V0T * II
          GS1S1 = GS1S1 + D1**2
          GS2S2 = GS2S2 + D2**2
          IISQ = II**2
          GS1B1 = GS1B1 + D1 * IISQ
          GS1S2 = GS1S2 + D1 * D2
          GS2B2 = GS2B2 + D2 * IISQ
          RS1 = RS1 + D1 * D3
          RS2 = RS2 + D2 * D3
        END DO
        W = ER(2,IE)
        I1 = 2 * J1 + 1
        I2 = 2 * J2 + 1
        G(I1,I1) = G(I1,I1) + W * GS1S1
        G(I2,I2) = G(I2,I2) + W * GS2S2
        WFAC = ER(3,IE)
        GS1B1 = WFAC * GS1B1
        G(I1,I1+1) = G(I1,I1+1) + GS1B1
        G(I1,I2) = G(I1,I2) - W * GS1S2
        G(I1,I2+1) = G(I1,I2+1) - GS1B1
        GS2B2 = WFAC * GS2B2
        G(I1+1,I2) = G(I1+1,I2) - GS2B2
        G(I2,I2+1) = G(I2,I2+1) + GS2B2
        R(I1) = R(I1) - W * RS1
        R(I2) = R(I2) + W * RS2
      END DO

      ! Velocity scatter error contributions
      VLSUM = EVS(1,1)
      W = EVS(2,1)
      WM = EVS(3,1)
      WDT = W * DTAV
      WDTM = WDT / M1
      IIL = IEVS(2,1)
      IIU = IEVS(3,1)
      DO J = 1, M1
        ITJ = IT(J)
        VIT = VITJ(J)
        RS = 0.D0
        DO L = J, M1
          GSSL(L) = 0.D0
          GSBL(L) = 0.D0
        END DO
        DO II = IIL, IIU
          VDIF = V(II+ITJ,J) - VIT
          RS = RS + VDIF
          DO L = J, M1
            VDIFL = V(II+IT(L),L) - VITJ(L)
            GSSL(L) = GSSL(L) + VDIFL * VDIF
            GSBL(L) = GSBL(L) + VDIFL * II
          END DO
        END DO
        I = 2 * J + 1
        G(I,I) = G(I,I) + W * GSSL(J)
        WFAC = WDT * GSBL(J)
        G(I,I+1) = G(I,I+1) - WFAC
        WFAC = WFAC / M1
        DO K = I, ( 2 * M1 + 1 ), 2
          G(I,K) = G(I,K) - WM * GSSL((K-1)/2)
          G(I,K+1) = G(I,K+1) + WFAC
          G(I+1,K) = G(I+1,K) + WDTM * GSBL((K-1)/2)
        END DO
        R(I) = R(I) - W * ( V(0,J) - VLSUM ) * RS
      END DO

      IF ( M2 .EQ. 0 ) GO TO 122
      VLSUM = EVS(1,2)
      W = EVS(2,2)
      WM = EVS(3,2)
      WDT = W * DTAV
      WDTM = WDT / M2
      J1 = IEVS(1,2)
      IIL = IEVS(2,2)
      IIU = IEVS(3,2)
      DO J = J1, M
        ITJ = IT(J)
        VIT = VITJ(J)
        RS = 0.D0
        DO L = J, M
          GSSL(L) = 0.D0
          GSBL(L) = 0.D0
        END DO
        DO II = IIL, IIU
          VDIF = V(II+ITJ,J) - VIT
          RS = RS + VDIF
          DO L = J, M
            VDIFL = V(II+IT(L),L) - VITJ(L)
            GSSL(L) = GSSL(L) + VDIFL * VDIF
            GSBL(L) = GSBL(L) + VDIFL * II
          END DO
        END DO
        I = 2 * J + 1
        G(I,I) = G(I,I) + W * GSSL(J)
        WFAC = WDT * GSBL(J)
        G(I,I+1) = G(I,I+1) - WFAC
        WFAC = WFAC / M2
        DO K = I, ( 2 * M + 1 ), 2
          G(I,K) = G(I,K) - WM * GSSL((K-1)/2)
          G(I,K+1) = G(I,K+1) + WFAC
          G(I+1,K) = G(I+1,K) + WDTM * GSBL((K-1)/2)
        END DO
        R(I) = R(I) - W * ( V(0,J) - VLSUM ) * RS
      END DO

      ! Momentum error contributions
  122 W = EM(2)
      WFAC = EM(1)
      IIL = IEM(2)
      IIU = IEM(3)
      DO J = JL, M
        WTJ = WT(J)
        SFAC = W * WTJ
        BFAC = WFAC * WTJ
        ITJ = IT(J)
        VIT = VITJ(J)
        DO L = J, M
          GSSL(L) = 0.D0
          GSBL(L) = 0.D0
        END DO
        DO II = IIL, IIU
          VDIF = V(II+ITJ,J) - VIT
          DO L = J, M
            VDIFL = V(II+IT(L),L) - VITJ(L)
            GSSL(L) = GSSL(L) + VDIFL * VDIF
            GSBL(L) = GSBL(L) + VDIFL * II
          END DO
        END DO
        I = 2 * J + 1
        DO K = I, ( 2 * M + 1 ), 2
          L = ( K - 1 ) / 2
          WTL = WT(L)
          G(I,K) = G(I,K) + SFAC * WTL * GSSL(L)
          G(I,K+1) = G(I,K+1) - BFAC * WTL * GSBL(J)
          G(I+1,K) = G(I+1,K) - BFAC * WTL * GSBL(L)
        END DO
      END DO

      ! Set lower symmetric triangle of G matrix
      DO K = NDL, ND
        DO I = K + 1, ND
          G(I,K) = G(K,I)
        END DO
      END DO

      RETURN
      END



      SUBROUTINE SOLVE( NDL, ND, G, R, X )

!***********************************************************************
!* Solves [G]{X} = {R} Problem Using Row Scaling, Row Pivoting,
!* and Gaussian Elimination
!***********************************************************************

      INTEGER NDL, ND

      INTEGER I, IB, IP, K, KS

      DOUBLE PRECISION G(42,42), R(42), X(42), TMP, PIV, FAC


      ! Perform row scaling and switch R vector to X vector
      DO I = NDL, ND
        FAC = ABS(G(I,NDL))
        DO K = NDL+1, ND
          FAC = MAX( FAC, ABS(G(I,K)) )
        END DO

        DO K = NDL, ND
          G(I,K) = G(I,K) / FAC
        END DO
        X(I) = R(I) / FAC
      END DO

      ! Perform row pivots and forward elimination
      DO K = NDL, ND
        ! Find largest magnitude in column below diagonal - the pivot
        IP = K
        PIV = G(K,K)
        DO I = K+1, ND
          IF ( ABS( G(I,K) ) .GT. ABS( PIV ) ) THEN
            PIV = G(I,K)
            IP = I
          END IF
        END DO

        ! Check for ill-conditioning / singularity
        IF ( ABS(PIV) .LT. 1.E-10 ) THEN
          WRITE( *, 991 )
          STOP ' '
        END IF

        ! Perform row pivoting
        IF ( IP .NE. K ) THEN
          DO KS = K, ND
            TMP = G(K,KS)
            G(K,KS) = G(IP,KS)
            G(IP,KS) = TMP
          END DO
          TMP = X(K)
          X(K) = X(IP)
          X(IP) = TMP
        END IF

        ! Perform forward elimination
        DO I = K + 1, ND
          FAC = G(I,K) / PIV
          DO KS = K + 1, ND
            G(I,KS) = G(I,KS) - FAC * G(K,KS)
          END DO
          X(I) = X(I) - FAC * X(K)
        END DO
      END DO

      ! Perform backward elimination for final X(K) solution
      DO I = ND, NDL, -1
        FAC = X(I) / G(I,I)
        X(I) = FAC
        DO IB = NDL, I - 1
          X(IB) = X(IB) - G(IB,I) * FAC
        END DO
      END DO

      RETURN

  991 FORMAT(/' *** ERROR: Singular or near-singular gradient ',
     & 'matrix')

      END



      SUBROUTINE EVAL( JL, M, M1, M2, WT, A, V, D, NFP_V, NLP_V,
     & DT, DTAV, DTVD, DTAD, IT, X,
     & ES, EB, ET, EA, IEA, NEA, EV, IEV, NEV, ED, IED, NED,
     & ER, IER, NER, EVS, IEVS, EM, IEM, ITCON, NTC, ERROR )

!***********************************************************************
!* Evaluates the Error Measure for a Given Solution
!***********************************************************************

      INTEGER JL, M, M1, M2, NFP_V, NLP_V,
     & IT(0:20), IEA(3,100), NEA, IEV(3,100), NEV,
     & IED(3,100), NED, IER(4,100), NER,
     & IEVS(3,2), IEM(3), ITCON(3,100), NTC

      INTEGER I, IE, II, ITJ, ITJ1, ITJ2, J, J1, J2

      REAL A(NFP_V-1000:NLP_V+1000,0:20),
     & V(NFP_V-1000:NLP_V+1000,0:20),
     & D(NFP_V-1000:NLP_V+1000,0:20)

      DOUBLE PRECISION WT(0:20), DT, DTAV, DTVD, DTAD, X(42),
     & ES(2,0:20), EB(2,0:20), ET(2,0:20),
     & EA(3,100), EV(3,100), ED(3,100), ER(3,100),
     & EVS(3,2), EM(2), ERROR, S(0:20), BJ,
     & B(0:20), BDT(0:20), BDTD(0:20), ERR, SJ, BPA, BDTJ,
     & VIN, VIT, DIT, SJ1, SJ2, VITJ(0:20), VAV, VDIFJ, VDIF(0:20)


      ! Zero-initialize error
      ERROR = 0.D0

      ! Set up solution arrays
      DO J = JL, M
        I = 2 * J + 1
        S(J) = X(I)
        BJ = X(I+1)
        B(J) = BJ
        BDT(J) = BJ * DTAV
        BDTD(J) = BJ * DTAD / 2.D0
        VITJ(J) = V(IT(J),J)
      END DO

      ! Scaling, bias, and time shift error contributions
      DO J = JL, M
        ERROR = ERROR + ES(2,J) * ( S(J) - ES(1,J) )**2
        ERROR = ERROR + EB(2,J) * ( B(J) - EB(1,J) )**2
        ERROR = ERROR + ET(2,J) * ( IT(J) * DT - ET(1,J) )**2
      END DO

      ! Acceleration error contributions
      DO IE = 1, NEA
        J = IEA(1,IE)
        SJ = S(J)
        BPA = B(J) + EA(1,IE)
        ITJ = IT(J)
        ERR = 0.D0
        DO II = IEA(2,IE) + ITJ, IEA(3,IE) + ITJ
          ERR = ERR + ( A(II,J) * SJ - BPA )**2
        END DO
        ERROR = ERROR + EA(2,IE) * ERR
      END DO

      ! Velocity error contributions
      DO IE = 1, NEV
        J = IEV(1,IE)
        SJ = S(J)
        BDTJ = BDT(J)
        VIN = V(0,J) - EV(1,IE)
        ITJ = IT(J)
        VIT = VITJ(J)
        ERR = 0.D0
        DO II = IEV(2,IE), IEV(3,IE)
          ERR = ERR +
     &     ( ( V(II+ITJ,J) - VIT ) * SJ - BDTJ * II + VIN )**2
        END DO
        ERROR = ERROR + EV(2,IE) * ERR
      END DO

      ! Displacement error contributions
      DO IE = 1, NED
        J = IED(1,IE)
        SJ = S(J)
        BDTJ = BDTD(J)
        ITJ = IT(J)
        VIT = ( V(0,J) - VITJ(J) * SJ ) * DTVD
        DIT = D(0,J) - ED(1,IE) - D(ITJ,J) * SJ
        ERR = 0.D0
        DO II = IED(2,IE), IED(3,IE)
          ERR = ERR +
     &     ( D(II+ITJ,J) * SJ + VIT * II + DIT - BDTJ * II**2 )**2
        END DO
        ERROR = ERROR + ED(2,IE) * ERR
      END DO

      ! Relative displacement error contributions
      DO IE = 1, NER
        J1 = IER(1,IE)
        J2 = IER(4,IE)
        SJ1 = S(J1)
        SJ2 = S(J2)
        BDTJ = BDTD(J1) - BDTD(J2)
        ITJ1 = IT(J1)
        ITJ2 = IT(J2)
        VIT = ( V(0,J1) - VITJ(J1) * SJ1 - V(0,J2) + VITJ(J2) * SJ2 )
     &   * DTVD
        DIT = D(0,J1) - D(0,J2) - ER(1,IE) - D(ITJ1,J1) * SJ1 +
     &   D(ITJ2,J2) * SJ2
        ERR = 0.D0
        DO II = IER(2,IE), IER(3,IE)
          ERR = ERR + ( D(II+ITJ1,J1) * SJ1 - D(II+ITJ2,J2) * SJ2 +
     &     VIT * II + DIT - BDTJ * II**2 )**2
        END DO
        ERROR = ERROR + ER(2,IE) * ERR
      END DO

      ! Velocity scatter error contributions
      ERR = 0.D0
      DO II = IEVS(2,1), IEVS(3,1)
        VAV = 0.D0
        DO J = 1, M1
          VDIFJ = ( V(II+IT(J),J) - VITJ(J) ) * S(J) - BDT(J) * II +
     &     V(0,J)
          VDIF(J) = VDIFJ
          VAV = VAV + VDIFJ
        END DO
        VAV = VAV / M1
        DO J = 1, M1
          ERR = ERR + ( VDIF(J) - VAV )**2
        END DO
      END DO
      ERROR = ERROR + EVS(2,1) * ERR

      IF ( M2 .EQ. 0 ) GO TO 117
      J1 = IEVS(1,2)
      ERR = 0.D0
      DO II = IEVS(2,2), IEVS(3,2)
        VAV = 0.D0
        DO J = J1, M
          VDIFJ = ( V(II+IT(J),J) - VITJ(J) ) * S(J) - BDT(J) * II +
     &     V(0,J)
          VDIF(J) = VDIFJ
          VAV = VAV + VDIFJ
        END DO
        VAV = VAV / M2
        DO J = J1, M
          ERR = ERR + ( VDIF(J) - VAV )**2
        END DO
      END DO
      ERROR = ERROR + EVS(2,2) * ERR

      ! Momentum error contributions
  117 ERR = 0.D0
      DO II = IEM(2), IEM(3)
        VAV = 0.D0
        DO J = JL, M
          VAV = VAV + WT(J) *
     &     ( ( V(II+IT(J),J) - VITJ(J) ) * S(J) - BDT(J) * II )
        END DO
        ERR = ERR + VAV**2
      END DO
      ERROR = ERROR + EM(2) * ERR

      ! Relative timing constraint penalty contribution
      DO IE = 1, NTC
        ERROR = ERROR + MAX(
     &   ( ITCON(3,IE) - IT(ITCON(1,IE) ) + IT(ITCON(2,IE))) * 1.D6,
     &   0.D0 )
      END DO

      ! Convert to RMS error
      ERROR = SQRT( ERROR )

      RETURN
      END



      SUBROUTINE REPORT( OUT_FILE, IDCODE, JL, M, DSCR, DT, CFAC,
     & ITMIN, XMIN, ERRAW, ERMIN, C, B, T )

!***********************************************************************
!* Prints a Report of the Minimum Error Solution
!***********************************************************************

      ! Headers
      INCLUDE 'platform.fi'

      INTEGER JL, M, ITMIN(0:20)

      INTEGER IOS, J, OUT_UNIT

      DOUBLE PRECISION DT, CFAC(0:20), XMIN(42), ERRAW, ERMIN,
     & C(0:20), B(0:20), T(0:20)

      CHARACTER OUT_FILE*255, IDCODE*10, DSCR(0:20)*30


      ! Compute the calibration, bias and time shift pulse errors
      DO J = JL, M
        C(J) = 1.D0 / XMIN(2*J+1)
        B(J) = XMIN(2*J+2) / CFAC(J)
        T(J) = ITMIN(J) * DT
      END DO

      ! Open output device
      IF ( OUT_FILE .EQ. ' ' ) OUT_FILE = SCREEN_DEV
      CALL OPEN_FS( OUT_UNIT, OUT_FILE, 'W', IOS )

      ! Print report header
      WRITE( OUT_UNIT, 901 ) IDCODE

      ! Print the raw vs. corrected error measure comparison
      WRITE( OUT_UNIT, 902, IOSTAT=IOS ) ERRAW, ERMIN

      ! Print the pulse error solution
      WRITE( OUT_UNIT, 903 )
      DO J = JL, M
        WRITE( OUT_UNIT, 904, IOSTAT=IOS ) J, DSCR(J), C(J), B(J), T(J)
        IF ( C(J) .LT. 0.D0 ) THEN ! Negative calibration warning
          WRITE( OUT_UNIT, '(A)', IOSTAT=IOS )
     &     '*** Negative calibration: '//
     &     'Signal may be sign-reversed'
          WRITE( *, '(/2A,I3,A)', IOSTAT=IOS ) ' *** WARNING: ',
     &     'Signal ', J, ' negative calibration: '//
     &     'Signal may be sign-reversed'
        ELSE IF ( ( C(J) .GT. 2.D0 ) .OR. ( C(J) .LT. .5D0 ) ) THEN
          ! Large calibration warning
          WRITE( OUT_UNIT, '(A)', IOSTAT=IOS )
     &     '*** Large calibration: '//
     &     'Signal may have wrong units'
          WRITE( *, '(/2A,I3,A)', IOSTAT=IOS ) ' *** WARNING: ',
     &     'Signal ', J, ' large calibration: '//
     &     'Signal may have wrong units'
        END IF
      END DO

      ! Close the output file
      CLOSE( OUT_UNIT )

      RETURN

  901 FORMAT(32X,'VeCor  Solution'/31X,17('-'),22X,A10/)
  902 FORMAT(/'Error measure reduced from ',F15.7,' to ',F15.7/)
  903 FORMAT('Pulse',5X,'Description',22X,'Calibration',5X,'Bias',
     & 6X,'Time Shift'/5('-'),5X,30('-'),1X,3(2X,11('-')))
  904 FORMAT(1X,I2,7X,A30,3X,F11.7,2X,F11.4,2X,F11.6)

      END



      SUBROUTINE OUTPUT( IDCODE, JL, M, C, B, T, ITMIN, TS_FILE,
     & NFP_V, NLP_V, U, Y )

!***********************************************************************
!* Outputs the VeCor-Corrected UDS Files
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'

      RECORD /UDS_Y/  U
      RECORD /UDS_CONTROL/  UC

      INTEGER JL, M, ITMIN(0:20), NFP_V, NLP_V

      INTEGER I, IDT, ITMINJ, IOS, J

      REAL Y(NFP_V:NLP_V)

      DOUBLE PRECISION C(0:20), B(0:20), T(0:20), CJ, BJ

      CHARACTER IDCODE*10, TS_FILE(0:20)*255, OUT_FILE*255


      ! Read, correct, & write UDS files
      CALL UDS_CONTROL_INIT( UC )
      UC.DIMSYS = ' '
      UC.CHANFORM = 'Y'
      DO J = JL, M
        CALL UDS_READ( TS_FILE(J), U, UC, IOS )
        IF ( IOS .NE. 0 ) GO TO 101
        IF ( J .EQ. JL ) UC.DIMSYS = U.DIMSYS

        ITMINJ = ITMIN(J)
        CJ = C(J)
        BJ = B(J)
        U.NFP = MAX( U.NFP, NFP_V )
        U.NLP = MIN( U.NLP, NLP_V )
        DO I = U.NFP, U.NFP - ITMINJ - 1
          Y(I) = 0.
        END DO
        DO I = U.NFP - ITMINJ, U.NLP - ITMINJ
          Y(I) = REAL( ( U.Y(I+ITMINJ) / CJ ) - BJ )
        END DO
        DO I = U.NLP - ITMINJ + 1, U.NLP
          Y(I) = 0.
        END DO
        DO I = U.NFP, U.NLP
          U.Y(I) = Y(I)
        END DO

        U.STATUS = 'VeCor-Generated'
        U.RD1 = REAL( CJ )
        U.RD2 = REAL( BJ )
        U.RD3 = REAL( T(J) )
        U.CD1 = 'VeCor ID: '//IDCODE

        OUT_FILE = TS_FILE(J)
        CALL FN_EXTRACT( OUT_FILE )
        CALL FN_FLAG( OUT_FILE, 'v', 2, IDT )
        CALL UDS_WRITE( OUT_FILE, U, UC, IOS )
  101   CONTINUE
      END DO

      RETURN
      END
