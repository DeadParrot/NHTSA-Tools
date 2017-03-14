      PROGRAM VC

!***********************************************************************
!* The Viscous Criterion Program
!*
!* Language: Fortran
!*
!* Author: C. Gabler
!*
!* Date: 2001/03/04
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'
      INCLUDE 'file.fi'


      ! Variables ______________________________________________________

      RECORD /UDS_Y/  U
      RECORD /UDS_CONTROL/  C
      RECORD /FILE/  HIT, REPORT, OUTPUT_LIST

      LOGICAL ALL_BATCH, NO_BATCH, NO_INCR, ONE_PASS, NEW_HIT, EOF

      INTEGER NUM_ARGS, MAX_ARGS, IOS, I

      PARAMETER ( MAX_ARGS = 12 )

      REAL VCMAX, TMAX, SFAC

      CHARACTER CL_ARG(0:MAX_ARGS)*255, PROG_NAME*8,
     & TEMPLATE_STR*255, DIR_OUT*255,
     & FILE_NAME*255,
     & PROMPT*79


      ! Initializations
      PROG_NAME = 'VC'
      CALL FILE_INIT( HIT )
      CALL GET_CL( CL_ARG, NUM_ARGS, MAX_ARGS )
      CALL PROC_CL( CL_ARG, NUM_ARGS, PROG_NAME, TEMPLATE_STR,
     & ALL_BATCH, NO_BATCH, NO_INCR, REPORT, OUTPUT_LIST, DIR_OUT,
     & ONE_PASS, .TRUE., .FALSE. )
      PROMPT = 'UDS chest compression file name/template/hitlist?'
      PROMPT(74:) = '[done]'
      CALL UDS_CONTROL_INIT( C )
      C.CHANFORM = 'Y'
      C.DIMSYS = 'MET'

      WRITE( *, '(//14X,A//)' )
     & '**  Side Impact Viscous Criterion Computation  **'

      ! Read the file name/template/hitlist
  100 CALL FILE_PROMPT( TEMPLATE_STR, PROMPT, DIR_OUT,
     & ONE_PASS, HIT, NEW_HIT, FILE_NAME, EOF )
      IF ( ( FILE_NAME .EQ. ' ' ) .OR. ( EOF ) ) GO TO 199 ! Done

      ! Read UDS file
      CALL REP_COLS( REPORT )
      CALL UDS_GET( FILE_NAME, U, C, REPORT, IOS )
      IF ( IOS .NE. 0 ) GO TO 190

      ! Check file fields
      IF ( ( U.CURTYP .NE. 'TIME SERIES' ) .OR.
     & ( U.XTYP .NE. 'TIME' ) .OR.
     & ( U.XUNITS .NE. 'SECONDS' ) )
     & CALL MSG_WRITE( '*** WARNING - Not a time series file',
     & REPORT )
      IF ( ( U.SENATT(:6) .NE. 'CHEST ' ) .OR.
     & ( U.YTYP .NE. 'DISPLACEMENT' ) )
     & CALL MSG_WRITE( '*** WARNING - Not CHEST DISPLACEMENT data',
     & REPORT )
      IF ( ( U.NFP .GT. 0 ) .OR. ( U.NLP .LE. 0 ) ) CALL MSG_WRITE(
     & '*** WARNING - Nonstandard x-axis span', REPORT )

      ! Convert Displacement to Meters
      SFAC = 1.0
      IF ( U.YUNITS .EQ. 'CENTIMETERS' ) THEN
        SFAC = .01
      ELSE IF ( U.YUNITS .EQ. 'MILLIMETERS' ) THEN
        SFAC = .001
      ELSE IF ( U.YUNITS .NE. 'METERS' ) THEN
        CALL MSG_WRITE(
     &     '*** ERROR - Unrecognized compression units: '//U.YUNITS,
     &     REPORT )
        GO TO 190
      END IF
      IF ( SFAC .NE. 1.0 ) THEN
        DO I = U.NFP, U.NLP
          U.Y(I) = U.Y(I) * SFAC
        END DO
      END IF

      ! Compute Viscous Criterion
      CALL VCSIDE( ' ', ' ', U, VCMAX, TMAX )

      ! Report results
      WRITE( *, '(/5X,A,1PG15.6,0P,A,5X,A,F9.3,A)',
     & IOSTAT=IOS )
     & 'VC = ', VCMAX, ' (m/s)', 'at T = ', TMAX*1000, ' (msec)'
      IF ( REPORT.OPEN )
     & WRITE( REPORT.UNIT, '(5X,A,1PG15.6,0P,A,5X,A,F9.3,A)',
     & IOSTAT=IOS )
     & 'VC = ', VCMAX, ' (m/s)', 'at T = ', TMAX*1000, ' (msec)'

      ! Loop for next file
  190 WRITE( *, '(80(''_''))' )
      GO TO 100

      ! Clean up and stop
  199 CALL CLEAN_UP( HIT, REPORT, OUTPUT_LIST, DIR_OUT )

      END



      SUBROUTINE VCSIDE( IN_FILE, DIR_OUT, U, VCMAX, TMAX )

!***********************************************************************
!* Computes the Viscous Criterion for a Side-Impacted Occupant
!*
!* Language: Fortran
!*
!* Author: C. Gabler
!*         Stuart G. Mentzer: Tools integration
!*
!* Date: 1999/10/18
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) IN_FILE

      CHARACTER*(*) DIR_OUT

      RECORD /UDS_Y/  U

      REAL VCMAX

      REAL TMAX


      ! Variables ______________________________________________________

      RECORD /UDS_Y/  V

      RECORD /UDS_CONTROL/  C

      INTEGER IIFLAG(5), IOS, I

      REAL FCUT, FSTP, FATT, XPOLE

      DOUBLE PRECISION CONST, SFAC

      CHARACTER V_FILE*255


      ! nth order Butterworth Filter
      FCUT = 100.
      FATT = -40.
      XPOLE = 4.
      FSTP = FCUT*10.**(-FATT/(20.*XPOLE))
      IIFLAG(1) = INT( FCUT )             ! CUTOFF FREQUENCY
      IIFLAG(2) = INT( FATT )             ! STOP FREQ ATTENTUATION
      IIFLAG(3) = 3                       ! PHASELESS FILTER
      IIFLAG(4) = INT( FSTP )             ! STOP FREQUENCY
      IIFLAG(5) = 0                       ! UNUSED
      CALL IIRFIL( U.Y(U.NFP), U.DEL, U.NLP-U.NFP+1, IIFLAG )

      ! Compute the velocity from the derivative of the compression
      CALL UDS_COPY( V, U )
      SFAC = 1.D0
      CALL DERIV( V.Y(V.NFP), V.NLP-V.NFP+1, V.DEL, SFAC )

      ! Filter the velocity curve - nth order Butterworth
      FCUT = 100.
      FATT = -40.
      XPOLE = 4.
      FSTP = FCUT*10.**(-FATT/(20.*XPOLE))
      IIFLAG(1) = INT( FCUT )             ! CUTOFF FREQUENCY
      IIFLAG(2) = INT( FATT )             ! STOP FREQ ATTENTUATION
      IIFLAG(3) = 3                       ! PHASELESS FILTER
      IIFLAG(4) = INT( FSTP )             ! STOP FREQUENCY
      IIFLAG(5) = 0                       ! UNUSED
      CALL IIRFIL( V.Y(V.NFP), V.DEL, V.NLP-V.NFP+1, IIFLAG )

      ! Set output fields
      V.STATUS = 'COMPUTED'
      V.HIC = 0.
      V.T1 = 0.
      V.T2 = 0.
      V.HICDTUP = 0.
      V.CLIP3M = 0.
      V.CSI = 0.

      ! Initialize the UDS i/o controls
      CALL UDS_CONTROL_INIT( C )

      ! Output velocity file
      IF ( IN_FILE .NE. ' ' ) THEN
        V.YTYP = 'VELOCITY'
        V.YUNITS = 'METERS/SEC'
        V.FCUT = FCUT
        V.FSTP = FSTP
        V.FCOR = FCUT / 2
        V_FILE = IN_FILE
        CALL FN_EXTRACT( V_FILE )
        CALL FN_FLAG( V_FILE, 'v', 3, I )
        CALL ADD_PATH( DIR_OUT, V_FILE )
        CALL FN_INCR( V_FILE )
        CALL UDS_WRITE( V_FILE, V, C, IOS )
      END IF

      ! Compute VC and VC_max
      CONST = 1.D0 / .1745D0
      VCMAX = 0.
      DO I = V.NFP, V.NLP
        V.Y(I) =  REAL( V.Y(I) * U.Y(I) * CONST )
        IF ( ( I .GE. 0 ) .AND. ( V.Y(I) .GT. VCMAX ) ) THEN
          VCMAX = V.Y(I)
          TMAX  = I * V.DEL
        END IF
      END DO

      ! Output VC file
      IF ( IN_FILE .NE. ' ' ) THEN
        V.YTYP = 'V*C'
        V.YUNITS = 'METERS/SEC'
        V.FCUT = 0.
        V.FSTP = 0.
        V.FCOR = 0.
        V_FILE = IN_FILE
        CALL FN_EXTRACT( V_FILE )
        CALL FN_FLAG( V_FILE, 'vvc', 3, I )
        CALL ADD_PATH( DIR_OUT, V_FILE )
        CALL FN_INCR( V_FILE )
        CALL UDS_WRITE( V_FILE, V, C, IOS )
      END IF

      RETURN
      END



      SUBROUTINE DERIV( Y, NP, DX, CONST )

!***********************************************************************
!* Computes the Derivative of Y With Respect To a Constant Increment of X (DX)
!*
!* Language: Fortran
!*
!* Author: M.R.Neale - Dec-1980
!*
!* Modification History:
!*  Ver/Ed    Date      Modification
!* ------- ---------  ------------------------------------------------
!*            2/93    Updated to conform to UDS-1992 SideImp program
!*
!* Date: 1999/10/18
!***********************************************************************


      ! Arguments ______________________________________________________

      INTEGER NP ! Number of points in Y

      REAL Y(NP) ! Data to be differentiated

      REAL DX ! Increment between points in Y

      DOUBLE PRECISION CONST


      ! Variables ______________________________________________________

      INTEGER N, I

      REAL Z, DX2, YM1, YM2, YT


      Z       = REAL( CONST/(12.0*DX) )
      DX2     = REAL( (0.5D0*CONST)/DX )
      N       = NP-2
      YM2     = Y(1)
      YM1     = Y(2)
      Y(1)    = ((-3.0*YM2)+(4.0*YM1)-Y(3))*DX2
      Y(2)    = ((-3.0*YM1)+(4.0*Y(3))-Y(4))*DX2

      DO I = 3, N
        YT   = Y(I)
        Y(I) = ((YM2-Y(I+2))-(8.0*(YM1-Y(I+1))))*Z
        YM2  = YM1
        YM1  = YT
      END DO

      YT      = Y(NP-1)
      Y(NP-1) = (YM2-(4.0*YM1)+(3.0*YT))*DX2
      Y(NP)   = (YM1-(4.0*YT)+(3.0*Y(NP)))*DX2

      RETURN
      END
