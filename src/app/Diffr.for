      PROGRAM DIFFR

!***********************************************************************
!* Differentiates UDS Files using a Centered Difference Formula
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/18
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'
      INCLUDE 'file.fi'


      ! Variables ______________________________________________________

      RECORD /UDS/  U
      RECORD /UDS_CONTROL/  C
      RECORD /FILE/  HIT, REPORT, OUTPUT_LIST

      LOGICAL ALL_BATCH, NO_BATCH, NO_INCR, ONE_PASS, NEW_HIT, EOF,
     & BATCH, NO_FLAG, INI_VAL, INI_FILE, MSG

      PARAMETER ( MSG = .TRUE. )

      INTEGER NUM_ARGS, MAX_ARGS, IOS, I

      PARAMETER ( MAX_ARGS = 12 )

      DOUBLE PRECISION DY0_CL, DY0_S

      CHARACTER CL_ARG(0:MAX_ARGS)*255, PROG_NAME*8,
     & TEMPLATE_STR*255, DIR_OUT*255,
     & FILE_NAME*255, OUT_FILE*255, DEF_FILE*255,
     & PROMPT*79,
     & DYFLAG, DY0_F


      ! Initializations
      PROG_NAME = 'Diffr'
      CALL FILE_INIT( HIT )
      CALL GET_CL( CL_ARG, NUM_ARGS, MAX_ARGS )
      CALL DIFFR_CL( CL_ARG, NUM_ARGS, PROG_NAME,
     & INI_VAL, INI_FILE, DY0_CL, NO_FLAG )
      CALL PROC_CL( CL_ARG, NUM_ARGS, PROG_NAME, TEMPLATE_STR,
     & ALL_BATCH, NO_BATCH, NO_INCR, REPORT, OUTPUT_LIST, DIR_OUT,
     & ONE_PASS, .TRUE., .TRUE. )
      PROMPT = 'UDS file name/template/hitlist?'
      PROMPT(74:) = '[done]'
      CALL UDS_CONTROL_INIT( C )

      WRITE( *, '(//24X,A//)' ) '**  Differentiate UDS Files  **'

      ! Read the file name/template/hitlist
  100 CALL FILE_PROMPT( TEMPLATE_STR, PROMPT, DIR_OUT,
     & ONE_PASS, HIT, NEW_HIT, FILE_NAME, EOF )
      IF ( ( FILE_NAME .EQ. ' ' ) .OR. ( EOF ) ) GO TO 199 ! Done

      ! Batch setup
      CALL BAT_SET( ALL_BATCH, NO_BATCH, NEW_HIT, HIT, BATCH )

      ! Read UDS file
      CALL REP_COLS( REPORT )
      CALL UDS_GET( FILE_NAME, U, C, REPORT, IOS )
      IF ( IOS .NE. 0 ) GO TO 190

      ! Check file type
      IF ( ( U.CHANFORM .NE. 'Y' ) .AND.
     & ( U.CHANFORM .NE. 'X-Y' ) ) THEN
        CALL MSG_WRITE( '*** ERROR - Unsupported channel format: '//
     &     U.CHANFORM, REPORT )
        GO TO 190
      END IF
      IF ( ( U.CHANFORM .EQ. 'Y' ) .AND. ( U.DEL .LE. 0. ) ) THEN
        CALL MSG_WRITE( '*** ERROR - Non-positive x-axis step value',
     &     REPORT )
        GO TO 190
      END IF
      IF ( U.NLP - U.NFP .LT. 3 ) THEN
        CALL MSG_WRITE( '*** ERROR - Too few data points', REPORT )
        GO TO 190
      END IF

      ! Set initial value specs
      IF ( INI_VAL ) THEN
        IF ( INI_FILE ) THEN ! Use file value, if any
          DY0_F = 'F'
        ELSE ! Use specified value
          DY0_S = DY0_CL
          DY0_F = 'V'
        END IF
      ELSE
        DY0_S = 0.D0
        DY0_F = ' '
      END IF

      ! Compute the derivative
      CALL DIFFR_S( U.DIMSYS, U.CHANFORM,
     & U.X(U.NFP), U.Y(U.NFP),
     & U.NFP, U.NLP, U.DEL, U.INIVEL,
     & U.XTYP, U.XUNITS, U.YTYP, U.YUNITS,
     & DYFLAG, DY0_F, DY0_S, MSG, IOS )
      IF ( IOS .NE. 0 ) GO TO 190

      ! Prepare output fields
      IF ( .NOT. NO_FLAG ) U.STATUS = 'COMPUTED'
      U.HIC = 0.
      U.T1 = 0.
      U.T2 = 0.
      U.HICDTUP = 0.
      U.CLIP3M = 0.
      U.CSI = 0.
      U.CD1 = 'Diffr differentiated'

      ! Set default output file name
      OUT_FILE = FILE_NAME
      CALL FN_EXTRACT( OUT_FILE )
      IF ( U.CHANFORM .EQ. 'Y' ) THEN
        IF ( NO_FLAG ) THEN
          CALL FN_FLAG( OUT_FILE, DYFLAG, 3, I )
        ELSE
          CALL FN_FLAG( OUT_FILE, DYFLAG//'c', 3, I )
        END IF
      ELSE
        CALL FN_FLAG( OUT_FILE, DYFLAG, 3, I )
      END IF
      CALL ADD_PATH( DIR_OUT, OUT_FILE )
      IF ( .NOT. NO_INCR ) CALL FN_INCR( OUT_FILE )
      CALL FN_NAME_EXTRACT( OUT_FILE )
      DEF_FILE = OUT_FILE

      ! Set the output UDS file name
      IF ( .NOT. BATCH ) THEN
  120   WRITE( *, '(/1X,2A/3A/'' >> '',$)' )
     &     U.YTYP, ' file name?',
     &     ' [', DEF_FILE(:L_TRIM(DEF_FILE)), ']'
        READ( *, '(A)', END=190 ) OUT_FILE
        IF ( OUT_FILE .EQ. ' ' ) THEN ! Use default file name
          OUT_FILE = DEF_FILE
        ELSE ! Check file name validity
          CALL ADD_PATH( DIR_OUT, OUT_FILE )
          IF ( .NOT. FN_VALID( OUT_FILE ) ) THEN
            WRITE( *, * ) '*** Invalid file name'
            GO TO 120
          END IF
        END IF
      ELSE ! Batch
        OUT_FILE = DEF_FILE
      END IF

      ! Write the output UDS file
      CALL UDS_PUT( OUT_FILE, U, C, REPORT, OUTPUT_LIST,
     & DIR_OUT, .TRUE., IOS )

      ! Loop for next file
  190 WRITE( *, '(80(''_''))' )
      GO TO 100

      ! Clean up and stop
  199 CALL CLEAN_UP( HIT, REPORT, OUTPUT_LIST, DIR_OUT )

      END



      SUBROUTINE DIFFR_CL( CL_ARG, NUM_ARGS, PROG_NAME,
     & INI_VAL, INI_FILE, DY0_CL, NO_FLAG )

!***********************************************************************
!* Processes the Program-Specific Command Line Arguments
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) CL_ARG(0:*) ! Command line argument array

      INTEGER NUM_ARGS ! Number of arguments

      CHARACTER*(*) PROG_NAME ! Program name

      LOGICAL INI_VAL ! Indicates to use specified/file initial value

      LOGICAL INI_FILE ! Indicates to use file initial value

      DOUBLE PRECISION DY0_CL ! Specified initial value

      LOGICAL NO_FLAG ! Indicates not to flag output file names


      ! Variables ______________________________________________________

      INTEGER I, NS, NV, IOS

      CHARACTER ARG*255, C


      ! Process command line arguments
      INI_VAL = .FALSE.
      INI_FILE = .FALSE.
      DY0_CL = 0.D0
      NO_FLAG = .FALSE.
      DO I = 1, NUM_ARGS
        CALL STR_UPCASE( ARG, CL_ARG(I) )
        C = ARG(1:1)
        IF ( ( C .EQ. '-' ) .OR. ( C .EQ. '/' ) ) THEN
          NS = 2
        ELSE
          NS = 1
        END IF
        IF ( ( ARG(NS:) .EQ. '?' ) .OR.
     &     ( ARG(NS:) .EQ. '??' ) .OR.
     &     ( ARG(NS:) .EQ. 'HELP' ) ) THEN ! Show command line syntax
          CALL SYNTAX_CL( PROG_NAME )
          WRITE( *, '(10X,A)' )
     &       '[INI_VAL[=<value>]] [NO_FLAG]'
          IF ( ( ARG(NS:) .EQ. '??' ) .OR.
     &       ( ARG(NS:) .EQ. 'HELP' ) ) THEN ! Show command line help
            CALL HELP_CL( PROG_NAME )
            WRITE( *, '(10X,A/10X,A)' )
     &         'INI_VAL :  '//
     &         'Use initial value from file or specified <value>',
     &         'NO_FLAG :  Don''t flag output file names'
          END IF
          STOP ' '
        ELSE IF ( ( ARG(NS:) .EQ. 'INI_VAL' ) .OR.
     &     ( ARG(NS:) .EQ. 'INIVAL' ) ) THEN
          INI_VAL = .TRUE.
          INI_FILE = .TRUE.
          CL_ARG(I) = ' '
        ELSE IF ( ( ARG(NS:NS+7) .EQ. 'INI_VAL=' ) .OR.
     &     ( ARG(NS:NS+7) .EQ. 'INI_VAL#' ) .OR.
     &     ( ARG(NS:NS+6) .EQ. 'INIVAL=' ) .OR.
     &     ( ARG(NS:NS+6) .EQ. 'INIVAL#' ) ) THEN
          INI_VAL = .TRUE.
          INI_FILE = .FALSE.
          IF ( ARG(NS:NS+6) .EQ. 'INI_VAL' ) THEN
            NV = NS + 8
          ELSE
            NV = NS + 7
          END IF
          READ( ARG(NV:NV+24), '(BN,F25.0)', IOSTAT=IOS ) DY0_CL
          IF ( IOS .NE. 0 ) THEN
            WRITE( *, '(/2A/)', IOSTAT=IOS )
     &         '*** Invalid initial value: ',
     &         ARG(NV:NV+24)
            STOP ' '
          END IF
          CL_ARG(I) = ' '
        ELSE IF ( ( ARG(NS:) .EQ. 'NO_FLAG' ) .OR.
     &     ( ARG(NS:) .EQ. 'NOFLAG' ) ) THEN
          NO_FLAG = .TRUE.
          CL_ARG(I) = ' '
        END IF
      END DO

      RETURN
      END
