      PROGRAM SCALE

!***********************************************************************
!* Scales UDS Files
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
     & BATCH, GROUP, NEW_GROUP, NO_FLAG

      INTEGER NUM_ARGS, MAX_ARGS, IOS, IOS_W, I

      PARAMETER ( MAX_ARGS = 12 )

      DOUBLE PRECISION SFAC_CL, SFAC

      CHARACTER CL_ARG(0:MAX_ARGS)*255, PROG_NAME*8,
     & TEMPLATE_STR*255, DIR_OUT*255,
     & FILE_NAME*255, OUT_FILE*255, DEF_FILE*255,
     & PROMPT*79,
     & AXIS_CL, TYPE_CL*20, UNITS_CL*20,
     & AXIS, SFAC_C*25, TYPE*20, UNITS*20


      ! Initializations
      PROG_NAME = 'Scale'
      CALL FILE_INIT( HIT )
      CALL GET_CL( CL_ARG, NUM_ARGS, MAX_ARGS )
      CALL SCALE_CL( CL_ARG, NUM_ARGS, PROG_NAME,
     & AXIS_CL, SFAC_CL, TYPE_CL, UNITS_CL, NO_FLAG )
      CALL PROC_CL( CL_ARG, NUM_ARGS, PROG_NAME, TEMPLATE_STR,
     & ALL_BATCH, NO_BATCH, NO_INCR, REPORT, OUTPUT_LIST, DIR_OUT,
     & ONE_PASS, .TRUE., .TRUE. )
      PROMPT = 'UDS file name/template/hitlist?'
      PROMPT(74:) = '[done]'
      CALL UDS_CONTROL_INIT( C )

      WRITE( *, '(//27X,A//)' ) '**  Scale UDS Files  **'

      ! Read the file name/template/hitlist
  100 CALL FILE_PROMPT( TEMPLATE_STR, PROMPT, DIR_OUT,
     & ONE_PASS, HIT, NEW_HIT, FILE_NAME, EOF )
      IF ( ( FILE_NAME .EQ. ' ' ) .OR. ( EOF ) ) GO TO 199 ! Done

      ! Batch setup
      CALL BAT_SET( ALL_BATCH, NO_BATCH, NEW_HIT, HIT, BATCH )
      CALL GROUP_SET( HIT, NEW_HIT, BATCH, 2, GROUP, NEW_GROUP )

      ! Group prompts
      IF ( NEW_GROUP ) THEN

        ! Get axis
  101   IF ( AXIS_CL .EQ. ' ' ) THEN
          WRITE( *, '(/A,54X,A/'' >> '',$)' )
     &       ' Axis to scale?   (X/Y)', '[Y]'
          READ( *, '(A)', IOSTAT=IOS ) AXIS
          IF ( IOS .NE. 0 ) THEN
            CALL MSG_WRITE( '*** Operation skipped', REPORT )
            CALL FILE_DEL( HIT )
            GO TO 190
          END IF
          CALL STR_UP( AXIS )
          IF ( .NOT. ANY_CHARS( AXIS, ' XY' ) ) THEN
            WRITE( *, * ) '*** Invalid response'
            GO TO 101
          END IF
          IF ( AXIS .EQ. ' ' ) AXIS = 'Y'
        ELSE
          AXIS = AXIS_CL
        END IF

        ! Get scale factor
  102   IF ( SFAC_CL .EQ. 0.D0 ) THEN
          WRITE( *, '(/A/'' >> '',$)' ) ' Scale factor?'
          READ( *, '(A)', IOSTAT=IOS ) SFAC_C
          IF ( IOS .NE. 0 ) THEN
            CALL MSG_WRITE( '*** Operation skipped', REPORT )
            CALL FILE_DEL( HIT )
            GO TO 190
          END IF
          READ( SFAC_C, '(BN,F25.0)', IOSTAT=IOS ) SFAC
          IF ( ( IOS .NE. 0 ) .OR. ( SFAC_C .EQ. ' ' ) ) THEN
            WRITE( *, * ) '*** Invalid scale factor'
            GO TO 102
          END IF
        ELSE
          SFAC = SFAC_CL
        END IF

        ! Get new data type
        IF ( TYPE_CL .EQ. ' ' ) THEN
          WRITE( *, '(/A,51X,A/'' >> '',$)' )
     &       ' Output data type?', '[no change]'
          READ( *, '(A)', IOSTAT=IOS ) TYPE
          IF ( IOS .NE. 0 ) THEN
            CALL MSG_WRITE( '*** Operation skipped', REPORT )
            CALL FILE_DEL( HIT )
            GO TO 190
          END IF
          CALL STR_UP( TYPE )
        ELSE IF ( TYPE_CL .NE. 'no_change' ) THEN
          TYPE = TYPE_CL
        ELSE
          TYPE = ' '
        END IF

        ! Get new units
        IF ( UNITS_CL .EQ. ' ' ) THEN
          WRITE( *, '(/A,55X,A/'' >> '',$)' )
     &     ' Output units?', '[no change]'
          READ( *, '(A)', IOSTAT=IOS ) UNITS
          IF ( IOS .NE. 0 ) THEN
            CALL MSG_WRITE( '*** Operation skipped', REPORT )
            CALL FILE_DEL( HIT )
            GO TO 190
          END IF
          CALL STR_UP( UNITS )
        ELSE IF ( UNITS_CL .NE. 'no_change' ) THEN
          UNITS = UNITS_CL
        ELSE
          UNITS = ' '
        END IF

      END IF

      ! Read UDS file
      CALL REP_COLS( REPORT )
      CALL UDS_GET( FILE_NAME, U, C, REPORT, IOS )
      IF ( IOS .NE. 0 ) GO TO 190

      IF ( .NOT. GROUP ) THEN ! Interactive prompts

        ! Get axis
  103   IF ( AXIS_CL .EQ. ' ' ) THEN
          WRITE( *, * )
          WRITE( *, * ) '  X-axis is ', U.XTYP(:L_TRIM(U.XTYP)),
     &     ' ('//U.XUNITS(:L_TRIM(U.XUNITS))//')'
          WRITE( *, * ) '  Y-axis is ', U.YTYP(:L_TRIM(U.YTYP)),
     &     ' ('//U.YUNITS(:L_TRIM(U.YUNITS))//')'
          WRITE( *, '(/A,54X,A/'' >> '',$)' )
     &     ' Axis to scale?   (X/Y)', '[Y]'
          READ( *, '(A)', END=190 ) AXIS
          CALL STR_UP( AXIS )
          IF ( .NOT. ANY_CHARS( AXIS, ' XY' ) ) THEN
            WRITE( *, * ) '*** Invalid response'
            GO TO 103
          END IF
          IF ( AXIS .EQ. ' ' ) AXIS = 'Y'
        ELSE
          AXIS = AXIS_CL
        END IF

        ! Get scale factor
  104   IF ( SFAC_CL .EQ. 0.D0 ) THEN
          WRITE( *, '(/A/'' >> '',$)' ) ' Scale factor?'
          READ( *, '(A)', END=190 ) SFAC_C
          READ( SFAC_C, '(BN,F25.0)', IOSTAT=IOS ) SFAC
          IF ( ( IOS .NE. 0 ) .OR. ( SFAC_C .EQ. ' ' ) ) THEN
            WRITE( *, * ) '*** Invalid scale factor'
            GO TO 104
          END IF
        ELSE
          SFAC = SFAC_CL
        END IF

        ! Get new data type
        IF ( TYPE_CL .EQ. ' ' ) THEN
          IF ( AXIS .EQ. 'X' ) THEN
            TYPE = U.XTYP
          ELSE
            TYPE = U.YTYP
          END IF
          WRITE( *, '(/A,40X,A/'' >> '',$)' )
     &     ' Output data type?', '['//TYPE//']'
          READ( *, '(A)', END=190 ) TYPE
          CALL STR_UP( TYPE )
        ELSE IF ( TYPE_CL .NE. 'no_change' ) THEN
          TYPE = TYPE_CL
        ELSE
          TYPE = ' '
        END IF

        ! Get new units
        IF ( UNITS_CL .EQ. ' ' ) THEN
          IF ( AXIS .EQ. 'X' ) THEN
            UNITS = U.XUNITS
          ELSE
            UNITS = U.YUNITS
          END IF
          WRITE( *, '(/A,44X,A/'' >> '',$)' )
     &     ' Output units?', '['//UNITS//']'
          READ( *, '(A)', END=190 ) UNITS
          CALL STR_UP( UNITS )
        ELSE IF ( UNITS_CL .NE. 'no_change' ) THEN
          UNITS = UNITS_CL
        ELSE
          UNITS = ' '
        END IF

      END IF

      ! Perform scaling
      IF ( AXIS .EQ. 'X' ) THEN
        IF ( U.CHANFORM .EQ. 'X-Y' ) THEN
          DO I = U.NFP, U.NLP
            U.X(I) = REAL( U.X(I) * SFAC )
          END DO
        END IF
        IF ( U.DEL .NE. 0. ) U.DEL = REAL( U.DEL * SFAC )
      ELSE
        DO I = U.NFP, U.NLP
          U.Y(I) = REAL( U.Y(I) * SFAC )
        END DO
      END IF

      ! Prepare output fields
      U.STATUS = 'SCALED'
      U.HIC = 0.
      U.T1 = 0.
      U.T2 = 0.
      U.HICDTUP = 0.
      U.CLIP3M = 0.
      U.CSI = 0.
      IF ( TYPE .NE. ' ' ) THEN
        IF ( AXIS .EQ. 'X' ) THEN
          U.XTYP = TYPE
        ELSE
          U.YTYP = TYPE
        END IF
      END IF
      IF ( UNITS .NE. ' ' ) THEN
        IF ( AXIS .EQ. 'X' ) THEN
          U.XUNITS = UNITS
        ELSE
          U.YUNITS = UNITS
        END IF
      END IF
      IF ( U.SCLFAC .NE. 0. ) THEN
        U.SCLFAC = REAL( U.SCLFAC * SFAC )
      ELSE
        U.SCLFAC = REAL( SFAC )
      END IF
      U.CD2 = AXIS//'-data scaled by '
      WRITE( U.CD2(20:), '(G15.6)', IOSTAT=IOS_W ) SFAC
      IF ( REPORT.OPEN ) WRITE( REPORT.UNIT, '(5X,A,G15.6)' )
     & AXIS//'-data scaled by ', SFAC

      ! Set default output file name
      OUT_FILE = FILE_NAME
      CALL FN_EXTRACT( OUT_FILE )
      IF ( .NOT. NO_FLAG ) CALL FN_FLAG( OUT_FILE, 's', 2, I )
      CALL ADD_PATH( DIR_OUT, OUT_FILE )
      IF ( .NOT. NO_INCR ) CALL FN_INCR( OUT_FILE )
      CALL FN_NAME_EXTRACT( OUT_FILE )
      DEF_FILE = OUT_FILE

      ! Set the output UDS file name
      IF ( .NOT. BATCH ) THEN
  120   WRITE( *, '(/A/3A/'' >> '',$)' )
     &   ' Output UDS file name?',
     &   ' [', DEF_FILE(:L_TRIM(DEF_FILE)), ']'
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
     & DIR_OUT, .FALSE., IOS )

      ! Loop for next file
  190 WRITE( *, '(80(''_''))' )
      GO TO 100

      ! Clean up and stop
  199 CALL CLEAN_UP( HIT, REPORT, OUTPUT_LIST, DIR_OUT )

      END



      SUBROUTINE SCALE_CL( CL_ARG, NUM_ARGS, PROG_NAME,
     & AXIS_CL, SFAC_CL, TYPE_CL, UNITS_CL, NO_FLAG )

!***********************************************************************
!* Processes the Program-Specific Command Line Arguments
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) CL_ARG(0:*) ! Command line argument array

      INTEGER NUM_ARGS ! Number of arguments

      CHARACTER*(*) PROG_NAME ! Program name

      CHARACTER AXIS_CL ! Axis to scale (X or Y)

      DOUBLE PRECISION SFAC_CL ! Scale factor

      CHARACTER*(*) TYPE_CL ! New data type

      CHARACTER*(*) UNITS_CL ! New data units

      LOGICAL NO_FLAG ! Indicates not to flag output file names


      ! Variables ______________________________________________________

      INTEGER I, NS, IOS

      CHARACTER ARG*255, C


      ! Process command line arguments
      AXIS_CL = ' '
      SFAC_CL = 0.D0
      TYPE_CL = ' '
      UNITS_CL = ' '
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
     &   ( ARG(NS:) .EQ. '??' ) .OR.
     &   ( ARG(NS:) .EQ. 'HELP' ) ) THEN ! Show command line syntax
          CALL SYNTAX_CL( PROG_NAME )
          WRITE( *, '(10X,A)' )
     &     '[AXIS=<axis>] [FAC=<factor>] '//
     &     '[TYPE=<type>] [UNITS=<units>] [NO_FLAG]'
          IF ( ( ARG(NS:) .EQ. '??' ) .OR.
     &     ( ARG(NS:) .EQ. 'HELP' ) ) THEN ! Show command line help
            CALL HELP_CL( PROG_NAME )
            WRITE( *, '(4(10X,A/),10X,A)' )
     &       '<axis> :  Axis to scale (X or Y)',
     &       '<factor> :  Scale factor',
     &       '<type> :  '//
     &       'New data type for the scaled axis [no change]',
     &       '<units> :  '//
     &       'New units for the scaled axis [no change]',
     &       'NO_FLAG :  Don''t flag output file names'
          END IF
          STOP ' '
        ELSE IF ( ( ARG(NS:NS+4) .EQ. 'AXIS=' ) .OR.
     &   ( ARG(NS:NS+4) .EQ. 'AXIS#' ) ) THEN
          AXIS_CL = ARG(NS+5:NS+5)
          IF ( .NOT. ANY_CHARS( AXIS_CL, 'XY' ) ) THEN
            WRITE( *, '(/2A)' ) ' *** Illegal scale axis: ', AXIS_CL
            AXIS_CL = ' '
          END IF
          CL_ARG(I) = ' '
        ELSE IF ( ( ARG(NS:NS+3) .EQ. 'FAC=' ) .OR.
     &   ( ARG(NS:NS+3) .EQ. 'FAC#' ) ) THEN
          READ( ARG(NS+4:), '(BN,F25.0)', IOSTAT=IOS ) SFAC_CL
          IF ( IOS .NE. 0 ) THEN
            WRITE( *, '(/2A)', IOSTAT=IOS )
     &       ' *** Illegal scale factor: ',
     &       ARG(NS+4:L_TRIM(ARG))
            SFAC_CL = 0.D0
          END IF
          CL_ARG(I) = ' '
        ELSE IF ( ( ARG(NS:NS+4) .EQ. 'TYPE=' ) .OR.
     &   ( ARG(NS:NS+4) .EQ. 'TYPE#' ) ) THEN
          TYPE_CL = ARG(NS+5:)
          IF ( TYPE_CL .EQ. ' ' ) TYPE_CL = 'no_change'
          CL_ARG(I) = ' '
        ELSE IF ( ARG(NS:) .EQ. 'TYPE' ) THEN
          TYPE_CL = 'no_change'
          CL_ARG(I) = ' '
        ELSE IF ( ( ARG(NS:NS+5) .EQ. 'UNITS=' ) .OR.
     &   ( ARG(NS:NS+5) .EQ. 'UNITS#' ) ) THEN
          UNITS_CL = ARG(NS+6:)
          IF ( UNITS_CL .EQ. ' ' ) UNITS_CL = 'no_change'
          CL_ARG(I) = ' '
        ELSE IF ( ARG(NS:) .EQ. 'UNITS' ) THEN
          UNITS_CL = 'no_change'
          CL_ARG(I) = ' '
        ELSE IF ( ( ARG(NS:) .EQ. 'NO_FLAG' ) .OR.
     &   ( ARG(NS:) .EQ. 'NOFLAG' ) ) THEN
          NO_FLAG = .TRUE.
          CL_ARG(I) = ' '
        END IF
      END DO

      RETURN
      END
