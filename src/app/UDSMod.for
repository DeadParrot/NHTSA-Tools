      PROGRAM UDSMOD

!***********************************************************************
!* UDS File Field Modification
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
     & BATCH, GROUP, NEW_GROUP,
     & NOTE, NO_FLAG, MSG

      PARAMETER ( MSG = .TRUE. )

      INTEGER NUM_ARGS, MAX_ARGS, IOS, I, NUM_MODS, MAX_MODS

      PARAMETER ( MAX_ARGS = 12, MAX_MODS = 100 )

      CHARACTER CL_ARG(0:MAX_ARGS)*255, PROG_NAME*8,
     & TEMPLATE_STR*255, DIR_OUT*255,
     & FILE_NAME*255, OUT_FILE*255, DEF_FILE*255,
     & PROMPT*79,
     & FIELD_NAME_CL*25, FIELD_VALUE_CL*128, NULL,
     & FIELD_NAME*25, FIELD_VALUE*128, REP_LINE*132,
     & FIELD_NAMES(MAX_MODS)*25, FIELD_VALUES(MAX_MODS)*128

      PARAMETER ( NULL = CHAR(0) )


      ! Initializations
      PROG_NAME = 'UDSMod'
      CALL FILE_INIT( HIT )
      CALL GET_CL( CL_ARG, NUM_ARGS, MAX_ARGS )
      CALL UDSMOD_CL( CL_ARG, NUM_ARGS, PROG_NAME,
     & FIELD_NAME_CL, FIELD_VALUE_CL, NOTE, NO_FLAG )
      CALL PROC_CL( CL_ARG, NUM_ARGS, PROG_NAME, TEMPLATE_STR,
     & ALL_BATCH, NO_BATCH, NO_INCR, REPORT, OUTPUT_LIST, DIR_OUT,
     & ONE_PASS, .TRUE., .TRUE. )
      PROMPT = 'UDS file name/template/hitlist?'
      PROMPT(74:) = '[done]'
      CALL UDS_CONTROL_INIT( C )
      C.FILL_X = .TRUE. ! In case CHANFORM changed to X-Y

      WRITE( *, '(//24X,A//)' ) '**  UDS Field Modification  **'

      ! Read the file name/template/hitlist
  100 CALL FILE_PROMPT( TEMPLATE_STR, PROMPT, DIR_OUT,
     & ONE_PASS, HIT, NEW_HIT, FILE_NAME, EOF )
      IF ( ( FILE_NAME .EQ. ' ' ) .OR. ( EOF ) ) GO TO 199 ! Done

      ! Batch setup
      CALL BAT_SET( ALL_BATCH, NO_BATCH, NEW_HIT, HIT, BATCH )
      CALL GROUP_SET( HIT, NEW_HIT, BATCH, 2, GROUP, NEW_GROUP )

      ! Group prompts
      IF ( NEW_GROUP ) THEN

        ! Get new field name
        NUM_MODS = 0
  101   IF ( FIELD_NAME_CL .EQ. ' ' ) THEN
          WRITE( *, '(/A/'' >> '',$)' )
     &       ' UDS field to modify?   ( ? - list fields )'
          READ( *, '(A)', IOSTAT=IOS ) FIELD_NAME
          IF ( IOS .NE. 0 ) THEN
            CALL MSG_WRITE( '*** Operation skipped', REPORT )
            CALL FILE_DEL( HIT )
            GO TO 190
          END IF
        ELSE
          FIELD_NAME = FIELD_NAME_CL
        END IF

        ! Check field name
        CALL UDS_SPEC_CHK( FIELD_NAME, MSG, IOS )
        IF ( IOS .NE. 0 ) THEN
          IF ( ( FIELD_NAME_CL .EQ. ' ' ) .OR.
     &       ( FIELD_VALUE_CL .EQ. NULL ) .OR.
     &       ( .NOT. ONE_PASS ) ) THEN
            FIELD_NAME_CL = ' '
            GO TO 101
          ELSE
            CALL MSG_WRITE( '*** Operation skipped', REPORT )
            CALL FILE_DEL( HIT )
            GO TO 190
          END IF
        END IF

        ! Get new field value
  102   IF ( FIELD_VALUE_CL .EQ. NULL ) THEN
          WRITE( *, '(/A/'' >> '',$)' ) ' New field value?'
          READ( *, '(A)', IOSTAT=IOS ) FIELD_VALUE
          IF ( IOS .NE. 0 ) THEN
            CALL MSG_WRITE( '*** Operation skipped', REPORT )
            CALL FILE_DEL( HIT )
            GO TO 190
          END IF
        ELSE
          FIELD_VALUE = FIELD_VALUE_CL
        END IF

        ! Check field value
        CALL UDS_SPEC_SET( U, FIELD_NAME, FIELD_VALUE, MSG, IOS )
        IF ( IOS .NE. 0 ) THEN
          IF ( ( FIELD_NAME_CL .EQ. ' ' ) .OR.
     &       ( FIELD_VALUE_CL .EQ. NULL ) .OR.
     &       ( .NOT. ONE_PASS ) ) THEN
            FIELD_VALUE_CL = NULL
            GO TO 102
          ELSE
            CALL MSG_WRITE( '*** Operation skipped', REPORT )
            CALL FILE_DEL( HIT )
            GO TO 190
          END IF
        END IF

        ! Set first modification array elements
        NUM_MODS = 1
        FIELD_NAMES(1) = FIELD_NAME
        FIELD_VALUES(1) = FIELD_VALUE

        ! Get more modifications
  103   IF ( ( FIELD_NAME_CL .EQ. ' ' ) .AND.
     &     ( NUM_MODS .LT. MAX_MODS ) ) THEN
          WRITE( *, '(/A,26X,A/'' >> '',$)' )
     &       ' Next UDS field to modify?   ( ? - list fields )',
     &       '[done]'
          READ( *, '(A)', IOSTAT=IOS ) FIELD_NAME
          IF ( IOS .NE. 0 ) THEN
            CALL MSG_WRITE( '*** Operation skipped', REPORT )
            CALL FILE_DEL( HIT )
            GO TO 190
          END IF
          IF ( FIELD_NAME .NE. ' ' ) THEN
            ! Check field name
            CALL UDS_SPEC_CHK( FIELD_NAME, MSG, IOS )
            IF ( IOS .NE. 0 ) GO TO 103
            ! Get new field value
  104       WRITE( *, '(/A/'' >> '',$)' ) ' New field value?'
            READ( *, '(A)', END=103, IOSTAT=IOS ) FIELD_VALUE
            IF ( IOS .NE. 0 ) THEN
              CALL MSG_WRITE( '*** Operation skipped', REPORT )
              CALL FILE_DEL( HIT )
              GO TO 190
            END IF
            ! Change field value
            CALL UDS_SPEC_SET( U, FIELD_NAME, FIELD_VALUE, MSG,
     &         IOS )
            IF ( IOS .NE. 0 ) GO TO 104
            ! Set modification array elements
            NUM_MODS = NUM_MODS + 1
            FIELD_NAMES(NUM_MODS) = FIELD_NAME
            FIELD_VALUES(NUM_MODS) = FIELD_VALUE
            GO TO 103
          END IF
        END IF
      END IF

      ! Read UDS file
      CALL REP_COLS( REPORT )
      CALL UDS_GET( FILE_NAME, U, C, REPORT, IOS )
      IF ( IOS .NE. 0 ) GO TO 190

      IF ( .NOT. GROUP ) THEN ! Interactive prompts

        ! Get new field name
  110   IF ( FIELD_NAME_CL .EQ. ' ' ) THEN
          WRITE( *, '(/A/'' >> '',$)' )
     &       ' UDS field to modify?   ( ? - list fields )'
          READ( *, '(A)', IOSTAT=IOS ) FIELD_NAME
          IF ( IOS .NE. 0 ) GO TO 190
        ELSE
          FIELD_NAME = FIELD_NAME_CL
        END IF

        ! Check field name and display current value
        CALL UDS_SPEC_SHOW( U, FIELD_NAME, MSG, IOS )
        IF ( IOS .NE. 0 ) THEN
          FIELD_NAME_CL = ' '
          GO TO 110
        END IF

        ! Get new field value
  111   IF ( FIELD_VALUE_CL .EQ. NULL ) THEN
          WRITE( *, '(/A/'' >> '',$)' ) ' New field value?'
          READ( *, '(A)', IOSTAT=IOS ) FIELD_VALUE
          IF ( IOS .NE. 0 ) GO TO 190
        ELSE
          FIELD_VALUE = FIELD_VALUE_CL
        END IF

        ! Change field value
        CALL UDS_SPEC_SET( U, FIELD_NAME, FIELD_VALUE, MSG, IOS )
        IF ( IOS .NE. 0 ) THEN
          FIELD_VALUE_CL = NULL
          GO TO 111
        END IF

        ! Report modification
        REP_LINE = FIELD_NAME
        REP_LINE(LEN_TRIM(REP_LINE)+1:) =
     &     ' modified to '//FIELD_VALUE
        IF ( REPORT.OPEN ) WRITE( REPORT.UNIT, '(5X,A)', IOSTAT=IOS )
     &     REP_LINE(:L_TRIM(REP_LINE))

        ! Do more modifications
  112   IF ( FIELD_NAME_CL .EQ. ' ' ) THEN
          WRITE( *, '(/A,26X,A/'' >> '',$)' )
     &       ' Next UDS field to modify?   ( ? - list fields )',
     &       '[done]'
          READ( *, '(A)', IOSTAT=IOS ) FIELD_NAME
          IF ( IOS .NE. 0 ) GO TO 190
          IF ( FIELD_NAME .NE. ' ' ) THEN
            ! Check field name and display current value
            CALL UDS_SPEC_SHOW( U, FIELD_NAME, MSG, IOS )
            IF ( IOS .NE. 0 ) GO TO 112
            ! Get new field value
  113       WRITE( *, '(/A/'' >> '',$)' ) ' New field value?'
            READ( *, '(A)', END=112, IOSTAT=IOS ) FIELD_VALUE
            IF ( IOS .NE. 0 ) GO TO 190
            ! Change field value
            CALL UDS_SPEC_SET( U, FIELD_NAME, FIELD_VALUE, MSG,
     &         IOS )
            IF ( IOS .NE. 0 ) GO TO 113
            ! Report modification
            REP_LINE = FIELD_NAME
            REP_LINE(LEN_TRIM(REP_LINE)+1:) =
     &         ' modified to '//FIELD_VALUE
            IF ( REPORT.OPEN )
     &         WRITE( REPORT.UNIT, '(5X,A)', IOSTAT=IOS )
     &         REP_LINE(:L_TRIM(REP_LINE))
            GO TO 112
          END IF
        END IF

      ELSE ! Group

        ! Perform modification(s)
        DO I = 1, NUM_MODS
          ! Change field value
          CALL UDS_SPEC_SET( U, FIELD_NAMES(I), FIELD_VALUES(I),
     &       MSG, IOS )
          ! Report modification
          REP_LINE = FIELD_NAMES(I)
          REP_LINE(LEN_TRIM(REP_LINE)+1:) =
     &       ' modified to '//FIELD_VALUES(I)
          IF ( REPORT.OPEN )
     &       WRITE( REPORT.UNIT, '(5X,A)', IOSTAT=IOS )
     &       REP_LINE(:L_TRIM(REP_LINE))
        END DO

      END IF

      ! Prepare output fields
      IF ( NOTE ) THEN
        IF ( FIELD_NAME .NE. 'STATUS' ) U.STATUS = 'MODIFIED'
      END IF

      ! Set default output file name
      OUT_FILE = FILE_NAME
      CALL FN_EXTRACT( OUT_FILE )
      IF ( .NOT. NO_FLAG ) CALL FN_FLAG( OUT_FILE, 'm', 2, I )
      CALL ADD_PATH( DIR_OUT, OUT_FILE )
      IF ( .NOT. NO_INCR ) CALL FN_INCR( OUT_FILE )
      CALL FN_NAME_EXTRACT( OUT_FILE )
      DEF_FILE = OUT_FILE

      ! Set the output UDS file name
      IF ( .NOT. BATCH ) THEN
  120   WRITE( *, '(/A/3A/'' >> '',$)' )
     &     ' Output UDS file name?',
     &     ' [', DEF_FILE(:L_TRIM(DEF_FILE)), ']'
        READ( *, '(A)', END=190 ) OUT_FILE
        IF ( OUT_FILE .EQ. ' ' ) THEN
          OUT_FILE = DEF_FILE
        ELSE IF ( .NOT. FN_VALID( OUT_FILE ) ) THEN
          WRITE( *, * ) '*** Invalid file name'
          GO TO 120
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



      SUBROUTINE UDSMOD_CL( CL_ARG, NUM_ARGS, PROG_NAME,
     & FIELD_NAME_CL, FIELD_VALUE_CL, NOTE, NO_FLAG )

!***********************************************************************
!* Processes the Program-Specific Command Line Arguments
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) CL_ARG(0:*) ! Command line argument array

      INTEGER NUM_ARGS ! Number of arguments

      CHARACTER*(*) PROG_NAME ! Program name

      CHARACTER*(*) FIELD_NAME_CL ! Field to modify

      CHARACTER*(*) FIELD_VALUE_CL ! New field value

      LOGICAL NOTE ! Indicates to note the modification in the file

      LOGICAL NO_FLAG ! Indicates not to flag output file names


      ! Variables ______________________________________________________

      INTEGER I, NS

      CHARACTER ARG*255, C, NULL

      PARAMETER ( NULL = CHAR(0) )


      ! Process command line arguments
      FIELD_NAME_CL = ' '
      FIELD_VALUE_CL = NULL
      NOTE = .FALSE.
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
     &       '[FIELD=<field>] [VALUE=<value>] [NOTE] [NO_FLAG]'
          IF ( ( ARG(NS:) .EQ. '??' ) .OR.
     &       ( ARG(NS:) .EQ. 'HELP' ) ) THEN ! Show command line help
            CALL HELP_CL( PROG_NAME )
            WRITE( *, '(3(10X,A/),10X,A)' )
     &         '<field> :  Name of UDS field to modify',
     &         '<value> :  New value for the field',
     &         'NOTE :  Note modification in file',
     &         'NO_FLAG :  Don''t flag output file names'
          END IF
          STOP ' '
        ELSE IF ( ( ARG(NS:NS+5) .EQ. 'FIELD=' ) .OR.
     &     ( ARG(NS:NS+5) .EQ. 'FIELD#' ) ) THEN
          FIELD_NAME_CL = ARG(NS+6:)
          CL_ARG(I) = ' '
        ELSE IF ( ( ARG(NS:NS+5) .EQ. 'VALUE=' ) .OR.
     &     ( ARG(NS:NS+5) .EQ. 'VALUE#' ) ) THEN
          FIELD_VALUE_CL = CL_ARG(I)(NS+6:)
          CL_ARG(I) = ' '
        ELSE IF ( ARG(NS:) .EQ. 'NOTE' ) THEN
          NOTE = .TRUE.
          CL_ARG(I) = ' '
        ELSE IF ( ( ARG(NS:) .EQ. 'NO_FLAG' ) .OR.
     &     ( ARG(NS:) .EQ. 'NOFLAG' ) ) THEN
          NO_FLAG = .TRUE.
          CL_ARG(I) = ' '
        END IF
      END DO

      RETURN
      END
