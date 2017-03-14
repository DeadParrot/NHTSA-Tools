      PROGRAM INTEG

!***********************************************************************
!* Integrates UDS Files using the Trapezoid Rule
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
     & INI_VAL, NO_FLAG, STANDARD, MSG, NO_MSG

      PARAMETER ( MSG=.TRUE., NO_MSG=.FALSE. )

      INTEGER NUM_ARGS, MAX_ARGS, IOS, I

      PARAMETER ( MAX_ARGS = 12 )

      DOUBLE PRECISION Z01, Z02

      CHARACTER CL_ARG(0:MAX_ARGS)*255, PROG_NAME*8,
     & TEMPLATE_STR*255, DIR_OUT*255,
     & FILE_NAME*255, OUT_FILE*255, DEF_FILE*255,
     & PROMPT*79,
     & INTEGRALS*2,
     & Z1_OUT, Z01_C*25, Z01_F, Z2_OUT, Z02_C*25, Z02_F,
     & ZTYP*20, ZUNITS*20, ZFLAG


      ! Initializations
      PROG_NAME = 'Integ'
      CALL FILE_INIT( HIT )
      CALL GET_CL( CL_ARG, NUM_ARGS, MAX_ARGS )
      CALL INTEG_CL( CL_ARG, NUM_ARGS, PROG_NAME,
     & INTEGRALS, INI_VAL, NO_FLAG )
      CALL PROC_CL( CL_ARG, NUM_ARGS, PROG_NAME, TEMPLATE_STR,
     & ALL_BATCH, NO_BATCH, NO_INCR, REPORT, OUTPUT_LIST, DIR_OUT,
     & ONE_PASS, .TRUE., .TRUE. )
      PROMPT = 'UDS file name/template/hitlist?'
      PROMPT(74:) = '[done]'
      CALL UDS_CONTROL_INIT( C )

      WRITE( *, '(//26X,A//)' ) '**  Integrate UDS Files  **'

      ! Read the file name/template/hitlist
  100 CALL FILE_PROMPT( TEMPLATE_STR, PROMPT, DIR_OUT,
     & ONE_PASS, HIT, NEW_HIT, FILE_NAME, EOF )
      IF ( ( FILE_NAME .EQ. ' ' ) .OR. ( EOF ) ) GO TO 199 ! Done

      ! Batch/group setup
      CALL BAT_SET( ALL_BATCH, NO_BATCH, NEW_HIT, HIT, BATCH )
      CALL GROUP_SET( HIT, NEW_HIT, BATCH, 2, GROUP, NEW_GROUP )

      ! Group prompts
      IF ( NEW_GROUP ) THEN

        ! First integrals
  101   IF ( INTEGRALS(1:1) .NE. ' ' ) THEN
          Z1_OUT = 'Y'
        ELSE IF ( INTEGRALS .NE. ' ' ) THEN
          Z1_OUT = 'N'
        ELSE
          WRITE( *, '(/A,50X,A/'' >> '',$)' )
     &       ' First integrals?   (Y/N)', '[Yes]'
          READ( *, '(A)', IOSTAT=IOS ) Z1_OUT
          IF ( IOS .NE. 0 ) THEN
            CALL MSG_WRITE( '*** Operation skipped', REPORT )
            CALL FILE_DEL( HIT )
            GO TO 190
          END IF
          CALL STR_UP( Z1_OUT )
          IF ( .NOT. ANY_CHARS( Z1_OUT, ' YN' ) ) THEN
            WRITE( *, * ) '*** Invalid response'
            GO TO 101
          END IF
        END IF
  102   IF ( INI_VAL ) THEN
          WRITE( *, '(/A,41X,A/'' >> '',$)' )
     &       ' Initial first integral value?', '[default]'
          READ( *, '(A)', IOSTAT=IOS ) Z01_C
          IF ( IOS .NE. 0 ) THEN
            CALL MSG_WRITE( '*** Operation skipped', REPORT )
            CALL FILE_DEL( HIT )
            GO TO 190
          END IF
          IF ( Z01_C .NE. ' ' ) THEN
            READ( Z01_C, '(BN,F25.0)', IOSTAT=IOS ) Z01
            IF ( IOS .NE. 0 ) THEN
              WRITE( *, * ) '*** Invalid initial value'
              GO TO 102
            END IF
            Z01_F = 'V'
          ELSE
            Z01_F = ' '
          END IF
        ELSE
          Z01_F = ' '
        END IF

        ! Second integrals
  103   IF ( INTEGRALS(2:2) .NE. ' ' ) THEN
          Z2_OUT = 'Y'
        ELSE IF ( INTEGRALS .NE. ' ' ) THEN
          Z2_OUT = 'N'
        ELSE
          WRITE( *, '(/A,49X,A/'' >> '',$)' )
     &       ' Second integrals?   (Y/N)', '[Yes]'
          READ( *, '(A)', IOSTAT=IOS ) Z2_OUT
          IF ( IOS .NE. 0 ) THEN
            CALL MSG_WRITE( '*** Operation skipped', REPORT )
            CALL FILE_DEL( HIT )
            GO TO 190
          END IF
          CALL STR_UP( Z2_OUT )
          IF ( .NOT. ANY_CHARS( Z2_OUT, ' YN' ) ) THEN
            WRITE( *, * ) '*** Invalid response'
            GO TO 103
          END IF
        END IF
  104   IF ( INI_VAL ) THEN
          IF ( Z2_OUT .NE. 'N' ) THEN
            WRITE( *, '(/A,40X,A/'' >> '',$)' )
     &         ' Initial second integral value?', '[default]'
            READ( *, '(A)', IOSTAT=IOS ) Z02_C
            IF ( IOS .NE. 0 ) THEN
              CALL MSG_WRITE( '*** Operation skipped', REPORT )
              CALL FILE_DEL( HIT )
              GO TO 190
            END IF
            IF ( Z02_C .NE. ' ' ) THEN
              READ( Z02_C, '(BN,F25.0)', IOSTAT=IOS ) Z02
              IF ( IOS .NE. 0 ) THEN
                WRITE( *, * ) '*** Invalid initial value'
                GO TO 104
              END IF
              Z02_F = 'V'
            ELSE
              Z02_F = ' '
            END IF
          END IF
        ELSE
          Z02_F = ' '
        END IF

        IF ( ( Z1_OUT .EQ. 'N' ) .AND. ( Z2_OUT .EQ. 'N' ) ) THEN
          WRITE( *, * ) '*** No integrals requested'
          GO TO 101
        END IF

      END IF

      ! Read UDS file
      CALL REP_COLS( REPORT )
      CALL UDS_GET( FILE_NAME, U, C, REPORT, IOS )
      IF ( IOS .NE. 0 ) GO TO 190

      ! Check file type
      IF ( ( U.DIMSYS .NE. 'MET' ) .AND.
     & ( U.DIMSYS .NE. 'SI' ) .AND.
     & ( U.DIMSYS .NE. 'ENG' ) ) THEN
        CALL MSG_WRITE(
     &     '*** ERROR - Unsupported dimensional system: '//U.DIMSYS,
     &     REPORT )
        GO TO 190
      ELSE IF ( ( U.CHANFORM .NE. 'Y' ) .AND.
     & ( U.CHANFORM .NE. 'X-Y' ) ) THEN
        CALL MSG_WRITE( '*** ERROR - Unsupported channel format: '//
     &     U.CHANFORM, REPORT )
        GO TO 190
      END IF

      ! Set flags for initial value handling
      IF ( .NOT. GROUP ) THEN ! Set whether to prompt for initial values
        IF ( INI_VAL ) THEN ! Prompt for initial values
          Z01_F = 'P'
          Z02_F = 'P'
        ELSE ! Don't prompt for initial values
          Z01_F = ' '
          Z02_F = ' '
        END IF
      END IF

      ! Get the first integral type
      CALL INTEG_TYPE( U.DIMSYS, U.CHANFORM,
     & U.XTYP, U.XUNITS, U.YTYP, U.YUNITS, ZTYP, ZUNITS,
     & ZFLAG, STANDARD, MSG, IOS )
      IF ( IOS .NE. 0 ) GO TO 190

      ! Set default output file name
      OUT_FILE = FILE_NAME
      CALL FN_EXTRACT( OUT_FILE )
      IF ( ( U.CHANFORM .EQ. 'Y' ) .AND. ( .NOT. NO_FLAG ) ) THEN
        CALL FN_FLAG( OUT_FILE, ZFLAG//'c', 3, I )
      ELSE ! Just flag Y type
        CALL FN_FLAG( OUT_FILE, ZFLAG, 3, I )
      END IF
      CALL ADD_PATH( DIR_OUT, OUT_FILE )
      IF ( .NOT. NO_INCR ) CALL FN_INCR( OUT_FILE )
      CALL FN_NAME_EXTRACT( OUT_FILE )
      DEF_FILE = OUT_FILE

      ! Prepare output fields
      U.STATUS = 'COMPUTED'
      U.HIC = 0.
      U.T1 = 0.
      U.T2 = 0.
      U.HICDTUP = 0.
      U.CLIP3M = 0.
      U.CSI = 0.

      ! Get the first integral UDS file name
      IF ( .NOT. BATCH ) THEN
  120   IF ( ( INTEGRALS .EQ. ' ' ) .OR.
     &     ( INTEGRALS(1:1) .NE. ' ' ) ) THEN
          WRITE( *, '(/1X,2A/3A/'' >> '',$)' )
     &       ZTYP, ' file name?   ( N - None )',
     &     ' [', DEF_FILE(:L_TRIM(DEF_FILE)), ']'
          READ( *, '(A)', END=190 ) OUT_FILE
          IF ( OUT_FILE .EQ. ' ' ) THEN ! Use default file name
            OUT_FILE = DEF_FILE
          ELSE IF ( ( OUT_FILE .EQ. 'N' ) .OR.
     &       ( OUT_FILE .EQ. 'n' ) ) THEN ! No output file
            OUT_FILE(1:1) = 'N'
          ELSE ! Check file name validity
            CALL ADD_PATH( DIR_OUT, OUT_FILE )
            IF ( .NOT. FN_VALID( OUT_FILE ) ) THEN
              WRITE( *, * ) '*** Invalid file name'
              GO TO 120
            END IF
          END IF
        ELSE
          OUT_FILE = 'N'
        END IF
      ELSE ! Batch
        IF ( Z1_OUT .EQ. 'N' ) THEN
          OUT_FILE = 'N'
        ELSE
          OUT_FILE = DEF_FILE
        END IF
      END IF

      ! Compute the first integral
      CALL INTEG_S( U.DIMSYS, U.CHANFORM,
     & U.X(U.NFP), U.Y(U.NFP),
     & U.NFP, U.NLP, U.DEL, U.INIVEL,
     & U.XTYP, U.XUNITS, U.YTYP, U.YUNITS,
     & ZFLAG, Z01_F, Z01, NO_MSG, IOS )
      IF ( IOS .NE. 0 ) GO TO 190

      ! Write the first integral UDS file
      IF ( OUT_FILE .NE. 'N' ) THEN
        CALL UDS_PUT( OUT_FILE, U, C, REPORT, OUTPUT_LIST,
     &     DIR_OUT, .TRUE., IOS )
        IF ( IOS .NE. 0 ) GO TO 190
      END IF

      ! Get the second integral type
      CALL INTEG_TYPE( U.DIMSYS, U.CHANFORM,
     & U.XTYP, U.XUNITS, U.YTYP, U.YUNITS, ZTYP, ZUNITS,
     & ZFLAG, STANDARD, MSG, IOS )
      IF ( IOS .NE. 0 ) GO TO 190

      ! Set default output file name
      OUT_FILE = FILE_NAME
      CALL FN_EXTRACT( OUT_FILE )
      IF ( ( U.CHANFORM .EQ. 'Y' ) .AND. ( .NOT. NO_FLAG ) ) THEN
        CALL FN_FLAG( OUT_FILE, ZFLAG//'c', 3, I )
      ELSE
        CALL FN_FLAG( OUT_FILE, ZFLAG, 3, I )
      END IF
      CALL ADD_PATH( DIR_OUT, OUT_FILE )
      IF ( .NOT. NO_INCR ) CALL FN_INCR( OUT_FILE )
      CALL FN_NAME_EXTRACT( OUT_FILE )
      DEF_FILE = OUT_FILE

      ! Get the second integral UDS file name
      IF ( .NOT. BATCH ) THEN
  130   IF ( ( ( STANDARD ) .AND.
     &     ( INTEGRALS .EQ. ' ' ) ) .OR.
     &     ( INTEGRALS(2:2) .NE. ' ' ) ) THEN
          WRITE( *, '(/1X,2A/3A/'' >> '',$)' )
     &       ZTYP, ' file name?   ( N - None )',
     &     ' [', DEF_FILE(:L_TRIM(DEF_FILE)), ']'
          READ( *, '(A)', END=190 ) OUT_FILE
          IF ( OUT_FILE .EQ. ' ' ) THEN ! Use default file name
            OUT_FILE = DEF_FILE
          ELSE IF ( ( OUT_FILE .EQ. 'N' ) .OR.
     &       ( OUT_FILE .EQ. 'n' ) ) THEN ! No output file
            OUT_FILE(1:1) = 'N'
          ELSE ! Check file name validity
            CALL ADD_PATH( DIR_OUT, OUT_FILE )
            IF ( .NOT. FN_VALID( OUT_FILE ) ) THEN
              WRITE( *, * ) '*** Invalid file name'
              GO TO 130
            END IF
          END IF
        ELSE
          OUT_FILE = 'N'
        END IF
      ELSE ! Batch
        IF ( Z2_OUT .EQ. 'N' ) THEN
          OUT_FILE = 'N'
        ELSE
          OUT_FILE = DEF_FILE
        END IF
      END IF

      ! Compute/write the second integral
      IF ( OUT_FILE .NE. 'N' ) THEN
        CALL INTEG_S( U.DIMSYS, U.CHANFORM,
     &     U.X(U.NFP), U.Y(U.NFP),
     &     U.NFP, U.NLP, U.DEL, U.INIVEL,
     &     U.XTYP, U.XUNITS, U.YTYP, U.YUNITS,
     &     ZFLAG, Z02_F, Z02, NO_MSG, IOS )
        IF ( IOS .NE. 0 ) GO TO 190
        CALL UDS_PUT( OUT_FILE, U, C, REPORT, OUTPUT_LIST,
     &     DIR_OUT, .TRUE., IOS )
      END IF

      ! Loop for next file
  190 WRITE( *, '(80(''_''))' )
      GO TO 100

      ! Clean up and stop
  199 CALL CLEAN_UP( HIT, REPORT, OUTPUT_LIST, DIR_OUT )

      END



      SUBROUTINE INTEG_CL( CL_ARG, NUM_ARGS, PROG_NAME,
     & INTEGRALS, INI_VAL, NO_FLAG )

!***********************************************************************
!* Processes the Program-Specific Command Line Arguments
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) CL_ARG(0:*) ! Command line argument array

      INTEGER NUM_ARGS ! Number of arguments

      CHARACTER*(*) PROG_NAME ! Program name

      CHARACTER*(*) INTEGRALS ! Indicates first &/or second integral(s)

      LOGICAL INI_VAL ! Indicates to use specified/file initial value

      LOGICAL NO_FLAG ! Indicates not to flag output file names


      ! Variables ______________________________________________________

      INTEGER I, NS

      CHARACTER ARG*255, C


      ! Process command line arguments
      INTEGRALS = ' '
      INI_VAL = .FALSE.
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
     &       '[FIRST|SECOND|BOTH] [INI_VAL] [NO_FLAG]'
          IF ( ( ARG(NS:) .EQ. '??' ) .OR.
     &       ( ARG(NS:) .EQ. 'HELP' ) ) THEN ! Show command line help
            CALL HELP_CL( PROG_NAME )
            WRITE( *, '(4(10X,A/),10X,A)' )
     &         'FIRST :  First integrals only',
     &         'SECOND :  Second integrals only',
     &         'BOTH :  First and second integrals',
     &         'INI_VAL :  Allow initial value control',
     &         'NO_FLAG :  Don''t flag output file names'
          END IF
          STOP ' '
        ELSE IF ( ARG(NS:) .EQ. 'FIRST' ) THEN
          INTEGRALS = 'X '
          CL_ARG(I) = ' '
        ELSE IF ( ARG(NS:) .EQ. 'SECOND' ) THEN
          INTEGRALS = ' X'
          CL_ARG(I) = ' '
        ELSE IF ( ARG(NS:) .EQ. 'BOTH' ) THEN
          INTEGRALS = 'XX'
          CL_ARG(I) = ' '
        ELSE IF ( ( ARG(NS:) .EQ. 'INI_VAL' ) .OR.
     &     ( ARG(NS:) .EQ. 'INIVAL' ) ) THEN
          INI_VAL = .TRUE.
          CL_ARG(I) = ' '
        ELSE IF ( ( ARG(NS:) .EQ. 'NO_FLAG' ) .OR.
     &     ( ARG(NS:) .EQ. 'NOFLAG' ) ) THEN
          NO_FLAG = .TRUE.
          CL_ARG(I) = ' '
        END IF
      END DO

      RETURN
      END
