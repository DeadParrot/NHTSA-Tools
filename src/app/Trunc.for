      PROGRAM TRUNC

!***********************************************************************
!* Truncates and Subsamples UDS Files
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
     & BATCH, GROUP, NEW_GROUP, NO_FLAG,
     & SUBSAMPLE, INTERPOLATE

      INTEGER NUM_ARGS, MAX_ARGS, IOS, IOS_W, I,
     & NS, NFP_I, NLP_I, NFP_N, NLP_N

      PARAMETER ( MAX_ARGS = 12 )

      DOUBLE PRECISION XINUM, XFNUM, PDEL, DEL_NEW

      CHARACTER CL_ARG(0:MAX_ARGS)*255, PROG_NAME*8,
     & TEMPLATE_STR*255, DIR_OUT*255,
     & FILE_NAME*255, OUT_FILE*255, DEF_FILE*255,
     & PROMPT*79,
     & PTYP*20, PUNITS*20, XIVAL*25, XFVAL*25, SUBVAL*25


      ! Initializations
      PROG_NAME = 'Trunc'
      CALL FILE_INIT( HIT )
      CALL GET_CL( CL_ARG, NUM_ARGS, MAX_ARGS )
      CALL TRUNC_CL( CL_ARG, NUM_ARGS, PROG_NAME,
     & NO_FLAG )
      CALL PROC_CL( CL_ARG, NUM_ARGS, PROG_NAME, TEMPLATE_STR,
     & ALL_BATCH, NO_BATCH, NO_INCR, REPORT, OUTPUT_LIST, DIR_OUT,
     & ONE_PASS, .TRUE., .TRUE. )
      PROMPT = 'UDS file name/template/hitlist?'
      PROMPT(74:) = '[done]'
      CALL UDS_CONTROL_INIT( C )
      SUBSAMPLE = .FALSE.
      INTERPOLATE = .FALSE.

      WRITE( *, '(//21X,A//)' ) '**  Truncate/Subsample UDS Files  **'

      ! Read the file name/template/hitlist
  100 CALL FILE_PROMPT( TEMPLATE_STR, PROMPT, DIR_OUT,
     & ONE_PASS, HIT, NEW_HIT, FILE_NAME, EOF )
      IF ( ( FILE_NAME .EQ. ' ' ) .OR. ( EOF ) ) GO TO 199 ! Done

      ! Batch setup
      CALL BAT_SET( ALL_BATCH, NO_BATCH, NEW_HIT, HIT, BATCH )
      CALL GROUP_SET( HIT, NEW_HIT, BATCH, 2, GROUP, NEW_GROUP )

      ! Group prompts
      IF ( NEW_GROUP ) THEN

        ! Get initial value
  101   WRITE( *, '(/A,48X,A/'' >> '',$)' )
     &     ' Initial domain value', '[no change]'
        READ( *, '(A)', IOSTAT=IOS ) XIVAL
        IF ( IOS .NE. 0 ) THEN
          CALL MSG_WRITE( '*** Operation skipped', REPORT )
          CALL FILE_DEL( HIT )
          GO TO 190
        END IF
        READ( XIVAL, '(BN,F25.0)', ERR=101 ) XINUM

        ! Get final value
  102   WRITE( *, '(/A,50X,A/'' >> '',$)' )
     &     ' Final domain value', '[no change]'
        READ( *, '(A)', END=101 ) XFVAL
        READ( XFVAL, '(BN,F25.0)', ERR=102 ) XFNUM
        IF ( ( XIVAL .NE. ' ' ) .AND. ( XFVAL .NE. ' ' ) .AND.
     &     ( XINUM .GT. XFNUM ) ) THEN
          WRITE( *, '(/A)' )
     &       ' *** Final value must be greater than initial'
          GO TO 101
        END IF

        ! Get subsampling rate
        SUBSAMPLE = .FALSE.
        INTERPOLATE = .FALSE.
  103   WRITE( *, '(/2A,20X,A/'' >> '',$)' )
     &     ' Subsampling rate (int) ',
     &     'or new step value (real)?', '[no change]'
        READ( *, '(A)', END=101 ) SUBVAL
        IF ( SUBVAL .NE. ' ' ) THEN
          READ( SUBVAL, '(BN,I25)', IOSTAT=IOS ) NS
          IF ( ( IOS .EQ. 0 ) .AND.
     &     ( INDEX( SUBVAL, '.' ) .EQ. 0 ) ) THEN ! Integer => Subsample
            IF ( NS .LE. 0 ) THEN
              WRITE( *, '(/A)' ) ' *** Illegal value'
              GO TO 103
            END IF
            SUBSAMPLE = .TRUE.
          ELSE ! Non-integer => Interpolate
            READ( SUBVAL, '(BN,F25.0)', IOSTAT=IOS ) DEL_NEW
            IF ( ( IOS .NE. 0 ) .OR. ( DEL_NEW .LT. 0.D0 ) ) THEN
              WRITE( *, '(/A)' ) ' *** Illegal value'
              GO TO 103
            END IF
            INTERPOLATE = .TRUE.
          END IF
        END IF

        IF ( ( XIVAL .EQ. ' ' ) .AND. ( XFVAL .EQ. ' ' ) .AND.
     &     ( SUBVAL .EQ. ' ' ) ) THEN
          WRITE( *, * ) '*** No operations requested'
          GO TO 101
        END IF

      END IF

      ! Read UDS file
      CALL REP_COLS( REPORT )
      CALL UDS_GET( FILE_NAME, U, C, REPORT, IOS )
      IF ( IOS .NE. 0 ) GO TO 190

      ! Show range info and set file info
      IF ( U.DEL .NE. 0. ) THEN
        PDEL = DP_TRAN( U.DEL )
        IF ( U.CHANFORM .EQ. 'Y' ) THEN
          PTYP = U.XTYP
          PUNITS = U.XUNITS
        ELSE
          PTYP = 'TIME'
          PUNITS = 'SECONDS'
        END IF
        IF ( .NOT. GROUP )
     &     WRITE( *, '(/6X,2A,F16.6,A,F16.6,1X,A)', IOSTAT=IOS )
     &     PTYP(:L_TRIM(PTYP)), ' range is ', U.NFP*U.DEL,
     &     '  to  ', U.NLP*U.DEL, PUNITS(:L_TRIM(PUNITS))
      ELSE
        PDEL = 1.D0
        PTYP = 'data index'
        PUNITS = 'points'
        IF ( .NOT. GROUP )
     &     WRITE( *, '(/6X,A,I7,A,I7)', IOSTAT=IOS )
     &     'Index range is ', U.NFP, '  to  ', U.NLP
      END IF

      IF ( .NOT. GROUP ) THEN ! Interactive prompts

        ! Get initial domain value
  104   WRITE( *, '(/3A,10X,A,F16.6,A/'' >> '',$)' ) ' Initial ',
     &     PTYP, '('//PUNITS//')?', '[',U.NFP*PDEL,']'
        READ( *, '(A)', END=190 ) XIVAL
        IF ( XIVAL .NE. ' ' )
     &     READ( XIVAL, '(BN,F25.0)', ERR=104 ) XINUM

        ! Get final domain value
  105   WRITE( *, '(/3A,12X,A,F16.6,A/'' >> '',$)' ) ' Final ',
     &     PTYP, '('//PUNITS//')?', '[',U.NLP*PDEL,']'
        READ( *, '(A)', END=190 ) XFVAL
        IF ( XFVAL .NE. ' ' )
     &     READ( XFVAL, '(BN,F25.0)', ERR=105 ) XFNUM
        IF ( ( XIVAL .NE. ' ' ) .AND. ( XFVAL .NE. ' ' ) .AND.
     &     ( XINUM .GT. XFNUM ) ) THEN
          WRITE( *, '(/A)' )
     &       ' *** Final value must be greater than initial'
          GO TO 104
        END IF

        ! Get subsampling rate
        SUBSAMPLE = .FALSE.
        INTERPOLATE = .FALSE.
  106   WRITE( *, '(/6X,A,I7,A,I7)' ) 'Index range is ',
     &     U.NFP, '  to  ', U.NLP
        WRITE( *, '(/6X,2A,F16.6,3A)', IOSTAT=IOS )
     &     PTYP(:L_TRIM(PTYP)), ' step is ', PDEL,
     &     ' (',PUNITS(:L_TRIM(PUNITS)),')'
        WRITE( *, '(/2A,20X,A/'' >> '',$)' ) ' Subsampling rate ',
     &     '(int) or new step value (real)?', '[no change]'
        READ( *, '(A)', END=190 ) SUBVAL
        IF ( SUBVAL .NE. ' ' ) THEN
          READ( SUBVAL, '(BN,I25)', IOSTAT=IOS ) NS
          IF ( ( IOS .EQ. 0 ) .AND.
     &     ( INDEX( SUBVAL, '.' ) .EQ. 0 ) ) THEN ! Integer => Subsample
            IF ( NS .LE. 0 ) THEN
              WRITE( *, '(/A)' ) ' *** Illegal value'
              GO TO 106
            END IF
            SUBSAMPLE = .TRUE.
          ELSE ! Non-integer => Interpolate
            READ( SUBVAL, '(BN,F25.0)', IOSTAT=IOS ) DEL_NEW
            IF ( ( IOS .NE. 0 ) .OR. ( DEL_NEW .LT. 0.D0 ) ) THEN
              WRITE( *, '(/A)' ) ' *** Illegal value'
              GO TO 106
            END IF
            INTERPOLATE = .TRUE.
          END IF
        END IF

      END IF

      ! Perform operations
      IF ( XIVAL .NE. ' ' ) THEN
        NFP_I = U.NFP
        U.NFP = NINT( MIN( MAX( XINUM, NFP_L*PDEL ),
     &     NLP_U*PDEL ) / PDEL )
        IF ( U.NFP .LT. NFP_I ) THEN ! Zero-fill extended portion
          DO I = U.NFP, NFP_I-1
            U.X(I) = 0.
            U.Y(I) = 0.
          END DO
        END IF
      END IF
      IF ( XFVAL .NE. ' ' ) THEN
        NLP_I = U.NLP
        U.NLP = NINT( MIN( MAX( XFNUM, NFP_L*PDEL, U.NFP*PDEL ),
     &     NLP_U*PDEL ) / PDEL )
        IF ( U.NLP .GT. NLP_I ) THEN ! Zero-fill extended portion
          DO I = NLP_I+1, U.NLP
            U.X(I) = 0.
            U.Y(I) = 0.
          END DO
        END IF
      END IF
      IF ( SUBSAMPLE ) THEN
        ! Compute new array range
        NFP_N = NINT( ( U.NFP + .9999999 ) / NS )
        NLP_N = MAX( NFP_N, NINT( ( U.NLP - .9999999 ) / NS ) )

        ! Perform subsampling
        DO I = MAX( 0, NFP_N ), MIN( NLP_N, U.NLP/NS )
          U.X(I) = U.X(I*NS)
          U.Y(I) = U.Y(I*NS)
        END DO
        DO I = MIN( 0, NLP_N ), MAX( NFP_N, U.NFP/NS ), -1
          U.X(I) = U.X(I*NS)
          U.Y(I) = U.Y(I*NS)
        END DO
        U.NFP = NFP_N
        U.NLP = NLP_N
        U.DEL = U.DEL * NS
        PDEL = PDEL * NS
      ELSE IF ( INTERPOLATE ) THEN
        IF ( U.CHANFORM .EQ. 'X-Y' )
     &     CALL INTERP_S( U.X, NFP_L, NLP_U, U.NFP, U.NLP, PDEL,
     &     DEL_NEW, IOS )
        CALL INTERP_S( U.Y, NFP_L, NLP_U, U.NFP, U.NLP, PDEL,
     &     DEL_NEW, IOS )
        IF ( U.DEL .NE. 0. ) U.DEL = REAL( DEL_NEW )
      END IF

      ! Prepare output fields
      U.STATUS = 'MODIFIED'
      U.HIC = 0.
      U.T1 = 0.
      U.T2 = 0.
      U.HICDTUP = 0.
      U.CLIP3M = 0.
      U.CSI = 0.

      ! Set default output file name
      OUT_FILE = FILE_NAME
      CALL FN_EXTRACT( OUT_FILE )
      IF ( .NOT. NO_FLAG ) CALL FN_FLAG( OUT_FILE, 't', 2, I )
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
      IF ( IOS .NE. 0 ) GO TO 190
      IF ( REPORT.OPEN ) THEN
        WRITE( REPORT.UNIT, '(5X,A,G15.6,A,G15.6,A)', IOSTAT=IOS_W )
     &     'Output range: ', U.NFP * PDEL, ' to ', U.NLP * PDEL,
     &     '('//PUNITS//')'
        IF ( SUBSAMPLE ) WRITE( REPORT.UNIT, '(5X,A,I5)',
     &     IOSTAT=IOS_W )
     &     'Subsampling rate = ', NS
        IF ( INTERPOLATE ) WRITE( REPORT.UNIT, '(5X,A,G15.6)',
     &     IOSTAT=IOS_W )
     &     'Output step = ', DEL_NEW
      END IF

      ! Loop for next file
  190 WRITE( *, '(80(''_''))' )
      GO TO 100

      ! Clean up and stop
  199 CALL CLEAN_UP( HIT, REPORT, OUTPUT_LIST, DIR_OUT )

      END



      SUBROUTINE TRUNC_CL( CL_ARG, NUM_ARGS, PROG_NAME,
     & NO_FLAG )

!***********************************************************************
!* Processes the Program-Specific Command Line Arguments
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) CL_ARG(0:*) ! Command line argument array

      INTEGER NUM_ARGS ! Number of arguments

      CHARACTER*(*) PROG_NAME ! Program name

      LOGICAL NO_FLAG ! Indicates not to flag output file names


      ! Variables ______________________________________________________

      INTEGER I, NS

      CHARACTER ARG*255, C


      ! Process command line arguments
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
     &       '[NO_FLAG]'
          IF ( ( ARG(NS:) .EQ. '??' ) .OR.
     &       ( ARG(NS:) .EQ. 'HELP' ) ) THEN ! Show command line help
            CALL HELP_CL( PROG_NAME )
            WRITE( *, '(10X,A)' )
     &         'NO_FLAG :  Don''t flag output file names'
          END IF
          STOP ' '
        ELSE IF ( ( ARG(NS:) .EQ. 'NO_FLAG' ) .OR.
     &     ( ARG(NS:) .EQ. 'NOFLAG' ) ) THEN
          NO_FLAG = .TRUE.
          CL_ARG(I) = ' '
        END IF
      END DO

      RETURN
      END
