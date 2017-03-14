      PROGRAM BWFILT

!***********************************************************************
!* Second-Order Butterworth Filter of UDS Y-Series Files
!*
!* Programmer: E. Cheung
!*             Stuart G. Mentzer - Tools framework
!*
!* Language: Fortran
!*
!* Date: 2003/07/18
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'
      INCLUDE 'file.fi'


      ! Variables ______________________________________________________

      RECORD /UDS_Y/  U
      RECORD /UDS_CONTROL/  C
      RECORD /FILE/  HIT, REPORT, OUTPUT_LIST

      LOGICAL ALL_BATCH, NO_BATCH, NO_INCR, ONE_PASS, NEW_HIT, EOF,
     & BATCH, GROUP, NEW_GROUP,
     & DEF_CUTOFF

      INTEGER NUM_ARGS, MAX_ARGS, IOS, IOS_W, I

      PARAMETER ( MAX_ARGS = 12 )

      REAL FCUT

      DOUBLE PRECISION CUTOFF, FCUT_DEF, FSTP_DEF

      CHARACTER CL_ARG(0:MAX_ARGS)*255, PROG_NAME*8,
     & TEMPLATE_STR*255, DIR_OUT*255,
     & FILE_NAME*255, OUT_FILE*255, DEF_FILE*255,
     & PROMPT*79,
     & FCUT_C*25, FCUT_DEF_C*7, FSTP_DEF_C*7, FILCOD


      ! Initializations
      PROG_NAME = 'BWFilt'
      CALL FILE_INIT( HIT )
      CALL GET_CL( CL_ARG, NUM_ARGS, MAX_ARGS )
      CALL BWFILT_CL( CL_ARG, NUM_ARGS, PROG_NAME,
     & DEF_CUTOFF, CUTOFF )
      CALL PROC_CL( CL_ARG, NUM_ARGS, PROG_NAME, TEMPLATE_STR,
     & ALL_BATCH, NO_BATCH, NO_INCR, REPORT, OUTPUT_LIST, DIR_OUT,
     & ONE_PASS, .TRUE., .TRUE. )
      PROMPT = 'UDS file name/template/hitlist?'
      PROMPT(74:) = '[done]'
      CALL UDS_CONTROL_INIT( C )
      C.CHANFORM = 'Y' ! Only accept Y-series file input

      WRITE( *, '(//25X,A//)' ) '**  Butterworth Filtering  **'

      ! Read the file name/template/hitlist
  100 CALL FILE_PROMPT( TEMPLATE_STR, PROMPT, DIR_OUT,
     & ONE_PASS, HIT, NEW_HIT, FILE_NAME, EOF )
      IF ( ( FILE_NAME .EQ. ' ' ) .OR. ( EOF ) ) GO TO 199 ! Done

      ! Batch/group setup
      CALL BAT_SET( ALL_BATCH, NO_BATCH, NEW_HIT, HIT, BATCH )
      CALL GROUP_SET( HIT, NEW_HIT, BATCH, 2, GROUP, NEW_GROUP )

      ! Group prompts
      IF ( NEW_GROUP ) THEN

        ! Get cutoff frequency
  101   IF ( DEF_CUTOFF ) THEN
          FCUT_C = ' '
        ELSE IF ( CUTOFF .GT. 0.D0 ) THEN
          FCUT = REAL( CUTOFF )
          FCUT_C = 'NOT BLANK'
        ELSE
          WRITE( *, '(/A,48X,A/'' >> '',$)' )
     &       ' Cutoff frequency (Hz)?', '[default]'
          READ( *, '(A)', IOSTAT=IOS ) FCUT_C
          IF ( IOS .NE. 0 ) THEN
            CALL MSG_WRITE( '*** Operation skipped', REPORT )
            CALL FILE_DEL( HIT )
            GO TO 190
          END IF
          IF ( FCUT_C .NE. ' ' ) THEN
            READ( FCUT_C, '(BN,F25.0)', IOSTAT=IOS ) FCUT
            IF ( ( IOS .NE. 0 ) .OR. ( FCUT .LE. 0. ) ) THEN
              WRITE( *, * ) '*** Invalid frequency'
              GO TO 101
            END IF
          END IF
        END IF

      END IF

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
      IF ( ( U.NFP .GT. 0 ) .OR. ( U.NLP .LE. 0 ) ) CALL MSG_WRITE(
     & '*** WARNING - Nonstandard x-axis span', REPORT )

      ! Determine default cutoff frequency value
      CALL FREQ_DEF( U, 0.D0, 0.D0, FCUT_DEF, FCUT_DEF_C,
     & FSTP_DEF, FSTP_DEF_C )

      IF ( .NOT. GROUP ) THEN ! Interactive prompts

        ! Get cutoff frequency value
  102   IF ( DEF_CUTOFF ) THEN
          FCUT_C = ' '
          FCUT = REAL( FCUT_DEF )
        ELSE IF ( CUTOFF .GT. 0.D0 ) THEN
          FCUT = REAL( CUTOFF )
          FCUT_C = 'NOT BLANK'
        ELSE
          ! Show file info
          WRITE( *, '(/5X,2A/5X,A,F9.3,A/5X,A,F9.3,A)',
     &       IOSTAT=IOS_W )
     &       ' Sensor Attachment: ', U.SENATT,
     &       ' Prefilter Frequency:  ', U.PREF, ' (Hz)',
     &       ' Cutoff Frequency:     ', U.FCUT, ' (Hz)'
          WRITE( *, '(/A,48X,A/'' >> '',$)' )
     &       ' Cutoff frequency (Hz)?', '['//FCUT_DEF_C//']'
          READ( *, '(A)', IOSTAT=IOS ) FCUT_C
          IF ( IOS .NE. 0 ) THEN
            CALL MSG_WRITE( '*** Operation skipped', REPORT )
            GO TO 190
          END IF
          IF ( FCUT_C .EQ. ' ' ) THEN
            FCUT = REAL( FCUT_DEF )
          ELSE
            READ( FCUT_C, '(BN,F25.0)', IOSTAT=IOS ) FCUT
            IF ( ( IOS .NE. 0 ) .OR. ( FCUT .LE. 0. ) ) THEN
              WRITE( *, * ) '*** Invalid frequency'
              GO TO 102
            END IF
          END IF
        END IF

      ELSE ! Group

        IF ( FCUT_C .EQ. ' ' ) FCUT = REAL( FCUT_DEF )

      END IF

      ! Perform filtering
      IF ( FCUT .GT. 0. ) THEN
        CALL BWFILT_S( U.Y(U.NFP), U.NFP, U.NLP, U.DEL, FCUT )
      ELSE
        CALL MSG_WRITE( '*** WARNING - No filtering performed',
     &     REPORT )
      END IF

      ! Prepare output fields
      U.STATUS = 'FILTERED'
      U.HIC = 0.
      U.T1 = 0.
      U.T2 = 0.
      U.HICDTUP = 0.
      U.CLIP3M = 0.
      U.CSI = 0.
      U.FCUT = FCUT
      U.FSTP = 2.5 * FCUT
      IF ( ( ( U.FCOR .GT. 0. ) .AND. ( U.FSTP .GT. U.FCOR ) ) .OR.
     & ( ( U.PREF .GT. 0. ) .AND. ( U.FSTP .GT. .5*U.PREF ) ) ) THEN
        CALL MSG_WRITE( '*** WARNING - Filter causes overlap '//
     &     'distortion with previous filtering', REPORT )
        U.STATUS = 'DISTORTED'
      END IF
      U.FCOR = .5 * FCUT
      U.CD1 = 'BWFILT Filtered'

      ! Set default output file name
      OUT_FILE = FILE_NAME
      CALL FN_EXTRACT( OUT_FILE )
      CALL FILTER_FLAG( U.CURTYP, U.FCUT, U.FSTP, FILCOD )
      CALL FN_FLAG( OUT_FILE, 'f'//FILCOD, 2, I )
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
     & DIR_OUT, .TRUE., IOS )

      ! Loop for next file
  190 WRITE( *, '(80(''_''))' )
      GO TO 100

      ! Clean up and stop
  199 CALL CLEAN_UP( HIT, REPORT, OUTPUT_LIST, DIR_OUT )

      END



      SUBROUTINE BWFILT_CL( CL_ARG, NUM_ARGS, PROG_NAME,
     & DEF_CUTOFF, CUTOFF )

!***********************************************************************
!* Processes the Program-Specific Command Line Arguments
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) CL_ARG(0:*) ! Command line argument array

      INTEGER NUM_ARGS ! Number of arguments

      CHARACTER*(*) PROG_NAME ! Program name

      LOGICAL DEF_CUTOFF ! Indicates to use default cutoff

      DOUBLE PRECISION CUTOFF ! Cutoff frequency


      ! Variables ______________________________________________________

      INTEGER I, NS, IOS

      CHARACTER ARG*255, C


      ! Process command line arguments
      DEF_CUTOFF = .FALSE.
      CUTOFF = 0.D0
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
          WRITE( *, '(10X,A)' ) '[CUTOFF=<cutoff_freq>]'
          IF ( ( ARG(NS:) .EQ. '??' ) .OR.
     &       ( ARG(NS:) .EQ. 'HELP' ) ) THEN ! Show command line help
            CALL HELP_CL( PROG_NAME )
            WRITE( *, '(10X,A)' )
     &         '<cutoff_freq> :  Cutoff frequency (blank => default)'
          END IF
          STOP ' '
        ELSE IF ( ( ARG(NS:NS+6) .EQ. 'CUTOFF=' ) .OR.
     &     ( ARG(NS:NS+6) .EQ. 'CUTOFF#' ) ) THEN
          IF ( ARG(NS+7:) .EQ. ' ' ) THEN ! Default
            DEF_CUTOFF = .TRUE.
            CUTOFF = 0.D0
          ELSE
            READ( ARG(NS+7:), '(BN,F25.0)', IOSTAT=IOS ) CUTOFF
            IF ( ( IOS .NE. 0 ) .OR. ( CUTOFF .LE. 0.D0 ) ) THEN
              WRITE( *, '(/2A)', IOSTAT=IOS )
     &           ' Illegal cutoff frequency: ',
     &           ARG(NS+7:L_TRIM(ARG))
              CUTOFF = 0.D0
            ELSE
              DEF_CUTOFF = .FALSE.
            END IF
          END IF
          CL_ARG(I) = ' '
        ELSE IF ( ARG(NS:) .EQ. 'CUTOFF' ) THEN ! Default
          DEF_CUTOFF = .TRUE.
          CUTOFF = 0.D0
          CL_ARG(I) = ' '
        END IF
      END DO

      RETURN
      END
