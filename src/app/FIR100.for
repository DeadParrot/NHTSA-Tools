      PROGRAM FIR100

!***********************************************************************
!* 100 Hz Finite Impulse Response Filter Program
!*
!* FIR100 performs 100 Hz finite impulse response filtering of a
!* UDS file containing a digitized time series.  FIR100 is general
!* purpose in nature, but is used primarily to filter side impacted
!* thoracic and pelvic signals.
!*
!* The program filters a signal using a four step process.  First,
!* FIR100 uses a four-pole, zero-phase Butterworth filter with a
!* 300 Hz cutoff.  This is done to prevent aliasing in the next step,
!* subsampling to a 1600 Hz sampling rate.  After the anti-aliasing
!* filter, the signal is subsampled to exactly 1600 Hz (time
!* between data samples = .000625 seconds). This is the required
!* sampling rate for FIR100 to filter to the correct frequencies.
!* At the user's option, any bias in the signal is next removed.
!* Finally, the finite impulse response filter is applied to
!* the signal, and the signal is output in UDS format.
!*
!* The resulting passband frequency is 100 Hz, the cutoff
!* frequency is 136 Hz, the stopband frequency is 189 Hz, the
!* passband ripple is 0.0225 dB, and the stopband gain is -50 dB.
!*
!* FIR100 started out life as the BIOPROC, and is the functional
!* equivalent of BIOPROC.
!*
!* The BIAS, IIRFIL, and FIRF16 subroutines were coded by M.R.
!* Neale of VRTC.
!*
!* Programmers:
!*   H. C. Gabler,  8/1/87  Initial Release
!*   J. H. Marcus, M. R. Neale
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
     & BATCH, REMOVE_BIAS

      INTEGER NUM_ARGS, MAX_ARGS, IOS, IOS_W, I

      PARAMETER ( MAX_ARGS = 12 )

      REAL S(NFP_L-25:NLP_U+25)

      CHARACTER CL_ARG(0:MAX_ARGS)*255, PROG_NAME*8,
     & TEMPLATE_STR*255, DIR_OUT*255,
     & FILE_NAME*255, OUT_FILE*255, DEF_FILE*255,
     & PROMPT*79,
     & REM_BIAS_CL, REM_BIAS


      ! Initializations
      PROG_NAME = 'FIR100'
      CALL FILE_INIT( HIT )
      CALL GET_CL( CL_ARG, NUM_ARGS, MAX_ARGS )
      CALL FIR100_CL( CL_ARG, NUM_ARGS, PROG_NAME, REM_BIAS_CL )
      CALL PROC_CL( CL_ARG, NUM_ARGS, PROG_NAME, TEMPLATE_STR,
     & ALL_BATCH, NO_BATCH, NO_INCR, REPORT, OUTPUT_LIST, DIR_OUT,
     & ONE_PASS, .TRUE., .TRUE. )
      PROMPT = 'UDS file name/template/hitlist?'
      PROMPT(74:) = '[done]'
      CALL UDS_CONTROL_INIT( C )
      C.CHANFORM = 'Y' ! Only accept Y-series file input

      WRITE( *, '(//25X,A//)' ) '**  FIR 100 Hz Filtering  **'

      ! Read the file name/template/hitlist
  100 CALL FILE_PROMPT( TEMPLATE_STR, PROMPT, DIR_OUT,
     & ONE_PASS, HIT, NEW_HIT, FILE_NAME, EOF )
      IF ( ( FILE_NAME .EQ. ' ' ) .OR. ( EOF ) ) GO TO 199 ! Done

      ! Batch setup
      CALL BAT_SET( ALL_BATCH, NO_BATCH, NEW_HIT, HIT, BATCH )

      ! Batch prompts
      IF ( ( NEW_HIT ) .AND. ( BATCH ) ) THEN

        ! Ask about bias removal
  101   IF ( REM_BIAS_CL .EQ. ' ' ) THEN
          WRITE( *, '(/A,54X,A/'' >> '',$)' )
     &       ' Remove bias?   (Y/N)', '[Yes]'
          READ( *, '(A)', IOSTAT=IOS ) REM_BIAS
          IF ( IOS .NE. 0 ) THEN
            CALL MSG_WRITE( '*** Operation skipped', REPORT )
            CALL FILE_DEL( HIT )
            GO TO 190
          END IF
          CALL STR_UP( REM_BIAS )
          IF ( .NOT. ANY_CHARS( REM_BIAS, ' YN' ) ) THEN
            WRITE( *, * ) '*** Invalid response'
            GO TO 101
          END IF
          IF ( REM_BIAS .EQ. ' ' ) REM_BIAS = 'Y'
        ELSE
          REM_BIAS = REM_BIAS_CL
        END IF
        IF ( ANY_CHARS( REM_BIAS, ' Y' ) ) THEN
          REMOVE_BIAS = .TRUE.
        ELSE
          REMOVE_BIAS = .FALSE.
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
      IF ( U.DEL .LE. 0. ) THEN
        CALL MSG_WRITE(
     &     '*** ERROR - Non-positive file x-axis step value', REPORT )
        GO TO 190
      END IF

      IF ( .NOT. BATCH ) THEN ! Interactive prompts

        ! Show file info
        WRITE( *, '(/5X,2A/5X,A,F9.3,A/5X,A,F9.3,A)', IOSTAT=IOS_W )
     &     ' Sensor Attachment: ', U.SENATT,
     &     ' Prefilter Frequency:  ', U.PREF, ' (Hz)',
     &     ' Cutoff Frequency:     ', U.FCUT, ' (Hz)'

        ! Ask about bias removal
  102   IF ( REM_BIAS_CL .EQ. ' ' ) THEN
          WRITE( *, '(/A,54X,A/'' >> '',$)' )
     &       ' Remove bias?   (Y/N)', '[Yes]'
          READ( *, '(A)', END=190 ) REM_BIAS
          CALL STR_UP( REM_BIAS )
          IF ( .NOT. ANY_CHARS( REM_BIAS, ' YN' ) ) THEN
            WRITE( *, * ) '*** Invalid response'
            GO TO 102
          END IF
          IF ( REM_BIAS .EQ. ' ' ) REM_BIAS = 'Y'
        ELSE
          REM_BIAS = REM_BIAS_CL
        END IF
        IF ( ANY_CHARS( REM_BIAS, ' Y' ) ) THEN
          REMOVE_BIAS = .TRUE.
        ELSE
          REMOVE_BIAS = .FALSE.
        END IF

      END IF

      ! Perform FIR100 filtering
      CALL FIR100_S( U.Y, S, NFP_L, NLP_U, U.NFP, U.NLP, U.DEL,
     & REMOVE_BIAS, IOS )
      IF ( IOS .NE. 0 ) THEN
        CALL MSG_WRITE( '*** Filtering failed', REPORT )
        GO TO 190
      END IF

      ! Prepare output fields
      U.STATUS = 'FILTERED'
      U.HIC = 0.
      U.T1 = 0.
      U.T2 = 0.
      U.HICDTUP = 0.
      U.CLIP3M = 0.
      U.CSI = 0.
      U.FCUT = 100.
      U.FSTP = 189.
      IF ( ( ( U.FCOR .GT. 0. ) .AND. ( U.FSTP .GT. U.FCOR ) ) .OR.
     & ( ( U.PREF .GT. 0. ) .AND. ( U.FSTP .GT. .5*U.PREF ) ) ) THEN
        CALL MSG_WRITE( '*** WARNING - Filter causes overlap '//
     &     'distortion with previous filtering', REPORT )
        U.STATUS = 'DISTORTED'
      END IF
      U.FCOR = 100.
      U.CD1 = 'FIR100 Filtered'

      ! Set default output file name
      OUT_FILE = FILE_NAME
      CALL FN_EXTRACT( OUT_FILE )
      CALL FN_FLAG( OUT_FILE, 'fd', 2, I )
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



      SUBROUTINE FIR100_CL( CL_ARG, NUM_ARGS, PROG_NAME, REM_BIAS_CL )

!***********************************************************************
!* Processes the Program-Specific Command Line Arguments
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) CL_ARG(0:*) ! Command line argument array

      INTEGER NUM_ARGS ! Number of arguments

      CHARACTER*(*) PROG_NAME ! Program name

      CHARACTER REM_BIAS_CL ! Indicates whether to remove bias


      ! Variables ______________________________________________________

      INTEGER I, NS

      CHARACTER ARG*255, C


      ! Process command line arguments
      REM_BIAS_CL = ' '
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
          WRITE( *, '(10X,A)' ) '[[NO_]BIAS]'
          IF ( ( ARG(NS:) .EQ. '??' ) .OR.
     &       ( ARG(NS:) .EQ. 'HELP' ) ) THEN ! Show command line help
            CALL HELP_CL( PROG_NAME )
            WRITE( *, '(10X,A/10X,A)' )
     &         'BIAS :  Remove signal bias',
     &         'NO_BIAS :  Don''t remove signal bias'
          END IF
          STOP ' '
        ELSE IF ( ARG(NS:) .EQ. 'BIAS' ) THEN
          REM_BIAS_CL = 'Y'
          CL_ARG(I) = ' '
        ELSE IF ( ( ARG(NS:) .EQ. 'NO_BIAS' ) .OR.
     &     ( ARG(NS:) .EQ. 'NOBIAS' ) ) THEN
          REM_BIAS_CL = 'N'
          CL_ARG(I) = ' '
        END IF
      END DO

      RETURN
      END
