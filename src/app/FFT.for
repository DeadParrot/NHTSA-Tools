      PROGRAM FFT

!***********************************************************************
!* Computes the DFT Magnitude and Phase of a UDS Y-Series File.
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

      RECORD /UDS_Y/  U
      RECORD /UDS_CONTROL/  C
      RECORD /FILE/  HIT, REPORT, OUTPUT_LIST

      LOGICAL ALL_BATCH, NO_BATCH, NO_INCR, ONE_PASS, NEW_HIT, EOF,
     & BATCH, GROUP, NEW_GROUP

      INTEGER NUM_ARGS, MAX_ARGS, IOS, I, M_MIN, M_MAX

      PARAMETER ( MAX_ARGS = 12 )

      REAL Y_PHS(0:NLP_U), PAD_FAC

      COMPLEX Y_DFT(0:NLP_2)

      EQUIVALENCE ( Y_PHS, Y_DFT ) ! Space saving - DFT not used

      CHARACTER CL_ARG(0:MAX_ARGS)*255, PROG_NAME*8,
     & TEMPLATE_STR*255, DIR_OUT*255,
     & FILE_NAME*255, OUT_FILE*255, DEF_FILE*255,
     & PROMPT*79,
     & OUTPUTS*2, MAG_OUT, PHS_OUT


      ! Initializations
      PROG_NAME = 'FFT'
      CALL FILE_INIT( HIT )
      CALL GET_CL( CL_ARG, NUM_ARGS, MAX_ARGS )
      CALL FFT_CL( CL_ARG, NUM_ARGS, PROG_NAME, OUTPUTS )
      CALL PROC_CL( CL_ARG, NUM_ARGS, PROG_NAME, TEMPLATE_STR,
     & ALL_BATCH, NO_BATCH, NO_INCR, REPORT, OUTPUT_LIST, DIR_OUT,
     & ONE_PASS, .TRUE., .TRUE. )
      PROMPT = 'UDS file name/template/hitlist?'
      PROMPT(74:) = '[done]'
      CALL UDS_CONTROL_INIT( C )
      C.CHANFORM = 'Y' ! Only accept Y-series file input
      M_MAX = INT( ( LOG( REAL( NLP_2 ) ) / LOG( 2. ) ) *
     & ( 1. + 2.E-7 ) )

      WRITE( *, '(//17X,A//)' )
     & '**  Discrete Fourier Transform Computation  **'

      ! Read the file name/template/hitlist
  100 CALL FILE_PROMPT( TEMPLATE_STR, PROMPT, DIR_OUT,
     & ONE_PASS, HIT, NEW_HIT, FILE_NAME, EOF )
      IF ( ( FILE_NAME .EQ. ' ' ) .OR. ( EOF ) ) GO TO 199 ! Done

      ! Batch setup
      CALL BAT_SET( ALL_BATCH, NO_BATCH, NEW_HIT, HIT, BATCH )
      CALL GROUP_SET( HIT, NEW_HIT, BATCH, 2, GROUP, NEW_GROUP )

      ! Group prompts
      IF ( NEW_GROUP ) THEN

        ! DFT magnitude files
  101   IF ( OUTPUTS(1:1) .NE. ' ' ) THEN
          MAG_OUT = 'Y'
        ELSE IF ( OUTPUTS .NE. ' ' ) THEN
          MAG_OUT = 'N'
        ELSE
          WRITE( *, '(/A,46X,A/'' >> '',$)' )
     &     ' DFT magnitude files?   (Y/N)', '[Yes]'
          READ( *, '(A)', IOSTAT=IOS ) MAG_OUT
          IF ( IOS .NE. 0 ) THEN
            CALL MSG_WRITE( '*** Operation skipped', REPORT )
            CALL FILE_DEL( HIT )
            GO TO 190
          END IF
          CALL STR_UP( MAG_OUT )
          IF ( .NOT. ANY_CHARS( MAG_OUT, ' YN' ) ) THEN
            WRITE( *, * ) '*** Invalid response'
            GO TO 101
          END IF
        END IF

        ! DFT phase files
  102   IF ( OUTPUTS(2:2) .NE. ' ' ) THEN
          PHS_OUT = 'Y'
        ELSE IF ( OUTPUTS .NE. ' ' ) THEN
          PHS_OUT = 'N'
        ELSE
          WRITE( *, '(/A,50X,A/'' >> '',$)' )
     &     ' DFT phase files?   (Y/N)', '[Yes]'
          READ( *, '(A)', IOSTAT=IOS ) PHS_OUT
          IF ( IOS .NE. 0 ) THEN
            CALL MSG_WRITE( '*** Operation skipped', REPORT )
            CALL FILE_DEL( HIT )
            GO TO 190
          END IF
          CALL STR_UP( PHS_OUT )
          IF ( .NOT. ANY_CHARS( PHS_OUT, ' YN' ) ) THEN
            WRITE( *, * ) '*** Invalid response'
            GO TO 102
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
      IF ( U.NFP .GT. 0 ) THEN
        CALL MSG_WRITE( '*** Zero-filling initial portion of signal',
     &   REPORT )
        DO I = 0, U.NFP-1
          U.Y(I) = 0.
        END DO
      END IF
      IF ( U.NLP .LE. 0 ) THEN
        CALL MSG_WRITE( '*** ERROR - Final index <= 0', REPORT )
        GO TO 190
      END IF
      IF ( U.DEL .LE. 0. ) THEN
        CALL MSG_WRITE( '*** ERROR - Nonpositive x-axis step',
     &   REPORT )
        GO TO 190
      END IF

      ! Prepare output fields
      U.XTYP = 'FREQUENCY'
      IF ( U.XUNITS .EQ. 'SECONDS' ) THEN
        U.XUNITS = 'HZ'
      ELSE
        U.XUNITS = ' '
      END IF
      U.NFP = 0
      U.FSTP = 1. / ( 2. * U.DEL )
      U.FCOR = U.FSTP
      U.FCUT = U.FSTP
      U.STATUS = 'COMPUTED'

      ! Compute DFT with FFT algorithm
      PAD_FAC = 2.5 ! Length padding factor
      M_MIN = 6 ! Minimum power of 2 to use for length
      CALL FFT_S( U.Y(0), U.Y(0), Y_PHS, NLP_U, U.NLP, Y_DFT, NLP_2,
     & U.DEL, PAD_FAC, M_MIN, IOS )
      IF ( IOS .NE. 0 ) THEN
        CALL MSG_WRITE( '*** ERROR - FFT computation failed', REPORT )
        GO TO 190
      END IF

      ! Set default magnitude file name
      OUT_FILE = FILE_NAME
      CALL FN_EXTRACT( OUT_FILE )
      CALL FN_FLAG( OUT_FILE, 'dm', 2, I )
      CALL ADD_PATH( DIR_OUT, OUT_FILE )
      IF ( .NOT. NO_INCR ) CALL FN_INCR( OUT_FILE )
      CALL FN_NAME_EXTRACT( OUT_FILE )
      DEF_FILE = OUT_FILE

      ! Get the magnitude UDS file name
      IF ( .NOT. BATCH ) THEN
  120   IF ( ( OUTPUTS .EQ. ' ' ) .OR.
     &   ( OUTPUTS(1:1) .NE. ' ' ) ) THEN
          WRITE( *, '(/A/3A/'' >> '',$)' )
     &     ' DFT magnitude file name?   ( N - None )',
     &     ' [', DEF_FILE(:L_TRIM(DEF_FILE)), ']'
          READ( *, '(A)', END=190 ) OUT_FILE
          IF ( OUT_FILE .EQ. ' ' ) THEN ! Use default file name
            OUT_FILE = DEF_FILE
          ELSE IF ( ( OUT_FILE .EQ. 'N' ) .OR.
     &     ( OUT_FILE .EQ. 'n' ) ) THEN ! No output file
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
        IF ( MAG_OUT .EQ. 'N' ) THEN
          OUT_FILE = 'N'
        ELSE
          OUT_FILE = DEF_FILE
        END IF
      END IF

      ! Write the magnitude UDS file
      IF ( OUT_FILE .NE. 'N' ) THEN
        ! Prepare magnitude output fields
        U.CURTYP = 'DFT MAG.'
        WRITE ( U.CURDSC, '(A)', IOSTAT=IOS )
     &   'Discrete Fourier Transform Magnitude'

        CALL UDS_PUT( OUT_FILE, U, C, REPORT, OUTPUT_LIST,
     &   DIR_OUT, .TRUE., IOS )
        IF ( IOS .NE. 0 ) GO TO 190
      END IF

      ! Construct default phase UDS file name
      OUT_FILE = FILE_NAME
      CALL FN_EXTRACT( OUT_FILE )
      CALL FN_FLAG( OUT_FILE, 'dp', 2, I )
      CALL ADD_PATH( DIR_OUT, OUT_FILE )
      IF ( .NOT. NO_INCR ) CALL FN_INCR( OUT_FILE )
      CALL FN_NAME_EXTRACT( OUT_FILE )
      DEF_FILE = OUT_FILE

      ! Get the phase UDS file name
      IF ( .NOT. BATCH ) THEN
  130   IF ( ( OUTPUTS .EQ. ' ' ) .OR.
     &   ( OUTPUTS(2:2) .NE. ' ' ) ) THEN
          WRITE( *, '(/A/3A/'' >> '',$)' )
     &     ' DFT phase file name?   ( N - None )',
     &     ' [', DEF_FILE(:L_TRIM(DEF_FILE)), ']'
          READ( *, '(A)', END=190 ) OUT_FILE
          IF ( OUT_FILE .EQ. ' ' ) THEN ! Use default file name
            OUT_FILE = DEF_FILE
          ELSE IF ( ( OUT_FILE .EQ. 'N' ) .OR.
     &     ( OUT_FILE .EQ. 'n' ) ) THEN ! No output file
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
        IF ( PHS_OUT .EQ. 'N' ) THEN
          OUT_FILE = 'N'
        ELSE
          OUT_FILE = DEF_FILE
        END IF
      END IF

      ! Write the phase UDS file
      IF ( OUT_FILE .NE. 'N' ) THEN
        ! Assign phase
        DO I = 0, U.NLP
          U.Y(I) = Y_PHS(I)
        END DO

        ! Prepare phase output fields
        U.YTYP = 'PHASE ANGLE'
        U.YUNITS = 'DEGREES'
        U.CURTYP = 'DFT PHASE'
        WRITE ( U.CURDSC, '(A)', IOSTAT=IOS )
     &   'Discrete Fourier Transform Phase Angle'

        CALL UDS_PUT( OUT_FILE, U, C, REPORT, OUTPUT_LIST,
     &   DIR_OUT, .TRUE., IOS )
      END IF

      ! Loop for next file
  190 WRITE( *, '(80(''_''))' )
      GO TO 100

      ! Clean up and stop
  199 CALL CLEAN_UP( HIT, REPORT, OUTPUT_LIST, DIR_OUT )

      END



      SUBROUTINE FFT_CL( CL_ARG, NUM_ARGS, PROG_NAME, OUTPUTS )

!***********************************************************************
!* Processes the Program-Specific Command Line Arguments
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) CL_ARG(0:*) ! Command line argument array

      INTEGER NUM_ARGS ! Number of arguments

      CHARACTER*(*) PROG_NAME ! Program name

      CHARACTER*(*) OUTPUTS ! Indicates magnitude &/or phase outputs


      ! Variables ______________________________________________________

      INTEGER I, NS

      CHARACTER ARG*255, C


      ! Process command line arguments
      OUTPUTS = ' '
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
          WRITE( *, '(10X,A)' ) '[MAG|PHASE|BOTH]'
          IF ( ( ARG(NS:) .EQ. '??' ) .OR.
     &     ( ARG(NS:) .EQ. 'HELP' ) ) THEN ! Show command line help
            CALL HELP_CL( PROG_NAME )
            WRITE( *, '(2(10X,A/),10X,A)' )
     &       'MAG :  Magnitude output only',
     &       'PHASE :  Phase output only',
     &       'BOTH :  Magnitude and phase outputs'
          END IF
          STOP ' '
        ELSE IF ( ARG(NS:) .EQ. 'MAG' ) THEN
          OUTPUTS = 'X '
          CL_ARG(I) = ' '
        ELSE IF ( ARG(NS:) .EQ. 'PHASE' ) THEN
          OUTPUTS = ' X'
          CL_ARG(I) = ' '
        ELSE IF ( ARG(NS:) .EQ. 'BOTH' ) THEN
          OUTPUTS = 'XX'
          CL_ARG(I) = ' '
        END IF
      END DO

      RETURN
      END
