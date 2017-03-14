      PROGRAM HIC

!***********************************************************************
!* Computes the maximum unwindowed Head Injury Criterion value
!* and the corresponding time interval using an efficient global
!* branch and bound algorithm.
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

      RECORD /UDS_Y/  U, S
      RECORD /UDS_CONTROL/  C
      RECORD /FILE/  HIT, REPORT, OUTPUT_LIST

      LOGICAL ALL_BATCH, NO_BATCH, NO_INCR, ONE_PASS, NEW_HIT, EOF,
     & BATCH,
     & UPDATE

      INTEGER NUM_ARGS, MAX_ARGS, IOS, NAR

      PARAMETER ( MAX_ARGS = 12 )

      PARAMETER ( NAR = NLP_U * 2 ) ! Larger NAR helps avoid slow method

      INTEGER JL(NAR), KL(NAR)

      CHARACTER CL_ARG(0:MAX_ARGS)*255, PROG_NAME*8,
     & TEMPLATE_STR*255, DIR_OUT*255,
     & FILE_NAME*255, OUT_FILE*255, DEF_FILE*255,
     & PROMPT*79


      ! Initializations
      PROG_NAME = 'HIC'
      CALL FILE_INIT( HIT )
      CALL GET_CL( CL_ARG, NUM_ARGS, MAX_ARGS )
      CALL HIC_CL( CL_ARG, NUM_ARGS, PROG_NAME, UPDATE )
      CALL PROC_CL( CL_ARG, NUM_ARGS, PROG_NAME, TEMPLATE_STR,
     & ALL_BATCH, NO_BATCH, NO_INCR, REPORT, OUTPUT_LIST, DIR_OUT,
     & ONE_PASS, .TRUE., .TRUE. )
      PROMPT = 'UDS file name/template/hitlist?'
      PROMPT(74:) = '[done]'
      CALL UDS_CONTROL_INIT( C )
      C.CHANFORM = 'Y' ! Only accept Y-series file input

      WRITE( *, '(//18X,A//)' )
     & '**  Head Injury Criterion Computation  **'

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

      ! Check file fields
      IF ( ( U.CURTYP .NE. 'TIME SERIES' ) .OR.
     & ( U.XTYP .NE. 'TIME' ) .OR.
     & ( U.XUNITS .NE. 'SECONDS' ) .OR.
     & ( ( U.CURNAM(:4) .NE. 'RES ') .AND. ( U.AXIS .NE. 'RS' ) ) )
     & CALL MSG_WRITE(
     & '*** WARNING - Not a resultant time series file', REPORT )
      IF ( ( U.SENATT .NE. 'HEAD CG' ) .OR.
     & ( U.YUNITS .NE. 'G''S' ) .OR.
     & ( U.FCUT .NE. 0. ) ) CALL MSG_WRITE(
     & '*** WARNING - Not raw HEAD CG acceleration data', REPORT )
      IF ( ( U.NFP .GT. 0 ) .OR. ( U.NLP .LE. 0 ) ) CALL
     & MSG_WRITE( '*** WARNING - Nonstandard x-axis span', REPORT )
      IF ( U.DEL .LE. 0. ) THEN
        CALL MSG_WRITE(
     &     '*** ERROR - Non-positive file time step value', REPORT )
        GO TO 190
      END IF

      ! Compute the HIC value
      U.HICDTUP = 0. ! Set unlimited HIC time window
      CALL HIC_S( U.Y(0), S.Y(0), U.NFP, U.NLP, JL, KL, NAR,
     & U.HICDTUP, U.DEL, U.HIC, U.T1, U.T2, IOS )

      ! Report results
      WRITE( *, '(/5X,A,F10.3,5X,A,F9.3,A,5X,A,F9.3,A)', IOSTAT=IOS )
     & 'HIC = ', U.HIC,
     & 'T1 = ', U.T1, ' (msec)',
     & 'T2 = ', U.T2, ' (msec)'
      IF ( REPORT.OPEN )
     & WRITE( REPORT.UNIT, '(5X,A,F10.3,5X,A,F9.3,A,5X,A,F9.3,A)',
     & IOSTAT=IOS )
     & 'HIC = ', U.HIC,
     & 'T1 = ', U.T1, ' (msec)',
     & 'T2 = ', U.T2, ' (msec)'

      ! Set default output file name
      OUT_FILE = FILE_NAME
      CALL FN_EXTRACT( OUT_FILE )
      DEF_FILE = OUT_FILE

      ! Set the output UDS file name
      IF ( .NOT. BATCH ) THEN
  120   WRITE( *, '(/A/3A/'' >> '',$)' )
     &     ' Output UDS file name?   ( N - None )',
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
      ELSE ! Batch
        IF ( UPDATE ) THEN
          OUT_FILE = DEF_FILE
        ELSE
          OUT_FILE = 'N'
        END IF
      END IF

      ! Write the output UDS file
      IF ( OUT_FILE .NE. 'N' ) THEN
        CALL UDS_PUT( OUT_FILE, U, C, REPORT, OUTPUT_LIST,
     &     DIR_OUT, .FALSE., IOS )
      END IF

      ! Loop for next file
  190 WRITE( *, '(80(''_''))' )
      GO TO 100

      ! Clean up and stop
  199 CALL CLEAN_UP( HIT, REPORT, OUTPUT_LIST, DIR_OUT )

      END



      SUBROUTINE HIC_CL( CL_ARG, NUM_ARGS, PROG_NAME, UPDATE )

!***********************************************************************
!* Processes the Program-Specific Command Line Arguments
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) CL_ARG(0:*) ! Command line argument array

      INTEGER NUM_ARGS ! Number of arguments

      CHARACTER*(*) PROG_NAME ! Program name

      LOGICAL UPDATE ! Indicates to output updated UDS files


      ! Variables ______________________________________________________

      INTEGER I, NS

      CHARACTER ARG*255, C


      ! Process command line arguments
      UPDATE = .FALSE.
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
          WRITE( *, '(10X,A)' ) '[UPDATE]'
          IF ( ( ARG(NS:) .EQ. '??' ) .OR.
     &       ( ARG(NS:) .EQ. 'HELP' ) ) THEN ! Show command line help
            CALL HELP_CL( PROG_NAME )
            WRITE( *, '(10X,A)' )
     &         'UPDATE :  Output HIC-updated UDS files'
          END IF
          STOP ' '
        ELSE IF ( ARG(NS:) .EQ. 'UPDATE' ) THEN
          UPDATE = .TRUE.
          CL_ARG(I) = ' '
        END IF
      END DO

      RETURN
      END
