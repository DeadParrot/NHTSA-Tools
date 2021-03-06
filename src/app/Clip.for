      PROGRAM CLIP

!***********************************************************************
!*
!* CLIP Version 2.1
!*
!* Computes the maximum value that the linear interpolation of a UDS time
!* series, typically a chest resultant acceleration filtered to 300 Hz
!* cutoff, meets or exceeds for an interval of at least 3 msec, based on
!* the linearly interpolated time series.
!*
!* The Chest Severity Index (CSI) is also computed.
!*
!* An updated version of the UDS file with the computed CLIP and CSI
!* values can be optionally output.
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
     & BATCH,
     & UPDATE

      INTEGER NUM_ARGS, MAX_ARGS, IOS, NFP_C, I

      PARAMETER ( MAX_ARGS = 12 )

      REAL DELC, TL, TR

      PARAMETER ( DELC = .003 )

      CHARACTER CL_ARG(0:MAX_ARGS)*255, PROG_NAME*8,
     & TEMPLATE_STR*255, DIR_OUT*255,
     & FILE_NAME*255, OUT_FILE*255, DEF_FILE*255,
     & PROMPT*79


      ! Initializations
      PROG_NAME = 'Clip'
      CALL FILE_INIT( HIT )
      CALL GET_CL( CL_ARG, NUM_ARGS, MAX_ARGS )
      CALL CLIP_CL( CL_ARG, NUM_ARGS, PROG_NAME, UPDATE )
      CALL PROC_CL( CL_ARG, NUM_ARGS, PROG_NAME, TEMPLATE_STR,
     & ALL_BATCH, NO_BATCH, NO_INCR, REPORT, OUTPUT_LIST, DIR_OUT,
     & ONE_PASS, .TRUE., .TRUE. )
      PROMPT = 'UDS file name/template/hitlist?'
      PROMPT(74:) = '[done]'
      CALL UDS_CONTROL_INIT( C )
      C.CHANFORM = 'Y' ! Only accept Y-series file input

      WRITE( *, '(//25X,A//)' ) '**  Clip Program (v.2.1)  **'

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
      IF ( ( U.SENATT .NE. 'CHEST' ) .OR. ( U.YUNITS .NE. 'G''S' )
     & .OR. ( U.FCUT .NE. 300. ) ) CALL MSG_WRITE(
     & '*** WARNING - Not 300 Hz CHEST acceleration in G''S', REPORT )
      IF ( ( U.NFP .GT. 0 ) .OR. ( U.NLP .LE. 0 ) ) CALL MSG_WRITE(
     & '*** WARNING - Nonstandard x-axis span', REPORT )
      IF ( U.DEL .LE. 0. ) THEN
        CALL MSG_WRITE(
     &     '*** ERROR - Non-positive file time step value', REPORT )
        GO TO 190
      END IF

      ! Set lower index for CLIP/CSI computation
      NFP_C = MAX( 0, U.NFP )

      ! Compute the CLIP value
      IF ( ( U.NLP - NFP_C ) * U.DEL .LT. DELC ) THEN ! < 3 msec of data
        CALL MSG_WRITE(
     &     '*** WARNING - Less than 3 msec of post-time-zero data - '//
     &     'CLIP3M set to zero', REPORT )
        U.CLIP3M = 0.
        TL = 0.
        TR = 0.
      ELSE
        CALL CLIP_S( U.Y(NFP_C), NFP_C, U.NLP, U.DEL,
     &     U.CLIP3M, TL, TR, IOS )
      END IF

      ! Compute the Chest Severity Index (CSI) value
      U.CSI = ( ABS( U.Y(NFP_C) )**2.5 + ABS( U.Y(U.NLP) )**2.5 ) / 2
      DO I = NFP_C+1, U.NLP-1
        U.CSI = U.CSI + ABS( U.Y(I) )**2.5
      END DO
      U.CSI = U.CSI * U.DEL

      ! Report results
      IF ( ( TL .LE. NFP_C * U.DEL * 1000 + 30. ) .OR.
     & ( TR .GE. U.NLP * U.DEL * 1000 - 30. ) )
     & CALL MSG_WRITE( '*** WARNING - CLIP span overlaps '//
     & 'first or last 30 msec of signal', REPORT )
      WRITE( *, '(/5X,A,F10.3,5X,A,F9.3,A,5X,A,F9.3,A/5X,A,F13.3)',
     & IOSTAT=IOS )
     & 'CLIP3M = ', U.CLIP3M, 'TL = ', TL, ' (msec)',
     & 'TR = ', TR, ' (msec)', 'CSI = ', U.CSI
      IF ( REPORT.OPEN ) WRITE( REPORT.UNIT,
     & '(5X,A,F10.3,5X,A,F9.3,A,5X,A,F9.3,A,5X,A,F13.3)',
     & IOSTAT=IOS )
     & 'CLIP3M = ', U.CLIP3M, 'TL = ', TL, ' (msec)',
     & 'TR = ', TR, ' (msec)', 'CSI = ', U.CSI

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
        IF ( IOS .NE. 0 ) GO TO 190
      END IF

      ! Loop for next file
  190 WRITE( *, '(80(''_''))' )
      GO TO 100

      ! Clean up and stop
  199 CALL CLEAN_UP( HIT, REPORT, OUTPUT_LIST, DIR_OUT )

      END



      SUBROUTINE CLIP_CL( CL_ARG, NUM_ARGS, PROG_NAME, UPDATE )

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
     &         'UPDATE :  Output CLIP/CSI-updated UDS files'
          END IF
          STOP ' '
        ELSE IF ( ARG(NS:) .EQ. 'UPDATE' ) THEN
          UPDATE = .TRUE.
          CL_ARG(I) = ' '
        END IF
      END DO

      RETURN
      END
