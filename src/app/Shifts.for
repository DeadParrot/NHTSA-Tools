      PROGRAM SHIFTS

!***********************************************************************
!* Shifts UDS Files
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
     & TAIL_SPEC_CL, TAIL_SHIFT_CL, TAIL_SHIFT

      INTEGER NUM_ARGS, MAX_ARGS, IOS, I, ISH, NFP_N, NLP_N

      PARAMETER ( MAX_ARGS = 12 )

      DOUBLE PRECISION SHIFT_CL, SHIFT

      CHARACTER CL_ARG(0:MAX_ARGS)*255, PROG_NAME*8,
     & TEMPLATE_STR*255, DIR_OUT*255,
     & FILE_NAME*255, OUT_FILE*255, DEF_FILE*255,
     & PROMPT*79,
     & AXIS_CL, AXIS, SHIFT_C*25, RESP


      ! Initializations
      PROG_NAME = 'Shifts'
      CALL FILE_INIT( HIT )
      CALL GET_CL( CL_ARG, NUM_ARGS, MAX_ARGS )
      CALL SHIFTS_CL( CL_ARG, NUM_ARGS, PROG_NAME,
     & AXIS_CL, SHIFT_CL, TAIL_SPEC_CL, TAIL_SHIFT_CL, NO_FLAG )
      CALL PROC_CL( CL_ARG, NUM_ARGS, PROG_NAME, TEMPLATE_STR,
     & ALL_BATCH, NO_BATCH, NO_INCR, REPORT, OUTPUT_LIST, DIR_OUT,
     & ONE_PASS, .TRUE., .TRUE. )
      PROMPT = 'UDS file name/template/hitlist?'
      PROMPT(74:) = '[done]'
      CALL UDS_CONTROL_INIT( C )
      TAIL_SHIFT = .FALSE.

      WRITE( *, '(//27X,A//)' ) '**  Shift UDS Files  **'

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
     &     ' Axis to shift?   (X/Y)', '[Y]'
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

        ! Get shift amount
  102   IF ( SHIFT_CL .EQ. 0.D0 ) THEN
          WRITE( *, '(/A/'' >> '',$)' ) ' Shift amount?'
          READ( *, '(A)', IOSTAT=IOS ) SHIFT_C
          IF ( IOS .NE. 0 ) THEN
            CALL MSG_WRITE( '*** Operation skipped', REPORT )
            CALL FILE_DEL( HIT )
            GO TO 190
          END IF
          READ( SHIFT_C, '(BN,F25.0)', IOSTAT=IOS ) SHIFT
          IF ( ( IOS .NE. 0 ) .OR. ( SHIFT_C .EQ. ' ' ) ) THEN
            WRITE( *, * ) '*** Invalid shift amount'
            GO TO 102
          END IF
        ELSE
          SHIFT = SHIFT_CL
        END IF

        ! Ask about shifting tails
        IF ( ( .NOT. TAIL_SPEC_CL ) .AND. ( AXIS .EQ. 'X' ) ) THEN
  103     WRITE( *, '(/A,39X,A/'' >> '',$)' )
     &     ' Shift y-series tail indices?   (Y/N)', '[No]'
          READ( *, '(A)', IOSTAT=IOS ) RESP
          IF ( IOS .NE. 0 ) THEN
            CALL MSG_WRITE( '*** Operation skipped', REPORT )
            CALL FILE_DEL( HIT )
            GO TO 190
          END IF
          CALL STR_UP( RESP )
          IF ( .NOT. ANY_CHARS( RESP, ' YN' ) ) THEN
            WRITE( *, * ) '*** Invalid response'
            GO TO 103
          END IF
          IF ( RESP .EQ. 'Y' ) THEN
            TAIL_SHIFT = .TRUE.
          ELSE
            TAIL_SHIFT = .FALSE.
          END IF
        ELSE
          TAIL_SHIFT = TAIL_SHIFT_CL
        END IF

      END IF

      ! Read UDS file
      CALL REP_COLS( REPORT )
      CALL UDS_GET( FILE_NAME, U, C, REPORT, IOS )
      IF ( IOS .NE. 0 ) GO TO 190

      IF ( .NOT. GROUP ) THEN ! Interactive prompts

        ! Get axis
  104   IF ( AXIS_CL .EQ. ' ' ) THEN
          WRITE( *, * )
          WRITE( *, * ) '  X-axis is ', U.XTYP(:L_TRIM(U.XTYP)),
     &     ' ('//U.XUNITS(:L_TRIM(U.XUNITS))//')'
          WRITE( *, * ) '  Y-axis is ', U.YTYP(:L_TRIM(U.YTYP)),
     &     ' ('//U.YUNITS(:L_TRIM(U.YUNITS))//')'
          WRITE( *, '(/A,54X,A/'' >> '',$)' )
     &     ' Axis to shift?   (X/Y)', '[Y]'
          READ( *, '(A)', END=190 ) AXIS
          CALL STR_UP( AXIS )
          IF ( .NOT. ANY_CHARS( AXIS, ' XY' ) ) THEN
            WRITE( *, * ) '*** Invalid response'
            GO TO 104
          END IF
          IF ( AXIS .EQ. ' ' ) AXIS = 'Y'
        ELSE
          AXIS = AXIS_CL
        END IF

        ! Get shift amount
  105   IF ( SHIFT_CL .EQ. 0.D0 ) THEN
          WRITE( *, '(/A/'' >> '',$)' ) ' Shift amount?'
          READ( *, '(A)', END=104 ) SHIFT_C
          READ( SHIFT_C, '(BN,F25.0)', IOSTAT=IOS ) SHIFT
          IF ( ( IOS .NE. 0 ) .OR. ( SHIFT_C .EQ. ' ' ) ) THEN
            WRITE( *, * ) '*** Invalid shift amount'
            GO TO 105
          END IF
        ELSE
          SHIFT = SHIFT_CL
        END IF

        ! Ask about shifting tails
        IF ( ( .NOT. TAIL_SPEC_CL ) .AND. ( AXIS .EQ. 'X' ) .AND.
     &   ( U.CHANFORM .EQ. 'Y' ) ) THEN
  106     WRITE( *, '(/A,39X,A/'' >> '',$)' )
     &     ' Shift y-series tail indices?   (Y/N)', '[No]'
          READ( *, '(A)', END=104 ) RESP
          CALL STR_UP( RESP )
          IF ( .NOT. ANY_CHARS( RESP, ' YN' ) ) THEN
            WRITE( *, * ) '*** Invalid response'
            GO TO 106
          END IF
          IF ( RESP .EQ. 'Y' ) THEN
            TAIL_SHIFT = .TRUE.
          ELSE
            TAIL_SHIFT = .FALSE.
          END IF
        ELSE
          TAIL_SHIFT = TAIL_SHIFT_CL
        END IF
      END IF

      ! Perform shift
      IF ( AXIS .EQ. 'X' ) THEN
        IF ( U.CHANFORM .EQ. 'Y' ) THEN ! Shift X data
          IF ( U.DEL .EQ. 0. ) THEN
            CALL MSG_WRITE(
     &       '*** ERROR - X-axis step (DEL) is zero', REPORT )
            GO TO 190
          END IF
          ISH = NINT( SHIFT / U.DEL )
          SHIFT = ISH * U.DEL
          IF ( ISH .NE. 0 ) THEN ! Do the shift
            IF ( ( U.NFP + ISH .GT. NLP_U ) .OR.
     &       ( U.NLP + ISH .LT. NFP_L ) ) THEN ! Shift beyond limits
              IF ( TAIL_SHIFT ) THEN ! Nothing to output
                CALL MSG_WRITE( '*** ERROR - Shift puts '//
     &           'signal beyond index bounds', REPORT )
                GO TO 190
              ELSE ! Zero-valued output
                CALL MSG_WRITE( '*** WARNING - Shift puts '//
     &           'signal beyond index bounds', REPORT )
              END IF
            ELSE IF ( ( ( U.NLP + ISH .GT. NLP_U ) .OR.
     &       ( U.NFP + ISH .LT. NFP_L ) ) .AND.
     &       ( TAIL_SHIFT ) ) THEN ! Truncation at limits
              CALL MSG_WRITE(
     &         '*** WARNING - Tail truncated at index bound',
     &         REPORT )
            ELSE IF ( ( ( U.NFP + ISH .GT. U.NLP ) .OR.
     &       ( U.NLP + ISH .LT. U.NFP ) ) .AND.
     &       ( .NOT. TAIL_SHIFT ) ) THEN ! Zero-valued output
              CALL MSG_WRITE(
     &         '*** WARNING - Data shifted outside domain => '//
     &         'Zero-valued output', REPORT )
            END IF
            IF ( TAIL_SHIFT ) THEN
              NFP_N = MIN( MAX( U.NFP + ISH, NFP_L ), NLP_U )
              NLP_N = MIN( MAX( U.NLP + ISH, NFP_L ), NLP_U )
            ELSE
              NFP_N = U.NFP
              NLP_N = U.NLP
            END IF
            IF ( ISH .GT. 0 ) THEN
              DO I = NLP_N, U.NFP+ISH, -1
                U.Y(I) = U.Y(I-ISH)
              END DO
              DO I = NFP_N, MIN( U.NFP+ISH-1, NLP_U )
                U.Y(I) = 0.
              END DO
            ELSE ! ISH < 0
              DO I = NFP_N, U.NLP+ISH
                U.Y(I) = U.Y(I-ISH)
              END DO
              DO I = NLP_N, MAX( U.NLP+ISH+1, NFP_L ), -1
                U.Y(I) = 0.
              END DO
            END IF
            IF ( TAIL_SHIFT ) THEN ! Reset tail indices
              U.NFP = NFP_N
              U.NLP = NLP_N
            END IF
          ELSE
            CALL MSG_WRITE(
     &       '*** Shift < (DEL/2)  =>  No x-axis shift', REPORT )
          END IF
        ELSE IF ( U.CHANFORM .EQ. 'X-Y' ) THEN ! Shift X data
          DO I = U.NFP, U.NLP
            U.X(I) = REAL( U.X(I) + SHIFT )
          END DO
        END IF
      ELSE ! Shift Y data
        DO I = U.NFP, U.NLP
          U.Y(I) = REAL( U.Y(I) + SHIFT )
        END DO
      END IF

      ! Prepare output fields
      U.STATUS = 'SHIFTED'
      U.HIC = 0.
      U.T1 = 0.
      U.T2 = 0.
      U.HICDTUP = 0.
      U.CLIP3M = 0.
      U.CSI = 0.
      U.CD2 = AXIS//'-data shifted by '
      WRITE( U.CD2(20:), '(G15.6)', IOSTAT=IOS ) SHIFT
      IF ( REPORT.OPEN ) WRITE( REPORT.UNIT, '(5X,A,G15.6)' )
     & AXIS//'-data shifted by ', SHIFT

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



      SUBROUTINE SHIFTS_CL( CL_ARG, NUM_ARGS, PROG_NAME,
     & AXIS_CL, SHIFT_CL, TAIL_SPEC_CL, TAIL_SHIFT_CL, NO_FLAG )

!***********************************************************************
!* Processes the Program-Specific Command Line Arguments
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) CL_ARG(0:*) ! Command line argument array

      INTEGER NUM_ARGS ! Number of arguments

      CHARACTER*(*) PROG_NAME ! Program name

      CHARACTER AXIS_CL ! Axis to shift (X or Y)

      DOUBLE PRECISION SHIFT_CL ! Shift amount

      LOGICAL TAIL_SPEC_CL ! Indicates tail shift was specified

      LOGICAL TAIL_SHIFT_CL ! Indicates whether to shift y-series tails

      LOGICAL NO_FLAG ! Indicates not to flag output file names


      ! Variables ______________________________________________________

      INTEGER I, NS, IOS

      CHARACTER ARG*255, C


      ! Process command line arguments
      AXIS_CL = ' '
      SHIFT_CL = 0.D0
      NO_FLAG = .FALSE.
      TAIL_SPEC_CL = .FALSE.
      TAIL_SHIFT_CL = .FALSE.
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
     &     '[AXIS=<axis>] [SHIFT=<shift>] [[NO_]TAILS] [NO_FLAG]'
          IF ( ( ARG(NS:) .EQ. '??' ) .OR.
     &     ( ARG(NS:) .EQ. 'HELP' ) ) THEN ! Show command line help
            CALL HELP_CL( PROG_NAME )
            WRITE( *, '(3(10X,A/),10X,A)' )
     &       '<axis> :  Axis to shift (X or Y)',
     &       '<shift> :  Shift amount',
     &       'TAILS/NO_TAILS :  '//
     &       'Do/don''t shift y-series tail indices',
     &       'NO_FLAG :  Don''t flag output file names'
          END IF
          STOP ' '
        ELSE IF ( ( ARG(NS:NS+4) .EQ. 'AXIS=' ) .OR.
     &   ( ARG(NS:NS+4) .EQ. 'AXIS#' ) ) THEN
          AXIS_CL = ARG(NS+5:NS+5)
          IF ( .NOT. ANY_CHARS( AXIS_CL, 'XY' ) ) THEN
            WRITE( *, '(/2A)' ) ' *** Illegal scale axis: ',
     &       AXIS_CL
            AXIS_CL = ' '
          END IF
          CL_ARG(I) = ' '
        ELSE IF ( ( ARG(NS:NS+5) .EQ. 'SHIFT=' ) .OR.
     &   ( ARG(NS:NS+5) .EQ. 'SHIFT#' ) ) THEN
          READ( ARG(NS+6:), '(BN,F25.0)', IOSTAT=IOS ) SHIFT_CL
          IF ( IOS .NE. 0 ) THEN
            WRITE( *, '(/2A)', IOSTAT=IOS )
     &       ' *** Illegal shift value: ',
     &       ARG(NS+6:L_TRIM(ARG))
            SHIFT_CL = 0.D0
          END IF
          CL_ARG(I) = ' '
        ELSE IF ( ARG(NS:) .EQ. 'TAILS' ) THEN
          TAIL_SHIFT_CL = .TRUE.
          TAIL_SPEC_CL = .TRUE.
          CL_ARG(I) = ' '
        ELSE IF ( ( ARG(NS:) .EQ. 'NO_TAILS' ) .OR.
     &   ( ARG(NS:) .EQ. 'NOTAILS' ) ) THEN
          TAIL_SHIFT_CL = .FALSE.
          TAIL_SPEC_CL = .TRUE.
          CL_ARG(I) = ' '
        ELSE IF ( ( ARG(NS:) .EQ. 'NO_FLAG' ) .OR.
     &   ( ARG(NS:) .EQ. 'NOFLAG' ) ) THEN
          NO_FLAG = .TRUE.
          CL_ARG(I) = ' '
        END IF
      END DO

      RETURN
      END
