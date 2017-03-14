      PROGRAM ALIGN

!***********************************************************************
!* Aligns UDS Y-Series Files Using One of Three Methods
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

      RECORD /UDS_Y/  R, U
      RECORD /UDS_CONTROL/  C
      RECORD /FILE/  HIT, REPORT, OUTPUT_LIST

      LOGICAL ALL_BATCH, NO_BATCH, NO_INCR, ONE_PASS, NEW_HIT, EOF,
     & BATCH, DONE, NO_FLAG, TAIL_SHIFT

      INTEGER NUM_ARGS, MAX_ARGS, IOS, NFP, NLP, MAX_SHIFT,
     & I, IR, IU, ISH, NFP_N, NLP_N

      PARAMETER ( MAX_ARGS = 12 )

      REAL R_PEAK, U_PEAK, YI

      DOUBLE PRECISION PERCENT, RMS, RMS_MIN, RSUM, RCH, USUM, UCH,
     & SHIFT

      CHARACTER CL_ARG(0:MAX_ARGS)*255, PROG_NAME*8,
     & TEMPLATE_STR*255, DIR_OUT*255,
     & FILE_NAME*255, OUT_FILE*255, DEF_FILE*255,
     & PROMPT_1*79, PROMPT_2*79,
     & METHOD, REF_MODE


      ! Initializations
      PROG_NAME = 'Align'
      CALL FILE_INIT( HIT )
      CALL GET_CL( CL_ARG, NUM_ARGS, MAX_ARGS )
      CALL ALIGN_CL( CL_ARG, NUM_ARGS, PROG_NAME, NO_FLAG )
      CALL PROC_CL( CL_ARG, NUM_ARGS, PROG_NAME, TEMPLATE_STR,
     & ALL_BATCH, NO_BATCH, NO_INCR, REPORT, OUTPUT_LIST, DIR_OUT,
     & ONE_PASS, .TRUE., .TRUE. )
      PROMPT_1 = 'Reference UDS file name/template/hitlist?'
      PROMPT_1(74:) = '[done]'
      PROMPT_2 = 'Align UDS file name/template/hitlist?'
      PROMPT_2(71:) = '[no more]'
      CALL UDS_CONTROL_INIT( C )
      C.CHANFORM = 'Y' ! Only accept Y-series file input
      ISH = 0

      WRITE( *, '(//27X,A//)' ) '**  Align UDS Files  **'

      ! Read the first file name/template/hitlist
  100 CALL FILE_PROMPT( TEMPLATE_STR, PROMPT_1, DIR_OUT,
     & ONE_PASS, HIT, NEW_HIT, FILE_NAME, EOF )
      IF ( ( FILE_NAME .EQ. ' ' ) .OR. ( EOF ) ) GO TO 199 ! Done

      ! Batch setup
      CALL BAT_SET( ALL_BATCH, NO_BATCH, NEW_HIT, HIT, BATCH )

      ! Batch prompts
      IF ( ( NEW_HIT ) .AND. ( BATCH ) ) THEN

        ! Get align process spec's
        CALL GET_ALIGN_SPECS( METHOD, TAIL_SHIFT, PERCENT, EOF )
        IF ( EOF ) THEN
          CALL MSG_WRITE( '*** Operation skipped', REPORT )
          CALL FILE_DEL( HIT )
          GO TO 190
        END IF

        ! Get reference file mode
  101   WRITE( *, '(/2A,20X,A/'' >> '',$)' )
     &   ' Reference file(s)?   ',
     &   '( F - First,  A - Alternating )', '[First]'
        READ( *, '(A)', IOSTAT=IOS ) REF_MODE
        IF ( IOS .NE. 0 ) THEN
          CALL MSG_WRITE( '*** Operation skipped', REPORT )
          CALL FILE_DEL( HIT )
          GO TO 190
        END IF
        CALL STR_UP( REF_MODE )
        IF ( .NOT. ANY_CHARS( REF_MODE, ' FA' ) ) THEN
          WRITE( *, * ) '*** Invalid response'
          GO TO 101
        END IF
        IF ( REF_MODE .EQ. ' ' ) REF_MODE = 'F'

      END IF

      ! Read first UDS file
      CALL REP_COLS( REPORT )
      C.DIMSYS = ' '
      CALL UDS_GET( FILE_NAME, R, C, REPORT, IOS )
      IF ( IOS .NE. 0 ) THEN ! UDS read error
        IF ( HIT.OPEN ) THEN
          IF ( ( BATCH ) .AND. ( REF_MODE .EQ. 'F' ) ) THEN
            CALL FILE_DEL( HIT )
          ELSE
            CALL FILE_SKIP( HIT ) ! Skip file
          END IF
        END IF
        GO TO 190
      END IF

      ! Check UDS fields
      IF ( R.DEL .EQ. 0. ) THEN
        CALL MSG_WRITE( '*** ERROR - DEL step = 0', REPORT )
        IF ( HIT.OPEN ) THEN
          IF ( ( BATCH ) .AND. ( REF_MODE .EQ. 'F' ) ) THEN
            CALL FILE_DEL( HIT )
          ELSE
            CALL FILE_SKIP( HIT ) ! Skip file
          END IF
        END IF
        GO TO 190
      END IF

      ! Process first UDS file
      C.DIMSYS = R.DIMSYS

      ! Read/process the other UDS files
  109 DONE = .FALSE.
      DO WHILE ( .NOT. DONE )

        ! Read another file name/template/hitlist
  110   CALL FILE_PROMPT( TEMPLATE_STR, PROMPT_2, DIR_OUT,
     &   ONE_PASS, HIT, NEW_HIT, FILE_NAME, EOF )

        IF ( EOF ) THEN
          GO TO 190
        ELSE IF ( FILE_NAME .EQ. ' ' ) THEN
          GO TO 190
        ELSE ! Read/process a file

          ! Batch setup
          CALL BAT_SET( ALL_BATCH, NO_BATCH, NEW_HIT, HIT, BATCH )

          ! Batch prompts
          IF ( ( NEW_HIT ) .AND. ( BATCH ) ) THEN

            ! Get align process spec's
            CALL GET_ALIGN_SPECS( METHOD, TAIL_SHIFT, PERCENT, EOF )
            IF ( EOF ) THEN
              CALL MSG_WRITE( '*** Operation skipped', REPORT )
              CALL FILE_DEL( HIT )
              GO TO 190
            END IF

            ! Get reference file mode
  111       WRITE( *, '(/2A,20X,A/'' >> '',$)' )
     &       ' Reference file(s)?   ',
     &       '( F - First,  A - Alternating )', '[First]'
            READ( *, '(A)', IOSTAT=IOS ) REF_MODE
            IF ( IOS .NE. 0 ) THEN
              CALL MSG_WRITE( '*** Operation skipped', REPORT )
              CALL FILE_DEL( HIT )
              GO TO 190
            END IF
            CALL STR_UP( REF_MODE )
            IF ( .NOT. ANY_CHARS( REF_MODE, ' FA' ) ) THEN
              WRITE( *, * ) '*** Invalid response'
              GO TO 111
            END IF
            IF ( REF_MODE .EQ. ' ' ) REF_MODE = 'F'

          END IF

          ! Read next UDS file
          CALL UDS_GET( FILE_NAME, U, C, REPORT, IOS )
          IF ( IOS .NE. 0 ) THEN ! UDS read error
            IF ( ( HIT.OPEN ) .OR. ( ONE_PASS ) ) THEN ! Skip to next
              IF ( ( BATCH ) .AND. ( REF_MODE .EQ. 'F' ) ) THEN
                CALL FILE_DEL( HIT )
              END IF
              GO TO 189
            ELSE
              GO TO 110
            END IF
          END IF

          ! Check UDS fields
          IF ( U.DEL .EQ. 0. ) THEN
            CALL MSG_WRITE( '*** ERROR - DEL step = 0', REPORT )
            IF ( ( HIT.OPEN ) .OR. ( ONE_PASS ) ) THEN ! Skip to next
              IF ( ( BATCH ) .AND. ( REF_MODE .EQ. 'F' ) ) THEN
                CALL FILE_DEL( HIT )
              END IF
              GO TO 189
            ELSE
              GO TO 110
            END IF
          END IF
          IF ( U.YUNITS .NE. R.YUNITS ) CALL MSG_WRITE(
     &     '*** WARNING - Inconsistent Y-units', REPORT )
          IF ( U.XUNITS .NE. R.XUNITS ) CALL MSG_WRITE(
     &     '*** WARNING - Inconsistent X-units', REPORT )

          DONE = .TRUE.
        END IF

      END DO

      IF ( .NOT. BATCH ) THEN ! Interactive prompts

        ! Get align process spec's
        CALL GET_ALIGN_SPECS( METHOD, TAIL_SHIFT, PERCENT, EOF )
        IF ( EOF ) THEN
          CALL MSG_WRITE( '*** Operation skipped', REPORT )
          CALL FILE_DEL( HIT )
          GO TO 190
        END IF

        ! Use "first" reference mode
        REF_MODE = 'F'

      END IF

      ! Perform alignment
      IF ( METHOD .EQ. 'P' ) THEN ! Peak alignment

        ! Find reference peak
        R_PEAK = 0.
        IR = R.NFP
        DO I = R.NFP, R.NLP
          YI = ABS( R.Y(I) )
          IF ( YI .GT. R_PEAK ) THEN
            R_PEAK = YI
            IR = I
          END IF
        END DO

        ! Find comparison peak
        U_PEAK = 0.
        IU = U.NFP
        DO I = U.NFP, U.NLP
          YI = ABS( U.Y(I) )
          IF ( YI .GT. U_PEAK ) THEN
            U_PEAK = YI
            IU = I
          END IF
        END DO

        ! Set shift value
        IF ( R_PEAK * U_PEAK .LT. 0. )
     &   CALL MSG_WRITE( '*** WARNING - Peak signs differ', REPORT )
        ISH = NINT( IR * R.DEL / U.DEL ) - IU
        U.CD1 = 'Peak aligned'

      ELSE IF ( METHOD .EQ. 'R' ) THEN

        ! Check for equal x-axis step
        IF ( .NOT. EQUAL_SP( U.DEL, R.DEL, 2.E-7 ) ) THEN
          CALL MSG_WRITE( '*** ERROR - Inconsistent DEL step', REPORT )
          IF ( ( HIT.OPEN ) .OR. ( ONE_PASS ) ) THEN ! Skip to next
            CALL MSG_WRITE( '*** Operation skipped', REPORT )
          END IF
          GO TO 189
        END IF

        ! Find shift that minimizes RMS difference
        ISH = 0
        NFP = MAX( R.NFP, U.NFP )
        NLP = MIN( R.NLP, U.NLP )
        CALL RMSDIFF( R.Y(NFP), U.Y(NFP), NFP, NLP, RMS_MIN )
        MAX_SHIFT = INT( ( U.NLP - U.NFP ) * PERCENT * .01D0 )
        DO I = MAX( -MAX_SHIFT, R.NFP-U.NLP ),
     &   MIN( MAX_SHIFT, R.NLP-U.NFP )
          NFP = MAX( R.NFP, U.NFP+I )
          NLP = MIN( R.NLP, U.NLP+I )
          CALL RMSDIFF( R.Y(NFP), U.Y(NFP-I), NFP, NLP, RMS )
          IF ( RMS .LT. RMS_MIN ) THEN
            RMS_MIN = RMS
            ISH = I
          END IF
        END DO

        U.CD1 = 'RMS aligned'

      ELSE IF ( METHOD .EQ. 'I' ) THEN

        ! Check for positive indexes
        IF ( MAX( R.NLP, U.NLP ) .LE. 0 ) THEN
          CALL MSG_WRITE( '*** ERROR - No positive index data', REPORT )
          IF ( ( HIT.OPEN ) .OR. ( ONE_PASS ) ) THEN ! Skip to next
            CALL MSG_WRITE( '*** Operation skipped', REPORT )
          END IF
          GO TO 189
        END IF

        ! Compute reference curve "integral"
        RSUM = 0.D0
        DO I = MAX( 0, R.NFP ), R.NLP-1
          RSUM = RSUM + ( R.Y(I) + R.Y(I+1) ) * .5D0
        END DO
        RCH = RSUM * PERCENT * .01D0
        IF ( RCH .EQ. 0.D0 ) THEN
          CALL MSG_WRITE(
     &     '*** ERROR - Reference curve integral is zero', REPORT )
          GO TO 189
        END IF
        I = MAX( 0, R.NFP )
        RSUM = 0.D0
        DO WHILE ( ( RSUM / RCH .LT. 1.D0 ) .AND. ( I .LT. R.NLP ) )
          RSUM = RSUM + ( R.Y(I) + R.Y(I+1) ) * .5D0
          I = I + 1
        END DO
        IR = I

        ! Compute comparison curve "integral"
        USUM = 0.D0
        DO I = MAX( 0, U.NFP ), U.NLP-1
          USUM = USUM + ( U.Y(I) + U.Y(I+1) ) * .5D0
        END DO
        UCH = USUM * PERCENT * .01D0
        IF ( UCH .EQ. 0.D0 ) THEN
          CALL MSG_WRITE(
     &     '*** ERROR - Comparison curve integral is zero', REPORT )
          GO TO 189
        END IF
        I = MAX( 0, U.NFP )
        USUM = 0.D0
        DO WHILE ( ( USUM / UCH .LT. 1.D0 ) .AND. ( I .LT. U.NLP ) )
          USUM = USUM + ( U.Y(I) + U.Y(I+1) ) * .5D0
          I = I + 1
        END DO
        IU = I

        ! Set shift value
        ISH = NINT( IR * R.DEL / U.DEL ) - IU
        U.CD1 = 'Integral aligned'

      END IF

      ! Shift comparison data
      SHIFT = ISH * U.DEL
      IF ( ISH .NE. 0 ) THEN ! Do the shift
        IF ( ( U.NFP + ISH .GT. NLP_U ) .OR.
     &   ( U.NLP + ISH .LT. NFP_L ) ) THEN ! Shift beyond limits
          IF ( TAIL_SHIFT ) THEN ! Nothing to output
            CALL MSG_WRITE( '*** ERROR - Shift puts '//
     &       'signal beyond index bounds', REPORT )
            GO TO 190
          ELSE ! Zero-valued output
            CALL MSG_WRITE( '*** WARNING - Shift puts '//
     &       'signal beyond index bounds', REPORT )
          END IF
        ELSE IF ( ( ( U.NLP + ISH .GT. NLP_U ) .OR.
     &   ( U.NFP + ISH .LT. NFP_L ) ) .AND.
     &   ( TAIL_SHIFT ) ) THEN ! Truncation at limits
          CALL MSG_WRITE(
     &     '*** WARNING - Tail truncated at index bound',
     &     REPORT )
        ELSE IF ( ( ( U.NFP + ISH .GT. U.NLP ) .OR.
     &   ( U.NLP + ISH .LT. U.NFP ) ) .AND.
     &   ( .NOT. TAIL_SHIFT ) ) THEN ! Zero-valued output
          CALL MSG_WRITE(
     &     '*** WARNING - Data shifted outside domain => '//
     &     'Zero-valued output', REPORT )
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
        CALL MSG_WRITE( '*** Shift < (DEL/2)  =>  No shift', REPORT )
      END IF

      ! Prepare output fields
      U.STATUS = 'ALIGNED'
      U.HIC = 0.
      U.T1 = 0.
      U.T2 = 0.
      U.HICDTUP = 0.
      U.CLIP3M = 0.
      U.CSI = 0.
      U.CD2 = 'X-data shifted by'
      WRITE( U.CD2(20:), '(G15.6)', IOSTAT=IOS ) SHIFT
      IF ( REPORT.OPEN ) WRITE( REPORT.UNIT, '(5X,A,G15.6)' )
     & 'X-data shifted by ', SHIFT

      ! Report results
      IF ( .NOT. BATCH ) WRITE( *, '(/5X,A,G15.6,A)' )
     & 'Shifted by: ', SHIFT,
     & ' ('//U.XUNITS(:L_TRIM(U.XUNITS))//')'

      ! Set default output file name
      OUT_FILE = FILE_NAME
      CALL FN_EXTRACT( OUT_FILE )
      IF ( .NOT. NO_FLAG ) CALL FN_FLAG( OUT_FILE, 'l', 2, I )
      CALL ADD_PATH( DIR_OUT, OUT_FILE )
      IF ( .NOT. NO_INCR ) CALL FN_INCR( OUT_FILE )
      CALL FN_NAME_EXTRACT( OUT_FILE )
      DEF_FILE = OUT_FILE

      ! Set the output UDS file name
      IF ( .NOT. BATCH ) THEN
  120   WRITE( *, '(/A/3A/'' >> '',$)' )
     &   ' Output UDS file name?',
     &   ' [', DEF_FILE(:L_TRIM(DEF_FILE)), ']'
        READ( *, '(A)', END=189 ) OUT_FILE
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
      IF ( REPORT.OPEN ) WRITE( REPORT.UNIT, '(5X,A,G15.6,A)' )
     & 'Shifted by ', ISH * U.DEL,
     & ' ('//U.XUNITS(:L_TRIM(U.XUNITS))//')'

      ! Loop for next align
  189 IF ( REF_MODE .EQ. 'F' ) THEN
        IF ( HIT.OPEN .OR. .NOT. BATCH ) THEN
          WRITE( *, '(80(''_'')/)' )
          GO TO 109
        END IF
      END IF
  190 WRITE( *, '(80(''_''))' )
      GO TO 100

      ! Clean up and stop
  199 CALL CLEAN_UP( HIT, REPORT, OUTPUT_LIST, DIR_OUT )

      END



      SUBROUTINE ALIGN_CL( CL_ARG, NUM_ARGS, PROG_NAME, NO_FLAG )

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
     &   ( ARG(NS:) .EQ. '??' ) .OR.
     &   ( ARG(NS:) .EQ. 'HELP' ) ) THEN ! Show command line syntax
          CALL SYNTAX_CL( PROG_NAME )
          WRITE( *, '(10X,A)' ) '[NO_FLAG]'
          IF ( ( ARG(NS:) .EQ. '??' ) .OR.
     &     ( ARG(NS:) .EQ. 'HELP' ) ) THEN ! Show command line help
            CALL HELP_CL( PROG_NAME )
            WRITE( *, '(10X,A)' )
     &       'NO_FLAG :  Don''t flag output file names'
          END IF
          STOP ' '
        ELSE IF ( ( ARG(NS:) .EQ. 'NO_FLAG' ) .OR.
     &   ( ARG(NS:) .EQ. 'NOFLAG' ) ) THEN
          NO_FLAG = .TRUE.
          CL_ARG(I) = ' '
        END IF
      END DO

      RETURN
      END



      SUBROUTINE GET_ALIGN_SPECS( METHOD, TAIL_SHIFT, PERCENT, EOF )

!***********************************************************************
!* Gets the Alignment Process Specifications
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER METHOD

      LOGICAL TAIL_SHIFT

      DOUBLE PRECISION PERCENT

      LOGICAL EOF


      ! Variables ______________________________________________________

      INTEGER IOS

      CHARACTER RESP, PERCENT_C*25


      ! Choose alignment method
  101 WRITE( *, '(/2A,16X,A/'' >> '',$)' )
     & ' Alignment method?   ',
     & '( P - Peak,  R - RMS,  I - Integral )', '[Peak]'
      READ( *, '(A)', END=190 ) METHOD
      CALL STR_UP( METHOD )
      IF ( .NOT. ANY_CHARS( METHOD, ' PRI' ) ) THEN
        WRITE( *, * ) '*** Invalid response'
        GO TO 101
      END IF
      IF ( METHOD .EQ. ' ' ) METHOD = 'P'

      ! Ask about shifting tails
  102 WRITE( *, '(/A,48X,A/'' >> '',$)' )
     & ' Shift tail indices?   (Y/N)', '[No]'
      READ( *, '(A)', END=101 ) RESP
      CALL STR_UP( RESP )
      IF ( .NOT. ANY_CHARS( RESP, ' YN' ) ) THEN
        WRITE( *, * ) '*** Invalid response'
        GO TO 102
      END IF
      IF ( RESP .EQ. 'Y' ) THEN
        TAIL_SHIFT = .TRUE.
      ELSE
        TAIL_SHIFT = .FALSE.
      END IF

      ! Get method-specific info
      IF ( METHOD .EQ. 'P' ) THEN
        ! No info needed
      ELSE IF ( METHOD .EQ. 'R' ) THEN
  103   WRITE( *, '(/A,40X,A/'' >> '',$)' )
     &   ' Max percent of span to shift curve?', '[25]'
        READ( *, '(A)', END=101 ) PERCENT_C
        IF ( PERCENT_C .EQ. ' ' ) THEN
          PERCENT = 25.D0
        ELSE
          READ( PERCENT_C, '(BN,F25.0)', IOSTAT=IOS ) PERCENT
          IF ( ( IOS .NE. 0 ) .OR. ( PERCENT .LE. 0.D0 ) .OR.
     &     ( PERCENT .GT. 100.D0 ) ) THEN
            WRITE( *, * ) '*** Invalid percent'
            GO TO 103
          END IF
        END IF
      ELSE IF ( METHOD .EQ. 'I' ) THEN
  104   WRITE( *, '(/A,37X,A/'' >> '',$)' )
     &   ' Percent of integral value to align on?', '[10]'
        READ( *, '(A)', END=101 ) PERCENT_C
        IF ( PERCENT_C .EQ. ' ' ) THEN
          PERCENT = 10.D0
        ELSE
          READ( PERCENT_C, '(BN,F25.0)', IOSTAT=IOS ) PERCENT
          IF ( ( IOS .NE. 0 ) .OR. ( PERCENT .LE. 0.D0 ) .OR.
     &     ( PERCENT .GT. 100.D0 ) ) THEN
            WRITE( *, * ) '*** Invalid percent'
            GO TO 104
          END IF
        END IF
      END IF

      EOF = .FALSE.
      RETURN

      ! Ctrl-Z handler
  190 EOF = .TRUE.
      RETURN

      END



      SUBROUTINE RMSDIFF( Y1, Y2, NFP, NLP, RMS )

!***********************************************************************
!* Computes the RMS Difference Between Two Signals
!***********************************************************************


      ! Arguments ______________________________________________________

      INTEGER NFP

      INTEGER NLP

      REAL Y1(NFP:NLP)

      REAL Y2(NFP:NLP)

      DOUBLE PRECISION RMS


      ! Variables ______________________________________________________

      INTEGER I


      ! Compute RMS difference
      RMS = 0.D0
      DO I = NFP, NLP
        RMS = RMS + ( DBLE( Y1(I) ) - Y2(I) )**2
      END DO
      RMS = SQRT( RMS / ( NLP - NFP + 1 ) )

      RETURN
      END
