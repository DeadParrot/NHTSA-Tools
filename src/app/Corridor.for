      PROGRAM CORRIDOR

!***********************************************************************
!* Calculates a Signal Corridor from Y-Series Files
!* and Outputs Average and Cumulative Variance.
!*
!* Language: Fortran
!*
!* Author: Dinesh Sharma
!*         Andrew Orndorff
!*         Shashi Kuppa
!*
!* Date: 2003/07/26
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'
      INCLUDE 'file.fi'


      ! Variables ______________________________________________________

      RECORD /UDS_Y/  S, U
      RECORD /UDS_CONTROL/  C
      RECORD /FILE/  HIT, REPORT, OUTPUT_LIST

      LOGICAL ALL_BATCH, NO_BATCH, NO_INCR, ONE_PASS, NEW_HIT, EOF,
     & BATCH, DONE, DEF_MINIMIZE_RANGE,
     & MINIMIZE_RANGE, WARN_LOW_NUM_POINTS

      INTEGER NUM_ARGS, MAX_ARGS, IOS, IOS_W, I, IE, NUM_FILES,
     & MIN_NFP, MAX_NFP, MIN_NLP, MAX_NLP, NFP_LIMIT, NLP_LIMIT,
     & ANUM_POINTS(NFP_L:NLP_U)

      REAL V_SQ(NFP_L:NLP_U), V(NFP_L:NLP_U), SD(NFP_L:NLP_U),
     & MN(NFP_L:NLP_U), AVERAGE_VARIANCE, CUMULATIVE_VARIANCE

      PARAMETER ( MAX_ARGS = 7 )

      CHARACTER CL_ARG(0:MAX_ARGS)*255, PROG_NAME*8,
     & TEMPLATE_STR*255, DIR_OUT*255,
     & FILE_NAME*255, OUT_FILE*255, DEF_FILE*255,
     & MPS_FILE*255, MMS_FILE*255, OMPS_FILE*255, OMMS_FILE*255,
     & PROMPT_1*79, PROMPT_2*79,
     & XUNITS_1*20, YUNITS_1*20, RANGE_C, MESSAGE*132


      ! Initializations
      PROG_NAME = 'Corridor'
      CALL FILE_INIT( HIT )
      MINIMIZE_RANGE = .FALSE.
      CALL GET_CL( CL_ARG, NUM_ARGS, MAX_ARGS )
      CALL CORRIDOR_CL( CL_ARG, NUM_ARGS, PROG_NAME,
     & DEF_MINIMIZE_RANGE, MINIMIZE_RANGE )
      CALL PROC_CL( CL_ARG, NUM_ARGS, PROG_NAME, TEMPLATE_STR,
     & ALL_BATCH, NO_BATCH, NO_INCR, REPORT, OUTPUT_LIST, DIR_OUT,
     & ONE_PASS, .TRUE., .TRUE. )
      PROMPT_1 = 'First UDS file name/template/hitlist?'
      PROMPT_1(74:) = '[done]'
      PROMPT_2 = 'Next UDS file name/template/hitlist?'
      PROMPT_2(71:) = '[no more]'
      CALL UDS_CONTROL_INIT( C )
      C.CHANFORM = 'Y' ! Only accept Y-series file input

      WRITE( *, '(//23X,A//)' ) '**  Calculate a Signal Corridor **'

      ! Read the first file name/template/hitlist
  100 CALL FILE_PROMPT( TEMPLATE_STR, PROMPT_1, DIR_OUT,
     & ONE_PASS, HIT, NEW_HIT, FILE_NAME, EOF )
      IF ( ( FILE_NAME .EQ. ' ' ) .OR. ( EOF ) ) GO TO 199 ! Done

      ! Batch setup
      CALL BAT_SET( ALL_BATCH, NO_BATCH, NEW_HIT, HIT, BATCH )

      ! Obtain other program settings
  101 IF ( DEF_MINIMIZE_RANGE ) THEN
        MINIMIZE_RANGE = .TRUE.
      ELSE
        WRITE( *, '(/A,25X,A/'' >> '',$)' )
     &     ' Use only data points common to all signals?   (Y/N)',
     &     '[N]'
        READ( *, '(A)', IOSTAT=IOS ) RANGE_C
        IF ( IOS .NE. 0 ) THEN
          CALL MSG_WRITE( '*** Operation skipped', REPORT )
          CALL FILE_DEL( HIT )
          GO TO 190
        END IF
        IF ( ANY_CHARS( RANGE_C, 'Yy' ) ) THEN
          MINIMIZE_RANGE = .TRUE.
        ELSE IF ( ANY_CHARS( RANGE_C, 'Nn ' ) ) THEN
          MINIMIZE_RANGE = .FALSE.
        ELSE
          WRITE( *, * ) '*** Invalid option'
          GO TO 101
        END IF
      END IF

      ! Read first UDS file
      CALL REP_COLS( REPORT )
      C.DIMSYS = ' '
      CALL UDS_GET( FILE_NAME, S, C, REPORT, IOS )
      IF ( IOS .NE. 0 ) THEN ! UDS read error
        IF ( HIT.OPEN ) CALL FILE_DEL( HIT )
        GO TO 190
      END IF

      ! Per-pass initializations
      WARN_LOW_NUM_POINTS = .TRUE.

      AVERAGE_VARIANCE = 0.0
      CUMULATIVE_VARIANCE = 0.0

      ! Process first UDS file
      NFP_LIMIT = 0
      NLP_LIMIT = 0

      MIN_NFP = S.NFP
      MAX_NFP = S.NFP
      MIN_NLP = S.NLP
      MAX_NLP = S.NLP

      XUNITS_1 = S.XUNITS
      YUNITS_1 = S.YUNITS
      C.DIMSYS = S.DIMSYS
      NUM_FILES = 1

      DO I = S.NFP, S.NLP
        ANUM_POINTS(I) = ANUM_POINTS(I) + 1
        V_SQ(I) = S.Y(I)**2
        V(I) = S.Y(I)
      END DO

      ! Read/process the other UDS files
      DONE = .FALSE.
      DO WHILE ( .NOT. DONE )

        ! Read another file name/template/hitlist
  110   CALL FILE_PROMPT( TEMPLATE_STR, PROMPT_2, DIR_OUT,
     &     ( ONE_PASS .OR. HIT.OPEN ), HIT, NEW_HIT, FILE_NAME, EOF )

        IF ( EOF ) THEN
          GO TO 190
        ELSE IF ( FILE_NAME .EQ. ' ' ) THEN ! No more files
          DONE = .TRUE.
        ELSE ! Read/process a file

          ! Read next UDS file
          CALL UDS_GET( FILE_NAME, U, C, REPORT, IOS )
          IF ( IOS .NE. 0 ) THEN ! UDS read error
            IF ( ( HIT.OPEN ) .OR. ( ONE_PASS ) ) THEN ! Skip to next
              CALL FILE_DEL( HIT )
              GO TO 190
            ELSE
              GO TO 110
            END IF
          END IF

          ! Check UDS fields for consistency
          IF ( .NOT. EQUAL_SP( U.DEL, S.DEL, 2.E-7 ) ) THEN
            CALL MSG_WRITE( '*** ERROR - Inconsistent DEL step',
     &         REPORT )
            IF ( ( HIT.OPEN ) .OR. ( ONE_PASS ) ) THEN ! Skip to next
              CALL FILE_DEL( HIT )
              GO TO 190
            ELSE
              GO TO 110
            END IF
          END IF
          IF ( U.YUNITS .NE. YUNITS_1 ) CALL MSG_WRITE(
     &       '*** WARNING - Inconsistent Y-units', REPORT )
          IF ( U.XUNITS .NE. XUNITS_1 ) CALL MSG_WRITE(
     &       '*** WARNING - Inconsistent X-units', REPORT )

          ! Calculate new minimum / maximum NFP and NLP values
          MIN_NFP = MIN(MIN_NFP,U.NFP)
          MAX_NFP = MAX(MAX_NFP,U.NFP)
          MIN_NLP = MIN(MIN_NLP,U.NLP)
          MAX_NLP = MAX(MAX_NLP,U.NLP)

          ! Merge UDS fields
          CALL UDS_MERGE( U, S )

          ! Begin adding signals
          DO I = S.NFP, S.NLP

            ANUM_POINTS(I) = ANUM_POINTS(I) + 1
            IF ( WARN_LOW_NUM_POINTS ) THEN
              IF ( ANUM_POINTS(I) .LT. NUM_FILES ) THEN
                WARN_LOW_NUM_POINTS = .FALSE.
                CALL MSG_WRITE( '*** WARNING - '//
     &             'Number of data points '//
     &             'less than number of curves',
     &             REPORT )
              END IF
            END IF

            S.Y(I) = S.Y(I) + U.Y(I)
            V_SQ(I) = V_SQ(I) + U.Y(I)**2
            V(I) = V(I) + U.Y(I)

          END DO

          ! Update for this file
          NUM_FILES = NUM_FILES + 1
        END IF

      END DO

      ! Compute mean and standard deviation
      IF ( MINIMIZE_RANGE ) THEN
        NFP_LIMIT = MAX_NFP
        NLP_LIMIT = MIN_NLP
      ELSE
        NFP_LIMIT = MIN_NFP
        NLP_LIMIT = MAX_NLP
      END IF

      DO I = NFP_LIMIT, NLP_LIMIT
        S.Y(I) = S.Y(I) / ANUM_POINTS(I)
        MN(I) = S.Y(I)
        SD(I) = SQRT(
     &     ( V_SQ(I) - ( 2 * V(I) * MN(I) ) +
     &     ( ANUM_POINTS(I) * ( MN(I)**2 ) ) ) /
     &     ( ANUM_POINTS(I) - 1 ) )
        CUMULATIVE_VARIANCE = CUMULATIVE_VARIANCE + SD(I)**2
      END DO

      AVERAGE_VARIANCE =
     & CUMULATIVE_VARIANCE / ( NLP_LIMIT - NFP_LIMIT + 1 )

      WRITE( MESSAGE, '(10X,A,F9.5)' )
     &  '*** Average Variance = ', AVERAGE_VARIANCE
      CALL MSG_WRITE( MESSAGE, REPORT )

      WRITE( MESSAGE, '(10X,A,F14.5)' )
     &  '*** Cumulative Variance = ', CUMULATIVE_VARIANCE
      CALL MSG_WRITE( MESSAGE, REPORT )

      WRITE( MESSAGE, '(10X,A,I6)' )
     &  '*** Number of Data Points = ', ( NLP_LIMIT - NFP_LIMIT + 1 )
      CALL MSG_WRITE( MESSAGE, REPORT )

      ! Prepare output fields
      S.NFP = NFP_LIMIT
      S.NLP = NLP_LIMIT
      S.CURNAM = 'AVG'
      S.STATUS = 'COMPUTED'
      S.HIC = 0.
      S.T1 = 0.
      S.T2 = 0.
      S.HICDTUP = 0.
      S.CLIP3M = 0.
      S.CSI = 0.
      S.CD2 = 'MEAN SIGNAL(     )'
      WRITE( S.CD2(13:17), '(I5)', IOSTAT=IOS_W ) NUM_FILES

      ! Set default output file names
      CALL FN_DEF( S, OUT_FILE )
      CALL FN_COMBO( S, OUT_FILE )
      CALL ADD_PATH( DIR_OUT, OUT_FILE )
      IF ( .NOT. NO_INCR ) CALL FN_INCR( OUT_FILE )
      CALL FN_NAME_EXTRACT( OUT_FILE )
      DEF_FILE = OUT_FILE
      IE = FT_POSN_S( DEF_FILE, '.uds' )
      IF ( IE .EQ. 0 ) IE = LEN_TRIM( DEF_FILE ) + 1
      OUT_FILE = DEF_FILE(:IE-1)//'.aps'
      CALL ADD_PATH( DIR_OUT, OUT_FILE )
      IF ( .NOT. NO_INCR ) CALL FN_INCR( OUT_FILE )
      CALL FN_NAME_EXTRACT( OUT_FILE )
      MPS_FILE = OUT_FILE
      OUT_FILE = DEF_FILE(:IE-1)//'.ams'
      CALL ADD_PATH( DIR_OUT, OUT_FILE )
      IF ( .NOT. NO_INCR ) CALL FN_INCR( OUT_FILE )
      CALL FN_NAME_EXTRACT( OUT_FILE )
      MMS_FILE = OUT_FILE

      ! Set the output UDS file names
      IF ( .NOT. BATCH ) THEN
  120   WRITE( *, '(/A/3A/'' >> '',$)' )
     &     ' Output UDS mean file name?',
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
  130   WRITE( *, '(/A/3A/'' >> '',$)' )
     &     ' Output UDS mean+sigma file name?',
     &     ' [', MPS_FILE(:L_TRIM(MPS_FILE)), ']'
        READ( *, '(A)', END=190 ) OMPS_FILE
        IF ( OMPS_FILE .EQ. ' ' ) THEN ! Use default file name
          OMPS_FILE = MPS_FILE
        ELSE ! Check file name validity
          CALL ADD_PATH( DIR_OUT, OMPS_FILE )
          IF ( .NOT. FN_VALID( OMPS_FILE ) ) THEN
            WRITE( *, * ) '*** Invalid file name'
            GO TO 130
          END IF
        END IF
  140   WRITE( *, '(/A/3A/'' >> '',$)' )
     &     ' Output UDS mean-sigma file name?',
     &     ' [', MMS_FILE(:L_TRIM(MMS_FILE)), ']'
        READ( *, '(A)', END=190 ) OMMS_FILE
        IF ( OMMS_FILE .EQ. ' ' ) THEN ! Use default file name
          OMMS_FILE = MMS_FILE
        ELSE ! Check file name validity
          CALL ADD_PATH( DIR_OUT, OMMS_FILE )
          IF ( .NOT. FN_VALID( OMMS_FILE ) ) THEN
            WRITE( *, * ) '*** Invalid file name'
            GO TO 140
          END IF
        END IF
      ELSE ! Batch
        OUT_FILE = DEF_FILE
        OMPS_FILE = MPS_FILE
        OMMS_FILE = MMS_FILE
      END IF

      ! Write the output UDS mean file
      CALL UDS_PUT( OUT_FILE, S, C, REPORT, OUTPUT_LIST,
     & DIR_OUT, .TRUE., IOS )
      IF ( IOS .NE. 0 ) GO TO 190

      ! Compute the mean + sigma signal
      DO I = NFP_LIMIT, NLP_LIMIT
        S.Y(I) = MN(I) + SD(I)
      END DO
      S.CURNAM = 'MPS'
      S.CD2 = 'MEAN + SIGMA(     )'
      WRITE( S.CD2(14:18), '(I5)', IOSTAT=IOS_W ) NUM_FILES

      ! Write the output UDS mean + sigma file
      CALL UDS_PUT( OMPS_FILE, S, C, REPORT, OUTPUT_LIST,
     & DIR_OUT, .TRUE., IOS )
      IF ( IOS .NE. 0 ) GO TO 190

      ! Compute the mean - sigma signal
      DO I = NFP_LIMIT, NLP_LIMIT
        S.Y(I) = MN(I) - SD(I)
      END DO
      S.CURNAM = 'MMS'
      S.CD2 = 'MEAN - SIGMA(     )'
      WRITE( S.CD2(14:18), '(I5)', IOSTAT=IOS_W ) NUM_FILES

      ! Write the output UDS mean - sigma file
      CALL UDS_PUT( OMMS_FILE, S, C, REPORT, OUTPUT_LIST,
     & DIR_OUT, .TRUE., IOS )
      IF ( IOS .NE. 0 ) GO TO 190

      WRITE( *, '(/A,I6,A)', IOSTAT=IOS )
     & ' *** ', NUM_FILES, ' files processed'

      ! Loop for next operation
  190 WRITE( *, '(80(''_''))' )
      GO TO 100

      ! Clean up and stop
  199 CALL CLEAN_UP( HIT, REPORT, OUTPUT_LIST, DIR_OUT )

      END



      SUBROUTINE CORRIDOR_CL( CL_ARG, NUM_ARGS, PROG_NAME,
     & DEF_MINIMIZE_RANGE, MINIMIZE_RANGE )

!***********************************************************************
!* Processes the Program-Specific Command Line Arguments
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) CL_ARG(0:*) ! Command line argument array

      INTEGER NUM_ARGS ! Number of arguments

      CHARACTER*(*) PROG_NAME ! Program name

      LOGICAL DEF_MINIMIZE_RANGE !

      LOGICAL MINIMIZE_RANGE ! Indicates to use range of common points


      ! Variables ______________________________________________________

      INTEGER I, NS

      CHARACTER ARG*255, C


      ! Process command line arguments
      DEF_MINIMIZE_RANGE = .FALSE.
      MINIMIZE_RANGE = .FALSE.
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
          WRITE( *, '(10X,A)' ) '[MINIMIZE_RANGE]'
          IF ( ( ARG(NS:) .EQ. '??' ) .OR.
     &       ( ARG(NS:) .EQ. 'HELP' ) ) THEN ! Show command line help
            CALL HELP_CL( PROG_NAME )
            WRITE( *, '(10X,A)' )
     &         'MINIMIZE_RANGE : '//
     &         'Use only those points in common for calculations'
          END IF
          STOP ' '
        ELSE IF ( ARG(NS:) .EQ. 'MINIMIZE_RANGE' ) THEN ! Default
          DEF_MINIMIZE_RANGE = .TRUE.
          MINIMIZE_RANGE = .TRUE.
          CL_ARG(I) = ' '
        END IF
      END DO

      RETURN
      END
