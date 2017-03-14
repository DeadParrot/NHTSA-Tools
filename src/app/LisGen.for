      PROGRAM LISGEN

!***********************************************************************
!* LisGen generates lists of related strings using repeating string elements,
!* ASCII character sequences, integer sequences, and, optionally, file name
!* directory searching
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2001/03/04
!***********************************************************************

      ! Headers
      INCLUDE 'platform.fi'
      INCLUDE 'file.fi'
      INCLUDE 'uds_fxn.fi'


      ! Variables ______________________________________________________

      RECORD /FILE/  HIT, REPORT, OUTPUT_LIST, LIST

      LOGICAL ALL_BATCH, NO_BATCH, NO_INCR, ONE_PASS,
     & APPEND, DIR_SEARCH, PATH_EXP,
     & INTERACTIVE, FILE_X, FIRST, DONE, GEN_STRINGS

      INTEGER NUM_ARGS, MAX_ARGS, IOS, IOS_W, N_REC_OLD, IN_UNIT,
     & I, LN

      PARAMETER ( MAX_ARGS = 12 )

      CHARACTER CL_ARG(0:MAX_ARGS)*255, PROG_NAME*8,
     & TEMPLATE_STR*255, DIR_OUT*255, TEMP_C*255


      ! Initializations
      PROG_NAME = 'LisGen'
      CALL FILE_INIT( HIT )
      CALL GET_CL( CL_ARG, NUM_ARGS, MAX_ARGS )
      CALL LISGEN_CL( CL_ARG, NUM_ARGS, PROG_NAME,
     & LIST, APPEND, DIR_SEARCH, PATH_EXP )
      CALL PROC_CL( CL_ARG, NUM_ARGS, PROG_NAME, TEMPLATE_STR,
     & ALL_BATCH, NO_BATCH, NO_INCR, REPORT, OUTPUT_LIST, DIR_OUT,
     & ONE_PASS, .FALSE., .TRUE. )
      IF ( LIST.EXISTS .OR. APPEND ) NO_INCR = .TRUE.
      INTERACTIVE = ( TEMPLATE_STR .EQ. ' ' )

      ! Open list file
      CALL ADD_PATH( DIR_OUT, LIST.NAME )
      IF ( .NOT. NO_INCR ) CALL FN_NINC( LIST.NAME )
      IF ( .NOT. APPEND ) THEN
        CALL OPEN_FS( LIST.UNIT, LIST.NAME, 'W', IOS )
        IF ( IOS .NE. 0 )
     &   CALL OPEN_FS( LIST.UNIT, LIST.NAME, ' ', IOS )
      ELSE
        CALL OPEN_FS( LIST.UNIT, LIST.NAME, ' ', IOS )
      END IF
      IF ( IOS .NE. 0 ) THEN
        WRITE( *, * ) '*** Hitlist file open failed'
        GO TO 199
      END IF
      INQUIRE( LIST.UNIT, NAME=LIST.NAME, IOSTAT=IOS )
      LIST.EXISTS = .TRUE.
      LIST.OPEN = .TRUE.
      LIST.N_REC = 0
      IF ( APPEND ) THEN ! Position at end of file
        IOS = 0
        DO WHILE ( IOS .EQ. 0 )
          READ( LIST.UNIT, '(A)', IOSTAT=IOS )
          IF ( IOS .EQ. 0 ) LIST.N_REC = LIST.N_REC + 1
        END DO
      END IF
      N_REC_OLD = LIST.N_REC

      ! Check/process input hitfile
      IF ( .NOT. ANY_CHARS( TEMPLATE_STR, '()'//WILDCARD_CHARS ) ) THEN
        INQUIRE( FILE=TEMPLATE_STR, EXIST=FILE_X, IOSTAT=IOS )
        IF ( ( FILE_X ) .AND. ( IOS .EQ. 0 ) ) THEN
          CALL OPEN_FS( IN_UNIT, TEMPLATE_STR, 'R', IOS )
          IF ( IOS .EQ. 0 ) THEN ! See if contents look like hitfile
            READ( IN_UNIT, '(A)', IOSTAT=IOS ) TEMP_C
            IF ( IOS .EQ. 0 )
     &       INQUIRE( FILE=TEMP_C, EXIST=FILE_X, IOSTAT=IOS )
            IF ( ( FILE_X ) .AND. ( IOS .EQ. 0 ) ) THEN ! Hitfile
              IF ( PATH_EXP ) INQUIRE( FILE=TEMP_C, NAME=TEMP_C,
     &         IOSTAT=IOS )
              WRITE( LIST.UNIT, '(A)', IOSTAT=IOS_W )
     &           TEMP_C(:L_TRIM(TEMP_C))
              LIST.N_REC = LIST.N_REC + 1
              IOS = 0
              DO WHILE ( IOS .EQ. 0 )
                READ( IN_UNIT, '(A)', IOSTAT=IOS ) TEMP_C
                IF ( IOS .EQ. 0 ) THEN
                  IF ( PATH_EXP )
     &             INQUIRE( FILE=TEMP_C, NAME=TEMP_C, IOSTAT=IOS )
                  LN = LEN_TRIM( TEMP_C )
                  IF ( LN .GT. 0 ) THEN
                    WRITE( LIST.UNIT, '(A)', IOSTAT=IOS_W )
     &               TEMP_C(:LN)
                  ELSE
                    WRITE( LIST.UNIT, '()', IOSTAT=IOS_W )
                  END IF
                  LIST.N_REC = LIST.N_REC + 1
                END IF
              END DO
              CLOSE( IN_UNIT, IOSTAT=IOS )
              TEMPLATE_STR = ' '
              INTERACTIVE = .TRUE.
            END IF
          END IF
        END IF
      END IF

      ! Get string template from user
      FIRST = .TRUE.
      DONE = .FALSE.
      DO WHILE ( .NOT. DONE )

        ! Template list build/display/manage
        GEN_STRINGS = .TRUE.
        IF ( ( INTERACTIVE ) .OR. ( .NOT. FIRST ) ) THEN
          IF ( LIST.N_REC .EQ. 0 ) THEN ! Empty list
            IF ( DIR_SEARCH ) THEN
              WRITE( *, '(/A,48X,A/'' >> '',$)' )
     &         ' File name template?   ', '[no more]'
            ELSE
              WRITE( *, '(/A,58X,A/'' >> '',$)' )
     &         ' Template?   ', '[no more]'
            END IF
          ELSE ! Non-empty list
            IF ( DIR_SEARCH ) THEN
              WRITE( *, '(/2A,8X,A/'' >> '',$)' )
     &         ' Next file name template?   ',
     &         '( S - Show,  U - Undo,  E - Erase )',
     &         '[no more]'
            ELSE
              WRITE( *, '(/2A,18X,A/'' >> '',$)' )
     &         ' Next template?   ',
     &         '( S - Show,  U - Undo,  E - Erase )',
     &         '[no more]'
            END IF
          END IF
          READ( *, '(A)', IOSTAT=IOS ) TEMPLATE_STR
          CALL STR_UPCASE( TEMP_C, TEMPLATE_STR )
          IF ( ( IOS .NE. 0 ) .OR. ( TEMP_C .EQ. ' ' ) ) THEN ! Done
            DONE = .TRUE.
          ELSE IF ( LIST.N_REC .GT. 0 ) THEN ! Non-empty list
            IF ( TEMP_C .EQ. 'S' ) THEN ! Show list
              REWIND( LIST.UNIT )
              DO I = 1, LIST.N_REC
                READ( LIST.UNIT, '(A)', IOSTAT=IOS ) TEMP_C
                IF ( IOS .EQ. 0 ) THEN
                  WRITE( *, *, IOSTAT=IOS_W ) TEMP_C(:L_TRIM(TEMP_C))
                END IF
              END DO
              GEN_STRINGS = .FALSE.
            ELSE IF ( TEMP_C .EQ. 'U' ) THEN ! Undo
              REWIND( LIST.UNIT )
              IF ( N_REC_OLD .GT. 0 ) THEN
                DO I = 1, N_REC_OLD - 1 ! Skip to last old record
                  READ( LIST.UNIT, '(A)', IOSTAT=IOS )
                END DO
                READ( LIST.UNIT, '(A)', IOSTAT=IOS ) TEMP_C
                BACKSPACE( LIST.UNIT )
                LN = LEN_TRIM( TEMP_C )
                IF ( LN .GT. 0 ) THEN
                  WRITE( LIST.UNIT, '(A)', IOSTAT=IOS_W ) TEMP_C(:LN)
                ELSE
                  WRITE( LIST.UNIT, '()', IOSTAT=IOS_W )
                END IF
              ELSE ! Set empty list
                WRITE( LIST.UNIT, '()', IOSTAT=IOS_W )
                BACKSPACE( LIST.UNIT )
              END IF
              LIST.N_REC = N_REC_OLD
              GEN_STRINGS = .FALSE.
            ELSE IF ( TEMP_C .EQ. 'E' ) THEN ! Erase list
              REWIND( LIST.UNIT )
              WRITE( LIST.UNIT, '()', IOSTAT=IOS_W )
              BACKSPACE( LIST.UNIT )
              LIST.N_REC = 0
              N_REC_OLD = 0
              GEN_STRINGS = .FALSE.
            END IF
          END IF
        END IF

        ! Generate the strings
        IF ( ( GEN_STRINGS ) .AND. ( .NOT. DONE ) ) THEN
          N_REC_OLD = LIST.N_REC
          CALL LISGEN_S( LIST, DIR_SEARCH, PATH_EXP, TEMPLATE_STR )
        END IF

        ! Set flags if first pass
        IF ( FIRST ) THEN
          FIRST = .FALSE.
          IF ( ONE_PASS ) DONE = .TRUE.
        END IF

      END DO

      ! Write the output list entry
      CALL OUT_ENTRY( LIST.NAME, OUTPUT_LIST )

      ! Clean up and stop
  199 CALL CLEAN_UP( HIT, REPORT, OUTPUT_LIST, DIR_OUT )
      IF ( LIST.N_REC .EQ. 0 ) THEN
        CALL FILE_DEL( LIST )
      ELSE
        CALL FILE_CLOSE( LIST )
      END IF

      END



      SUBROUTINE LISGEN_CL( CL_ARG, NUM_ARGS, PROG_NAME,
     & LIST, APPEND, DIR_SEARCH, PATH_EXP )

!***********************************************************************
!* Processes the Program-Specific Command Line Arguments
!***********************************************************************

      ! Headers
      INCLUDE 'file.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) CL_ARG(0:*) ! Command line argument array

      INTEGER NUM_ARGS ! Number of arguments

      CHARACTER*(*) PROG_NAME ! Program name

      RECORD /FILE/  LIST ! Hit list file object

      LOGICAL APPEND ! Indicates to append to existing hit list file

      LOGICAL DIR_SEARCH ! Indicates directory search on list strings

      LOGICAL PATH_EXP ! Indicates to use full path file names on list


      ! Variables ______________________________________________________

      INTEGER I, NS

      CHARACTER ARG*255, C


      ! Process command line arguments
      CALL FILE_INIT( LIST )
      LIST.NAME = 'lisgen.hit' ! Default list file name
      APPEND = .FALSE.
      DIR_SEARCH = .TRUE.
      PATH_EXP = .TRUE.
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
     &     '[HIT=<hit_file>] [APPEND] [NO_DIR] [NO_PATH]'
          IF ( ( ARG(NS:) .EQ. '??' ) .OR.
     &     ( ARG(NS:) .EQ. 'HELP' ) ) THEN ! Show command line help
            CALL HELP_CL( PROG_NAME )
            WRITE( *, '(3(10X,A/),10X,A)' )
     &       'HIT=<hit_file> :  Hit file name [lisgen.hit]',
     &       'APPEND :  Append to hitfile if it exists already',
     &       'NO_DIR :  '//
     &       'Don''t perform directory search on list strings',
     &       'NO_PATH :  '//
     &       'Don''t include full paths in list file names'
          END IF
          STOP ' '
        ELSE IF ( ( ARG(NS:NS+3) .EQ. 'HIT=' ) .OR.
     &   ( ARG(NS:NS+3) .EQ. 'HIT#' ) ) THEN
          LIST.EXISTS = .TRUE.
          LIST.NAME = CL_ARG(I)(NS+4:)
          CL_ARG(I) = ' '
        ELSE IF ( ARG(NS:) .EQ. 'APPEND' ) THEN
          APPEND = .TRUE.
          CL_ARG(I) = ' '
        ELSE IF ( ( ARG(NS:) .EQ. 'NO_DIR' ) .OR.
     &   ( ARG(NS:) .EQ. 'NODIR' ) ) THEN
          DIR_SEARCH = .FALSE.
          CL_ARG(I) = ' '
        ELSE IF ( ( ARG(NS:) .EQ. 'NO_PATH' ) .OR.
     &   ( ARG(NS:) .EQ. 'NOPATH' ) ) THEN
          PATH_EXP = .FALSE.
          CL_ARG(I) = ' '
        END IF
      END DO

      RETURN
      END
