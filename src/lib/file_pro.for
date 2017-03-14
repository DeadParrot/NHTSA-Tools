      SUBROUTINE FILE_PROMPT( TEMPLATE_STR, PROMPT_STR, DIR_OUT,
     & NO_PROMPT, HIT, NEW_HIT, FILE_NAME, EOF )

!***********************************************************************
!* Prompt/Process File Name/Template/Hitlist
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/10/18
!***********************************************************************

      ! Headers
      INCLUDE 'file.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) TEMPLATE_STR ! Input file name or template
      ! Ignored if HIT.OPEN / Blanked after usage

      CHARACTER*(*) PROMPT_STR ! Initial prompt string

      CHARACTER*(*) DIR_OUT ! Output directory

      LOGICAL NO_PROMPT ! Specifies not to prompt for inputs

      RECORD /FILE/  HIT ! Hit file

      LOGICAL NEW_HIT ! Set to indicate new hit file entry or list

      CHARACTER*(*) FILE_NAME ! File name returned from top of list

      LOGICAL EOF ! Indicates user-entered EOF (Ctrl-Z)


      ! Variables ______________________________________________________

      LOGICAL PROMPT, DONE, NO_MATCH

      INTEGER IOS, IOS_W, I, LN, N_REC_OLD


      ! Functions ______________________________________________________

      CHARACTER UPCASE*50

      EXTERNAL UPCASE


      ! Initializations
      NEW_HIT = .FALSE.
      EOF = .FALSE.
      PROMPT = ( .NOT. NO_PROMPT )

      ! Check/read existing hitlist
      IF ( HIT.OPEN ) THEN ! Read from existing hitlist
        READ( HIT.UNIT, '(A)', IOSTAT=IOS ) FILE_NAME
        IF ( IOS .EQ. 0 ) THEN ! Return with the file name
          HIT.N_REC = HIT.N_REC - 1
          INQUIRE( FILE=FILE_NAME, NAME=FILE_NAME, IOSTAT=IOS )
          RETURN
        ELSE ! Hitlist file end or error
          CALL FILE_DEL( HIT )
        END IF
      ELSE ! Initialize HIT file structure
        CALL FILE_INIT( HIT )
      END IF

      ! Check/process input template / Prompt for template
  101 IF ( BLANK( TEMPLATE_STR ) ) THEN ! No input template
        IF ( .NOT. PROMPT ) THEN ! Return blank file name
          FILE_NAME = ' '
          RETURN
        END IF
        ! Prompt for file name/template/hitlist
        WRITE( *, '(/1X,A/'' >> '',$)' )
     &     PROMPT_STR(:L_TRIM(PROMPT_STR))
        READ( *, '(A)', END=199 ) FILE_NAME
        IF ( BLANK( FILE_NAME ) ) RETURN
      ELSE ! Use input template
        IF ( FILE_NAME .NE. TEMPLATE_STR ) PROMPT = .FALSE.
        FILE_NAME = TEMPLATE_STR
        TEMPLATE_STR = ' '
      END IF

      ! Check/process template or hitlist
      N_REC_OLD = 0
      CALL FILE_LIST( FILE_NAME, DIR_OUT, HIT, NO_MATCH )
      IF ( NO_MATCH ) THEN ! No matching file - Loop
        GO TO 101
      ELSE IF ( ( HIT.OPEN ) .AND. ( PROMPT ) ) THEN
        DONE = .FALSE.
        DO WHILE ( .NOT. DONE )
          WRITE( *, '(/2A,2X,A/'' >> '',$)' )
     &       ' Next file name/template/hitlist?   ',
     &       '( S - Show, U - Undo, E - Erase )',
     &       '[no more]'
          READ( *, '(A)', IOSTAT=IOS ) FILE_NAME
          IF ( IOS .EQ. -1 ) THEN ! Ctrl-Z entered - Start over
            CALL FILE_DEL( HIT )
            GO TO 101
          ELSE IF ( ( IOS .NE. 0 ) .OR. ( BLANK( FILE_NAME ) ) )
     &       THEN ! Done
            DONE = .TRUE.
          ELSE IF ( UPCASE( FILE_NAME ) .EQ. 'S' ) THEN ! Show list
            REWIND( HIT.UNIT )
            DO I = 1, HIT.N_REC
              READ( HIT.UNIT, '(A)', IOSTAT=IOS ) FILE_NAME
              IF ( IOS .EQ. 0 ) THEN
                WRITE( *, *, IOSTAT=IOS_W )
     &             FILE_NAME(:L_TRIM(FILE_NAME))
              END IF
            END DO
          ELSE IF ( UPCASE( FILE_NAME ) .EQ. 'U' ) THEN ! Undo last
            REWIND( HIT.UNIT )
            IF ( N_REC_OLD .GT. 0 ) THEN
              DO I = 1, N_REC_OLD - 1 ! Skip to last old record
                READ( HIT.UNIT, '(A)', IOSTAT=IOS )
              END DO
              READ( HIT.UNIT, '(A)', IOSTAT=IOS ) FILE_NAME
              IF ( IOS .NE. 0 ) THEN ! List corrupted
                WRITE( *, * ) '*** List corrupted - Deleting'
                CALL FILE_DEL( HIT )
                GO TO 101
              END IF
              BACKSPACE( HIT.UNIT )
              LN = LEN_TRIM( FILE_NAME )
              IF ( LN .GT. 0 ) THEN
                WRITE( HIT.UNIT, '(A)', IOSTAT=IOS_W )
     &             FILE_NAME(:LN)
              ELSE
                WRITE( HIT.UNIT, '()', IOSTAT=IOS_W )
              END IF
              HIT.N_REC = N_REC_OLD
            ELSE ! Delete list file
              CALL FILE_DEL( HIT )
              GO TO 101
            END IF
          ELSE IF ( UPCASE( FILE_NAME ) .EQ. 'E' ) THEN ! Erase list
            CALL FILE_DEL( HIT )
            GO TO 101
          ELSE ! Process new input
            N_REC_OLD = HIT.N_REC
            CALL FILE_LIST( FILE_NAME, DIR_OUT, HIT, NO_MATCH )
          END IF
        END DO
      END IF

      ! Set up to return a file name
      IF ( HIT.OPEN ) THEN ! Read from new hitlist
        REWIND( HIT.UNIT )
        READ( HIT.UNIT, '(A)', IOSTAT=IOS ) FILE_NAME
        IF ( IOS .NE. 0 ) THEN ! Hitlist end or error
          CALL FILE_DEL( HIT )
          FILE_NAME = ' '
          RETURN
        END IF
        HIT.N_REC = HIT.N_REC - 1
        IF ( HIT.N_REC .LE. 0 ) CALL FILE_DEL( HIT ) ! Return 1 file
      END IF
      INQUIRE( FILE=FILE_NAME, NAME=FILE_NAME, IOSTAT=IOS )
      NEW_HIT = .TRUE.

      RETURN

      ! Ctrl-Z handler
  199 CALL FILE_DEL( HIT )
      FILE_NAME = ' '
      EOF = .TRUE.
      RETURN

      END
