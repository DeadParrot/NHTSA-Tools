      SUBROUTINE FN_LOCAL( FILE_NAME )

!***********************************************************************
!* Translates a File Name to the Operating System Path Format
!* . Supports PC (DOS, Windows, OS/2), Un*x, and OpenVMS Formats
!*
!* Notes:
!* . Device/drive names are removed
!* . Intended for existing files before open/read
!* . DOS-Un*x translation skipped if any platform separators present to support
!*   mixed-separator network/unc path names
!* . OpenVMS-like file names such as [ABC]xyz.ext may be valid on platform:
!*   . FN_LOCAL will translate them if file doesn't exist
!*   . For files to be created call FN_LOCAL only if (.NOT.FN_VALID)
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************

      ! Headers
      INCLUDE 'platform.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) FILE_NAME ! File name to translate


      ! Variables ______________________________________________________

      LOGICAL FILE_X

      INTEGER I, I1, I2, IL, IR, IOS, LN

      CHARACTER SEP, TEMP_NAME*256


      ! Return if file exists
      INQUIRE( FILE=FILE_NAME, EXIST=FILE_X, IOSTAT=IOS )
      IF ( ( FILE_X ) .AND. ( IOS .EQ. 0 ) ) RETURN

      ! Translate file name
      IF ( PLAT_NAME .EQ. 'VMS' ) THEN ! Translate to OpenVMS
        IL = FIRST_CHAR( FILE_NAME, '\/' )
        IF ( IL .GT. 0 ) THEN ! Translate
          ! Find start index (after any drive: spec)
          I1 = IL
          DO WHILE ( ( I1 .GT. 1 ) .AND.
     &       ( FILE_NAME(I1-1:I1-1) .NE. ':' ) )
            I1 = I1 - 1
          END DO
          I2 = IL - 1
          IF ( ( I2 .EQ. I1 + 1 ) .AND.
     &       ( FILE_NAME(I1:I2) .EQ. '..' ) ) THEN ! ..\___
            TEMP_NAME = '[-'
          ELSE IF ( ( I2 .EQ. I1 ) .AND.
     &       ( FILE_NAME(I1:I2) .EQ. '.' ) ) THEN ! .\___
            TEMP_NAME = '['
          ELSE IF ( I1 .EQ. IL ) THEN ! \___
            IF ( ANY_CHARS( FILE_NAME(IL+1:), '\/' ) ) THEN
              IR = IL + FIRST_CHAR( FILE_NAME(IL+1:), '\/' )
              TEMP_NAME = '['//FILE_NAME(IL+1:IR-1)
              IL = IR
            ELSE ! \<file> - Assume root directory
              TEMP_NAME = '[000000]'
            END IF
          ELSE ! <dir>\___
            TEMP_NAME = '[.'//FILE_NAME(I1:I2)
          END IF
          IR = IL
          DO WHILE ( ANY_CHARS( FILE_NAME(IR+1:), '\/' ) )
            IL = IR
            I1 = IL + 1
            IR = IL + FIRST_CHAR( FILE_NAME(IL+1:), '\/' )
            I2 = IR - 1
            LN = LEN_TRIM( TEMP_NAME )
            IF ( ( I2 .EQ. I1 + 1 ) .AND.
     &         ( FILE_NAME(I1:I2) .EQ. '..' ) ) THEN ! \..\___
              TEMP_NAME(LN+1:) = '.-'
            ELSE IF ( ( I2 .EQ. I1 ) .AND.
     &         ( FILE_NAME(I1:I2) .EQ. '.' ) ) THEN ! \.\___
              ! Skip
            ELSE ! <dir>\___
              TEMP_NAME(LN+1:) = '.'//FILE_NAME(I1:I2)
            END IF
          END DO
          LN = LEN_TRIM( TEMP_NAME )
          IF ( TEMP_NAME(LN:LN) .NE. ']' ) THEN
            TEMP_NAME(LN+1:LN+1) = ']'
            LN = LN + 1
          END IF
          TEMP_NAME(LN+1:) = FILE_NAME(IR+1:)
          FILE_NAME = TEMP_NAME
        END IF
      ELSE ! Translate from OpenVMS or between DOS and Un*x
        IL = INDEX( FILE_NAME, '[' )
        IR = IL + INDEX( FILE_NAME(IL+1:), ']' )
        IF ( ( IL .GT. 0 ) .AND. ( IR .GT. IL ) .AND.
     &     ( .NOT. ANY_CHARS( FILE_NAME(IR+1:), '[]' ) ) ) THEN
          ! Translate from OpenVMS
          SEP = PATH_CHARS
          I1 = IL + 1
          IF ( FILE_NAME(I1:I1) .EQ. ']' ) THEN ! [] - Discard
            I2 = IR
            TEMP_NAME = ' '
          ELSE IF ( ( FILE_NAME(I1:I1) .EQ. '-' ) .AND.
     &       ( ANY_CHARS( FILE_NAME(I1+1:I1+1), '.-]' ) ) ) THEN ! [-
            I2 = I1
            TEMP_NAME = '..'
          ELSE IF ( FILE_NAME(I1:I1) .EQ. '.' ) THEN ! [.<dir>
            I2 = I1 + FIRST_CHAR( FILE_NAME(I1+1:), '.]' ) - 1
            TEMP_NAME = FILE_NAME(I1+1:I2)
          ELSE ! [<dir>
            IF ( ( FILE_NAME(I1:I1+5) .EQ. '000000' ) .AND.
     &         ( ANY_CHARS( FILE_NAME(I1+6:I1+6), '.]' ) ) ) THEN
              ! [000000
              I2 = I1 + 5
            ELSE
              I2 = IL - 1
            END IF
            TEMP_NAME = ' '
          END IF
          DO WHILE ( I2 .LT. IR - 1 )
            LN = LEN_TRIM( TEMP_NAME )
            IF ( ( FILE_NAME(I2+1:I2+1) .EQ. '-' ) .AND.
     &         ( ANY_CHARS( FILE_NAME(I2+2:I2+2), '.-]' ) ) ) THEN
              ! -.<dir>
              I1 = I2 + 1
              I2 = I1
              TEMP_NAME(LN+1:) = SEP//'..'
            ELSE IF ( ( FILE_NAME(I2+2:I2+2) .EQ. '-' ) .AND.
     &         ( ANY_CHARS( FILE_NAME(I2+3:I2+3), '.-]' ) ) ) THEN
              ! .-.<dir>
              I1 = I2 + 2
              I2 = I1
              TEMP_NAME(LN+1:) = SEP//'..'
            ELSE ! .<dir>
              I1 = I2 + 2
              I2 = I1 + FIRST_CHAR( FILE_NAME(I1+1:), '.]' ) - 1
              TEMP_NAME(LN+1:) = SEP//FILE_NAME(I1:I2)
            END IF
          END DO
          LN = LEN_TRIM( TEMP_NAME )
          IF ( ( I2 .LT. IR ) .AND. ( ( LN .EQ. 0 ) .OR.
     &       ( TEMP_NAME(LN:LN) .NE. SEP ) ) ) THEN
            TEMP_NAME(LN+1:LN+1) = SEP
            LN = LN + 1
          END IF
          TEMP_NAME(LN+1:) = FILE_NAME(IR+1:)
          FILE_NAME = TEMP_NAME
        ELSE IF ( .NOT. ANY_CHARS( FILE_NAME, DIR_FILE_SEP ) ) THEN
          ! Translate between DOS and Un*x
          IF ( DIR_FILE_SEP .EQ. '\' ) THEN ! Translate Un*x to DOS
            DO I = 1, LEN_TRIM( FILE_NAME )
              IF ( FILE_NAME(I:I) .EQ. '/' ) THEN
                FILE_NAME(I:I) = DIR_FILE_SEP
              END IF
            END DO
          ELSE IF ( DIR_FILE_SEP .EQ. '/' ) THEN ! Translate DOS to Un*x
            DO I = 1, LEN_TRIM( FILE_NAME )
              IF ( FILE_NAME(I:I) .EQ. '\' ) THEN
                FILE_NAME(I:I) = DIR_FILE_SEP
              END IF
            END DO
          END IF
        END IF
      END IF

      ! Truncate file name for platform
      CALL FN_TRUNC( FILE_NAME )

      RETURN
      END
