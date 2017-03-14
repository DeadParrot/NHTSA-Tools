      SUBROUTINE FILE_LIST( FILE_NAME, DIR_OUT, HIT, NO_MATCH )

!***********************************************************************
!* Gets the File Name/Template/Hitlist
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/18
!***********************************************************************

      ! Headers
      INCLUDE 'platform.fi'
      INCLUDE 'file.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) FILE_NAME ! File name/template/hitlist input

      CHARACTER*(*) DIR_OUT ! Output directory

      RECORD /FILE/  HIT ! Hit file

      LOGICAL NO_MATCH ! Indicates no file matches found


      ! Variables ______________________________________________________

      LOGICAL FILE_X, TEMPLATE, DIR_SEARCH, PATH_EXP

      PARAMETER ( DIR_SEARCH = .TRUE., PATH_EXP = .TRUE. )

      INTEGER IN_UNIT, IOS, IOS_I, N_REC_OLD

      CHARACTER FNT*250


      ! Functions ______________________________________________________

      CHARACTER LJUST*250

      EXTERNAL LJUST


      ! Checks / Initializations
      IF ( BLANK( FILE_NAME ) ) THEN
        FILE_NAME = ' '
        NO_MATCH = .TRUE.
        RETURN
      END IF
      NO_MATCH = .FALSE.

      ! Read input as hitlist, file, or file name template
      IF ( ANY_CHARS( FILE_NAME, '()'//WILDCARD_CHARS ) )
     & THEN ! Template
        TEMPLATE = .TRUE.
      ELSE ! Analyze input string
        TEMPLATE = .FALSE.
        INQUIRE( FILE=FILE_NAME, EXIST=FILE_X, IOSTAT=IOS )

        IF ( ( FILE_X ) .AND. ( IOS .EQ. 0 ) ) THEN ! Open the file
          CALL OPEN_FS( IN_UNIT, FILE_NAME, 'R', IOS )
          IF ( IOS .NE. 0 ) THEN ! File open failed - Maybe a directory
            FILE_X = .FALSE.
            CLOSE( UNIT=IN_UNIT, IOSTAT=IOS )
          END IF
        END IF

        IF ( FILE_X ) THEN ! Hitlist or single file

          ! Test file as hitlist
          READ( IN_UNIT, '(A)', IOSTAT=IOS ) FNT
          IF ( IOS .EQ. 0 ) THEN
            FNT = LJUST( FNT )
            INQUIRE( FILE=FNT(:L_TRIM(FNT)), EXIST=FILE_X,
     &         NAME=FNT, IOSTAT=IOS )
          END IF
          IF ( ( FILE_X ) .AND. ( IOS .EQ. 0 ) ) THEN ! Hitlist
            IF ( .NOT. HIT.OPEN ) THEN ! Open a new hitlist
              CALL OPEN_HIT( DIR_OUT, HIT, IOS )
              IF ( IOS .NE. 0 ) THEN
                FILE_NAME = ' '
                NO_MATCH = .TRUE.
                RETURN
              END IF
            END IF
            WRITE( HIT.UNIT, '(A)', IOSTAT=IOS )
     &         FNT(:L_TRIM(FNT))
            HIT.N_REC = HIT.N_REC + 1
            IOS = 0
            DO WHILE ( IOS .EQ. 0 )
              READ( IN_UNIT, '(A)', IOSTAT=IOS ) FNT
              IF ( IOS .EQ. 0 ) THEN
                FNT = LJUST( FNT )
                INQUIRE( FILE=FNT(:L_TRIM(FNT)), EXIST=FILE_X,
     &             NAME=FNT, IOSTAT=IOS_I )
                IF ( ( FILE_X ) .AND. ( IOS_I .EQ. 0 ) ) THEN ! Add
                  WRITE( HIT.UNIT, '(A)', IOSTAT=IOS )
     &               FNT(:L_TRIM(FNT))
                  HIT.N_REC = HIT.N_REC + 1
                END IF
              END IF
            END DO
          ELSE ! Assume single file
            INQUIRE( IN_UNIT, NAME=FILE_NAME, IOSTAT=IOS )
            IF ( HIT.OPEN ) THEN ! Add to list
              WRITE( HIT.UNIT, '(A)', IOSTAT=IOS )
     &           FILE_NAME(:L_TRIM(FILE_NAME))
              HIT.N_REC = HIT.N_REC + 1
            END IF
          END IF
          CLOSE( UNIT=IN_UNIT, IOSTAT=IOS )

        ELSE ! Consider implicit .* suffix

          IF ( ( FN_POSN( FILE_NAME ) .GT. 0 ) .AND.
     &       ( FE_POSN( FILE_NAME ) .EQ. 0 ) .AND.
     &       ( FT_POSN( FILE_NAME ) .EQ. 0 ) ) THEN
            ! Try as template with implicit .* suffix
            TEMPLATE = .TRUE.
          ELSE ! Has period or ends in path char - Don't add .*
            WRITE( *, * ) '*** No matching files found'
            FILE_NAME = ' '
            NO_MATCH = .TRUE.
          END IF

        END IF

      END IF

      IF ( TEMPLATE ) THEN ! Generate file list from template

        ! Open unique hitlist if not already open
        IF ( .NOT. HIT.OPEN ) THEN ! Open hitlist
          CALL OPEN_HIT( DIR_OUT, HIT, IOS )
          IF ( IOS .NE. 0 ) THEN
            FILE_NAME = ' '
            NO_MATCH = .TRUE.
            RETURN
          END IF
        END IF

        ! Expand template(s)
        N_REC_OLD = HIT.N_REC
        CALL LISGEN_S( HIT, DIR_SEARCH, PATH_EXP, FILE_NAME )
        IF ( HIT.N_REC .LE. N_REC_OLD ) NO_MATCH = .TRUE.
        IF ( HIT.N_REC .EQ. 0 ) CALL FILE_DEL( HIT )

      END IF

      RETURN
      END
