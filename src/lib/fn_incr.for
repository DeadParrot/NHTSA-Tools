      SUBROUTINE FN_INCR( FILE_NAME )

!***********************************************************************
!* Increments a File Name Version if File Already Exists
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*         Andrew Orndorff
!*
!* Date: 2004/10/25
!*
!* Notes:
!* . Mimics the OpenVMS file version scheme:
!*   . Version numbers follow a ~ (tilde) character suffix
!*   . File versions are numbered from 1 up to a max of 999999
!* . Files with no version suffix are treated as version 0, including:
!*   . Existing ~<nonnumeric> suffixes
!*   . Existing ~ suffixes
!* . Existing ~<number> suffixes are assumed to be file versions
!* . File versions are incremented until an available file name is found
!* . File names are truncated as nec to provide room for the version
!* . If no available file name is found the last name tried is returned
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) FILE_NAME ! File name to increment


      ! Variables ______________________________________________________

      LOGICAL FILE_X

      INTEGER IOS, IV, LFN, PFN, VERSION

      CHARACTER VERSION_STRING*6


      ! Functions ______________________________________________________

      CHARACTER LJUST*6

      EXTERNAL LJUST


      ! See if the file exists already
      CALL FN_TRUNC( FILE_NAME )
      INQUIRE( FILE=FILE_NAME, EXIST=FILE_X, IOSTAT=IOS )
      IF ( ( .NOT. FILE_X ) .OR. ( IOS .NE. 0 ) ) RETURN

      ! Get/set version position and number
      PFN = FN_POSN( FILE_NAME )
      IF ( PFN .EQ. 0 ) RETURN
      IV = FV_POSN( FILE_NAME )
      LFN = LEN( FILE_NAME )
      IF ( LFN .LT. PFN + 2 ) RETURN
      VERSION = 0
      IF ( IV .GT. 0 ) THEN ! Version separator present
        IF ( IV .LT. LFN ) THEN ! Has room for version
          READ( FILE_NAME(IV+1:), '(BN,I6)', IOSTAT=IOS ) VERSION
          IF ( IOS .NE. 0 ) THEN ! Append a new version
            VERSION = 0 ! Protect against i/o bug
            IV = MIN( LEN_TRIM( FILE_NAME ) + 1, LFN - 1 )
            FILE_NAME(IV:) = '~1'
          END IF
        ELSE ! Move separator to make room for version number
          IV = LFN - 1
          FILE_NAME(IV:) = '~1'
        END IF
      ELSE ! Add version separator
        IV = MIN( LEN_TRIM( FILE_NAME ) + 1, LFN - 1 )
        FILE_NAME(IV:) = '~1'
      END IF

      ! Increment version until available name found (or return last)
      IOS = 0
      DO WHILE ( ( FILE_X ) .AND. ( IOS .EQ. 0 ) )
        VERSION = VERSION + 1
        WRITE( VERSION_STRING, '(I6)', IOSTAT=IOS ) VERSION
        IF ( IOS .EQ. 0 ) THEN
          VERSION_STRING = LJUST( VERSION_STRING )
          IF ( IV + LEN_TRIM( VERSION_STRING ) .GT. LFN )
     &     THEN ! Too long
            IV = LFN - LEN_TRIM( VERSION_STRING ) ! New separator posn
            IF ( IV .GT. PFN ) THEN ! Use new position
              FILE_NAME(IV:) = '~'
            ELSE ! No room for version
              RETURN
            END IF
          END IF
          FILE_NAME(IV+1:) = VERSION_STRING
          INQUIRE( FILE=FILE_NAME, EXIST=FILE_X, IOSTAT=IOS )
        END IF
      END DO

      RETURN
      END
