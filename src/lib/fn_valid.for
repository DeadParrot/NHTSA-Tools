      LOGICAL FUNCTION FN_VALID( FILE_NAME )

!***********************************************************************
!* Checks Validity of a File Name
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/09/10
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) FILE_NAME ! File name to check


      ! Variables ______________________________________________________

      LOGICAL FILE_X

      INTEGER IOS, IOC, LUN


      ! Functions ______________________________________________________

      LOGICAL BLANK

      INTEGER GET_LUN

      EXTERNAL BLANK, GET_LUN


      ! Check file name validity
      IF ( BLANK( FILE_NAME ) ) THEN
        FN_VALID = .FALSE.
      ELSE ! Check if such a file exists or can be opened
        INQUIRE( FILE=FILE_NAME, EXIST=FILE_X, IOSTAT=IOS )
        IF ( ( FILE_X ) .AND. ( IOS .EQ. 0 ) ) THEN ! Exists => Valid
          FN_VALID = .TRUE.
        ELSE ! Try to open the file
          LUN = GET_LUN()
          OPEN( UNIT=LUN, FILE=FILE_NAME, STATUS='NEW', IOSTAT=IOS )
          CLOSE( UNIT=LUN, STATUS='DELETE', IOSTAT=IOC )
          IF ( IOS .EQ. 0 ) THEN
            FN_VALID = .TRUE.
          ELSE
            FN_VALID = .FALSE.
          END IF
        END IF
      END IF

      RETURN
      END
