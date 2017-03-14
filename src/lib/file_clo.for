      SUBROUTINE FILE_CLOSE( FILE_OBJ )

!***********************************************************************
!* Closes a FILE Object, Deleting if Empty
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************

      ! Headers
      INCLUDE 'file.fi'


      ! Arguments ______________________________________________________

      RECORD /FILE/  FILE_OBJ ! File object


      ! Variables ______________________________________________________

      LOGICAL FILE_X

      INTEGER IOS


      ! Close/delete the file / Reset file flags
      IF ( FILE_OBJ.OPEN ) THEN
        CALL CLOSE_FILE( FILE_OBJ.UNIT )
        INQUIRE( FILE=FILE_OBJ.NAME, EXIST=FILE_X, IOSTAT=IOS )
        IF ( ( FILE_X ) .AND. ( IOS .EQ. 0 ) ) THEN ! File still exists
          FILE_OBJ.UNIT = 0
          FILE_OBJ.OPEN = .FALSE.
        ELSE ! File was empty => deleted
          CALL FILE_INIT( FILE_OBJ )
        END IF
      END IF

      RETURN
      END
