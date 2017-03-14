      SUBROUTINE FILE_DEL( FILE_OBJ )

!***********************************************************************
!* Deletes a FILE Object
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/09/10
!***********************************************************************

      ! Headers
      INCLUDE 'file.fi'


      ! Arguments ______________________________________________________

      RECORD /FILE/  FILE_OBJ ! File object


      ! Variables ______________________________________________________

      LOGICAL FILE_X, FILE_OPEN

      INTEGER IOS

      CHARACTER ACCESS*10, FORM*11


      ! Functions ______________________________________________________

      INTEGER GET_LUN

      EXTERNAL GET_LUN


      ! Close/delete the file / Reset file flags
      INQUIRE( FILE=FILE_OBJ.NAME, EXIST=FILE_X, OPENED=FILE_OPEN,
     & ACCESS=ACCESS, FORM=FORM, IOSTAT=IOS )
      IF ( ( FILE_X ) .AND. ( IOS .EQ. 0 ) ) THEN
        IF ( .NOT. FILE_OPEN ) THEN
          FILE_OBJ.UNIT = GET_LUN()
          IF ( FILE_OBJ.UNIT .GT. 0 )
     &       OPEN( UNIT=FILE_OBJ.UNIT, FILE=FILE_OBJ.NAME,
     &       ACCESS=ACCESS, FORM=FORM, STATUS='OLD', IOSTAT=IOS )
        END IF
        CLOSE( UNIT=FILE_OBJ.UNIT, STATUS='DELETE', IOSTAT=IOS )
      END IF
      CALL FILE_INIT( FILE_OBJ )

      RETURN
      END
