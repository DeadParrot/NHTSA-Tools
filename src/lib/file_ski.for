      SUBROUTINE FILE_SKIP( FILE_OBJ )

!***********************************************************************
!* Skips Forward a Record in a File
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

      RECORD /FILE/  FILE_OBJ


      ! Variables ______________________________________________________

      INTEGER IOS


      ! Skip forward by one record
      IF ( FILE_OBJ.OPEN ) READ( FILE_OBJ.UNIT, '(A)', IOSTAT=IOS )

      RETURN
      END
