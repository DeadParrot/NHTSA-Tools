      SUBROUTINE FN_NINC( FILE_NAME )

!***********************************************************************
!* Increments a File Name Version if File Already Exists
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*         Andrew Orndorff
!*
!* Date: 2004/10/25
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) FILE_NAME ! File name to increment


      ! Increment the file name
      CALL FN_INCR( FILE_NAME )

      RETURN
      END
