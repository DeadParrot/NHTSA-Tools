      SUBROUTINE OPEN_PRN( LUN, FILE_NAME, IOS )

!***********************************************************************
!* Opens the Print File/Device
!*
!* Language: Fortran
!*
!* Platform: Windows/GCC
!*
!* Compiler: GFortran
!*
!* Author: Stuart G. Mentzer
!*         Andrew Orndorff
!*
!* Date: 2017/02/08
!***********************************************************************


      ! Arguments ______________________________________________________

      INTEGER LUN ! Logical i/o unit

      CHARACTER*(*) FILE_NAME ! File name to open

      INTEGER IOS ! Status flag


      ! Functions ______________________________________________________

      INTEGER GET_LUN

      EXTERNAL GET_LUN


      ! Get a logical i/o unit
      LUN = GET_LUN()
      IF ( LUN .LE. 0 ) THEN ! No unit available
        IOS = -3
        RETURN
      END IF

      ! Open the file
      OPEN( UNIT=LUN, FILE=FILE_NAME, STATUS='UNKNOWN',
     & ACCESS='STREAM', FORM='UNFORMATTED',
     & IOSTAT=IOS )

      RETURN
      END
