      SUBROUTINE OPEN_PRN( LUN, FILE_NAME, IOS )

!***********************************************************************
!* Opens the Print File/Device
!*
!* Language: Fortran
!*
!* Platform: Linux/IC
!*
!* Compiler: Intel Fortran
!*
!* Author: Stuart G. Mentzer
!*         Andrew Orndorff
!*
!* Date: 2004/10/25
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
      OPEN( UNIT=LUN, FILE=FILE_NAME,
     & ACCESS='SEQUENTIAL', FORM='BINARY',
     & RECORDTYPE='STREAM',
     & BUFFERED='YES',
     & IOSTAT=IOS )

      RETURN
      END
