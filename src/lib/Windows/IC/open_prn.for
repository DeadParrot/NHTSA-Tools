      SUBROUTINE OPEN_PRN( LUN, FILE_NAME, IOS )

!***********************************************************************
!* Opens the Print File/Device
!*
!* Language: Fortran
!*
!* Platform: Windows/IC
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

      LOGICAL BLANK

      INTEGER GET_LUN

      EXTERNAL BLANK, GET_LUN


      ! Get a logical i/o unit
      LUN = GET_LUN()
      IF ( LUN .LE. 0 ) THEN ! No unit available
        IOS = -3
        RETURN
      END IF

      ! Open the printer device
      IF ( BLANK( FILE_NAME ) ) THEN ! Use default printer port
        OPEN( UNIT=LUN, FILE='PRN',
     &   ACCESS='SEQUENTIAL', FORM='BINARY',
     &   RECORDTYPE='STREAM',
     &   BUFFERED='YES',
     &   IOSTAT=IOS )
      ELSE ! Use specified file
        OPEN( UNIT=LUN, FILE=FILE_NAME,
     &   ACCESS='SEQUENTIAL', FORM='BINARY',
     &   RECORDTYPE='STREAM',
     &   BUFFERED='YES',
     &   IOSTAT=IOS )
      END IF

      RETURN
      END
