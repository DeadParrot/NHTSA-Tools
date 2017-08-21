      SUBROUTINE OPEN_US( LUN, FILE_NAME, ROW, IOS )

!***********************************************************************
!* Opens an Unformatted, Sequential File
!*
!* Language: Fortran
!*
!* Platform: Linux/GCC
!*
!* Compiler: Fortran
!*
!* Author: Stuart G. Mentzer
!*         Andrew Orndorff
!*
!* Date: 1999/09/13
!***********************************************************************


      ! Arguments ______________________________________________________

      INTEGER LUN ! Logical i/o unit

      CHARACTER*(*) FILE_NAME ! File name to open

      CHARACTER ROW ! Read or Write flag

      INTEGER IOS ! Status flag


      ! Functions ______________________________________________________

      INTEGER GET_LUN

      CHARACTER UPCASE

      EXTERNAL GET_LUN, UPCASE


      ! Get a logical i/o unit
      LUN = GET_LUN()
      IF ( LUN .LE. 0 ) THEN ! No unit available
        IOS = -3
        RETURN
      END IF

      ! Open the file
      IF ( UPCASE( ROW ) .EQ. 'R' ) THEN ! Read
        OPEN( UNIT=LUN, FILE=FILE_NAME, STATUS='OLD',
     &   ACCESS='SEQUENTIAL', FORM='UNFORMATTED',
     &   ACTION='READ',
     &   IOSTAT=IOS )
      ELSE IF ( UPCASE( ROW ) .EQ. 'W' ) THEN ! Write
        OPEN( UNIT=LUN, FILE=FILE_NAME, STATUS='UNKNOWN',
     &   ACCESS='SEQUENTIAL', FORM='UNFORMATTED',
     &   IOSTAT=IOS )
      ELSE IF ( UPCASE( ROW ) .EQ. 'S' ) THEN ! Scratch
        OPEN( UNIT=LUN, STATUS='SCRATCH',
     &   ACCESS='SEQUENTIAL', FORM='UNFORMATTED',
     &   IOSTAT=IOS )
      ELSE ! Read or write
        OPEN( UNIT=LUN, FILE=FILE_NAME, STATUS='UNKNOWN',
     &   ACCESS='SEQUENTIAL', FORM='UNFORMATTED',
     &   IOSTAT=IOS )
      END IF

      RETURN
      END