      SUBROUTINE OPEN_FS( LUN, FILE_NAME, ROW, IOS )

!***********************************************************************
!* Opens a Formatted, Sequential File
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
!* Date: 1999/09/23
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
     &   ACCESS='SEQUENTIAL', FORM='FORMATTED',
     &   RECL=1024,
     &   ACTION='READ',
     &   IOSTAT=IOS )
      ELSE IF ( UPCASE( ROW ) .EQ. 'W' ) THEN ! Write
        IF ( FILE_NAME .EQ. '/dev/tty' ) THEN
          OPEN( UNIT=LUN, FILE=FILE_NAME, STATUS='OLD',
     &     ACCESS='SEQUENTIAL', FORM='FORMATTED',
     &     IOSTAT=IOS )
        ELSE
          OPEN( UNIT=LUN, FILE=FILE_NAME, STATUS='UNKNOWN',
     &     ACCESS='SEQUENTIAL', FORM='FORMATTED',
     &     RECL=1024,
     &     IOSTAT=IOS )
        END IF
      ELSE IF ( UPCASE( ROW ) .EQ. 'S' ) THEN ! Scratch
        OPEN( UNIT=LUN, STATUS='SCRATCH',
     &   ACCESS='SEQUENTIAL', FORM='FORMATTED',
     &   RECL=1024,
     &   IOSTAT=IOS )
      ELSE ! Read or write
        IF ( FILE_NAME .EQ. '/dev/tty' ) THEN
          OPEN( UNIT=LUN, FILE=FILE_NAME, STATUS='OLD',
     &     ACCESS='SEQUENTIAL', FORM='FORMATTED',
     &     IOSTAT=IOS )
        ELSE
          OPEN( UNIT=LUN, FILE=FILE_NAME, STATUS='UNKNOWN',
     &     ACCESS='SEQUENTIAL', FORM='FORMATTED',
     &     RECL=1024,
     &     IOSTAT=IOS )
        END IF
      END IF

      RETURN
      END
