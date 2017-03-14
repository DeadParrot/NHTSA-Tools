      SUBROUTINE CLOSE_FILE( FILE_UNIT )

!***********************************************************************
!* Closes a File, Deleting it if Empty
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/09/10
!***********************************************************************

      ! Headers
      INCLUDE 'platform.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      INTEGER FILE_UNIT ! Logical i/o unit


      ! Variables ______________________________________________________

      LOGICAL FILE_OPEN

      INTEGER IOS

      CHARACTER FILE_NAME*252, ACCESS*10, FORM*11, SCREEN_NAME*252, B


      ! Check unit status
      INQUIRE( FILE_UNIT, OPENED=FILE_OPEN, NAME=FILE_NAME,
     & ACCESS=ACCESS, FORM=FORM, IOSTAT=IOS )
      IF ( .NOT. FILE_OPEN ) RETURN ! No file open on FILE_UNIT

      ! Check for screen device
      INQUIRE( FILE=SCREEN_DEV, NAME=SCREEN_NAME, IOSTAT=IOS )
      IF ( FN_EQUAL( FILE_NAME, SCREEN_NAME ) ) THEN ! Close screen dev
        CLOSE( UNIT=FILE_UNIT, IOSTAT=IOS )
        RETURN
      END IF

      ! Check if file is empty
      IF ( ACCESS .EQ. 'SEQUENTIAL' ) THEN
        REWIND( FILE_UNIT )
        IF ( FORM .EQ. 'FORMATTED' ) THEN
          READ( FILE_UNIT, FMT='(A1)', IOSTAT=IOS ) B
        ELSE IF ( FORM .EQ. 'UNFORMATTED' ) THEN
          READ( FILE_UNIT, IOSTAT=IOS ) B
        ELSE
          IOS = 0
        END IF
      ELSE IF ( ACCESS .EQ. 'DIRECT' ) THEN
        IF ( FORM .EQ. 'FORMATTED' ) THEN
          READ( FILE_UNIT, REC=1, FMT='(A1)', IOSTAT=IOS ) B
        ELSE IF ( FORM .EQ. 'UNFORMATTED' ) THEN
          READ( FILE_UNIT, REC=1, IOSTAT=IOS ) B
        ELSE
          IOS = 0
        END IF
      ELSE
        IOS = 0
      END IF

      ! Close the file
      IF ( IOS .EQ. -1 ) THEN ! File is empty - Delete
        CLOSE( UNIT=FILE_UNIT, STATUS='DELETE', IOSTAT=IOS )
      ELSE
        CLOSE( UNIT=FILE_UNIT, IOSTAT=IOS )
      END IF

      RETURN
      END
