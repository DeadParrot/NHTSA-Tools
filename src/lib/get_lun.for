      INTEGER FUNCTION GET_LUN()

!***********************************************************************
!* Gets an Available Logical Unit for I/O
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************

      ! Variables ______________________________________________________

      LOGICAL LUN_EXISTS, LUN_OPEN

      INTEGER IOS, LUN


      ! Find an available logical unit
      LUN_EXISTS = .TRUE.
      LUN_OPEN = .TRUE.
      IOS = 0
      LUN = 6
      DO WHILE ( ( LUN_EXISTS ) .AND. ( LUN_OPEN ) .AND.
     & ( IOS .EQ. 0 ) ) ! Try next logical unit
        LUN = LUN + 1
        INQUIRE( UNIT=LUN, EXIST=LUN_EXISTS, OPENED=LUN_OPEN,
     &     IOSTAT=IOS )
      END DO
      IF ( ( LUN_EXISTS ) .AND. ( .NOT. LUN_OPEN ) .AND.
     & ( IOS .EQ. 0 ) ) THEN ! Found available logical unit
        GET_LUN = LUN
      ELSE ! No logical unit available
        GET_LUN = 0
      END IF

      RETURN
      END
