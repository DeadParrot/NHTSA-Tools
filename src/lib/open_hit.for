      SUBROUTINE OPEN_HIT( DIR_OUT, HIT, IOS )

!***********************************************************************
!* Opens a Hitlist File
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************

      ! Headers
      INCLUDE 'file.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) DIR_OUT ! Output directory

      RECORD /FILE/  HIT ! Hit file object

      INTEGER IOS ! Status flag returned


      ! Variables ______________________________________________________

      CHARACTER HIT_NAME*252


      ! Open unique hitlist if not already open
      IF ( .NOT. HIT.OPEN ) THEN ! Open hitlist
        HIT_NAME = HIT.NAME
        CALL FILE_INIT( HIT )
        IF ( BLANK( HIT_NAME ) ) THEN
          HIT.NAME = 'file_lis.hit'
        ELSE
          HIT.NAME = HIT_NAME
        END IF
        CALL FN_UNIQ( DIR_OUT, HIT.NAME )
        CALL OPEN_FS( HIT.UNIT, HIT.NAME, 'W', IOS )
        IF ( IOS .NE. 0 ) THEN
          WRITE( *, * ) '*** Hit file open failed'
          CALL FILE_DEL( HIT )
          RETURN
        END IF
        HIT.OPEN = .TRUE.
        INQUIRE( HIT.UNIT, NAME=HIT.NAME, IOSTAT=IOS )
      END IF

      RETURN
      END
