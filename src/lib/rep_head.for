      SUBROUTINE REP_HEAD( REP_UNIT, PROG_NAME )

!***********************************************************************
!* Writes Standard Header to Report File
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/10/18
!***********************************************************************


      ! Arguments ______________________________________________________

      INTEGER REP_UNIT ! Report file logical i/o unit

      CHARACTER*(*) PROG_NAME ! Program name


      ! Variables ______________________________________________________

      CHARACTER HEADING*20, CUR_DATE*10, CUR_TIME*8


      ! Variables ______________________________________________________

      INTEGER IOS


      ! Get date and time
      CALL DATEISO( CUR_DATE )
      CALL TIMEISO( CUR_TIME )

      ! Write the header
      HEADING = PROG_NAME
      HEADING(10:) = ' Run Report'
      WRITE( REP_UNIT, '(56X,A,33X,A,2X,A/55X,22(''-''))',
     & IOSTAT=IOS )
     & HEADING, CUR_DATE, CUR_TIME

      RETURN
      END
