      SUBROUTINE TIMEISO( CUR_TIME )

!***********************************************************************
!* Returns the Current Time in the ISO Format hh:mm:ss
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2017/02/08
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) CUR_TIME ! Current time string returned (LEN >= 8)


      ! Variables ______________________________________________________

      CHARACTER F_TIME*10


      ! Get the full time string
      CALL DATE_AND_TIME( F_TIME )

      ! Set the time
      CUR_TIME = F_TIME(1:2)//':'//F_TIME(3:4)//':'//F_TIME(5:6)

      RETURN
      END
