      SUBROUTINE DATEISO( CUR_DATE )

!***********************************************************************
!* Returns the Current Date in the ISO Format: YYYY/MM/DD
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*         Andrew Orndorff
!*
!* Date: 2004/10/25
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) CUR_DATE ! Current date string returned (LEN >= 10)


      ! Variables ______________________________________________________

      CHARACTER DATE_STR*8


      ! Initialize the date string
      CALL DATE_AND_TIME( DATE=DATE_STR )
      CUR_DATE = DATE_STR(1:4)//'/'//DATE_STR(5:6)//'/'//DATE_STR(7:8)

      RETURN
      END
