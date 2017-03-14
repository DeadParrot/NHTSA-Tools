      LOGICAL FUNCTION EQUAL_SP( VAL1, VAL2, REL_DIF )

!***********************************************************************
!* Tolerant Equality Between two Single Precision Numbers
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************


      ! Arguments ______________________________________________________

      REAL VAL1 ! First value to compare

      REAL VAL2 ! Second value to compare

      REAL REL_DIF ! Relative difference for comparison


      ! Test for equality with tolerance
      IF ( ABS( VAL1 - VAL2 ) .LE.
     & REL_DIF * MAX( ABS( VAL1 ), ABS( VAL2 ) ) ) THEN
        EQUAL_SP = .TRUE.
      ELSE
        EQUAL_SP = .FALSE.
      END IF

      RETURN
      END



      LOGICAL FUNCTION EQUAL_DP( VAL1, VAL2, REL_DIF )

!***********************************************************************
!* Tolerant Equality Between two Double Precision Numbers
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************


      ! Arguments ______________________________________________________

      DOUBLE PRECISION VAL1 ! First value to compare

      DOUBLE PRECISION VAL2 ! Second value to compare

      DOUBLE PRECISION REL_DIF ! Relative difference for comparison


      ! Test for equality with tolerance
      IF ( ABS( VAL1 - VAL2 ) .LE.
     & REL_DIF * MAX( ABS( VAL1 ), ABS( VAL2 ) ) ) THEN
        EQUAL_DP = .TRUE.
      ELSE
        EQUAL_DP = .FALSE.
      END IF

      RETURN
      END
