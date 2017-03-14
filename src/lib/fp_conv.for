      SUBROUTINE FPN_CONV( FP_NUM, NUM_I, NUM_O )

!***********************************************************************
!* Converts Floating Point Number Format of a REAL Number
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2001/03/03
!***********************************************************************


      ! Arguments ______________________________________________________

      INTEGER FP_NUM ! Floating point value (aliased as an integer)
      ! Alias as an integer to avoid invalid real assignment errors

      CHARACTER*(*) NUM_I ! Input number format

      CHARACTER*(*) NUM_O ! Output number format


      ! Variables ______________________________________________________

      INTEGER ONE

      PARAMETER ( ONE = 1 )


      ! Call array function
      CALL FPA_CONV( FP_NUM, ONE, NUM_I, NUM_O )

      RETURN
      END



      SUBROUTINE FPA_CONV( FP_AR, NAR, NUM_I, NUM_O )

!***********************************************************************
!* Converts Floating Point Number Format of a REAL Array
!*
!* Notes:
!* . IEEE/SUN infinities, NaNs, and large #'s map to max mag VAX #
!* . IEEE/SUN 0.0, -0.0, and denormals are mapped to VAX zero
!* . VAX zero (Exp=Sign=0) may have nonzero mantissa
!* . VAX resv. op. faults (Exp=0, Sign=1) map to zero on IEEE/SUN
!* . VAX small #'s (Exp=1) map to IEEE/SUN zero, not to denormals
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2001/03/03
!***********************************************************************

      ! Headers
      INCLUDE 'num_conv.fi' ! Declares INTEGER*1|BYTE BB(4), BDUM


      ! Arguments ______________________________________________________

      INTEGER FP_AR(*) ! Floating point array (aliased as integer)
      ! Alias as integer to avoid invalid real assignment errors

      INTEGER NAR ! Floating point array dimension

      CHARACTER*(*) NUM_I ! Input number format

      CHARACTER*(*) NUM_O ! Output number format


      ! Variables ______________________________________________________

      INTEGER*1, PARAMETER :: ONE = 1
      INTEGER I

      INTEGER FP_DUM ! Aliased floating point value

      EQUIVALENCE ( BB, FP_DUM )


      IF ( NUM_I .EQ. NUM_O ) THEN ! No conversion required
        RETURN
      ELSE IF ( ( NUM_I .EQ. 'VAX' ) .AND. ( NUM_O .EQ. 'IEEE' ) ) THEN
        DO I = 1, NAR
          FP_DUM = FP_AR(I)
          IF ( ( BB(2) .EQ. 0 ) .OR. ( BB(2) .EQ. -128 ) ) THEN
            ! Exp=0 or 1 -> Zero on IEEE
            BB(1) = 0
            BB(2) = 0
            BB(3) = 0
            BB(4) = 0
          ELSE
            BDUM = BB(1)
            BB(1) = BB(3)
            BB(3) = BDUM
            BDUM = BB(2)
            BB(2) = BB(4)
            BB(4) = BDUM - ONE
          END IF
          FP_AR(I) = FP_DUM
        END DO
      ELSE IF ( ( NUM_I .EQ. 'IEEE' ) .AND. ( NUM_O .EQ. 'VAX' ) ) THEN
        DO I = 1, NAR
          FP_DUM = FP_AR(I)
          IF ( ( ( BB(4) .EQ. 0 ) .OR. ( BB(4) .EQ. -128 ) ) .AND.
     &       ( BB(3) .GE. 0 ) ) THEN
            ! Exp=0 => Zero or denormal -> Zero on VAX
            BB(1) = 0
            BB(2) = 0
            BB(3) = 0
            BB(4) = 0
          ELSE IF ( ( BB(4) .EQ. 127 ) .OR. ( BB(4) .EQ. -1 ) ) THEN
            ! Exp>=254 -> Max mag on VAX
            BB(1) = -ONE
            BB(2) = BB(4)
            BB(3) = -ONE
            BB(4) = -ONE
          ELSE
            BDUM = BB(1)
            BB(1) = BB(3)
            BB(3) = BDUM
            BDUM = BB(4)
            BB(4) = BB(2)
            BB(2) = BDUM + ONE
          END IF
          FP_AR(I) = FP_DUM
        END DO
      ELSE IF ( ( NUM_I .EQ. 'VAX' ) .AND. ( NUM_O .EQ. 'SUN' ) ) THEN
        DO I = 1, NAR
          FP_DUM = FP_AR(I)
          IF ( ( BB(2) .EQ. 0 ) .OR. ( BB(2) .EQ. -128 ) ) THEN
            ! Exp=0 or 1 -> Zero on SUN
            BB(1) = 0
            BB(2) = 0
            BB(3) = 0
            BB(4) = 0
          ELSE
            BDUM = BB(1)
            BB(1) = BB(2) - ONE
            BB(2) = BDUM
            BDUM = BB(4)
            BB(4) = BB(3)
            BB(3) = BDUM
          END IF
          FP_AR(I) = FP_DUM
        END DO
      ELSE IF ( ( NUM_I .EQ. 'SUN' ) .AND. ( NUM_O .EQ. 'VAX' ) ) THEN
        DO I = 1, NAR
          FP_DUM = FP_AR(I)
          IF ( ( ( BB(1) .EQ. 0 ) .OR. ( BB(1) .EQ. -128 ) ) .AND.
     &       ( BB(2) .GE. 0 ) ) THEN
            ! Exp=0 => Zero or denormal -> Zero on VAX
            BB(1) = 0
            BB(2) = 0
            BB(3) = 0
            BB(4) = 0
          ELSE IF ( ( BB(1) .EQ. 127 ) .OR. ( BB(1) .EQ. -1 ) ) THEN
            ! Exp>=254 -> Max mag on VAX
            BB(1) = -ONE
            BB(2) = BB(1)
            BB(3) = -ONE
            BB(4) = -ONE
          ELSE
            BDUM = BB(2)
            BB(2) = BB(1) + ONE
            BB(1) = BDUM
            BDUM = BB(4)
            BB(4) = BB(3)
            BB(3) = BDUM
          END IF
          FP_AR(I) = FP_DUM
        END DO
      ELSE IF ( ( ( NUM_I .EQ. 'IEEE' ) .AND. ( NUM_O .EQ. 'SUN' ) )
     & .OR. ( ( NUM_I .EQ. 'SUN' ) .AND. ( NUM_O .EQ. 'IEEE' ) ) ) THEN
        DO I = 1, NAR
          FP_DUM = FP_AR(I)
          BDUM = BB(1)
          BB(1) = BB(4)
          BB(4) = BDUM
          BDUM = BB(2)
          BB(2) = BB(3)
          BB(3) = BDUM
          FP_AR(I) = FP_DUM
        END DO
      END IF

      RETURN
      END
