      SUBROUTINE INT_CONV( INT_NUM, ENDIAN_I, ENDIAN_O )

!***********************************************************************
!* Performs Byte Reordering of an INTEGER Number
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*         Andrew Orndorff
!*
!* Date: 1999/08/20
!***********************************************************************


      ! Arguments ______________________________________________________

      INTEGER INT_NUM ! Integer to convert

      CHARACTER*(*) ENDIAN_I ! Input endian type

      CHARACTER*(*) ENDIAN_O ! Output endian type


      ! Variables ______________________________________________________

      INTEGER ONE

      PARAMETER ( ONE = 1 )


      ! Call array function
      CALL INTA_CONV( INT_NUM, ONE, ENDIAN_I, ENDIAN_O )

      RETURN
      END



      SUBROUTINE INTA_CONV( INT_AR, NAR, ENDIAN_I, ENDIAN_O )

!***********************************************************************
!* Performs Byte Reordering of an INTEGER Array
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*         Andrew Orndorff
!*
!* Date: 1999/08/20
!***********************************************************************

      ! Headers
      INCLUDE 'num_conv.fi' ! Declares INTEGER*1|BYTE BB(4), BDUM


      ! Arguments ______________________________________________________

      INTEGER INT_AR(*) ! Integer array to convert

      INTEGER NAR ! Integer array dimension

      CHARACTER*(*) ENDIAN_I ! Input endian type

      CHARACTER*(*) ENDIAN_O ! Output endian type


      ! Variables ______________________________________________________

      INTEGER I, INT_DUM

      EQUIVALENCE ( BB, INT_DUM )


      IF ( ENDIAN_I .EQ. ENDIAN_O ) THEN ! No conversion required
        RETURN
      ELSE ! Assume that there are only two types of byte ordering
        DO I = 1, NAR
          INT_DUM = INT_AR(I)
          BDUM = BB(1)
          BB(1) = BB(4)
          BB(4) = BDUM
          BDUM = BB(2)
          BB(2) = BB(3)
          BB(3) = BDUM
          INT_AR(I) = INT_DUM
        END DO
      END IF

      RETURN
      END
