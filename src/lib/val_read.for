      SUBROUTINE INT_READ( I_VAL, BUFF, IP )

!***********************************************************************
!* Reads a 4-Byte Integer from Buffer and Advances Pointer
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************

      INTEGER I_VAL, IP, I_REP

      CHARACTER BUFF*(*), C_REP*4

      EQUIVALENCE ( I_REP, C_REP )


      ! Load the integer from current position in buffer
      C_REP = BUFF(IP:IP+3)
      I_VAL = I_REP ! Use integer to avoid invalid non-native fl. pt.

      ! Advance buffer pointer
      IP = IP + 4

      RETURN
      END



      SUBROUTINE REAL_READ( R_VAL, BUFF, IP )

!***********************************************************************
!* Reads a 4-Byte Real from Buffer and Advances Pointer
!* . Real is aliased as integer to handle non-native fl. pt. values
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************

      INTEGER R_VAL, IP, I_REP

      CHARACTER BUFF*(*), C_REP*4

      EQUIVALENCE ( I_REP, C_REP )


      ! Load the integer from current position in buffer
      C_REP = BUFF(IP:IP+3)
      R_VAL = I_REP ! Use integer to avoid invalid non-native fl. pt.

      ! Advance buffer pointer
      IP = IP + 4

      RETURN
      END



      SUBROUTINE STR_READ( S_VAL, BUFF, IP )

!***********************************************************************
!* Reads a String Field from Buffer and Advances Pointer
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************

      INTEGER IP, S_LEN

      CHARACTER S_VAL*(*), BUFF*(*)


      ! Load the string from current position in buffer
      S_LEN = LEN( S_VAL )
      S_VAL = BUFF(IP:IP+S_LEN-1)

      ! Advance buffer pointer
      IP = IP + S_LEN

      RETURN
      END
