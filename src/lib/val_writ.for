      SUBROUTINE INT_WRITE( I_VAL, BUFF, IP )

!***********************************************************************
!* Writes a 4-Byte Integer to Buffer and Advances Pointer
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************


      ! Arguments ______________________________________________________

      INTEGER I_VAL ! Value to write

      CHARACTER*(*) BUFF ! Buffer to write to

      INTEGER IP ! Buffer index pointer


      ! Variables ______________________________________________________

      INTEGER I_REP

      CHARACTER C_REP*4

      EQUIVALENCE ( I_REP, C_REP )


      ! Copy the integer to the current position in buffer
      I_REP = I_VAL ! Use integer to avoid invalid non-native fl. pt.
      BUFF(IP:IP+3) = C_REP

      ! Advance buffer pointer
      IP = IP + 4

      RETURN
      END



      SUBROUTINE REAL_WRITE( R_VAL, BUFF, IP )

!***********************************************************************
!* Writes a 4-Byte Real to Buffer and Advances Pointer
!* . Real is aliased as integer to handle non-native fl. pt. values
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************


      ! Arguments ______________________________________________________

      INTEGER R_VAL ! Value to write

      CHARACTER*(*) BUFF ! Buffer to write to

      INTEGER IP ! Buffer index pointer


      ! Variables ______________________________________________________

      INTEGER I_REP

      CHARACTER C_REP*4

      EQUIVALENCE ( I_REP, C_REP )


      ! Copy the integer to the current position in buffer
      I_REP = R_VAL ! Use integer to avoid invalid non-native fl. pt.
      BUFF(IP:IP+3) = C_REP

      ! Advance buffer pointer
      IP = IP + 4

      RETURN
      END



      SUBROUTINE STR_WRITE( S_VAL, BUFF, IP )

!***********************************************************************
!* Writes a String Field to Buffer and Advances Pointer
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) S_VAL ! String to write

      CHARACTER*(*) BUFF ! Buffer to write to

      INTEGER IP ! Buffer index pointer


      ! Variables ______________________________________________________

      INTEGER S_LEN


      ! Copy the string to current position in buffer
      S_LEN = LEN( S_VAL )
      BUFF(IP:IP+S_LEN-1) = S_VAL

      ! Advance buffer pointer
      IP = IP + S_LEN

      RETURN
      END
