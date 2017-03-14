      SUBROUTINE UDS_COPY( D, S )

!***********************************************************************
!* Copies UDS Objects
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'


      ! Arguments ______________________________________________________

      RECORD /UDS/  D ! UDS object to copy to

      RECORD /UDS/  S ! UDS object to copy from


      ! Variables ______________________________________________________

      INTEGER I


      ! Copy specs
      CALL UDS_SPEC_COPY( D, S )

      ! Copy data
      DO I = S.NFP, S.NLP
        D.X(I) = S.X(I)
        D.Y(I) = S.Y(I)
      END DO

      RETURN
      END
