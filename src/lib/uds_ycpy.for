      SUBROUTINE UDS_Y_COPY( D, S )

!***********************************************************************
!* Copies UDS_Y Objects
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

      RECORD /UDS_Y/  D ! UDS_Y object to copy to

      RECORD /UDS_Y/  S ! UDS_Y object to copy from


      ! Variables ______________________________________________________

      INTEGER I


      ! Copy specs
      CALL UDS_SPEC_COPY( D, S )

      ! Copy data
      DO I = S.NFP, S.NLP
        D.Y(I) = S.Y(I)
      END DO

      RETURN
      END
