      SUBROUTINE UDS_SPEC_COPY( D, S )

!***********************************************************************
!* Copies UDS Specifications
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

      RECORD /UDS_SPEC/  D ! UDS_SPEC object to copy to

      RECORD /UDS_SPEC/  S ! UDS_SPEC object to copy from


      ! Copy
      D = S

      RETURN
      END
