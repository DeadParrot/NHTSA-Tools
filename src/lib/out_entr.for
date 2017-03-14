      SUBROUTINE OUT_ENTRY( OUT_FILE, OUTPUT_LIST )

!***********************************************************************
!* Writes Standard Entry to Output List File
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************

      ! Headers
      INCLUDE 'file.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) OUT_FILE ! Output file name entry for output list

      RECORD /FILE/  OUTPUT_LIST ! Output list file object


      ! Variables ______________________________________________________

      INTEGER IOS


      ! Write output list entry
      IF ( OUTPUT_LIST.OPEN )
     & WRITE( OUTPUT_LIST.UNIT, '(A)', IOSTAT=IOS )
     & OUT_FILE(:L_TRIM(OUT_FILE))

      RETURN
      END
