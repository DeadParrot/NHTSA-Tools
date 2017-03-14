      SUBROUTINE FN_EXTRACT( FILE_NAME )

!***********************************************************************
!* Removes the Path and Version Portion of a File Name
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) FILE_NAME ! File name to extract


      ! Variables ______________________________________________________

      INTEGER IV, PFN

      CHARACTER FNAM*256


      ! Extract path-free portion of file name
      PFN = FN_POSN( FILE_NAME )
      IF ( PFN .GT. 0 ) THEN
        FNAM = FILE_NAME(PFN:)
        FILE_NAME = FNAM
      ELSE
        FILE_NAME = ' '
        RETURN
      END IF

      ! Remove version
      IV = FV_POSN( FILE_NAME )
      IF ( IV .GT. 0 ) FILE_NAME(IV:) = ' '

      RETURN
      END



      SUBROUTINE FN_NAME_EXTRACT( FILE_NAME )

!***********************************************************************
!* Removes the Path Portion of a File Name
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) FILE_NAME ! File name to extract


      ! Variables ______________________________________________________

      INTEGER PFN

      CHARACTER FNAM*256


      ! Extract path-free portion of file name
      PFN = FN_POSN( FILE_NAME )
      IF ( PFN .GT. 0 ) THEN
        FNAM = FILE_NAME(PFN:)
        FILE_NAME = FNAM
      ELSE
        FILE_NAME = ' '
        RETURN
      END IF

      RETURN
      END
