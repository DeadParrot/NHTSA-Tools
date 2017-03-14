      INTEGER FUNCTION FE_POSN( FILE_NAME )

!***********************************************************************
!* Finds Start Position of File Name Extension
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/18
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) FILE_NAME ! File name


      ! Variables ______________________________________________________

      INTEGER LFN, PFN, PFT, PFV, PFE


      ! Functions ______________________________________________________

      INTEGER FN_POSN, FT_POSN, FV_POSN, LAST_INDEX

      EXTERNAL FN_POSN, FT_POSN, FV_POSN, LAST_INDEX


      ! Find start of file name extension
      PFN = FN_POSN( FILE_NAME )
      IF ( PFN .GT. 0 ) THEN ! File name is present
        LFN = LEN_TRIM( FILE_NAME )
        PFT = FT_POSN( FILE_NAME )
        IF ( PFT .EQ. 0 ) PFT = LFN + 1
        PFV = FV_POSN( FILE_NAME )
        IF ( PFV .EQ. 0 ) PFV = LFN + 1
        PFE = MIN( PFT, PFV ) - 1
        FE_POSN = LAST_INDEX( FILE_NAME(PFN:PFE), '.' )
        IF ( FE_POSN .GT. 0 ) FE_POSN = FE_POSN + PFN - 1
      ELSE ! No file name
        FE_POSN = 0
      END IF

      RETURN
      END
