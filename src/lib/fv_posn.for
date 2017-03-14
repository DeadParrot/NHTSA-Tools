      INTEGER FUNCTION FV_POSN( FILE_NAME )

!***********************************************************************
!* Finds Start Position of File Name Version
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************

      ! Headers
      INCLUDE 'platform.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) FILE_NAME ! File name


      ! Variables ______________________________________________________

      INTEGER IOS, IVER, PFN

      CHARACTER VER*25


      ! Functions ______________________________________________________

      INTEGER FN_POSN, LAST_INDEX

      EXTERNAL FN_POSN, LAST_INDEX


      ! Find start of file name version (position of version separator)
      IF ( VERSION_SEP .EQ. ' ' ) THEN ! No versions supported
        FV_POSN = 0
        RETURN
      END IF
      PFN = FN_POSN( FILE_NAME )
      IF ( PFN .GT. 0 ) THEN ! File name is present
        FV_POSN = LAST_INDEX( FILE_NAME(PFN:), VERSION_SEP )
        IF ( FV_POSN .GT. 0 ) THEN
          FV_POSN = FV_POSN + PFN - 1
          VER = FILE_NAME(FV_POSN+1:)
          READ( VER, '(BN,I25)', IOSTAT=IOS ) IVER ! Check it's a number
          IF ( IOS .NE. 0 ) FV_POSN = 0 ! Not a number
        END IF
      ELSE ! No file name
        FV_POSN = 0
      END IF

      RETURN
      END
