      INTEGER FUNCTION FT_POSN( FILE_NAME )

!***********************************************************************
!* Finds Start Position of Recognized Type Extension in a File Name
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/26
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) FILE_NAME ! File name


      ! Variables ______________________________________________________

      INTEGER PFN, PFV, PFE


      ! Functions ______________________________________________________

      LOGICAL FN_EQUAL

      INTEGER FN_POSN, FV_POSN, LAST_INDEX

      EXTERNAL FN_EQUAL, FN_POSN, FV_POSN, LAST_INDEX


      ! Find start of file name type extension
      PFN = FN_POSN( FILE_NAME )
      IF ( PFN .GT. 0 ) THEN ! File name is present
        PFV = FV_POSN( FILE_NAME )
        IF ( PFV .EQ. 0 ) PFV = LEN_TRIM( FILE_NAME ) + 1
        PFE = PFV - 1
        FT_POSN = LAST_INDEX( FILE_NAME(PFN:PFE), '.' )
        IF ( FT_POSN .GT. 0 ) THEN
          FT_POSN = FT_POSN + PFN - 1
          IF ( FN_EQUAL( FILE_NAME(FT_POSN:PFE), '.uds' ) ) THEN
            ! OK
          ELSE IF ( FN_EQUAL( FILE_NAME(FT_POSN:PFE), '.txt' ) ) THEN
            ! OK
          ELSE IF ( FN_EQUAL( FILE_NAME(FT_POSN:PFE), '.lis' ) ) THEN
            ! OK
          ELSE IF ( FN_EQUAL( FILE_NAME(FT_POSN:PFE), '.asc' ) ) THEN
            ! OK
          ELSE IF ( FN_EQUAL( FILE_NAME(FT_POSN:PFE), '.dat' ) ) THEN
            ! OK
          ELSE ! Not a recognized type extension
            FT_POSN = 0
          END IF
        END IF
      ELSE ! No file name
        FT_POSN = 0
      END IF

      RETURN
      END



      INTEGER FUNCTION FT_POSN_S( FILE_NAME, TYPE_EXTENSION )

!***********************************************************************
!* Finds Start Position of Specified Type Extension in a File Name
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/26
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) FILE_NAME ! File name

      CHARACTER*(*) TYPE_EXTENSION ! Type extension to look for


      ! Variables ______________________________________________________

      INTEGER PFN, PFV, PFE


      ! Functions ______________________________________________________

      LOGICAL FN_EQUAL

      INTEGER FN_POSN, FV_POSN, LAST_INDEX

      EXTERNAL FN_EQUAL, FN_POSN, FV_POSN, LAST_INDEX


      ! Find start of file name type extension
      PFN = FN_POSN( FILE_NAME )
      IF ( PFN .GT. 0 ) THEN ! File name is present
        PFV = FV_POSN( FILE_NAME )
        IF ( PFV .EQ. 0 ) PFV = LEN_TRIM( FILE_NAME ) + 1
        PFE = PFV - 1
        FT_POSN_S = LAST_INDEX( FILE_NAME(PFN:PFE), '.' )
        IF ( FT_POSN_S .GT. 0 ) THEN
          FT_POSN_S = FT_POSN_S + PFN - 1
          IF ( TYPE_EXTENSION(1:1) .EQ. '.' ) THEN
            IF ( .NOT. FN_EQUAL( FILE_NAME(FT_POSN_S:PFE),
     &       TYPE_EXTENSION ) ) THEN
              FT_POSN_S = 0
            END IF
          ELSE
            IF ( .NOT. FN_EQUAL( FILE_NAME(FT_POSN_S:PFE),
     &       '.'//TYPE_EXTENSION ) ) THEN
              FT_POSN_S = 0
            END IF
          END IF
        END IF
      ELSE ! No file name
        FT_POSN_S = 0
      END IF

      RETURN
      END
