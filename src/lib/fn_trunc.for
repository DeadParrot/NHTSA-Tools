      SUBROUTINE FN_TRUNC( FILE_NAME )

!***********************************************************************
!* Truncates a File Name for the Operating System
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/18
!***********************************************************************

      ! Headers
      INCLUDE 'platform.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) FILE_NAME ! File name to truncate


      ! Variables ______________________________________________________

      INTEGER IE, ITV, PFN, L_NAM, L_EXT

      CHARACTER TEMP_NAME*256


      ! Functions ______________________________________________________

      CHARACTER LJUST*256

      EXTERNAL LJUST


      ! Find start of path-free file name
      FILE_NAME = LJUST( FILE_NAME )
      PFN = FN_POSN( FILE_NAME )
      IF ( PFN .EQ. 0 ) RETURN

      ! Truncate name if necessary
      IE = FE_POSN( FILE_NAME(PFN:) )
      IF ( IE .GT. 0 ) THEN
        L_NAM = IE - 1
      ELSE
        L_NAM = LEN_TRIM( FILE_NAME(PFN:) )
      END IF
      IF ( L_NAM .GT. LEN_FN_NAM ) THEN ! Truncate name
        TEMP_NAME =
     &     FILE_NAME(:PFN-1+LEN_FN_NAM)//FILE_NAME(PFN+L_NAM:)
        FILE_NAME = TEMP_NAME
      END IF

      ! Truncate extension if necessary
      IE = FE_POSN( FILE_NAME )
      IF ( IE .GT. 0 ) THEN ! Has extension
        ITV = FT_POSN( FILE_NAME )
        IF ( ITV .EQ. 0 ) ITV = FV_POSN( FILE_NAME )
        IF ( ITV .EQ. 0 ) THEN ! No type/version
          L_EXT = LEN_TRIM( FILE_NAME(IE+1:) )
        ELSE IF ( ITV .GT. IE + 1 ) THEN
          L_EXT = LEN_TRIM( FILE_NAME(IE+1:ITV-1) )
        ELSE
          L_EXT = 0
        END IF
        IF ( L_EXT .GT. LEN_FN_EXT ) THEN ! Truncate extension
          IF ( ITV .EQ. 0 ) THEN ! No type/version
            FILE_NAME(IE+LEN_FN_EXT+1:) = ' '
          ELSE ! Has type/version
            TEMP_NAME =
     &         FILE_NAME(:IE+LEN_FN_EXT)//FILE_NAME(ITV:)
            FILE_NAME = TEMP_NAME
          END IF
        END IF
      END IF

      RETURN
      END
