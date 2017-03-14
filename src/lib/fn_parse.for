      SUBROUTINE FN_PARSE( FileSpec, Path, Name, Extension,
     & TypeExtension, Version )

!***********************************************************************
!* Parses a File Specification
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

      CHARACTER*(*) FileSpec ! File specification to parse

      CHARACTER*(*) Path ! File path

      CHARACTER*(*) Name ! File name (w/o path or extension)

      CHARACTER*(*) Extension ! File extension (w/o separator)

      CHARACTER*(*) TypeExtension ! File type extension (w/o separator)

      CHARACTER*(*) Version ! File version (w/o separator)


      ! Variables ______________________________________________________

      INTEGER IE, L, PFN, PFE, PFT, PFV


      ! Parse the file specification
      PFN = FN_POSN( FileSpec )
      PFE = FE_POSN( FileSpec )
      PFT = FT_POSN( FileSpec )
      PFV = FV_POSN( FileSpec )
      IF ( PFN .GT. 1 ) THEN
        Path = FileSpec(:PFN-1)
        L = LEN_TRIM( Path )
        IF ( ( Path(L:L) .EQ. DIR_FILE_SEP ) .AND.
     &     ( ANY_CHARS( DIR_FILE_SEP, '\/' ) ) ) THEN
          ! Trim DOS/Un*x end separator
          Path(L:L) = ' '
        END IF
      ELSE
        Path = ' '
      END IF
      IE = L_TRIM( FileSpec )
      IF ( PFV .GT. 0 ) THEN ! Has version
        Version = FileSpec(PFV+1:IE)
        IE = PFV - 1
      ELSE
        Version = ' '
      END IF
      IF ( PFT .GT. 0 ) THEN ! Has type extension
        TypeExtension = FileSpec(PFT+1:IE)
        IE = PFT - 1
      ELSE
        TypeExtension = ' '
      END IF
      IF ( PFE .GT. 0 ) THEN ! Has extension
        Extension = FileSpec(PFE+1:IE)
        IE = PFE - 1
      ELSE
        Extension = ' '
      END IF
      IF ( PFN .GT. 0 ) THEN ! Has name
        Name = FileSpec(PFN:IE)
        IE = PFN - 1
      ELSE
        Name = ' '
      END IF

      RETURN
      END
