      SUBROUTINE FN_MERGE( FileSpec, Path, Name, Extension, Version )

!***********************************************************************
!* Merges a File Specification from its Path, Name, Extension, and Version
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************

      ! Headers
      INCLUDE 'platform.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) FileSpec ! File specification to create

      CHARACTER*(*) Path ! File path

      CHARACTER*(*) Name ! File name (without path or extension)

      CHARACTER*(*) Extension ! File extension (without separator)

      CHARACTER*(*) Version ! File version (without separator)


      ! Variables ______________________________________________________

      INTEGER L, LFS


      ! Merge the file specification
      LFS = LEN( FileSpec )
      FileSpec = Path
      L = LEN_TRIM( FileSpec )
      IF ( ( L .GT. 0 ) .AND. ( L .LT. LFS ) ) THEN ! Has path
        IF ( FileSpec(L:L) .NE. DIR_FILE_SEP ) THEN
          FileSpec(L+1:) = DIR_FILE_SEP
          L = LEN_TRIM( FileSpec )
        END IF
      END IF
      IF ( L .LT. LFS ) FileSpec(L+1:) = Name
      L = LEN_TRIM( FileSpec )
      IF ( ( .NOT. BLANK( Extension ) ) .AND. ( L .LT. LFS ) ) THEN
        ! Has extension
        IF ( Extension(1:1) .NE. '.' ) THEN
          FileSpec(L+1:L+1) = '.'
          L = L + 1
        END IF
        IF ( L .LT. LFS ) FileSpec(L+1:) = Extension
        L = LEN_TRIM( FileSpec )
      END IF
      IF ( ( .NOT. BLANK( Version ) ) .AND. ( L .LT. LFS ) ) THEN
        ! Has version
        IF ( Version(1:1) .NE. VERSION_SEP ) THEN
          FileSpec(L+1:) = VERSION_SEP
          L = LEN_TRIM( FileSpec )
        END IF
        IF ( L .LT. LFS ) FileSpec(L+1:) = Version
      END IF

      RETURN
      END
