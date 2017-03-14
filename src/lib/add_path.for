      SUBROUTINE ADD_PATH( DIR_OUT, FILE_NAME )

!***********************************************************************
!* Adds a Directory Specification to a Path-Free File Name
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/10/18
!***********************************************************************

      ! Headers
      INCLUDE 'platform.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) DIR_OUT ! Output directory

      CHARACTER*(*) FILE_NAME ! File name


      ! Variables ______________________________________________________

      INTEGER LD

      CHARACTER FILE_TEMP*512


      ! Add DIR_OUT if FILE_NAME is path-free
      IF ( ( .NOT. BLANK( DIR_OUT ) ) .AND.
     & ( .NOT. ANY_CHARS( FILE_NAME, PATH_CHARS ) ) ) THEN
        FILE_TEMP = DIR_OUT
        LD = LEN_TRIM( DIR_OUT )
        IF ( .NOT. ANY_CHARS( DIR_OUT(LD:LD), PATH_CHARS ) ) THEN
          FILE_TEMP(LEN_TRIM(FILE_TEMP)+1:) = DIR_FILE_SEP
        END IF
        FILE_TEMP(LEN_TRIM(FILE_TEMP)+1:) = FILE_NAME
        FILE_NAME = FILE_TEMP
      END IF

      RETURN
      END
