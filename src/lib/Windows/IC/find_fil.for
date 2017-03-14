      SUBROUTINE FIND_FILES( LIST, PATH_EXP, TEMP_O, MATCHED )

!***********************************************************************
!* Finds Files Matching File Name Template and Writes to File
!*
!* Language: Fortran
!*
!* Platform: Windows/IC
!*
!* Compiler: Intel Fortran
!*
!* Author: Stuart G. Mentzer
!*         Andrew Orndorff
!*
!* Date: 2004/10/25
!*
!* Preconditions:
!* . LIST.NAME is a full path case-correct file name
!***********************************************************************

      ! Modules
      USE IFPORT

      ! Headers
      INCLUDE 'file.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      RECORD /FILE/  LIST ! List file object

      LOGICAL PATH_EXP ! Specifies full path names

      CHARACTER*(*) TEMP_O ! File name template

      LOGICAL MATCHED ! Indicates matching files found


      ! Variables ______________________________________________________

      INTEGER(KIND=INT_PTR_KIND()) length
      INTEGER(4) iNAME
      INTEGER(KIND=INT_PTR_KIND()) handle
      INTEGER(4) IOS

      INTEGER(4) FILE_SKIP
      PARAMETER ( FILE_SKIP = FILE$DIR .OR. FILE$HIDDEN .OR.
     & FILE$SYSTEM .OR. FILE$VOLUME )

      CHARACTER($MAXPATH) FILE_SEEK
      CHARACTER(2) drive
      CHARACTER($MAXPATH) dir
      CHARACTER(FILE$MAXNAME) name
      CHARACTER(FILE$MAXNAME) ext
      CHARACTER($MAXPATH) FILE_NAME

      TYPE (FILE$INFO) info


      ! Find files and add to list
      FILE_SEEK = TEMP_O
      IF ( FN_POSN( FILE_SEEK ) .EQ. 0 ) RETURN
      IF ( FE_POSN( FILE_SEEK ) .EQ. 0 ) THEN ! Add implied .* extension
        FILE_SEEK(LEN_TRIM(FILE_SEEK)+1:) = '.*'
      END IF
      length = SPLITPATHQQ( FILE_SEEK, drive, dir, name, ext )
      FILE_NAME = drive//dir
      iNAME = LEN_TRIM( FILE_NAME ) + 1
      handle = FILE$FIRST
      DO WHILE ( .TRUE. ) ! Process each file matching template
        length = GETFILEINFOQQ( FILE_SEEK, info, handle )
        IF ( handle .EQ. FILE$LAST ) THEN ! No more files
          EXIT
        ELSE IF ( handle .EQ. FILE$ERROR ) THEN ! Error
          EXIT
        ELSE IF ( ( length .GT. 0 ) .AND.
     &   ( IAND( info.permit, FILE_SKIP ) .EQ. 0 ) ) THEN ! Found a file
          FILE_NAME(iNAME:) = info.NAME ! Append file name on path
          IF ( FILE_NAME .NE. LIST.NAME ) THEN ! Add to list
            IF ( .NOT. PATH_EXP ) FILE_NAME = info.NAME
            WRITE( LIST.UNIT, '(A)', IOSTAT=IOS )
     &       FILE_NAME(:L_TRIM(FILE_NAME))
            IF ( IOS .NE. 0 ) THEN
              WRITE( *, * ) '*** ERROR - List file write failed'
              RETURN
            END IF
            LIST.N_REC = LIST.N_REC + 1
            MATCHED = .TRUE.
          END IF
        END IF
      END DO

      RETURN
      END
