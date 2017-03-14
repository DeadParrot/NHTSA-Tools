      SUBROUTINE FIND_FILES( LIST, PATH_EXP, TEMP_O, MATCHED )

!***********************************************************************
!* Finds Files Matching File Name Template and Writes to File
!*
!* Language: Fortran
!*
!* Platform: Linux/GCC
!*
!* Compiler: GFortran
!*
!* Author: Stuart G. Mentzer
!*         Andrew Orndorff
!*
!* Date: 1999/08/20
!*
!* Notes:
!*
!* . Uses external subroutine WILD_MATCH (C) to perform pattern
!*   matching in lieu of non-standard internal pattern matching
!*   that some operating systems/compilers offer.
!*
!* . Try searches both without and with a trailing ".*" for conformity
!*   with search algorithms on other platforms.
!*
!* . Filename strings are not null terminated because EPC allows us to
!*   handle strings as string descriptors (meaning - we know the length)
!***********************************************************************

      ! Headers
      INCLUDE 'file.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      RECORD /FILE/  LIST ! List file object

      LOGICAL PATH_EXP ! Specifies full path names

      CHARACTER*(*) TEMP_O ! File name template

      LOGICAL MATCHED ! Indicates matching files found


      ! Variables ______________________________________________________

      INTEGER IOS, IERR, IV, LLEN, PFN

      PARAMETER ( LLEN = 256 )

      CHARACTER FILE_NAME*(LLEN), FNAM*(LLEN),
     & TEMP_I*(LLEN), TEMP_I2*(LLEN)


      ! Functions ______________________________________________________

      ! C wrapper functions
      INTEGER OPEN_DIR, READ_DIR, CLOSE_DIR, WILD_MATCH

      EXTERNAL OPEN_DIR, READ_DIR, CLOSE_DIR, WILD_MATCH


      ! Set/check file name template
      FILE_NAME = TEMP_O
      PFN = FN_POSN( FILE_NAME )
      IF ( PFN .EQ. 0 ) RETURN

      ! Need simple filename for pattern matching
      TEMP_I = FILE_NAME(PFN:)
      TEMP_I2 = TEMP_I
      IF ( FE_POSN( TEMP_I ) .EQ. 0 ) THEN ! No extension
        IV = FV_POSN( TEMP_I )
        IF ( IV .EQ. 0 ) THEN ! No version
          TEMP_I2(LEN_TRIM(TEMP_I2)+1:) = '.*'
        ELSE ! Has version
          TEMP_I2(IV:) = '.*'//TEMP_I(IV:)
        END IF
      END IF

      ! Find files and add to list
      IERR = OPEN_DIR( FILE_NAME )
      IF ( IERR .EQ. 0 ) IERR = READ_DIR( FNAM )
      DO WHILE ( ( IERR .EQ. 0 ) .AND. ( .NOT. BLANK( FNAM ) ) )
        ! Perform a comparison between the starting name and
        ! the name returned by the READ_DIR() call
        IERR = WILD_MATCH( TEMP_I, FNAM )
        IF ( IERR .NE. 0 ) THEN
          IERR = WILD_MATCH( TEMP_I2, FNAM )
        END IF
        IF ( IERR .EQ. 0 ) THEN ! Write name to list
          FILE_NAME(FN_POSN(FILE_NAME):) = FNAM
          INQUIRE( FILE=FILE_NAME, NAME=FILE_NAME, IOSTAT=IOS )
          IF ( FILE_NAME .NE. LIST.NAME ) THEN ! Add to list
            IF ( .NOT. PATH_EXP ) CALL FN_NAME_EXTRACT( FILE_NAME )
            WRITE( LIST.UNIT, '(A)', IOSTAT=IOS )
     &       FILE_NAME(:L_TRIM(FILE_NAME))
            IF ( IOS .NE. 0 ) THEN
              WRITE( *, * ) '*** ERROR - List file write failed'
              IERR = CLOSE_DIR()
              RETURN
            END IF
            LIST.N_REC = LIST.N_REC + 1
            MATCHED = .TRUE.
          END IF
        END IF
        IERR = READ_DIR( FNAM )
      END DO
      IERR = CLOSE_DIR()

      RETURN
      END
