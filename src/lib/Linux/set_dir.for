      SUBROUTINE SET_DIR( DIR_OUT )

!***********************************************************************
!* Sets an Output Directory if Specified or if Local is Read-Only
!*
!* Language: Fortran
!*
!* Platform: Linux
!*
!* Compiler: Fortran
!*
!* Author: Stuart G. Mentzer
!*         Andrew Orndorff
!*
!* Date: 1999/10/18
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) DIR_OUT ! Output directory


      ! Variables ______________________________________________________

      INTEGER IOS, IOS_W


      ! Set output directory
      IF ( BLANK( DIR_OUT ) ) THEN ! Look for valid output directory
        ! Try local directory
        IOS = TRY_DIR( DIR_OUT )
        IF ( IOS .NE. 0 ) THEN ! Look for non-local directory
          ! Check DIR_OUT environment variable
          CALL GET_ENV( 'DIR_OUT', DIR_OUT )
          IF ( .NOT. BLANK( DIR_OUT ) ) THEN ! Use DIR_OUT path
            IOS = TRY_DIR( DIR_OUT )
            IF ( IOS .NE. 0 ) THEN ! Try creating the directory
              IOS = MAKE_DIR( DIR_OUT )
              IF ( IOS .EQ. 0 ) IOS = TRY_DIR( DIR_OUT )
              IF ( IOS .NE. 0 ) THEN
                WRITE( *, *, IOSTAT=IOS_W )
     &           '*** Output failed to DIR_OUT = ',
     &           DIR_OUT(:L_TRIM(DIR_OUT))
                STOP ' '
              END IF
            END IF
          ELSE ! Use a default output directory
            DIR_OUT = '/tmp/'
            ! Create output subdirectory if necessary
            IOS = TRY_DIR( DIR_OUT )
            IF ( IOS .NE. 0 ) THEN
              IOS = MAKE_DIR( DIR_OUT )
              IF ( IOS .EQ. 0 ) IOS = TRY_DIR( DIR_OUT )
              IF ( IOS .NE. 0 ) THEN
                WRITE( *, * ) '*** Output failed to /tmp/'
                STOP ' '
              END IF
            END IF
          END IF
        END IF
      ELSE ! Try specified directory
        IOS = TRY_DIR( DIR_OUT )
        IF ( IOS .NE. 0 ) THEN
          IOS = MAKE_DIR( DIR_OUT )
          IF ( IOS .EQ. 0 ) IOS = TRY_DIR( DIR_OUT )
          IF ( IOS .NE. 0 ) THEN
            WRITE( *, *, IOSTAT=IOS_W )
     &       '*** Output failed to DIR_OUT = ',
     &       DIR_OUT(:L_TRIM(DIR_OUT))
            STOP ' '
          END IF
        END IF
      END IF

      RETURN
      END



      INTEGER FUNCTION TRY_DIR( DIR_OUT )

!***********************************************************************
!* Tries a Specified Directory for Output
!*
!* Language: Fortran
!*
!* Platform: Linux
!*
!* Compiler: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/09/10
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) DIR_OUT ! Output directory


      ! Variables ______________________________________________________

      INTEGER LUN, IOS, IOC, LD, LD1

      CHARACTER FILE_NAME*256


      ! Functions ______________________________________________________

      LOGICAL ANY_CHARS

      EXTERNAL ANY_CHARS


      ! Try DIR_OUT for output
      LD = LEN_TRIM( DIR_OUT )
      LD1 = MAX( LD, 1 )
      IF ( ( LD .GT. 0 ) .AND.
     & ( .NOT. ANY_CHARS( DIR_OUT(LD1:LD1), '/' ) ) ) THEN
        ! Add path separator
        LD = LD + 1
        DIR_OUT(LD:LD) = '/'
      END IF
      FILE_NAME = DIR_OUT(:LD)//'try_dir.out'
      CALL FN_INCR( FILE_NAME )
      CALL OPEN_FS( LUN, FILE_NAME, 'W', IOS )
      CLOSE( UNIT=LUN, STATUS='DELETE', IOSTAT=IOC )
      TRY_DIR = IOS

      RETURN
      END
