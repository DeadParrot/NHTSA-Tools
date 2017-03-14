      SUBROUTINE FN_UNIQ( DIR_OUT, FILE_NAME )

!***********************************************************************
!* Selects a Unique File Name in a Specified Directory
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*         Andrew Orndorff
!*
!* Date: 2004/10/25
!***********************************************************************

      ! Headers
      INCLUDE 'platform.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) DIR_OUT ! Output directory

      CHARACTER*(*) FILE_NAME ! File name to increment


      ! Variables ______________________________________________________

      LOGICAL FILE_X

      INTEGER IOS, PFN, IP, LEXT, LTOT, ITIME

      PARAMETER ( LEXT = LEN_FN_EXT+1, LTOT = LEN_FN_NAM+LEXT )

      CHARACTER FNAM*(LEN_FN_NAM), FEXT*(LEXT), FFNAM*(LTOT)


      ! Set base file name
      IF ( ( BLANK( FILE_NAME ) ) .OR.
     & ( .NOT. FN_VALID( FILE_NAME ) ) ) FILE_NAME = 'fnunique.fil'
      CALL ADD_PATH( DIR_OUT, FILE_NAME )

      ! See if the file exists already
      CALL FN_TRUNC( FILE_NAME )
      INQUIRE( FILE=FILE_NAME, EXIST=FILE_X, IOSTAT=IOS )
      IF ( ( .NOT. FILE_X ) .OR. ( IOS .NE. 0 ) ) RETURN

      ! Get file name components
      PFN = FN_POSN( FILE_NAME )
      IP = FE_POSN( FILE_NAME )
      IF ( IP .EQ. 0 ) IP = FV_POSN( FILE_NAME )
      IF ( PFN .EQ. 0 ) THEN ! No file name
        FNAM = 'fnunique'
        FEXT = '.fil'
      ELSE IF ( IP .GT. 0 ) THEN ! Has extension/version
        FNAM = FILE_NAME(PFN:IP-1)
        FEXT = FILE_NAME(IP:)
      ELSE ! No extension/version
        FNAM = FILE_NAME(PFN:)
        FEXT = ' '
      END IF

      ! Find time-based unique file name
      IOS = 0
      DO WHILE ( ( FILE_X ) .AND. ( IOS .EQ. 0 ) )
        ITIME = MOD( NINT( SECNDS(0.) ), 1000000 )
        WRITE( FNAM(3:8), '(I6)', IOSTAT=IOS ) ITIME
        FFNAM = FNAM(:LEN_TRIM(FNAM))//FEXT
        CALL ZERO_FILL( FFNAM )
        FILE_NAME(PFN:) = FFNAM
        INQUIRE( FILE=FILE_NAME, EXIST=FILE_X, IOSTAT=IOS )
      END DO

      RETURN
      END
