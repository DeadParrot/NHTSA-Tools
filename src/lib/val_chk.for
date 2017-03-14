      LOGICAL FUNCTION INT_CHK( STRING )

!***********************************************************************
!* Checks a Character String is Valid Integer
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/10/18
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) STRING ! String to check


      ! Variables ______________________________________________________

      INTEGER IOS, IOS_W, LSTR

      INTEGER VAL_TMP

      CHARACTER LSTR_C*3, FMT*11


      ! Functions ______________________________________________________

      CHARACTER LJUST*3

      EXTERNAL LJUST


      ! Read the number from the character string
      LSTR = MIN( MAX( LEN_TRIM( STRING ), 1 ), 999 )
      WRITE( LSTR_C, '(I3)', IOSTAT=IOS_W ) LSTR
      FMT = '(BN,I'
      FMT(6:8) = LJUST( LSTR_C )
      FMT(LEN_TRIM(FMT)+1:) = ')'
      READ( STRING(:LSTR), FMT, IOSTAT=IOS ) VAL_TMP
      IF ( IOS .EQ. 0 ) THEN
        INT_CHK = .TRUE.
      ELSE
        INT_CHK = .FALSE.
      END IF

      RETURN
      END



      LOGICAL FUNCTION REAL_CHK( STRING )

!***********************************************************************
!* Checks a Character String is Valid Real Number
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/10/18
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) STRING ! String to check


      ! Variables ______________________________________________________

      INTEGER IOS, IOS_W, LSTR

      REAL VAL_TMP

      CHARACTER LSTR_C*3, FMT*11


      ! Functions ______________________________________________________

      CHARACTER LJUST*3

      EXTERNAL LJUST


      ! Read the number from the character string
      LSTR = MIN( MAX( LEN_TRIM( STRING ), 1 ), 999 )
      WRITE( LSTR_C, '(I3)', IOSTAT=IOS_W ) LSTR
      FMT = '(BN,F'
      FMT(6:8) = LJUST( LSTR_C )
      FMT(LEN_TRIM(FMT)+1:) = '.0)'
      READ( STRING(:LSTR), FMT, IOSTAT=IOS ) VAL_TMP
      IF ( IOS .EQ. 0 ) THEN
        REAL_CHK = .TRUE.
      ELSE
        REAL_CHK = .FALSE.
      END IF

      RETURN
      END



      LOGICAL FUNCTION DP_CHK( STRING )

!***********************************************************************
!* Checks a Character String is Valid Double Precision Number
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/10/18
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) STRING ! String to check


      ! Variables ______________________________________________________

      INTEGER IOS, IOS_W, LSTR

      DOUBLE PRECISION VAL_TMP

      CHARACTER LSTR_C*3, FMT*11


      ! Functions ______________________________________________________

      CHARACTER LJUST*3

      EXTERNAL LJUST


      ! Read the number from the character string
      LSTR = MIN( MAX( LEN_TRIM( STRING ), 1 ), 999 )
      WRITE( LSTR_C, '(I3)', IOSTAT=IOS_W ) LSTR
      FMT = '(BN,F'
      FMT(6:8) = LJUST( LSTR_C )
      FMT(LEN_TRIM(FMT)+1:) = '.0)'
      READ( STRING(:LSTR), FMT, IOSTAT=IOS ) VAL_TMP
      IF ( IOS .EQ. 0 ) THEN
        DP_CHK = .TRUE.
      ELSE
        DP_CHK = .FALSE.
      END IF

      RETURN
      END
