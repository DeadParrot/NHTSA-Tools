      SUBROUTINE INT_ASSIGN( STRING, VAL, MSG, IOS )

!***********************************************************************
!* Assigns a Character String to an Integer
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/10/18
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) STRING ! String to assign from

      INTEGER VAL ! Value to assign

      LOGICAL MSG ! Specifies that messages are allowed

      INTEGER IOS ! Status flag


      ! Variables ______________________________________________________

      INTEGER IOS_W, LSTR, VAL_TMP

      CHARACTER LSTR_C*3, FMT*11


      ! Functions ______________________________________________________

      CHARACTER LJUST*3

      EXTERNAL LJUST


      ! Read the number from the character string
      IF ( BLANK( STRING ) ) THEN ! Don't accept a blank as zero
        IOS = 1
        RETURN
      END IF
      LSTR = MIN( MAX( LEN_TRIM( STRING ), 1 ), 999 )
      WRITE( LSTR_C, '(I3)', IOSTAT=IOS_W ) LSTR
      FMT = '(BN,I'
      FMT(6:8) = LJUST( LSTR_C )
      FMT(LEN_TRIM(FMT)+1:) = ')'
      READ( STRING(:LSTR), FMT, IOSTAT=IOS ) VAL_TMP
      IF ( IOS .EQ. 0 ) THEN
        VAL = VAL_TMP
      ELSE IF ( MSG ) THEN
        WRITE( *, *, IOSTAT=IOS_W )
     &     '*** ERROR - Numeric assignment failed on: ',
     &     STRING(:LEN_TRIM(STRING))
      END IF

      RETURN
      END



      SUBROUTINE REAL_ASSIGN( STRING, VAL, MSG, IOS )

!***********************************************************************
!* Assigns a Character String to a Real Number
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/10/18
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) STRING ! String to assign from

      REAL VAL ! Value to assign

      LOGICAL MSG ! Specifies that messages are allowed

      INTEGER IOS ! Status flag


      ! Variables ______________________________________________________

      INTEGER IOS_W, LSTR

      REAL VAL_TMP

      CHARACTER LSTR_C*3, FMT*11


      ! Functions ______________________________________________________

      CHARACTER LJUST*3

      EXTERNAL LJUST


      ! Read the number from the character string
      IF ( BLANK( STRING ) ) THEN ! Don't accept a blank as zero
        IOS = 1
        RETURN
      END IF
      LSTR = MIN( MAX( LEN_TRIM( STRING ), 1 ), 999 )
      WRITE( LSTR_C, '(I3)', IOSTAT=IOS_W ) LSTR
      FMT = '(BN,F'
      FMT(6:8) = LJUST( LSTR_C )
      FMT(LEN_TRIM(FMT)+1:) = '.0)'
      READ( STRING(:LSTR), FMT, IOSTAT=IOS ) VAL_TMP
      IF ( IOS .EQ. 0 ) THEN
        VAL = VAL_TMP
      ELSE IF ( MSG ) THEN
        WRITE( *, *, IOSTAT=IOS_W )
     &     '*** ERROR - Numeric assignment failed on: ',
     &     STRING(:LEN_TRIM(STRING))
      END IF

      RETURN
      END



      SUBROUTINE DP_ASSIGN( STRING, VAL, MSG, IOS )

!***********************************************************************
!* Assigns a Character String to a Double Precision Number
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/10/18
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) STRING ! String to assign from

      DOUBLE PRECISION VAL ! Value to assign

      LOGICAL MSG ! Specifies that messages are allowed

      INTEGER IOS ! Status flag


      ! Variables ______________________________________________________

      INTEGER IOS_W, LSTR

      DOUBLE PRECISION VAL_TMP

      CHARACTER LSTR_C*3, FMT*11


      ! Functions ______________________________________________________

      CHARACTER LJUST*3

      EXTERNAL LJUST


      ! Read the number from the character string
      IF ( BLANK( STRING ) ) THEN ! Don't accept a blank as zero
        IOS = 1
        RETURN
      END IF
      LSTR = MIN( MAX( LEN_TRIM( STRING ), 1 ), 999 )
      WRITE( LSTR_C, '(I3)', IOSTAT=IOS_W ) LSTR
      FMT = '(BN,F'
      FMT(6:8) = LJUST( LSTR_C )
      FMT(LEN_TRIM(FMT)+1:) = '.0)'
      READ( STRING(:LSTR), FMT, IOSTAT=IOS ) VAL_TMP
      IF ( IOS .EQ. 0 ) THEN
        VAL = VAL_TMP
      ELSE IF ( MSG ) THEN
        WRITE( *, *, IOSTAT=IOS_W )
     &     '*** ERROR - Numeric assignment failed on: ',
     &     STRING(:LEN_TRIM(STRING))
      END IF

      RETURN
      END



      INTEGER FUNCTION INT_ASGN( STRING )

!***********************************************************************
!* Translates a Character String to an Integer
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/10/18
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) STRING ! String to assign from


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
        INT_ASGN = VAL_TMP
      ELSE
        INT_ASGN = 0
      END IF

      RETURN
      END



      REAL FUNCTION REAL_ASGN( STRING )

!***********************************************************************
!* Translates a Character String to a Real Number
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/10/18
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) STRING ! String to assign from


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
        REAL_ASGN = VAL_TMP
      ELSE
        REAL_ASGN = 0.
      END IF

      RETURN
      END



      DOUBLE PRECISION FUNCTION DP_ASGN( STRING )

!***********************************************************************
!* Translates a Character String to a Double Precision Number
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/10/18
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) STRING ! String to assign from


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
        DP_ASGN = VAL_TMP
      ELSE
        DP_ASGN = 0.D0
      END IF

      RETURN
      END
