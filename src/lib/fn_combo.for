      SUBROUTINE FN_COMBO( S, DEF_FILE )

!***********************************************************************
!* Sets Default File Name Extension for Combined UDS Files
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/18
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      RECORD /UDS_SPEC/  S ! UDS spec object

      CHARACTER*(*) DEF_FILE ! Default file name returned


      ! Variables ______________________________________________________

      INTEGER IOS, IE, LDF, LDT, PFN, LOCSEN


      ! Functions ______________________________________________________

      CHARACTER LJUST*30

      EXTERNAL LJUST


      ! Set extension for combined UDS file
      LDF = LEN( DEF_FILE )
      PFN = FN_POSN( DEF_FILE )
      IF ( PFN .EQ. 0 ) RETURN
      IE = FE_POSN( DEF_FILE )
      IF ( IE .EQ. 0 ) THEN ! Add extension
        IE = LEN_TRIM( DEF_FILE ) + 1
        IF ( IE .LE. LDF ) DEF_FILE(IE:IE) = '.'
      END IF
      IF ( IE + 1 .GT. LDF ) RETURN
      DEF_FILE(IE+1:) = ' '
      IF ( IE + 3 .GT. LDF ) RETURN
      DEF_FILE(IE+1:IE+3) = LJUST( S.CURNAM )
      IF ( S.SENLOC .NE. ' ' ) THEN ! Occupant attachment
        IF ( S.SENLOC .EQ. 'OT' ) THEN
          DEF_FILE(IE+2:IE+2) = 'o'
        ELSE IF ( S.CMPNO .LT. 2 ) THEN
          DEF_FILE(IE+2:IE+2) = S.SENLOC(2:2)
        ELSE ! Component/vehicle 2
          READ( S.SENLOC, '(BN,I2)', IOSTAT=IOS ) LOCSEN
          IF ( IOS .NE. 0 ) LOCSEN = 0
          LOCSEN = LOCSEN + 5
          IF ( LOCSEN .GT. 9 ) LOCSEN = 0
          WRITE( DEF_FILE(IE+2:IE+2), '(I1)', IOSTAT=IOS ) LOCSEN
          IF ( IOS .NE. 0 ) DEF_FILE(IE+2:IE+2) = '0'
        END IF
        IF ( LEN_TRIM( S.SENATT ) .GT. 0 ) THEN
          DEF_FILE(IE+3:IE+3) = LJUST( S.SENATT )
        ELSE ! Generic occupant attachment
          DEF_FILE(IE+3:IE+3) = 'o'
        END IF
      ELSE IF ( ( S.TSTCFN .EQ. 'VTV' ) .OR.
     & ( S.TSTCFN .EQ. 'VTI' ) .OR.
     & ( S.TSTCFN .EQ. 'ITV' ) .OR.
     & ( S.TSTCFN .EQ. 'ITI' ) .OR.
     & ( S.CMPNO .GT. 1 ) ) THEN ! Multiple components
        WRITE( DEF_FILE(IE+2:IE+2), '(I1)', IOSTAT=IOS ) S.CMPNO
        IF ( IOS .NE. 0 ) DEF_FILE(IE+2:IE+2) = '0'
        IF ( LEN_TRIM( S.SENATT ) .GT. 0 ) THEN
          DEF_FILE(IE+3:IE+3) = LJUST( S.SENATT )
        ELSE ! Generic component attachment
          DEF_FILE(IE+3:IE+3) = 'c'
        END IF
      ELSE IF ( LEN_TRIM( S.SENATT ) .GT. 0 ) THEN
        ! Common sensor attachment
        DEF_FILE(IE+3:IE+3) = LJUST( S.SENATT )
      END IF
      LDT = LEN_TRIM( DEF_FILE )
      IF ( LDT + 4 .LE. LDF ) THEN ! Add type extension
        DEF_FILE(LDT+1:) = '.uds'
      END IF
      CALL STR_DN( DEF_FILE )
      CALL FN_TRUNC( DEF_FILE )

      RETURN
      END
