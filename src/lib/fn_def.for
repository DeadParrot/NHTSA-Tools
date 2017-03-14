      SUBROUTINE FN_DEF( S, DEF_FILE )

!***********************************************************************
!* Sets a Basic Default File Name Based on UDS Specifications
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

      INTEGER IOS, LDF, LDT

      CHARACTER FFLAG


      ! Functions ______________________________________________________

      CHARACTER LJUST*20, COMPRESS*4, DNCASE*50

      EXTERNAL LJUST, COMPRESS, DNCASE


      ! Set default file name
      DEF_FILE = ' '
      LDF = LEN( DEF_FILE )
      IF ( LDF .LT. 12 ) RETURN ! Too short for basic name
      IF ( LEN_TRIM( S.TSTSRC ) .GT. 0 ) THEN
        DEF_FILE(1:1) = LJUST( S.TSTSRC )
      ELSE ! Flag as generic UDS file
        DEF_FILE(1:1) = 'u'
      END IF
      IF ( ( S.TSTNO .NE. 0 ) .OR. ( LEN_TRIM( S.TSTNAM ) .EQ. 0 ) )
     & THEN
        WRITE( DEF_FILE(2:5), '(I4)', IOSTAT=IOS ) S.TSTNO
      ELSE ! Use Test Name
        DEF_FILE(2:5) = COMPRESS( S.TSTNAM )
      END IF
      CALL FILTER_FLAG( S.CURTYP, S.FCUT, S.FSTP, FFLAG )
      DEF_FILE(LEN_TRIM(DEF_FILE)+1:) =
     & FN_TYPE(S.YTYP)//S.STATUS(1:1)//FFLAG//'.'
      LDT = LEN_TRIM( DEF_FILE )
      IF ( ( S.CURNO .NE. 0 ) .OR. ( LEN_TRIM( S.CURNAM ) .EQ. 0 ) )
     & THEN
        WRITE( DEF_FILE(LDT+1:LDT+3), '(I3)', IOSTAT=IOS ) S.CURNO
      ELSE
        DEF_FILE(LDT+1:LDT+3) = COMPRESS( S.CURNAM )
      END IF
      CALL ZERO_FILL( DEF_FILE )
      LDT = LEN_TRIM( DEF_FILE )
      IF ( LDT + 4 .LE. LDF ) THEN ! Add type extension
        DEF_FILE(LDT+1:) = '.uds'
      END IF
      DEF_FILE = DNCASE( DEF_FILE )
      CALL FN_TRUNC( DEF_FILE )

      RETURN
      END
