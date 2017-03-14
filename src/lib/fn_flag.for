      SUBROUTINE FN_FLAG( FILE_NAME, FLAG, OFFSET, POSN )

!***********************************************************************
!* Sets File Name Flag Characters
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/18
!***********************************************************************

      ! Headers
      INCLUDE 'platform.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) FILE_NAME ! File name

      CHARACTER*(*) FLAG ! Flag string

      INTEGER OFFSET ! Requested offset behind period

      INTEGER POSN ! Position (starting index) where FLAG is placed


      ! Variables ______________________________________________________

      INTEGER IE, IP, IPR, IOFF, IT, LF, PFN

      CHARACTER TEMP_NAME*256


      ! Initializations
      POSN = 0

      ! Find reference period location
      CALL FN_TRUNC( FILE_NAME )
      PFN = FN_POSN( FILE_NAME )
      IF ( PFN .EQ. 0 ) RETURN
      IE = FE_POSN( FILE_NAME )
      IP = IE
      IF ( IP .EQ. 0 ) THEN ! No extension: Try type extension
        IP = FT_POSN( FILE_NAME )
      ELSE IF ( INDEX( FILE_NAME(PFN:IP-1), '.' ) .GT. 0 ) THEN
        ! Periods in name portion: Use type extension
        IT = FT_POSN( FILE_NAME )
        IF ( IT .GT. 0 ) IP = IT
      END IF
      IF ( IP .EQ. 0 ) IP = LEN_TRIM( FILE_NAME ) + 1

      ! Set flag
      IPR = IP - PFN + 1
      LF = LEN_TRIM( FLAG )
      IOFF = MAX( OFFSET, LF )
      IF ( IPR .GE. MAX( 5, IOFF+2 ) ) THEN ! Overwrite with FLAG
        POSN = IP - IOFF
        FILE_NAME(POSN:POSN+LF-1) = FLAG
      ELSE ! Append/overlap FLAG
        TEMP_NAME = FILE_NAME
        POSN = MIN( IPR, LEN_FN_NAM-LF+1 ) + PFN - 1
        FILE_NAME = TEMP_NAME(:POSN-1)//FLAG(:LF)//TEMP_NAME(IP:)
      END IF

      RETURN
      END
