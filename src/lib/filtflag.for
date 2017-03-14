      SUBROUTINE FILTER_FLAG( CURTYP, FCUT, FSTP, FLAG )

!***********************************************************************
!* Sets Standard UDS File Filtering Flag Character
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) CURTYP ! Curve type

      REAL FCUT ! Cutoff frequency

      REAL FSTP ! Stop frequency

      CHARACTER FLAG ! Filter class flag returned


      ! Variables ______________________________________________________

      REAL FMAX


      ! Set filtering flag
      IF ( CURTYP .EQ. 'TIME SERIES' ) THEN
        IF ( FCUT .EQ. 0. ) THEN
          FLAG = '0'
        ELSE IF ( FCUT .EQ. 15. ) THEN
          FLAG = 'a'
        ELSE IF ( FCUT .EQ. 30. ) THEN
          FLAG = 'b'
        ELSE IF ( FCUT .EQ. 60. ) THEN
          FLAG = 'c'
        ELSE IF ( FCUT .EQ. 100. ) THEN
          FLAG = 'd'
        ELSE IF ( FCUT .EQ. 300. ) THEN
          FLAG = 'e'
        ELSE IF ( FCUT .EQ. 500. ) THEN
          FLAG = 'f'
        ELSE IF ( FCUT .EQ. 1000. ) THEN
          FLAG = 'g'
        ELSE
          FLAG = 'x'
        END IF
      ELSE IF ( CURTYP(1:4) .EQ. 'DFT ') THEN
        IF ( FSTP .GT. 0. ) THEN
          FMAX = FSTP
        ELSE
          FMAX = FCUT * 2.5
        END IF
        IF ( FMAX .GE. 2500. ) THEN
          FLAG = 'g'
        ELSE IF ( FMAX .GE. 1250. ) THEN
          FLAG = 'f'
        ELSE IF ( FMAX .GE. 750. ) THEN
          FLAG = 'e'
        ELSE IF ( FMAX .GE. 250. ) THEN
          FLAG = 'd'
        ELSE IF ( FMAX .GE. 150. ) THEN
          FLAG = 'c'
        ELSE IF ( FMAX .GE. 75. ) THEN
          FLAG = 'b'
        ELSE IF ( FMAX .GE. 37.5 ) THEN
          FLAG = 'a'
        ELSE IF ( FMAX .GE. 0. ) THEN
          FLAG = 'x'
        ELSE
          FLAG = '0'
        END IF
      END IF

      RETURN
      END
