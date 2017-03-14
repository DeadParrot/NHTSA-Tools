      CHARACTER FUNCTION FN_TYPE( TYPE )

!***********************************************************************
!* Sets Data Type Character for File Name Flag
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) TYPE ! Data type


      ! Variables ______________________________________________________

      INTEGER IB


      ! Functions ______________________________________________________

      CHARACTER DNCASE

      EXTERNAL DNCASE


      ! Set data type character
      IB = INDEX( TYPE, ' ' )
      IF ( ( IB .EQ. 0 ) .OR. ( IB .GT. LEN_TRIM( TYPE ) ) ) THEN
        ! Single word
        FN_TYPE = DNCASE( TYPE(1:1) )
      ELSE IF ( ( TYPE(:IB) .EQ. 'ANGULAR ' ) .OR.
     & ( TYPE(:IB) .EQ. 'ABSOLUTE ' ) .OR.
     & ( TYPE(:IB) .EQ. 'GAUGE ' ) ) THEN
        ! Use first letter of second word
        FN_TYPE = DNCASE( TYPE(IB+1:IB+1) )
      ELSE
        ! Use first letter
        FN_TYPE = DNCASE( TYPE(1:1) )
      END IF

      RETURN
      END
