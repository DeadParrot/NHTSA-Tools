      CHARACTER*(*) FUNCTION LJUST( STRING )

!***********************************************************************
!* Left Justifies a String
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) STRING ! String


      ! Variables ______________________________________________________

      INTEGER I, LS

      CHARACTER TAB

      PARAMETER ( TAB = CHAR(9) )


      ! Left justify string
      LJUST = ' '
      IF ( STRING .EQ. ' ' ) RETURN
      LS = LEN_TRIM( STRING )
      I = 1
      DO WHILE ( ( I .LE. LS ) .AND.
     & ( ( STRING(I:I) .EQ. ' ' ) .OR.
     & ( STRING(I:I) .EQ. CHAR(0) ) .OR.
     & ( STRING(I:I) .EQ. TAB ) ) )
        I = I + 1
      END DO
      LJUST = STRING(I:)

      RETURN
      END



      CHARACTER*(*) FUNCTION RJUST( STRING )

!***********************************************************************
!* Right Justifies a String
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) STRING ! String


      ! Variables ______________________________________________________

      INTEGER I, IS

      CHARACTER TAB

      PARAMETER ( TAB = CHAR(9) )


      ! Right justify string
      RJUST = ' '
      IF ( STRING .EQ. ' ' ) RETURN
      I = LEN( STRING )
      DO WHILE ( ( I .GT. 0 ) .AND.
     & ( ( STRING(I:I) .EQ. ' ' ) .OR.
     & ( STRING(I:I) .EQ. CHAR(0) ) .OR.
     & ( STRING(I:I) .EQ. TAB ) ) )
        I = I - 1
      END DO
      IS = MAX( LEN( RJUST ) - I + 1, 1 )
      RJUST(IS:) = STRING

      RETURN
      END



      CHARACTER*(*) FUNCTION COMPRESS( STRING )

!***********************************************************************
!* Compresses Out Blanks and Left Justifies a String
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) STRING ! String


      ! Variables ______________________________________________________

      INTEGER I, IC, LC, LS

      CHARACTER TAB

      PARAMETER ( TAB = CHAR(9) )


      ! Left justify string
      COMPRESS = ' '
      IF ( STRING .EQ. ' ' ) RETURN
      LS = LEN_TRIM( STRING )
      I = 1
      DO WHILE ( ( I .LE. LS ) .AND.
     & ( ( STRING(I:I) .EQ. ' ' ) .OR.
     & ( STRING(I:I) .EQ. CHAR(0) ) .OR.
     & ( STRING(I:I) .EQ. TAB ) ) )
        I = I + 1
      END DO

      ! Compress string
      LC = LEN( COMPRESS )
      IC = 1
      DO WHILE ( ( I .LE. LS ) .AND. ( IC .LE. LC ) )
        IF ( STRING(I:I) .NE. ' ' ) THEN
          COMPRESS(IC:IC) = STRING(I:I)
          IC = IC + 1
        END IF
        I = I + 1
      END DO

      RETURN
      END
