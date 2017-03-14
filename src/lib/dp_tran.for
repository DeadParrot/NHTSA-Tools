      DOUBLE PRECISION FUNCTION DP_TRAN( SPNUM )

!***********************************************************************
!* Translates a Single Precision Number into a Double Precision Number
!* Getting Full Double Precision for Apparently "Rounded" Values
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/10/17
!***********************************************************************


      ! Arguments ______________________________________________________

      REAL SPNUM ! Single precision input value


      ! Variables ______________________________________________________

      INTEGER IOS, IE, L10, LDIG, I

      REAL SPLOG

      CHARACTER DP_C*20, DIG*2


      ! Check for zero case
      IF ( SPNUM .EQ. 0. ) THEN
        DP_TRAN = 0.D0
        RETURN
      END IF

      ! Translate SP number to get DP precision
      WRITE( DP_C, '(E20.6)', IOSTAT=IOS ) SPNUM
      IE = INDEX( DP_C, 'E' )
      IF ( DP_C(IE-2:IE-1) .EQ. '00' ) THEN ! Assume rounded value
        SPLOG = LOG10( ABS( SPNUM ) )
        L10 = INT( ABS( SPLOG ) ) + 1
        LDIG = INT( SPLOG + L10 ) - L10
        IF ( ( LDIG .GE. -13 ) .AND. ( LDIG .LE. 16 ) ) THEN
          READ( DP_C, '(BN,F20.0)', IOSTAT=IOS ) DP_TRAN
          IF ( IOS .NE. 0 ) THEN
            DP_TRAN = SPNUM
            RETURN
          END IF
          WRITE( DIG, '(I2)', IOSTAT=IOS ) MAX( 3-LDIG, 0 )
          WRITE( DP_C, '(F20.'//DIG//')', IOSTAT=IOS ) DP_TRAN
          IF ( LDIG .GT. 3 ) THEN ! Zero out past 4 signif. digits
            DO I = 23-LDIG, 19
              DP_C(I:I) = '0'
            END DO
          END IF
        END IF
        READ( DP_C, '(BN,F20.0)', IOSTAT=IOS ) DP_TRAN
        IF ( IOS .NE. 0 ) DP_TRAN = SPNUM
      ELSE ! Straight assignment
        DP_TRAN = SPNUM
      END IF

      RETURN
      END
