      SUBROUTINE BAT_SET( ALL_BATCH, NO_BATCH, NEW_HIT, HIT, BATCH )

!***********************************************************************
!* Sets Batch Mode
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************

      ! Headers
      INCLUDE 'file.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      LOGICAL ALL_BATCH ! Specifies BATCH always on

      LOGICAL NO_BATCH ! Specifies BATCH never on

      LOGICAL NEW_HIT ! Specifies a new hit file

      RECORD /FILE/  HIT ! Hit file

      LOGICAL BATCH ! BATCH setting to return


      ! Variables ______________________________________________________

      INTEGER IOS

      CHARACTER RESP


      ! Batch setup
      IF ( ALL_BATCH ) THEN
        BATCH = .TRUE.
      ELSE IF ( NO_BATCH ) THEN
        BATCH = .FALSE.
      ELSE IF ( ( NEW_HIT ) .AND. ( HIT.OPEN ) ) THEN
  101   WRITE( *, '(/A,52X,A/'' >> '',$)' )
     &     ' Batch process?   (Y/N)','[Yes]'
        READ( *, '(A)', IOSTAT=IOS ) RESP
        IF ( IOS .LT. 0 ) THEN ! EOF Entered
          BATCH = .FALSE.
        ELSE IF ( ANY_CHARS( RESP, ' Yy' ) ) THEN
          BATCH = .TRUE.
        ELSE IF ( ANY_CHARS( RESP, 'Nn' ) ) THEN
          BATCH = .FALSE.
        ELSE
          WRITE( *, * ) '*** Unacceptable response'
          GO TO 101
        END IF
      ELSE IF ( .NOT. HIT.OPEN ) THEN
        BATCH = .FALSE.
      END IF

      RETURN
      END
