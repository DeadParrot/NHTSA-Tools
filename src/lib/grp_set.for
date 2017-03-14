      SUBROUTINE GROUP_SET( HIT, NEW_HIT, BATCH, GROUP_MIN,
     & GROUP, NEW_GROUP )

!***********************************************************************
!* Sets Group Mode
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************

      ! Headers
      INCLUDE 'file.fi'


      ! Arguments ______________________________________________________

      RECORD /FILE/  HIT ! Hit file

      LOGICAL NEW_HIT ! Specifies new hit list

      LOGICAL BATCH ! Specifies batch processing

      INTEGER GROUP_MIN ! Minimum number of files for group operation

      LOGICAL GROUP ! Indicates group mode

      LOGICAL NEW_GROUP ! Indicates new group mode started


      ! Group setup
      IF ( NEW_HIT ) THEN ! New file or hit list - Set up Group flags
        GROUP = ( ( BATCH ) .AND. ( HIT.OPEN ) .AND.
     &     ( HIT.N_REC + 1 .GE. GROUP_MIN ) )
        NEW_GROUP = GROUP
      ELSE ! Reset Group flags
        IF ( ( .NOT. BATCH ) .OR. ( .NOT. HIT.OPEN ) )
     &     GROUP = .FALSE.
        NEW_GROUP = .FALSE.
      END IF

      RETURN
      END
