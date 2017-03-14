      INTEGER FUNCTION REC_LEN( NUM_BYTES )

!***********************************************************************
!* Sets the RECL Value for Unformatted OPEN
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************

      ! Headers
      INCLUDE 'platform.fi'

      INTEGER NUM_BYTES


      ! Set the RECL value
      IF ( RECL_USIZE .EQ. 1 ) THEN ! Use size in bytes
        REC_LEN = NUM_BYTES
      ELSE ! Round up to next unit size
        REC_LEN = ( NUM_BYTES + RECL_USIZE - 1 ) / RECL_USIZE
      END IF

      RETURN
      END
