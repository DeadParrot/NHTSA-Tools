      SUBROUTINE FREQ_DEF( U, FMAX, FSTOP, FCUT, FCUT_C, FSTP, FSTP_C )

!***********************************************************************
!* Sets Default Filtering Frequency Values
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'


      ! Arguments ______________________________________________________

      RECORD /UDS_SPEC/  U ! UDS spec object

      DOUBLE PRECISION FMAX ! Max (Nyquist) frequency in data

      DOUBLE PRECISION FSTOP ! Current stop frequency

      DOUBLE PRECISION FCUT ! Default cutoff frequency returned

      CHARACTER*(*) FCUT_C ! Default cutoff frequency returned string

      DOUBLE PRECISION FSTP ! Default stop frequency returned

      CHARACTER*(*) FSTP_C ! Default stop frequency returned string


      ! Set default cutoff and stop frequencies
      IF ( U.SENATT(:5) .EQ. 'HEAD ' ) THEN
        FCUT = MAX( FMAX, FSTOP )
        FCUT_C = 'None'
        FSTP = MAX( FMAX, FSTOP )
        FSTP_C = 'Max'
      ELSE IF ( U.SENATT(:6) .EQ. 'FEMUR ' ) THEN
        FCUT = 1000.D0
        FCUT_C = '1000'
        FSTP = 2500.D0
        FSTP_C = '2500'
      ELSE IF ( ( U.SENATT(:6) .EQ. 'CHEST ' ) .OR.
     & ( U.SENATT(:7) .EQ. 'PELVIS ' ) .OR.
     & ( U.SENATT(:4) .EQ. 'RIB ' ) .OR.
     & ( U.SENATT(:8) .EQ. 'STERNUM ' ) .OR.
     & ( U.SENATT(:6) .EQ. 'SPINE ' ) ) THEN
        FCUT = 300.D0
        FCUT_C = '300'
        FSTP = 750.D0
        FSTP_C = '750'
      ELSE
        FCUT = 100.D0
        FCUT_C = '100'
        FSTP = 250.D0
        FSTP_C = '250'
      END IF

      RETURN
      END
