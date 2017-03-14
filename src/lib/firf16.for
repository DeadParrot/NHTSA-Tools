      SUBROUTINE FIRF16( Y, YPT, NP, DX, ISTAT )

!************************************************************************
!* This routine filters the data in array Y using a finite impulse
!*  response filter.  The passband frequency is the current sampling
!*  frequency divided by 16, the cutoff frequency is 1.36 times the
!*  passband, the stopband frequency is 1.89 times the passband, the
!*  passband ripple is 0.0225 dB, and the stopband gain is -50 dB.  The
!*  array Y will contain the same number of points on output as when
!*  input.  NFILT is the number of filter coefficients.
!*
!* Y(I) = Summation of H(K)*(Y(I-K)+Y(I+K)) for K = 1, NFILT
!*
!* Arguments:
!*  Y     = data to be filtered
!*  YPT   = scratch array (must be dimensioned NP+50)
!*  NP    = number of data points in Y
!*  DX    = time increment between points in Y
!*  ISTAT = status code  [0 = no error, -1 = no filtering]
!*
!* Written By:  M.R.Neale Dec-1980
!*       from HSRI Thoracic Injury Project (DOT-HS-4-00921)
!*
!* Modification History:
!*   Ver/Ed    Date      Modification
!*   ------- ---------  ------------------------------------------------
!*    MRN01  24-Jun-81  Changed filter type to specific code for this
!*    HCG01  10-Jul-87  Converted for use on the HQ VAX 11/780.
!*
!* Language: Fortran
!*
!* Author: M.R.Neale
!*
!* Date: 1999/08/20
!************************************************************************


      ! Arguments ____________________________________________________

      INTEGER NP ! Number of data points

      REAL Y(NP) ! Data array

      REAL YPT(NP+50) ! Scratch array

      REAL DX ! Time step (delta-t) (sec) in time series (>0)

      INTEGER ISTAT ! Status flag returned


      ! Variables ____________________________________________________

      INTEGER NFILT, I, K, NL, NR

      PARAMETER ( NFILT = 25 )

      REAL H(NFILT), PIVOT

      DOUBLE PRECISION HYSUM

      SAVE H

      DATA H /
     & 0.00209983, 0.00129339, 0.00065168,-0.00085462,-0.00290368,
     & -0.00474660,-0.00539656,-0.00400504,-0.00031044, 0.00502427,
     & 0.01036730, 0.01347790, 0.01223700, 0.00555978,-0.00587902,
     & -0.01924700,-0.03001660,-0.03298210,-0.02377640,-0.00039844,
     & 0.03574880, 0.07971110, 0.12385300, 0.15960801, 0.17961000 /

!
!#######################################################################


      ! Check frequency (assumes 100 Hz) vs. sampling rate
      IF ( ( DX .EQ. 0. ) .OR. ( .5/DX .LE. 100. ) ) THEN
        WRITE( *, * )
     &     '*** ERROR - Filter cutoff freq must be < (sample rate) / 2'
        ISTAT = -1
        RETURN
      END IF

      ! Transfer data leaving room for reqd padding at beginning & end
      DO I = 1, NP
        YPT(NFILT+I) = Y(I)
      END DO

      ! Pad beginning
      PIVOT = 2.0 * YPT(NFILT+1)
      DO I = 1, NFILT
        YPT(NFILT+1-I) = PIVOT - YPT(NFILT+I)
      END DO

      ! Pad end
      PIVOT = 2.0 * YPT(NP+NFILT)
      DO I = 1, NFILT
        YPT(NP+NFILT+I) = PIVOT - YPT(NP+NFILT+1-I)
      END DO

      ! Now, do the actual filtering
      DO I = 1, NP
        NL = I - 1
        NR = I + 2 * NFILT
        HYSUM = 0.
        DO K = 1, NFILT
          HYSUM = HYSUM + H(K) * ( YPT(NL+K) + YPT(NR-K) )
        END DO
        Y(I) = REAL( HYSUM )
      END DO
      ISTAT = 0

      RETURN
      END
