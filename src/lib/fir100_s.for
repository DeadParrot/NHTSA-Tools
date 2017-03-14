      SUBROUTINE FIR100_S( Y, S, NFP_L, NLP_U, NFP, NLP, DEL,
     & REMOVE_BIAS, IOS )

!***********************************************************************
!* 100 Hz Finite Impulse Response Filter Routine
!*
!* FIR100 performs 100 Hz finite impulse response filtering of a
!* digitized time series.  FIR100 is general purpose in nature,
!* but is used primarily to filter side impacted thoracic and
!* pelvic signals.
!*
!* The program filters a signal using a four step process.  First,
!* FIR100 uses a four-pole, zero-phase Butterworth filter with a
!* 300 Hz cutoff.  This is done to prevent aliasing in the next step,
!* subsampling to a 1600 Hz sampling rate.  After the anti-aliasing
!* filter, the signal is subsampled to exactly 1600 Hz (time
!* between data samples = .000625 seconds). This is the required
!* sampling rate for FIR100 to filter to the correct frequencies.
!* At the user's option, any bias in the signal is next removed.
!* Finally, the finite impulse response filter is applied to the
!* signal.
!*
!* The resulting passband frequency is 100 Hz, the cutoff
!* frequency is 136 Hz, the stopband frequency is 189 Hz, the
!* passband ripple is 0.0225 dB, and the stopband gain is -50 dB.
!*
!* FIR100 started out life as the BIOPROC, and is the functional
!* equivalent of BIOPROC.
!*
!* The BIAS, IIRFIL, and FIRF16 subroutines were coded by M.R.
!* Neale of VRTC.
!*
!* Language: Fortran
!*
!* Author: H. C. Gabler: 1987/08/01 Initial Release
!*         J. H. Marcus, M. R. Neale
!*
!* Date: 1999/08/20
!***********************************************************************


      ! Arguments ______________________________________________________

      INTEGER NFP_L ! Data array lower index limit

      INTEGER NLP_U ! Data array upper index limit

      REAL Y(NFP_L:NLP_U) ! Time series data array -> Filtered array

      REAL S(NFP_L-25:NLP_U+25) ! Scratch array

      INTEGER NFP ! Index of first data point

      INTEGER NLP ! Index of last data point

      REAL DEL ! Time step (sec) of data (>0) -> .000625

      LOGICAL REMOVE_BIAS ! Specifies to remove the signal bias

      INTEGER IOS ! Status flag returned


      ! Variables ______________________________________________________

      INTEGER IFL(5)

      REAL FCUT, FATT, FSTP, XPOLE

      DOUBLE PRECISION DEL_DP, DEL_NEW


      ! Functions ______________________________________________________

      DOUBLE PRECISION DP_TRAN

      EXTERNAL DP_TRAN


      ! 300 Hz Butterworth filter
      FCUT = 300.
      FATT = -40.
      XPOLE = 4.
      FSTP = FCUT*10.**(-FATT/(20.*XPOLE))
      IFL(1) = INT( FCUT )             ! Cutoff Frequency
      IFL(2) = INT( FATT )             ! Stop Freq Attentuation
      IFL(3) = 3                       ! Phaseless Filter
      IFL(4) = INT( FSTP )             ! Stop Frequency
      IFL(5) = 0                       ! Unused
      CALL IIRFIL( Y(NFP), DEL, NLP-NFP+1, IFL )

      ! Subsample/interpolate to 1600 Hz
      DEL_DP = DP_TRAN( DEL )
      DEL_NEW = .000625D0
      CALL INTERP_S( Y, NFP_L, NLP_U, NFP, NLP, DEL_DP, DEL_NEW, IOS )
      IF ( IOS .NE. 0 ) RETURN
      DEL = REAL( DEL_NEW )

      ! Remove bias
      IF ( REMOVE_BIAS ) CALL BIAS( Y(NFP), S(NFP), NLP-NFP+1 )

      ! Finite Impulse Response filter
      CALL FIRF16( Y(NFP), S, NLP-NFP+1, DEL, IOS )

      RETURN
      END
