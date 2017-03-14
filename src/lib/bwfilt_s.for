!***********************************************************************
!* Butterworth Filter Routines:
!*  BWFILT_S - In-place filter (overwrites input array)
!*  BWFILT_SN - Out-of-place filter (outputs to separate array)
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************



      SUBROUTINE BWFILT_S( Y, NFP, NLP, DEL, FCUT )

!***********************************************************************
!* In-Place Second-Order Butterworth Filter of Time Series
!*
!* Function:
!*  Filters data forward and backward with a second order
!*  Butterworth algorithm, giving zero phase shift and -3dB
!*  at the specified cutoff frequency.
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************


      ! Arguments ______________________________________________________

      INTEGER NFP ! Index of first data point

      INTEGER NLP ! Index of last data point

      REAL Y(NFP:NLP) ! Time series data array (send Y(NFP))

      REAL DEL ! Time step (delta-t) (sec) of time series data (>0)

      REAL FCUT ! Cutoff frequency (Hz) desired


      ! Perform the in-place filter
      CALL BWFILT_SN( Y, Y, NFP, NLP, DEL, FCUT )

      RETURN
      END



      SUBROUTINE BWFILT_SN( Y, YF, NFP, NLP, DEL, FCUT )

!***********************************************************************
!* Second-Order Butterworth Filter of Time Series Files
!*
!* Function:
!*  Filters data forward and backward with a second order Butterworth
!*  algorithm giving zero phase shift and -3dB at the specified cutoff
!*  frequency
!*
!* Language: Fortran
!*
!* Author: C. Louie - 1978/11
!*         Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************


      ! Arguments ______________________________________________________

      INTEGER NFP ! Index of first data point

      INTEGER NLP ! Index of last data point

      REAL Y(NFP:NLP) ! Time series data array (send Y(NFP))

      REAL YF(NFP:NLP) ! Filtered time series data array (send YF(NFP))
      ! Can be passed the same array as Y to overwrite the input array

      REAL DEL ! Time step (delta-t) (sec) of time series data (>0)

      REAL FCUT ! Cutoff frequency (Hz) desired


      ! Variables ______________________________________________________

      INTEGER NTP, NTPH, I

      REAL FM6DB, PI, WD, WA, B0, B1, B2, A1, A2,
     & X0, X1, X2, Y0, Y1, Y2, YNFP2


      ! Check positive frequency and time step
      IF ( ( DEL .LE. 0. ) .OR. ( FCUT .LE. 0. ) ) THEN
        WRITE( *, * )
     &     '*** BWFILT Error - Nonpositive time step or cutoff'
        RETURN
      END IF

      ! Check if the sampling rate can support this cutoff frequency
      !
      ! sampling rate is lower than the cutoff frequency - return true
      ! BwFilt goes unstable as fCut approaches 0.5/del
      !
      IF ( FCUT .GT. ( ( 0.5 / DEL ) * 0.775 ) ) THEN
        WRITE( *, * )
     &     '*** BWFILT Error - '//
     &     'Sampling rate is lower than cutoff frequency'
        RETURN
      END IF

      ! Set 6dB attenuation frequency
      FM6DB = FCUT * 1.25

      ! Compute filter coefficients
      PI = 3.141592654
      WD = 2 * PI * FM6DB
      WA = SIN(WD*DEL/2.) / COS(WD*DEL/2.)
      B0 = WA**2 / (1.+SQRT(2.)*WA+WA**2)
      B1 = 2 * B0
      B2 = B0
      A1 = -2.*(WA**2-1.) / (1.+SQRT(2.)*WA+WA**2)
      A2 = (-1.+SQRT(2.)*WA-WA**2) / (1.+SQRT(2.)*WA+WA**2)

      ! Set the number of tail points to use
      NTP = MAX( NINT( .1 / ( FCUT * DEL ) ), 1 )
      NTP = MIN( NTP, NLP - NFP + 1 )
      NTPH = ( NTP / 2 ) + 1

      ! Set up pre-start array - Inverted mirror
      YNFP2 = 2 * Y(NFP)
      X1 = YNFP2 - Y(NFP+NTP)
      X0 = YNFP2 - Y(NFP+NTP-1)
      Y1 = 0.
      DO I = -NTP, -NTPH
        Y1 = Y1 + Y(NFP-I)
      END DO
      Y1 = YNFP2 - ( Y1 / ( NTP - NTPH + 1 ) )
      Y0 = Y1
      DO I = -NTP+2, -1
        X2 = X1
        X1 = X0
        X0 = YNFP2 - Y(NFP-I)
        Y2 = Y1
        Y1 = Y0
        Y0 = B0*X0 + B1*X1 + B2*X2 + A1*Y1 + A2*Y2
      END DO

      ! Filter forward
      DO I = NFP, NLP
        X2 = X1
        X1 = X0
        X0 = Y(I)
        Y2 = Y1
        Y1 = Y0
        Y0 = B0*X0 + B1*X1 + B2*X2 + A1*Y1 + A2*Y2
        YF(I) = Y0
      END DO

      ! Filter backwards
      X1 = 2 * YF(NLP) - YF(NLP-2)
      X0 = 2 * YF(NLP) - YF(NLP-1)
      Y1 = 2 * Y(NLP) - Y(NLP-2)
      Y0 = 2 * Y(NLP) - Y(NLP-1)
      DO I = NLP, NFP, -1
        X2 = X1
        X1 = X0
        X0 = YF(I)
        Y2 = Y1
        Y1 = Y0
        Y0 = B0*X0 + B1*X1 + B2*X2 + A1*Y1 + A2*Y2
        YF(I) = Y0
      END DO

      RETURN
      END
