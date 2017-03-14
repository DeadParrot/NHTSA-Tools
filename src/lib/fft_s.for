!***********************************************************************
!* FFT Routines:
!*   FFT_S - In-place FFT
!*   FFT_SN - Out-of-place FFT
!*   FFT_SD - Out-of-place FFT that returns DFT only
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************



      SUBROUTINE FFT_S( Y, Y_MAG, Y_PHS, NLP_U, NLP, Y_DFT, NLP_2,
     & DEL, PAD_FAC, M_MIN, IOS )

!***********************************************************************
!* Computes the DFT Magnitude and Phase of a Series
!* . Overwrites NLP and DEL
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************


      ! Arguments ______________________________________________________

      INTEGER NLP_U ! Data array upper index limit

      INTEGER NLP ! Index of last data point

      INTEGER NLP_2 ! Power-of-2 DFT array last point index (>= NLP)

      REAL Y(0:NLP) ! Data array (send Y(0))

      REAL Y_MAG(0:NLP_U) ! DFT magnitude output array (send Y_MAG(0))

      REAL Y_PHS(0:NLP_U) ! DFT phase output array (send Y_PHS(0))

      COMPLEX Y_DFT(0:NLP_2-1) ! DFT output array (send Y_DFT(0))

      REAL DEL ! X-axis step of data (>0)

      REAL PAD_FAC ! Desired minimum length padding factor

      INTEGER M_MIN ! Min power of 2 for DFT length (2**M_MIN <= NLP_2)

      INTEGER IOS ! Status flag returned


      ! Perform the in-place DFT
      CALL FFT_SN( Y, Y_MAG, Y_PHS, NLP_U, NLP, Y_DFT, NLP_2,
     & DEL, PAD_FAC, M_MIN, NLP, DEL, IOS )

      RETURN
      END



      SUBROUTINE FFT_SN( Y, Y_MAG, Y_PHS, NLP_U, NLP, Y_DFT, NLP_2,
     & DEL, PAD_FAC, M_MIN, NLP_OUT, DEL_OUT, IOS )

!***********************************************************************
!* Computes the DFT Magnitude and Phase of a Series
!*
!* Language: Fortran
!*
!* Author: Jeff Marcus
!*         Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************


      ! Arguments ______________________________________________________

      INTEGER NLP_U ! Data array upper index limit

      INTEGER NLP ! Index of last data point

      INTEGER NLP_2 ! Power-of-2 DFT array last point index (>= NLP)

      REAL Y(0:NLP) ! Data array (send Y(0))

      REAL Y_MAG(0:NLP_U) ! DFT magnitude output array (send Y_MAG(0))

      REAL Y_PHS(0:NLP_U) ! DFT phase output array (send Y_PHS(0))

      COMPLEX Y_DFT(0:NLP_2-1) ! DFT output array (send Y_DFT(0))

      REAL DEL ! X-axis step of data (>0)

      REAL PAD_FAC ! Desired minimum length padding factor

      INTEGER M_MIN ! Min power of 2 for DFT length (2**M_MIN <= NLP_2)

      INTEGER NLP_OUT ! Index of last output data point

      REAL DEL_OUT ! X-axis output frequency step of data (>0)

      INTEGER IOS ! Status flag returned


      ! Variables ______________________________________________________

      INTEGER I

      REAL PI, Y_R, Y_I

      COMPLEX Y_D


      ! Compute the DFT
      CALL FFT_SD( Y, NLP, Y_DFT, NLP_2,
     & DEL, PAD_FAC, M_MIN, NLP_OUT, DEL_OUT, IOS )
      IF ( IOS .NE. 0 ) RETURN

      ! Compute magnitude and phase data
      PI = 4.0 * ATAN( 1.0 )
      NLP_OUT = MIN( NLP_OUT, NLP_U )
      DO I = 0, NLP_OUT
        Y_D = Y_DFT(I)
        Y_R = REAL( Y_D )
        Y_I = AIMAG( Y_D )
        Y_MAG(I) = CABS( Y_D )
        IF ( ABS( Y_I ) .LT. 1.E10 * ABS( Y_R ) ) THEN ! Phase in deg
          Y_PHS(I) = ATAN( Y_I / Y_R ) * 180. / PI
        ELSE ! Set phase = 90 degrees
          Y_PHS(I) = 90.
        END IF
      END DO

      RETURN
      END



      SUBROUTINE FFT_SD( Y, NLP, Y_DFT, NLP_2,
     & DEL, PAD_FAC, M_MIN, NLP_OUT, DEL_OUT, IOS )

!***********************************************************************
!* Computes the Complex DFT of a Series
!*
!* Language: Fortran
!*
!* Author: Jeff Marcus
!*         Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************


      ! Arguments ______________________________________________________

      INTEGER NLP ! Index of last data point

      INTEGER NLP_2 ! Power-of-2 DFT array last point index (>= NLP)

      REAL Y(0:NLP) ! Data array (send Y(0))

      COMPLEX Y_DFT(0:NLP_2-1) ! DFT output array (send Y_DFT(0))

      REAL DEL ! X-axis step of data (>0)

      REAL PAD_FAC ! Desired minimum length padding factor

      INTEGER M_MIN ! Min power of 2 for DFT length (2**M_MIN <= NLP_2)

      INTEGER NLP_OUT ! Index of last output data point

      REAL DEL_OUT ! X-axis output frequency step of data (>0)

      INTEGER IOS ! Status flag returned


      ! Variables ______________________________________________________

      INTEGER I, M, M_MAX, NPT, NPT2


      ! Check input fields
      IF ( NLP .LE. 0 ) THEN ! No data error
        IOS = 1
        RETURN
      END IF
      IF ( NLP .GE. NLP_2 ) THEN ! DFT array too small
        IOS = 2
        RETURN
      END IF
      IOS = 0

      ! Set up FFT dimensions
      M_MAX = INT( ( LOG( REAL( NLP_2 ) ) / LOG( 2. ) ) *
     & ( 1. + 2.E-7 ) )
      NPT = NLP + 1
      M = INT(
     & ( ( LOG( NPT * MAX( PAD_FAC, 1. ) ) / LOG( 2. ) ) + 1. ) *
     & ( 1. + 2.E-7 ) )
      M = MIN( MAX( M, M_MIN ), M_MAX )
      NPT2 = 2**M

      ! Initialize complex array and pad to a power of 2 length
      DO I = 0, NLP ! Initialize
        Y_DFT(I) = CMPLX( Y(I), 0. )
      END DO
      DO I = NLP+1, NPT2-1 ! Pad zeros
        Y_DFT(I) = CMPLX( 0., 0. )
      END DO

      ! Compute using FFT
      CALL COMPUTE_FFT( Y_DFT, M )

      ! Prepare output fields
      NLP_OUT = NPT2 / 2 ! Limit to Nyquist frequency
      DEL_OUT = 1.0 / ( DEL * NPT2 ) ! Frequency step

      RETURN
      END



      SUBROUTINE COMPUTE_FFT( A, M )

!***********************************************************************
!* Computes Radix 2, In-Place FFT
!***********************************************************************


      ! Arguments ______________________________________________________

      COMPLEX A(*) ! DFT output array

      INTEGER M ! Power of 2 DFT length


      ! Variables ______________________________________________________

      INTEGER N, NV2, NM1, I, IP, J, K, L, LE, LE1

      REAL PI

      COMPLEX U, W, T


      ! Initializations
      PI = 4. * ATAN( 1. )
      N = 2**M
      NV2 = N / 2
      NM1 = N - 1
      J = 1

      DO I = 1, NM1
        IF ( I .LT. J ) THEN ! Swap A_i and A_j
          T = A(J)
          A(J) = A(I)
          A(I) = T
        END IF
        K = NV2
        DO WHILE ( K .LT. J )
          J = J - K
          K = K / 2
        END DO
        J = J + K
      END DO

      DO L = 1, M
        LE = 2**L
        LE1 = LE / 2
        U = ( 1., 0. )
        W = CMPLX( COS(PI/LE1), SIN(PI/LE1) )
        DO J = 1, LE1
          DO I = J, N, LE
            IP = I + LE1
            T = A(IP) * U
            A(IP) = A(I) - T
            A(I) = A(I) + T
          END DO
          U = U * W
        END DO
      END DO

      RETURN
      END
