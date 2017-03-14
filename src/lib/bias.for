      SUBROUTINE BIAS( Y, YY, NP )

!************************************************************************
!* Subroutine to remove DC bias from a data channel using the UMTRI
!* algorithm.  The bias is found here by first transferring
!* all points before the absolute peak value into a scratch array
!* YY sorted into descending order.  Then the pair of points whose
!* difference is smallest and who are separated by IP/2 points is
!* found.  The median value in this range is assumed to be the bias.
!*
!* Arguments:
!*  Y  = data array
!*  YY = scratch array
!*  NP = number of points in Y
!*
!* Language: Fortran
!*
!* Written By:  M.R.Neale
!*
!* Modification History:
!*   Ver/Ed    Date      Modification
!*   ------- ---------  ------------------------------------------------
!*    MRN01  11-Jun-81  Added bias removed message
!*    MRN02  06-Jul-87  Correct search for smallest difference (ABS)
!*    HCG01  17-Jul-87  Increase size of YY array to 11001 real elem.
!*
!* Date: 1999/08/20
!************************************************************************


      ! Arguments ____________________________________________________

      INTEGER NP ! Number of data points

      REAL Y(NP) ! Data array

      REAL YY(NP) ! Scratch array


      ! Variables ____________________________________________________

      INTEGER I, IP, IW, J, K

      REAL PEAK, YI, Z, TEST, D


      ! Search for absolute peak value
      PEAK = ABS( Y(1) )
      IP = 1
      DO I = 2, NP
        IF ( ABS( Y(I) ) .GT. PEAK ) THEN
          PEAK = ABS( Y(I) )
          IP = I
        END IF
      END DO

      ! Sort points prior to peak into descending order
      YY(1) = Y(1)
      DO I = 2, IP
        YI = Y(I)
        DO J = 1, I-1
          IF ( YI .GT. YY(J) ) THEN
            DO K = I, J+1, -1
              YY(K) = YY(K-1)
            END DO
            YY(J) = Y(I)
            GO TO 101
          END IF
        END DO
        YY(I) = Y(I)
  101   CONTINUE
      END DO

      ! Search for pair of points separated by IP/2 pts and whose
      ! difference is smallest of all like pairs.  Note that the
      ! last (smallest) occurrence is saved instead of the first
      ! such occurrence.
      IW = IP/2
      TEST = ABS( YY(1) - YY(IW+1) )
      J = 1
      DO I = 1, IP-IW
        D = ABS( YY(I) - YY(IW+I) )
        IF ( D .LE. TEST ) THEN
          TEST = D
          J = I
        END IF
      END DO

      ! Finally, remove the bias
      Z = YY(J+(IW+1)/2)
      DO I = 1, NP
        Y(I) = Y(I) - Z
      END DO

      RETURN
      END
