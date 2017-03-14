      SUBROUTINE IIRFIL(Y,DX,NPTS,IFL)

      INTEGER NPTS, IFL(5), IP, IPLTMP, I
      REAL DX, Y(NPTS)

!************************************************************************
!* Programmable, n-th order, Butterworth, low-pass, recursive,
!*  infinite impulse response, bidirectional digital filter.
!*  IIRFIL determines which filters (1-,2-,3-, or 4-pole) to
!*  call in order to produce the desired n-pole filter.
!*
!* Arguments:
!*  Y      = Data from channel
!*  DX     = Sampling period  (DELTAt)
!*  NPTS   = Number of data points
!*  IFL(1) = Cutoff frequency (3-dB down point)
!*  IFL(2) = Attenuation (dB) at stop band frequency
!*  IFL(3) = Direction of filtering:[1=Forward, 2=Backward,
!*           3=Phaseless (forward and backward)]
!*  IFL(4) = Stop band frequency (Fsb)
!*  IFL(5) = (not used)
!*
!* Restrictions:  (none)
!*
!* Language: Fortran
!*
!* Author: B. Stevens - Jul-1980
!*
!* Modification History:
!*   Ver/Ed    Date      Modification
!*   ------- ---------  ------------------------------------------------
!*    BMS01  19-Dec-80  Redefined IFILTR for stopband & attenuation
!*    BMS02  02-Feb-81  Rewritten for command driven program
!*    MRN01  23-Apr-85  Added display of filter coefficients
!*
!* Date: 1999/08/20
!************************************************************************
!
      DOUBLE PRECISION C, PI
!-----------------------------------------------------------------------
! These physical constants are defined with sufficient accuracy so
! that they may be declared DOUBLE PRECISION in the user program.
!
      PARAMETER (PI=3.141592653589793)
!  PARAMETER (E=2.718281828459045) !Base of natural log
!  PARAMETER (RTOD=57.2957795131)  !Radians to degrees
!  PARAMETER (G=32.1725)           !Gravity  (ft/sec**2)
!  PARAMETER (GSI=9.80665)         !Gravity  (m/sec**2)
!-----------------------------------------------------------------------
!  CHARACTER IMESG(3)*9
!  SAVE IMESG
!  DATA IMESG / 'FORWARD', 'BACKWARD', 'PHASELESS' /
!
!#######################################################################
!
! Convert from Fsb and attenuation to # of poles
!
      IP = INT( -IFL(2)/(20.0*LOG10(FLOAT(IFL(4))/IFL(1))) + 0.5 )
!
! Check for odd order & phaseless
!
      IF (IFL(3).EQ.3) IP = (IP+1)/2
      IF (IP.LE.0) THEN
        WRITE(*,35) 'Invalid number of poles',IP
   35   FORMAT(1X,10X,A,' : ',I2)
      ENDIF
!
! Now begin filtering
!
      C = DCOS(PI*IFL(1)*DX)/DSIN(PI*IFL(1)*DX)
      GOTO (10,20,30,40,50,60,70,80,90,100,110,120) IP
      GOTO 130

   10 CALL LP1(Y,C,NPTS,IFL)
      GOTO 150
   20 CALL LP2(Y,C,NPTS,IFL)
      GOTO 150
   30 CALL LP3(Y,C,NPTS,IFL)
      GOTO 150
   40 CALL LP4(Y,C,NPTS,IFL)
      GOTO 150
   50 CALL LP3(Y,C,NPTS,IFL)
      GOTO 20
   60 CALL LP3(Y,C,NPTS,IFL)
      GOTO 30
   70 CALL LP4(Y,C,NPTS,IFL)
      GOTO 30
   80 CALL LP4(Y,C,NPTS,IFL)
      GOTO 40
   90 CALL LP3(Y,C,NPTS,IFL)
      GOTO 60
  100 CALL LP4(Y,C,NPTS,IFL)
      GOTO 60
  110 CALL LP4(Y,C,NPTS,IFL)
      GOTO 70
  120 CALL LP4(Y,C,NPTS,IFL)
      GOTO 80
!
! Find # of 4-poles needed to implement specified filter
!
  130 IPLTMP = (IP-13)/4+1 ! IP > 12
      DO 140 I = 1, IPLTMP
        CALL LP4( Y, C, NPTS, IFL )
  140 CONTINUE
!
! Add the basic 9, 10, 11, or 12 pole filter
!
      IPLTMP = (IP-IPLTMP*4)-8
      GOTO (90,100,110,120) IPLTMP
!
! Print filter message and return
!
  150 IF (IFL(3).EQ.3)  IP = IP*2
!  WRITE(*,9000) IFL(1),IP,IMESG(IFL(3)),IFL(4),IFL(2)
!9000 FORMAT(' >Data filtered to'I5' Hz with a'I3' pole, '
!    &     A' filter' / '    Fsb ='I6', Att ='I4' dB')

      RETURN
      END



      SUBROUTINE LP1(Y,C,NPTS,IFL)

      INTEGER NPTS, IFL(5), I
      REAL Y(NPTS)
      DOUBLE PRECISION C

!*----------------------------------------------------------------------
!* 1st order Butterworth filter routine
!* Recursive,low-pass,IIR filter
!*
!* Normalized filter: 1/(1+p)
!*
!*----------------------------------------------------------------------

      DOUBLE PRECISION D,A0,A1,B1,X0,X1,Y0,Y1
!
!************************************************************************
!
   10 D = 1.0/(1.0+C)
      A0 = 1.0*D
      A1 = 1.0*D
      B1 = (1.0-C)*D
      !
      ! Backward filtering only?
      !
      IF (IFL(3).EQ.2)  GOTO 40
      !
      ! Forward filtering
      !
   20 X0 = Y(1)
      Y0 = Y(1)
      DO 30 I = 2, NPTS
        Y1 = Y0
        Y0 = Y(I)
        X1 = X0
        X0 = Y0
        Y0 = A0*X0+A1*X1-B1*Y1
        Y(I) = REAL( Y0 )
   30 CONTINUE
      !
      ! Forward filtering only?
      !
      IF (IFL(3).EQ.1)  RETURN
      !
      ! Backward filtering
      !
   40 X0 = Y(NPTS)
      Y0 = Y(NPTS)
      DO 50 I = (NPTS-1), 1, -1
        Y1 = Y0
        Y0 = Y(I)
        X1 = X0
        X0 = Y0
        Y0 = A0*X0+A1*X1-B1*Y1
        Y(I) = REAL( Y0 )
   50 CONTINUE

      RETURN
      END



      SUBROUTINE LP2(Y,C,NPTS,IFL)

      INTEGER NPTS, IFL(5), I
      REAL Y(NPTS)
      DOUBLE PRECISION C

!*----------------------------------------------------------------------
!* 2nd order Butterworth filter routine
!* Recursive,low-pass,IIR filter
!*
!* Normalized filter: 1/(1+SQRT(2)p+p**2)
!*
!*----------------------------------------------------------------------

      DOUBLE PRECISION D,A0,A1,A2,B1,B2
      DOUBLE PRECISION X0,X1,X2,Y0,Y1,Y2
!
!************************************************************************
!
   10 D = 1.0/(1.0+SQRT(2.0D0)*C+C**2)
      A0 = 1.0*D
      A1 = 2.0*D
      A2 = 1.0*D
      B1 = (2.0-2.0*C**2)*D
      B2 = (1.0D0-SQRT(2.0D0)*C+C**2)*D
      !
      ! Backward filtering only?
      !
      IF (IFL(3).EQ.2)  GOTO 40
      !
      ! Forward filtering
      !
   20 X1 = Y(1)
      X0 = Y(2)
      Y1 = Y(1)
      Y0 = Y(2)
      DO 30 I = 3, NPTS
        Y2 = Y1
        Y1 = Y0
        Y0 = Y(I)
        X2 = X1
        X1 = X0
        X0 = Y0
        Y0 = A0*X0+A1*X1+A2*X2-B1*Y1-B2*Y2
        Y(I) = REAL( Y0 )
   30 CONTINUE
      !
      ! Forward filtering only?
      !
      IF (IFL(3).EQ.1)  RETURN
      !
      ! Backward filtering
      !
   40 X1 = Y(NPTS)
      X0 = Y(NPTS-1)
      Y1 = Y(NPTS)
      Y0 = Y(NPTS-1)
      DO 50 I = (NPTS-2), 1, -1
        Y2 = Y1
        Y1 = Y0
        Y0 = Y(I)
        X2 = X1
        X1 = X0
        X0 = Y0
        Y0 = A0*X0+A1*X1+A2*X2-B1*Y1-B2*Y2
        Y(I) = REAL( Y0 )
   50 CONTINUE

      RETURN
      END



      SUBROUTINE LP3(Y,C,NPTS,IFL)

      INTEGER NPTS, IFL(5), I
      REAL Y(NPTS)
      DOUBLE PRECISION C

!*----------------------------------------------------------------------
!* 3rd order Butterworth filter routine
!* Recursive,low-pass,IIR filter
!*
!* Normalized filter: 1/(1+2p+2p**2+p**3)
!*
!*----------------------------------------------------------------------

      DOUBLE PRECISION D,A0,A1,A2,A3,B1,B2,B3
      DOUBLE PRECISION X0,X1,X2,X3,Y0,Y1,Y2,Y3
!
!************************************************************************
!
   10 D = 1.0/(1.0+2.0*C+2.0*C**2+C**3)
      A0 = 1.0*D
      A1 = 3.0*D
      A2 = 3.0*D
      A3 = 1.0*D
      B1 = (3.0+2.0*C-2.0*C**2-3.0*C**3)*D
      B2 = (3.0-2.0*C-2.0*C**2+3.0*C**3)*D
      B3 = (1.0-2.0*C+2.0*C**2-C**3)*D
      !
      ! Backward filtering only?
      !
      IF (IFL(3).EQ.2)  GOTO 40
      !
      ! Forward filtering
      !
   20 X2 = Y(1)
      X1 = Y(2)
      X0 = Y(3)
      Y2 = Y(1)
      Y1 = Y(2)
      Y0 = Y(3)
      DO 30 I = 4, NPTS
        Y3 = Y2
        Y2 = Y1
        Y1 = Y0
        Y0 = Y(I)
        X3 = X2
        X2 = X1
        X1 = X0
        X0 = Y0
        Y0 = A0*X0+A1*X1+A2*X2+A3*X3-B1*Y1-B2*Y2-B3*Y3
        Y(I) = REAL( Y0 )
   30 CONTINUE
      !
      ! Forward filtering only?
      !
      IF (IFL(3).EQ.1)  RETURN
      !
      ! Backward filtering
      !
   40 X2 = Y(NPTS)
      X1 = Y(NPTS-1)
      X0 = Y(NPTS-2)
      Y2 = Y(NPTS)
      Y1 = Y(NPTS-1)
      Y0 = Y(NPTS-2)
      DO 50 I = (NPTS-3), 1, -1
        Y3 = Y2
        Y2 = Y1
        Y1 = Y0
        Y0 = Y(I)
        X3 = X2
        X2 = X1
        X1 = X0
        X0 = Y0
        Y0 = A0*X0+A1*X1+A2*X2+A3*X3-B1*Y1-B2*Y2-B3*Y3
        Y(I) = REAL( Y0 )
   50 CONTINUE

      RETURN
      END



      SUBROUTINE LP4(Y,C,NPTS,IFL)

      INTEGER NPTS, IFL(5), I
      REAL Y(NPTS)
      DOUBLE PRECISION C

!*----------------------------------------------------------------------
!* 4th order Butterworth filter routine
!* Recursive,low-pass,IIR filter
!*
!* Normalized filter: 1/(1+2.6p+3.4p**2+2.6p**3+p**4)
!*
!*----------------------------------------------------------------------

      DOUBLE PRECISION E1,E2,D,A0,A1,A2,A3,A4
      DOUBLE PRECISION B1,B2,B3,B4,X4,X3,X2,X1,X0
      DOUBLE PRECISION Y0,Y1,Y2,Y3,Y4
!
!************************************************************************
!
   10 E1 = 2.6131259D0
      E2 = 3.4142136D0
      D = 1.0/(1.0+E1*C+E2*C**2+E1*C**3+C**4)
      A0 = 1.0*D
      A1 = 4.0*D
      A2 = 6.0*D
      A3 = 4.0*D
      A4 = 1.0*D
      B1 = (4.0+2.0*E1*C-2.0*E1*C**3-4.0*C**4)*D
      B2 = (6.0-2.0*E2*C**2+6.*C**4)*D
      B3 = (4.0-2.0*E1*C+2.0*E1*C**3-4.0*C**4)*D
      B4 = (1.0-E1*C+E2*C**2-E1*C**3+C**4)*D
      !
      ! Backward filtering only?
      !
      IF (IFL(3).EQ.2)  GOTO 40
      !
      ! Forward filtering
      !
   20 X3 = Y(1)
      X2 = Y(2)
      X1 = Y(3)
      X0 = Y(4)
      Y3 = Y(1)
      Y2 = Y(2)
      Y1 = Y(3)
      Y0 = Y(4)
      DO 30 I = 5, NPTS
        Y4 = Y3
        Y3 = Y2
        Y2 = Y1
        Y1 = Y0
        Y0 = Y(I)
        X4 = X3
        X3 = X2
        X2 = X1
        X1 = X0
        X0 = Y0
        Y0 = A0*X0+A1*X1+A2*X2+A3*X3+A4*X4-B1*Y1-B2*Y2-B3*Y3-B4*Y4
        Y(I) = REAL( Y0 )
   30 CONTINUE
      !
      ! Forward filtering only?
      !
      IF (IFL(3).EQ.1)  RETURN
      !
      ! Backward filtering
      !
   40 X3 = Y(NPTS)
      X2 = Y(NPTS-1)
      X1 = Y(NPTS-2)
      X0 = Y(NPTS-3)
      Y3 = Y(NPTS)
      Y2 = Y(NPTS-1)
      Y1 = Y(NPTS-2)
      Y0 = Y(NPTS-3)
      DO 50 I = (NPTS-4), 1, -1
        Y4 = Y3
        Y3 = Y2
        Y2 = Y1
        Y1 = Y0
        Y0 = Y(I)
        X4 = X3
        X3 = X2
        X2 = X1
        X1 = X0
        X0 = Y0
        Y0 = A0*X0+A1*X1+A2*X2+A3*X3+A4*X4-B1*Y1-B2*Y2-B3*Y3-B4*Y4
        Y(I) = REAL( Y0 )
   50 CONTINUE

      RETURN
      END
