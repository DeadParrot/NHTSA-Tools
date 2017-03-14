!***********************************************************************
!* Integration Routines:
!*   INTEG_S - In-place Y/X-Y integration
!*   INTEG_Y_S - In-place Y integration
!*   INTEG_XY_S - In-place X-Y integration
!*   INTEG_SN - Out-of-place Y/X-Y integration
!*   INTEG_Y_SN - Out-of-place Y integration
!*   INTEG_XY_SN - Out-of-place X-Y integration
!*
!* Note:
!* . In-place: overwrites Y array, type, and units with integral
!* . Out-of-place: writes Z array, type, and units with integral
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/17
!***********************************************************************



      SUBROUTINE INTEG_S( DIMSYS, CHANFORM, X, Y,
     & NFP, NLP, DEL, INIVEL,
     & XTYP, XUNITS, YTYP, YUNITS,
     & ZFLAG, Z0_F, Z0_S, MSG, IOS )

!***********************************************************************
!* In-Place Integration
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/17
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) DIMSYS ! Dimensional system code

      CHARACTER*(*) CHANFORM ! Data channel format (Y or X-Y)

      INTEGER NFP ! Index of first data point

      INTEGER NLP ! Index of last data point

      REAL X(NFP:NLP) ! X-axis data array of X-Y data (send X(NFP))

      REAL Y(NFP:NLP) ! Y-axis data array (send Y(NFP)) -> Integral

      REAL DEL ! X-axis step (delta-x) of Y data (>0)

      REAL INIVEL ! Initial velocity along data axis of motion signals

      CHARACTER*(*) XTYP ! X-axis data type

      CHARACTER*(*) XUNITS ! X-axis data units

      CHARACTER*(*) YTYP ! Y-axis data type -> Integral data type

      CHARACTER*(*) YUNITS ! Y-axis data units -> Integral data units

      CHARACTER ZFLAG ! Flag character for integral data type

      CHARACTER Z0_F ! Initial value handler flag

      DOUBLE PRECISION Z0_S ! Specified initial value

      LOGICAL MSG ! Specifies messages allowed

      INTEGER IOS ! Status flag returned


      ! Perform in-place integration
      CALL INTEG_SN( DIMSYS, CHANFORM, X, Y, Y,
     & NFP, NLP, DEL, INIVEL,
     & XTYP, XUNITS, YTYP, YUNITS, YTYP, YUNITS,
     & ZFLAG, Z0_F, Z0_S, MSG, IOS )

      RETURN
      END



      SUBROUTINE INTEG_Y_S( DIMSYS, Y,
     & NFP, NLP, DEL, INIVEL,
     & XTYP, XUNITS, YTYP, YUNITS,
     & ZFLAG, Z0_F, Z0_S, MSG, IOS )

!***********************************************************************
!* In-Place Integration of Y-Series
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/17
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) DIMSYS ! Dimensional system code

      INTEGER NFP ! Index of first data point

      INTEGER NLP ! Index of last data point

      REAL Y(NFP:NLP) ! Y-axis data array (send Y(NFP)) -> Integral

      REAL DEL ! X-axis step (delta-x) of data (>0)

      REAL INIVEL ! Initial velocity along data axis of motion signals

      CHARACTER*(*) XTYP ! X-axis data type

      CHARACTER*(*) XUNITS ! X-axis data units

      CHARACTER*(*) YTYP ! Y-axis data type -> Integral data type

      CHARACTER*(*) YUNITS ! Y-axis data units -> Integral data units

      CHARACTER ZFLAG ! Flag character for integral data type

      CHARACTER Z0_F ! Initial value handler flag

      DOUBLE PRECISION Z0_S ! Specified initial value

      LOGICAL MSG ! Specifies messages allowed

      INTEGER IOS ! Status flag returned


      ! Perform in-place Y-series integration
      CALL INTEG_Y_SN( DIMSYS, Y, Y,
     & NFP, NLP, DEL, INIVEL,
     & XTYP, XUNITS, YTYP, YUNITS, YTYP, YUNITS,
     & ZFLAG, Z0_F, Z0_S, MSG, IOS )

      RETURN
      END



      SUBROUTINE INTEG_XY_S( DIMSYS, X, Y,
     & NFP, NLP, INIVEL,
     & XTYP, XUNITS, YTYP, YUNITS,
     & ZFLAG, Z0_F, Z0_S, MSG, IOS )

!***********************************************************************
!* In-Place Integration of X-Y Signal
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/17
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) DIMSYS ! Dimensional system code

      INTEGER NFP ! Index of first data point

      INTEGER NLP ! Index of last data point

      REAL X(NFP:NLP) ! X-axis data array (send X(NFP))

      REAL Y(NFP:NLP) ! Y-axis data array (send Y(NFP)) -> Integral

      REAL INIVEL ! Initial velocity along data axis of motion signals

      CHARACTER*(*) XTYP ! X-axis data type

      CHARACTER*(*) XUNITS ! X-axis data units

      CHARACTER*(*) YTYP ! Y-axis data type -> Integral data type

      CHARACTER*(*) YUNITS ! Y-axis data units -> Integral data units

      CHARACTER ZFLAG ! Flag character for integral data type

      CHARACTER Z0_F ! Initial value handler flag

      DOUBLE PRECISION Z0_S ! Specified initial value

      LOGICAL MSG ! Specifies messages allowed

      INTEGER IOS ! Status flag returned


      ! Perform in-place X-Y integration
      CALL INTEG_XY_SN( DIMSYS, X, Y, Y,
     & NFP, NLP, INIVEL,
     & XTYP, XUNITS, YTYP, YUNITS, YTYP, YUNITS,
     & ZFLAG, Z0_F, Z0_S, MSG, IOS )

      RETURN
      END



      SUBROUTINE INTEG_SN( DIMSYS, CHANFORM, X, Y, Z,
     & NFP, NLP, DEL, INIVEL,
     & XTYP, XUNITS, YTYP, YUNITS, ZTYP, ZUNITS,
     & ZFLAG, Z0_F, Z0_S, MSG, IOS )

!***********************************************************************
!* Trapezoid Rule Integration
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/17
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) DIMSYS ! Dimensional system code

      CHARACTER*(*) CHANFORM ! Data channel format (Y or X-Y)

      INTEGER NFP ! Index of first data point

      INTEGER NLP ! Index of last data point

      REAL X(NFP:NLP) ! X-axis data array of X-Y data (send X(NFP))

      REAL Y(NFP:NLP) ! Y-axis data array (send Y(NFP))

      REAL Z(NFP:NLP) ! Integral data array (send Z(NFP) or Y(NFP))

      REAL DEL ! X-axis step (delta-x) of Y data (>0)

      REAL INIVEL ! Initial velocity along data axis of motion signals

      CHARACTER*(*) XTYP ! X-axis data type

      CHARACTER*(*) XUNITS ! X-axis data units

      CHARACTER*(*) YTYP ! Y-axis data type

      CHARACTER*(*) YUNITS ! Y-axis data units

      CHARACTER*(*) ZTYP ! Integral data type (can send YTYP)

      CHARACTER*(*) ZUNITS ! Integral data units (can send YUNITS)

      CHARACTER ZFLAG ! Flag character for integral data type

      CHARACTER Z0_F ! Initial value handler flag

      DOUBLE PRECISION Z0_S ! Specified initial value

      LOGICAL MSG ! Specifies messages allowed

      INTEGER IOS ! Status flag returned


      ! Variables ______________________________________________________

      LOGICAL STANDARD

      INTEGER IOS_R, IOS_W

      DOUBLE PRECISION ZFAC, Z0_D, Z0

      CHARACTER Z0_C*25, PROMPT*58


      ! Functions ______________________________________________________

      INTEGER L_TRIM

      DOUBLE PRECISION DP_TRAN

      EXTERNAL L_TRIM, DP_TRAN


      ! Set up for integration
      CALL INTEG_SETUP( DIMSYS, CHANFORM,
     & XTYP, XUNITS, YTYP, YUNITS, ZTYP, ZUNITS,
     & ZFLAG, ZFAC, STANDARD, MSG, IOS )
      IF ( IOS .NE. 0 ) RETURN

      ! Set default initial value
      IF ( ( XTYP .EQ. 'TIME' ) .AND.
     & ( XUNITS .EQ. 'SECONDS' ) .AND.
     & ( ( ZTYP .EQ. 'VELOCITY' ) .OR.
     & ( ZUNITS .EQ. 'KILOMETERS/HOUR' ) .OR.
     & ( ZUNITS .EQ. 'MILES/HOUR' ) ) ) THEN
        Z0_D = DP_TRAN( INIVEL )
      ELSE
        Z0_D = 0.D0
      END IF

      ! Set initial value
      IF ( Z0_F .EQ. 'V' ) THEN ! Use specified value
        Z0 = Z0_S
      ELSE IF ( Z0_F .EQ. 'P' ) THEN ! Prompt for value
        PROMPT = ' Initial '//ZTYP(:L_TRIM(ZTYP))//' ('//
     &     ZUNITS(:L_TRIM(ZUNITS))//')?'
  101   WRITE( *, '(/A,A,G20.6,A/'' >> '',$)', IOSTAT=IOS_W )
     &     PROMPT, '[',Z0,']'
        READ( *, '(A)', END=201 ) Z0_C
        IF ( Z0_C .NE. ' ' ) THEN
          READ( Z0_C, '(BN,F25.0)', IOSTAT=IOS_R ) Z0
          IF ( IOS_R .NE. 0 ) THEN
            WRITE( *, * ) '*** Invalid initial value'
            GO TO 101
          END IF
        END IF
      ELSE ! Use default value
        Z0 = Z0_D
      END IF

      ! Compute integral
      IF ( CHANFORM .EQ. 'Y' ) THEN
        CALL TRAP_INTEG_Y( Y, Z, NFP, NLP, DP_TRAN( DEL ), Z0, ZFAC )
      ELSE
        CALL TRAP_INTEG_XY( X, Y, Z, NFP, NLP, Z0, ZFAC )
      END IF

      ! Normal return
      IOS = 0
      RETURN

      ! Return forced by EOF
  201 IOS = 1
      RETURN

      END



      SUBROUTINE INTEG_Y_SN( DIMSYS, Y, Z,
     & NFP, NLP, DEL, INIVEL,
     & XTYP, XUNITS, YTYP, YUNITS, ZTYP, ZUNITS,
     & ZFLAG, Z0_F, Z0_S, MSG, IOS )

!***********************************************************************
!* Trapezoid Rule Integration of Y-Series
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/17
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) DIMSYS ! Dimensional system code

      INTEGER NFP ! Index of first data point

      INTEGER NLP ! Index of last data point

      REAL Y(NFP:NLP) ! Y-axis data array (send Y(NFP))

      REAL Z(NFP:NLP) ! Integral data array (send Z(NFP) or Y(NFP))

      REAL DEL ! X-axis step (delta-x) of data (>0)

      REAL INIVEL ! Initial velocity along data axis of motion signals

      CHARACTER*(*) XTYP ! X-axis data type

      CHARACTER*(*) XUNITS ! X-axis data units

      CHARACTER*(*) YTYP ! Y-axis data type

      CHARACTER*(*) YUNITS ! Y-axis data units

      CHARACTER*(*) ZTYP ! Integral data type (can send YTYP)

      CHARACTER*(*) ZUNITS ! Integral data units (can send YUNITS)

      CHARACTER ZFLAG ! Flag character for integral data type

      CHARACTER Z0_F ! Initial value handler flag

      DOUBLE PRECISION Z0_S ! Specified initial value

      LOGICAL MSG ! Specifies messages allowed

      INTEGER IOS ! Status flag returned


      ! Perform Y-series integration
      CALL INTEG_SN( DIMSYS, 'Y', Y, Y, Z,
     & NFP, NLP, DEL, INIVEL,
     & XTYP, XUNITS, YTYP, YUNITS, ZTYP, ZUNITS,
     & ZFLAG, Z0_F, Z0_S, MSG, IOS )

      RETURN
      END



      SUBROUTINE INTEG_XY_SN( DIMSYS, X, Y, Z,
     & NFP, NLP, INIVEL,
     & XTYP, XUNITS, YTYP, YUNITS, ZTYP, ZUNITS,
     & ZFLAG, Z0_F, Z0_S, MSG, IOS )

!***********************************************************************
!* Trapezoid Rule Integration of X-Y Signal
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/17
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) DIMSYS ! Dimensional system code

      INTEGER NFP ! Index of first data point

      INTEGER NLP ! Index of last data point

      REAL X(NFP:NLP) ! X-axis data array (send X(NFP))

      REAL Y(NFP:NLP) ! Y-axis data array (send Y(NFP))

      REAL Z(NFP:NLP) ! Integral data array (send Z(NFP) or Y(NFP))

      REAL INIVEL ! Initial velocity along data axis of motion signals

      CHARACTER*(*) XTYP ! X-axis data type

      CHARACTER*(*) XUNITS ! X-axis data units

      CHARACTER*(*) YTYP ! Y-axis data type

      CHARACTER*(*) YUNITS ! Y-axis data units

      CHARACTER*(*) ZTYP ! Integral data type (can send YTYP)

      CHARACTER*(*) ZUNITS ! Integral data units (can send YUNITS)

      CHARACTER ZFLAG ! Flag character for integral data type

      CHARACTER Z0_F ! Initial value handler flag

      DOUBLE PRECISION Z0_S ! Specified initial value

      LOGICAL MSG ! Specifies messages allowed

      INTEGER IOS ! Status flag returned


      ! Perform X-Y integration
      CALL INTEG_SN( DIMSYS, 'X-Y', X, Y, Z,
     & NFP, NLP, 0., INIVEL,
     & XTYP, XUNITS, YTYP, YUNITS, ZTYP, ZUNITS,
     & ZFLAG, Z0_F, Z0_S, MSG, IOS )

      RETURN
      END



      SUBROUTINE INTEG_SETUP( DIMSYS, CHANFORM,
     & XTYP, XUNITS, YTYP, YUNITS, ZTYP, ZUNITS,
     & ZFLAG, ZFAC, STANDARD, MSG, IOS )

!***********************************************************************
!* Integration Setup
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/17
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) DIMSYS ! Dimensional system code

      CHARACTER*(*) CHANFORM ! Data channel format (Y or X-Y)

      CHARACTER*(*) XTYP ! X-axis data type

      CHARACTER*(*) XUNITS ! X-axis data units

      CHARACTER*(*) YTYP ! Y-axis data type

      CHARACTER*(*) YUNITS ! Y-axis data units

      CHARACTER*(*) ZTYP ! Integral data type (can send YTYP)

      CHARACTER*(*) ZUNITS ! Integral data units (can send YUNITS)

      CHARACTER ZFLAG ! Flag character for integral data type

      DOUBLE PRECISION ZFAC ! Integration units conversion factor

      LOGICAL STANDARD ! Indicates derivative is a standard quantity

      LOGICAL MSG ! Specifies messages allowed

      INTEGER IOS ! Status flag returned


      ! Variables ______________________________________________________

      INTEGER LZ, LM


      ! Initializations
      ZFLAG = ' '
      ZFAC = 1.D0
      STANDARD = .TRUE.

      ! Check file fields
      IOS = 0
      IF ( ( DIMSYS .NE. 'MET' ) .AND. ( DIMSYS .NE. 'SI' ) .AND.
     & ( DIMSYS .NE. 'ENG' ) ) THEN
        IF ( MSG ) WRITE( *, '(3A)' ) ' *** ERROR - ',
     &     'Unsupported dimensional system: ', DIMSYS
        IOS = 1
        RETURN
      ELSE IF ( ( CHANFORM .NE. 'Y' ) .AND. ( CHANFORM .NE. 'X-Y' ) )
     & THEN
        IF ( MSG ) WRITE( *, '(3A)' ) ' *** ERROR - ',
     &     'Unsupported channel format: ', CHANFORM
        IOS = 2
        RETURN
      END IF

      ! Set up for integration
      IF ( ( XTYP .EQ. 'TIME' ) .AND. ( XUNITS .EQ. 'SECONDS' ) ) THEN
        IF ( YUNITS .EQ. 'G''S' ) THEN
          ZFLAG = 'v'
          ZTYP = 'VELOCITY'
          IF ( DIMSYS .EQ. 'MET' ) THEN
            ZUNITS = 'KILOMETERS/HOUR'
            ZFAC = 35.30394D0
          ELSE IF ( DIMSYS .EQ. 'SI' ) THEN
            ZUNITS = 'METERS/SEC'
            ZFAC = 9.80665
          ELSE IF ( DIMSYS .EQ. 'ENG' ) THEN
            ZUNITS = 'MILES/HOUR'
            ZFAC = 21.93675D0
          END IF
        ELSE IF ( YUNITS .EQ. 'METERS/SEC**2' ) THEN
          ZFLAG = 'v'
          ZTYP = 'VELOCITY'
          IF ( DIMSYS .EQ. 'MET' ) THEN
            ZUNITS = 'KILOMETERS/HOUR'
            ZFAC = 3600.D0 / 1.D3
          ELSE IF ( DIMSYS .EQ. 'SI' ) THEN
            ZUNITS = 'METERS/SEC'
            ZFAC = 1.D0
          END IF
        ELSE IF ( YUNITS .EQ. 'KILOMETERS/HOUR' ) THEN
          ZFLAG = 'd'
          ZTYP = 'DISPLACEMENT'
          IF ( DIMSYS .EQ. 'MET' ) THEN
            ZUNITS = 'MILLIMETERS'
            ZFAC = 1.D6 / 3600.D0
          ELSE IF ( DIMSYS .EQ. 'SI' ) THEN
            ZUNITS = 'METERS'
            ZFAC = 1.D3 / 3600.D0
          END IF
        ELSE IF ( YUNITS .EQ. 'METERS/SEC' ) THEN
          ZFLAG = 'd'
          ZTYP = 'DISPLACEMENT'
          IF ( DIMSYS .EQ. 'MET' ) THEN
            ZUNITS = 'MILLIMETERS'
            ZFAC = 1.D3
          ELSE IF ( DIMSYS .EQ. 'SI' ) THEN
            ZUNITS = 'METERS'
            ZFAC = 1.D0
          END IF
        ELSE IF ( YUNITS .EQ. 'MILES/HOUR' ) THEN
          ZFLAG = 'd'
          ZTYP = 'DISPLACEMENT'
          ZUNITS = 'INCHES'
          ZFAC = 17.6D0
        ELSE IF ( YUNITS .EQ. 'FEET/SEC' ) THEN
          ZFLAG = 'd'
          ZTYP = 'DISPLACEMENT'
          ZUNITS = 'INCHES'
          ZFAC = 12.D0
        ELSE IF ( YUNITS .EQ. 'INCHES/SEC' ) THEN
          ZFLAG = 'd'
          ZTYP = 'DISPLACEMENT'
          ZUNITS = 'INCHES'
        ELSE IF ( YUNITS .EQ. 'NEWTONS' ) THEN
          ZFLAG = 'i'
          ZTYP = 'IMPULSE'
          ZUNITS = 'NEWTON-SECONDS'
        ELSE IF ( YUNITS .EQ. 'POUNDS' ) THEN
          ZFLAG = 'i'
          ZTYP = 'IMPULSE'
          ZUNITS = 'POUND-SECONDS'
        ELSE IF ( YUNITS .EQ. 'NEWTON-METERS' ) THEN
          ZFLAG = 'i'
          ZTYP = 'ANGULAR IMPULSE'
          ZUNITS = 'NEWTON-METER-SECONDS'
        ELSE IF ( YUNITS .EQ. 'POUND-FEET' ) THEN
          ZFLAG = 'i'
          ZTYP = 'ANGULAR IMPULSE'
          ZUNITS = 'POUND-FOOT-SECONDS'
        ELSE IF ( YUNITS .EQ. 'DEGREES/SEC**2' ) THEN
          ZFLAG = 'v'
          ZTYP = 'ANGULAR VELOCITY'
          ZUNITS = 'DEGREES/SEC'
        ELSE IF ( YUNITS .EQ. 'DEGREES/SEC' ) THEN
          ZFLAG = 'd'
          ZTYP = 'ANGULAR DISPLACEMENT'
          ZUNITS = 'DEGREES'
        ELSE IF ( YUNITS .EQ. 'RADIANS/SEC**2' ) THEN
          ZFLAG = 'v'
          ZTYP = 'ANGULAR VELOCITY'
          ZUNITS = 'RADIANS/SEC'
        ELSE IF ( YUNITS .EQ. 'RADIANS/SEC' ) THEN
          ZFLAG = 'd'
          ZTYP = 'ANGULAR DISPLACEMENT'
          ZUNITS = 'RADIANS'
        ELSE ! Generic integral wrt time
          ZFLAG = 'g'
          ZTYP = YTYP
          ZTYP(MIN(LEN_TRIM(ZTYP),LEN(ZTYP)-7)+1:) = ' x TIME'
          ZUNITS = YUNITS
          ZUNITS(MIN(LEN_TRIM(ZUNITS),LEN(ZUNITS)-4)+1:) =
     &       '-SECONDS'
          STANDARD = .FALSE.
        END IF
      ELSE IF ( CHANFORM .EQ. 'Y' ) THEN ! Generic integral
        ZFLAG = 'g'
        ZTYP = YTYP
        LZ = LEN( ZTYP )
        LM = ( LEN_TRIM( ZTYP ) + 3 + LEN_TRIM( XTYP ) - LZ ) / 2
        ZTYP(LEN_TRIM(ZTYP)-MAX(LM,0)+1:) = ' x '//XTYP
        ZUNITS = YUNITS
        LZ = LEN( ZUNITS )
        LM = ( LEN_TRIM( ZUNITS ) + 1 + LEN_TRIM( XUNITS ) - LZ ) / 2
        ZUNITS(LEN_TRIM(ZUNITS)-MAX(LM,0)+1:) = '-'//XUNITS
        STANDARD = .FALSE.
      ELSE IF ( CHANFORM .EQ. 'X-Y' ) THEN
        IF ( ( YUNITS .EQ. 'NEWTONS' ) .AND.
     &     ( ( XUNITS .EQ. 'MILLIMETERS') .OR.
     &     ( XUNITS .EQ. 'CENTIMETERS' ) .OR.
     &     ( XUNITS .EQ. 'METERS' ) ) ) THEN
          IF ( XUNITS .EQ. 'MILLIMETERS' ) THEN
            ZFAC = .001D0
          ELSE IF ( XUNITS .EQ. 'CENTIMETERS' ) THEN
            ZFAC = .01D0
          END IF
          ZFLAG = 'e'
          ZTYP = 'ENERGY'
          ZUNITS = 'JOULES'
        ELSE IF ( ( YUNITS .EQ. 'POUNDS' ) .AND.
     &     ( ( XUNITS .EQ. 'INCHES' ) .OR.
     &     ( XUNITS .EQ. 'FEET' ) ) ) THEN
          IF ( XUNITS .EQ. 'INCHES' ) ZFAC = 1.D0 / 12.D0
          ZFLAG = 'e'
          ZTYP = 'ENERGY'
          ZUNITS = 'FOOT-POUNDS'
        ELSE ! Generic integral
          ZFLAG = 'g'
          ZTYP = YTYP
          LZ = LEN( ZTYP )
          LM = ( LEN_TRIM( ZTYP ) + 3 + LEN_TRIM( XTYP ) - LZ ) / 2
          ZTYP(LEN_TRIM(ZTYP)-MAX(LM,0)+1:) = ' x '//XTYP
          ZUNITS = YUNITS
          LZ = LEN( ZUNITS )
          LM = ( LEN_TRIM( ZUNITS ) + 1 + LEN_TRIM( XUNITS ) - LZ )
     &       / 2
          ZUNITS(LEN_TRIM(ZUNITS)-MAX(LM,0)+1:) = '-'//XUNITS
          STANDARD = .FALSE.
        END IF
      END IF

      RETURN
      END



      SUBROUTINE INTEG_TYPE( DIMSYS, CHANFORM,
     & XTYP, XUNITS, YTYP, YUNITS, ZTYP, ZUNITS,
     & ZFLAG, STANDARD, MSG, IOS )

!***********************************************************************
!* Integration Type
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/17
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) DIMSYS ! Dimensional system code

      CHARACTER*(*) CHANFORM ! Data channel format (Y or X-Y)

      CHARACTER*(*) XTYP ! X-axis data type

      CHARACTER*(*) XUNITS ! X-axis data units

      CHARACTER*(*) YTYP ! Y-axis data type

      CHARACTER*(*) YUNITS ! Y-axis data units

      CHARACTER*(*) ZTYP ! Integral data type (can send YTYP)

      CHARACTER*(*) ZUNITS ! Integral data units (can send YUNITS)

      CHARACTER ZFLAG ! Flag character for integral data type

      LOGICAL STANDARD ! Indicates derivative is a standard quantity

      LOGICAL MSG ! Specifies messages allowed

      INTEGER IOS ! Status flag returned


      ! Variables ______________________________________________________

      DOUBLE PRECISION ZFAC


      ! Get setup info (ZFAC not returned)
      CALL INTEG_SETUP( DIMSYS, CHANFORM,
     & XTYP, XUNITS, YTYP, YUNITS, ZTYP, ZUNITS,
     & ZFLAG, ZFAC, STANDARD, MSG, IOS )

      RETURN
      END



      SUBROUTINE TRAP_INTEG( CHANFORM, X, Y, Z, NFP, NLP, DX,
     & Z0, ZFAC )

!***********************************************************************
!* Performs Trapezoid Rule Integration of Y or X-Y Signal
!*
!* Note: Integration starts at index = min(max(0,nfp),nlp)
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/17
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) CHANFORM ! Data channel format (Y or X-Y)

      INTEGER NFP ! Index of first data point

      INTEGER NLP ! Index of last data point

      REAL X(NFP:NLP) ! X-axis data array of X-Y data (send X(NFP))

      REAL Y(NFP:NLP) ! Y-axis data array (send Y(NFP))

      REAL Z(NFP:NLP) ! Integral data array (send Z(NFP) or Y(NFP))

      DOUBLE PRECISION DX ! X-axis step (delta-x) of Y data (>0)

      DOUBLE PRECISION Z0 ! Starting value of integral

      DOUBLE PRECISION ZFAC ! Integration units conversion factor


      ! Perform appropriate type of integration
      IF ( CHANFORM .EQ. 'Y' ) THEN ! Integrate using DX as even step

        CALL TRAP_INTEG_Y( Y, Z, NFP, NLP, DX, Z0, ZFAC )

      ELSE IF ( CHANFORM .EQ. 'X-Y' ) THEN ! Integrate vs. X() array

        CALL TRAP_INTEG_XY( X, Y, Z, NFP, NLP, Z0, ZFAC )

      END IF

      RETURN
      END



      SUBROUTINE TRAP_INTEG_Y( Y, Z, NFP, NLP, DX, Z0, ZFAC )

!***********************************************************************
!* Performs Trapezoid Rule Integration of Y-Series Signal
!*
!* Note: Integration starts at index = min(max(0,nfp),nlp)
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/17
!***********************************************************************


      ! Arguments ______________________________________________________

      INTEGER NFP ! Index of first data point

      INTEGER NLP ! Index of last data point

      REAL Y(NFP:NLP) ! Y-axis data array (send Y(NFP))

      REAL Z(NFP:NLP) ! Integral data array (send Z(NFP) or Y(NFP))

      DOUBLE PRECISION DX ! X-axis step (delta-x) of data (>0)

      DOUBLE PRECISION Z0 ! Starting value of integral

      DOUBLE PRECISION ZFAC ! Integration units conversion factor


      ! Variables ______________________________________________________

      INTEGER N0, I

      REAL Y0

      DOUBLE PRECISION FAC, YM, YI, ZM, ZI


      ! Initializations
      N0 = MIN( MAX( 0, NFP ), NLP )
      Y0 = Y(N0)
      Z(N0) = REAL( Z0 )

      ! Integration factor
      FAC = DX * ZFAC / 2.D0

      ! Post-zero integration
      YM = Y0
      ZM = Z0
      DO I = N0+1, NLP
        YI = Y(I)
        ZI = ZM + FAC * ( YM + YI )
        Z(I) = REAL( ZI )
        YM = YI
        ZM = ZI
      END DO

      ! Pre-zero integration
      YM = Y0
      ZM = Z0
      DO I = N0-1, NFP, -1
        YI = Y(I)
        ZI = ZM - FAC * ( YM + YI )
        Z(I) = REAL( ZI )
        YM = YI
        ZM = ZI
      END DO

      RETURN
      END



      SUBROUTINE TRAP_INTEG_XY( X, Y, Z, NFP, NLP, Z0, ZFAC )

!***********************************************************************
!* Performs Trapezoid Rule Integration of X-Y Signal
!*
!* Note: Integration starts at index = min(max(0,nfp),nlp)
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/17
!***********************************************************************


      ! Arguments ______________________________________________________

      INTEGER NFP ! Index of first data point

      INTEGER NLP ! Index of last data point

      REAL X(NFP:NLP) ! X-axis data array of data (send X(NFP))

      REAL Y(NFP:NLP) ! Y-axis data array (send Y(NFP))

      REAL Z(NFP:NLP) ! Integral data array (send Z(NFP) or Y(NFP))

      DOUBLE PRECISION Z0 ! Starting value of integral

      DOUBLE PRECISION ZFAC ! Integration units conversion factor


      ! Variables ______________________________________________________

      INTEGER N0, I

      REAL Y0

      DOUBLE PRECISION FAC, XM, XI, YM, YI, ZM, ZI


      ! Initializations
      N0 = MIN( MAX( 0, NFP ), NLP )
      Y0 = Y(N0)
      Z(N0) = REAL( Z0 )

      ! Integration factor
      FAC = ZFAC / 2.D0

      ! Post-zero integration
      XM = X(N0)
      YM = Y0
      ZM = Z0
      DO I = N0+1, NLP
        XI = X(I)
        YI = Y(I)
        ZI = ZM + FAC * ( XI - XM ) * ( YM + YI )
        Z(I) = REAL( ZI )
        XM = XI
        YM = YI
        ZM = ZI
      END DO

      ! Pre-zero integration
      XM = X(N0)
      YM = Y0
      ZM = Z0
      DO I = N0-1, NFP, -1
        XI = X(I)
        YI = Y(I)
        ZI = ZM - FAC * ( XM - XI ) * ( YM + YI )
        Z(I) = REAL( ZI )
        XM = XI
        YM = YI
        ZM = ZI
      END DO

      RETURN
      END
