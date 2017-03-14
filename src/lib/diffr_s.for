!***********************************************************************
!* Differentiation Routines:
!*  DIFFR_S - In-place Y/X-Y differentiation
!*  DIFFR_Y_S - In-place Y differentiation
!*  DIFFR_XY_S - In-place X-Y differentiation
!*  DIFFR_SN - Out-of-place Y/X-Y differentiation
!*  DIFFR_Y_SN - Out-of-place Y differentiation
!*  DIFFR_XY_SN - Out-of-place X-Y differentiation
!*
!* Notes:
!* . In-place: overwrites Y array, type, and units with derivative
!* . Out-of-place: writes Z array, type, and units with derivative
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/17
!***********************************************************************



      SUBROUTINE DIFFR_S( DIMSYS, CHANFORM, X, Y,
     & NFP, NLP, DEL, INIVEL,
     & XTYP, XUNITS, YTYP, YUNITS,
     & ZFLAG, Z0_F, Z0_S, MSG, IOS )

!***********************************************************************
!* In-Place Differentiation
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

      REAL Y(NFP:NLP) ! Y-axis data array (send Y(NFP)) -> dY/dX

      REAL DEL ! X-axis step (delta-x) of Y data (>0)

      REAL INIVEL ! Initial velocity along data axis of motion signals

      CHARACTER*(*) XTYP ! X-axis data type

      CHARACTER*(*) XUNITS ! X-axis data units

      CHARACTER*(*) YTYP ! Y-axis data type -> dY/dX data type

      CHARACTER*(*) YUNITS ! Y-axis data units -> dY/dX data units

      CHARACTER ZFLAG ! Flag character for dY/dX data type

      CHARACTER Z0_F ! Initial value handler flag

      DOUBLE PRECISION Z0_S ! Specified initial value

      LOGICAL MSG ! Specifies messages allowed

      INTEGER IOS ! Status flag returned


      ! Perform in-place differentiation
      CALL DIFFR_SN( DIMSYS, CHANFORM, X, Y, Y,
     & NFP, NLP, DEL, INIVEL,
     & XTYP, XUNITS, YTYP, YUNITS, YTYP, YUNITS,
     & ZFLAG, Z0_F, Z0_S, MSG, IOS )

      RETURN
      END



      SUBROUTINE DIFFR_Y_S( DIMSYS, Y,
     & NFP, NLP, DEL, INIVEL,
     & XTYP, XUNITS, YTYP, YUNITS,
     & ZFLAG, Z0_F, Z0_S, MSG, IOS )

!***********************************************************************
!* In-Place Differentiation of Y-Series
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

      REAL Y(NFP:NLP) ! Y-axis data array (send Y(NFP)) -> dY/dX

      REAL DEL ! X-axis step (delta-x) of data (>0)

      REAL INIVEL ! Initial velocity along data axis of motion signals

      CHARACTER*(*) XTYP ! X-axis data type

      CHARACTER*(*) XUNITS ! X-axis data units

      CHARACTER*(*) YTYP ! Y-axis data type -> dY/dX data type

      CHARACTER*(*) YUNITS ! Y-axis data units -> dY/dX data units

      CHARACTER ZFLAG ! Flag character for dY/dX data type

      CHARACTER Z0_F ! Initial value handler flag

      DOUBLE PRECISION Z0_S ! Specified initial value

      LOGICAL MSG ! Specifies messages allowed

      INTEGER IOS ! Status flag returned


      ! Perform in-place Y-series differentiation
      CALL DIFFR_Y_SN( DIMSYS, Y, Y,
     & NFP, NLP, DEL, INIVEL,
     & XTYP, XUNITS, YTYP, YUNITS, YTYP, YUNITS,
     & ZFLAG, Z0_F, Z0_S, MSG, IOS )

      RETURN
      END



      SUBROUTINE DIFFR_XY_S( DIMSYS, X, Y,
     & NFP, NLP, INIVEL,
     & XTYP, XUNITS, YTYP, YUNITS,
     & ZFLAG, Z0_F, Z0_S, MSG, IOS )

!***********************************************************************
!* In-Place Differentiation of X-Y Signal
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

      REAL Y(NFP:NLP) ! Y-axis data array (send Y(NFP)) -> dY/dX

      REAL INIVEL ! Initial velocity along data axis of motion signals

      CHARACTER*(*) XTYP ! X-axis data type

      CHARACTER*(*) XUNITS ! X-axis data units

      CHARACTER*(*) YTYP ! Y-axis data type -> dY/dX data type

      CHARACTER*(*) YUNITS ! Y-axis data units -> dY/dX data units

      CHARACTER ZFLAG ! Flag character for dY/dX data type

      CHARACTER Z0_F ! Initial value handler flag

      DOUBLE PRECISION Z0_S ! Specified initial value

      LOGICAL MSG ! Specifies messages allowed

      INTEGER IOS ! Status flag returned


      ! Perform in-place X-Y differentiation
      CALL DIFFR_XY_SN( DIMSYS, X, Y, Y,
     & NFP, NLP, INIVEL,
     & XTYP, XUNITS, YTYP, YUNITS, YTYP, YUNITS,
     & ZFLAG, Z0_F, Z0_S, MSG, IOS )

      RETURN
      END



      SUBROUTINE DIFFR_SN( DIMSYS, CHANFORM, X, Y, Z,
     & NFP, NLP, DEL, INIVEL,
     & XTYP, XUNITS, YTYP, YUNITS, ZTYP, ZUNITS,
     & ZFLAG, Z0_F, Z0_S, MSG, IOS )

!***********************************************************************
!* Differentiation
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

      REAL Z(NFP:NLP) ! dY/dX data array (send Z(NFP)) (can send Y(NFP))

      REAL DEL ! X-axis step (delta-x) of Y data (>0)

      REAL INIVEL ! Initial velocity along data axis of motion signals

      CHARACTER*(*) XTYP ! X-axis data type

      CHARACTER*(*) XUNITS ! X-axis data units

      CHARACTER*(*) YTYP ! Y-axis data type

      CHARACTER*(*) YUNITS ! Y-axis data units

      CHARACTER*(*) ZTYP ! dY/dX data type (can send YTYP)

      CHARACTER*(*) ZUNITS ! dY/dX data units (can send YUNITS)

      CHARACTER ZFLAG ! Flag character for dY/dX data type

      CHARACTER Z0_F ! Initial value handler flag

      DOUBLE PRECISION Z0_S ! Specified initial value

      LOGICAL MSG ! Specifies messages allowed

      INTEGER IOS ! Status flag returned


      ! Variables ______________________________________________________

      LOGICAL STANDARD, Z0_SPEC

      DOUBLE PRECISION ZFAC, Z0_D


      ! Functions ______________________________________________________

      DOUBLE PRECISION DP_TRAN

      EXTERNAL DP_TRAN


      ! Set up for differentiation
      CALL DIFFR_SETUP( DIMSYS, CHANFORM,
     & XTYP, XUNITS, YTYP, YUNITS, ZTYP, ZUNITS,
     & ZFLAG, ZFAC, STANDARD, MSG, IOS )
      IF ( IOS .NE. 0 ) RETURN

      ! Set default initial value
      IF ( ( XTYP .EQ. 'TIME' ) .AND.
     & ( XUNITS .EQ. 'SECONDS' ) .AND.
     & ( ( ZTYP .EQ. 'VELOCITY' ) .OR.
     & ( ZUNITS .EQ. 'KILOMETERS/HOUR' ) .OR.
     & ( ZUNITS .EQ. 'METERS/SEC' ) .OR.
     & ( ZUNITS .EQ. 'MILES/HOUR' ) ) ) THEN
        Z0_D = DP_TRAN( INIVEL )
        Z0_SPEC = .TRUE.
      ELSE
        Z0_D = 0.D0
        Z0_SPEC = .FALSE.
      END IF

      ! Compute derivative
      IF ( CHANFORM .EQ. 'Y' ) THEN
        CALL DIFFR_Y( Y, Z, NFP, NLP, DP_TRAN( DEL ), ZFAC, IOS )
      ELSE
        CALL DIFFR_XY( X, Y, Z, NFP, NLP, ZFAC, IOS )
      END IF
      IF ( IOS .NE. 0 ) RETURN

      ! Set initial value
      IF ( Z0_F .EQ. 'V' ) THEN ! Use specified value
        Z(0) = REAL( Z0_S )
      ELSE IF ( ( Z0_F .EQ. 'F' ) .AND. Z0_SPEC ) THEN ! Use file value
        Z(0) = REAL( Z0_D )
      END IF

      ! Normal return
      IOS = 0
      RETURN
      END



      SUBROUTINE DIFFR_Y_SN( DIMSYS, Y, Z,
     & NFP, NLP, DEL, INIVEL,
     & XTYP, XUNITS, YTYP, YUNITS, ZTYP, ZUNITS,
     & ZFLAG, Z0_F, Z0_S, MSG, IOS )

!***********************************************************************
!* Differentiation of Y-Series
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

      REAL Z(NFP:NLP) ! dY/dX data array (send Z(NFP)) (can send Y(NFP))

      REAL DEL ! X-axis step (delta-x) of data (>0)

      REAL INIVEL ! Initial velocity along data axis of motion signals

      CHARACTER*(*) XTYP ! X-axis data type

      CHARACTER*(*) XUNITS ! X-axis data units

      CHARACTER*(*) YTYP ! Y-axis data type

      CHARACTER*(*) YUNITS ! Y-axis data units

      CHARACTER*(*) ZTYP ! dY/dX data type (can send YTYP)

      CHARACTER*(*) ZUNITS ! dY/dX data units (can send YUNITS)

      CHARACTER ZFLAG ! Flag character for dY/dX data type

      CHARACTER Z0_F ! Initial value handler flag

      DOUBLE PRECISION Z0_S ! Specified initial value

      LOGICAL MSG ! Specifies messages allowed

      INTEGER IOS ! Status flag returned


      ! Perform Y-series differentiation
      CALL DIFFR_SN( DIMSYS, 'Y', Y, Y, Z,
     & NFP, NLP, DEL, INIVEL,
     & XTYP, XUNITS, YTYP, YUNITS, ZTYP, ZUNITS,
     & ZFLAG, Z0_F, Z0_S, MSG, IOS )

      RETURN
      END



      SUBROUTINE DIFFR_XY_SN( DIMSYS, X, Y, Z,
     & NFP, NLP, INIVEL,
     & XTYP, XUNITS, YTYP, YUNITS, ZTYP, ZUNITS,
     & ZFLAG, Z0_F, Z0_S, MSG, IOS )

!***********************************************************************
!* Differentiation of X-Y Signal
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

      REAL Z(NFP:NLP) ! dY/dX data array (send Z(NFP)) (can send Y(NFP))

      REAL INIVEL ! Initial velocity along data axis of motion signals

      CHARACTER*(*) XTYP ! X-axis data type

      CHARACTER*(*) XUNITS ! X-axis data units

      CHARACTER*(*) YTYP ! Y-axis data type

      CHARACTER*(*) YUNITS ! Y-axis data units

      CHARACTER*(*) ZTYP ! dY/dX data type (can send YTYP)

      CHARACTER*(*) ZUNITS ! dY/dX data units (can send YUNITS)

      CHARACTER ZFLAG ! Flag character for dY/dX data type

      CHARACTER Z0_F ! Initial value handler flag

      DOUBLE PRECISION Z0_S ! Specified initial value

      LOGICAL MSG ! Specifies messages allowed

      INTEGER IOS ! Status flag returned


      ! Perform X-Y differentiation
      CALL DIFFR_SN( DIMSYS, 'X-Y', X, Y, Z,
     & NFP, NLP, 0., INIVEL,
     & XTYP, XUNITS, YTYP, YUNITS, ZTYP, ZUNITS,
     & ZFLAG, Z0_F, Z0_S, MSG, IOS )

      RETURN
      END



      SUBROUTINE DIFFR_SETUP( DIMSYS, CHANFORM,
     & XTYP, XUNITS, YTYP, YUNITS, ZTYP, ZUNITS,
     & ZFLAG, ZFAC, STANDARD, MSG, IOS )

!***********************************************************************
!* Differentiation Setup
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

      CHARACTER*(*) ZTYP ! dY/dX data type (can send YTYP)

      CHARACTER*(*) ZUNITS ! dY/dX data units (can send YUNITS)

      CHARACTER ZFLAG ! Flag character for dY/dX data type

      DOUBLE PRECISION ZFAC ! Differentiation units conversion factor

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

      ! Set up for differentiation
      IF ( ( XTYP .EQ. 'TIME' ) .AND. ( XUNITS .EQ. 'SECONDS' ) ) THEN
        IF ( YUNITS .EQ. 'MILLIMETERS' ) THEN
          ZFLAG = 'v'
          ZTYP = 'VELOCITY'
          IF ( DIMSYS .EQ. 'MET' ) THEN
            ZUNITS = 'KILOMETERS/HOUR'
            ZFAC = 3600.D0 / 1.D6
          ELSE IF ( DIMSYS .EQ. 'SI' ) THEN
            ZUNITS = 'METERS/SEC'
            ZFAC = 1.D-3
          END IF
        ELSE IF ( YUNITS .EQ. 'CENTIMETERS' ) THEN
          ZFLAG = 'v'
          ZTYP = 'VELOCITY'
          IF ( DIMSYS .EQ. 'MET' ) THEN
            ZUNITS = 'KILOMETERS/HOUR'
            ZFAC = 3600.D0 / 1.D5
          ELSE IF ( DIMSYS .EQ. 'SI' ) THEN
            ZUNITS = 'METERS/SEC'
            ZFAC = 1.D-2
          END IF
        ELSE IF ( YUNITS .EQ. 'METERS' ) THEN
          ZFLAG = 'v'
          ZTYP = 'VELOCITY'
          IF ( DIMSYS .EQ. 'MET' ) THEN
            ZUNITS = 'KILOMETERS/HOUR'
            ZFAC = 3600.D0 / 1.D3
          ELSE IF ( DIMSYS .EQ. 'SI' ) THEN
            ZUNITS = 'METERS/SEC'
            ZFAC = 1.D0
          END IF
        ELSE IF ( YUNITS .EQ. 'INCHES' ) THEN
          ZFLAG = 'v'
          ZTYP = 'VELOCITY'
          ZUNITS = 'MILES/HOUR'
          ZFAC = 3600.D0 / 63360.D0
        ELSE IF ( YUNITS .EQ. 'FEET' ) THEN
          ZFLAG = 'v'
          ZTYP = 'VELOCITY'
          ZUNITS = 'MILES/HOUR'
          ZFAC = 3600.D0 / 5280.D0
        ELSE IF ( YUNITS .EQ. 'KILOMETERS/HOUR' ) THEN
          ZFLAG = 'a'
          ZTYP = 'ACCELERATION'
          IF ( DIMSYS .EQ. 'MET' ) THEN
            ZUNITS = 'G''S'
            ZFAC = 1.D3 / ( 3600.D0 * 9.80665D0 )
          ELSE IF ( DIMSYS .EQ. 'SI' ) THEN
            ZUNITS = 'METERS/SEC**2'
            ZFAC = 1.D3 / 3600.D0
          END IF
        ELSE IF ( YUNITS .EQ. 'METERS/SEC' ) THEN
          ZFLAG = 'a'
          ZTYP = 'ACCELERATION'
          IF ( DIMSYS .EQ. 'MET' ) THEN
            ZUNITS = 'G''S'
            ZFAC = 1.D0 / 9.80665D0
          ELSE IF ( DIMSYS .EQ. 'SI' ) THEN
            ZUNITS = 'METERS/SEC**2'
            ZFAC = 1.D0
          END IF
        ELSE IF ( YUNITS .EQ. 'MILES/HOUR' ) THEN
          ZFLAG = 'a'
          ZTYP = 'ACCELERATION'
          ZUNITS = 'G''S'
          ZFAC = 5280.D0 / ( 3600.D0 * 32.1739D0 )
        ELSE IF ( YUNITS .EQ. 'NEWTON-SECONDS' ) THEN
          ZFLAG = 'f'
          ZTYP = 'FORCE'
          ZUNITS = 'NEWTONS'
        ELSE IF ( YUNITS .EQ. 'POUND-SECONDS' ) THEN
          ZFLAG = 'f'
          ZTYP = 'FORCE'
          ZUNITS = 'POUNDS'
        ELSE IF ( YUNITS .EQ. 'NEWTON-METER-SECONDS' ) THEN
          ZFLAG = 't'
          ZTYP = 'TORQUE'
          ZUNITS = 'NEWTON-METERS'
        ELSE IF ( YUNITS .EQ. 'POUND-FOOT-SECONDS' ) THEN
          ZFLAG = 't'
          ZTYP = 'TORQUE'
          ZUNITS = 'POUND-FEET'
        ELSE IF ( YUNITS .EQ. 'DEGREES' ) THEN
          ZFLAG = 'v'
          ZTYP = 'ANGULAR VELOCITY'
          ZUNITS = 'DEGREES/SEC'
        ELSE IF ( YUNITS .EQ. 'DEGREES/SEC' ) THEN
          ZFLAG = 'a'
          ZTYP = 'ANGULAR ACCELERATION'
          ZUNITS = 'DEGREES/SEC**2'
        ELSE IF ( YUNITS .EQ. 'RADIANS' ) THEN
          ZFLAG = 'v'
          ZTYP = 'ANGULAR VELOCITY'
          ZUNITS = 'RADIANS/SEC'
        ELSE IF ( YUNITS .EQ. 'RADIANS/SEC' ) THEN
          ZFLAG = 'a'
          ZTYP = 'ANGULAR ACCELERATION'
          ZUNITS = 'RADIANS/SEC**2'
        ELSE ! Generic derivative wrt time
          ZFLAG = 'g'
          ZTYP = YTYP
          ZTYP(MIN(LEN_TRIM(ZTYP),LEN(ZTYP)-7)+1:) = ' / TIME'
          ZUNITS = YUNITS
          ZUNITS(MIN(LEN_TRIM(ZUNITS),LEN(ZUNITS)-4)+1:) = '/SEC'
          STANDARD = .FALSE.
        END IF
      ELSE IF ( CHANFORM .EQ. 'Y' ) THEN ! Generic derivative
        ZFLAG = 'g'
        ZTYP = YTYP
        LZ = LEN( ZTYP )
        LM = ( LEN_TRIM( ZTYP ) + 3 + LEN_TRIM( XTYP ) - LZ ) / 2
        ZTYP(LEN_TRIM(ZTYP)-MAX(LM,0)+1:) = ' / '//XTYP
        ZUNITS = YUNITS
        LZ = LEN( ZUNITS )
        LM = ( LEN_TRIM( ZUNITS ) + 1 + LEN_TRIM( XUNITS ) - LZ ) / 2
        ZUNITS(LEN_TRIM(ZUNITS)-MAX(LM,0)+1:) = '/'//XUNITS
        STANDARD = .FALSE.
      ELSE IF ( CHANFORM .EQ. 'X-Y' ) THEN
        IF ( ( ( YUNITS .EQ. 'JOULES' ) .OR.
     &     ( YUNITS .EQ. 'METER-NEWTONS' ) ) .AND.
     &     ( ( XUNITS .EQ. 'MILLIMETERS') .OR.
     &     ( XUNITS .EQ. 'CENTIMETERS' ) .OR.
     &     ( XUNITS .EQ. 'METERS' ) ) ) THEN
          IF ( XUNITS .EQ. 'MILLIMETERS' ) THEN
            ZFAC = 1.D3
          ELSE IF ( XUNITS .EQ. 'CENTIMETERS' ) THEN
            ZFAC = 1.D2
          END IF
          ZFLAG = 'f'
          ZTYP = 'FORCE'
          ZUNITS = 'NEWTONS'
        ELSE IF ( ( ( YUNITS .EQ. 'FOOT-POUNDS' ) .OR.
     &     ( YUNITS .EQ. 'INCH-POUNDS' ) ) .AND.
     &     ( ( XUNITS .EQ. 'INCHES' ) .OR.
     &     ( XUNITS .EQ. 'FEET' ) ) ) THEN
          IF ( YUNITS .EQ. 'FOOT-POUNDS' ) THEN
            IF ( XUNITS .EQ. 'INCHES' ) ZFAC = 12.D0
          ELSE
            IF ( XUNITS .EQ. 'FEET' ) ZFAC = 1.D0 / 12.D0
          END IF
          ZFLAG = 'f'
          ZTYP = 'FORCE'
          ZUNITS = 'POUNDS'
        ELSE ! Generic derivative
          ZFLAG = 'g'
          ZTYP = YTYP
          LZ = LEN( ZTYP )
          LM = ( LEN_TRIM( ZTYP ) + 3 + LEN_TRIM( XTYP ) - LZ ) / 2
          ZTYP(LEN_TRIM(ZTYP)-MAX(LM,0)+1:) = ' / '//XTYP
          ZUNITS = YUNITS
          LZ = LEN( ZUNITS )
          LM = ( LEN_TRIM( ZUNITS ) + 1 + LEN_TRIM( XUNITS ) - LZ )
     &       / 2
          ZUNITS(LEN_TRIM(ZUNITS)-MAX(LM,0)+1:) = '/'//XUNITS
          STANDARD = .FALSE.
        END IF
      END IF

      RETURN
      END



      SUBROUTINE DIFFR_TYPE( DIMSYS, CHANFORM,
     & XTYP, XUNITS, YTYP, YUNITS, ZTYP, ZUNITS,
     & ZFLAG, STANDARD, MSG, IOS )

!***********************************************************************
!* Differentiation Type
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

      CHARACTER*(*) ZTYP ! dY/dX data type (can send YTYP)

      CHARACTER*(*) ZUNITS ! dY/dX data units (can send YUNITS)

      CHARACTER ZFLAG ! Flag character for dY/dX data type

      LOGICAL STANDARD ! Indicates derivative is a standard quantity

      LOGICAL MSG ! Specifies messages allowed

      INTEGER IOS ! Status flag returned


      ! Variables ______________________________________________________

      DOUBLE PRECISION ZFAC


      ! Get setup info (ZFAC not returned)
      CALL DIFFR_SETUP( DIMSYS, CHANFORM,
     & XTYP, XUNITS, YTYP, YUNITS, ZTYP, ZUNITS,
     & ZFLAG, ZFAC, STANDARD, MSG, IOS )

      RETURN
      END



      SUBROUTINE DIFFR_RUN( CHANFORM, X, Y, Z, NFP, NLP, DX, ZFAC,
     & IOS )

!***********************************************************************
!* Performs Differentiation of Y or X-Y Signal
!*
!* Note: Differentiation starts at index = min(max(0,nfp),nlp)
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

      REAL Z(NFP:NLP) ! dY/dX data array (send Z(NFP)) (can send Y(NFP))

      DOUBLE PRECISION DX ! X-axis step (delta-x) of Y data (>0)

      DOUBLE PRECISION ZFAC ! Differentiation units conversion factor

      INTEGER IOS ! Status flag returned


      ! Perform appropriate type of differentiation
      IF ( CHANFORM .EQ. 'Y' ) THEN ! Differentiate using DX as step

        CALL DIFFR_Y( Y, Z, NFP, NLP, DX, ZFAC, IOS )

      ELSE IF ( CHANFORM .EQ. 'X-Y' ) THEN ! Differentiate vs. X() array

        CALL DIFFR_XY( X, Y, Z, NFP, NLP, ZFAC, IOS )

      END IF

      RETURN
      END



      SUBROUTINE DIFFR_Y( Y, Z, NFP, NLP, DX, ZFAC, IOS )

!***********************************************************************
!* Performs Differentiation of Y-Series
!*
!* Note: Differentiation starts at index = min(max(0,nfp),nlp)
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

      REAL Z(NFP:NLP) ! dY/dX data array (send Z(NFP)) (can send Y(NFP))

      DOUBLE PRECISION DX ! X-axis step (delta-x) of data (>0)

      DOUBLE PRECISION ZFAC ! Differentiation units conversion factor

      INTEGER IOS ! Status flag returned


      ! Variables ______________________________________________________

      INTEGER I

      DOUBLE PRECISION DXFAC, YM2, YM, YI, YP, YP2, YP3


      ! Check for enough points
      IF ( NLP - NFP .LT. 3 ) THEN
        IOS = -3
        RETURN
      END IF

      ! Compute derivative
      DXFAC = ZFAC / ( 2 * DX )
      I = NFP
      YM = Y(I) ! Suppress may be used uninitialized warning
      YI = Y(I)
      YP = Y(I+1)
      YP2 = Y(I+2)
      YP3 = Y(I+3)
      Z(I) = REAL( ( -7 * YI + 6 * YP + 3 * YP2 - 2 * YP3 ) *
     & DXFAC / 3 )
      YM2 = Y(NLP-3)
      DO I = NFP+1, NLP-1
        YM = YI
        YI = YP
        YP = Y(I+1)
        Z(I) = REAL( ( YP - YM ) * DXFAC )
      END DO
      I = NLP
      Z(I) = REAL( ( 7 * YP - 6 * YI - 3 * YM + 2 * YM2 ) * DXFAC / 3 )

      RETURN
      END



      SUBROUTINE DIFFR_XY( X, Y, Z, NFP, NLP, ZFAC, IOS )

!***********************************************************************
!* Performs Differentiation of X-Y Signal
!*
!* Note: Differentiation starts at index = min(max(0,nfp),nlp)
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

      REAL Z(NFP:NLP) ! dY/dX data array (send Z(NFP)) (can send Y(NFP))

      DOUBLE PRECISION ZFAC ! Differentiation units conversion factor

      INTEGER IOS ! Status flag returned


      ! Variables ______________________________________________________

      INTEGER IOS0, I

      DOUBLE PRECISION DXFAC,
     & XM3, XM2, XM, XI, XP, XP2, XP3,
     & YM3, YM2, YM, YI, YP, YP2, YP3,
     & DYI, DY1, DY2, ALPHA, BIG

      PARAMETER ( BIG = 1.D38 )


      ! Functions ______________________________________________________

      DOUBLE PRECISION DIFFR_XY_VAL, DIFFR_XY_TVAL


      ! Check for enough points
      IF ( NLP - NFP .LT. 3 ) THEN
        IOS = -3
        RETURN
      END IF

      ! Compute derivative
      DXFAC = ZFAC
      I = NFP
      XM3 = X(I)
      XM2 = XM3
      XM = XM2
      XI = XM
      XP = X(I+1)
      XP2 = X(I+2)
      XP3 = X(I+3)
      YM3 = Y(I)
      YM2 = YM3
      YM = YM2
      YI = YM
      YP = Y(I+1)
      YP2 = Y(I+2)
      YP3 = Y(I+3)
      DYI = DIFFR_XY_TVAL( XI, XP, XP2, XP3, YI, YP, YP2, YP3, IOS0 )
     & * DXFAC
      Z(I) = REAL( MAX( MIN( DYI, BIG ), -BIG ) )
      DO I = NFP+1, NLP-1
        XM3 = XM2
        XM2 = XM
        XM = XI
        XI = XP
        XP = X(I+1)
        YM3 = YM2
        YM2 = YM
        YM = YI
        YI = YP
        YP = Y(I+1)
        IF ( I .LE. NLP-2 ) THEN
          XP2 = X(I+2)
          YP2 = Y(I+2)
        END IF
        IF ( I .LE. NLP-3 ) THEN
          XP3 = X(I+3)
          YP3 = Y(I+3)
        END IF
        DYI = DIFFR_XY_VAL( XM, XI, XP, YM, YI, YP, 1, IOS0 ) * DXFAC
        IF ( ( IOS0 .EQ. 3 ) .AND.
     &   ( I .GE. NFP+3 ) .AND. ( I .LE. NLP-3 ) ) THEN ! Local extrema
          DY1 = DIFFR_XY_TVAL( XI, XM, XM2, XM3, YI, YM, YM2, YM3,
     &       IOS0 ) * DXFAC
          DY2 = DIFFR_XY_TVAL( XI, XP, XP2, XP3, YI, YP, YP2, YP3,
     &       IOS0 ) * DXFAC
          ALPHA =
     &       ABS( XI - XM ) / ( ABS( XI - XM ) + ABS( XP - XI ) )
          DYI = ALPHA * DY2 + ( 1.D0 - ALPHA ) * DY1
        ELSE ! Non-extremal
          IF ( ( IOS0 .EQ. 1 ) .AND. ( I .GE. NFP+3 ) ) THEN
            DYI = DIFFR_XY_TVAL( XI, XM, XM2, XM3,
     &         YI, YM, YM2, YM3, IOS0 ) * DXFAC
          ELSE IF ( ( IOS0 .EQ. 2 ) .AND. ( I .LE. NLP-3 ) ) THEN
            DYI = DIFFR_XY_TVAL( XI, XP, XP2, XP3,
     &         YI, YP, YP2, YP3, IOS0 ) * DXFAC
          END IF
        END IF
        Z(I) = REAL( MAX( MIN( DYI, BIG ), -BIG ) )
      END DO
      I = NLP
      DYI = DIFFR_XY_TVAL( XP, XI, XM, XM2, YP, YI, YM, YM2, IOS0 )
     & * DXFAC
      Z(I) = REAL( MAX( MIN( DYI, BIG ), -BIG ) )

      RETURN
      END



      DOUBLE PRECISION FUNCTION DIFFR_XY_VAL( X0, X1, X2, Y0, Y1, Y2,
     & IX, IOS )

!***********************************************************************
!* Computes the Three-Point Derivative Value at a Given X-Y Data Point
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/17
!***********************************************************************


      ! Arguments ______________________________________________________

      DOUBLE PRECISION X0 ! Point 0 X value

      DOUBLE PRECISION X1 ! Point 1 X value

      DOUBLE PRECISION X2 ! Point 2 X value

      DOUBLE PRECISION Y0 ! Point 0 Y value

      DOUBLE PRECISION Y1 ! Point 1 Y value

      DOUBLE PRECISION Y2 ! Point 2 Y value

      INTEGER IX ! Point number for derivative output

      INTEGER IOS ! Status flag returned


      ! Variables ______________________________________________________

      DOUBLE PRECISION X, XT, ALPHA, BIG

      PARAMETER ( BIG = 1.D38 )


      ! Functions ______________________________________________________

      LOGICAL EQUAL_DP

      EXTERNAL EQUAL_DP


      ! Compute the derivative value
      IOS = 0
      DIFFR_XY_VAL = 0.D0
      IF ( IX .EQ. 0 ) THEN
        X = X0
      ELSE IF ( IX .EQ. 1 ) THEN
        X = X1
      ELSE IF ( IX .EQ. 2 ) THEN
        X = X2
      ELSE
        IOS = -5
        RETURN
      END IF
      IF ( ( X0 .EQ. X1 ) .AND. ( X1 .EQ. X2 ) ) THEN ! Vertical segs
        IF ( ( IX .EQ. 1 ) .AND. ( Y0 .EQ. Y2 ) ) THEN
          DIFFR_XY_VAL = 0.D0
          IOS = -3
        ELSE IF ( ( IX .EQ. 0 ) .AND. ( Y0 .EQ. Y1 ) ) THEN
          DIFFR_XY_VAL = 0.D0
          IOS = -3
        ELSE IF ( ( IX .EQ. 2 ) .AND. ( Y1 .EQ. Y2 ) ) THEN
          DIFFR_XY_VAL = 0.D0
          IOS = -3
        ELSE
          DIFFR_XY_VAL = SIGN( BIG, ( Y2 - Y0 ) )
          IOS = -4
        END IF
      ELSE IF ( X0 .EQ. X1 ) THEN ! Use second segment slope
        DIFFR_XY_VAL = ( Y2 - Y1 ) / ( X2 - X1 )
        IOS = 2
      ELSE IF ( X1 .EQ. X2 ) THEN ! Use first segment slope
        DIFFR_XY_VAL = ( Y1 - Y0 ) / ( X1 - X0 )
        IOS = 1
      ELSE ! Non-degenerate segments
        ALPHA = ABS( X1 - X0 ) / ( ABS( X1 - X0 ) + ABS( X2 - X1 ) )
        IF ( ( ALPHA .LT. .25D0 ) .AND.
     &     ( EQUAL_DP( X1, X0, 1.D-5 ) ) ) THEN ! Precision
          ! Use second segment slope
          DIFFR_XY_VAL = ( Y2 - Y1 ) / ( X2 - X1 )
          IOS = 2
        ELSE IF ( ( ALPHA .GT. .75D0 ) .AND.
     &     ( EQUAL_DP( X1, X2, 1.D-5 ) ) ) THEN ! Precision
          ! Use first segment slope
          DIFFR_XY_VAL = ( Y1 - Y0 ) / ( X1 - X0 )
          IOS = 1
        ELSE IF ( ( X1 .GT. MAX( X0, X2 ) ) .OR.
     &     ( X1 .LT. MIN( X0, X2 ) ) ) THEN ! X1 is local extrema
          IF ( IX .EQ. 1 ) THEN ! Use wtd avg of 2-pt formulas
            DIFFR_XY_VAL = ALPHA * ( Y2 - Y1 ) / ( X2 - X1 ) +
     &         ( 1.D0 - ALPHA ) * ( Y1 - Y0 ) / ( X1 - X0 )
            IOS = 3
          ELSE IF ( IX .EQ. 0 ) THEN ! Use first segment slope
            DIFFR_XY_VAL = ( Y1 - Y0 ) / ( X1 - X0 )
            IOS = 1
          ELSE IF ( IX .EQ. 2 ) THEN ! Use second segment slope
            DIFFR_XY_VAL = ( Y2 - Y1 ) / ( X2 - X1 )
            IOS = 2
          END IF
        ELSE ! Monotonic X's - Use 3-pt formula
          IF ( ( ALPHA .LE. .5D0 ) .AND.
     &       ( EQUAL_DP( X1, X0, 1.D-5 ) ) ) THEN ! Precision flag
            IOS = 3
          ELSE IF ( ( ALPHA .GT. .5D0 ) .AND.
     &       ( EQUAL_DP( X1, X2, 1.D-5 ) ) ) THEN ! Precision flag
            IOS = 3
          END IF
          XT = 2 * X
          DIFFR_XY_VAL =
     &       Y0 * ( XT - X1 - X2 ) / ( ( X0 - X1 ) * ( X0 - X2 ) ) +
     &       Y1 * ( XT - X0 - X2 ) / ( ( X1 - X0 ) * ( X1 - X2 ) ) +
     &       Y2 * ( XT - X0 - X1 ) / ( ( X2 - X0 ) * ( X2 - X1 ) )
        END IF
      END IF

      RETURN
      END



      DOUBLE PRECISION FUNCTION DIFFR_XY_TVAL( X0, X1, X2, X3,
     & Y0, Y1, Y2, Y3, IOS )

!***********************************************************************
!* Computes the One-Sided Derivative Value at a Given X-Y Data Point
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/17
!***********************************************************************


      ! Arguments ______________________________________________________

      DOUBLE PRECISION X0 ! Point 0 X value

      DOUBLE PRECISION X1 ! Point 1 X value

      DOUBLE PRECISION X2 ! Point 2 X value

      DOUBLE PRECISION X3 ! Point 3 X value

      DOUBLE PRECISION Y0 ! Point 0 Y value

      DOUBLE PRECISION Y1 ! Point 1 Y value

      DOUBLE PRECISION Y2 ! Point 2 Y value

      DOUBLE PRECISION Y3 ! Point 3 Y value

      INTEGER IOS ! Status flag returned


      ! Variables ______________________________________________________

      INTEGER IOS0, IOS1, IOS2

      DOUBLE PRECISION DY0, DY1, DY2, ALPHA


      ! Functions ______________________________________________________

      DOUBLE PRECISION DIFFR_XY_VAL


      ! Compute tail derivative at (X0,Y0)
      IOS = 0
      DY0 = DIFFR_XY_VAL( X0, X1, X2, Y0, Y1, Y2, 0, IOS0 )
      DY1 = DIFFR_XY_VAL( X0, X1, X2, Y0, Y1, Y2, 1, IOS1 )
      DY2 = DIFFR_XY_VAL( X1, X2, X3, Y1, Y2, Y3, 1, IOS2 )
      IF ( ( IOS1 .EQ. 0 ) .AND. ( IOS2 .EQ. 0 ) ) THEN ! Use wtd avg
        ALPHA = ( X1 - X0 ) / ( X2 - X1 )
        DIFFR_XY_TVAL =
     &     ( DY0 + 2 * ( DY1 + ALPHA * ( DY1 - DY2 ) ) ) / 3.D0
      ELSE
        DIFFR_XY_TVAL = DY0
      END IF

      RETURN
      END
