      SUBROUTINE DSN_CONV( RNUM, DATA_TYPE, UNITS, DIM_I, DIM_O,
     & NUM_C )

!***********************************************************************
!* Converts a Real Number's Dimensional System
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/26
!***********************************************************************

      ! Headers
      INCLUDE 'platform.fi'


      ! Arguments ______________________________________________________

      REAL RNUM ! Real value

      CHARACTER*(*) DATA_TYPE ! Data type

      CHARACTER*(*) UNITS ! Units (blank => use standard units)

      CHARACTER*(*) DIM_I ! Input dimensional system

      CHARACTER*(*) DIM_O ! Output dimensional system

      CHARACTER*(*) NUM_C ! Current number format of RNUM


      ! Variables ______________________________________________________

      DOUBLE PRECISION DSFAC

      CHARACTER UNITS_C*20


      ! Functions ______________________________________________________

      CHARACTER DS_UNITS*20

      EXTERNAL DS_UNITS


      ! Check if conversion required
      IF ( DIM_I .EQ. DIM_O ) RETURN

      ! Get units and conversion factor
      UNITS_C = UNITS
      IF ( UNITS_C .EQ. ' ' ) UNITS_C = DS_UNITS( DATA_TYPE, DIM_I )
      CALL DSU_CONV( DATA_TYPE, UNITS_C, DIM_I, DIM_O, DSFAC )
      IF ( DSFAC .EQ. 1.D0 ) RETURN

      ! Set to local floating point format if necessary
      IF ( NUM_C .NE. NUMFORM_P )
     & CALL FPN_CONV( RNUM, NUM_C, NUMFORM_P )

      ! Apply conversion
      RNUM = REAL( RNUM * DSFAC )

      ! Reset floating point format if necessary
      IF ( NUM_C .NE. NUMFORM_P )
     & CALL FPN_CONV( RNUM, NUMFORM_P, NUM_C )

      RETURN
      END



      SUBROUTINE DSA_CONV( RAR, NAR, DATA_TYPE, UNITS, DIM_I, DIM_O,
     & NUM_C )

!***********************************************************************
!* Converts a Real Array's Dimensional System
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/26
!***********************************************************************

      ! Headers
      INCLUDE 'platform.fi'


      ! Arguments ______________________________________________________

      REAL RAR(*) ! Real array

      INTEGER NAR ! Real array dimension

      CHARACTER*(*) DATA_TYPE ! Data type

      CHARACTER*(*) UNITS ! Units (blank => use standard units)

      CHARACTER*(*) DIM_I ! Input dimensional system

      CHARACTER*(*) DIM_O ! Output dimensional system

      CHARACTER*(*) NUM_C ! Current number format of RAR


      ! Variables ______________________________________________________

      INTEGER I

      DOUBLE PRECISION DSFAC

      CHARACTER UNITS_C*20


      ! Functions ______________________________________________________

      CHARACTER DS_UNITS*20

      EXTERNAL DS_UNITS


      ! Check if conversion required
      IF ( DIM_I .EQ. DIM_O ) RETURN

      ! Get units and conversion factor
      UNITS_C = UNITS
      IF ( UNITS_C .EQ. ' ' ) UNITS_C = DS_UNITS( DATA_TYPE, DIM_I )
      CALL DSU_CONV( DATA_TYPE, UNITS_C, DIM_I, DIM_O, DSFAC )
      IF ( DSFAC .EQ. 1.D0 ) RETURN

      ! Set to local floating point format if necessary
      IF ( NUM_C .NE. NUMFORM_P )
     & CALL FPA_CONV( RAR, NAR, NUM_C, NUMFORM_P )

      ! Convert the array
      DO I = 1, NAR
        RAR(I) = REAL( RAR(I) * DSFAC )
      END DO

      ! Reset floating point format if necessary
      IF ( NUM_C .NE. NUMFORM_P )
     & CALL FPA_CONV( RAR, NAR, NUMFORM_P, NUM_C )

      RETURN
      END



      SUBROUTINE DS_RD_CONV( RAR, NAR, UNITS, NUM_C )

!***********************************************************************
!* Converts a Real Array's Units from Radians-Based to Degrees-Based
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/26
!***********************************************************************

      ! Headers
      INCLUDE 'dimsys.fi'
      INCLUDE 'platform.fi'


      ! Arguments ______________________________________________________

      REAL RAR(*) ! Real array

      INTEGER NAR ! Real array dimension

      CHARACTER*(*) UNITS ! Units (blank => use standard units)

      CHARACTER*(*) NUM_C ! Current number format of RAR


      ! Variables ______________________________________________________

      INTEGER I


      ! Check if conversion required
      IF ( ( UNITS .NE. 'RADIANS' ) .AND.
     & ( UNITS(1:8) .NE. 'RADIANS/' ) ) RETURN

      ! Set to local floating point format if necessary
      IF ( NUM_C .NE. NUMFORM_P )
     & CALL FPA_CONV( RAR, NAR, NUM_C, NUMFORM_P )

      ! Convert the array
      DO I = 1, NAR
        RAR(I) = REAL( RAR(I) * ANGDISP_CON_RD )
      END DO
      UNITS(1:7) = 'DEGREES'

      ! Reset floating point format if necessary
      IF ( NUM_C .NE. NUMFORM_P )
     & CALL FPA_CONV( RAR, NAR, NUMFORM_P, NUM_C )

      RETURN
      END



      SUBROUTINE DS_DR_CONV( RAR, NAR, UNITS, NUM_C )

!***********************************************************************
!* Converts a Real Array's Units from Degrees-Based to Radians-Based
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/26
!***********************************************************************

      ! Headers
      INCLUDE 'dimsys.fi'
      INCLUDE 'platform.fi'


      ! Arguments ______________________________________________________

      REAL RAR(*) ! Real array

      INTEGER NAR ! Real array dimension

      CHARACTER*(*) UNITS ! Units (blank => use standard units)

      CHARACTER*(*) NUM_C ! Current number format of RAR


      ! Variables ______________________________________________________

      INTEGER I


      ! Check if conversion required
      IF ( ( UNITS .NE. 'DEGREES' ) .AND.
     & ( UNITS(1:8) .NE. 'DEGREES/' ) ) RETURN

      ! Set to local floating point format if necessary
      IF ( NUM_C .NE. NUMFORM_P )
     & CALL FPA_CONV( RAR, NAR, NUM_C, NUMFORM_P )

      ! Convert the array
      DO I = 1, NAR
        RAR(I) = REAL( RAR(I) * ANGDISP_CON_DR )
      END DO
      UNITS(1:7) = 'RADIANS'

      ! Reset floating point format if necessary
      IF ( NUM_C .NE. NUMFORM_P )
     & CALL FPA_CONV( RAR, NAR, NUMFORM_P, NUM_C )

      RETURN
      END



      SUBROUTINE DSU_CONV( DATA_TYPE, UNITS, DIM_I, DIM_O, DSFAC )

!***********************************************************************
!* Converts Units and Sets Conversion Factor
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/26
!***********************************************************************

      ! Headers
      INCLUDE 'dimsys.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) DATA_TYPE ! Data type

      CHARACTER*(*) UNITS ! Units (gets converted)

      CHARACTER*(*) DIM_I ! Input dimensional system

      CHARACTER*(*) DIM_O ! Output dimensional system

      DOUBLE PRECISION DSFAC ! Dimensional system conversion factor


      ! Variables ______________________________________________________

      INTEGER IP

      CHARACTER DATA_TYPE_U*20, UNITS_TMP*20


      ! Initialization
      CALL STR_UP( UNITS )

      ! Update nonstandard units
      IP = INDEX( UNITS, '(2)' )
      DO WHILE ( IP .GT. 0 )
        UNITS(IP:IP+2) = '**2'
        IP = INDEX( UNITS, '(2)' )
      END DO
      IP = INDEX( UNITS, '(3)' )
      DO WHILE ( IP .GT. 0 )
        UNITS(IP:IP+2) = '**3'
        IP = INDEX( UNITS, '(3)' )
      END DO
      IP = INDEX( UNITS, '(4)' )
      DO WHILE ( IP .GT. 0 )
        UNITS(IP:IP+2) = '**4'
        IP = INDEX( UNITS, '(4)' )
      END DO
      IF ( UNITS .EQ. 'DEG' ) THEN
        UNITS = 'DEGREES'
      ELSE IF ( UNITS .EQ. 'DEG/SEC' ) THEN
        UNITS = 'DEGREES/SEC'
      ELSE IF ( UNITS .EQ. 'DEG/SEC**2' ) THEN
        UNITS = 'DEGREES/SEC**2'
      ELSE IF ( UNITS .EQ. 'RAD' ) THEN
        UNITS = 'RADIANS'
      ELSE IF ( UNITS .EQ. 'RAD/SEC' ) THEN
        UNITS = 'RADIANS/SEC'
      ELSE IF ( UNITS .EQ. 'RAD/SEC**2' ) THEN
        UNITS = 'RADIANS/SEC**2'
      ELSE IF ( UNITS .EQ. 'POUNDS-FEET' ) THEN
        UNITS = 'POUND-FEET'
      ELSE IF ( UNITS .EQ. 'MICROMETER/METER' ) THEN
        UNITS = 'MICROMET/MET'
      ELSE IF ( UNITS .EQ. 'MICROMM/MM' ) THEN
        UNITS = 'MICROMET/MET'
      ELSE IF ( UNITS .EQ. 'MICROIN/INCH' ) THEN
        UNITS = 'MICROIN/IN'
      ELSE IF ( UNITS(1:14) .EQ. 'POUNDS/INCH**2' ) THEN
        UNITS_TMP = UNITS
        UNITS = 'POUNDS/IN**2'//UNITS_TMP(15:)
      ELSE IF ( UNITS .EQ. 'RECIPROCAL MMS' ) THEN
        UNITS = 'RECIPROCAL MM'
      ELSE IF ( UNITS .EQ. 'RECIPROCAL CM' ) THEN
        UNITS = 'RECIPROCAL CMS'
      ELSE IF ( UNITS .EQ. 'RECIPROCAL METE' ) THEN
        UNITS = 'RECIPROCAL MET'
      ELSE IF ( UNITS .EQ. 'RECIPROCAL METERS' ) THEN
        UNITS = 'RECIPROCAL MET'
      ELSE IF ( UNITS .EQ. 'RECIPROCAL IN' ) THEN
        UNITS = 'RECIPROCAL INS'
      ELSE IF ( UNITS .EQ. 'RECIPROCAL INCHES' ) THEN
        UNITS = 'RECIPROCAL INS'
      END IF

      ! Set converted units and conversion factor
      DSFAC = 1.D0
      CALL STR_UPCASE( DATA_TYPE_U, DATA_TYPE )
      IF ( UNITS .EQ. 'SECONDS' ) THEN
        RETURN ! No conversion required
      ELSE IF ( UNITS .EQ. 'MILLISECONDS' ) THEN
        UNITS = 'SECONDS'
        DSFAC = 1.D-3
      ELSE IF ( UNITS .EQ. 'DEGREES' ) THEN
        RETURN ! No conversion required
      ELSE IF ( UNITS .EQ. 'DEGREES/SEC' ) THEN
        RETURN ! No conversion required
      ELSE IF ( UNITS .EQ. 'DEGREES/SEC**2' ) THEN
        RETURN ! No conversion required
      ELSE IF ( UNITS .EQ. 'RADIANS' ) THEN
        RETURN ! No conversion required
      ELSE IF ( UNITS .EQ. 'RADIANS/SEC' ) THEN
        RETURN ! No conversion required
      ELSE IF ( UNITS .EQ. 'RADIANS/SEC**2' ) THEN
        RETURN ! No conversion required
      ELSE IF ( UNITS .EQ. 'PERCENT STRAIN' ) THEN
        IF ( DIM_O .EQ. 'MET' ) THEN
          UNITS = 'MICROMET/MET'
        ELSE IF ( DIM_O .EQ. 'SI' ) THEN
          UNITS = 'MICROMET/MET'
        ELSE IF ( DIM_O .EQ. 'ENG' ) THEN
          UNITS = 'MICROIN/IN'
        END IF
        DSFAC = 1.D1
      ELSE IF ( ( DIM_I .EQ. 'MET' ) .AND. ( DIM_O .EQ. 'MET' ) ) THEN
        ! Update to standard Metric units
        IF ( UNITS .EQ. 'METERS' ) THEN
          UNITS = 'MILLIMETERS'
          DSFAC = 1.D3
        ELSE IF ( UNITS .EQ. 'CENTIMETERS' ) THEN
          UNITS = 'MILLIMETERS'
          DSFAC = 1.D1
        ELSE IF ( UNITS .EQ. 'RECIPROCAL MET' ) THEN
          UNITS = 'RECIPROCAL MM'
          DSFAC = 1.D-3
        ELSE IF ( UNITS .EQ. 'RECIPROCAL CMS' ) THEN
          UNITS = 'RECIPROCAL MM'
          DSFAC = 1.D-1
        ELSE IF ( UNITS .EQ. 'METERS**2' ) THEN
          UNITS = 'MILLIMETERS**2'
          DSFAC = 1.D3**2
        ELSE IF ( UNITS .EQ. 'CENTIMETERS**2' ) THEN
          UNITS = 'MILLIMETERS**2'
          DSFAC = 1.D1**2
        ELSE IF ( UNITS .EQ. 'METERS**4' ) THEN
          UNITS = 'MILLIMETERS**4'
          DSFAC = 1.D3**4
        ELSE IF ( UNITS .EQ. 'CENTIMETERS**4' ) THEN
          UNITS = 'MILLIMETERS**4'
          DSFAC = 1.D1**4
        ELSE IF ( UNITS .EQ. 'METERS/SEC**2' ) THEN
          UNITS = 'G''S'
          DSFAC = ACC_CON_SM
        ELSE IF ( UNITS .EQ. 'NEWTON-METERS' ) THEN
          IF ( DATA_TYPE_U .EQ. 'ENERGY' ) THEN ! Nonstandard units
            UNITS = 'JOULES'
          END IF
        ELSE IF ( UNITS .EQ. 'NEWTON-MILLIMETERS' ) THEN
          IF ( DATA_TYPE_U .EQ. 'ENERGY' ) THEN ! Nonstandard units
            UNITS = 'JOULES'
            DSFAC = 1.D-3
          ELSE ! Assume torque
            UNITS = 'NEWTON-METERS'
            DSFAC = 1.D-3
          END IF
        ELSE IF ( UNITS .EQ. 'METER-NEWTONS' ) THEN
          IF ( DATA_TYPE_U .EQ. 'TORQUE' ) THEN ! Nonstandard units
            UNITS = 'NEWTON-METERS'
          ELSE ! Assume energy
            UNITS = 'JOULES'
          END IF
        ELSE IF ( UNITS .EQ. 'MILLIMETER-NEWTONS' ) THEN
          IF ( DATA_TYPE_U .EQ. 'TORQUE' ) THEN ! Nonstandard units
            UNITS = 'NEWTON-METERS'
            DSFAC = 1.D-3
          ELSE ! Assume energy
            UNITS = 'JOULES'
            DSFAC = 1.D-3
          END IF
        END IF
      ELSE IF ( ( DIM_I .EQ. 'SI' ) .AND. ( DIM_O .EQ. 'SI' ) ) THEN
        ! Update to standard SI units
        IF ( UNITS .EQ. 'MILLIMETERS' ) THEN
          UNITS = 'METERS'
          DSFAC = 1.D-3
        ELSE IF ( UNITS .EQ. 'CENTIMETERS' ) THEN
          UNITS = 'METERS'
          DSFAC = 1.D-2
        ELSE IF ( UNITS .EQ. 'RECIPROCAL MM' ) THEN
          UNITS = 'RECIPROCAL MET'
          DSFAC = 1.D3
        ELSE IF ( UNITS .EQ. 'RECIPROCAL CMS' ) THEN
          UNITS = 'RECIPROCAL MET'
          DSFAC = 1.D2
        ELSE IF ( UNITS .EQ. 'MILLIMETERS**2' ) THEN
          UNITS = 'METERS**2'
          DSFAC = 1.D-3**2
        ELSE IF ( UNITS .EQ. 'CENTIMETERS**2' ) THEN
          UNITS = 'METERS**2'
          DSFAC = 1.D-2**2
        ELSE IF ( UNITS .EQ. 'MILLIMETERS**4' ) THEN
          UNITS = 'METERS**4'
          DSFAC = 1.D-3**4
        ELSE IF ( UNITS .EQ. 'CENTIMETERS**4' ) THEN
          UNITS = 'METERS**4'
          DSFAC = 1.D-2**4
        ELSE IF ( UNITS .EQ. 'G''S' ) THEN
          UNITS = 'METERS/SEC**2'
          DSFAC = ACC_CON_MS
        ELSE IF ( UNITS .EQ. 'NEWTON-METERS' ) THEN
          IF ( DATA_TYPE_U .EQ. 'ENERGY' ) THEN ! Nonstandard units
            UNITS = 'JOULES'
          END IF
        ELSE IF ( UNITS .EQ. 'NEWTON-MILLIMETERS' ) THEN
          IF ( DATA_TYPE_U .EQ. 'ENERGY' ) THEN ! Nonstandard units
            UNITS = 'JOULES'
            DSFAC = 1.D-3
          ELSE ! Assume torque
            UNITS = 'NEWTON-METERS'
            DSFAC = 1.D-3
          END IF
        ELSE IF ( UNITS .EQ. 'METER-NEWTONS' ) THEN
          IF ( DATA_TYPE_U .EQ. 'TORQUE' ) THEN ! Nonstandard units
            UNITS = 'NEWTON-METERS'
          ELSE ! Assume energy
            UNITS = 'JOULES'
          END IF
        ELSE IF ( UNITS .EQ. 'MILLIMETER-NEWTONS' ) THEN
          IF ( DATA_TYPE_U .EQ. 'TORQUE' ) THEN ! Nonstandard units
            UNITS = 'NEWTON-METERS'
            DSFAC = 1.D-3
          ELSE ! Assume energy
            UNITS = 'JOULES'
            DSFAC = 1.D-3
          END IF
        END IF
      ELSE IF ( ( DIM_I .EQ. 'ENG' ) .AND. ( DIM_O .EQ. 'ENG' ) ) THEN
        ! Update to standard English units
        IF ( UNITS .EQ. 'FEET' ) THEN
          UNITS = 'INCHES'
          DSFAC = 12.D0
        ELSE IF ( UNITS .EQ. 'FEET**2' ) THEN
          UNITS = 'INCHES**2'
          DSFAC = 12.D0**2
        ELSE IF ( UNITS .EQ. 'FEET**4' ) THEN
          UNITS = 'INCHES**4'
          DSFAC = 12.D0**4
        ELSE IF ( UNITS .EQ. 'FOOT-POUNDS' ) THEN
          IF ( DATA_TYPE_U .EQ. 'TORQUE' ) THEN ! Nonstandard units
            UNITS = 'POUND-FEET'
          END IF
        ELSE IF ( UNITS .EQ. 'INCH-POUNDS' ) THEN
          IF ( DATA_TYPE_U .EQ. 'TORQUE' ) THEN ! Nonstandard units
            UNITS = 'POUND-FEET'
            DSFAC = 1.D0 / 12.D0
          ELSE ! Assume energy
            UNITS = 'FOOT-POUNDS'
            DSFAC = 1.D0 / 12.D0
          END IF
        ELSE IF ( UNITS .EQ. 'POUND-FEET' ) THEN
          IF ( DATA_TYPE_U .EQ. 'ENERGY' ) THEN ! Nonstandard units
            UNITS = 'FOOT-POUNDS'
          END IF
        ELSE IF ( UNITS .EQ. 'POUND-INCHES' ) THEN
          IF ( DATA_TYPE_U .EQ. 'ENERGY' ) THEN ! Nonstandard units
            UNITS = 'FOOT-POUNDS'
            DSFAC = 1.D0 / 12.D0
          ELSE ! Assume torque
            UNITS = 'POUND-FEET'
            DSFAC = 1.D0 / 12.D0
          END IF
        ELSE IF ( UNITS .EQ. 'POUND-INCH-SECONDS' ) THEN
          UNITS = 'POUND-FOOT-SECONDS'
          DSFAC = 1.D0 / 12.D0
        END IF
      ELSE IF ( ( DIM_I .EQ. 'MET' ) .AND. ( DIM_O .EQ. 'SI' ) ) THEN
        ! Metric to SI
        IF ( UNITS .EQ. 'MILLIMETERS' ) THEN
          UNITS = 'METERS'
          DSFAC = DISP_CON_MS
        ELSE IF ( UNITS .EQ. 'CENTIMETERS' ) THEN
          UNITS = 'METERS'
          DSFAC = 1.D-2
        ELSE IF ( UNITS .EQ. 'RECIPROCAL MM' ) THEN
          UNITS = 'RECIPROCAL MET'
          DSFAC = 1.D0 / DISP_CON_MS
        ELSE IF ( UNITS .EQ. 'RECIPROCAL CMS' ) THEN
          UNITS = 'RECIPROCAL MET'
          DSFAC = 1.D2
        ELSE IF ( UNITS .EQ. 'MILLIMETERS**2' ) THEN
          UNITS = 'METERS**2'
          DSFAC = ( DISP_CON_MS )**2
        ELSE IF ( UNITS .EQ. 'CENTIMETERS**2' ) THEN
          UNITS = 'METERS**2'
          DSFAC = ( DISP_CON_MS * 1.D1 )**2
        ELSE IF ( UNITS .EQ. 'MILLIMETERS**4' ) THEN
          UNITS = 'METERS**4'
          DSFAC = ( DISP_CON_MS )**4
        ELSE IF ( UNITS .EQ. 'CENTIMETERS**4' ) THEN
          UNITS = 'METERS**4'
          DSFAC = ( DISP_CON_MS * 1.D1 )**4
        ELSE IF ( UNITS .EQ. 'KILOMETERS/HOUR' ) THEN
          UNITS = 'METERS/SEC'
          DSFAC = VEL_CON_MS
        ELSE IF ( UNITS .EQ. 'G''S' ) THEN
          UNITS = 'METERS/SEC**2'
          DSFAC = ACC_CON_MS
        ELSE IF ( UNITS .EQ. 'NEWTON-METERS' ) THEN
          IF ( DATA_TYPE_U .EQ. 'ENERGY' ) THEN ! Nonstandard units
            UNITS = 'JOULES'
          END IF
        ELSE IF ( UNITS .EQ. 'NEWTON-MILLIMETERS' ) THEN
          IF ( DATA_TYPE_U .EQ. 'ENERGY' ) THEN ! Nonstandard units
            UNITS = 'JOULES'
            DSFAC = 1.D-3
          ELSE ! Assume torque
            UNITS = 'NEWTON-METERS'
            DSFAC = 1.D-3
          END IF
        ELSE IF ( UNITS .EQ. 'METER-NEWTONS' ) THEN
          IF ( DATA_TYPE_U .EQ. 'TORQUE' ) THEN ! Nonstandard units
            UNITS = 'NEWTON-METERS'
          ELSE ! Assume energy
            UNITS = 'JOULES'
          END IF
        ELSE IF ( UNITS .EQ. 'MILLIMETER-NEWTONS' ) THEN
          IF ( DATA_TYPE_U .EQ. 'TORQUE' ) THEN ! Nonstandard units
            UNITS = 'NEWTON-METERS'
            DSFAC = 1.D-3
          ELSE ! Assume energy
            UNITS = 'JOULES'
            DSFAC = 1.D-3
          END IF
        ELSE IF ( UNITS .EQ. 'KILOPASCALS' ) THEN
          UNITS = 'PASCALS'
          DSFAC = PRESS_CON_MS
        ELSE IF ( UNITS .EQ. 'KILOPASCALS AB' ) THEN
          UNITS = 'PASCALS AB'
          DSFAC = PRESS_CON_MS
        ELSE IF ( UNITS .EQ. 'KILOPASCALS GA' ) THEN
          UNITS = 'PASCALS GA'
          DSFAC = PRESS_CON_MS
        END IF
      ELSE IF ( ( DIM_I .EQ. 'SI' ) .AND. ( DIM_O .EQ. 'MET' ) ) THEN
        ! SI to Metric
        IF ( UNITS .EQ. 'METERS' ) THEN
          UNITS = 'MILLIMETERS'
          DSFAC = DISP_CON_SM
        ELSE IF ( UNITS .EQ. 'CENTIMETERS' ) THEN
          UNITS = 'MILLIMETERS'
          DSFAC = 1.D1
        ELSE IF ( UNITS .EQ. 'RECIPROCAL MET' ) THEN
          UNITS = 'RECIPROCAL MM'
          DSFAC = 1.D0 / DISP_CON_SM
        ELSE IF ( UNITS .EQ. 'RECIPROCAL CMS' ) THEN
          UNITS = 'RECIPROCAL MM'
          DSFAC = 1.D-1
        ELSE IF ( UNITS .EQ. 'METERS**2' ) THEN
          UNITS = 'MILLIMETERS**2'
          DSFAC = ( DISP_CON_SM )**2
        ELSE IF ( UNITS .EQ. 'CENTIMETERS**2' ) THEN
          UNITS = 'MILLIMETERS**2'
          DSFAC = ( 1.D1 )**2
        ELSE IF ( UNITS .EQ. 'METERS**4' ) THEN
          UNITS = 'MILLIMETERS**4'
          DSFAC = ( DISP_CON_SM )**4
        ELSE IF ( UNITS .EQ. 'CENTIMETERS**4' ) THEN
          UNITS = 'MILLIMETERS**4'
          DSFAC = ( 1.D1 )**4
        ELSE IF ( UNITS .EQ. 'METERS/SEC' ) THEN
          UNITS = 'KILOMETERS/HOUR'
          DSFAC = VEL_CON_SM
        ELSE IF ( UNITS .EQ. 'METERS/SEC**2' ) THEN
          UNITS = 'G''S'
          DSFAC = ACC_CON_SM
        ELSE IF ( UNITS .EQ. 'NEWTON-METERS' ) THEN
          IF ( DATA_TYPE_U .EQ. 'ENERGY' ) THEN ! Nonstandard units
            UNITS = 'JOULES'
          END IF
        ELSE IF ( UNITS .EQ. 'NEWTON-MILLIMETERS' ) THEN
          IF ( DATA_TYPE_U .EQ. 'ENERGY' ) THEN ! Nonstandard units
            UNITS = 'JOULES'
            DSFAC = 1.D-3
          ELSE ! Assume torque
            UNITS = 'NEWTON-METERS'
            DSFAC = 1.D-3
          END IF
        ELSE IF ( UNITS .EQ. 'METER-NEWTONS' ) THEN
          IF ( DATA_TYPE_U .EQ. 'TORQUE' ) THEN ! Nonstandard units
            UNITS = 'NEWTON-METERS'
          ELSE ! Assume energy
            UNITS = 'JOULES'
          END IF
        ELSE IF ( UNITS .EQ. 'MILLIMETER-NEWTONS' ) THEN
          IF ( DATA_TYPE_U .EQ. 'TORQUE' ) THEN ! Nonstandard units
            UNITS = 'NEWTON-METERS'
            DSFAC = 1.D-3
          ELSE ! Assume energy
            UNITS = 'JOULES'
            DSFAC = 1.D-3
          END IF
        ELSE IF ( UNITS .EQ. 'PASCALS' ) THEN
          UNITS = 'KILOPASCALS'
          DSFAC = PRESS_CON_SM
        ELSE IF ( UNITS .EQ. 'PASCALS AB' ) THEN
          UNITS = 'KILOPASCALS AB'
          DSFAC = PRESS_CON_SM
        ELSE IF ( UNITS .EQ. 'PASCALS GA' ) THEN
          UNITS = 'KILOPASCALS GA'
          DSFAC = PRESS_CON_SM
        END IF
      ELSE IF ( ( DIM_I .EQ. 'MET' ) .AND. ( DIM_O .EQ. 'ENG' ) ) THEN
        ! Metric to English
        IF ( UNITS .EQ. 'MILLIMETERS' ) THEN
          UNITS = 'INCHES'
          DSFAC = DISP_CON_ME
        ELSE IF ( UNITS .EQ. 'METERS' ) THEN
          UNITS = 'INCHES'
          DSFAC = DISP_CON_ME * 1.D3
        ELSE IF ( UNITS .EQ. 'CENTIMETERS' ) THEN
          UNITS = 'INCHES'
          DSFAC = DISP_CON_ME * 1.D1
        ELSE IF ( UNITS .EQ. 'RECIPROCAL MM' ) THEN
          UNITS = 'RECIPROCAL INS'
          DSFAC = 1.D0 / DISP_CON_ME
        ELSE IF ( UNITS .EQ. 'RECIPROCAL MET' ) THEN
          UNITS = 'RECIPROCAL INS'
          DSFAC = 1.D0 / ( DISP_CON_ME * 1.D3 )
        ELSE IF ( UNITS .EQ. 'RECIPROCAL CMS' ) THEN
          UNITS = 'RECIPROCAL INS'
          DSFAC = 1.D0 / ( DISP_CON_ME * 1.D1 )
        ELSE IF ( UNITS .EQ. 'MILLIMETERS**2' ) THEN
          UNITS = 'INCHES**2'
          DSFAC = ( DISP_CON_ME )**2
        ELSE IF ( UNITS .EQ. 'METERS**2' ) THEN
          UNITS = 'INCHES**2'
          DSFAC = ( DISP_CON_ME * 1.D3 )**2
        ELSE IF ( UNITS .EQ. 'CENTIMETERS**2' ) THEN
          UNITS = 'INCHES**2'
          DSFAC = ( DISP_CON_ME * 1.D1 )**2
        ELSE IF ( UNITS .EQ. 'MILLIMETERS**4' ) THEN
          UNITS = 'INCHES**4'
          DSFAC = ( DISP_CON_ME )**4
        ELSE IF ( UNITS .EQ. 'METERS**4' ) THEN
          UNITS = 'INCHES**4'
          DSFAC = ( DISP_CON_ME * 1.D3 )**4
        ELSE IF ( UNITS .EQ. 'CENTIMETERS**4' ) THEN
          UNITS = 'INCHES**4'
          DSFAC = ( DISP_CON_ME * 1.D1 )**4
        ELSE IF ( UNITS .EQ. 'KILOMETERS/HOUR' ) THEN
          UNITS = 'MILES/HOUR'
          DSFAC = VEL_CON_ME
        ELSE IF ( UNITS .EQ. 'METERS/SEC**2' ) THEN
          UNITS = 'G''S'
          DSFAC = ACC_CON_SE
        ELSE IF ( UNITS .EQ. 'KILOGRAMS' ) THEN
          UNITS = 'POUNDS'
          DSFAC = WT_CON_ME
        ELSE IF ( UNITS .EQ. 'NEWTONS' ) THEN
          UNITS = 'POUNDS'
          DSFAC = FRC_CON_ME
        ELSE IF ( UNITS .EQ. 'NEWTON-SECONDS' ) THEN
          UNITS = 'POUND-SECONDS'
          DSFAC = IMP_CON_ME
        ELSE IF ( UNITS .EQ. 'NEWTON-METERS' ) THEN
          IF ( DATA_TYPE_U .EQ. 'ENERGY' ) THEN ! Nonstandard units
            UNITS = 'FOOT-POUNDS'
            DSFAC = ENERGY_CON_ME
          ELSE ! Assume torque
            UNITS = 'POUND-FEET'
            DSFAC = TORQ_CON_ME
          END IF
        ELSE IF ( UNITS .EQ. 'NEWTON-METER-SECONDS' ) THEN
          UNITS = 'POUND-FOOT-SECONDS'
          DSFAC = ANGIMP_CON_ME
        ELSE IF ( UNITS .EQ. 'JOULES' ) THEN
          UNITS = 'FOOT-POUNDS'
          DSFAC = ENERGY_CON_ME
        ELSE IF ( UNITS .EQ. 'METER-NEWTONS' ) THEN
          IF ( DATA_TYPE_U .EQ. 'TORQUE' ) THEN ! Nonstandard units
            UNITS = 'POUND-FEET'
            DSFAC = TORQ_CON_ME
          ELSE ! Assume energy
            UNITS = 'FOOT-POUNDS'
            DSFAC = ENERGY_CON_ME
          END IF
        ELSE IF ( UNITS .EQ. 'MILLIMETER-NEWTONS' ) THEN
          IF ( DATA_TYPE_U .EQ. 'TORQUE' ) THEN ! Nonstandard units
            UNITS = 'POUND-FEET'
            DSFAC = TORQ_CON_ME * 1.D-3
          ELSE ! Assume energy
            UNITS = 'FOOT-POUNDS'
            DSFAC = ENERGY_CON_ME * 1.D-3
          END IF
        ELSE IF ( UNITS .EQ. 'KILOPASCALS' ) THEN
          UNITS = 'POUNDS/IN**2'
          DSFAC = PRESS_CON_ME
        ELSE IF ( UNITS .EQ. 'KILOPASCALS AB' ) THEN
          UNITS = 'POUNDS/IN**2 AB'
          DSFAC = PRESS_CON_ME
        ELSE IF ( UNITS .EQ. 'KILOPASCALS GA' ) THEN
          UNITS = 'POUNDS/IN**2 GA'
          DSFAC = PRESS_CON_ME
        ELSE IF ( UNITS .EQ. 'PASCALS' ) THEN
          UNITS = 'POUNDS/IN**2'
          DSFAC = PRESS_CON_ME * 1.D-3
        ELSE IF ( UNITS .EQ. 'PASCALS AB' ) THEN
          UNITS = 'POUNDS/IN**2 AB'
          DSFAC = PRESS_CON_ME * 1.D-3
        ELSE IF ( UNITS .EQ. 'PASCALS GA' ) THEN
          UNITS = 'POUNDS/IN**2 GA'
          DSFAC = PRESS_CON_ME * 1.D-3
        ELSE IF ( UNITS .EQ. 'MICROMET/MET' ) THEN
          UNITS = 'MICROIN/IN'
        END IF
      ELSE IF ( ( DIM_I .EQ. 'ENG' ) .AND. ( DIM_O .EQ. 'MET' ) ) THEN
        ! English to Metric
        IF ( UNITS .EQ. 'INCHES' ) THEN
          UNITS = 'MILLIMETERS'
          DSFAC = DISP_CON_EM
        ELSE IF ( UNITS .EQ. 'FEET' ) THEN
          UNITS = 'MILLIMETERS'
          DSFAC = DISP_CON_EM * 12.D0
        ELSE IF ( UNITS .EQ. 'RECIPROCAL INS' ) THEN
          UNITS = 'RECIPROCAL MM'
          DSFAC = 1.D0 / DISP_CON_EM
        ELSE IF ( UNITS .EQ. 'INCHES**2' ) THEN
          UNITS = 'MILLIMETERS**2'
          DSFAC = DISP_CON_EM**2
        ELSE IF ( UNITS .EQ. 'FEET**2' ) THEN
          UNITS = 'MILLIMETERS**2'
          DSFAC = ( DISP_CON_EM * 12.D0 )**2
        ELSE IF ( UNITS .EQ. 'INCHES**4' ) THEN
          UNITS = 'MILLIMETERS**4'
          DSFAC = DISP_CON_EM**4
        ELSE IF ( UNITS .EQ. 'FEET**4' ) THEN
          UNITS = 'MILLIMETERS**4'
          DSFAC = ( DISP_CON_EM * 12.D0 )**4
        ELSE IF ( UNITS .EQ. 'MILES/HOUR' ) THEN
          UNITS = 'KILOMETERS/HOUR'
          DSFAC = VEL_CON_EM
        ELSE IF ( UNITS .EQ. 'POUNDS' ) THEN
          IF ( DATA_TYPE_U .EQ. WT_TYP ) THEN
            UNITS = 'KILOGRAMS'
            DSFAC = WT_CON_EM
          ELSE ! Treat as force
            UNITS = 'NEWTONS'
            DSFAC = FRC_CON_EM
          END IF
        ELSE IF ( UNITS .EQ. 'POUND-SECONDS' ) THEN
          UNITS = 'NEWTON-SECONDS'
          DSFAC = IMP_CON_EM
        ELSE IF ( UNITS .EQ. 'POUND-FEET' ) THEN
          IF ( DATA_TYPE_U .EQ. 'ENERGY' ) THEN ! Nonstandard units
            UNITS = 'JOULES'
            DSFAC = ENERGY_CON_EM
          ELSE ! Assume torque
            UNITS = 'NEWTON-METERS'
            DSFAC = TORQ_CON_EM
          END IF
        ELSE IF ( UNITS .EQ. 'POUND-INCHES' ) THEN
          IF ( DATA_TYPE_U .EQ. 'ENERGY' ) THEN ! Nonstandard units
            UNITS = 'JOULES'
            DSFAC = ENERGY_CON_EM / 12.D0
          ELSE ! Assume torque
            UNITS = 'NEWTON-METERS'
            DSFAC = TORQ_CON_EM  / 12.D0
          END IF
        ELSE IF ( UNITS .EQ. 'POUND-FOOT-SECONDS' ) THEN
          UNITS = 'NEWTON-METER-SECONDS'
          DSFAC = ANGIMP_CON_EM
        ELSE IF ( UNITS .EQ. 'POUND-INCH-SECONDS' ) THEN
          UNITS = 'NEWTON-METER-SECONDS'
          DSFAC = ANGIMP_CON_EM / 12.D0
        ELSE IF ( UNITS .EQ. 'FOOT-POUNDS' ) THEN
          IF ( DATA_TYPE_U .EQ. 'TORQUE' ) THEN ! Nonstandard units
            UNITS = 'NEWTON-METERS'
            DSFAC = TORQ_CON_EM
          ELSE ! Assume energy
            UNITS = 'JOULES'
            DSFAC = ENERGY_CON_EM
          END IF
        ELSE IF ( UNITS .EQ. 'INCH-POUNDS' ) THEN
          IF ( DATA_TYPE_U .EQ. 'TORQUE' ) THEN ! Nonstandard units
            UNITS = 'NEWTON-METERS'
            DSFAC = TORQ_CON_EM / 12.D0
          ELSE ! Assume energy
            UNITS = 'JOULES'
            DSFAC = ENERGY_CON_EM / 12.D0
          END IF
        ELSE IF ( UNITS .EQ. 'POUNDS/IN**2' ) THEN
          UNITS = 'KILOPASCALS'
          DSFAC = PRESS_CON_EM
        ELSE IF ( UNITS .EQ. 'POUNDS/IN**2 AB' ) THEN
          UNITS = 'KILOPASCALS AB'
          DSFAC = PRESS_CON_EM
        ELSE IF ( UNITS .EQ. 'POUNDS/IN**2 GA' ) THEN
          UNITS = 'KILOPASCALS GA'
          DSFAC = PRESS_CON_EM
        ELSE IF ( UNITS .EQ. 'MICROIN/IN' ) THEN
          UNITS = 'MICROMET/MET'
        END IF
      ELSE IF ( ( DIM_I .EQ. 'SI' ) .AND. ( DIM_O .EQ. 'ENG' ) ) THEN
        ! SI to English
        IF ( UNITS .EQ. 'METERS' ) THEN
          UNITS = 'INCHES'
          DSFAC = DISP_CON_SE
        ELSE IF ( UNITS .EQ. 'MILLIMETERS' ) THEN
          UNITS = 'INCHES'
          DSFAC = DISP_CON_SE * 1.D-3
        ELSE IF ( UNITS .EQ. 'CENTIMETERS' ) THEN
          UNITS = 'INCHES'
          DSFAC = DISP_CON_SE * 1.D-2
        ELSE IF ( UNITS .EQ. 'RECIPROCAL MET' ) THEN
          UNITS = 'RECIPROCAL INS'
          DSFAC = 1.D0 / DISP_CON_SE
        ELSE IF ( UNITS .EQ. 'RECIPROCAL MM' ) THEN
          UNITS = 'RECIPROCAL INS'
          DSFAC = 1.D0 / ( DISP_CON_SE * 1.D-3 )
        ELSE IF ( UNITS .EQ. 'RECIPROCAL CMS' ) THEN
          UNITS = 'RECIPROCAL INS'
          DSFAC = 1.D0 / ( DISP_CON_SE * 1.D-2 )
        ELSE IF ( UNITS .EQ. 'METERS**2' ) THEN
          UNITS = 'INCHES**2'
          DSFAC = ( DISP_CON_SE )**2
        ELSE IF ( UNITS .EQ. 'MILLIMETERS**2' ) THEN
          UNITS = 'INCHES**2'
          DSFAC = ( DISP_CON_SE * 1.D-3 )**2
        ELSE IF ( UNITS .EQ. 'CENTIMETERS**2' ) THEN
          UNITS = 'INCHES**2'
          DSFAC = ( DISP_CON_SE * 1.D-2 )**2
        ELSE IF ( UNITS .EQ. 'METERS**4' ) THEN
          UNITS = 'INCHES**4'
          DSFAC = ( DISP_CON_SE )**4
        ELSE IF ( UNITS .EQ. 'MILLIMETERS**4' ) THEN
          UNITS = 'INCHES**4'
          DSFAC = ( DISP_CON_SE * 1.D-3 )**4
        ELSE IF ( UNITS .EQ. 'CENTIMETERS**4' ) THEN
          UNITS = 'INCHES**4'
          DSFAC = ( DISP_CON_SE * 1.D-2 )**4
        ELSE IF ( UNITS .EQ. 'METERS/SEC' ) THEN
          UNITS = 'MILES/HOUR'
          DSFAC = VEL_CON_SE
        ELSE IF ( UNITS .EQ. 'METERS/SEC**2' ) THEN
          UNITS = 'G''S'
          DSFAC = ACC_CON_SE
        ELSE IF ( UNITS .EQ. 'KILOGRAMS' ) THEN
          UNITS = 'POUNDS'
          DSFAC = WT_CON_SE
        ELSE IF ( UNITS .EQ. 'NEWTONS' ) THEN
          UNITS = 'POUNDS'
          DSFAC = FRC_CON_SE
        ELSE IF ( UNITS .EQ. 'NEWTON-SECONDS' ) THEN
          UNITS = 'POUND-SECONDS'
          DSFAC = IMP_CON_SE
        ELSE IF ( UNITS .EQ. 'NEWTON-METERS' ) THEN
          IF ( DATA_TYPE_U .EQ. 'ENERGY' ) THEN ! Nonstandard units
            UNITS = 'FOOT-POUNDS'
            DSFAC = ENERGY_CON_SE
          ELSE ! Assume torque
            UNITS = 'POUND-FEET'
            DSFAC = TORQ_CON_SE
          END IF
        ELSE IF ( UNITS .EQ. 'NEWTON-METER-SECONDS' ) THEN
          UNITS = 'POUND-FOOT-SECONDS'
          DSFAC = ANGIMP_CON_SE
        ELSE IF ( UNITS .EQ. 'JOULES' ) THEN
          UNITS = 'FOOT-POUNDS'
          DSFAC = ENERGY_CON_SE
        ELSE IF ( UNITS .EQ. 'METER-NEWTONS' ) THEN
          IF ( DATA_TYPE_U .EQ. 'TORQUE' ) THEN ! Nonstandard units
            UNITS = 'POUND-FEET'
            DSFAC = TORQ_CON_sE
          ELSE ! Assume energy
            UNITS = 'FOOT-POUNDS'
            DSFAC = ENERGY_CON_sE
          END IF
        ELSE IF ( UNITS .EQ. 'MILLIMETER-NEWTONS' ) THEN
          IF ( DATA_TYPE_U .EQ. 'TORQUE' ) THEN ! Nonstandard units
            UNITS = 'POUND-FEET'
            DSFAC = TORQ_CON_SE * 1.D-3
          ELSE ! Assume energy
            UNITS = 'FOOT-POUNDS'
            DSFAC = ENERGY_CON_SE * 1.D-3
          END IF
        ELSE IF ( UNITS .EQ. 'PASCALS' ) THEN
          UNITS = 'POUNDS/IN**2'
          DSFAC = PRESS_CON_SE
        ELSE IF ( UNITS .EQ. 'PASCALS AB' ) THEN
          UNITS = 'POUNDS/IN**2 AB'
          DSFAC = PRESS_CON_SE
        ELSE IF ( UNITS .EQ. 'PASCALS GA' ) THEN
          UNITS = 'POUNDS/IN**2 GA'
          DSFAC = PRESS_CON_SE
        ELSE IF ( UNITS .EQ. 'KILOPASCALS' ) THEN
          UNITS = 'POUNDS/IN**2'
          DSFAC = PRESS_CON_SE * 1.D3
        ELSE IF ( UNITS .EQ. 'KILOPASCALS AB' ) THEN
          UNITS = 'POUNDS/IN**2 AB'
          DSFAC = PRESS_CON_SE * 1.D3
        ELSE IF ( UNITS .EQ. 'KILOPASCALS GA' ) THEN
          UNITS = 'POUNDS/IN**2 GA'
          DSFAC = PRESS_CON_SE * 1.D3
        ELSE IF ( UNITS .EQ. 'MICROMET/MET' ) THEN
          UNITS = 'MICROIN/IN'
        END IF
      ELSE IF ( ( DIM_I .EQ. 'ENG' ) .AND. ( DIM_O .EQ. 'SI' ) ) THEN
        ! English to SI
        IF ( UNITS .EQ. 'INCHES' ) THEN
          UNITS = 'METERS'
          DSFAC = DISP_CON_ES
        ELSE IF ( UNITS .EQ. 'FEET' ) THEN
          UNITS = 'METERS'
          DSFAC = DISP_CON_ES * 12.D0
        ELSE IF ( UNITS .EQ. 'RECIPROCAL INS' ) THEN
          UNITS = 'RECIPROCAL MET'
          DSFAC = 1.D0 / DISP_CON_ES
        ELSE IF ( UNITS .EQ. 'INCHES**2' ) THEN
          UNITS = 'METERS**2'
          DSFAC = DISP_CON_ES**2
        ELSE IF ( UNITS .EQ. 'FEET**2' ) THEN
          UNITS = 'METERS**2'
          DSFAC = ( DISP_CON_ES * 12.D0 )**2
        ELSE IF ( UNITS .EQ. 'INCHES**4' ) THEN
          UNITS = 'METERS**4'
          DSFAC = DISP_CON_ES**4
        ELSE IF ( UNITS .EQ. 'FEET**4' ) THEN
          UNITS = 'METERS**4'
          DSFAC = ( DISP_CON_ES * 12.D0 )**4
        ELSE IF ( UNITS .EQ. 'MILES/HOUR' ) THEN
          UNITS = 'METERS/SEC'
          DSFAC = VEL_CON_ES
        ELSE IF ( UNITS .EQ. 'G''S' ) THEN
          UNITS = 'METERS/SEC**2'
          DSFAC = ACC_CON_ES
        ELSE IF ( UNITS .EQ. 'POUNDS' ) THEN
          IF ( DATA_TYPE_U .EQ. WT_TYP ) THEN
            UNITS = 'KILOGRAMS'
            DSFAC = WT_CON_ES
          ELSE ! Treat as force
            UNITS = 'NEWTONS'
            DSFAC = FRC_CON_ES
          END IF
        ELSE IF ( UNITS .EQ. 'POUND-SECONDS' ) THEN
          UNITS = 'NEWTON-SECONDS'
          DSFAC = IMP_CON_ES
        ELSE IF ( UNITS .EQ. 'POUND-FEET' ) THEN
          IF ( DATA_TYPE_U .EQ. 'ENERGY' ) THEN ! Nonstandard units
            UNITS = 'JOULES'
            DSFAC = ENERGY_CON_ES
          ELSE ! Assume torque
            UNITS = 'NEWTON-METERS'
            DSFAC = TORQ_CON_ES
          END IF
        ELSE IF ( UNITS .EQ. 'POUND-INCHES' ) THEN
          IF ( DATA_TYPE_U .EQ. 'ENERGY' ) THEN ! Nonstandard units
            UNITS = 'JOULES'
            DSFAC = ENERGY_CON_ES / 12.D0
          ELSE ! Assume torque
            UNITS = 'NEWTON-METERS'
            DSFAC = TORQ_CON_ES  / 12.D0
          END IF
        ELSE IF ( UNITS .EQ. 'POUND-FOOT-SECONDS' ) THEN
          UNITS = 'NEWTON-METER-SECONDS'
          DSFAC = ANGIMP_CON_ES
        ELSE IF ( UNITS .EQ. 'POUND-INCH-SECONDS' ) THEN
          UNITS = 'NEWTON-METER-SECONDS'
          DSFAC = ANGIMP_CON_ES / 12.D0
        ELSE IF ( UNITS .EQ. 'FOOT-POUNDS' ) THEN
          IF ( DATA_TYPE_U .EQ. 'TORQUE' ) THEN ! Nonstandard units
            UNITS = 'NEWTON-METERS'
            DSFAC = TORQ_CON_ES
          ELSE ! Assume energy
            UNITS = 'JOULES'
            DSFAC = ENERGY_CON_ES
          END IF
        ELSE IF ( UNITS .EQ. 'INCH-POUNDS' ) THEN
          IF ( DATA_TYPE_U .EQ. 'TORQUE' ) THEN ! Nonstandard units
            UNITS = 'NEWTON-METERS'
            DSFAC = TORQ_CON_ES / 12.D0
          ELSE ! Assume energy
            UNITS = 'JOULES'
            DSFAC = ENERGY_CON_ES / 12.D0
          END IF
        ELSE IF ( UNITS .EQ. 'POUNDS/IN**2' ) THEN
          UNITS = 'PASCALS'
          DSFAC = PRESS_CON_ES
        ELSE IF ( UNITS .EQ. 'POUNDS/IN**2 AB' ) THEN
          UNITS = 'PASCALS AB'
          DSFAC = PRESS_CON_ES
        ELSE IF ( UNITS .EQ. 'POUNDS/IN**2 GA' ) THEN
          UNITS = 'PASCALS GA'
          DSFAC = PRESS_CON_ES
        ELSE IF ( UNITS .EQ. 'MICROIN/IN' ) THEN
          UNITS = 'MICROMET/MET'
        END IF
      END IF

      RETURN
      END



      CHARACTER*(*) FUNCTION DS_UNITS( DATA_TYPE, DIMSYS )

!***********************************************************************
!* Sets Standard Units for a Given Data Type and Dimensional System
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/26
!***********************************************************************

      ! Headers
      INCLUDE 'dimsys.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) DATA_TYPE ! Data type

      CHARACTER*(*) DIMSYS ! Dimensional system code


      ! Variables ______________________________________________________

      CHARACTER DATA_TYPE_U*20


      ! Set standard units
      DS_UNITS = ' '
      CALL STR_UPCASE( DATA_TYPE_U, DATA_TYPE )
      IF ( DIMSYS .EQ. 'MET' ) THEN ! Set Metric units
        IF ( DATA_TYPE_U .EQ. TIME_TYP ) THEN
          DS_UNITS = TIME_UNITS_MET
        ELSE IF ( DATA_TYPE_U .EQ. DISP_TYP ) THEN
          DS_UNITS = DISP_UNITS_MET
        ELSE IF ( DATA_TYPE_U .EQ. 'LENGTH' ) THEN
          DS_UNITS = DISP_UNITS_MET
        ELSE IF ( DATA_TYPE_U .EQ. 'POSITION' ) THEN
          DS_UNITS = DISP_UNITS_MET
        ELSE IF ( DATA_TYPE_U .EQ. 'DEFLECTION' ) THEN
          DS_UNITS = DISP_UNITS_MET
        ELSE IF ( DATA_TYPE_U .EQ. VEL_TYP ) THEN
          DS_UNITS = VEL_UNITS_MET
        ELSE IF ( DATA_TYPE_U .EQ. 'REL. VELOCITY' ) THEN
          DS_UNITS = VEL_UNITS_MET
        ELSE IF ( DATA_TYPE_U .EQ. ACC_TYP ) THEN
          DS_UNITS = ACC_UNITS_MET
        ELSE IF ( DATA_TYPE_U .EQ. ANGDISP_TYP ) THEN
          DS_UNITS = ANGDISP_UNITS_MET
        ELSE IF ( DATA_TYPE_U .EQ. ANGVEL_TYP ) THEN
          DS_UNITS = ANGVEL_UNITS_MET
        ELSE IF ( DATA_TYPE_U .EQ. ANGACC_TYP ) THEN
          DS_UNITS = ANGACC_UNITS_MET
        ELSE IF ( DATA_TYPE_U .EQ. WT_TYP ) THEN
          DS_UNITS = WT_UNITS_MET
        ELSE IF ( DATA_TYPE_U .EQ. FRC_TYP ) THEN
          DS_UNITS = FRC_UNITS_MET
        ELSE IF ( DATA_TYPE_U .EQ. 'STATIC FORCE' ) THEN
          DS_UNITS = FRC_UNITS_MET
        ELSE IF ( DATA_TYPE_U .EQ. IMP_TYP ) THEN
          DS_UNITS = IMP_UNITS_MET
        ELSE IF ( DATA_TYPE_U .EQ. TORQ_TYP ) THEN
          DS_UNITS = TORQ_UNITS_MET
        ELSE IF ( DATA_TYPE_U .EQ. ANGIMP_TYP ) THEN
          DS_UNITS = ANGIMP_UNITS_MET
        ELSE IF ( DATA_TYPE_U .EQ. ENERGY_TYP ) THEN
          DS_UNITS = ENERGY_UNITS_MET
        ELSE IF ( DATA_TYPE_U .EQ. PRESS_TYP ) THEN
          DS_UNITS = PRESS_UNITS_MET
        END IF
      ELSE IF ( DIMSYS .EQ. 'SI' ) THEN ! Set SI units
        IF ( DATA_TYPE_U .EQ. TIME_TYP ) THEN
          DS_UNITS = TIME_UNITS_SI
        ELSE IF ( DATA_TYPE_U .EQ. DISP_TYP ) THEN
          DS_UNITS = DISP_UNITS_SI
        ELSE IF ( DATA_TYPE_U .EQ. 'LENGTH' ) THEN
          DS_UNITS = DISP_UNITS_SI
        ELSE IF ( DATA_TYPE_U .EQ. 'POSITION' ) THEN
          DS_UNITS = DISP_UNITS_SI
        ELSE IF ( DATA_TYPE_U .EQ. 'DEFLECTION' ) THEN
          DS_UNITS = DISP_UNITS_SI
        ELSE IF ( DATA_TYPE_U .EQ. VEL_TYP ) THEN
          DS_UNITS = VEL_UNITS_SI
        ELSE IF ( DATA_TYPE_U .EQ. 'REL. VELOCITY' ) THEN
          DS_UNITS = VEL_UNITS_SI
        ELSE IF ( DATA_TYPE_U .EQ. ACC_TYP ) THEN
          DS_UNITS = ACC_UNITS_SI
        ELSE IF ( DATA_TYPE_U .EQ. ANGDISP_TYP ) THEN
          DS_UNITS = ANGDISP_UNITS_SI
        ELSE IF ( DATA_TYPE_U .EQ. ANGVEL_TYP ) THEN
          DS_UNITS = ANGVEL_UNITS_SI
        ELSE IF ( DATA_TYPE_U .EQ. ANGACC_TYP ) THEN
          DS_UNITS = ANGACC_UNITS_SI
        ELSE IF ( DATA_TYPE_U .EQ. WT_TYP ) THEN
          DS_UNITS = WT_UNITS_SI
        ELSE IF ( DATA_TYPE_U .EQ. FRC_TYP ) THEN
          DS_UNITS = FRC_UNITS_SI
        ELSE IF ( DATA_TYPE_U .EQ. 'STATIC FORCE' ) THEN
          DS_UNITS = FRC_UNITS_SI
        ELSE IF ( DATA_TYPE_U .EQ. IMP_TYP ) THEN
          DS_UNITS = IMP_UNITS_SI
        ELSE IF ( DATA_TYPE_U .EQ. TORQ_TYP ) THEN
          DS_UNITS = TORQ_UNITS_SI
        ELSE IF ( DATA_TYPE_U .EQ. ANGIMP_TYP ) THEN
          DS_UNITS = ANGIMP_UNITS_SI
        ELSE IF ( DATA_TYPE_U .EQ. ENERGY_TYP ) THEN
          DS_UNITS = ENERGY_UNITS_SI
        ELSE IF ( DATA_TYPE_U .EQ. PRESS_TYP ) THEN
          DS_UNITS = PRESS_UNITS_SI
        END IF
      ELSE IF ( DIMSYS .EQ. 'ENG' ) THEN ! Set English units
        IF ( DATA_TYPE_U .EQ. TIME_TYP ) THEN
          DS_UNITS = TIME_UNITS_ENG
        ELSE IF ( DATA_TYPE_U .EQ. DISP_TYP ) THEN
          DS_UNITS = DISP_UNITS_ENG
        ELSE IF ( DATA_TYPE_U .EQ. 'LENGTH' ) THEN
          DS_UNITS = DISP_UNITS_ENG
        ELSE IF ( DATA_TYPE_U .EQ. 'POSITION' ) THEN
          DS_UNITS = DISP_UNITS_ENG
        ELSE IF ( DATA_TYPE_U .EQ. 'DEFLECTION' ) THEN
          DS_UNITS = DISP_UNITS_ENG
        ELSE IF ( DATA_TYPE_U .EQ. VEL_TYP ) THEN
          DS_UNITS = VEL_UNITS_ENG
        ELSE IF ( DATA_TYPE_U .EQ. 'REL. VELOCITY' ) THEN
          DS_UNITS = VEL_UNITS_ENG
        ELSE IF ( DATA_TYPE_U .EQ. ACC_TYP ) THEN
          DS_UNITS = ACC_UNITS_ENG
        ELSE IF ( DATA_TYPE_U .EQ. ANGDISP_TYP ) THEN
          DS_UNITS = ANGDISP_UNITS_ENG
        ELSE IF ( DATA_TYPE_U .EQ. ANGVEL_TYP ) THEN
          DS_UNITS = ANGVEL_UNITS_ENG
        ELSE IF ( DATA_TYPE_U .EQ. ANGACC_TYP ) THEN
          DS_UNITS = ANGACC_UNITS_ENG
        ELSE IF ( DATA_TYPE_U .EQ. WT_TYP ) THEN
          DS_UNITS = WT_UNITS_ENG
        ELSE IF ( DATA_TYPE_U .EQ. FRC_TYP ) THEN
          DS_UNITS = FRC_UNITS_ENG
        ELSE IF ( DATA_TYPE_U .EQ. 'STATIC FORCE' ) THEN
          DS_UNITS = FRC_UNITS_ENG
        ELSE IF ( DATA_TYPE_U .EQ. IMP_TYP ) THEN
          DS_UNITS = IMP_UNITS_ENG
        ELSE IF ( DATA_TYPE_U .EQ. TORQ_TYP ) THEN
          DS_UNITS = TORQ_UNITS_ENG
        ELSE IF ( DATA_TYPE_U .EQ. ANGIMP_TYP ) THEN
          DS_UNITS = ANGIMP_UNITS_ENG
        ELSE IF ( DATA_TYPE_U .EQ. ENERGY_TYP ) THEN
          DS_UNITS = ENERGY_UNITS_ENG
        ELSE IF ( DATA_TYPE_U .EQ. PRESS_TYP ) THEN
          DS_UNITS = PRESS_UNITS_ENG
        END IF
      END IF

      RETURN
      END



      CHARACTER*(*) FUNCTION DS_TYPE( UNITS )

!***********************************************************************
!* Sets Standard Type for a Given Data Units
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/26
!***********************************************************************

      ! Headers
      INCLUDE 'dimsys.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) UNITS ! Data units


      ! Variables ______________________________________________________

      CHARACTER UNITS_U*20


      ! Set standard units
      DS_TYPE = ' '
      CALL STR_UPCASE( UNITS_U, UNITS )
      IF ( UNITS_U .EQ. TIME_UNITS_MET ) THEN
        DS_TYPE = TIME_TYP
      ELSE IF ( UNITS_U .EQ. DISP_UNITS_MET ) THEN
        DS_TYPE = DISP_TYP
      ELSE IF ( UNITS_U .EQ. 'CENTIMETERS' ) THEN
        DS_TYPE = DISP_TYP
      ELSE IF ( UNITS_U .EQ. 'METERS' ) THEN
        DS_TYPE = DISP_TYP
      ELSE IF ( UNITS_U .EQ. VEL_UNITS_MET ) THEN
        DS_TYPE = VEL_TYP
      ELSE IF ( UNITS_U .EQ. ACC_UNITS_MET ) THEN
        DS_TYPE = ACC_TYP
      ELSE IF ( UNITS_U .EQ. ANGDISP_UNITS_MET ) THEN
        DS_TYPE = ANGDISP_TYP
      ELSE IF ( UNITS_U .EQ. 'RADIANS' ) THEN
        DS_TYPE = ANGDISP_TYP
      ELSE IF ( UNITS_U .EQ. ANGVEL_UNITS_MET ) THEN
        DS_TYPE = ANGVEL_TYP
      ELSE IF ( UNITS_U .EQ. 'RADIANS/SEC' ) THEN
        DS_TYPE = ANGVEL_TYP
      ELSE IF ( UNITS_U .EQ. ANGACC_UNITS_MET ) THEN
        DS_TYPE = ANGACC_TYP
      ELSE IF ( UNITS_U .EQ. 'RADIANS/SEC**2' ) THEN
        DS_TYPE = ANGACC_TYP
      ELSE IF ( UNITS_U .EQ. WT_UNITS_MET ) THEN
        DS_TYPE = WT_TYP
      ELSE IF ( UNITS_U .EQ. FRC_UNITS_MET ) THEN
        DS_TYPE = FRC_TYP
      ELSE IF ( UNITS_U .EQ. IMP_UNITS_MET ) THEN
        DS_TYPE = IMP_TYP
      ELSE IF ( UNITS_U .EQ. TORQ_UNITS_MET ) THEN
        DS_TYPE = TORQ_TYP
      ELSE IF ( UNITS_U .EQ. ANGIMP_UNITS_MET ) THEN
        DS_TYPE = ANGIMP_TYP
      ELSE IF ( UNITS_U .EQ. ENERGY_UNITS_MET ) THEN
        DS_TYPE = ENERGY_TYP
      ELSE IF ( UNITS_U .EQ. 'METER-NEWTONS' ) THEN
        DS_TYPE = ENERGY_TYP
      ELSE IF ( UNITS_U .EQ. PRESS_UNITS_MET ) THEN
        DS_TYPE = PRESS_TYP
      ELSE IF ( UNITS_U .EQ. TIME_UNITS_SI ) THEN
        DS_TYPE = TIME_TYP
      ELSE IF ( UNITS_U .EQ. DISP_UNITS_SI ) THEN
        DS_TYPE = DISP_TYP
      ELSE IF ( UNITS_U .EQ. 'MILLIMETERS' ) THEN
        DS_TYPE = DISP_TYP
      ELSE IF ( UNITS_U .EQ. 'CENTIMETERS' ) THEN
        DS_TYPE = DISP_TYP
      ELSE IF ( UNITS_U .EQ. VEL_UNITS_SI ) THEN
        DS_TYPE = VEL_TYP
      ELSE IF ( UNITS_U .EQ. ACC_UNITS_SI ) THEN
        DS_TYPE = ACC_TYP
      ELSE IF ( UNITS_U .EQ. ANGDISP_UNITS_SI ) THEN
        DS_TYPE = ANGDISP_TYP
      ELSE IF ( UNITS_U .EQ. 'RADIANS' ) THEN
        DS_TYPE = ANGDISP_TYP
      ELSE IF ( UNITS_U .EQ. ANGVEL_UNITS_SI ) THEN
        DS_TYPE = ANGVEL_TYP
      ELSE IF ( UNITS_U .EQ. 'RADIANS/SEC' ) THEN
        DS_TYPE = ANGVEL_TYP
      ELSE IF ( UNITS_U .EQ. ANGACC_UNITS_SI ) THEN
        DS_TYPE = ANGACC_TYP
      ELSE IF ( UNITS_U .EQ. 'RADIANS/SEC**2' ) THEN
        DS_TYPE = ANGACC_TYP
      ELSE IF ( UNITS_U .EQ. WT_UNITS_SI ) THEN
        DS_TYPE = WT_TYP
      ELSE IF ( UNITS_U .EQ. FRC_UNITS_SI ) THEN
        DS_TYPE = FRC_TYP
      ELSE IF ( UNITS_U .EQ. IMP_UNITS_SI ) THEN
        DS_TYPE = IMP_TYP
      ELSE IF ( UNITS_U .EQ. TORQ_UNITS_SI ) THEN
        DS_TYPE = TORQ_TYP
      ELSE IF ( UNITS_U .EQ. 'POUND-INCHES' ) THEN
        DS_TYPE = TORQ_TYP
      ELSE IF ( UNITS_U .EQ. ANGIMP_UNITS_SI ) THEN
        DS_TYPE = ANGIMP_TYP
      ELSE IF ( UNITS_U .EQ. ENERGY_UNITS_SI ) THEN
        DS_TYPE = ENERGY_TYP
      ELSE IF ( UNITS_U .EQ. 'INCH-POUNDS' ) THEN
        DS_TYPE = ENERGY_TYP
      ELSE IF ( UNITS_U .EQ. PRESS_UNITS_SI ) THEN
        DS_TYPE = PRESS_TYP
      ELSE IF ( UNITS_U .EQ. TIME_UNITS_ENG ) THEN
        DS_TYPE = TIME_TYP
      ELSE IF ( UNITS_U .EQ. DISP_UNITS_ENG ) THEN
        DS_TYPE = DISP_TYP
      ELSE IF ( UNITS_U .EQ. 'FEET' ) THEN
        DS_TYPE = DISP_TYP
      ELSE IF ( UNITS_U .EQ. VEL_UNITS_ENG ) THEN
        DS_TYPE = VEL_TYP
      ELSE IF ( UNITS_U .EQ. ACC_UNITS_ENG ) THEN
        DS_TYPE = ACC_TYP
      ELSE IF ( UNITS_U .EQ. ANGDISP_UNITS_ENG ) THEN
        DS_TYPE = ANGDISP_TYP
      ELSE IF ( UNITS_U .EQ. 'RADIANS' ) THEN
        DS_TYPE = ANGDISP_TYP
      ELSE IF ( UNITS_U .EQ. ANGVEL_UNITS_ENG ) THEN
        DS_TYPE = ANGVEL_TYP
      ELSE IF ( UNITS_U .EQ. 'RADIANS/SEC' ) THEN
        DS_TYPE = ANGVEL_TYP
      ELSE IF ( UNITS_U .EQ. ANGACC_UNITS_ENG ) THEN
        DS_TYPE = ANGACC_TYP
      ELSE IF ( UNITS_U .EQ. 'RADIANS/SEC**2' ) THEN
        DS_TYPE = ANGACC_TYP
      ELSE IF ( UNITS_U .EQ. WT_UNITS_ENG ) THEN
        DS_TYPE = WT_TYP
      ELSE IF ( UNITS_U .EQ. FRC_UNITS_ENG ) THEN
        DS_TYPE = FRC_TYP
      ELSE IF ( UNITS_U .EQ. IMP_UNITS_ENG ) THEN
        DS_TYPE = IMP_TYP
      ELSE IF ( UNITS_U .EQ. TORQ_UNITS_ENG ) THEN
        DS_TYPE = TORQ_TYP
      ELSE IF ( UNITS_U .EQ. 'POUND-INCHES' ) THEN
        DS_TYPE = TORQ_TYP
      ELSE IF ( UNITS_U .EQ. ANGIMP_UNITS_ENG ) THEN
        DS_TYPE = ANGIMP_TYP
      ELSE IF ( UNITS_U .EQ. ENERGY_UNITS_ENG ) THEN
        DS_TYPE = ENERGY_TYP
      ELSE IF ( UNITS_U .EQ. 'INCH-POUNDS' ) THEN
        DS_TYPE = ENERGY_TYP
      ELSE IF ( UNITS_U .EQ. PRESS_UNITS_ENG ) THEN
        DS_TYPE = PRESS_TYP
      END IF

      RETURN
      END
