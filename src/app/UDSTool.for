      PROGRAM UDSTOOL

!***********************************************************************
!* UDS File Display and Modification Tool
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/08/17
!***********************************************************************

      ! Headers
      INCLUDE 'platform.fi'
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'


      ! Variables ______________________________________________________

      RECORD /UDS/  U
      RECORD /UDS_Y/  A, V, D

      LOGICAL EOF, DONE

      INTEGER NUM_ARGS, MAX_ARGS, DISPLAY_UNIT, IOS, I

      PARAMETER ( MAX_ARGS = 1 )

      DOUBLE PRECISION DT

      CHARACTER CL_ARG(0:MAX_ARGS)*255,
     & FILE_NAME*252, RESP, OSTAT, REDIRECT*252, OUT_MODE,
     & U_TYP, TTYP, TUN*22


      ! Initializations
      CALL GET_CL( CL_ARG, NUM_ARGS, MAX_ARGS )
      OSTAT = '1'
      OUT_MODE = ' '
      DISPLAY_UNIT = -1

      ! Program header
      WRITE( *, '(//15X,A/)' )
     & '**  UDSTool - UDS File Display and Modification  **'

      ! Read the UDS file
      IF ( NUM_ARGS .GT. 0 ) THEN
        FILE_NAME = CL_ARG(1)
      ELSE
        FILE_NAME = ' '
      END IF
  100 CALL UDS_IN( FILE_NAME, U, DT, OSTAT, DONE )
      IF ( DONE ) GO TO 199
      IF ( FILE_NAME .EQ. ' ' ) GO TO 100

      ! Identify relevant integrals/derivatives of signal
      CALL IDENTIFY_SIGNALS( U, U_TYP, IOS )
      IF ( IOS .NE. 0 ) GO TO 100

      ! Compute relevant integrals/derivatives of signal
      CALL COMPUTE_SIGNALS( U, U_TYP, A, V, D, IOS )
      IF ( IOS .NE. 0 ) GO TO 100

      ! Set "time" axis flag
      IF ( U.CHANFORM .EQ. 'Y' ) THEN ! Use actual X
        TTYP = U.XTYP(1:1)
        TUN = '('//U.XUNITS(:L_TRIM(U.XUNITS))//')'
      ELSE ! Use index value
        TTYP = 'I'
        TUN = ' '
      END IF

      ! Prompt for the process
  101 WRITE( *, '(/2A,20X,A/14X,A/'' >> '',$)' ) ' Process? :   ',
     & 'D - Display,   M - Modify,   O - Output,', '[done]',
     & 'R - Redirect display,   N - New input'
      READ( *, '(A)', END=199 ) RESP
      CALL STR_UP( RESP )

      ! Perform requested process
      IF ( RESP .EQ. 'D' ) THEN ! Display

        WRITE( *, '(/2A/'' >> '',$)' ) ' Display? :   ',
     &   'O - Overview,  T - Table,  S - Specs '
        READ( *, '(A)', END=101 ) RESP
        CALL STR_UP( RESP )

        IF ( RESP .EQ. 'O' ) THEN ! Overview
          CALL OVERVIEW( DISPLAY_UNIT, U, A, V, D, DT )
          IF ( OUT_MODE .EQ. 'F' )
     &     WRITE( *, '(A/1X,A)' ) ' *** Overview written to:',
     &     REDIRECT(:L_TRIM(REDIRECT))
        ELSE IF ( RESP .EQ. 'T' ) THEN ! Table
          CALL TABLE( DISPLAY_UNIT, U, A, V, D, DT )
          IF ( OUT_MODE .EQ. 'F' )
     &     WRITE( *, '(A/1X,A)' ) ' *** Table written to:',
     &     REDIRECT(:L_TRIM(REDIRECT))
        ELSE IF ( RESP .EQ. 'S' ) THEN ! Specs
          CALL SPEC_DISPLAY( DISPLAY_UNIT, U )
          IF ( OUT_MODE .EQ. 'F' )
     &     WRITE( *, '(A/1X,A)' ) ' *** Specs written to:',
     &     REDIRECT(:L_TRIM(REDIRECT))
        END IF

      ELSE IF ( RESP .EQ. 'M' ) THEN ! Modification

  102   WRITE( *, '(/2A/18X,A/'' >> '',$)' ) ' Modification? :  ',
     &   'C - Calibration,  B - Bias,  I - Interpolation, ',
     &   'V - Value,  D - Domain,  S - Specs'
        READ( *, '(A)', END=101 ) RESP
        CALL STR_UP( RESP )

        IF ( RESP .EQ. 'C' ) THEN ! Calibration

          IF ( U.CHANFORM .EQ. 'Y' ) THEN
            CALL CAL_MOD( U, TTYP, DT, U.YTYP, U.YUNITS, U.Y )
          ELSE
            WRITE( *, '(/A,50X,A/'' >> '',$)' )
     &       ' Axis to calibrate?   (X/Y)', '[Y]'
            READ( *, '(A)', END=102 ) RESP
            CALL STR_UP( RESP )
            IF ( RESP .EQ. 'X' ) THEN
              CALL CAL_MOD( U, TTYP, DT, U.XTYP, U.XUNITS, U.X )
            ELSE IF ( ANY_CHARS( RESP, ' Y' ) ) THEN
              CALL CAL_MOD( U, TTYP, DT, U.YTYP, U.YUNITS, U.Y )
            END IF
          END IF

          ! Warn about INIVEL
          IF ( ( U.CHANFORM .EQ. 'Y' ) .OR. ( RESP .EQ. 'Y' ) ) THEN
            IF ( ( U_TYP .EQ. 'V' ) .OR. ( U_TYP .EQ. 'D' ) ) THEN
              IF ( V.YTYP .EQ. 'VELOCITY' ) WRITE( *, '(/A/)' )
     &         ' *** Initial velocity (INIVEL) spec '//
     &         'might need updating'
            END IF
          END IF

        ELSE IF ( RESP .EQ. 'B' ) THEN ! Bias

          IF ( U.CHANFORM .EQ. 'Y' ) THEN
            CALL BIAS_MOD( U, TTYP, DT, U.YTYP, U.YUNITS, U.Y )
          ELSE
            WRITE( *, '(/A,55X,A/'' >> '',$)' )
     &       ' Axis to bias?   (X/Y)', '[Y]'
            READ( *, '(A)', END=102 ) RESP
            CALL STR_UP( RESP )
            IF ( RESP .EQ. 'X' ) THEN
              CALL BIAS_MOD( U, TTYP, DT, U.XTYP, U.XUNITS, U.X )
            ELSE IF ( ANY_CHARS( RESP, ' Y' ) ) THEN
              CALL BIAS_MOD( U, TTYP, DT, U.YTYP, U.YUNITS, U.Y )
            END IF
          END IF

          ! Warn about INIVEL
          IF ( ( U.CHANFORM .EQ. 'Y' ) .OR. ( RESP .EQ. 'Y' ) ) THEN
            IF ( U_TYP .EQ. 'V' ) THEN
              IF ( V.YTYP .EQ. 'VELOCITY' ) WRITE( *, '(/A/)' )
     &         ' *** Initial velocity (INIVEL) spec '//
     &         'might need updating'
            END IF
          END IF

        ELSE IF ( RESP .EQ. 'I' ) THEN ! Interpolation

          IF ( U.CHANFORM .EQ. 'Y' ) THEN
            CALL INTERP_MOD( U, TTYP, DT, U.YTYP, U.YUNITS, U.Y )
          ELSE
            WRITE( *, '(/A,48X,A/'' >> '',$)' )
     &       ' Axis to interpolate?   (X/Y)', '[Y]'
            READ( *, '(A)', END=102 ) RESP
            CALL STR_UP( RESP )
            IF ( RESP .EQ. 'X' ) THEN
              CALL INTERP_MOD( U, TTYP, DT, U.XTYP, U.XUNITS, U.X )
            ELSE IF ( ANY_CHARS( RESP, ' Y' ) ) THEN
              CALL INTERP_MOD( U, TTYP, DT, U.YTYP, U.YUNITS, U.Y )
            END IF
          END IF

        ELSE IF ( RESP .EQ. 'V' ) THEN ! Value

          ! Prompt for individual values or new file
          WRITE( *, '(/2A,14X,A/'' >> '',$)' )
     &     ' Value modification type?   ',
     &     '( I - Individual,  F - File input )', '[I]'
          READ( *, '(A)', END=102 ) RESP
          CALL STR_UP( RESP )
          IF ( RESP .EQ. 'F' ) THEN
            CALL GET_FILE( U, .TRUE., .TRUE., .TRUE. )
          ELSE IF ( U.CHANFORM .EQ. 'Y' ) THEN
            CALL VAL_MOD( U, TTYP, DT, U.YTYP, U.YUNITS, U.Y )
          ELSE
            WRITE( *, '(/A,47X,A/'' >> '',$)' )
     &       ' Axis to value change?   (X/Y)', '[Y]'
            READ( *, '(A)', END=102 ) RESP
            CALL STR_UP( RESP )
            IF ( RESP .EQ. 'X' ) THEN
              CALL VAL_MOD( U, TTYP, DT, U.XTYP, U.XUNITS, U.X )
            ELSE
              CALL VAL_MOD( U, TTYP, DT, U.YTYP, U.YUNITS, U.Y )
            END IF
          END IF

        ELSE IF ( RESP .EQ. 'D' ) THEN ! Domain

          WRITE( *, '(/2A/'' >> '',$)' )
     &     ' Domain modification? :   ',
     &     'S - Shift,  R - Range,  B - Subsample'
          READ( *, '(A)', END=102 ) RESP
          CALL STR_UP( RESP )

          IF ( RESP .EQ. 'S' ) THEN ! Time/Freq shift
            CALL SHIFT_MOD( U, TTYP, TUN, DT )
          ELSE IF ( RESP .EQ. 'R' ) THEN ! Range
            CALL RANGE_MOD( U, TTYP, TUN, DT )
          ELSE IF ( RESP .EQ. 'B' ) THEN ! Subsample
            CALL SUBSAMP( U, DT )
          END IF

        ELSE IF ( RESP .EQ. 'S' ) THEN ! Specifications

          CALL SPEC_MOD( U )

        ELSE IF ( RESP .NE. ' ' ) THEN

          WRITE( *, '(/A)' ) '*** Unrecognized response'
          GO TO 102

        ELSE ! No modification

          GO TO 101

        END IF

        ! Recompute X array
        IF ( U.CHANFORM .EQ. 'Y' ) THEN
          DO I = U.NFP, U.NLP
            U.X(I) = I * U.DEL
          END DO
        END IF

        ! Compute relevant integrals/derivatives of signal
        CALL COMPUTE_SIGNALS( U, U_TYP, A, V, D, IOS )
        IF ( IOS .NE. 0 ) GO TO 100

        ! Loop for next modification
        IF ( OSTAT .NE. 'N' ) OSTAT = 'M'
        GO TO 102

      ELSE IF ( RESP .EQ. 'O' ) THEN ! UDS file output

        CALL UDS_OUT( FILE_NAME, U, OSTAT, EOF )
        IF ( EOF ) GO TO 101

      ELSE IF ( RESP .EQ. 'R' ) THEN ! Redirect display

        ! Prompt for the redirection
  103   WRITE( *, '(/A,43X,A/'' >> '',$)' )
     &   ' Filename for display output?', '[screen]'
        READ( *, '(A)', END=101 ) REDIRECT

        IF ( REDIRECT .EQ. ' ' ) THEN

          IF ( OUT_MODE .NE. 'S' ) THEN ! Redirect to terminal
            CLOSE( DISPLAY_UNIT )
            OUT_MODE = 'S'
            DISPLAY_UNIT = -1
          END IF

        ELSE ! Redirect to file

          IF ( ( FN_EQUAL( REDIRECT, SCREEN_DEV ) ) .OR.
     &     ( .NOT. FN_VALID( REDIRECT ) ) ) THEN
            WRITE( *, * ) '*** Invalid file name'
            GO TO 103
          END IF
          IF ( DISPLAY_UNIT .GE. 0 ) CLOSE( DISPLAY_UNIT )
          CALL OPEN_FS( DISPLAY_UNIT, REDIRECT, 'W', IOS )
          IF ( IOS .NE. 0 )
     &     CALL OPEN_FS( DISPLAY_UNIT, REDIRECT, ' ', IOS )
          IF ( IOS .NE. 0 ) THEN
            WRITE( *, '(/2A)' ) ' *** File open failed - ',
     &       'Display directed to terminal'
            CLOSE( DISPLAY_UNIT )
            OUT_MODE = 'S'
            DISPLAY_UNIT = -1
          ELSE
            OUT_MODE = 'F'
            WRITE( DISPLAY_UNIT, '(2A)', IOSTAT=IOS )
     &       'UDSTool display from file: ',
     &       FILE_NAME(:L_TRIM(FILE_NAME))
          END IF

        END IF

      ELSE IF ( RESP .EQ. 'N' ) THEN ! New input file

        IF ( ( OSTAT .EQ. 'M' ) .OR. ( OSTAT .EQ. 'N' ) ) THEN
          IF ( OSTAT .EQ. 'M' ) THEN
            WRITE( *, '(/A,46X,A/'' >> '',$)' )
     &       ' Output modified file?   (Y/N)', '[No]'
          ELSE IF ( OSTAT .EQ. 'N' ) THEN
            WRITE( *, '(/A,47X,A/'' >> '',$)' )
     &       ' Output created file?   (Y/N)', '[No]'
          END IF
          READ( *, '(A)', END=101 ) RESP
          CALL STR_UP( RESP )
          IF ( RESP .EQ. 'Y' ) THEN
            CALL UDS_OUT( FILE_NAME, U, OSTAT, EOF )
            IF ( EOF ) GO TO 101
          END IF

        END IF
        WRITE( *, '(80(''_''))' )
        GO TO 100

      ELSE IF ( RESP .EQ. ' ' ) THEN ! Done

        IF ( ( OSTAT .EQ. 'M' ) .OR. ( OSTAT .EQ. 'N' ) ) THEN
          IF ( OSTAT .EQ. 'M' ) THEN
            WRITE( *, '(/A,46X,A/'' >> '',$)' )
     &       ' Output modified file?   (Y/N)', '[No]'
          ELSE IF ( OSTAT .EQ. 'N' ) THEN
            WRITE( *, '(/A,47X,A/'' >> '',$)' )
     &       ' Output created file?   (Y/N)', '[No]'
          END IF
          READ( *, '(A)', END=101 ) RESP
          CALL STR_UP( RESP )
          IF ( RESP .EQ. 'Y') THEN
            CALL UDS_OUT( FILE_NAME, U, OSTAT, EOF )
            IF ( EOF ) GO TO 101
          END IF
        END IF
        IF ( DISPLAY_UNIT .GE. 0 ) CLOSE( DISPLAY_UNIT )
        GO TO 199

      ELSE

        WRITE( *, '(/A)' ) ' *** Unacceptable response'

      END IF

      ! Loop for next process
      GO TO 101

  199 END



      SUBROUTINE UDS_IN( FILE_NAME, U, DT, OSTAT, DONE )

!***********************************************************************
!* Reads the UDS Input File and Sets Pertinent Values
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) FILE_NAME

      RECORD /UDS/  U

      DOUBLE PRECISION DT

      CHARACTER OSTAT

      LOGICAL DONE


      ! Variables ______________________________________________________

      RECORD /UDS_CONTROL/  C

      LOGICAL FILE_X

      INTEGER IOS, I

      CHARACTER RESP, DEF_FILE*255


      ! Obtain UDS input file name
      CALL UDS_CONTROL_INIT( C )
      C.FILL_X = .TRUE.
      DEF_FILE = FILE_NAME
  101 IF ( ( OSTAT .EQ. '1' ) .AND. ( FILE_NAME .NE. ' ' ) ) THEN
        ! Use FILE_NAME
        OSTAT = ' '
      ELSE IF ( DEF_FILE .EQ. ' ' ) THEN
        WRITE( *, '(/A,41X,A/'' >> '',$)' )
     &   ' UDS file name (new or existing)?', '[done]'
        READ( *, '(A)', END=199 ) FILE_NAME
        IF ( FILE_NAME .EQ. ' ' ) GO TO 199
      ELSE
        WRITE( *, '(/2A,16X,A/'' >> '',$)' )
     &   ' UDS file name (new or existing)?   ',
     &   '( P - Previous input )', '[done]'
        READ( *, '(A)', END=199 ) FILE_NAME
        IF ( FILE_NAME .EQ. ' ' ) THEN
          GO TO 199
        ELSE IF ( ( FILE_NAME .EQ. 'P' ) .OR.
     &   ( FILE_NAME .EQ. 'p' ) ) THEN
          FILE_NAME = DEF_FILE
          WRITE( *, *, IOSTAT=IOS ) '*** Reading: ',
     &     FILE_NAME(:L_TRIM(FILE_NAME))
        END IF
      END IF

      ! Zero-initialize UDS arrays
      DO I = NFP_L, NLP_U
        U.X(I) = 0.
        U.Y(I) = 0.
      END DO

      ! Read or create the UDS file
      INQUIRE( FILE=FILE_NAME, EXIST=FILE_X, IOSTAT=IOS )
      OSTAT = ' '
      IF ( ( FILE_X ) .AND. ( IOS .EQ. 0 ) ) THEN ! Read UDS file
        CALL UDS_READ( FILE_NAME, U, C, IOS )
        IF ( IOS .NE. 0 ) GO TO 101
        OSTAT = 'O'
      ELSE ! No such file - Ask whether to create new UDS file
        WRITE( *, * ) '*** File does not exist'
        WRITE( *, '(/A,45X,A/'' >> '',$)' )
     &   ' Create a new UDS file?   (Y/N)', '[No]'
        READ( *, '(A)', END=101 ) RESP
        IF ( ( RESP .NE. 'Y' ) .AND. ( RESP .NE. 'y' ) ) THEN
          FILE_NAME = ' '
          GO TO 101
        END IF
        IF ( .NOT. FN_VALID( FILE_NAME ) ) THEN
          WRITE( *, * ) '*** Invalid file name'
          GO TO 101
        END IF
        CALL UDS_CREATE( U )
        OSTAT = 'N'
      END IF

      ! Show capsule of file specifications
      WRITE( *, 601 ) U.TSTNAM, U.CURNAM, U.STATUS
  601 FORMAT(/' Test: ',A,5X,'Curve: ',A,5X,'Status: ',A)
      WRITE( *, '(2A)' ) ' Sensor attachment: ',
     & U.SENATT(:L_TRIM(U.SENATT))
      IF ( ( U.OCCTYP .NE. ' ' ) .OR. ( U.SENLOC .NE. ' '  ) ) THEN
        WRITE( *, 602 ) U.SENLOC, U.OCCTYP
  602   FORMAT(' Occupant: ',A,5X,A)
      END IF
      WRITE( *, '(4A)' ) ' Curve type: ', U.CURTYP, ' Axis: ', U.AXIS
      WRITE( *, 603 )
     & U.YTYP(:L_TRIM(U.YTYP)), U.YUNITS(:L_TRIM(U.YUNITS)),
     & U.XTYP(:L_TRIM(U.XTYP)), U.XUNITS(:L_TRIM(U.XUNITS))
  603 FORMAT(1X,A,' (',A,')  vs.  ',A,' (',A,')')
      IF ( U.CURDSC .NE. ' ' ) WRITE( *, '(2A)' ) ' Curve desc: ',
     & U.CURDSC(:L_TRIM(U.CURDSC))
      WRITE( *, * )

      ! Translate DEL to double precision
      IF ( U.CHANFORM .EQ. 'Y' ) THEN
        DT = DP_TRAN( U.DEL )
      ELSE
        DT = 1.D0
      END IF

      DONE = .FALSE.
      RETURN

      ! Done: Blank or Crtl-Z entered
  199 DONE = .TRUE.
      RETURN

      END



      SUBROUTINE IDENTIFY_SIGNALS( U, U_TYP, IOS )

!***********************************************************************
!* Identifies the Relevant Integrals/Derivatives of the Input Signal
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'


      ! Arguments ______________________________________________________

      RECORD /UDS/  U

      CHARACTER U_TYP

      INTEGER IOS


      ! Variables ______________________________________________________

      LOGICAL MSG, STANDARD

      PARAMETER ( MSG = .TRUE. )

      CHARACTER ZTYP*20, ZUNITS*20, ZFLAG


      ! Initializations
      U_TYP = ' '
      IOS = 0

      ! Identify relevant signals ______________________________________

      ! Try first integral
      CALL INTEG_TYPE( U.DIMSYS, U.CHANFORM,
     & U.XTYP, U.XUNITS, U.YTYP, U.YUNITS, ZTYP, ZUNITS,
     & ZFLAG, STANDARD, MSG, IOS )
      IF ( IOS .NE. 0 ) RETURN

      IF ( STANDARD ) THEN ! Try second integral

        CALL INTEG_TYPE( U.DIMSYS, U.CHANFORM,
     &   U.XTYP, U.XUNITS, ZTYP, ZUNITS, ZTYP, ZUNITS,
     &   ZFLAG, STANDARD, MSG, IOS )
        IF ( IOS .NE. 0 ) RETURN

        IF ( STANDARD ) THEN ! Input is A

          U_TYP = 'A'

        ELSE ! One standard integral - Try a derivative

          CALL DIFFR_TYPE( U.DIMSYS, U.CHANFORM,
     &     U.XTYP, U.XUNITS, U.YTYP, U.YUNITS, ZTYP, ZUNITS,
     &     ZFLAG, STANDARD, MSG, IOS )
          IF ( IOS .NE. 0 ) RETURN

          IF ( STANDARD ) THEN ! Input is V
            U_TYP = 'V'
          ELSE ! Use input as A
            U_TYP = 'A'
          END IF

        END IF

      ELSE ! No standard integral - Try a derivative

        CALL DIFFR_TYPE( U.DIMSYS, U.CHANFORM,
     &   U.XTYP, U.XUNITS, U.YTYP, U.YUNITS, ZTYP, ZUNITS,
     &   ZFLAG, STANDARD, MSG, IOS )
        IF ( IOS .NE. 0 ) RETURN

        IF ( STANDARD ) THEN ! Try second derivative

          CALL DIFFR_TYPE( U.DIMSYS, U.CHANFORM,
     &     U.XTYP, U.XUNITS, ZTYP, ZUNITS, ZTYP, ZUNITS,
     &     ZFLAG, STANDARD, MSG, IOS )
          IF ( IOS .NE. 0 ) RETURN

          IF ( STANDARD ) THEN ! Input is D
            U_TYP = 'D'
          ELSE ! One standard derivative - Input is V
            U_TYP = 'V'
          END IF

        ELSE ! No standard integral or derivative - Use input as A

          U_TYP = 'A'

        END IF

      END IF

      RETURN
      END



      SUBROUTINE COMPUTE_SIGNALS( U, U_TYP, A, V, D, IOS )

!***********************************************************************
!* Computes the Relevant Integrals/Derivatives of the Current Input Signal
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'


      ! Arguments ______________________________________________________

      RECORD /UDS/  U

      CHARACTER U_TYP

      RECORD /UDS_Y/  A, V, D

      INTEGER IOS


      ! Variables ______________________________________________________


      LOGICAL MSG

      PARAMETER ( MSG = .TRUE. )

      INTEGER I

      DOUBLE PRECISION DD

      CHARACTER ZFLAG


      ! Initializations
      CALL UDS_SPEC_COPY( A, U )
      CALL UDS_SPEC_COPY( V, U )
      CALL UDS_SPEC_COPY( D, U )
      IOS = 0

      ! Compute relevant signals
      IF ( U_TYP .EQ. 'A' ) THEN ! Integrate twice
        DO I = U.NFP, U.NLP
          A.Y(I) = U.Y(I)
        END DO
        CALL INTEG_SN( U.DIMSYS, U.CHANFORM,
     &   U.X(U.NFP), A.Y(U.NFP), V.Y(U.NFP),
     &   U.NFP, U.NLP, U.DEL, U.INIVEL,
     &   U.XTYP, U.XUNITS, A.YTYP, A.YUNITS,
     &   V.YTYP, V.YUNITS, ZFLAG, ' ', DD, MSG, IOS )
        IF ( IOS .NE. 0 ) RETURN
        CALL INTEG_SN( U.DIMSYS, U.CHANFORM,
     &   U.X(U.NFP), V.Y(U.NFP), D.Y(U.NFP),
     &   U.NFP, U.NLP, U.DEL, U.INIVEL,
     &   U.XTYP, U.XUNITS, V.YTYP, V.YUNITS,
     &   D.YTYP, D.YUNITS, ZFLAG, ' ', DD, MSG, IOS )
        IF ( IOS .NE. 0 ) RETURN
      ELSE IF ( U_TYP .EQ. 'V' ) THEN ! Integrate & differentiate once
        DO I = U.NFP, U.NLP
          V.Y(I) = U.Y(I)
        END DO
        CALL INTEG_SN( U.DIMSYS, U.CHANFORM,
     &   U.X(U.NFP), V.Y(U.NFP), D.Y(U.NFP),
     &   U.NFP, U.NLP, U.DEL, U.INIVEL,
     &   U.XTYP, U.XUNITS, V.YTYP, V.YUNITS,
     &   D.YTYP, D.YUNITS, ZFLAG, ' ', DD, MSG, IOS )
        IF ( IOS .NE. 0 ) RETURN
        CALL DIFFR_SN( U.DIMSYS, U.CHANFORM,
     &   U.X(U.NFP), V.Y(U.NFP), A.Y(U.NFP),
     &   U.NFP, U.NLP, U.DEL, U.INIVEL,
     &   U.XTYP, U.XUNITS, V.YTYP, V.YUNITS,
     &   A.YTYP, A.YUNITS, ZFLAG, 'F', DD, MSG, IOS )
        IF ( IOS .NE. 0 ) RETURN
      ELSE IF ( U_TYP .EQ. 'D' ) THEN ! Differentiate twice
        DO I = U.NFP, U.NLP
          D.Y(I) = U.Y(I)
        END DO
        CALL DIFFR_SN( U.DIMSYS, U.CHANFORM,
     &   U.X(U.NFP), D.Y(U.NFP), V.Y(U.NFP),
     &   U.NFP, U.NLP, U.DEL, U.INIVEL,
     &   U.XTYP, U.XUNITS, D.YTYP, D.YUNITS,
     &   V.YTYP, V.YUNITS, ZFLAG, 'F', DD, MSG, IOS )
        IF ( IOS .NE. 0 ) RETURN
        CALL DIFFR_SN( U.DIMSYS, U.CHANFORM,
     &   U.X(U.NFP), V.Y(U.NFP), A.Y(U.NFP),
     &   U.NFP, U.NLP, U.DEL, U.INIVEL,
     &   U.XTYP, U.XUNITS, V.YTYP, V.YUNITS,
     &   A.YTYP, A.YUNITS, ZFLAG, 'F', DD, MSG, IOS )
        IF ( IOS .NE. 0 ) RETURN
      ELSE ! Signal type not identified
        IOS = 3
      END IF

      RETURN
      END



      SUBROUTINE OVERVIEW( DISPLAY_UNIT, U, A, V, D, DT )

!***********************************************************************
!* Displays an Overview of the Data and its Integrals
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'


      ! Arguments ______________________________________________________

      INTEGER DISPLAY_UNIT

      RECORD /UDS/  U

      RECORD /UDS_Y/  A, V, D

      DOUBLE PRECISION DT


      ! Variables ______________________________________________________

      LOGICAL EOF

      INTEGER N1, N2, IOS, I

      REAL A_MIN, A_MAX, V_MIN, V_MAX, D_MIN, D_MAX,
     & XA_MIN, XA_MAX, XV_MIN, XV_MAX, XD_MIN, XD_MAX, AI, VI, DI

      CHARACTER BUFF*132


      ! Get X range for overview
      CALL XRANGE( U, DT, N1, N2, EOF )
      IF ( EOF ) GO TO 199

      ! Write overview header
      BUFF = ' '
      DO I = 1, 4
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
      END DO
      WRITE( BUFF, '(11X,A,2X,A,2X,A)' )
     & A.YTYP, V.YTYP, D.YTYP
      CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
      WRITE( BUFF, '(11X,A,2X,A,2X,A)' )
     & A.YUNITS, V.YUNITS, D.YUNITS
      CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
      WRITE( BUFF, '(9X,3(2X,20(''_'')))' )
      CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )


      ! Obtain max and min values
      A_MAX = A.Y(N1)
      XA_MAX = U.X(N1)
      A_MIN = A_MAX
      XA_MIN = XA_MAX

      V_MAX = V.Y(N1)
      XV_MAX = U.X(N1)
      V_MIN = V_MAX
      XV_MIN = XV_MAX

      D_MAX = D.Y(N1)
      XD_MAX = U.X(N1)
      D_MIN = D_MAX
      XD_MIN = XD_MAX

      DO I = N1, N2
        AI = A.Y(I)
        IF ( AI .GT. A_MAX ) THEN
          A_MAX = AI
          XA_MAX = U.X(I)
        ELSE IF ( AI .LT. A_MIN ) THEN
          A_MIN = AI
          XA_MIN = U.X(I)
        END IF

        VI = V.Y(I)
        IF ( VI .GT. V_MAX ) THEN
          V_MAX = VI
          XV_MAX = U.X(I)
        ELSE IF ( VI .LT. V_MIN ) THEN
          V_MIN = VI
          XV_MIN = U.X(I)
        END IF

        DI = D.Y(I)
        IF ( DI .GT. D_MAX ) THEN
          D_MAX = DI
          XD_MAX = U.X(I)
        ELSE IF ( DI .LT. D_MIN ) THEN
          D_MIN = DI
          XD_MIN = U.X(I)
        END IF
      END DO

      ! Write entries for overview
      BUFF = ' '
      CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
      WRITE( BUFF, 602, IOSTAT=IOS )
     & ' Initial ', A.Y(N1), V.Y(N1), D.Y(N1)
      CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
      BUFF = ' '
      CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
      WRITE( BUFF, 602, IOSTAT=IOS )
     & '   Max   ', A_MAX, V_MAX, D_MAX
      CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
      WRITE( BUFF, 603, IOSTAT=IOS )
     & '   @ ', U.XTYP(:1),' = ',
     & XA_MAX, XV_MAX, XD_MAX
      CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
      BUFF = ' '
      CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
      WRITE( BUFF, 602, IOSTAT=IOS )
     & '   Min   ', A_MIN, V_MIN, D_MIN
      CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
      WRITE( BUFF, 603, IOSTAT=IOS )
     & '   @ ', U.XTYP(:1),' = ',
     & XA_MIN, XV_MIN, XD_MIN
      CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
      BUFF = ' '
      CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
      WRITE( BUFF, 602, IOSTAT=IOS )
     & '  Final  ', A.Y(N2), V.Y(N2), D.Y(N2)
      CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
      BUFF = ' '
      CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
      WRITE( BUFF, 602, IOSTAT=IOS )
     & ' Change  ', A.Y(N2)-A.Y(N1), V.Y(N2)-V.Y(N1),
     & D.Y(N2)-D.Y(N1)
      CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
      BUFF = ' '
      CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
      CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
  602 FORMAT(1X,A,3X,F16.6,2(6X,F16.6))
  603 FORMAT(1X,3A,3X,F16.6,2(6X,F16.6))

  199 RETURN
      END



      SUBROUTINE TABLE( DISPLAY_UNIT, U, A, V, D, DT )

!***********************************************************************
!* Produces a Table Listing of the Data and its Integrals
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'


      ! Arguments ______________________________________________________

      INTEGER DISPLAY_UNIT

      RECORD /UDS/  U

      RECORD /UDS_Y/  A, V, D

      DOUBLE PRECISION DT


      ! Variables ______________________________________________________

      LOGICAL EOF

      INTEGER N1, N2, NL, NS, IOS, I

      CHARACTER RESP, BUFF*132


      ! Get X range for table
      CALL XRANGE( U, DT, N1, N2, EOF )
      IF ( EOF ) GO TO 199

      ! Obtain sampling rate for table
      NL = N2 - N1 + 1
  101 WRITE( *, 601, IOSTAT=IOS ) NL
  601 FORMAT(/' Full table has ',I7,' lines'
     & //' Table specs? :   S(subsampling rate),   M(max # of lines)',
     & 10X,'[full table]'/' >> ',$)
      READ( *, '(BN,A1,I25)', IOSTAT=IOS, END=199 ) RESP, NS
      IF ( IOS .NE. 0 ) THEN
        WRITE( *, * ) '*** Invalid response'
        GO TO 101
      END IF
      CALL STR_UP( RESP )
      IF ( RESP .EQ. ' ' ) THEN
        NS = 1
      ELSE IF ( RESP .EQ. 'M' ) THEN
        NS = INT( (NL-1)/(MAX(NS,1)*(1.-1.E-7))*(1.-1.E-7)+1. )
      ELSE IF ( RESP .EQ. 'S' ) THEN
        ! NS specified
      ELSE
        WRITE( *, * ) '*** Invalid response'
        GO TO 101
      END IF

      ! Compute dimensions for table
      NS = MIN( MAX( NS,1), MAX(N2-N1,1) )
      N1 = MIN( MAX( NINT( REAL(N1)/NS )*NS, (A.NFP/NS)*NS ),
     & A.NLP/NS*NS )
      N2 = MIN( MAX( NINT( REAL(N2)/NS )*NS, N1 ), (A.NLP/NS)*NS )

      ! Write header and entries for table
      BUFF = ' '
      CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
      CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
      WRITE( BUFF, '(3(A19,1X),A19)' )
     & U.XTYP, A.YTYP, V.YTYP, D.YTYP
      CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
      WRITE( BUFF, '(3(A19,1X),A19)' )
     & U.XUNITS, A.YUNITS, V.YUNITS, D.YUNITS
      CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
      WRITE( BUFF, '(3(19(''_''),1X),19(''_''))' )
      CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
      DO I = N1, N2, NS
        WRITE( BUFF, '(3(1PG19.8,1X),1PG19.8)', IOSTAT=IOS )
     &   U.X(I), A.Y(I), V.Y(I), D.Y(I)
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
      END DO

  199 RETURN
      END



      SUBROUTINE XRANGE( U, DT, N1, N2, EOF )

!***********************************************************************
!* Gets an X Range from User
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'


      ! Arguments ______________________________________________________

      RECORD /UDS/  U

      DOUBLE PRECISION DT

      INTEGER N1

      INTEGER N2

      LOGICAL EOF


      ! Variables ______________________________________________________

      INTEGER IOS

      DOUBLE PRECISION X1, X2

      CHARACTER XVAL*25


      ! Obtain initial X value
  101 IF ( U.CHANFORM .EQ. 'Y' ) THEN
        WRITE( *, '(/5A,21X,A/'' >> '',$)' ) ' Initial ', U.XTYP,
     &   ' (',U.XUNITS,')?', '[tail]'
        READ( *, '(A)', END=199 ) XVAL
        IF ( XVAL .EQ. ' ' ) THEN
          N1 = U.NFP
        ELSE
          READ( XVAL, '(BN,F25.0)', IOSTAT=IOS ) X1
          IF ( IOS .NE. 0 ) THEN
            WRITE( *, * ) '*** Illegal value'
            GO TO 101
          END IF
          N1 = NINT( MAX( MIN( X1, U.NLP*DT ), U.NFP*DT ) / DT )
        END IF
      ELSE
        WRITE( *, '(/3A,38X,A/'' >> '',$)' ) ' Initial ', U.XTYP,
     &   ' index?', '[tail]'
        READ( *, '(A)', END=199 ) XVAL
        IF ( XVAL .EQ. ' ' ) THEN
          N1 = U.NFP
        ELSE
          READ( XVAL, '(BN,I25)', IOSTAT=IOS ) N1
          IF ( IOS .NE. 0 ) THEN
            WRITE( *, * ) '*** Illegal value'
            GO TO 101
          END IF
          N1 = MAX( MIN( N1, U.NLP ), U.NFP )
        END IF
      END IF

      ! Obtain final X value
  102 IF ( U.CHANFORM .EQ. 'Y' ) THEN
        WRITE( *, '(5A,21X,A/'' >> '',$)' ) ' Final   ', U.XTYP,
     &   ' (',U.XUNITS,')?', '[tail]'
        READ( *, '(A)', END=199 ) XVAL
        IF ( XVAL .EQ. ' ' ) THEN
          N2 = U.NLP
        ELSE
          READ( XVAL, '(BN,F25.0)', IOSTAT=IOS ) X2
          IF ( IOS .NE. 0 ) THEN
            WRITE( *, * ) '*** Illegal value'
            GO TO 102
          END IF
          N2 = NINT( MAX( MIN( X2, U.NLP*DT ), N1*DT ) / DT )
        END IF
      ELSE
        WRITE( *, '(3A,38X,A/'' >> '',$)' ) ' Final   ', U.XTYP,
     &   ' index?', '[tail]'
        READ( *, '(A)', END=199 ) XVAL
        IF ( XVAL .EQ. ' ' ) THEN
          N2 = U.NLP
        ELSE
          READ( XVAL, '(BN,I25)', IOSTAT=IOS ) N2
          IF ( IOS .NE. 0 ) THEN
            WRITE( *, * ) '*** Illegal value'
            GO TO 101
          END IF
          N2 = MAX( MIN( N2, U.NLP ), U.NFP )
        END IF
      END IF

      EOF = .FALSE.
      RETURN

      ! Return forced by EOF
  199 EOF = .TRUE.
      RETURN

      END



      SUBROUTINE SPEC_DISPLAY( DISPLAY_UNIT, S )

!***********************************************************************
!* Performs Specification Display
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      INTEGER DISPLAY_UNIT

      RECORD /UDS/  S


      ! Variables ______________________________________________________

      INTEGER IOS

      CHARACTER RESP, BUFF*132


      ! Prompt for specification group
  101 WRITE( *, '(/2A/17X,A/'' >> '',$)' ) ' Spec group? :   ',
     & 'T - Test,  V - Veh/Cmp,  O - Occupant,  I - Injury,  2 - Cmp2',
     & 'C - Curve,  M - Measurement,  S - Supplementary'
      READ( *, '(A)', END=199 ) RESP
      CALL STR_UP( RESP )

      ! Display selected group
      IF ( RESP .EQ. ' ' ) THEN

        RETURN

      ELSE IF ( RESP .EQ. 'T' ) THEN

        BUFF = ' '
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  FILEVER = ',
     &   S.FILEVER(:L_TRIM(S.FILEVER))
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  NCHAN = ', S.NCHAN
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  FILEFORM = ',
     &   S.FILEFORM(:L_TRIM(S.FILEFORM))
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  NUMFORM = ', S.NUMFORM
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  DIMSYS = ', S.DIMSYS
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  TSTSRC = ',
     &   S.TSTSRC(:L_TRIM(S.TSTSRC))
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  TSTNO = ', S.TSTNO
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  TSTNAM = ',
     &   S.TSTNAM(:L_TRIM(S.TSTNAM))
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  TITLE = ',
     &   S.TITLE(:L_TRIM(S.TITLE))
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  TSTPRF = ',
     &   S.TSTPRF(:L_TRIM(S.TSTPRF))
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  TSTREF = ',
     &   S.TSTREF(:L_TRIM(S.TSTREF))
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  TSTCFN = ', S.TSTCFN
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  IMPANG = ', S.IMPANG
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  CLSSPD = ', S.CLSSPD
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )

      ELSE IF ( RESP .EQ. 'V' ) THEN

        BUFF = ' '
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  CMPNO = ', S.CMPNO, '  (= VEHNO)'
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  CMPTYP = ', S.CMPTYP
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  CMPDSC = ',
     &   S.CMPDSC(:L_TRIM(S.CMPDSC)), '  (= VEHDSC)'
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  MAKE = ', S.MAKE(:L_TRIM(S.MAKE))
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  MODEL = ',
     &   S.MODEL(:L_TRIM(S.MODEL))
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  YEAR = ', S.YEAR
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  BODY = ', S.BODY(:L_TRIM(S.BODY))
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  ENGINE = ', S.ENGINE
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  CMPWGT = ', S.CMPWGT,
     &   '  (= VEHTWT)'
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  CMPSPD = ', S.CMPSPD,
     &   '  (= VEHSPD)'
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )

      ELSE IF ( RESP .EQ. 'O' ) THEN

        BUFF = ' '
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  OCCTYP = ', S.OCCTYP
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  OCCAGE = ', S.OCCAGE
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  OCCSEX = ', S.OCCSEX
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  OCCWT = ', S.OCCWT
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  DUMSIZ = ', S.DUMSIZ
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  RESTR1 = ',
     &   S.RESTR1(:L_TRIM(S.RESTR1))
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  RESTR2 = ',
     &   S.RESTR2(:L_TRIM(S.RESTR2))
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )

      ELSE IF ( RESP .EQ. 'I' ) THEN

        BUFF = ' '
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  HIC = ', S.HIC
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  T1 = ', S.T1
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  T2 = ', S.T2
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  HICDTUP = ', S.HICDTUP
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  CLIP3M = ', S.CLIP3M
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  CSI = ', S.CSI
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  AIS = ', S.AIS
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )

      ELSE IF ( RESP .EQ. '2' ) THEN

        BUFF = ' '
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  CMPNO2 = ', S.CMPNO2,
     &   '  (= VEHNO2)'
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  CMPTYP2 = ', S.CMPTYP2
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  CMPDSC2 = ',
     &   S.CMPDSC2(:L_TRIM(S.CMPDSC2)), '  (= VEHDSC2)'
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  MAKE2 = ',
     &   S.MAKE2(:L_TRIM(S.MAKE2))
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  MODEL2 = ',
     &   S.MODEL2(:L_TRIM(S.MODEL2))
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  YEAR2 = ', S.YEAR2
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  BODY2 = ',
     &   S.BODY2(:L_TRIM(S.BODY2))
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  ENGINE2 = ', S.ENGINE2
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  CMPWGT2 = ', S.CMPWGT2,
     &   '  (= VEHTWT2)'
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  CMPSPD2 = ', S.CMPSPD2,
     &   '  (= VEHSPD2)'
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  OCCTYP2 = ', S.OCCTYP2
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  OCCAGE2 = ', S.OCCAGE2
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  OCCSEX2 = ', S.OCCSEX2
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  OCCWT2 = ', S.OCCWT2
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  DUMSIZ2 = ', S.DUMSIZ2
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  RESTR12 = ',
     &   S.RESTR12(:L_TRIM(S.RESTR12))
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  RESTR22 = ',
     &   S.RESTR22(:L_TRIM(S.RESTR22))
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )

      ELSE IF ( RESP .EQ. 'C' ) THEN

        BUFF = ' '
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  ICHAN = ', S.ICHAN
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  CHANFORM = ', S.CHANFORM
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  CURNO = ', S.CURNO
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  CURNAM = ', S.CURNAM
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  SENATT = ',
     &   S.SENATT(:L_TRIM(S.SENATT))
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  SENLOC = ', S.SENLOC
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  SENNUM = ', S.SENNUM
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  BANDNO = ', S.BANDNO
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  GAGNO = ', S.GAGNO
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  AXIS = ', S.AXIS
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  YTYP = ',
     &   S.YTYP(:L_TRIM(S.YTYP))
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  YUNITS = ',
     &   S.YUNITS(:L_TRIM(S.YUNITS))
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  XTYP = ',
     &   S.XTYP(:L_TRIM(S.XTYP))
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  XUNITS = ',
     &   S.XUNITS(:L_TRIM(S.XUNITS))
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  STATUS = ',
     &   S.STATUS(:L_TRIM(S.STATUS))
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  CURTYP = ',
     &   S.CURTYP(:L_TRIM(S.CURTYP))
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  CURDSC = ',
     &   S.CURDSC(:L_TRIM(S.CURDSC))
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )

      ELSE IF ( RESP .EQ. 'M' ) THEN

        BUFF = ' '
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  NFP = ', S.NFP
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  NLP = ', S.NLP
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  DEL = ', S.DEL
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  INIVEL = ', S.INIVEL
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  PREF = ', S.PREF
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  FCUT = ', S.FCUT
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  FCOR = ', S.FCOR
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  FSTP = ', S.FSTP
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  SCLFAC = ', S.SCLFAC
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )

      ELSE IF ( RESP .EQ. 'S' ) THEN

        BUFF = ' '
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  ID1 = ', S.ID1
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  ID2 = ', S.ID2
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  ID3 = ', S.ID3
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  ID4 = ', S.ID4
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  ID5 = ', S.ID5
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  RD1 = ', S.RD1
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  RD2 = ', S.RD2
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  RD3 = ', S.RD3
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  RD4 = ', S.RD4
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  RD5 = ', S.RD5
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  CD1 = ', S.CD1(:L_TRIM(S.CD1))
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )
        WRITE( BUFF, * ) '  CD2 = ', S.CD2(:L_TRIM(S.CD2))
        CALL WRITE_DEV( DISPLAY_UNIT, BUFF, IOS )

      ELSE

        WRITE( *, '(/A/)' ) ' *** Unrecognized input'
        GO TO 101

      END IF

      ! Loop for next spec group
      GO TO 101

      ! EOF handler
  199 RETURN

      END



      SUBROUTINE CAL_MOD( U, TTYP, DT, ZTYP, ZUNITS, Z )

!***********************************************************************
!* Applies Segmented Linear Calibration Function to the Pulse
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      RECORD /UDS/  U

      CHARACTER TTYP

      DOUBLE PRECISION DT

      CHARACTER*(*) ZTYP

      CHARACTER*(*) ZUNITS

      REAL Z(NFP_L:NLP_U) ! Might be X or Y array


      ! Variables ______________________________________________________

      INTEGER IOS, I1, I2, I

      DOUBLE PRECISION TM1, CFAC1, TM2, CFAC2, FACSLP

      CHARACTER UNITS_S*20, CVAL*25, TVAL*25


      ! Display axis type info
      WRITE( *, '(/5X,4A)' ) ' Data is ', ZTYP(:L_TRIM(ZTYP)),
     & ' in ', ZUNITS(:L_TRIM(ZUNITS))

      ! Obtain starting calibration factor
      TM1 = U.NFP * DT
      I1 = U.NFP
  101 WRITE( *, 601, IOSTAT=IOS ) TTYP, TM1, 1.0
      READ( *, '(A)', END=199 ) CVAL
      IF ( CVAL .EQ. ' ' ) THEN
        CFAC1 = 1.D0
      ELSE
        READ( CVAL, '(BN,F25.0)', IOSTAT=IOS ) CFAC1
        IF ( IOS .NE. 0 ) THEN
          WRITE( *, * ) '*** Illegal value'
          GO TO 101
        END IF
      END IF
      WRITE( *, 602, IOSTAT=IOS ) CFAC1, TTYP, TM1

      ! Obtain end of segment x-val/index
  102 WRITE( *, 603, IOSTAT=IOS ) TTYP, U.NLP * DT
      READ( *, '(A)', END=199 ) TVAL
      IF ( TVAL .EQ. ' ' ) THEN
        TM2 = U.NLP * DT
      ELSE
        READ( TVAL, '(BN,F25.0)', IOSTAT=IOS ) TM2
        IF ( IOS .NE. 0 ) THEN
          WRITE( *, * ) '*** Illegal value'
          GO TO 102
        END IF
        TM2 = MIN( MAX( TM2, TM1 ), U.NLP * DT )
      END IF

      ! Compute end of segment index
      I2 = INT( TM2/DT + ( 2.D0 + 1.D-14 ) * NINT( ABS( TM2/DT ) ) ) -
     & 2 * NINT( ABS( TM2/DT ) ) ! Round down

      ! Obtain calibration factor for end of segment
  103 WRITE( *, 601, IOSTAT=IOS ) TTYP, TM2, CFAC1
      READ( *, '(A)', END=199 ) CVAL
      IF ( CVAL .EQ. ' ' ) THEN
        CFAC2 = CFAC1
      ELSE
        READ( CVAL, '(BN,F25.0)', IOSTAT=IOS ) CFAC2
        IF ( IOS .NE. 0 ) THEN
          WRITE( *, * ) '*** Illegal value'
          GO TO 103
        END IF
      END IF
      WRITE( *, 602, IOSTAT=IOS ) CFAC2, TTYP, TM2

      ! Apply linear calibration function to data segment
      IF ( TM2 .EQ. TM1 ) THEN
        FACSLP = 0.D0
      ELSE
        FACSLP = ( CFAC2 - CFAC1 ) / ( TM2 - TM1 )
      END IF
      DO I = I1, I2
        Z(I) = REAL( Z(I) * ( CFAC1 + (I*DT-TM1) * FACSLP ) )
      END DO

      ! Set up for next segment if not at end of pulse
      IF ( I2 .LT. U.NLP ) THEN
        TM1 = TM2
        I1 = I2+1
        CFAC1 = CFAC2
        GO TO 102
      END IF

      ! Prompt for new units if global change
      IF ( ( CFAC2 .EQ. CFAC1 ) .AND. ( CFAC2 .NE. 1.D0 ) .AND.
     & ( I1 .EQ. U.NFP ) .AND. ( I2 .EQ. U.NLP ) ) THEN
        WRITE( *, 604 ) ZUNITS
        READ( *, '(A)', END=199 ) UNITS_S
        IF ( UNITS_S .NE. ' ' ) ZUNITS = UNITS_S
      END IF

  199 RETURN

  601 FORMAT(/' Calibration factor at ',A,' = ',F16.6,' ?',17X,
     & '[',F16.6,']'/' >> ',$)
  602 FORMAT(' *** Factor of  ',F16.6,'  at  ',A,' = ',F16.6)
  603 FORMAT(/' Segment end ',A,'?',47X,'[',F16.6,']'/' >> ',$)
  604 FORMAT(/' New Units value?',41X,'[',A,']'/' >> ',$)

      END



      SUBROUTINE BIAS_MOD( U, TTYP, DT, ZTYP, ZUNITS, Z )

!***********************************************************************
!* Applies Segmented Linear Bias Function to the Pulse
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      RECORD /UDS/  U

      CHARACTER TTYP

      DOUBLE PRECISION DT

      CHARACTER*(*) ZTYP

      CHARACTER*(*) ZUNITS

      REAL Z(NFP_L:NLP_U) ! Might be X or Y array


      ! Variables ______________________________________________________

      INTEGER IOS, I1, I2, I

      DOUBLE PRECISION TM1, BIAS1, TM2, BIAS2, BSLP

      CHARACTER BVAL*25, TVAL*25


      ! Display axis type info
      WRITE( *, '(/5X,4A)' ) ' Data is ', ZTYP(:L_TRIM(ZTYP)),
     & ' in ', ZUNITS(:L_TRIM(ZUNITS))

      ! Obtain starting bias correction
      TM1 = U.NFP * DT
      I1 = U.NFP
  101 WRITE( *, 601, IOSTAT=IOS ) TTYP, TM1, 0.
      READ( *, '(A)', END=199 ) BVAL
      IF ( BVAL .EQ. ' ' ) THEN
        BIAS1 = 0.
      ELSE
        READ( BVAL, '(BN,F25.0)', IOSTAT=IOS ) BIAS1
        IF ( IOS .NE. 0 ) THEN
          WRITE( *, * ) '*** Illegal value'
          GO TO 101
        END IF
      END IF
      WRITE( *, 602, IOSTAT=IOS ) BIAS1, TTYP, TM1

      ! Obtain end of segment x-val/index
  102 WRITE( *, 603, IOSTAT=IOS ) TTYP, U.NLP * DT
      READ( *, '(A)', END=199 ) TVAL
      IF ( TVAL .EQ. ' ' ) THEN
        TM2 = U.NLP * DT
      ELSE
        READ( TVAL, '(BN,F25.0)', IOSTAT=IOS ) TM2
        IF ( IOS .NE. 0 ) THEN
          WRITE( *, * ) '*** Illegal value'
          GO TO 102
        END IF
        TM2 = MIN( MAX( TM2, TM1 ), U.NLP * DT )
      END IF

      ! Compute end of segment index
      I2 = INT( TM2/DT + ( 2.D0 + 1.D-14 ) * NINT( ABS( TM2/DT ) ) ) -
     & 2 * NINT( ABS( TM2/DT ) ) ! Round down

      ! Obtain bias correction for end of segment
  103 WRITE( *, 601, IOSTAT=IOS ) TTYP, TM2, BIAS1
      READ( *, '(A)', END=199 ) BVAL
      IF ( BVAL .EQ. ' ' ) THEN
        BIAS2 = BIAS1
      ELSE
        READ( BVAL, '(BN,F25.0)', IOSTAT=IOS ) BIAS2
        IF ( IOS .NE. 0 ) THEN
          WRITE( *, * ) '*** Illegal value'
          GO TO 103
        END IF
      END IF
      WRITE( *, 602, IOSTAT=IOS ) BIAS2, TTYP, TM2

      ! Apply linear bias correction to data segment
      IF ( TM2 .EQ. TM1 ) THEN
        BSLP = 0.D0
      ELSE
        BSLP = ( BIAS2 - BIAS1 ) / ( TM2 - TM1 )
      END IF
      DO I = I1, I2
        Z(I) = REAL( Z(I) + ( BIAS1 + (I*DT-TM1) * BSLP ) )
      END DO

      ! Set up for next segment if not at end of pulse
      IF ( I2 .LT. U.NLP ) THEN
        TM1 = TM2
        I1 = I2+1
        BIAS1 = BIAS2
        GO TO 102
      END IF

  199 RETURN

  601 FORMAT(/' Bias change at ',A,' = ',F16.6,' ?',24X,
     & '[',F16.6,']'/' >> ',$)
  602 FORMAT(' *** Bias change of  ',F16.6,'  at  ',A,' = ',F16.6)
  603 FORMAT(/' Segment end ',A,'?',47X,'[',F16.6,']'/' >> ',$)

      END



      SUBROUTINE INTERP_MOD( U, TTYP, DT, ZTYP, ZUNITS, Z )

!***********************************************************************
!* Linearly Interpolates the Pulse Between Two Points
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      RECORD /UDS/  U

      CHARACTER TTYP

      DOUBLE PRECISION DT

      CHARACTER*(*) ZTYP

      CHARACTER*(*) ZUNITS

      REAL Z(NFP_L:NLP_U) ! Might be X or Y array


      ! Variables ______________________________________________________

      INTEGER IOS, I1, I2, I

      DOUBLE PRECISION TM1, TM2, Z1, Z2, SLP

      CHARACTER TVAL*25


      ! Display axis type info
      WRITE( *, '(/5X,4A)' ) ' Data is ', ZTYP(:L_TRIM(ZTYP)),
     & ' in ', ZUNITS(:L_TRIM(ZUNITS))

      ! Obtain starting x-val/index
  101 WRITE( *, 601, IOSTAT=IOS ) TTYP, U.NFP * DT
      READ( *, '(A)', END=199 ) TVAL
      IF ( TVAL .EQ. ' ' ) THEN
        TM1 = U.NFP * DT
      ELSE
        READ( TVAL, '(BN,F25.0)', IOSTAT=IOS ) TM1
        IF ( IOS .NE. 0 ) THEN
          WRITE( *, * ) '*** Illegal value'
          GO TO 101
        END IF
        TM1 = MAX( TM1, U.NFP * DT )
      END IF

      ! Obtain end x-val/index
  102 WRITE( *, 602, IOSTAT=IOS ) TTYP, U.NLP * DT
      READ( *, '(A)', END=199 ) TVAL
      IF ( TVAL .EQ. ' ' ) THEN
        TM2 = U.NLP * DT
      ELSE
        READ( TVAL, '(BN,F25.0)', IOSTAT=IOS ) TM2
        IF ( IOS .NE. 0 ) THEN
          WRITE( *, * ) '*** Illegal value'
          GO TO 102
        END IF
        TM2 = MIN( MAX( TM2, TM1 ), U.NLP * DT )
      END IF

      ! Compute interpolation segment indices and tail values
      I1 = NINT( TM1/DT )
      I2 = NINT( TM2/DT )
      Z1 = Z(I1)
      Z2 = Z(I2)

      ! Linearly interpolate
      IF ( I2 .EQ. I1 ) THEN
        SLP = 0.D0
      ELSE
        SLP = ( Z2 - Z1 ) / ( I2 - I1 )
      END IF
      DO I = I1+1, I2-1
        Z(I) = REAL( Z1 + ( I - I1 ) * SLP )
      END DO

  199 RETURN

  601 FORMAT(/' Interpolation start ',A,'?',39X,'[',F16.6,']'/' >> ',$)
  602 FORMAT(/' Interpolation end ',A,'?',41X,'[',F16.6,']'/' >> ',$)

      END



      SUBROUTINE VAL_MOD( U, TTYP, DT, ZTYP, ZUNITS, Z )

!***********************************************************************
!* Performs Modification of Individual Data Values
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      RECORD /UDS/  U

      CHARACTER TTYP

      DOUBLE PRECISION DT

      CHARACTER*(*) ZTYP

      CHARACTER*(*) ZUNITS

      REAL Z(NFP_L:NLP_U) ! Might be X or Y array


      ! Variables ______________________________________________________

      INTEGER IOS, I

      REAL ZI

      DOUBLE PRECISION TM1

      CHARACTER ZVAL*25


      ! Display axis type info
      WRITE( *, '(/5X,4A)' ) ' Data is ', ZTYP(:L_TRIM(ZTYP)),
     & ' in ', ZUNITS(:L_TRIM(ZUNITS))

      ! Obtain time/freq value for start of changes
  101 WRITE( *, 601 ) TTYP
      READ( *, '(BN,F25.0)', IOSTAT=IOS, END=199 ) TM1
      IF ( IOS .NE. 0 ) THEN
        WRITE( *, * ) '*** Illegal value'
        GO TO 101
      END IF

      ! Compute starting index
      I = NINT( MAX( MIN( TM1, U.NLP * DT ), U.NFP * DT ) / DT )

      ! Write Current Array Position and Value / Prompt for New Value
  102 WRITE( *, 602, IOSTAT=IOS ) I, TTYP, I * DT, Z(I)
      WRITE( *, 603 )
      READ( *, '(A)', END=199 ) ZVAL
      CALL STR_UP( ZVAL )

      ! Perform specified movement or value change
      IF ( ( ZVAL .EQ. 'N' ) .OR. ( ZVAL .EQ. ' '  ) ) THEN ! Next value
        IF ( I .GE. U.NLP ) WRITE( *, * ) '*** At last point'
        I = MIN( I+1, U.NLP )
        GO TO 102
      ELSE IF ( ZVAL .EQ. 'B' ) THEN ! Previous value
        IF ( I .LE. U.NFP ) WRITE( *, * ) '*** At first point'
        I = MAX( I-1, U.NFP )
        GO TO 102
      ELSE IF ( ZVAL .EQ. 'J' ) THEN ! Jump to start prompt
        GO TO 101
      ELSE IF ( ZVAL .EQ. 'D' ) THEN ! Done with changes
        RETURN
      ELSE ! Apply new value
        READ( ZVAL, '(BN,F25.0)', IOSTAT=IOS ) ZI
        IF ( IOS .NE. 0 ) THEN
          WRITE( *, * ) '*** Illegal value'
          GO TO 102
        END IF

        WRITE( *, 604, IOSTAT=IOS ) ZI, TTYP, I * DT
        Z(I) = ZI
        I = MIN( I+1, NLP_U )
        GO TO 102
      END IF

  199 RETURN

  601 FORMAT(/' ',A,' for value change?',55X,'[0.0]'/' >> ',$)
  602 FORMAT(/'    I = ',I5,5X,A,' = ',1PG19.8,5X,'Value = ',1PG19.8)
  603 FORMAT(/' New value? :   ( N - Next,  B - Backup,  ',
     & 'J - Jump,  D - Done )',11X,'[Next]'/' >> ',$)
  604 FORMAT(' *** Value changed to  ',1PG19.8,'  at  ',
     & A,' = ',1PG19.8)

      END



      SUBROUTINE SHIFT_MOD( U, TTYP, TUN, DT )

!***********************************************************************
!* Performs Shifting of the Entire Pulse
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      RECORD /UDS/  U

      CHARACTER TTYP

      CHARACTER*(*) TUN

      DOUBLE PRECISION DT


      ! Variables ______________________________________________________

      INTEGER NS, IOS, I

      DOUBLE PRECISION TS

      CHARACTER RESP


      ! Obtain shift value
  101 WRITE( *, 601, IOSTAT=IOS ) TTYP, TUN
      READ( *, '(BN,F25.0)', IOSTAT=IOS, END=199 ) TS
      IF ( IOS .NE. 0 ) THEN
        WRITE( *, * ) '*** Illegal value'
        GO TO 101
      END IF
      NS = NINT( TS / DT )
      WRITE( *, 602, IOSTAT=IOS ) TTYP, NS * DT, TUN(:L_TRIM(TUN))

      ! Perform specified shift
      IF ( NS .GT. 0 ) THEN ! Shift ahead
        DO I = NLP_U, NFP_L+NS, -1
          IF ( U.CHANFORM .EQ. 'X-Y' ) U.X(I) = U.X(I-NS)
          U.Y(I) = U.Y(I-NS)
        END DO
        DO I = MIN( NFP_L+NS-1, NLP_U ), NFP_L, -1
          IF ( U.CHANFORM .EQ. 'X-Y' ) U.X(I) = 0.
          U.Y(I) = 0.
        END DO
      ELSE IF ( NS .LT. 0 ) THEN ! Shift back
        DO I = NFP_L, NLP_U+NS
          IF ( U.CHANFORM .EQ. 'X-Y' ) U.X(I) = U.X(I-NS)
          U.Y(I) = U.Y(I-NS)
        END DO
        DO I = MAX( NLP_U+NS+1, NFP_L ), NLP_U
          IF ( U.CHANFORM .EQ. 'X-Y' ) U.X(I) = 0.
          U.Y(I) = 0.
        END DO
      END IF

      ! Shift index range
      WRITE( *, '(/A,38X,A/'' >> '',$)' )
     & ' Shift array index range also?   (Y/N)', '[No]'
      READ( *, '(A)', END=199 ) RESP
      CALL STR_UP( RESP )
      IF ( RESP .EQ. 'Y' ) THEN
        U.NFP = MAX( MIN( U.NFP+NS, NLP_U ), NFP_L )
        U.NLP = MAX( MIN( U.NLP+NS, NLP_U ), NFP_L )
      END IF

  199 RETURN

  601 FORMAT(/' ',A,' shift ',A,' ?',42X,'[0.0]'/' >> ',$)
  602 FORMAT(' *** ',A,' shift of  ',F16.6,1X,A)

      END



      SUBROUTINE RANGE_MOD( U, TTYP, TUN, DT )

!***********************************************************************
!* Performs Curve Range Truncation/Extension
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'


      ! Arguments ______________________________________________________

      RECORD /UDS/  U

      CHARACTER TTYP

      CHARACTER*(*) TUN

      DOUBLE PRECISION DT


      ! Variables ______________________________________________________

      INTEGER IOS

      DOUBLE PRECISION TFP, TLP

      CHARACTER TVAL*25


      ! Obtain initial range value for pulse domain
  101 WRITE( *, 601, IOSTAT=IOS ) TTYP, TUN, U.NFP * DT
      READ( *, '(A)', END=199 ) TVAL
      IF ( TVAL .NE. ' ' ) THEN
        READ( TVAL, '(BN,F25.0)', IOSTAT=IOS ) TFP
        IF ( IOS .NE. 0 ) THEN
          WRITE( *, * ) '*** Illegal value'
          GO TO 101
        END IF
        U.NFP = NINT( MIN( MAX( TFP, NFP_L * DT ), NLP_U * DT ) / DT )
        WRITE( *, 603, IOSTAT=IOS ) 'Initial ', TTYP, U.NFP * DT, TUN
      END IF

      ! Obtain final range value for pulse domain
  102 WRITE( *, 602, IOSTAT=IOS ) TTYP, TUN, U.NLP * DT
      READ( *, '(A)', END=199 ) TVAL
      IF ( TVAL .NE. ' ' ) THEN
        READ( TVAL, '(BN,F25.0)', IOSTAT=IOS ) TLP
        IF ( IOS .NE. 0 ) THEN
          WRITE( *, * ) '*** Illegal value'
          GO TO 102
        END IF
        U.NLP = NINT( MIN( MAX( TLP, NFP_L * DT ), NLP_U * DT ) / DT )
        WRITE( *, 603, IOSTAT=IOS ) 'Final ', TTYP, U.NLP * DT, TUN
      END IF

  199 RETURN

  601 FORMAT(/' Initial pulse ',A,1X,A,' ?',21X,'[',F16.6,']'/
     & ' >> ',$)
  602 FORMAT(/' Final pulse   ',A,1X,A,' ?',21X,'[',F16.6,']'/
     & ' >> ',$)
  603 FORMAT(' *** ',2A,' changed to  ',F16.6,1X,A)

      END



      SUBROUTINE SUBSAMP( U, DT )

!***********************************************************************
!* Performs Curve Subsampling
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'


      ! Arguments ______________________________________________________

      RECORD /UDS/  U

      DOUBLE PRECISION DT


      ! Variables ______________________________________________________

      INTEGER NS, NFP_N, NLP_N, IOS, I

      CHARACTER RESP


      ! Obtain subsampling rate
  101 WRITE( *, '(/A,59X,A/'' >> '',$)' ) ' Subsampling rate?', '[1]'
      READ( *, '(BN,I25)', IOSTAT=IOS, END=199 ) NS
      IF ( ( IOS .NE. 0 ) .OR. ( NS .LT. 0 ) ) THEN
        WRITE( *, * ) '*** Illegal value'
        GO TO 101
      END IF
      IF ( ( NS .EQ. 1 ) .OR. ( NS .EQ. 0 ) ) RETURN

      ! Compute new array range
      NFP_N = NINT( ( U.NFP + .9999999 ) / NS )
      NLP_N = MAX( NFP_N, NINT( ( U.NLP - .9999999 ) / NS ) )

      ! Perform subsampling
      DO I = MAX( 0, NFP_N ), MIN( NLP_N, U.NLP / NS )
        U.X(I) = U.X(I*NS)
        U.Y(I) = U.Y(I*NS)
      END DO
      DO I = MIN( 0, NLP_N ), MAX( NFP_N, U.NFP / NS ), -1
        U.X(I) = U.X(I*NS)
        U.Y(I) = U.Y(I*NS)
      END DO
      U.NFP = NFP_N
      U.NLP = NLP_N

      ! Scale U.DEL
      IF ( U.DEL .NE. 0. ) THEN
        WRITE( *, '(/A,46X,A/'' >> '',$)' )
     &   ' Scale DEL step also?   (Y/N)','[Yes]'
        READ( *, '(A)', END=199 ) RESP
        CALL STR_UP( RESP )
        IF ( RESP .NE. 'N' ) THEN
          DT = DT * NS
          U.DEL = U.DEL * NS
        END IF
      END IF

  199 RETURN
      END



      SUBROUTINE UDS_CREATE( U )

!***********************************************************************
!* Creates a New UDS File
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'


      ! Arguments ______________________________________________________

      RECORD /UDS/  U


      ! Variables ______________________________________________________

      LOGICAL S_NFP, S_NLP, S_DEL, EOF


      ! Get basic UDS file spec's
  101 CALL GET_SPECS( U, S_NFP, S_NLP, S_DEL, EOF )
      IF ( EOF ) RETURN

      ! Read measurement file
  102 IF ( ( U.CHANFORM .EQ. 'Y' ) .OR. ( U.CHANFORM .EQ. 'X-Y' ) )
     & CALL GET_FILE( U, S_NFP, S_NLP, S_DEL )

      RETURN
      END



      SUBROUTINE GET_SPECS( S, S_NFP, S_NLP, S_DEL, EOF )

!***********************************************************************
!* Gets Basic UDS File Specifications
!***********************************************************************

      ! Headers
      INCLUDE 'platform.fi'
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      RECORD /UDS_SPEC/  S

      LOGICAL S_NFP

      LOGICAL S_NLP

      LOGICAL S_DEL

      LOGICAL EOF


      ! Variables ______________________________________________________


      RECORD /UDS_CONTROL/  CR

      LOGICAL MSG

      PARAMETER ( MSG = .TRUE. )

      INTEGER IOS

      CHARACTER IN_LINE*255, NAME*25, VALUE*132,
     & SPEC_FILE*255, RESP, YUNITS*20, XUNITS*20


      ! Functions ______________________________________________________

      CHARACTER DS_UNITS*20

      EXTERNAL DS_UNITS


      ! Initialize UDS fields
  101 CALL UDS_INIT( S )
      CALL UDS_CONTROL_INIT( CR )

      ! Get UDS file for spec's
      WRITE( *, '(/A,39X,A/'' >> '',$)' )
     & ' UDS file with base specifications?', '[none]'
      READ( *, '(A)', END=199 ) SPEC_FILE
      IF ( SPEC_FILE .NE. ' ' ) THEN ! Read spec file

        CR.SPECS_ONLY = .TRUE.
        CALL UDS_READ( SPEC_FILE, S, CR, IOS )
        IF ( IOS .NE. 0 ) GO TO 101
        WRITE( *, '(/A,12X,A/'' >> '',$)' )
     &   ' Use index range (NFP-NLP) and step (DEL) from base file?'//
     &   '  (Y/N)', '[no]'
        READ( *, '(A)', END=101 ) RESP
        CALL STR_UP( RESP )
        IF ( RESP .EQ. 'Y' ) THEN
          S_NFP = .TRUE.
          S_NLP = .TRUE.
          S_DEL = .TRUE.
        ELSE
          S_NFP = .FALSE.
          S_NLP = .FALSE.
          S_DEL = .FALSE.
          S.NFP = 0
          S.NLP = 0
          S.DEL = 0.
        END IF

      ELSE ! Prompt for basic spec's

        ! Set key field defaults
        S.FILEVER = 'UDS-1992'
        S.AXIS = 'XG'
        S_NFP = .FALSE.
        S_NLP = .FALSE.
        S_DEL = .FALSE.

        ! Dimensional system
  102   WRITE( *, '(/A,12X,A/'' >> '',$)' )
     &   ' Dimensional system?   ( M - Metric,  S - SI,  E - English )',
     &   '[Metric]'
        READ( *, '(A)', END=101 ) RESP
        CALL STR_UP( RESP )
        IF ( .NOT. ANY_CHARS( RESP, ' MSE' ) ) THEN
          WRITE( *, * ) '*** Unacceptable response'
          GO TO 102
        END IF
        IF ( RESP .EQ. 'E' ) THEN
          S.DIMSYS = 'ENG'
        ELSE IF ( RESP .EQ. 'S' ) THEN
          S.DIMSYS = 'SI'
        ELSE
          S.DIMSYS = 'MET'
        END IF

        ! Channel format
  103   WRITE( *, '(/A,51X,A/'' >> '',$)' )
     &   ' Channel format?   (Y/X-Y)', '[Y]'
        READ( *, '(A)', END=101 ) S.CHANFORM
        CALL STR_UP( S.CHANFORM )
        IF ( ( S.CHANFORM .EQ. 'Y' ) .OR. ( S.CHANFORM .EQ. ' ' ) ) THEN
          S.CHANFORM = 'Y'
        ELSE IF ( S.CHANFORM .NE. 'X-Y' ) THEN
          WRITE( *, * ) '*** Unacceptable response'
          GO TO 103
        END IF

        ! Y data type
        WRITE( *, '(/A,53X,A/'' >> '',$)' )
     &   ' Y data type?', '[ACCELERATION]'
        READ( *, '(A)', END=101 ) S.YTYP
        CALL STR_UP( S.YTYP )
        IF ( S.YTYP .EQ. ' ' ) S.YTYP = 'ACCELERATION'
        YUNITS = DS_UNITS( S.YTYP, S.DIMSYS )

        ! Y units
        WRITE( *, '(/A,49X,A/'' >> '',$)' )
     &   ' Y units?', '['//YUNITS//']'
        READ( *, '(A)', END=101 ) S.YUNITS
        CALL STR_UP( S.YUNITS )
        IF ( S.YUNITS .EQ. ' ' ) S.YUNITS = YUNITS

        ! X data type
        WRITE( *, '(/A,61X,A/'' >> '',$)' )
     &   ' X data type?', '[TIME]'
        READ( *, '(A)', END=101 ) S.XTYP
        CALL STR_UP( S.XTYP )
        IF ( S.XTYP .EQ. ' ' ) S.XTYP = 'TIME'
        XUNITS = DS_UNITS( S.XTYP, S.DIMSYS )

        ! X units
        WRITE( *, '(/A,49X,A/'' >> '',$)' )
     &   ' X units?', '['//XUNITS//']'
        READ( *, '(A)', END=101 ) S.XUNITS
        CALL STR_UP( S.XUNITS )
        IF ( S.XUNITS .EQ. ' ' ) S.XUNITS = XUNITS

        ! Set curve type
        IF ( ( S.CHANFORM .EQ. 'Y' ) .AND. ( S.XTYP .EQ. 'TIME' ) ) THEN
          S.CURTYP = 'TIME SERIES'
        ELSE IF ( S.CHANFORM .EQ. 'X-Y' ) THEN
          S.CURTYP = 'X-Y'
        ELSE
          S.CURTYP = ' '
        END IF

        ! X data step
        IF ( S.CHANFORM .EQ. 'Y' ) THEN
          WRITE( *, '(/A,48X,A/'' >> '',$)' )
     &     ' X data step (DEL)?', '[from X data]'
          READ( *, '(A)', END=101 ) VALUE
          IF ( VALUE .NE. ' ' ) THEN
            CALL REAL_ASSIGN( VALUE, S.DEL, .TRUE., IOS )
            IF ( ( IOS .NE. 0 ) .OR. ( S.DEL .LE. 0. ) )
     &       WRITE( *, '(/A)' )
     &       ' *** WARNING - Nonstandard field value'
            S_DEL = .TRUE.
          END IF
        ELSE ! Use DEL=0 for X-Y channels
          S_DEL = .TRUE.
        END IF

      END IF

      ! Display key UDS fields
      WRITE( *, * )
  104 WRITE( *, '(/A/)' ) ' Key UDS fields:'
      WRITE( *, * ) '  FILEVER = ', S.FILEVER(:L_TRIM(S.FILEVER)),
     & '   (UDS file version)'
      WRITE( *, * ) '  DIMSYS = ', S.DIMSYS,
     & '   (dimensional system: MET - Metric, SI - SI, ENG - English)'
      WRITE( *, * ) '  TSTNO = ', S.TSTNO,
     & '   (test number)'
      WRITE( *, * ) '  TSTNAM = ', S.TSTNAM,
     & '   (test name)'
      WRITE( *, * ) '  CHANFORM = ', S.CHANFORM(:L_TRIM(S.CHANFORM)),
     & '   (channel format: Y or X-Y)'
      WRITE( *, * ) '  CURNO = ', S.CURNO,
     & '   (curve number)'
      WRITE( *, * ) '  CURNAM = ', S.CURNAM,
     & '   (curve name)'
      WRITE( *, * ) '  SENATT = ', S.SENATT(:L_TRIM(S.SENATT)),
     & '   (sensor attachment)'
      WRITE( *, * ) '  AXIS = ', S.AXIS,
     & '   (data axis: XG for X-Global, YL for Y-Local, etc.)'
      WRITE( *, * ) '  YTYP = ', S.YTYP(:L_TRIM(S.YTYP)),
     & '   (Y data type)'
      WRITE( *, * ) '  YUNITS = ', S.YUNITS(:L_TRIM(S.YUNITS)),
     & '   (Y units)'
      WRITE( *, * ) '  XTYP = ', S.XTYP(:L_TRIM(S.XTYP)),
     & '   (X data type)'
      WRITE( *, * ) '  XUNITS = ', S.XUNITS(:L_TRIM(S.XUNITS)),
     & '   (X units)'
      WRITE( *, * ) '  CURTYP = ', S.CURTYP(:L_TRIM(S.CURTYP))
      IF ( S_NFP ) THEN
        WRITE( *, * ) '  NFP = ', S.NFP, '   (first data index)'
      ELSE
        WRITE( *, * ) '  NFP = <from data>', '   (first data index)'
      END IF
      IF ( S_NLP ) THEN
        WRITE( *, * ) '  NLP = ', S.NLP, '   (last data index)'
      ELSE
        WRITE( *, * ) '  NLP = <end of data>   (last data index)'
      END IF
      IF ( S_DEL ) THEN
        WRITE( *, * ) '  DEL = ', S.DEL, '   (data step)'
      ELSE IF ( S.CHANFORM .EQ. 'Y' ) THEN
        WRITE( *, * ) '  DEL = <from X data>   (X data step)'
      ELSE IF ( S.CHANFORM .EQ. 'X-Y' ) THEN
        WRITE( *, * ) '  DEL = <unspecified>   (data step)'
      END IF
      WRITE( *, * ) '  INIVEL = ', S.INIVEL,
     & '   (initial velocity along axis)'

      ! Prompt for field change
  105 WRITE( *, '(/A,31X,A/'' >> '',$)' )
     & ' To change a field enter:  <name> = <value>', '[done]'
      READ( *, '(A)', END=199 ) IN_LINE
      IF ( IN_LINE .NE. ' ' ) THEN

        ! Parse input for field NAME and VALUE
        CALL PARSE_NV( IN_LINE, NAME, VALUE )

        ! Change field value
        CALL UDS_SPEC_SET( S, NAME, VALUE, MSG, IOS )
        IF ( IOS .NE. 0 ) GO TO 105

        ! Update specified value flags
        IF ( NAME .EQ. 'NFP' ) THEN
          S_NFP = .TRUE.
        ELSE IF ( NAME .EQ. 'NLP' ) THEN
          S_NLP = .TRUE.
        ELSE IF ( NAME .EQ. 'DEL' ) THEN
          S_DEL = .TRUE.
        END IF

        ! Display key UDS fields again
        GO TO 104

      END IF

      ! Set other UDS fields
      S.FILEFORM = S.CHANFORM
      S.NUMFORM = NUMFORM_P
      S.NCHAN = 1
      S.ICHAN = 1

      EOF = .FALSE.
      RETURN

      ! EOF handler
  199 EOF = .TRUE.
      RETURN

      END



      SUBROUTINE SPEC_MOD( S )

!***********************************************************************
!* Performs Specification Modifications
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      RECORD /UDS/  S


      ! Variables ______________________________________________________

      LOGICAL MSG

      PARAMETER ( MSG = .TRUE. )

      INTEGER IOS

      CHARACTER RESP, IN_LINE*255, NAME*25, VALUE*132


      ! Prompt for specification group
  101 WRITE( *, '(/2A/17X,A/'' >> '',$)' ) ' Spec group? :   ',
     & 'T - Test,  V - Veh/Cmp,  O - Occupant,  I - Injury,  2 - Cmp2',
     & 'C - Curve,  M - Measurement,  S - Supplementary'
      READ( *, '(A)', END=199 ) RESP
      CALL STR_UP( RESP )

      ! Modify selected group
      IF ( RESP .EQ. ' ' ) THEN

        RETURN

      ELSE IF ( RESP .EQ. 'T' ) THEN

  102   WRITE( *, * )
        WRITE( *, * ) '  FILEVER = ', S.FILEVER(:L_TRIM(S.FILEVER))
        WRITE( *, * ) '  NCHAN = ', S.NCHAN
        WRITE( *, * ) '  FILEFORM = ',
     &   S.FILEFORM(:L_TRIM(S.FILEFORM))
        WRITE( *, * ) '  NUMFORM = ', S.NUMFORM
        WRITE( *, * ) '  DIMSYS = ', S.DIMSYS
        WRITE( *, * ) '  TSTSRC = ', S.TSTSRC(:L_TRIM(S.TSTSRC))
        WRITE( *, * ) '  TSTNO = ', S.TSTNO
        WRITE( *, * ) '  TSTNAM = ', S.TSTNAM(:L_TRIM(S.TSTNAM))
        WRITE( *, * ) '  TITLE = ', S.TITLE(:L_TRIM(S.TITLE))
        WRITE( *, * ) '  TSTPRF = ', S.TSTPRF(:L_TRIM(S.TSTPRF))
        WRITE( *, * ) '  TSTREF = ', S.TSTREF(:L_TRIM(S.TSTREF))
        WRITE( *, * ) '  TSTCFN = ', S.TSTCFN
        WRITE( *, * ) '  IMPANG = ', S.IMPANG
        WRITE( *, * ) '  CLSSPD = ', S.CLSSPD

        WRITE( *, 601 )
        READ( *, '(A)', END=101 ) IN_LINE
        IF ( IN_LINE .EQ. ' ' ) GO TO 101

        ! Parse input for field NAME and VALUE
        CALL PARSE_NV( IN_LINE, NAME, VALUE )

        ! Change field value
        CALL UDS_SPEC_SET( S, NAME, VALUE, MSG, IOS )
        GO TO 102

      ELSE IF ( RESP .EQ. 'V' ) THEN

  103   WRITE( *, * )
        WRITE( *, * ) '  CMPNO = ', S.CMPNO, '  (= VEHNO)'
        WRITE( *, * ) '  CMPTYP = ', S.CMPTYP
        WRITE( *, * ) '  CMPDSC = ', S.CMPDSC(:L_TRIM(S.CMPDSC)),
     &   '  (= VEHDSC)'
        WRITE( *, * ) '  MAKE = ', S.MAKE(:L_TRIM(S.MAKE))
        WRITE( *, * ) '  MODEL = ', S.MODEL(:L_TRIM(S.MODEL))
        WRITE( *, * ) '  YEAR = ', S.YEAR
        WRITE( *, * ) '  BODY = ', S.BODY(:L_TRIM(S.BODY))
        WRITE( *, * ) '  ENGINE = ', S.ENGINE
        WRITE( *, * ) '  CMPWGT = ', S.CMPWGT, '  (= VEHTWT)'
        WRITE( *, * ) '  CMPSPD = ', S.CMPSPD, '  (= VEHSPD)'

        WRITE( *, 601 )
        READ( *, '(A)', END=101 ) IN_LINE
        IF ( IN_LINE .EQ. ' ' ) GO TO 101

        ! Parse input for field NAME and VALUE
        CALL PARSE_NV( IN_LINE, NAME, VALUE )

        ! Change field value
        CALL UDS_SPEC_SET( S, NAME, VALUE, MSG, IOS )
        GO TO 103

      ELSE IF ( RESP .EQ. 'O' ) THEN

  104   WRITE( *, * )
        WRITE( *, * ) '  OCCTYP = ', S.OCCTYP
        WRITE( *, * ) '  OCCAGE = ', S.OCCAGE
        WRITE( *, * ) '  OCCSEX = ', S.OCCSEX
        WRITE( *, * ) '  OCCWT = ', S.OCCWT
        WRITE( *, * ) '  DUMSIZ = ', S.DUMSIZ
        WRITE( *, * ) '  RESTR1 = ', S.RESTR1(:L_TRIM(S.RESTR1))
        WRITE( *, * ) '  RESTR2 = ', S.RESTR2(:L_TRIM(S.RESTR2))

        WRITE( *, 601 )
        READ( *, '(A)', END=101 ) IN_LINE
        IF ( IN_LINE .EQ. ' ' ) GO TO 101

        ! Parse input for field NAME and VALUE
        CALL PARSE_NV( IN_LINE, NAME, VALUE )

        ! Change field value
        CALL UDS_SPEC_SET( S, NAME, VALUE, MSG, IOS )
        GO TO 104

      ELSE IF ( RESP .EQ. 'I' ) THEN

  105   WRITE( *, * )
        WRITE( *, * ) '  HIC = ', S.HIC
        WRITE( *, * ) '  T1 = ', S.T1
        WRITE( *, * ) '  T2 = ', S.T2
        WRITE( *, * ) '  HICDTUP = ', S.HICDTUP
        WRITE( *, * ) '  CLIP3M = ', S.CLIP3M
        WRITE( *, * ) '  CSI = ', S.CSI
        WRITE( *, * ) '  AIS = ', S.AIS

        WRITE( *, 601 )
        READ( *, '(A)', END=101 ) IN_LINE
        IF ( IN_LINE .EQ. ' ' ) GO TO 101

        ! Parse input for field NAME and VALUE
        CALL PARSE_NV( IN_LINE, NAME, VALUE )

        ! Change field value
        CALL UDS_SPEC_SET( S, NAME, VALUE, MSG, IOS )
        GO TO 105

      ELSE IF ( RESP .EQ. '2' ) THEN

  106   WRITE( *, * )
        WRITE( *, * ) '  CMPNO2 = ', S.CMPNO2, '  (= VEHNO2)'
        WRITE( *, * ) '  CMPTYP2 = ', S.CMPTYP2
        WRITE( *, * ) '  CMPDSC2 = ',
     &   S.CMPDSC2(:L_TRIM(S.CMPDSC2)), '  (= VEHDSC2)'
        WRITE( *, * ) '  MAKE2 = ', S.MAKE2(:L_TRIM(S.MAKE2))
        WRITE( *, * ) '  MODEL2 = ', S.MODEL2(:L_TRIM(S.MODEL2))
        WRITE( *, * ) '  YEAR2 = ', S.YEAR2
        WRITE( *, * ) '  BODY2 = ', S.BODY2(:L_TRIM(S.BODY2))
        WRITE( *, * ) '  ENGINE2 = ', S.ENGINE2
        WRITE( *, * ) '  CMPWGT2 = ', S.CMPWGT2, '  (= VEHTWT2)'
        WRITE( *, * ) '  CMPSPD2 = ', S.CMPSPD2, '  (= VEHSPD2)'
        WRITE( *, * ) '  OCCTYP2 = ', S.OCCTYP2
        WRITE( *, * ) '  OCCAGE2 = ', S.OCCAGE2
        WRITE( *, * ) '  OCCSEX2 = ', S.OCCSEX2
        WRITE( *, * ) '  OCCWT2 = ', S.OCCWT2
        WRITE( *, * ) '  DUMSIZ2 = ', S.DUMSIZ2
        WRITE( *, * ) '  RESTR12 = ', S.RESTR12(:L_TRIM(S.RESTR12))
        WRITE( *, * ) '  RESTR22 = ', S.RESTR22(:L_TRIM(S.RESTR22))

        WRITE( *, 601 )
        READ( *, '(A)', END=101 ) IN_LINE
        IF ( IN_LINE .EQ. ' ' ) GO TO 101

        ! Parse input for field NAME and VALUE
        CALL PARSE_NV( IN_LINE, NAME, VALUE )

        ! Change field value
        CALL UDS_SPEC_SET( S, NAME, VALUE, MSG, IOS )
        GO TO 106

      ELSE IF ( RESP .EQ. 'C' ) THEN

  107   WRITE( *, * )
        WRITE( *, * ) '  ICHAN = ', S.ICHAN
        WRITE( *, * ) '  CHANFORM = ', S.CHANFORM
        WRITE( *, * ) '  CURNO = ', S.CURNO
        WRITE( *, * ) '  CURNAM = ', S.CURNAM
        WRITE( *, * ) '  SENATT = ', S.SENATT(:L_TRIM(S.SENATT))
        WRITE( *, * ) '  SENLOC = ', S.SENLOC
        WRITE( *, * ) '  SENNUM = ', S.SENNUM
        WRITE( *, * ) '  BANDNO = ', S.BANDNO
        WRITE( *, * ) '  GAGNO = ', S.GAGNO
        WRITE( *, * ) '  AXIS = ', S.AXIS
        WRITE( *, * ) '  YTYP = ', S.YTYP(:L_TRIM(S.YTYP))
        WRITE( *, * ) '  YUNITS = ', S.YUNITS(:L_TRIM(S.YUNITS))
        WRITE( *, * ) '  XTYP = ', S.XTYP(:L_TRIM(S.XTYP))
        WRITE( *, * ) '  XUNITS = ', S.XUNITS(:L_TRIM(S.XUNITS))
        WRITE( *, * ) '  STATUS = ', S.STATUS(:L_TRIM(S.STATUS))
        WRITE( *, * ) '  CURTYP = ', S.CURTYP(:L_TRIM(S.CURTYP))
        WRITE( *, * ) '  CURDSC = ', S.CURDSC(:L_TRIM(S.CURDSC))

        WRITE( *, 601 )
        READ( *, '(A)', END=101 ) IN_LINE
        IF ( IN_LINE .EQ. ' ' ) GO TO 101

        ! Parse input for field NAME and VALUE
        CALL PARSE_NV( IN_LINE, NAME, VALUE )

        ! Change field value
        CALL UDS_SPEC_SET( S, NAME, VALUE, MSG, IOS )
        GO TO 107

      ELSE IF ( RESP .EQ. 'M' ) THEN

  108   WRITE( *, * )
        WRITE( *, * ) '  NFP = ', S.NFP
        WRITE( *, * ) '  NLP = ', S.NLP
        WRITE( *, * ) '  DEL = ', S.DEL
        WRITE( *, * ) '  INIVEL = ', S.INIVEL
        WRITE( *, * ) '  PREF = ', S.PREF
        WRITE( *, * ) '  FCUT = ', S.FCUT
        WRITE( *, * ) '  FCOR = ', S.FCOR
        WRITE( *, * ) '  FSTP = ', S.FSTP
        WRITE( *, * ) '  SCLFAC = ', S.SCLFAC

        WRITE( *, 601 )
        READ( *, '(A)', END=101 ) IN_LINE
        IF ( IN_LINE .EQ. ' ' ) GO TO 101

        ! Parse input for field NAME and VALUE
        CALL PARSE_NV( IN_LINE, NAME, VALUE )

        ! Change field value
        CALL UDS_SPEC_SET( S, NAME, VALUE, MSG, IOS )
        GO TO 108

      ELSE IF ( RESP .EQ. 'S' ) THEN

  109   WRITE( *, * )
        WRITE( *, * ) '  ID1 = ', S.ID1
        WRITE( *, * ) '  ID2 = ', S.ID2
        WRITE( *, * ) '  ID3 = ', S.ID3
        WRITE( *, * ) '  ID4 = ', S.ID4
        WRITE( *, * ) '  ID5 = ', S.ID5
        WRITE( *, * ) '  RD1 = ', S.RD1
        WRITE( *, * ) '  RD2 = ', S.RD2
        WRITE( *, * ) '  RD3 = ', S.RD3
        WRITE( *, * ) '  RD4 = ', S.RD4
        WRITE( *, * ) '  RD5 = ', S.RD5
        WRITE( *, * ) '  CD1 = ', S.CD1(:L_TRIM(S.CD1))
        WRITE( *, * ) '  CD2 = ', S.CD2(:L_TRIM(S.CD2))

        WRITE( *, 601 )
        READ( *, '(A)', END=101 ) IN_LINE
        IF ( IN_LINE .EQ. ' ' ) GO TO 101

        ! Parse input for field NAME and VALUE
        CALL PARSE_NV( IN_LINE, NAME, VALUE )

        ! Change field value
        CALL UDS_SPEC_SET( S, NAME, VALUE, MSG, IOS )
        GO TO 109

      ELSE

        WRITE( *, '(/A/)' ) ' *** Unrecognized input'
        GO TO 101

      END IF

  199 RETURN


  601 FORMAT(/' Enter:  <name> = <value>',49X,'[done]'/' >> ',$)

      END



      SUBROUTINE GET_FILE( U, S_NFP, S_NLP, S_DEL )

!***********************************************************************
!* Gets File Measurement Data
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'
      INCLUDE 'file.fi'


      ! Arguments ______________________________________________________

      RECORD /UDS/  U

      LOGICAL S_NFP

      LOGICAL S_NLP

      LOGICAL S_DEL


      ! Variables ______________________________________________________

      RECORD /UDS_SPEC/  S

      RECORD /UDS_CONTROL/  C

      RECORD /FILE/  REPORT

      LOGICAL FILE_X, WARN_FC, WARN_IC, X_FORMULA, EOF

      INTEGER IOS, IOS_W, I, ID, IR, J, L, LD, ASCII_UNIT,
     & NCOL, NNCOL, NRCOL, NNRCOL, IICOL, IXCOL, IYCOL, ICOL,
     & ISTART, NSTEP, NLP_STOP

      DOUBLE PRECISION X_A, X_B, X_SCL, Y_SCL, DDREC(99),
     & DI, DIP, DIS, DEL_T

      CHARACTER DATA_FILE*255, DATA_REC*256, IBUF*8


      ! Initializations
      DIS = 0.0D0
      DEL_T = 0.0D0
      CALL FILE_INIT( REPORT )

      ! Get data filename
  101 WRITE( *, '(/A,45X,A/'' >> '',$)' )
     & ' UDS or ASCII Data file name?', '[none]'
      READ( *, '(A)', END=199 ) DATA_FILE
      IF ( DATA_FILE .EQ. ' ' ) RETURN

      ! Inquire for file existence
      INQUIRE( FILE=DATA_FILE, EXIST=FILE_X, IOSTAT=IOS )
      IF ( ( .NOT. FILE_X ) .OR. ( IOS .NE. 0 ) ) THEN
        WRITE( *, * ) '*** ERROR - File does not exist'
        GO TO 101
      END IF

      ! Try opening as UDS file first - Read specs
      CALL UDS_SPEC_COPY( S, U ) ! Save specs
      CALL UDS_CONTROL_INIT( C )
      C.DIMSYS = U.DIMSYS
      C.NO_MSG = .TRUE.
      CALL UDS_READ( DATA_FILE, U, C, IOS )
      IF ( IOS .EQ. 0 ) THEN ! UDS File

        IF ( U.CHANFORM .NE. S.CHANFORM ) THEN
          WRITE( *, * ) '*** Using input file CHANFORM'
          U.CHANFORM = S.CHANFORM
        END IF
        IF ( .NOT. EQUAL_SP( U.DEL, S.DEL, 2.E-7 ) ) THEN
          WRITE( *, * ) '*** Using input file DEL step'
          U.DEL = S.DEL
        END IF
        IF ( U.NFP .NE. S.NFP ) THEN
          WRITE( *, * ) '*** Using input file NFP index'
          U.NFP = S.NFP
        END IF
        IF ( U.NLP .NE. S.NLP ) THEN
          WRITE( *, * ) '*** Using input file NLP index'
          U.NLP = S.NLP
        END IF
        IF ( ( U.XUNITS .NE. S.XUNITS ) .OR.
     &   ( U.YUNITS .NE. S.YUNITS ) )
     &   WRITE( *, * ) '*** Inconsistent units'

        ! Merge from original specs
        CALL UDS_MERGE( S, U )

        RETURN

      ELSE

        ! Restore original specs
        CALL UDS_SPEC_COPY( U, S )

      END IF

      ! Open ASCII file
      CALL OPEN_FS( ASCII_UNIT, DATA_FILE, 'R', IOS )
      IF ( IOS .NE. 0 ) THEN
        CALL MSG_WRITE( '*** ASCII file open failed', REPORT )
        GO TO 101
      END IF

      ! Find first numerical record
      NNCOL = 0
      DO WHILE ( NNCOL .EQ. 0 )
        READ( ASCII_UNIT, '(A)', IOSTAT=IOS ) DATA_REC
        IF ( IOS .NE. 0 ) THEN ! File end or error
          CALL MSG_WRITE( '*** Unacceptable input file', REPORT )
          GO TO 101
        END IF
        L = 1
        NCOL = 0
        CALL GET_COL( DATA_REC, L, NCOL, NNCOL, DDREC )
      END DO

      ! Determine file column structure
      L = 1
      LD = LEN_TRIM( DATA_REC )
      NCOL = 0
      NNCOL = 0
      DO WHILE ( L .LE. LD )
        CALL GET_COL( DATA_REC, L, NCOL, NNCOL, DDREC )
      END DO

      ! Show first numeric record
      WRITE( *, '(/I4,2A/)', IOSTAT=IOS_W ) NNCOL,
     & ' numeric columns detected - ',
     & 'First record (col)=values are:'
      WRITE( *, '(4(A,I2,A,1PG14.7,0P))', IOSTAT=IOS )
     & ( ' (', J, ')=', DDREC(J), J = 1, NNCOL )

      ! Get column numbers
      CALL GET_COL_INFO( S_DEL, IICOL, X_FORMULA, X_A, X_B,
     & IXCOL, X_SCL, IYCOL, Y_SCL, U.CHANFORM, NNCOL, EOF )
      IF ( EOF ) GO TO 101

      ! Assign/check starting index
      IF ( ( IICOL .GT. 0 ) .OR.
     & ( ( IXCOL .GT. 0 ) .AND. ( U.CHANFORM .EQ. 'Y' ) ) )
     & THEN ! Compute index
        IF ( IICOL .GT. 0 ) THEN
          ICOL = IICOL
          DEL_T = 1.D0
        ELSE
          ICOL = IXCOL
        END IF
        DI = DDREC( ICOL )
        READ( ASCII_UNIT, '(A)', IOSTAT=IOS ) DATA_REC
        BACKSPACE( ASCII_UNIT )
        IF ( IOS .EQ. 0 ) THEN ! Compute index from first 2 values
          L = 1
          LD = LEN_TRIM( DATA_REC )
          NRCOL = 0
          NNRCOL = 0
          DO WHILE ( L .LE. LD )
            CALL GET_COL( DATA_REC, L, NRCOL, NNRCOL, DDREC )
          END DO
          DIP = DDREC( ICOL )
          IF ( DIP .GT. DI ) THEN
            IF ( IICOL .EQ. 0 ) THEN
              IF ( S_DEL ) THEN
                DEL_T = DP_TRAN( U.DEL )
              ELSE
                DEL_T = DIP - DI
              END IF
            END IF
            I = NINT( DI / ( DIP - DI ) )
          ELSE
            CALL MSG_WRITE(
     &       '*** ERROR - Index cannot be computed', REPORT )
            GO TO 190
          END IF
        ELSE ! Only 1 numeric record
          IF ( ( IICOL .EQ. 0 ) .AND.
     &     ( IXCOL .GT. 0 ) .AND. ( U.CHANFORM .EQ. 'Y' ) .AND.
     &     ( S_DEL ) .AND. ( U.DEL .GT. 0. ) ) THEN ! Index from X
            I = NINT( ( DI * X_SCL ) / U.DEL )
          ELSE IF ( S_NFP ) THEN ! Use specified index
            I = U.NFP
          ELSE ! Use zero
            I = MIN( MAX( 0, NFP_L ), NLP_U )
          END IF
        END IF
        IF ( .NOT. S_NFP ) THEN ! Use first index as NFP
          IF ( ( I .LT. NFP_L ) .OR. ( I .GT. NLP_U ) ) THEN
            CALL MSG_WRITE(
     &       '*** ERROR - Index column violates array bounds',
     &       REPORT )
            GO TO 190
          END IF
          U.NFP = I
        ELSE IF ( I .LT. U.NFP ) THEN ! Skip to NFP
          DO IR = I+1, U.NFP
            READ( ASCII_UNIT, *, IOSTAT=IOS )
            IF ( IOS .NE. 0 ) THEN
              CALL MSG_WRITE(
     &         '*** ERROR - End of file occurs before NFP index',
     &         REPORT )
              GO TO 190
            END IF
          END DO
          I = U.NFP
        ELSE IF ( I .GT. U.NFP ) THEN ! Zero-fill up to I
          IF ( I .GT. NLP_U ) THEN
            CALL MSG_WRITE(
     &       '*** ERROR - Index column violates array bounds',
     &       REPORT )
            GO TO 190
          END IF
          CALL MSG_WRITE(
     &     '*** Zero-filling missing data above NFP index',
     &     REPORT )
          DO ID = U.NFP, I-1
            U.Y(I) = 0.
            U.X(I) = 0.
          END DO
        END IF
      ELSE ! Use specified value or 0
        IF ( S_NFP ) THEN ! Use specified value
          I = U.NFP
        ELSE ! Use zero
          I = MIN( MAX( 0, NFP_L ), NLP_U )
          U.NFP = I
        END IF
      END IF
      IF ( ( S_NLP ) .AND. ( I .GT. U.NLP ) ) THEN
        CALL MSG_WRITE(
     &   '*** ERROR - Starting index exceeds specified NLP', REPORT )
        GO TO 190
      ELSE IF ( I .GT. NLP_U ) THEN
        CALL MSG_WRITE(
     &   '*** ERROR - Starting index exceeds array upper bound',
     &   REPORT )
        GO TO 190
      END IF

      ! Read data
      BACKSPACE( ASCII_UNIT ) ! Move back to first numeric record
      WARN_FC = .FALSE.
      WARN_IC = .FALSE.
      IF ( S_NLP ) THEN
        NLP_STOP = MIN( U.NLP, NLP_U )
      ELSE
        NLP_STOP = NLP_U
      END IF
      ISTART = I
      I = I - 1
      DO WHILE ( I .LT. NLP_STOP )

        ! Read/process ASCII record
        DATA_REC = ' '
        DO WHILE ( DATA_REC .EQ. ' ' )
          READ( ASCII_UNIT, '(A)', ERR=104, END=104, IOSTAT=IOS )
     &     DATA_REC
        END DO
        L = 1
        LD = LEN_TRIM( DATA_REC )
        NRCOL = 0
        NNRCOL = 0
        DO WHILE ( L .LE. LD )
          CALL GET_COL( DATA_REC, L, NRCOL, NNRCOL, DDREC )
        END DO
        IF ( ( .NOT. WARN_FC ) .AND. ( NNRCOL .NE. NNCOL ) ) THEN
          CALL MSG_WRITE(
     &     '*** WARNING - File column structure varies', REPORT )
          WARN_FC = .TRUE.
        END IF
        I = I + 1

        ! Check index
        IF ( ( IICOL .GT. 0 ) .OR.
     &   ( ( IXCOL .GT. 0 ) .AND. ( U.CHANFORM .EQ. 'Y' ) ) )
     &   THEN ! Compute index
          IF ( IICOL .GT. 0 ) THEN
            ICOL = IICOL
          ELSE
            ICOL = IXCOL
          END IF
          DI = DDREC( ICOL )
          IF ( I .EQ. ISTART ) THEN ! Set lower index value
            DIS = DI
          ELSE ! Check index
            NSTEP = NINT( ( DI - DIS ) / DEL_T )
            IF ( ( NSTEP .NE. I-ISTART ) .AND. ( .NOT. WARN_IC ) ) THEN
              WRITE( IBUF, '(I8)' ) I
              CALL MSG_WRITE(
     &         '*** WARNING - Index/x data problems '//
     &         'starting at index: '//ibuf, REPORT )
              WARN_IC = .TRUE.
            END IF
            IF ( ( IICOL .EQ. 0 ) .AND. ( .NOT. S_DEL ) )
     &       DEL_T = ( DI - DIS ) / ( I - ISTART ) ! Update DEL est
          END IF
        END IF

        ! Assign X
        IF ( ( U.CHANFORM .EQ. 'X-Y' ) .OR. ( .NOT. S_DEL ) ) THEN
          IF ( X_FORMULA ) THEN
            U.X(I) = REAL( X_A * I + X_B )
          ELSE
            U.X(I) = REAL( DDREC( IXCOL ) * X_SCL )
          END IF
        END IF

        ! Assign Y
        U.Y(I) = REAL( DDREC( IYCOL ) * Y_SCL )

      END DO

      ! Check/set upper index
  104 IF ( ( I .LT. U.NFP ) .OR. ( ( IOS .NE. 0 ) .AND.
     & ( IOS .NE. -1 ) ) ) THEN
        CALL MSG_WRITE( '*** Unacceptable input file', REPORT )
        GO TO 190
      END IF
      IF ( ( S_NLP ) .AND. ( I .LT. U.NLP ) ) THEN
        CALL MSG_WRITE(
     &   '*** Zero-filling missing data up to NLP index', REPORT )
        DO ID = I+1, U.NLP
          U.Y(I) = 0.
          U.X(I) = 0.
        END DO
      ELSE IF ( .NOT. S_NLP ) THEN ! Set upper index
        U.NLP = I
      END IF

      ! Set computed X step
      IF ( ( U.CHANFORM .EQ. 'Y' ) .AND. ( .NOT. S_DEL ) ) THEN
        IF ( U.NLP .GT. U.NFP ) THEN
          U.DEL = ( U.X(U.NLP) - U.X(U.NFP) ) / ( U.NLP - U.NFP )
        END IF
      END IF

  190 CLOSE( ASCII_UNIT, IOSTAT=IOS )

  199 RETURN
      END



      SUBROUTINE GET_COL_INFO( S_DEL, IICOL, X_FORMULA, X_A, X_B,
     & IXCOL, X_SCL, IYCOL, Y_SCL, CHANFORM, NNCOL, EOF )

!***********************************************************************
!* Gets Data Column Numbers From User
!***********************************************************************


      ! Arguments ______________________________________________________

      LOGICAL S_DEL

      INTEGER IICOL

      LOGICAL X_FORMULA

      DOUBLE PRECISION X_A

      DOUBLE PRECISION X_B

      INTEGER IXCOL

      DOUBLE PRECISION X_SCL

      INTEGER IYCOL

      DOUBLE PRECISION Y_SCL

      CHARACTER*(*) CHANFORM

      INTEGER NNCOL

      LOGICAL EOF


      ! Variables ______________________________________________________

      INTEGER IOS_R

      CHARACTER RESP*25


      ! Index column
  101 WRITE( *, '(/A,42X,A/'' >> '',$)' )
     & ' Array index data column number?', '[none]'
      READ( *, '(A)', END=199 ) RESP
      IF ( RESP .NE. ' ' ) THEN
        READ( RESP, '(BN,I25)', END=199, IOSTAT=IOS_R ) IICOL
        IF ( ( IOS_R .NE. 0 ) .OR. ( IICOL .LT. 1 ) .OR.
     &   ( ( NNCOL .GT. 0 ) .AND. ( IICOL .GT. NNCOL ) ) ) THEN
          IICOL = 0
          WRITE( *, * ) '*** Unacceptable value'
          GO TO 101
        END IF
      ELSE
        IICOL = 0
      END IF

      ! X data column/formula
      IF ( ( CHANFORM .EQ. 'X-Y' ) .OR. ( .NOT. S_DEL ) .OR.
     & ( IICOL .EQ. 0 ) ) THEN
  102   IF ( ( CHANFORM .EQ. 'X-Y' ) .OR. ( .NOT. S_DEL ) ) THEN
          WRITE( *, '(/A/'' >> '',$)' )
     &     ' X data column number?   ( F - Formula-based X data )'
        ELSE
          WRITE( *, '(/A,21X,A/'' >> '',$)' )
     &     ' X data column number?   ( F - Formula-based X data )',
     &     '[none]'
        END IF
        READ( *, '(A)', END=101 ) RESP
        CALL STR_UP( RESP )
        IF ( ( RESP .EQ. ' ' ) .AND. ( CHANFORM .NE. 'X-Y' ) .AND.
     &   ( S_DEL ) ) THEN ! No optional X data column/formula
          X_FORMULA = .FALSE.
          IXCOL = 0
        ELSE IF ( RESP .EQ. 'F' ) THEN ! Construct X data from formula
          X_FORMULA = .TRUE.
          IXCOL = 0
          WRITE( *, * ) '   X data formula:   x = a*i + b',
     &     '     (i = array index)'
  103     WRITE( *, '(26X,A,$)' ) 'a='
          READ( *, '(BN,F25.0)', END=102, ERR=103 ) X_A
  104     WRITE( *, '(32X,A,$)' ) 'b='
          READ( *, '(BN,F25.0)', END=102, ERR=103 ) X_B
        ELSE ! Column number specified
          X_FORMULA = .FALSE.
          READ( RESP, '(BN,I25)', IOSTAT=IOS_R ) IXCOL
          IF ( ( IOS_R .NE. 0 ) .OR. ( IXCOL .LT. 1 ) .OR.
     &     ( ( NNCOL .GT. 0 ) .AND. ( IXCOL .GT. NNCOL ) ) ) THEN
            WRITE( *, * ) '*** Unacceptable value'
            GO TO 102
          END IF
  105     WRITE( *, '(/A,53X,A/'' >> '',$)' )
     &     ' X data scale factor?', '[none]'
          READ( *, '(A)', END=101 ) RESP
          IF ( RESP .EQ. ' ' ) THEN ! No X scaling
            X_SCL = 1.D0
          ELSE
            READ( RESP, '(BN,F25.0)', ERR=105 ) X_SCL
          END IF
        END IF
      ELSE ! No X data column/formula
        X_FORMULA = .FALSE.
        IXCOL = 0
      END IF

      ! Y data column
  106 WRITE( *, '(/A/'' >> '',$)' ) ' Y data column number?'
      READ( *, '(A)', END=101 ) RESP
      READ( RESP, '(BN,I25)', END=101, IOSTAT=IOS_R ) IYCOL
      IF ( ( IOS_R .NE. 0 ) .OR. ( IYCOL .LT. 1 ) .OR.
     & ( ( NNCOL .GT. 0 ) .AND. ( IYCOL .GT. NNCOL ) ) ) THEN
        WRITE( *, * ) '*** Unacceptable value'
        GO TO 106
      END IF
  107 WRITE( *, '(/A,53X,A/'' >> '',$)' ) ' Y data scale factor?',
     & '[none]'
      READ( *, '(A)', END=101 ) RESP
      IF ( RESP .EQ. ' ' ) THEN ! No Y scaling
        Y_SCL = 1.D0
      ELSE
        READ( RESP, '(BN,F25.0)', ERR=107 ) Y_SCL
      END IF

      EOF = .FALSE.
      RETURN

      ! EOF handler
  199 EOF = .TRUE.
      RETURN

      END



      SUBROUTINE GET_COL( DATA_REC, L, NCOL, NNCOL, DDREC )

!***********************************************************************
!* Extracts Next Data Column from String
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) DATA_REC

      INTEGER L

      INTEGER NCOL

      INTEGER NNCOL

      DOUBLE PRECISION DDREC(99)


      ! Variables ______________________________________________________

      INTEGER I, LD, IS, IE, IC, IOS_R

      DOUBLE PRECISION DD

      CHARACTER NSTR*25


      ! Initializations
      I = L
      LD = LEN_TRIM( DATA_REC )

      ! Read through column separator
      IC = ICHAR( DATA_REC(I:I) )
      DO WHILE ( ( I .LE. LD ) .AND. ( ( IC .LT. 43 ) .OR.
     & ( IC .GT. 122 ) .OR. ( IC .EQ. 44 ) ) )
        I = I + 1
        IF ( I .LE. LD ) IC = ICHAR( DATA_REC(I:I) )
      END DO

      ! Read the column
      IF ( I .LE. LD ) THEN
        ! Increment number of columns counter
        NCOL = NCOL + 1

        ! Find the end of the column
        IS = I
        DO WHILE ( ( I .LE. LD ) .AND. ( IC .GE. 43 ) .AND.
     &   ( IC .LE. 122 ) .AND. ( IC .NE. 44 ) )
          I = I + 1
          IF ( I .LE. LD ) IC = ICHAR( DATA_REC(I:I) )
        END DO

        ! Assign column if numeric
        IE = I - 1
        NSTR = DATA_REC(IS:IE)
        READ( NSTR, '(BN,F25.0)', IOSTAT=IOS_R ) DD
        IF ( IOS_R .EQ. 0 ) THEN ! Numeric
          NNCOL = NNCOL + 1
          IF ( NNCOL .LE. 99 ) THEN
            DDREC( NNCOL ) = DD
          ELSE
            WRITE( *, * ) '*** Numeric columns past 99 ignored'
          END IF
        END IF

        L = IE + 1
      ELSE
        L = I
      END IF

      RETURN
      END



      SUBROUTINE PARSE_NV( IN_LINE, NAME, VALUE )

!***********************************************************************
!* Extracts the NAME and VALUE Fields from IN_LINE
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) IN_LINE

      CHARACTER*(*) NAME

      CHARACTER*(*) VALUE


      ! Variables ______________________________________________________

      INTEGER I, LI


      ! Functions ______________________________________________________

      CHARACTER LJUST*255

      EXTERNAL LJUST


      ! Assign first field to NAME and second field to VALUE
      NAME = ' '
      VALUE = ' '
      IN_LINE = LJUST( IN_LINE )
      IF ( IN_LINE .NE. ' ' ) THEN
        IF ( IN_LINE(1:1) .EQ. '=' ) RETURN ! Blank NAME

        ! Scan for end of lead string and assign to NAME
        I = 2
        LI = LEN_TRIM( IN_LINE )
        DO WHILE ( ( I .LE. LI ) .AND.
     &   ( .NOT. ANY_CHARS( IN_LINE(I:I), ' =' ) ) )
          I = I + 1
        END DO
        NAME = IN_LINE(:I-1)
        CALL STR_UP( NAME )

        ! Find and assign VALUE
        DO WHILE ( ( I .LE. LI ) .AND.
     &   ( ANY_CHARS( IN_LINE(I:I), ' =' ) ) )
          I = I + 1
        END DO
        IF ( I .LE. LI ) VALUE = LJUST( IN_LINE(I:) )
      END IF

      RETURN
      END



      SUBROUTINE UDS_OUT( FILE_NAME, U, OSTAT, EOF )

!***********************************************************************
!* Outputs the UDS File
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) FILE_NAME

      RECORD /UDS/  U

      CHARACTER OSTAT

      LOGICAL EOF


      ! Variables ______________________________________________________

      RECORD /UDS_CONTROL/  C

      INTEGER IOS, I

      CHARACTER DEF_FILE*255, OUT_FILE*255


      ! Create default output UDS filename
      OUT_FILE = FILE_NAME
      IF ( OSTAT .EQ. 'N' ) THEN ! New file
        ! Use specified filename
      ELSE ! New version of input file
        CALL FN_EXTRACT( OUT_FILE )
        DEF_FILE = OUT_FILE
        IF ( ( OSTAT .EQ. 'M' ) .OR. ( OSTAT .EQ. 'm' ) ) THEN
          CALL FN_FLAG( DEF_FILE, 'm', 2, I )
        END IF
        CALL FN_INCR( DEF_FILE )
  101   WRITE( *, '(/A/3A/'' >> '',$)' )
     &   ' Output UDS file name?',
     &   ' [', DEF_FILE(:L_TRIM(DEF_FILE)), ']'
        READ( *, '(A)', END=199 ) OUT_FILE
        IF ( OUT_FILE .EQ. ' ' ) OUT_FILE = DEF_FILE
        IF ( .NOT. FN_VALID( OUT_FILE ) ) THEN
          WRITE( *, * ) '*** Invalid file name'
          GO TO 101
        END IF
      END IF

      ! Output UDS file
      CALL UDS_CONTROL_INIT( C )
      CALL UDS_WRITE( OUT_FILE, U, C, IOS )
      IF ( IOS .NE. 0 ) GO TO 199
      WRITE( *, '(A/1X,A)', IOSTAT=IOS ) ' *** UDS output to file:',
     & OUT_FILE(:L_TRIM(OUT_FILE))
      IF ( OSTAT .EQ. 'M' ) THEN
        OSTAT = 'm'
      ELSE IF ( OSTAT .EQ. 'N' ) THEN
        OSTAT = 'O'
      END IF

      EOF = .FALSE.
      RETURN

      ! Return forced by EOF
  199 EOF = .TRUE.
      RETURN

      END
