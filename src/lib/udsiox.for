      SUBROUTINE UDSIOX( ROW, SOF, FILE_NAME, LUN, X, Y,
     & NFP_L, NLP_U, NUMFORM_R, DIMSYS_R, MSG, IOS )

!***********************************************************************
!* UDSIOX - UDS File Read/Write Utility Routine
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/17
!***********************************************************************

      ! Headers
      INCLUDE 'udsiox.fi'
      INCLUDE 'udsiox_w.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER ROW ! Read Or Write flag

      CHARACTER SOF ! Specs Or Full read flag

      CHARACTER*(*) FILE_NAME ! UDS file name

      INTEGER LUN ! Logical unit number for UDS i/o

      INTEGER NFP_L ! Array index lower bound

      INTEGER NLP_U ! Array index upper bound

      INTEGER X(NFP_L:NLP_U) ! UDS X data array (alias as integer)

      INTEGER Y(NFP_L:NLP_U) ! UDS Y data array (alias as integer)

      CHARACTER*(*) NUMFORM_R ! Number format request

      CHARACTER*(*) DIMSYS_R ! Dimensional system request

      LOGICAL MSG ! Specifies messages allowed

      INTEGER IOS ! Status flag (returned)


      ! Variables ______________________________________________________

      LOGICAL FILE_X, FILE_OPEN, DONE, CHAN_READ,
     & FIRST, Y_READ, X_READ, W_READ, ASSIGN_DATA, USE_LUN

      PARAMETER ( USE_LUN = .FALSE. )

      INTEGER IOS_W, IOS_C, LF, I, IP, IR, IS, IE,
     & NCHAN_I, NFP_I, NLP_I, NFP_O, NLP_O, NOUT, NDREC

      ! Alias reals as int's to avoid assign of illegal f.p. values
      INTEGER W_INT(125), RD_INT

      REAL DEL_P, RD, W(125)

      DOUBLE PRECISION DD

      CHARACTER NUM_P*5, NUM_I*5, NUM_O*5, DIM_I*3, DIM_O*3,
     & ENDIAN_P*6, ENDIAN_I*6, ENDIAN_O*6,
     & REC_HDR*12, REC_HDR_B*12, CHANFORM_R*10, CHANFORM_B*10,
     & YUN_I*20, XUN_I*20, VEL_TYP*20, WT_TYP*20,
     & DATA_F, FNAM*252, IO_BUFF*512, UPCASE

      PARAMETER ( VEL_TYP = 'VELOCITY', WT_TYP = 'WEIGHT' )

      PARAMETER ( NUM_P = NUMFORM_P )

      EQUIVALENCE ( RD, RD_INT )

      EQUIVALENCE ( IO_BUFF(1:12), REC_HDR )

      EQUIVALENCE ( IO_BUFF(13:), W_INT, W )

      EQUIVALENCE ( IO_BUFF(17:26), CHANFORM_B )


      ! Route around UDSIOY ENTRY point
      CHANFORM_R = ' '
      GO TO 101

      ! UDSIOY ENTRY: No X array parameter
      ENTRY UDSIOY( ROW, SOF, FILE_NAME, LUN, Y,
     & NFP_L, NLP_U, NUMFORM_R, DIMSYS_R, MSG, IOS )
      CHANFORM_R = 'Y'
  101 CONTINUE


      ! Initializations
      IOS = 0
      IF ( FILE_NAME .EQ. ' ' ) THEN ! Check for open file and get name
        INQUIRE( LUN, OPENED=FILE_OPEN, NAME=FNAM )
        IF ( .NOT. FILE_OPEN ) THEN
          IF ( MSG ) WRITE( *, '(/2A/)' ) ' *** UDSIOX ERROR - ',
     &       'No UDS file specified or already open'
          RETURN
        END IF
        REWIND( LUN )
      ELSE
        CLOSE( UNIT=LUN, IOSTAT=IOS_C )
        FNAM = FILE_NAME
      END IF
      LF = L_TRIM( FNAM )


      ! Check array bound parameters
      IF ( NFP_L .GT. NLP_U ) THEN
        IF ( MSG ) WRITE( *, '(/2A/)' ) ' *** UDSIOX ERROR - ',
     &     'Crossed array bound parameters: NFP_L > NLP_U'
        IOS = -1
        RETURN
      ELSE IF ( NFP_L .EQ. NLP_U ) THEN
        IF ( MSG ) WRITE( *, '(/2A,I7/)', IOSTAT=IOS_W )
     &     ' *** UDSIOX WARNING - ',
     &     'Equal array bound parameters: NFP_L=NLP_U=', NLP_U
      END IF

      ! Check conversion parameters
      IF ( ( .NOT. BLANK( NUMFORM_R ) ) .AND.
     & ( NUMFORM_R .NE. 'VAX' ) .AND. ( NUMFORM_R .NE. 'IEEE' ) .AND.
     & ( NUMFORM_R .NE. 'SUN' ) .AND. ( NUMFORM_R .NE. 'NONE' ) ) THEN
        IF ( MSG ) WRITE( *, '(/3A/)' ) ' *** UDSIOX ERROR - ',
     &     'Unsupported NUMFORM_R parameter: ', NUMFORM_R
        IOS = -1
        RETURN
      END IF
      IF ( ( .NOT. BLANK( DIMSYS_R ) ) .AND.
     & ( DIMSYS_R .NE. 'MET' ) .AND.
     & ( DIMSYS_R .NE. 'SI' ) .AND.
     & ( DIMSYS_R .NE. 'ENG' ) ) THEN
        IF ( MSG ) WRITE( *, '(/3A/)' ) ' *** UDSIOX ERROR - ',
     &     'Unsupported DIMSYS_R parameter: ', DIMSYS_R
        IOS = -1
        RETURN
      END IF

      ! Set up platform byte order flag
      IF ( ( NUM_P .EQ. 'VAX' ) .OR.
     & ( NUM_P .EQ. 'IEEE' ) ) THEN
        ENDIAN_P = 'LITTLE'
      ELSE IF ( NUM_P .EQ. 'SUN' ) THEN
        ENDIAN_P = 'BIG'
      ELSE
        IF ( MSG ) WRITE( *, '(/3A/)' )
     &     ' *** UDSIOX ERROR - ',
     &     'Unrecognized platform number format ', NUM_P
        IOS = -1
        RETURN
      END IF


      ! Read or Write a UDS file
      IF ( UPCASE( ROW ) .EQ. 'R' ) THEN ! Read a UDS file

        ! Open file
        IF ( FILE_NAME .NE. ' ' ) THEN
          INQUIRE( FILE=FILE_NAME, EXIST=FILE_X, IOSTAT=IOS )
          IF ( ( .NOT. FILE_X ) .OR. ( IOS .NE. 0 ) ) THEN
            IF ( MSG ) WRITE( *, '(/2A/1X,A)', IOSTAT=IOS_W )
     &         ' *** UDSIOX ERROR - ',
     &         'No such file:', FILE_NAME(:LF)
            RETURN
          END IF
          CALL UDSIO_OPEN( LUN, USE_LUN, FILE_NAME, 'R', IOS )
          IF ( IOS .NE. 0 ) THEN
            IF ( MSG ) WRITE( *, '(/2A/1X,A)', IOSTAT=IOS_W )
     &         ' *** UDSIOX ERROR - ',
     &         'File open failed - Not a UDS-1992 file:',
     &         FILE_NAME(:LF)
            CALL UDSIO_CLOSE( LUN )
            RETURN
          END IF
        END IF


        ! Initialize UDS fields
        CALL UDSIO_INIT()


        ! Read header record
        CALL UDSIO_BUF_READ( LUN, IO_BUFF, IOS )
        IF ( IOS .NE. 0 ) GO TO 201

        ! Check file version field
        IF ( REC_HDR .NE. 'UDS-1992' ) THEN
          IF ( MSG ) WRITE( *, '(/2A/1X,A)', IOSTAT=IOS_W )
     &       ' *** UDSIOX ERROR - ',
     &       'Not a UDS-1992 file:', FNAM(:LF)
          IF ( FILE_NAME .NE. ' ' ) CALL UDSIO_CLOSE( LUN )
          RETURN
        END IF

        ! Load file header fields
        IP = 1
        CALL STR_READ( FILEVER, IO_BUFF, IP )
        CALL INT_READ( NCHAN, IO_BUFF, IP )
        CALL STR_READ( FILEFORM, IO_BUFF, IP )
        CALL STR_READ( NUMFORM, IO_BUFF, IP )
        CALL STR_READ( DIMSYS, IO_BUFF, IP )
        CALL STR_READ( TSTSRC, IO_BUFF, IP )
        CALL INT_READ( TSTNO, IO_BUFF, IP )
        CALL STR_READ( TSTNAM, IO_BUFF, IP )
        CALL STR_READ( TITLE, IO_BUFF, IP )
        CALL STR_READ( TSTPRF, IO_BUFF, IP )
        CALL STR_READ( TSTREF, IO_BUFF, IP )
        CALL STR_READ( TSTCFN, IO_BUFF, IP )
        CALL INT_READ( IMPANG, IO_BUFF, IP )
        CALL REAL_READ( CLSSPD, IO_BUFF, IP )
        CALL INT_READ( CMPNO, IO_BUFF, IP )
        CALL STR_READ( CMPTYP, IO_BUFF, IP )
        CALL STR_READ( CMPDSC, IO_BUFF, IP )
        CALL STR_READ( MAKE, IO_BUFF, IP )
        CALL STR_READ( MODEL, IO_BUFF, IP )
        CALL INT_READ( YEAR, IO_BUFF, IP )
        CALL STR_READ( BODY, IO_BUFF, IP )
        CALL STR_READ( ENGINE, IO_BUFF, IP )
        CALL REAL_READ( CMPWGT, IO_BUFF, IP )
        CALL REAL_READ( CMPSPD, IO_BUFF, IP )
        CALL STR_READ( OCCTYP, IO_BUFF, IP )
        CALL INT_READ( OCCAGE, IO_BUFF, IP )
        CALL STR_READ( OCCSEX, IO_BUFF, IP )
        CALL REAL_READ( OCCWT, IO_BUFF, IP )
        CALL STR_READ( DUMSIZ, IO_BUFF, IP )
        CALL STR_READ( RESTR1, IO_BUFF, IP )
        CALL STR_READ( RESTR2, IO_BUFF, IP )
        CALL REAL_READ( HIC, IO_BUFF, IP )
        CALL REAL_READ( T1, IO_BUFF, IP )
        CALL REAL_READ( T2, IO_BUFF, IP )
        CALL REAL_READ( HICDTUP, IO_BUFF, IP )
        CALL REAL_READ( CLIP3M, IO_BUFF, IP )
        CALL REAL_READ( CSI, IO_BUFF, IP )
        CALL STR_READ( AIS, IO_BUFF, IP )
        CALL STR_READ( HDR_RESV, IO_BUFF, IP )

        ! Check file header fields
        IF ( ( NUMFORM .NE. 'VAX' ) .AND.
     &     ( NUMFORM .NE. 'IEEE' ) .AND. ( NUMFORM .NE. 'SUN' ) ) THEN
          IF ( MSG ) WRITE( *, '(/4A/1X,A/)', IOSTAT=IOS_W )
     &       ' *** UDSIOX ERROR - ',
     &       'Unsupported real number format: ', NUMFORM,
     &       ' in file:', FNAM(:LF)
          IOS = -2
          RETURN
        END IF
        IF ( ( DIMSYS .NE. 'MET' ) .AND. ( DIMSYS .NE. 'SI' ) .AND.
     &     ( DIMSYS .NE. 'ENG' ) ) THEN
          IF ( MSG ) WRITE( *, '(/4A/1X,A/)', IOSTAT=IOS_W )
     &       ' *** UDSIOX ERROR - ',
     &       'Unsupported dimensional system: ', DIMSYS,
     &       ' in file:', FNAM(:LF)
          IOS = -2
          RETURN
        END IF

        ! Set up byte order flags
        IF ( ( NUMFORM .EQ. 'VAX' ) .OR.
     &     ( NUMFORM .EQ. 'IEEE' ) ) THEN
          ENDIAN_I = 'LITTLE'
        ELSE IF ( NUMFORM .EQ. 'SUN' ) THEN
          ENDIAN_I = 'BIG'
        END IF
        IF ( ( NUMFORM_R .EQ. 'VAX' ) .OR.
     &     ( NUMFORM_R .EQ. 'IEEE' ) ) THEN
          ENDIAN_O = 'LITTLE'
        ELSE IF ( NUMFORM_R .EQ. 'SUN' ) THEN
          ENDIAN_O = 'BIG'
        ELSE IF ( NUMFORM_R .EQ. 'NONE' ) THEN
          ENDIAN_O = ENDIAN_I
        ELSE ! Default to local platform byte order
          ENDIAN_O = ENDIAN_P
        END IF

        ! Set up number format conversion flags
        IF ( ( NUMFORM_R .EQ. 'VAX' ) .OR.
     &     ( NUMFORM_R .EQ. 'IEEE' ) .OR.
     &     ( NUMFORM_R .EQ. 'SUN' ) ) THEN
          NUM_O = NUMFORM_R
        ELSE IF ( NUMFORM_R .EQ. 'NONE' ) THEN
          NUM_O = NUMFORM
        ELSE ! Default to local platform format
          NUM_O = NUM_P
        END IF
        NUM_I = NUMFORM
        NUMFORM = NUM_O

        ! Set up dimensional system conversion flags
        IF ( ( DIMSYS_R .EQ. 'MET' ) .OR.
     &     ( DIMSYS_R .EQ. 'SI' ) .OR.
     &     ( DIMSYS_R .EQ. 'ENG' ) ) THEN
          DIM_O = DIMSYS_R
        ELSE ! No change
          DIM_O = DIMSYS
        END IF
        DIM_I = DIMSYS
        DIMSYS = DIM_O

        ! Check/set number of channels = 1
        NCHAN_I = NCHAN
        CALL INT_CONV( NCHAN_I, ENDIAN_I, ENDIAN_P )
        IF ( NCHAN_I .GT. 1 ) THEN
          IF ( MSG ) WRITE( *, '(/2A/1X,A/)', IOSTAT=IOS_W )
     &       ' *** UDSIOX WARNING - ',
     &       'Only one channel read from file:', FNAM(:LF)
          NCHAN = 1
          CALL INT_CONV( NCHAN, ENDIAN_P, ENDIAN_I )
        END IF

        ! Byte reordering
        IF ( ENDIAN_I .NE. ENDIAN_O ) THEN
          CALL INT_CONV( NCHAN, ENDIAN_I, ENDIAN_O )
          CALL INT_CONV( TSTNO, ENDIAN_I, ENDIAN_O )
          CALL INT_CONV( IMPANG, ENDIAN_I, ENDIAN_O )
          CALL INT_CONV( CMPNO, ENDIAN_I, ENDIAN_O )
          CALL INT_CONV( YEAR, ENDIAN_I, ENDIAN_O )
          CALL INT_CONV( OCCAGE, ENDIAN_I, ENDIAN_O )
        END IF

        ! Number format conversion
        IF ( NUM_I .NE. NUM_O ) THEN
          CALL FPN_CONV( CLSSPD, NUM_I, NUM_O )
          CALL FPN_CONV( CMPWGT, NUM_I, NUM_O )
          CALL FPN_CONV( CMPSPD, NUM_I, NUM_O )
          CALL FPN_CONV( OCCWT, NUM_I, NUM_O )
          CALL FPN_CONV( HIC, NUM_I, NUM_O )
          CALL FPN_CONV( T1, NUM_I, NUM_O )
          CALL FPN_CONV( T2, NUM_I, NUM_O )
          CALL FPN_CONV( HICDTUP, NUM_I, NUM_O )
          CALL FPN_CONV( CLIP3M, NUM_I, NUM_O )
          CALL FPN_CONV( CSI, NUM_I, NUM_O )
        END IF

        ! Dimensional system conversion
        IF ( DIM_I .NE. DIM_O ) THEN
          CALL DSN_CONV( CLSSPD, VEL_TYP, ' ', DIM_I, DIM_O, NUM_O )
          CALL DSN_CONV( CMPWGT, WT_TYP, ' ', DIM_I, DIM_O, NUM_O )
          CALL DSN_CONV( CMPSPD, VEL_TYP, ' ', DIM_I, DIM_O, NUM_O )
          CALL DSN_CONV( OCCWT, WT_TYP, ' ', DIM_I, DIM_O, NUM_O )
        END IF

        ! Read rest of records
        DONE = .FALSE.
        CHAN_READ = .FALSE.
        DO WHILE ( .NOT. DONE )

          ! Read the record
          CALL UDSIO_BUF_READ( LUN, IO_BUFF, IOS )
          IF ( IOS .EQ. -1 ) THEN
            GO TO 120
          ELSE IF ( IOS .NE. 0 ) THEN
            GO TO 201
          END IF

          IF ( REC_HDR .EQ. 'CMP-2 HDR' ) THEN ! Component-2 record

            ! Load component-2 fields
            IP = 13
            CALL INT_READ( CMPNO2, IO_BUFF, IP )
            CALL STR_READ( CMPTYP2, IO_BUFF, IP )
            CALL STR_READ( CMPDSC2, IO_BUFF, IP )
            CALL STR_READ( MAKE2, IO_BUFF, IP )
            CALL STR_READ( MODEL2, IO_BUFF, IP )
            CALL INT_READ( YEAR2, IO_BUFF, IP )
            CALL STR_READ( BODY2, IO_BUFF, IP )
            CALL STR_READ( ENGINE2, IO_BUFF, IP )
            CALL REAL_READ( CMPWGT2, IO_BUFF, IP )
            CALL REAL_READ( CMPSPD2, IO_BUFF, IP )
            CALL STR_READ( OCCTYP2, IO_BUFF, IP )
            CALL INT_READ( OCCAGE2, IO_BUFF, IP )
            CALL STR_READ( OCCSEX2, IO_BUFF, IP )
            CALL REAL_READ( OCCWT2, IO_BUFF, IP )
            CALL STR_READ( DUMSIZ2, IO_BUFF, IP )
            CALL STR_READ( RESTR12, IO_BUFF, IP )
            CALL STR_READ( RESTR22, IO_BUFF, IP )
            CALL STR_READ( CMP2_RESV, IO_BUFF, IP )

            ! Byte reordering
            IF ( ENDIAN_I .NE. ENDIAN_O ) THEN
              CALL INT_CONV( CMPNO2, ENDIAN_I, ENDIAN_O )
              CALL INT_CONV( YEAR2, ENDIAN_I, ENDIAN_O )
              CALL INT_CONV( OCCAGE2, ENDIAN_I, ENDIAN_O )
            END IF

            ! Number format conversion
            IF ( NUM_I .NE. NUM_O ) THEN
              CALL FPN_CONV( CMPWGT2, NUM_I, NUM_O )
              CALL FPN_CONV( CMPSPD2, NUM_I, NUM_O )
              CALL FPN_CONV( OCCWT2, NUM_I, NUM_O )
            END IF

            ! Dimensional system conversion
            IF ( DIM_I .NE. DIM_O ) THEN
              CALL DSN_CONV( CMPWGT2, WT_TYP, ' ', DIM_I, DIM_O,
     &           NUM_O )
              CALL DSN_CONV( CMPSPD2, VEL_TYP, ' ', DIM_I, DIM_O,
     &           NUM_O )
              CALL DSN_CONV( OCCWT2, WT_TYP, ' ', DIM_I, DIM_O,
     &           NUM_O )
            END IF

          ELSE IF ( ( REC_HDR .EQ. 'CURVE HDR' ) .AND.
     &       ( ( BLANK( CHANFORM_R ) ) .OR.
     &       ( CHANFORM_B .EQ. CHANFORM_R ) ) ) THEN ! Curve channel

            ! Load curve header fields
            IP = 13
            CALL INT_READ( ICHAN, IO_BUFF, IP )
            CALL STR_READ( CHANFORM, IO_BUFF, IP )
            CALL INT_READ( CURNO, IO_BUFF, IP )
            CALL STR_READ( CURNAM, IO_BUFF, IP )
            CALL STR_READ( SENATT, IO_BUFF, IP )
            CALL STR_READ( SENLOC, IO_BUFF, IP )
            CALL INT_READ( SENNUM, IO_BUFF, IP )
            CALL INT_READ( BANDNO, IO_BUFF, IP )
            CALL INT_READ( GAGNO, IO_BUFF, IP )
            CALL STR_READ( AXIS, IO_BUFF, IP )
            CALL STR_READ( YTYP, IO_BUFF, IP )
            CALL STR_READ( YUNITS, IO_BUFF, IP )
            CALL STR_READ( XTYP, IO_BUFF, IP )
            CALL STR_READ( XUNITS, IO_BUFF, IP )
            CALL STR_READ( STATUS, IO_BUFF, IP )
            CALL STR_READ( CURTYP, IO_BUFF, IP )
            CALL STR_READ( CURDSC, IO_BUFF, IP )
            CALL INT_READ( NFP, IO_BUFF, IP )
            CALL INT_READ( NLP, IO_BUFF, IP )
            CALL REAL_READ( DEL, IO_BUFF, IP )
            CALL REAL_READ( INIVEL, IO_BUFF, IP )
            CALL REAL_READ( PREF, IO_BUFF, IP )
            CALL REAL_READ( FCUT, IO_BUFF, IP )
            CALL REAL_READ( FCOR, IO_BUFF, IP )
            CALL REAL_READ( FSTP, IO_BUFF, IP )
            CALL REAL_READ( SCLFAC, IO_BUFF, IP )
            CALL INT_READ( ID1, IO_BUFF, IP )
            CALL INT_READ( ID2, IO_BUFF, IP )
            CALL INT_READ( ID3, IO_BUFF, IP )
            CALL INT_READ( ID4, IO_BUFF, IP )
            CALL INT_READ( ID5, IO_BUFF, IP )
            CALL REAL_READ( RD1, IO_BUFF, IP )
            CALL REAL_READ( RD2, IO_BUFF, IP )
            CALL REAL_READ( RD3, IO_BUFF, IP )
            CALL REAL_READ( RD4, IO_BUFF, IP )
            CALL REAL_READ( RD5, IO_BUFF, IP )
            CALL STR_READ( CD1, IO_BUFF, IP )
            CALL STR_READ( CD2, IO_BUFF, IP )
            CALL STR_READ( CURV_RESV, IO_BUFF, IP )

            ! Flag that a channel was read
            CHAN_READ = .TRUE.

            ! Byte reordering
            IF ( ENDIAN_I .NE. ENDIAN_O ) THEN
              CALL INT_CONV( ICHAN, ENDIAN_I, ENDIAN_O )
              CALL INT_CONV( CURNO, ENDIAN_I, ENDIAN_O )
              CALL INT_CONV( SENNUM, ENDIAN_I, ENDIAN_O )
              CALL INT_CONV( BANDNO, ENDIAN_I, ENDIAN_O )
              CALL INT_CONV( GAGNO, ENDIAN_I, ENDIAN_O )
              CALL INT_CONV( NFP, ENDIAN_I, ENDIAN_O )
              CALL INT_CONV( NLP, ENDIAN_I, ENDIAN_O )
              CALL INT_CONV( ID1, ENDIAN_I, ENDIAN_O )
              CALL INT_CONV( ID2, ENDIAN_I, ENDIAN_O )
              CALL INT_CONV( ID3, ENDIAN_I, ENDIAN_O )
              CALL INT_CONV( ID4, ENDIAN_I, ENDIAN_O )
              CALL INT_CONV( ID5, ENDIAN_I, ENDIAN_O )
            END IF

            ! Number format conversion
            IF ( NUM_I .EQ. NUM_P ) THEN
              DEL_P = DEL
            ELSE
              CALL FPN_CONV( DEL, NUM_I, NUM_P )
              DEL_P = DEL
              CALL FPN_CONV( DEL, NUM_P, NUM_I )
            END IF
            IF ( NUM_I .NE. NUM_O ) THEN
              CALL FPN_CONV( DEL, NUM_I, NUM_O )
              CALL FPN_CONV( INIVEL, NUM_I, NUM_O )
              CALL FPN_CONV( PREF, NUM_I, NUM_O )
              CALL FPN_CONV( FCUT, NUM_I, NUM_O )
              CALL FPN_CONV( FCOR, NUM_I, NUM_O )
              CALL FPN_CONV( FSTP, NUM_I, NUM_O )
              CALL FPN_CONV( SCLFAC, NUM_I, NUM_O )
              CALL FPN_CONV( RD1, NUM_I, NUM_O )
              CALL FPN_CONV( RD2, NUM_I, NUM_O )
              CALL FPN_CONV( RD3, NUM_I, NUM_O )
              CALL FPN_CONV( RD4, NUM_I, NUM_O )
              CALL FPN_CONV( RD5, NUM_I, NUM_O )
            END IF

            ! Dimensional system conversion
            YUN_I = YUNITS
            XUN_I = XUNITS
            IF ( DIM_I .NE. DIM_O ) THEN
              CALL DSU_CONV( YTYP, YUNITS, DIM_I, DIM_O, DD )
              CALL DSU_CONV( XTYP, XUNITS, DIM_I, DIM_O, DD )
              IF ( CHANFORM .EQ. 'Y' ) THEN
                CALL DSN_CONV( DEL, XTYP, XUN_I, DIM_I, DIM_O,
     &             NUM_O )
                CALL DSN_CONV( DEL_P, XTYP, XUN_I, DIM_I, DIM_O,
     &             NUM_P )
              END IF
              CALL DSN_CONV( INIVEL, VEL_TYP, ' ', DIM_I, DIM_O,
     &           NUM_O )
            END IF

            ! Check array indices
            NFP_I = NFP
            CALL INT_CONV( NFP_I, ENDIAN_O, ENDIAN_P )
            NFP_O = NFP_I
            NLP_I = NLP
            CALL INT_CONV( NLP_I, ENDIAN_O, ENDIAN_P )
            NLP_O = NLP_I
            IF ( NFP_I .GT. NLP_I ) THEN
              IF ( MSG ) WRITE( *, '(/2A/1X,A/)', IOSTAT=IOS_W )
     &           ' *** UDSIOX ERROR - ',
     &           'Crossed array indices: NFP > NLP in file:',
     &           FNAM(:LF)
              IOS = -3
              RETURN
            ELSE IF ( NFP_I .EQ. NLP_I ) THEN
              IF ( MSG ) WRITE( *, '(/2A,I7,A/1X,A/)',
     &           IOSTAT=IOS_W )
     &           ' *** UDSIOX WARNING - ',
     &           'Equal array indices: NFP=NLP=', NLP_I,
     &           ' in file:', FNAM(:LF)
            END IF

            ! Read curve data
            IF ( UPCASE( SOF ) .EQ. 'S' ) THEN ! Done - Return

              DONE = .TRUE.

            ELSE IF ( ( CHANFORM .EQ. 'Y' ) .OR.
     &         ( CHANFORM .EQ. 'X-Y' ) ) THEN

              ! Check array indices against bounds
              IF ( ( NLP_I .LT. NFP_L ) .OR.
     &           ( NFP_I .GT. NLP_U ) ) THEN
                IF ( MSG ) WRITE( *, '(/2A/1X,A/)',
     &             IOSTAT=IOS_W )
     &             ' *** UDSIOX ERROR - ',
     &             'Array indices outside valid range in file:',
     &             FNAM(:LF)
                IOS = -3
                RETURN
              END IF
              IF ( NFP_I .LT. NFP_L ) THEN
                IF ( MSG ) WRITE( *, '(/2A,I7,A/1X,A/)',
     &             IOSTAT=IOS_W )
     &             ' *** UDSIOX WARNING - ',
     &             'Array lower index truncated to: ', NFP_L,
     &             ' in read from file:', FNAM(:LF)
                NFP_O = NFP_L
                NFP = NFP_L
                CALL INT_CONV( NFP, ENDIAN_P, ENDIAN_O )
              END IF
              IF ( NLP_I .GT. NLP_U ) THEN
                IF ( MSG ) WRITE( *, '(/2A,I7,A/1X,A/)',
     &             IOSTAT=IOS_W )
     &             ' *** UDSIOX WARNING - ',
     &             'Array upper index truncated to: ', NLP_U,
     &             ' in read from file:', FNAM(:LF)
                NLP_O = NLP_U
                NLP = NLP_U
                CALL INT_CONV( NLP, ENDIAN_P, ENDIAN_O )
              END IF

              ! Read the data records
              NDREC = ( NLP_I - NFP_I ) / 125 + 1
              Y_READ = .FALSE.
              IF ( CHANFORM .EQ. 'X-Y' ) THEN
                X_READ = .FALSE.
              ELSE
                X_READ = .TRUE.
              END IF
              DO WHILE ( .NOT. ( Y_READ .AND. X_READ ) )

                ! Read the first data record
                CALL UDSIO_BUF_READ( LUN, IO_BUFF, IOS )
                IF ( IOS .NE. 0 ) GO TO 202

                ! Set up for type of data
                ASSIGN_DATA = .TRUE.
                IF ( REC_HDR .EQ. 'Y DATA' ) THEN
                  DATA_F = 'Y'
                  W_READ = Y_READ
                  Y_READ = .TRUE.
                ELSE IF ( ( REC_HDR .EQ. 'X DATA' ) .AND.
     &             ( CHANFORM .EQ. 'X-Y' ) ) THEN
                  DATA_F = 'X'
                  W_READ = X_READ
                  X_READ = .TRUE.
                ELSE
                  IF ( MSG ) WRITE( *, '(/5A/1X,A/)',
     &               IOSTAT=IOS_W )
     &               ' *** UDSIOX WARNING - ',
     &               REC_HDR(:L_TRIM(REC_HDR)),
     &               ' records skipped in read from ',
     &               CHANFORM(:L_TRIM(CHANFORM)),
     &               ' channel in file:', FNAM(:LF)
                  ASSIGN_DATA = .FALSE.
                END IF

                ! Check for repeat data
                IF ( ( ASSIGN_DATA ) .AND. ( W_READ ) ) THEN
                  IF ( MSG ) WRITE( *, '(/4A/1X,A/)',
     &               IOSTAT=IOS_W )
     &               ' *** UDSIOX WARNING - ',
     &               'Repeat ', REC_HDR(:L_TRIM(REC_HDR)),
     &               ' records ignored in file:', FNAM(:LF)
                  ASSIGN_DATA = .FALSE.
                END IF

                ! Assign data
                FIRST = .TRUE.
                REC_HDR_B = REC_HDR
                DO IR = 1, NDREC
                  IF ( FIRST ) THEN
                    FIRST = .FALSE.
                  ELSE
                    CALL UDSIO_BUF_READ( LUN, IO_BUFF, IOS )
                    IF ( IOS .NE. 0 ) GO TO 202
                  END IF
                  IF ( REC_HDR .NE. REC_HDR_B ) THEN
                    IF ( MSG ) WRITE( *, '(/7A/1X,A/)',
     &                 IOSTAT=IOS_W )
     &                 ' *** UDSIOX ERROR - ', DATA_F,
     &                 ' data record type is ',
     &                 REC_HDR(:L_TRIM(REC_HDR)), ' not ',
     &                 REC_HDR_B(:L_TRIM(REC_HDR_B)),
     &                 ' in file:', FNAM(:LF)
                    IOS = -4
                    RETURN
                  END IF
                  IF ( ASSIGN_DATA ) THEN
                    IS = NFP_I + 125*(IR-1)
                    IE = MIN( IS+124, NLP_O )
                    IS = MAX( IS, NFP_O )
                    IF ( REC_HDR .EQ. 'Y DATA' ) THEN
                      DO I = IS, IE
                        Y(I) = W_INT(I-IS+1)
                      END DO
                    ELSE IF ( REC_HDR .EQ. 'X DATA' ) THEN
                      DO I = IS, IE
                        X(I) = W_INT(I-IS+1)
                      END DO
                    END IF
                  END IF
                END DO

                ! Number format conversion
                IF ( NUM_I .NE. NUM_O ) THEN
                  IF ( REC_HDR .EQ. 'Y DATA' ) THEN
                    CALL FPA_CONV( Y(NFP_O), NLP_O-NFP_O+1,
     &                 NUM_I, NUM_O )
                  ELSE IF ( REC_HDR .EQ. 'X DATA' ) THEN
                    CALL FPA_CONV( X(NFP_O), NLP_O-NFP_O+1,
     &                 NUM_I, NUM_O )
                  END IF
                END IF

                ! Dimensional system conversion
                IF ( DIM_I .NE. DIM_O ) THEN
                  IF ( REC_HDR .EQ. 'Y DATA' ) THEN
                    CALL DSA_CONV( Y(NFP_O), NLP_O-NFP_O+1,
     &                 YTYP, YUN_I, DIM_I, DIM_O, NUM_O )
                  ELSE IF ( REC_HDR .EQ. 'X DATA' ) THEN
                    CALL DSA_CONV( X(NFP_O), NLP_O-NFP_O+1,
     &                 XTYP, XUN_I, DIM_I, DIM_O, NUM_O )
                  END IF
                END IF

              END DO

              ! Assign even X values for Y channels
              IF ( ( CHANFORM .EQ. 'Y' ) .AND.
     &           ( CHANFORM_R .NE. 'Y' ) ) THEN ! Assign X values

                ! Set even X values
                DO I = NFP_O, NLP_O
                  RD = I * DEL_P
                  X(I) = RD_INT
                END DO

                ! Number format conversion
                IF ( NUM_P .NE. NUM_O ) THEN
                  CALL FPA_CONV( X(NFP_O), NLP_O-NFP_O+1,
     &               NUM_P, NUM_O )
                END IF

              END IF

            ELSE IF ( MSG ) THEN

              WRITE( *, '(/4A/1X,A/)', IOSTAT=IOS_W )
     &           ' *** UDSIOX WARNING - ',
     &           'Unsupported channel format for data read: ',
     &           CHANFORM(:L_TRIM(CHANFORM)), ' in file:',
     &           FNAM(:LF)

            END IF

            ! Set fields for single channel read
            FILEFORM = CHANFORM
            ICHAN = 1
            CALL INT_CONV( ICHAN, ENDIAN_P, ENDIAN_O )

            DONE = .TRUE.

          ELSE IF ( REC_HDR .EQ. 'CURVE HDR' ) THEN ! Wrong channel

            DONE = .TRUE.

          END IF

        END DO

        ! Check if channel read
  120   IF ( ( .NOT. CHAN_READ ) .AND. ( IOS .EQ. 0 ) ) THEN
          IF ( MSG ) WRITE( *, '(/2A/1X,A)', IOSTAT=IOS_W )
     &       ' *** UDSIOX ERROR - ',
     &       'Supported channel format not found in file:', FNAM(:LF)
          IOS = -1
        END IF

      ELSE IF ( UPCASE( ROW ) .EQ. 'W' ) THEN ! Write UDS file

        ! Check UDS fields
        IF ( ( FILEVER .NE. 'UDS-1992' ) .AND.
     &     ( FILEVER .NE. 'UDS-1991' ) .AND.
     &     ( .NOT. BLANK( FILEVER ) ) ) THEN
          IF ( MSG ) WRITE( *, '(/4A/1X,A/)', IOSTAT=IOS_W )
     &       ' *** UDSIOX ERROR - ',
     &       'Invalid file version: ', FILEVER(:L_TRIM(FILEVER)),
     &       ' in write to file:', FNAM(:LF)
          IOS = -6
          RETURN
        END IF
        IF ( ( ( CHANFORM .NE. 'Y' ) .AND.
     &     ( CHANFORM .NE. 'X-Y' ) ) .OR.
     &     ( ( CHANFORM .EQ. 'X-Y' ) .AND.
     &     ( CHANFORM_R .EQ. 'Y' ) ) ) THEN
          IF ( MSG ) WRITE( *, '(/4A/1X,A/)', IOSTAT=IOS_W )
     &       ' *** UDSIOX ERROR - ',
     &       'Unsupported channel format: ',
     &       CHANFORM(:L_TRIM(CHANFORM)), ' in write to file:',
     &       FNAM(:LF)
          IOS = -6
          RETURN
        END IF
        IF ( BLANK( NUMFORM ) ) THEN
          NUM_I = NUM_P
        ELSE
          NUM_I = NUMFORM
        END IF
        IF ( ( NUM_I .NE. 'VAX' ) .AND.
     &     ( NUM_I .NE. 'IEEE' ) .AND. ( NUM_I .NE. 'SUN' ) ) THEN
          IF ( MSG ) WRITE( *, '(/4A/1X,A/)', IOSTAT=IOS_W )
     &       ' *** UDSIOX ERROR - ',
     &       'Unsupported real number format: ', NUM_I,
     &       ' in write to file:', FNAM(:LF)
          IOS = -6
          RETURN
        END IF
        IF ( ( DIMSYS .NE. 'MET' ) .AND. ( DIMSYS .NE. 'SI' ) .AND.
     &     ( DIMSYS .NE. 'ENG' ) ) THEN
          IF ( MSG ) WRITE( *, '(/4A/1X,A/)', IOSTAT=IOS_W )
     &       ' *** UDSIOX ERROR - ',
     &       'Unsupported dimensional system: ', DIMSYS,
     &       ' in write to file:', FNAM(:LF)
          IOS = -6
          RETURN
        END IF

        ! Set up byte order flags
        IF ( ( NUMFORM .EQ. 'VAX' ) .OR.
     &     ( NUMFORM .EQ. 'IEEE' ) ) THEN
          ENDIAN_I = 'LITTLE'
        ELSE IF ( NUMFORM .EQ. 'SUN' ) THEN
          ENDIAN_I = 'BIG'
        END IF
        IF ( ( NUMFORM_R .EQ. 'VAX' ) .OR.
     &     ( NUMFORM_R .EQ. 'IEEE' ) ) THEN
          ENDIAN_O = 'LITTLE'
        ELSE IF ( NUMFORM_R .EQ. 'SUN' ) THEN
          ENDIAN_O = 'BIG'
        ELSE IF ( NUMFORM_R .EQ. 'NONE' ) THEN
          ENDIAN_O = ENDIAN_I
        ELSE ! Default to local platform byte order
          ENDIAN_O = ENDIAN_P
        END IF

        ! Set up number format conversion flags
        IF ( ( NUMFORM_R .EQ. 'VAX' ) .OR.
     &     ( NUMFORM_R .EQ. 'IEEE' ) .OR.
     &     ( NUMFORM_R .EQ. 'SUN' ) ) THEN
          NUM_O = NUMFORM_R
        ELSE IF ( NUMFORM_R .EQ. 'NONE' ) THEN
          NUM_O = NUM_I
        ELSE ! Default to local platform format
          NUM_O = NUM_P
        END IF

        ! Set up dimensional system conversion flags
        IF ( ( DIMSYS_R .EQ. 'MET' ) .OR.
     &     ( DIMSYS_R .EQ. 'SI' ) .OR.
     &     ( DIMSYS_R .EQ. 'ENG' ) ) THEN
          DIM_O = DIMSYS_R
        ELSE ! No change
          DIM_O = DIMSYS
        END IF
        DIM_I = DIMSYS

        ! Check array indices
        NFP_I = NFP
        CALL INT_CONV( NFP_I, ENDIAN_I, ENDIAN_P )
        NFP_O = NFP_I
        NLP_I = NLP
        CALL INT_CONV( NLP_I, ENDIAN_I, ENDIAN_P )
        NLP_O = NLP_I
        IF ( NFP_I .GT. NLP_I ) THEN
          IF ( MSG ) WRITE( *, '(/2A/1X,A/)', IOSTAT=IOS_W )
     &       ' *** UDSIOX ERROR - ',
     &       'Crossed array indices: NFP > NLP in write to file:',
     &       FNAM(:LF)
          IOS = -3
          RETURN
        ELSE IF ( NFP_I .EQ. NLP_I ) THEN
          IF ( MSG ) WRITE( *, '(/2A,I7,A/1X,A/)', IOSTAT=IOS_W )
     &       ' *** UDSIOX WARNING - ',
     &       'Equal array indices: NFP=NLP=', NLP_I,
     &       ' in write to file:', FNAM(:LF)
        END IF
        IF ( ( NLP_I .LT. NFP_L ) .OR. ( NFP_I .GT. NLP_U ) ) THEN
          IF ( MSG ) WRITE( *, '(/2A/1X,A/)', IOSTAT=IOS_W )
     &       ' *** UDSIOX ERROR - ',
     &       'Array indices outside valid range in write to file:',
     &       FNAM(:LF)
          IOS = -3
          RETURN
        END IF

        ! Load output specifications
        FILEVER_W = 'UDS-1992'
        NCHAN_W = NCHAN
        FILEFORM_W = CHANFORM
        NUMFORM_W = NUM_O
        DIMSYS_W = DIM_O
        TSTSRC_W = TSTSRC
        TSTNO_W = TSTNO
        TSTNAM_W = TSTNAM
        TITLE_W = TITLE
        TSTPRF_W = TSTPRF
        TSTREF_W = TSTREF
        TSTCFN_W = TSTCFN
        IMPANG_W = IMPANG
        CLSSPD_W = CLSSPD
        CMPNO_W = CMPNO
        CMPTYP_W = CMPTYP
        CMPDSC_W = CMPDSC
        MAKE_W = MAKE
        MODEL_W = MODEL
        YEAR_W = YEAR
        BODY_W = BODY
        ENGINE_W = ENGINE
        CMPWGT_W = CMPWGT
        CMPSPD_W = CMPSPD
        OCCTYP_W = OCCTYP
        OCCAGE_W = OCCAGE
        OCCSEX_W = OCCSEX
        OCCWT_W = OCCWT
        DUMSIZ_W = DUMSIZ
        RESTR1_W = RESTR1
        RESTR2_W = RESTR2
        HIC_W = HIC
        T1_W = T1
        T2_W = T2
        HICDTUP_W = HICDTUP
        CLIP3M_W = CLIP3M
        CSI_W = CSI
        AIS_W = AIS
        HDR_RESV_W = HDR_RESV
        CMPNO2_W = CMPNO2
        CMPDSC2_W = CMPDSC2
        CMPTYP2_W = CMPTYP2
        MAKE2_W = MAKE2
        MODEL2_W = MODEL2
        YEAR2_W = YEAR2
        BODY2_W = BODY2
        ENGINE2_W = ENGINE2
        CMPWGT2_W = CMPWGT2
        CMPSPD2_W = CMPSPD2
        OCCTYP2_W = OCCTYP2
        OCCAGE2_W = OCCAGE2
        OCCSEX2_W = OCCSEX2
        OCCWT2_W = OCCWT2
        DUMSIZ2_W = DUMSIZ2
        RESTR12_W = RESTR12
        RESTR22_W = RESTR22
        CMP2_RESV_W = CMP2_RESV
        ICHAN_W = ICHAN
        CHANFORM_W = CHANFORM
        CURNO_W = CURNO
        CURNAM_W = CURNAM
        SENATT_W = SENATT
        SENLOC_W = SENLOC
        SENNUM_W = SENNUM
        BANDNO_W = BANDNO
        GAGNO_W = GAGNO
        AXIS_W = AXIS
        YTYP_W = YTYP
        YUNITS_W = YUNITS
        XTYP_W = XTYP
        XUNITS_W = XUNITS
        STATUS_W = STATUS
        CURTYP_W = CURTYP
        CURDSC_W = CURDSC
        NFP_W = NFP
        NLP_W = NLP
        DEL_W = DEL
        INIVEL_W = INIVEL
        PREF_W = PREF
        FCUT_W = FCUT
        FCOR_W = FCOR
        FSTP_W = FSTP
        SCLFAC_W = SCLFAC
        ID1_W = ID1
        ID2_W = ID2
        ID3_W = ID3
        ID4_W = ID4
        ID5_W = ID5
        RD1_W = RD1
        RD2_W = RD2
        RD3_W = RD3
        RD4_W = RD4
        RD5_W = RD5
        CD1_W = CD1
        CD2_W = CD2
        CURV_RESV_W = CURV_RESV

        ! Byte reordering
        IF ( ENDIAN_I .NE. ENDIAN_O ) THEN
          CALL INT_CONV( NCHAN_W, ENDIAN_I, ENDIAN_O )
          CALL INT_CONV( TSTNO_W, ENDIAN_I, ENDIAN_O )
          CALL INT_CONV( IMPANG_W, ENDIAN_I, ENDIAN_O )
          CALL INT_CONV( CMPNO_W, ENDIAN_I, ENDIAN_O )
          CALL INT_CONV( YEAR_W, ENDIAN_I, ENDIAN_O )
          CALL INT_CONV( OCCAGE_W, ENDIAN_I, ENDIAN_O )
        END IF

        ! Dimensional system conversion
        IF ( DIM_I .NE. DIM_O ) THEN
          CALL DSN_CONV( CLSSPD_W, VEL_TYP, ' ', DIM_I, DIM_O,
     &       NUM_I )
          CALL DSN_CONV( CMPWGT_W, WT_TYP, ' ', DIM_I, DIM_O,
     &       NUM_I )
          CALL DSN_CONV( CMPSPD_W, VEL_TYP, ' ', DIM_I, DIM_O,
     &       NUM_I )
          CALL DSN_CONV( OCCWT_W, WT_TYP, ' ', DIM_I, DIM_O,
     &       NUM_I )
        END IF

        ! Number format conversion
        IF ( NUM_I .NE. NUM_O ) THEN
          CALL FPN_CONV( CLSSPD_W, NUM_I, NUM_O )
          CALL FPN_CONV( CMPWGT_W, NUM_I, NUM_O )
          CALL FPN_CONV( CMPSPD_W, NUM_I, NUM_O )
          CALL FPN_CONV( OCCWT_W, NUM_I, NUM_O )
          CALL FPN_CONV( HIC_W, NUM_I, NUM_O )
          CALL FPN_CONV( T1_W, NUM_I, NUM_O )
          CALL FPN_CONV( T2_W, NUM_I, NUM_O )
          CALL FPN_CONV( HICDTUP_W, NUM_I, NUM_O )
          CALL FPN_CONV( CLIP3M_W, NUM_I, NUM_O )
          CALL FPN_CONV( CSI_W, NUM_I, NUM_O )
        END IF

        ! Check/set number of channels = 1
        NCHAN_I = NCHAN_W
        CALL INT_CONV( NCHAN_I, ENDIAN_O, ENDIAN_P )
        IF ( NCHAN_I .GT. 1 ) THEN
          IF ( MSG ) WRITE( *, '(/2A/1X,A/)', IOSTAT=IOS_W )
     &       ' *** UDSIOX WARNING - ',
     &       'Only one channel written to file:', FNAM(:LF)
          NCHAN_W = 1
          CALL INT_CONV( NCHAN_W, ENDIAN_P, ENDIAN_O )
        END IF


        ! Open file
        IF ( FILE_NAME .NE. ' ' ) THEN
          CALL UDSIO_OPEN( LUN, USE_LUN, FILE_NAME, 'W', IOS )
          IF ( IOS .NE. 0 )
     &       CALL UDSIO_OPEN( LUN, USE_LUN, FILE_NAME, ' ', IOS )
          IF ( IOS .NE. 0 ) THEN
            IF ( MSG ) WRITE( *, '(/2A/1X,A/)', IOSTAT=IOS_W )
     &         ' *** UDSIOX ERROR - ',
     &         'File open failure on write to file:', FNAM(:LF)
            RETURN
          END IF
        END IF


        ! Load file header fields
        IP = 1
        CALL STR_WRITE( FILEVER_W, IO_BUFF, IP )
        CALL INT_WRITE( NCHAN_W, IO_BUFF, IP )
        CALL STR_WRITE( FILEFORM_W, IO_BUFF, IP )
        CALL STR_WRITE( NUMFORM_W, IO_BUFF, IP )
        CALL STR_WRITE( DIMSYS_W, IO_BUFF, IP )
        CALL STR_WRITE( TSTSRC_W, IO_BUFF, IP )
        CALL INT_WRITE( TSTNO_W, IO_BUFF, IP )
        CALL STR_WRITE( TSTNAM_W, IO_BUFF, IP )
        CALL STR_WRITE( TITLE_W, IO_BUFF, IP )
        CALL STR_WRITE( TSTPRF_W, IO_BUFF, IP )
        CALL STR_WRITE( TSTREF_W, IO_BUFF, IP )
        CALL STR_WRITE( TSTCFN_W, IO_BUFF, IP )
        CALL INT_WRITE( IMPANG_W, IO_BUFF, IP )
        CALL REAL_WRITE( CLSSPD_W, IO_BUFF, IP )
        CALL INT_WRITE( CMPNO_W, IO_BUFF, IP )
        CALL STR_WRITE( CMPTYP_W, IO_BUFF, IP )
        CALL STR_WRITE( CMPDSC_W, IO_BUFF, IP )
        CALL STR_WRITE( MAKE_W, IO_BUFF, IP )
        CALL STR_WRITE( MODEL_W, IO_BUFF, IP )
        CALL INT_WRITE( YEAR_W, IO_BUFF, IP )
        CALL STR_WRITE( BODY_W, IO_BUFF, IP )
        CALL STR_WRITE( ENGINE_W, IO_BUFF, IP )
        CALL REAL_WRITE( CMPWGT_W, IO_BUFF, IP )
        CALL REAL_WRITE( CMPSPD_W, IO_BUFF, IP )
        CALL STR_WRITE( OCCTYP_W, IO_BUFF, IP )
        CALL INT_WRITE( OCCAGE_W, IO_BUFF, IP )
        CALL STR_WRITE( OCCSEX_W, IO_BUFF, IP )
        CALL REAL_WRITE( OCCWT_W, IO_BUFF, IP )
        CALL STR_WRITE( DUMSIZ_W, IO_BUFF, IP )
        CALL STR_WRITE( RESTR1_W, IO_BUFF, IP )
        CALL STR_WRITE( RESTR2_W, IO_BUFF, IP )
        CALL REAL_WRITE( HIC_W, IO_BUFF, IP )
        CALL REAL_WRITE( T1_W, IO_BUFF, IP )
        CALL REAL_WRITE( T2_W, IO_BUFF, IP )
        CALL REAL_WRITE( HICDTUP_W, IO_BUFF, IP )
        CALL REAL_WRITE( CLIP3M_W, IO_BUFF, IP )
        CALL REAL_WRITE( CSI_W, IO_BUFF, IP )
        CALL STR_WRITE( AIS_W, IO_BUFF, IP )
        CALL STR_WRITE( HDR_RESV_W, IO_BUFF, IP )

        ! Write header record
        CALL UDSIO_BUF_WRITE( LUN, IO_BUFF, IOS )
        IF ( IOS .NE. 0 ) GO TO 203


        ! Component-2 record
        IF ( ( TSTSRC .EQ. 'COMPONENT DB' ) .OR.
     &     ( CMPNO2 .NE. 0 ) ) THEN ! Write component-2 record

          ! Byte reordering
          IF ( ENDIAN_I .NE. ENDIAN_O ) THEN
            CALL INT_CONV( CMPNO2_W, ENDIAN_I, ENDIAN_O )
            CALL INT_CONV( YEAR2_W, ENDIAN_I, ENDIAN_O )
            CALL INT_CONV( OCCAGE2_W, ENDIAN_I, ENDIAN_O )
          END IF

          ! Dimensional system conversion
          IF ( DIM_I .NE. DIM_O ) THEN
            CALL DSN_CONV( CMPWGT2_W, WT_TYP, ' ', DIM_I, DIM_O,
     &         NUM_I )
            CALL DSN_CONV( CMPSPD2_W, VEL_TYP, ' ', DIM_I, DIM_O,
     &         NUM_I )
            CALL DSN_CONV( OCCWT2_W, WT_TYP, ' ', DIM_I, DIM_O,
     &         NUM_I )
          END IF

          ! Number format conversion
          IF ( NUM_I .NE. NUM_O ) THEN
            CALL FPN_CONV( CMPWGT2_W, NUM_I, NUM_O )
            CALL FPN_CONV( CMPSPD2_W, NUM_I, NUM_O )
            CALL FPN_CONV( OCCWT2_W, NUM_I, NUM_O )
          END IF

          ! Write component-2 record
          IO_BUFF(:12) = 'CMP-2 HDR'
          IP = 13
          CALL INT_WRITE( CMPNO2_W, IO_BUFF, IP )
          CALL STR_WRITE( CMPTYP2_W, IO_BUFF, IP )
          CALL STR_WRITE( CMPDSC2_W, IO_BUFF, IP )
          CALL STR_WRITE( MAKE2_W, IO_BUFF, IP )
          CALL STR_WRITE( MODEL2_W, IO_BUFF, IP )
          CALL INT_WRITE( YEAR2_W, IO_BUFF, IP )
          CALL STR_WRITE( BODY2_W, IO_BUFF, IP )
          CALL STR_WRITE( ENGINE2_W, IO_BUFF, IP )
          CALL REAL_WRITE( CMPWGT2_W, IO_BUFF, IP )
          CALL REAL_WRITE( CMPSPD2_W, IO_BUFF, IP )
          CALL STR_WRITE( OCCTYP2_W, IO_BUFF, IP )
          CALL INT_WRITE( OCCAGE2_W, IO_BUFF, IP )
          CALL STR_WRITE( OCCSEX2_W, IO_BUFF, IP )
          CALL REAL_WRITE( OCCWT2_W, IO_BUFF, IP )
          CALL STR_WRITE( DUMSIZ2_W, IO_BUFF, IP )
          CALL STR_WRITE( RESTR12_W, IO_BUFF, IP )
          CALL STR_WRITE( RESTR22_W, IO_BUFF, IP )
          CALL STR_WRITE( CMP2_RESV_W, IO_BUFF, IP )
          CALL UDSIO_BUF_WRITE( LUN, IO_BUFF, IOS )
          IF ( IOS .NE. 0 ) GO TO 203

        END IF


        ! Output curve specification record ____________________________

        ! Adjust output curve specifications
        ICHAN_W = 1
        CALL INT_CONV( ICHAN_W, ENDIAN_P, ENDIAN_I )
        IF ( NFP_I .LT. NFP_L ) THEN
          IF ( MSG ) WRITE( *, '(/2A,I7,A/1X,A/)', IOSTAT=IOS_W )
     &       ' *** UDSIOX WARNING - ',
     &       'Array lower index truncated to: ', NFP_L,
     &       ' in write to file:', FNAM(:LF)
          NFP_O = NFP_L
          NFP_W = NFP_L
          CALL INT_CONV( NFP_W, ENDIAN_P, ENDIAN_I )
        END IF
        IF ( NLP_I .GT. NLP_U ) THEN
          IF ( MSG ) WRITE( *, '(/2A,I7,A/1X,A/)', IOSTAT=IOS_W )
     &       ' *** UDSIOX WARNING - ',
     &       'Array upper index truncated to: ', NLP_U,
     &       ' in write to file:', FNAM(:LF)
          NLP_O = NLP_U
          NLP_W = NLP_U
          CALL INT_CONV( NLP_W, ENDIAN_P, ENDIAN_I )
        END IF

        ! Byte reordering
        IF ( ENDIAN_I .NE. ENDIAN_O ) THEN
          CALL INT_CONV( ICHAN_W, ENDIAN_I, ENDIAN_O )
          CALL INT_CONV( CURNO_W, ENDIAN_I, ENDIAN_O )
          CALL INT_CONV( SENNUM_W, ENDIAN_I, ENDIAN_O )
          CALL INT_CONV( BANDNO_W, ENDIAN_I, ENDIAN_O )
          CALL INT_CONV( GAGNO_W, ENDIAN_I, ENDIAN_O )
          CALL INT_CONV( NFP_W, ENDIAN_I, ENDIAN_O )
          CALL INT_CONV( NLP_W, ENDIAN_I, ENDIAN_O )
          CALL INT_CONV( ID1_W, ENDIAN_I, ENDIAN_O )
          CALL INT_CONV( ID2_W, ENDIAN_I, ENDIAN_O )
          CALL INT_CONV( ID3_W, ENDIAN_I, ENDIAN_O )
          CALL INT_CONV( ID4_W, ENDIAN_I, ENDIAN_O )
          CALL INT_CONV( ID5_W, ENDIAN_I, ENDIAN_O )
        END IF

        ! Dimensional system conversion
        IF ( DIM_I .NE. DIM_O ) THEN
          CALL DSU_CONV( YTYP_W, YUNITS_W, DIM_I, DIM_O, DD )
          CALL DSU_CONV( XTYP_W, XUNITS_W, DIM_I, DIM_O, DD )
          IF ( CHANFORM_W .EQ. 'Y' )
     &       CALL DSN_CONV( DEL_W, XTYP_W, XUNITS, DIM_I, DIM_O,
     &       NUM_I )
          CALL DSN_CONV( INIVEL_W, VEL_TYP, ' ', DIM_I, DIM_O,
     &       NUM_I )
        END IF

        ! Number format conversion
        IF ( NUM_I .NE. NUM_O ) THEN
          CALL FPN_CONV( DEL_W, NUM_I, NUM_O )
          CALL FPN_CONV( INIVEL_W, NUM_I, NUM_O )
          CALL FPN_CONV( PREF_W, NUM_I, NUM_O )
          CALL FPN_CONV( FCUT_W, NUM_I, NUM_O )
          CALL FPN_CONV( FCOR_W, NUM_I, NUM_O )
          CALL FPN_CONV( FSTP_W, NUM_I, NUM_O )
          CALL FPN_CONV( SCLFAC_W, NUM_I, NUM_O )
          CALL FPN_CONV( RD1_W, NUM_I, NUM_O )
          CALL FPN_CONV( RD2_W, NUM_I, NUM_O )
          CALL FPN_CONV( RD3_W, NUM_I, NUM_O )
          CALL FPN_CONV( RD4_W, NUM_I, NUM_O )
          CALL FPN_CONV( RD5_W, NUM_I, NUM_O )
        END IF

        ! Write curve specification record
        IO_BUFF(:12) = 'CURVE HDR'
        IP = 13
        CALL INT_WRITE( ICHAN_W, IO_BUFF, IP )
        CALL STR_WRITE( CHANFORM_W, IO_BUFF, IP )
        CALL INT_WRITE( CURNO_W, IO_BUFF, IP )
        CALL STR_WRITE( CURNAM_W, IO_BUFF, IP )
        CALL STR_WRITE( SENATT_W, IO_BUFF, IP )
        CALL STR_WRITE( SENLOC_W, IO_BUFF, IP )
        CALL INT_WRITE( SENNUM_W, IO_BUFF, IP )
        CALL INT_WRITE( BANDNO_W, IO_BUFF, IP )
        CALL INT_WRITE( GAGNO_W, IO_BUFF, IP )
        CALL STR_WRITE( AXIS_W, IO_BUFF, IP )
        CALL STR_WRITE( YTYP_W, IO_BUFF, IP )
        CALL STR_WRITE( YUNITS_W, IO_BUFF, IP )
        CALL STR_WRITE( XTYP_W, IO_BUFF, IP )
        CALL STR_WRITE( XUNITS_W, IO_BUFF, IP )
        CALL STR_WRITE( STATUS_W, IO_BUFF, IP )
        CALL STR_WRITE( CURTYP_W, IO_BUFF, IP )
        CALL STR_WRITE( CURDSC_W, IO_BUFF, IP )
        CALL INT_WRITE( NFP_W, IO_BUFF, IP )
        CALL INT_WRITE( NLP_W, IO_BUFF, IP )
        CALL REAL_WRITE( DEL_W, IO_BUFF, IP )
        CALL REAL_WRITE( INIVEL_W, IO_BUFF, IP )
        CALL REAL_WRITE( PREF_W, IO_BUFF, IP )
        CALL REAL_WRITE( FCUT_W, IO_BUFF, IP )
        CALL REAL_WRITE( FCOR_W, IO_BUFF, IP )
        CALL REAL_WRITE( FSTP_W, IO_BUFF, IP )
        CALL REAL_WRITE( SCLFAC_W, IO_BUFF, IP )
        CALL INT_WRITE( ID1_W, IO_BUFF, IP )
        CALL INT_WRITE( ID2_W, IO_BUFF, IP )
        CALL INT_WRITE( ID3_W, IO_BUFF, IP )
        CALL INT_WRITE( ID4_W, IO_BUFF, IP )
        CALL INT_WRITE( ID5_W, IO_BUFF, IP )
        CALL REAL_WRITE( RD1_W, IO_BUFF, IP )
        CALL REAL_WRITE( RD2_W, IO_BUFF, IP )
        CALL REAL_WRITE( RD3_W, IO_BUFF, IP )
        CALL REAL_WRITE( RD4_W, IO_BUFF, IP )
        CALL REAL_WRITE( RD5_W, IO_BUFF, IP )
        CALL STR_WRITE( CD1_W, IO_BUFF, IP )
        CALL STR_WRITE( CD2_W, IO_BUFF, IP )
        CALL STR_WRITE( CURV_RESV_W, IO_BUFF, IP )
        CALL UDSIO_BUF_WRITE( LUN, IO_BUFF, IOS )
        IF ( IOS .NE. 0 ) GO TO 203


        ! Output curve data records ____________________________________

        ! Compute number of data records
        NDREC = ( NLP_O - NFP_O ) / 125 + 1

        ! Write the Y data records
        REC_HDR = 'Y DATA'
        DO IR = 1, NDREC
          IS = NFP_O + 125*(IR-1)
          IE = MIN( IS+124, NLP_O )
          NOUT = IE - IS + 1
          DO I = 1, NOUT
            W_INT(I) = Y(IS-1+I)
          END DO
          DO I = NOUT+1, 125
            W(I) = 0.
          END DO

          ! Dimensional system conversion
          IF ( DIM_I .NE. DIM_O ) THEN
            CALL DSA_CONV( W, NOUT, YTYP_W, YUNITS,
     &         DIM_I, DIM_O, NUM_I )
          END IF

          ! Number format conversion
          IF ( NUM_I .NE. NUM_O ) THEN
            CALL FPA_CONV( W, NOUT, NUM_I, NUM_O )
          END IF

          ! Write the data
          CALL UDSIO_BUF_WRITE( LUN, IO_BUFF, IOS )
          IF ( IOS .NE. 0 ) GO TO 203
        END DO

        ! Write X data if X-Y channel
        IF ( CHANFORM_W .EQ. 'X-Y' ) THEN

          ! Write the X data records
          REC_HDR = 'X DATA'
          DO IR = 1, NDREC
            IS = NFP_O + 125*(IR-1)
            IE = MIN( IS+124, NLP_O )
            NOUT = IE - IS + 1
            DO I = 1, NOUT
              W_INT(I) = X(IS-1+I)
            END DO
            DO I = NOUT+1, 125
              W(I) = 0.
            END DO

            ! Dimensional system conversion
            IF ( DIM_I .NE. DIM_O ) THEN
              CALL DSA_CONV( W, NOUT, XTYP_W, XUNITS,
     &           DIM_I, DIM_O, NUM_I )
            END IF

            ! Number format conversion
            IF ( NUM_I .NE. NUM_O ) THEN
              CALL FPA_CONV( W, NOUT, NUM_I, NUM_O )
            END IF

            ! Write the data
            CALL UDSIO_BUF_WRITE( LUN, IO_BUFF, IOS )
            IF ( IOS .NE. 0 ) GO TO 203
          END DO

        END IF

      ELSE ! Illegal Read Or Write flag

        IF ( MSG ) WRITE( *, '(/2A/1X,A/)', IOSTAT=IOS_W )
     &     ' *** UDSIOX ERROR - ',
     &     'Illegal Read-Or-Write flag value for file:', FNAM(:LF)
        IOS = -5
        RETURN

      END IF

      RETURN


      ! Error handlers _________________________________________________

      ! Specification read error
  201 IF ( MSG ) WRITE( *, '(/2A/1X,A/)', IOSTAT=IOS_W )
     & ' *** UDSIOX ERROR - ',
     & 'Specification data read error in file:', FNAM(:LF)
      IOS = -2
      RETURN

      ! Measurement read error
  202 IF ( MSG ) WRITE( *, '(/2A/1X,A/)', IOSTAT=IOS_W )
     & ' *** UDSIOX ERROR - ',
     & 'Measurement data read error in file:', FNAM(:LF)
      IOS = -4
      RETURN

      ! File write error
  203 IF ( MSG ) WRITE( *, '(/2A/1X,A/)', IOSTAT=IOS_W )
     & ' *** UDSIOX ERROR - ',
     & 'File write error on file:', FNAM(:LF)
      IOS = -6
      RETURN

      END



      SUBROUTINE UDSIO_BUF_READ( LUN, BUFF, IOS )

!***********************************************************************
!* Reads to a Buffer
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/17
!***********************************************************************


      ! Arguments ______________________________________________________

      INTEGER LUN ! Logical i/o unit

      CHARACTER*(*) BUFF ! Buffer

      INTEGER IOS ! Status flag


      ! Read buffer
      READ( LUN, IOSTAT=IOS ) BUFF

      RETURN
      END



      SUBROUTINE UDSIO_BUF_WRITE( LUN, BUFF, IOS )

!***********************************************************************
!* Writes from a Buffer
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/17
!***********************************************************************


      ! Arguments ______________________________________________________

      INTEGER LUN ! Logical i/o unit

      CHARACTER*(*) BUFF ! Buffer

      INTEGER IOS ! Status flag


      ! Write buffer
      WRITE( LUN, IOSTAT=IOS ) BUFF

      RETURN
      END
