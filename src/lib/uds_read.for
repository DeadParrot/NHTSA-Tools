      SUBROUTINE UDS_READ( FILE_NAME, U, C, IOS )

!***********************************************************************
!* UDS File Read Routine
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/17
!***********************************************************************

      ! Headers
      INCLUDE 'platform.fi'
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) FILE_NAME ! UDS file name

      RECORD /UDS/  U ! UDS object

      RECORD /UDS_CONTROL/  C ! UDS_CONTROL object

      INTEGER IOS ! Status flag returned


      ! Variables ______________________________________________________

      LOGICAL MSG, FILE_X, DONE, CHAN_READ, FIRST, Y_READ, X_READ,
     & W_READ, ASSIGN_DATA

      INTEGER IOS_W, LF, LUN, I, IP, IR, IS, IE, NCHAN_I,
     & NFP_I, NLP_I, NFP_O, NLP_O, NDREC, W_INT(125)

      REAL DEL_P

      DOUBLE PRECISION DD

      CHARACTER NUM_P*5, NUM_I*5, NUM_O*5,
     & ENDIAN_P*6, ENDIAN_I*6, ENDIAN_O*6, DIM_I*3, DIM_O*3,
     & REC_HDR*12, REC_HDR_B*12, CHANFORM_B*10, IN_BUFF*512,
     & YUN_I*20, XUN_I*20, VEL_TYP*20, WT_TYP*20, DATA_F

      PARAMETER ( VEL_TYP = 'VELOCITY', WT_TYP = 'WEIGHT' )

      PARAMETER ( NUM_P = NUMFORM_P )

      EQUIVALENCE ( IN_BUFF(1:12), REC_HDR )

      EQUIVALENCE ( IN_BUFF(13:), W_INT )

      EQUIVALENCE ( IN_BUFF(17:26), CHANFORM_B )


      ! Initializations
      CHAN_READ = .FALSE.
      IOS = 0
      LF = L_TRIM( FILE_NAME )
      MSG = ( .NOT. C.NO_MSG )


      ! Check array bounds
      IF ( NFP_L .GT. NLP_U ) THEN
        IF ( MSG ) WRITE( *, '(/2A/)' ) ' *** UDS_READ ERROR - ',
     &     'Crossed array bounds: NFP_L > NLP_U'
        IOS = -2
        RETURN
      ELSE IF ( NFP_L .EQ. NLP_U ) THEN
        IF ( MSG ) WRITE( *, '(/2A,I7/)', IOSTAT=IOS_W )
     &     ' *** UDS_READ WARNING - ',
     &     'Equal array bounds: NFP_L=NLP_U=', NLP_U
      END IF

      ! Check conversion controls
      IF ( ( .NOT. BLANK( C.NUMFORM ) ) .AND.
     & ( C.NUMFORM .NE. 'VAX' ) .AND. ( C.NUMFORM .NE. 'IEEE' ) .AND.
     & ( C.NUMFORM .NE. 'SUN' ) .AND. ( C.NUMFORM .NE. 'NONE' ) ) THEN
        IF ( MSG ) WRITE( *, '(/3A/)' ) ' *** UDS_READ ERROR - ',
     &     'Unsupported NUMFORM control: ', C.NUMFORM
        IOS = -2
        RETURN
      END IF
      IF ( ( .NOT. BLANK( C.DIMSYS ) ) .AND.
     & ( C.DIMSYS .NE. 'MET' ) .AND.
     & ( C.DIMSYS .NE. 'SI' ) .AND.
     & ( C.DIMSYS .NE. 'ENG' ) ) THEN
        IF ( MSG ) WRITE( *, '(/3A/)' ) ' *** UDS_READ ERROR - ',
     &     'Unsupported DIMSYS control: ', C.DIMSYS
        IOS = -2
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
     &     ' *** UDS_READ ERROR - ',
     &     'Unrecognized platform number format ', NUM_P
        IOS = -2
        RETURN
      END IF


      ! Open file
      INQUIRE( FILE=FILE_NAME, EXIST=FILE_X, IOSTAT=IOS )
      IF ( ( .NOT. FILE_X ) .OR. ( IOS .NE. 0 ) ) THEN
        IF ( MSG ) WRITE( *, '(/2A/1X,A)', IOSTAT=IOS_W )
     &     ' *** UDS_READ ERROR - ',
     &     'No such file:', FILE_NAME(:LF)
        IOS = -3
        RETURN
      END IF
      CALL UDS_OPEN( LUN, FILE_NAME, 'R', IOS )
      IF ( IOS .NE. 0 ) THEN
        IF ( MSG ) WRITE( *, '(/2A/1X,A)', IOSTAT=IOS_W )
     &     ' *** UDS_READ ERROR - ',
     &     'File open failed - Not a UDS-1992 file:', FILE_NAME(:LF)
        IOS = -3
        CALL UDS_CLOSE( LUN )
        RETURN
      END IF


      ! Initialize UDS fields
      CALL UDS_INIT( U )


      ! Read header record
      CALL BUF_READ( LUN, IN_BUFF, IOS )
      IF ( IOS .NE. 0 ) GO TO 201

      ! Check file version field
      IF ( REC_HDR .NE. 'UDS-1992' ) THEN
        IF ( MSG ) WRITE( *, '(/2A/1X,A)', IOSTAT=IOS_W )
     &     ' *** UDS_READ ERROR - ',
     &     'Not a UDS-1992 file:', FILE_NAME(:LF)
        IOS = -3
        CALL UDS_CLOSE( LUN )
        RETURN
      END IF

      ! Load file header fields
      IP = 1
      CALL STR_READ( U.FILEVER, IN_BUFF, IP )
      CALL INT_READ( U.NCHAN, IN_BUFF, IP )
      CALL STR_READ( U.FILEFORM, IN_BUFF, IP )
      CALL STR_READ( U.NUMFORM, IN_BUFF, IP )
      CALL STR_READ( U.DIMSYS, IN_BUFF, IP )
      CALL STR_READ( U.TSTSRC, IN_BUFF, IP )
      CALL INT_READ( U.TSTNO, IN_BUFF, IP )
      CALL STR_READ( U.TSTNAM, IN_BUFF, IP )
      CALL STR_READ( U.TITLE, IN_BUFF, IP )
      CALL STR_READ( U.TSTPRF, IN_BUFF, IP )
      CALL STR_READ( U.TSTREF, IN_BUFF, IP )
      CALL STR_READ( U.TSTCFN, IN_BUFF, IP )
      CALL INT_READ( U.IMPANG, IN_BUFF, IP )
      CALL REAL_READ( U.CLSSPD, IN_BUFF, IP )
      CALL INT_READ( U.CMPNO, IN_BUFF, IP )
      CALL STR_READ( U.CMPTYP, IN_BUFF, IP )
      CALL STR_READ( U.CMPDSC, IN_BUFF, IP )
      CALL STR_READ( U.MAKE, IN_BUFF, IP )
      CALL STR_READ( U.MODEL, IN_BUFF, IP )
      CALL INT_READ( U.YEAR, IN_BUFF, IP )
      CALL STR_READ( U.BODY, IN_BUFF, IP )
      CALL STR_READ( U.ENGINE, IN_BUFF, IP )
      CALL REAL_READ( U.CMPWGT, IN_BUFF, IP )
      CALL REAL_READ( U.CMPSPD, IN_BUFF, IP )
      CALL STR_READ( U.OCCTYP, IN_BUFF, IP )
      CALL INT_READ( U.OCCAGE, IN_BUFF, IP )
      CALL STR_READ( U.OCCSEX, IN_BUFF, IP )
      CALL REAL_READ( U.OCCWT, IN_BUFF, IP )
      CALL STR_READ( U.DUMSIZ, IN_BUFF, IP )
      CALL STR_READ( U.RESTR1, IN_BUFF, IP )
      CALL STR_READ( U.RESTR2, IN_BUFF, IP )
      CALL REAL_READ( U.HIC, IN_BUFF, IP )
      CALL REAL_READ( U.T1, IN_BUFF, IP )
      CALL REAL_READ( U.T2, IN_BUFF, IP )
      CALL REAL_READ( U.HICDTUP, IN_BUFF, IP )
      CALL REAL_READ( U.CLIP3M, IN_BUFF, IP )
      CALL REAL_READ( U.CSI, IN_BUFF, IP )
      CALL STR_READ( U.AIS, IN_BUFF, IP )
      CALL STR_READ( U.HDR_RESV, IN_BUFF, IP )

      ! Check file header fields
      IF ( ( .NOT. BLANK( C.FILEVER ) ) .AND.
     & ( U.FILEVER .NE. C.FILEVER ) ) THEN
        IF ( MSG ) WRITE( *, '(/6A/1X,A/)', IOSTAT=IOS_W )
     &     ' *** UDS_READ ERROR - ',
     &     'File version is ', U.FILEVER(:L_TRIM(U.FILEVER)),
     &     ' not ', C.FILEVER(:L_TRIM(C.FILEVER)), ' in file:',
     &     FILE_NAME(:LF)
        IOS = -5
        GO TO 200
      END IF
      IF ( ( U.NUMFORM .NE. 'VAX' ) .AND.
     & ( U.NUMFORM .NE. 'IEEE' ) .AND. ( U.NUMFORM .NE. 'SUN' ) ) THEN
        IF ( MSG ) WRITE( *, '(/4A/1X,A/)', IOSTAT=IOS_W )
     &     ' *** UDS_READ ERROR - ',
     &     'Unsupported real number format: ', U.NUMFORM,
     &     ' in file:', FILE_NAME(:LF)
        IOS = -5
        GO TO 200
      END IF
      IF ( ( U.DIMSYS .NE. 'MET' ) .AND.
     & ( U.DIMSYS .NE. 'SI' ) .AND.
     & ( U.DIMSYS .NE. 'ENG' ) ) THEN
        IF ( MSG ) WRITE( *, '(/4A/1X,A/)', IOSTAT=IOS_W )
     &     ' *** UDS_READ ERROR - ',
     &     'Unsupported dimensional system: ', U.DIMSYS,
     &     ' in file:', FILE_NAME(:LF)
        IOS = -5
        GO TO 200
      END IF
      IF ( ( .NOT. BLANK( C.FILEFORM ) ) .AND.
     & ( U.FILEFORM .NE. C.FILEFORM ) ) THEN
        IF ( MSG ) WRITE( *, '(/6A/1X,A/)', IOSTAT=IOS_W )
     &     ' *** UDS_READ ERROR - ',
     &     'File format is ', U.FILEFORM(:L_TRIM(U.FILEFORM)),
     &     ' not ', C.FILEFORM(:L_TRIM(C.FILEFORM)), ' in file:',
     &     FILE_NAME(:LF)
        IOS = -5
        GO TO 200
      END IF

      ! Set up byte order flags
      IF ( ( U.NUMFORM .EQ. 'VAX' ) .OR.
     & ( U.NUMFORM .EQ. 'IEEE' ) ) THEN
        ENDIAN_I = 'LITTLE'
      ELSE IF ( U.NUMFORM .EQ. 'SUN' ) THEN
        ENDIAN_I = 'BIG'
      END IF
      IF ( ( C.NUMFORM .EQ. 'VAX' ) .OR.
     & ( C.NUMFORM .EQ. 'IEEE' ) ) THEN
        ENDIAN_O = 'LITTLE'
      ELSE IF ( C.NUMFORM .EQ. 'SUN' ) THEN
        ENDIAN_O = 'BIG'
      ELSE IF ( C.NUMFORM .EQ. 'NONE' ) THEN
        ENDIAN_O = ENDIAN_I
      ELSE ! Default to local platform byte order
        ENDIAN_O = ENDIAN_P
      END IF

      ! Set up number format conversion flags
      IF ( ( C.NUMFORM .EQ. 'VAX' ) .OR.
     & ( C.NUMFORM .EQ. 'IEEE' ) .OR.
     & ( C.NUMFORM .EQ. 'SUN' ) ) THEN
        NUM_O = C.NUMFORM
      ELSE IF ( C.NUMFORM .EQ. 'NONE' ) THEN
        NUM_O = U.NUMFORM
      ELSE ! Default to local platform format
        NUM_O = NUM_P
      END IF
      NUM_I = U.NUMFORM
      U.NUMFORM = NUM_O

      ! Set up dimensional system conversion flags
      IF ( ( C.DIMSYS .EQ. 'MET' ) .OR.
     & ( C.DIMSYS .EQ. 'SI' ) .OR.
     & ( C.DIMSYS .EQ. 'ENG' ) ) THEN
        DIM_O = C.DIMSYS
      ELSE ! No change
        DIM_O = U.DIMSYS
      END IF
      DIM_I = U.DIMSYS
      U.DIMSYS = DIM_O

      ! Check/set number of channels = 1
      NCHAN_I = U.NCHAN
      CALL INT_CONV( NCHAN_I, ENDIAN_I, ENDIAN_P )
      IF ( NCHAN_I .GT. 1 ) THEN
        IF ( MSG ) WRITE( *, '(/2A/1X,A/)', IOSTAT=IOS_W )
     &     ' *** UDS_READ WARNING - ',
     &     'Only one channel read from file:', FILE_NAME(:LF)
        U.NCHAN = 1
        CALL INT_CONV( U.NCHAN, ENDIAN_P, ENDIAN_I )
      END IF

      ! Byte reordering
      IF ( ENDIAN_I .NE. ENDIAN_O ) THEN
        CALL INT_CONV( U.NCHAN, ENDIAN_I, ENDIAN_O )
        CALL INT_CONV( U.TSTNO, ENDIAN_I, ENDIAN_O )
        CALL INT_CONV( U.IMPANG, ENDIAN_I, ENDIAN_O )
        CALL INT_CONV( U.CMPNO, ENDIAN_I, ENDIAN_O )
        CALL INT_CONV( U.YEAR, ENDIAN_I, ENDIAN_O )
        CALL INT_CONV( U.OCCAGE, ENDIAN_I, ENDIAN_O )
      END IF

      ! Number format conversion
      IF ( NUM_I .NE. NUM_O ) THEN
        CALL FPN_CONV( U.CLSSPD, NUM_I, NUM_O )
        CALL FPN_CONV( U.CMPWGT, NUM_I, NUM_O )
        CALL FPN_CONV( U.CMPSPD, NUM_I, NUM_O )
        CALL FPN_CONV( U.OCCWT, NUM_I, NUM_O )
        CALL FPN_CONV( U.HIC, NUM_I, NUM_O )
        CALL FPN_CONV( U.T1, NUM_I, NUM_O )
        CALL FPN_CONV( U.T2, NUM_I, NUM_O )
        CALL FPN_CONV( U.HICDTUP, NUM_I, NUM_O )
        CALL FPN_CONV( U.CLIP3M, NUM_I, NUM_O )
        CALL FPN_CONV( U.CSI, NUM_I, NUM_O )
      END IF

      ! Dimensional system conversion
      IF ( DIM_I .NE. DIM_O ) THEN
        CALL DSN_CONV( U.CLSSPD, VEL_TYP, ' ', DIM_I, DIM_O, NUM_O )
        CALL DSN_CONV( U.CMPWGT, WT_TYP, ' ', DIM_I, DIM_O, NUM_O )
        CALL DSN_CONV( U.CMPSPD, VEL_TYP, ' ', DIM_I, DIM_O, NUM_O )
        CALL DSN_CONV( U.OCCWT, WT_TYP, ' ', DIM_I, DIM_O, NUM_O )
      END IF

      ! Read rest of records
      DONE = .FALSE.
      CHAN_READ = .FALSE.
      DO WHILE ( .NOT. DONE )

        ! Read the record
        CALL BUF_READ( LUN, IN_BUFF, IOS )
        IF ( IOS .EQ. -1 ) THEN
          GO TO 200
        ELSE IF ( IOS .NE. 0 ) THEN
          GO TO 201
        END IF

        IF ( REC_HDR .EQ. 'CMP-2 HDR' ) THEN ! Component-2 record

          ! Load component-2 fields
          IP = 13
          CALL INT_READ( U.CMPNO2, IN_BUFF, IP )
          CALL STR_READ( U.CMPTYP2, IN_BUFF, IP )
          CALL STR_READ( U.CMPDSC2, IN_BUFF, IP )
          CALL STR_READ( U.MAKE2, IN_BUFF, IP )
          CALL STR_READ( U.MODEL2, IN_BUFF, IP )
          CALL INT_READ( U.YEAR2, IN_BUFF, IP )
          CALL STR_READ( U.BODY2, IN_BUFF, IP )
          CALL STR_READ( U.ENGINE2, IN_BUFF, IP )
          CALL REAL_READ( U.CMPWGT2, IN_BUFF, IP )
          CALL REAL_READ( U.CMPSPD2, IN_BUFF, IP )
          CALL STR_READ( U.OCCTYP2, IN_BUFF, IP )
          CALL INT_READ( U.OCCAGE2, IN_BUFF, IP )
          CALL STR_READ( U.OCCSEX2, IN_BUFF, IP )
          CALL REAL_READ( U.OCCWT2, IN_BUFF, IP )
          CALL STR_READ( U.DUMSIZ2, IN_BUFF, IP )
          CALL STR_READ( U.RESTR12, IN_BUFF, IP )
          CALL STR_READ( U.RESTR22, IN_BUFF, IP )
          CALL STR_READ( U.CMP2_RESV, IN_BUFF, IP )

          ! Byte reordering
          IF ( ENDIAN_I .NE. ENDIAN_O ) THEN
            CALL INT_CONV( U.CMPNO2, ENDIAN_I, ENDIAN_O )
            CALL INT_CONV( U.YEAR2, ENDIAN_I, ENDIAN_O )
            CALL INT_CONV( U.OCCAGE2, ENDIAN_I, ENDIAN_O )
          END IF

          ! Number format conversion
          IF ( NUM_I .NE. NUM_O ) THEN
            CALL FPN_CONV( U.CMPWGT2, NUM_I, NUM_O )
            CALL FPN_CONV( U.CMPSPD2, NUM_I, NUM_O )
            CALL FPN_CONV( U.OCCWT2, NUM_I, NUM_O )
          END IF

          ! Dimensional system conversion
          IF ( DIM_I .NE. DIM_O ) THEN
            CALL DSN_CONV( U.CMPWGT2, WT_TYP, ' ', DIM_I, DIM_O,
     &         NUM_O )
            CALL DSN_CONV( U.CMPSPD2, VEL_TYP, ' ', DIM_I, DIM_O,
     &         NUM_O )
            CALL DSN_CONV( U.OCCWT2, WT_TYP, ' ', DIM_I, DIM_O,
     &         NUM_O )
          END IF

        ELSE IF ( ( REC_HDR .EQ. 'CURVE HDR' ) .AND.
     &     ( ( BLANK( C.CHANFORM ) ) .OR.
     &     ( CHANFORM_B .EQ. C.CHANFORM ) ) ) THEN ! Curve channel

          ! Load curve header fields
          IP = 13
          CALL INT_READ( U.ICHAN, IN_BUFF, IP )
          CALL STR_READ( U.CHANFORM, IN_BUFF, IP )
          CALL INT_READ( U.CURNO, IN_BUFF, IP )
          CALL STR_READ( U.CURNAM, IN_BUFF, IP )
          CALL STR_READ( U.SENATT, IN_BUFF, IP )
          CALL STR_READ( U.SENLOC, IN_BUFF, IP )
          CALL INT_READ( U.SENNUM, IN_BUFF, IP )
          CALL INT_READ( U.BANDNO, IN_BUFF, IP )
          CALL INT_READ( U.GAGNO, IN_BUFF, IP )
          CALL STR_READ( U.AXIS, IN_BUFF, IP )
          CALL STR_READ( U.YTYP, IN_BUFF, IP )
          CALL STR_READ( U.YUNITS, IN_BUFF, IP )
          CALL STR_READ( U.XTYP, IN_BUFF, IP )
          CALL STR_READ( U.XUNITS, IN_BUFF, IP )
          CALL STR_READ( U.STATUS, IN_BUFF, IP )
          CALL STR_READ( U.CURTYP, IN_BUFF, IP )
          CALL STR_READ( U.CURDSC, IN_BUFF, IP )
          CALL INT_READ( U.NFP, IN_BUFF, IP )
          CALL INT_READ( U.NLP, IN_BUFF, IP )
          CALL REAL_READ( U.DEL, IN_BUFF, IP )
          CALL REAL_READ( U.INIVEL, IN_BUFF, IP )
          CALL REAL_READ( U.PREF, IN_BUFF, IP )
          CALL REAL_READ( U.FCUT, IN_BUFF, IP )
          CALL REAL_READ( U.FCOR, IN_BUFF, IP )
          CALL REAL_READ( U.FSTP, IN_BUFF, IP )
          CALL REAL_READ( U.SCLFAC, IN_BUFF, IP )
          CALL INT_READ( U.ID1, IN_BUFF, IP )
          CALL INT_READ( U.ID2, IN_BUFF, IP )
          CALL INT_READ( U.ID3, IN_BUFF, IP )
          CALL INT_READ( U.ID4, IN_BUFF, IP )
          CALL INT_READ( U.ID5, IN_BUFF, IP )
          CALL REAL_READ( U.RD1, IN_BUFF, IP )
          CALL REAL_READ( U.RD2, IN_BUFF, IP )
          CALL REAL_READ( U.RD3, IN_BUFF, IP )
          CALL REAL_READ( U.RD4, IN_BUFF, IP )
          CALL REAL_READ( U.RD5, IN_BUFF, IP )
          CALL STR_READ( U.CD1, IN_BUFF, IP )
          CALL STR_READ( U.CD2, IN_BUFF, IP )
          CALL STR_READ( U.CURV_RESV, IN_BUFF, IP )

          ! Flag that a channel was read
          CHAN_READ = .TRUE.

          ! Byte reordering
          IF ( ENDIAN_I .NE. ENDIAN_O ) THEN
            CALL INT_CONV( U.ICHAN, ENDIAN_I, ENDIAN_O )
            CALL INT_CONV( U.CURNO, ENDIAN_I, ENDIAN_O )
            CALL INT_CONV( U.SENNUM, ENDIAN_I, ENDIAN_O )
            CALL INT_CONV( U.BANDNO, ENDIAN_I, ENDIAN_O )
            CALL INT_CONV( U.GAGNO, ENDIAN_I, ENDIAN_O )
            CALL INT_CONV( U.NFP, ENDIAN_I, ENDIAN_O )
            CALL INT_CONV( U.NLP, ENDIAN_I, ENDIAN_O )
            CALL INT_CONV( U.ID1, ENDIAN_I, ENDIAN_O )
            CALL INT_CONV( U.ID2, ENDIAN_I, ENDIAN_O )
            CALL INT_CONV( U.ID3, ENDIAN_I, ENDIAN_O )
            CALL INT_CONV( U.ID4, ENDIAN_I, ENDIAN_O )
            CALL INT_CONV( U.ID5, ENDIAN_I, ENDIAN_O )
          END IF

          ! Number format conversion
          IF ( NUM_I .EQ. NUM_P ) THEN
            DEL_P = U.DEL
          ELSE
            CALL FPN_CONV( U.DEL, NUM_I, NUM_P )
            DEL_P = U.DEL
            CALL FPN_CONV( U.DEL, NUM_P, NUM_I )
          END IF
          IF ( NUM_I .NE. NUM_O ) THEN
            CALL FPN_CONV( U.DEL, NUM_I, NUM_O )
            CALL FPN_CONV( U.INIVEL, NUM_I, NUM_O )
            CALL FPN_CONV( U.PREF, NUM_I, NUM_O )
            CALL FPN_CONV( U.FCUT, NUM_I, NUM_O )
            CALL FPN_CONV( U.FCOR, NUM_I, NUM_O )
            CALL FPN_CONV( U.FSTP, NUM_I, NUM_O )
            CALL FPN_CONV( U.SCLFAC, NUM_I, NUM_O )
            CALL FPN_CONV( U.RD1, NUM_I, NUM_O )
            CALL FPN_CONV( U.RD2, NUM_I, NUM_O )
            CALL FPN_CONV( U.RD3, NUM_I, NUM_O )
            CALL FPN_CONV( U.RD4, NUM_I, NUM_O )
            CALL FPN_CONV( U.RD5, NUM_I, NUM_O )
          END IF

          ! Dimensional system conversion
          YUN_I = U.YUNITS
          XUN_I = U.XUNITS
          IF ( DIM_I .NE. DIM_O ) THEN
            CALL DSU_CONV( U.YTYP, U.YUNITS, DIM_I, DIM_O, DD )
            CALL DSU_CONV( U.XTYP, U.XUNITS, DIM_I, DIM_O, DD )
            IF ( U.CHANFORM .EQ. 'Y' ) THEN
              CALL DSN_CONV( U.DEL, U.XTYP, XUN_I, DIM_I, DIM_O,
     &           NUM_O )
              CALL DSN_CONV( DEL_P, U.XTYP, XUN_I, DIM_I, DIM_O,
     &           NUM_P )
            END IF
            CALL DSN_CONV( U.INIVEL, VEL_TYP, ' ', DIM_I, DIM_O,
     &         NUM_O )
          END IF

          ! Check array indices
          NFP_I = U.NFP
          CALL INT_CONV( NFP_I, ENDIAN_O, ENDIAN_P )
          NFP_O = NFP_I
          NLP_I = U.NLP
          CALL INT_CONV( NLP_I, ENDIAN_O, ENDIAN_P )
          NLP_O = NLP_I
          IF ( NFP_I .GT. NLP_I ) THEN
            IF ( MSG ) WRITE( *, '(/2A/1X,A/)', IOSTAT=IOS_W )
     &         ' *** UDS_READ ERROR - ',
     &         'Crossed array indices: NFP > NLP in file:',
     &         FILE_NAME(:LF)
            IOS = -5
            GO TO 200
          ELSE IF ( NFP_I .EQ. NLP_I ) THEN
            IF ( MSG ) WRITE( *, '(/2A,I7,A/1X,A/)', IOSTAT=IOS_W )
     &         ' *** UDS_READ WARNING - ',
     &         'Equal array indices: NFP=NLP=', NLP_I, ' in file:',
     &         FILE_NAME(:LF)
          END IF

          ! Read curve data
          IF ( C.SPECS_ONLY ) THEN ! Done - Return

            DONE = .TRUE.

          ELSE IF ( ( U.CHANFORM .EQ. 'Y' ) .OR.
     &       ( U.CHANFORM .EQ. 'X-Y' ) ) THEN

            ! Check array indices against bounds
            IF ( ( NLP_I .LT. NFP_L ) .OR.
     &         ( NFP_I .GT. NLP_U ) ) THEN
              IF ( MSG ) WRITE( *, '(/2A/1X,A/)', IOSTAT=IOS_W )
     &           ' *** UDS_READ ERROR - ',
     &           'Array indices outside valid range in file:',
     &           FILE_NAME(:LF)
              IOS = -5
              GO TO 200
            END IF
            IF ( NFP_I .LT. NFP_L ) THEN
              IF ( MSG ) WRITE( *, '(/2A,I7,A/1X,A/)',
     &           IOSTAT=IOS_W )
     &           ' *** UDS_READ WARNING - ',
     &           'Array lower index truncated to: ', NFP_L,
     &           ' in read from file:', FILE_NAME(:LF)
              NFP_O = NFP_L
              U.NFP = NFP_L
              CALL INT_CONV( U.NFP, ENDIAN_P, ENDIAN_O )
            END IF
            IF ( NLP_I .GT. NLP_U ) THEN
              IF ( MSG ) WRITE( *, '(/2A,I7,A/1X,A/)',
     &           IOSTAT=IOS_W )
     &           ' *** UDS_READ WARNING - ',
     &           'Array upper index truncated to: ', NLP_U,
     &           ' in read from file:', FILE_NAME(:LF)
              NLP_O = NLP_U
              U.NLP = NLP_U
              CALL INT_CONV( U.NLP, ENDIAN_P, ENDIAN_O )
            END IF

            ! Read the data records
            NDREC = ( NLP_I - NFP_I ) / 125 + 1
            Y_READ = .FALSE.
            IF ( U.CHANFORM .EQ. 'X-Y' ) THEN
              X_READ = .FALSE.
            ELSE
              X_READ = .TRUE.
            END IF
            DO WHILE ( .NOT. ( Y_READ .AND. X_READ ) )

              ! Read the first data record
              CALL BUF_READ( LUN, IN_BUFF, IOS )
              IF ( IOS .NE. 0 ) GO TO 201

              ! Set up for type of data
              ASSIGN_DATA = .TRUE.
              IF ( REC_HDR .EQ. 'Y DATA' ) THEN
                DATA_F = 'Y'
                W_READ = Y_READ
                Y_READ = .TRUE.
              ELSE IF ( ( REC_HDR .EQ. 'X DATA' ) .AND.
     &           ( U.CHANFORM .EQ. 'X-Y' ) ) THEN
                DATA_F = 'X'
                W_READ = X_READ
                X_READ = .TRUE.
              ELSE
                IF ( MSG ) WRITE( *, '(/5A/1X,A/)',
     &             IOSTAT=IOS_W )
     &             ' *** UDS_READ WARNING - ',
     &             REC_HDR(:L_TRIM(REC_HDR)),
     &             ' records skipped in read from ',
     &             U.CHANFORM(:L_TRIM(U.CHANFORM)),
     &             ' channel in file:', FILE_NAME(:LF)
                ASSIGN_DATA = .FALSE.
              END IF

              ! Check for repeat data
              IF ( ( ASSIGN_DATA ) .AND. ( W_READ ) ) THEN
                IF ( MSG ) WRITE( *, '(/4A/1X,A/)',
     &             IOSTAT=IOS_W )
     &             ' *** UDS_READ WARNING - ',
     &             'Repeat ', REC_HDR(:L_TRIM(REC_HDR)),
     &             ' records ignored in file:', FILE_NAME(:LF)
                ASSIGN_DATA = .FALSE.
              END IF

              ! Assign data
              FIRST = .TRUE.
              REC_HDR_B = REC_HDR
              DO IR = 1, NDREC
                IF ( FIRST ) THEN
                  FIRST = .FALSE.
                ELSE
                  CALL BUF_READ( LUN, IN_BUFF, IOS )
                  IF ( IOS .NE. 0 ) GO TO 201
                END IF
                IF ( REC_HDR .NE. REC_HDR_B ) THEN
                  IF ( MSG ) WRITE( *, '(/7A/1X,A/)',
     &               IOSTAT=IOS_W )
     &               ' *** UDS_READ ERROR - ', DATA_F,
     &               ' data record type is ',
     &               REC_HDR(:L_TRIM(REC_HDR)), ' not ',
     &               REC_HDR_B(:L_TRIM(REC_HDR_B)),
     &               ' in file:', FILE_NAME(:LF)
                  IOS = -4
                  RETURN
                END IF
                IF ( ASSIGN_DATA ) THEN
                  IS = NFP_I + 125*(IR-1)
                  IE = MIN( IS+124, NLP_O )
                  IS = MAX( IS, NFP_O )
                  IF ( REC_HDR .EQ. 'Y DATA' ) THEN
                    DO I = IS, IE
                      U.Y_INT(I) = W_INT(I-IS+1)
                    END DO
                  ELSE IF ( REC_HDR .EQ. 'X DATA' ) THEN
                    DO I = IS, IE
                      U.X_INT(I) = W_INT(I-IS+1)
                    END DO
                  END IF
                END IF
              END DO

              ! Number format conversion
              IF ( NUM_I .NE. NUM_O ) THEN
                IF ( REC_HDR .EQ. 'Y DATA' ) THEN
                  CALL FPA_CONV( U.Y_INT(NFP_O), NLP_O-NFP_O+1,
     &               NUM_I, NUM_O )
                ELSE IF ( REC_HDR .EQ. 'X DATA' ) THEN
                  CALL FPA_CONV( U.X_INT(NFP_O), NLP_O-NFP_O+1,
     &               NUM_I, NUM_O )
                END IF
              END IF

              ! Dimensional system conversion
              IF ( DIM_I .NE. DIM_O ) THEN
                IF ( REC_HDR .EQ. 'Y DATA' ) THEN
                  CALL DSA_CONV( U.Y(NFP_O), NLP_O-NFP_O+1,
     &               U.YTYP, YUN_I, DIM_I, DIM_O, NUM_O )
                ELSE IF ( REC_HDR .EQ. 'X DATA' ) THEN
                  CALL DSA_CONV( U.X(NFP_O), NLP_O-NFP_O+1,
     &               U.XTYP, XUN_I, DIM_I, DIM_O, NUM_O )
                END IF
              END IF

            END DO

            ! Assign even X values for Y channels
            IF ( ( U.CHANFORM .EQ. 'Y' ) .AND.
     &         ( C.FILL_X ) ) THEN ! Assign X values

              ! Set even X values
              DO I = NFP_O, NLP_O
                U.X(I) = I * DEL_P
              END DO

              ! Number format conversion
              IF ( NUM_P .NE. NUM_O ) THEN
                CALL FPA_CONV( U.X_INT(NFP_O), NLP_O-NFP_O+1,
     &             NUM_P, NUM_O )
              END IF

            END IF

          ELSE IF ( MSG ) THEN

            WRITE( *, '(/4A/1X,A/)', IOSTAT=IOS_W )
     &         ' *** UDS_READ WARNING - ',
     &         'Unsupported channel format for data read: ',
     &         U.CHANFORM(:L_TRIM(U.CHANFORM)), ' in file:',
     &         FILE_NAME(:LF)

          END IF

          ! Set fields for single channel read
          U.FILEFORM = U.CHANFORM
          U.ICHAN = 1
          CALL INT_CONV( U.ICHAN, ENDIAN_P, ENDIAN_O )

          DONE = .TRUE.

        ELSE IF ( REC_HDR .EQ. 'CURVE HDR' ) THEN ! Wrong channel

          DONE = .TRUE.

        END IF

      END DO


      ! Close UDS file and return
  200 CALL UDS_CLOSE( LUN )
      IF ( ( .NOT. CHAN_READ ) .AND. ( IOS .EQ. 0 ) ) THEN
        IF ( MSG ) WRITE( *, '(/2A/1X,A/)', IOSTAT=IOS_W )
     &     ' *** UDS_READ ERROR - ',
     &     'Supported channel format not found in file:', FILE_NAME(:LF)
        IOS = -1
      END IF
      RETURN


      ! Error handlers _________________________________________________

      ! File read error
  201 IF ( MSG ) WRITE( *, '(/2A/1X,A/)', IOSTAT=IOS_W )
     & ' *** UDS_READ ERROR - ',
     & 'File read error on file:', FILE_NAME(:LF)
      IOS = -4
      CALL UDS_CLOSE( LUN )
      RETURN


      END
