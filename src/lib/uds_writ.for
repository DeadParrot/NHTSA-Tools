      SUBROUTINE UDS_WRITE( FILE_NAME, U, C, IOS )

!***********************************************************************
!* UDS File Write Routine
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

      LOGICAL MSG

      INTEGER IOS_W, LF, LUN, I, IP, IR, IS, IE, NCHAN_I,
     & NFP_I, NLP_I, NFP_O, NLP_O, NOUT, NDREC, W_INT(125)

      REAL W(125)

      DOUBLE PRECISION DD

      CHARACTER NUM_P*5, NUM_I*5, NUM_O*5,
     & ENDIAN_P*6, ENDIAN_I*6, ENDIAN_O*6, DIM_I*3, DIM_O*3,
     & REC_HDR*12, OUT_BUFF*512,
     & VEL_TYP*20, WT_TYP*20

      RECORD /UDS_SPEC/  S ! UDS_SPEC object of output specifications

      PARAMETER ( VEL_TYP = 'VELOCITY', WT_TYP = 'WEIGHT' )

      PARAMETER ( NUM_P = NUMFORM_P )

      EQUIVALENCE ( OUT_BUFF(1:12), REC_HDR )

      EQUIVALENCE ( OUT_BUFF(13:), W_INT, W )


      ! Initializations
      IOS = 0
      LF = L_TRIM( FILE_NAME )
      MSG = ( .NOT. C.NO_MSG )


      ! Check array bounds
      IF ( NFP_L .GT. NLP_U ) THEN
        IF ( MSG ) WRITE( *, '(/2A/)' ) ' *** UDS_WRITE ERROR - ',
     &     'Crossed array bounds: NFP_L > NLP_U'
        IOS = -2
        RETURN
      ELSE IF ( NFP_L .EQ. NLP_U ) THEN
        IF ( MSG ) WRITE( *, '(/2A,I7/)', IOSTAT=IOS_W )
     &     ' *** UDS_WRITE WARNING - ',
     &     'Equal array bounds: NFP_L=NLP_U=', NLP_U
      END IF

      ! Check conversion controls
      IF ( ( .NOT. BLANK( C.NUMFORM ) ) .AND.
     & ( C.NUMFORM .NE. 'VAX' ) .AND. ( C.NUMFORM .NE. 'IEEE' ) .AND.
     & ( C.NUMFORM .NE. 'SUN' ) .AND. ( C.NUMFORM .NE. 'NONE' ) ) THEN
        IF ( MSG ) WRITE( *, '(/3A/)' ) ' *** UDS_WRITE ERROR - ',
     &     'Unsupported NUMFORM control: ', C.NUMFORM
        IOS = -2
        RETURN
      END IF
      IF ( ( .NOT. BLANK( C.DIMSYS ) ) .AND.
     & ( C.DIMSYS .NE. 'MET' ) .AND.
     & ( C.DIMSYS .NE. 'SI' ) .AND.
     & ( C.DIMSYS .NE. 'ENG' ) ) THEN
        IF ( MSG ) WRITE( *, '(/3A/)' ) ' *** UDS_WRITE ERROR - ',
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
     &     ' *** UDS_WRITE ERROR - ',
     &     'Unrecognized platform number format ', NUM_P
        IOS = -2
        RETURN
      END IF

      ! Check UDS fields
      IF ( ( U.FILEVER .NE. 'UDS-1992' ) .AND.
     & ( U.FILEVER .NE. 'UDS-1991' ) .AND.
     & ( .NOT. BLANK( U.FILEVER ) ) ) THEN
        IF ( MSG ) WRITE( *, '(/4A/1X,A/)', IOSTAT=IOS_W )
     &     ' *** UDS_WRITE ERROR - ',
     &     'Invalid file version: ', U.FILEVER(:L_TRIM(U.FILEVER)),
     &     ' in write to file:', FILE_NAME(:LF)
        IOS = -5
        RETURN
      END IF
      IF ( ( U.CHANFORM .NE. 'Y' ) .AND. ( U.CHANFORM .NE. 'X-Y' ) )
     & THEN
        IF ( MSG ) WRITE( *, '(/4A/1X,A/)', IOSTAT=IOS_W )
     &     ' *** UDS_WRITE ERROR - ',
     &     'Unsupported channel format: ',
     &     U.CHANFORM(:L_TRIM(U.CHANFORM)), ' in write to file:',
     &     FILE_NAME(:LF)
        IOS = -5
        RETURN
      END IF
      IF ( BLANK( U.NUMFORM ) ) THEN
        NUM_I = NUM_P
      ELSE
        NUM_I = U.NUMFORM
      END IF
      IF ( ( NUM_I .NE. 'VAX' ) .AND.
     & ( NUM_I .NE. 'IEEE' ) .AND. ( NUM_I .NE. 'SUN' ) ) THEN
        IF ( MSG ) WRITE( *, '(/4A/1X,A/)', IOSTAT=IOS_W )
     &     ' *** UDS_WRITE ERROR - ',
     &     'Unsupported real number format: ', NUM_I,
     &     ' in write to file:', FILE_NAME(:LF)
        IOS = -5
        RETURN
      END IF
      IF ( ( U.DIMSYS .NE. 'MET' ) .AND.
     & ( U.DIMSYS .NE. 'SI' ) .AND.
     & ( U.DIMSYS .NE. 'ENG' ) ) THEN
        IF ( MSG ) WRITE( *, '(/4A/1X,A/)', IOSTAT=IOS_W )
     &     ' *** UDS_WRITE ERROR - ',
     &     'Unsupported dimensional system: ', U.DIMSYS,
     &     ' in write to file:', FILE_NAME(:LF)
        IOS = -5
        RETURN
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
        NUM_O = NUM_I
      ELSE ! Default to local platform format
        NUM_O = NUM_P
      END IF

      ! Set up dimensional system conversion flags
      IF ( ( C.DIMSYS .EQ. 'MET' ) .OR.
     & ( C.DIMSYS .EQ. 'SI' ) .OR.
     & ( C.DIMSYS .EQ. 'ENG' ) ) THEN
        DIM_O = C.DIMSYS
      ELSE ! No change
        DIM_O = U.DIMSYS
      END IF
      DIM_I = U.DIMSYS

      ! Check array indices
      NFP_I = U.NFP
      CALL INT_CONV( NFP_I, ENDIAN_I, ENDIAN_P )
      NFP_O = NFP_I
      NLP_I = U.NLP
      CALL INT_CONV( NLP_I, ENDIAN_I, ENDIAN_P )
      NLP_O = NLP_I
      IF ( NFP_I .GT. NLP_I ) THEN
        IF ( MSG ) WRITE( *, '(/2A/1X,A/)', IOSTAT=IOS_W )
     &     ' *** UDS_WRITE ERROR - ',
     &     'Crossed array indices: NFP > NLP in write to file:',
     &     FILE_NAME(:LF)
        IOS = -5
        RETURN
      ELSE IF ( NFP_I .EQ. NLP_I ) THEN
        IF ( MSG ) WRITE( *, '(/2A,I7,A/1X,A/)', IOSTAT=IOS_W )
     &     ' *** UDS_WRITE WARNING - ',
     &     'Equal array indices: NFP=NLP=', NLP_I,
     &     ' in write to file:', FILE_NAME(:LF)
      END IF
      IF ( ( NLP_I .LT. NFP_L ) .OR. ( NFP_I .GT. NLP_U ) ) THEN
        IF ( MSG ) WRITE( *, '(/2A/1X,A/)', IOSTAT=IOS_W )
     &     ' *** UDS_WRITE ERROR - ',
     &     'Array indices outside valid range in write to file:',
     &     FILE_NAME(:LF)
        IOS = -5
        RETURN
      END IF

      ! Load output specifications
      CALL UDS_SPEC_COPY( S, U )

      ! Byte reordering
      IF ( ENDIAN_I .NE. ENDIAN_O ) THEN
        CALL INT_CONV( S.NCHAN, ENDIAN_I, ENDIAN_O )
        CALL INT_CONV( S.TSTNO, ENDIAN_I, ENDIAN_O )
        CALL INT_CONV( S.IMPANG, ENDIAN_I, ENDIAN_O )
        CALL INT_CONV( S.CMPNO, ENDIAN_I, ENDIAN_O )
        CALL INT_CONV( S.YEAR, ENDIAN_I, ENDIAN_O )
        CALL INT_CONV( S.OCCAGE, ENDIAN_I, ENDIAN_O )
      END IF

      ! Dimensional system conversion
      IF ( DIM_I .NE. DIM_O ) THEN
        CALL DSN_CONV( S.CLSSPD, VEL_TYP, ' ', DIM_I, DIM_O, NUM_I )
        CALL DSN_CONV( S.CMPWGT, WT_TYP, ' ', DIM_I, DIM_O, NUM_I )
        CALL DSN_CONV( S.CMPSPD, VEL_TYP, ' ', DIM_I, DIM_O, NUM_I )
        CALL DSN_CONV( S.OCCWT, WT_TYP, ' ', DIM_I, DIM_O, NUM_I )
      END IF

      ! Number format conversion
      IF ( NUM_I .NE. NUM_O ) THEN
        CALL FPN_CONV( S.CLSSPD, NUM_I, NUM_O )
        CALL FPN_CONV( S.CMPWGT, NUM_I, NUM_O )
        CALL FPN_CONV( S.CMPSPD, NUM_I, NUM_O )
        CALL FPN_CONV( S.OCCWT, NUM_I, NUM_O )
        CALL FPN_CONV( S.HIC, NUM_I, NUM_O )
        CALL FPN_CONV( S.T1, NUM_I, NUM_O )
        CALL FPN_CONV( S.T2, NUM_I, NUM_O )
        CALL FPN_CONV( S.HICDTUP, NUM_I, NUM_O )
        CALL FPN_CONV( S.CLIP3M, NUM_I, NUM_O )
        CALL FPN_CONV( S.CSI, NUM_I, NUM_O )
      END IF

      ! Set up output specifications
      S.FILEVER = 'UDS-1992'
      S.FILEFORM = S.CHANFORM ! Only one channel supported
      S.NUMFORM = NUM_O
      S.DIMSYS = DIM_O

      ! Check/set number of channels = 1
      NCHAN_I = S.NCHAN
      CALL INT_CONV( NCHAN_I, ENDIAN_O, ENDIAN_P )
      IF ( NCHAN_I .GT. 1 ) THEN
        IF ( MSG ) WRITE( *, '(/2A/1X,A/)', IOSTAT=IOS_W )
     &     ' *** UDS_WRITE WARNING - ',
     &     'Only one channel written to file:', FILE_NAME(:LF)
        S.NCHAN = 1
        CALL INT_CONV( S.NCHAN, ENDIAN_P, ENDIAN_O )
      END IF


      ! Open file
      IF ( C.FN_INCR ) CALL FN_INCR( FILE_NAME )
      CALL UDS_OPEN( LUN, FILE_NAME, 'W', IOS )
      IF ( IOS .NE. 0 ) CALL UDS_OPEN( LUN, FILE_NAME, ' ', IOS )
      IF ( IOS .NE. 0 ) THEN
        IF ( MSG ) WRITE( *, '(/2A/1X,A/)', IOSTAT=IOS_W )
     &     ' *** UDS_WRITE ERROR - ',
     &     'File open failure on file:', FILE_NAME(:LF)
        GO TO 200
      END IF


      ! Load file header fields
      IP = 1
      CALL STR_WRITE( S.FILEVER, OUT_BUFF, IP )
      CALL INT_WRITE( S.NCHAN, OUT_BUFF, IP )
      CALL STR_WRITE( S.FILEFORM, OUT_BUFF, IP )
      CALL STR_WRITE( S.NUMFORM, OUT_BUFF, IP )
      CALL STR_WRITE( S.DIMSYS, OUT_BUFF, IP )
      CALL STR_WRITE( S.TSTSRC, OUT_BUFF, IP )
      CALL INT_WRITE( S.TSTNO, OUT_BUFF, IP )
      CALL STR_WRITE( S.TSTNAM, OUT_BUFF, IP )
      CALL STR_WRITE( S.TITLE, OUT_BUFF, IP )
      CALL STR_WRITE( S.TSTPRF, OUT_BUFF, IP )
      CALL STR_WRITE( S.TSTREF, OUT_BUFF, IP )
      CALL STR_WRITE( S.TSTCFN, OUT_BUFF, IP )
      CALL INT_WRITE( S.IMPANG, OUT_BUFF, IP )
      CALL REAL_WRITE( S.CLSSPD, OUT_BUFF, IP )
      CALL INT_WRITE( S.CMPNO, OUT_BUFF, IP )
      CALL STR_WRITE( S.CMPTYP, OUT_BUFF, IP )
      CALL STR_WRITE( S.CMPDSC, OUT_BUFF, IP )
      CALL STR_WRITE( S.MAKE, OUT_BUFF, IP )
      CALL STR_WRITE( S.MODEL, OUT_BUFF, IP )
      CALL INT_WRITE( S.YEAR, OUT_BUFF, IP )
      CALL STR_WRITE( S.BODY, OUT_BUFF, IP )
      CALL STR_WRITE( S.ENGINE, OUT_BUFF, IP )
      CALL REAL_WRITE( S.CMPWGT, OUT_BUFF, IP )
      CALL REAL_WRITE( S.CMPSPD, OUT_BUFF, IP )
      CALL STR_WRITE( S.OCCTYP, OUT_BUFF, IP )
      CALL INT_WRITE( S.OCCAGE, OUT_BUFF, IP )
      CALL STR_WRITE( S.OCCSEX, OUT_BUFF, IP )
      CALL REAL_WRITE( S.OCCWT, OUT_BUFF, IP )
      CALL STR_WRITE( S.DUMSIZ, OUT_BUFF, IP )
      CALL STR_WRITE( S.RESTR1, OUT_BUFF, IP )
      CALL STR_WRITE( S.RESTR2, OUT_BUFF, IP )
      CALL REAL_WRITE( S.HIC, OUT_BUFF, IP )
      CALL REAL_WRITE( S.T1, OUT_BUFF, IP )
      CALL REAL_WRITE( S.T2, OUT_BUFF, IP )
      CALL REAL_WRITE( S.HICDTUP, OUT_BUFF, IP )
      CALL REAL_WRITE( S.CLIP3M, OUT_BUFF, IP )
      CALL REAL_WRITE( S.CSI, OUT_BUFF, IP )
      CALL STR_WRITE( S.AIS, OUT_BUFF, IP )
      CALL STR_WRITE( S.HDR_RESV, OUT_BUFF, IP )

      ! Write header record
      CALL BUF_WRITE( LUN, OUT_BUFF, IOS )
      IF ( IOS .NE. 0 ) GO TO 201


      ! Component-2 record
      IF ( ( S.TSTSRC .EQ. 'COMPONENT DB' ) .OR.
     & ( U.CMPNO2 .NE. 0 ) ) THEN ! Write component-2 record

        ! Byte reordering
        IF ( ENDIAN_I .NE. ENDIAN_O ) THEN
          CALL INT_CONV( S.CMPNO2, ENDIAN_I, ENDIAN_O )
          CALL INT_CONV( S.YEAR2, ENDIAN_I, ENDIAN_O )
          CALL INT_CONV( S.OCCAGE2, ENDIAN_I, ENDIAN_O )
        END IF

        ! Dimensional system conversion
        IF ( DIM_I .NE. DIM_O ) THEN
          CALL DSN_CONV( S.CMPWGT2, WT_TYP, ' ', DIM_I, DIM_O,
     &       NUM_I )
          CALL DSN_CONV( S.CMPSPD2, VEL_TYP, ' ', DIM_I, DIM_O,
     &       NUM_I )
          CALL DSN_CONV( S.OCCWT2, WT_TYP, ' ', DIM_I, DIM_O,
     &       NUM_I )
        END IF

        ! Number format conversion
        IF ( NUM_I .NE. NUM_O ) THEN
          CALL FPN_CONV( S.CMPWGT2, NUM_I, NUM_O )
          CALL FPN_CONV( S.CMPSPD2, NUM_I, NUM_O )
          CALL FPN_CONV( S.OCCWT2, NUM_I, NUM_O )
        END IF

        ! Write component-2 record
        OUT_BUFF(:12) = 'CMP-2 HDR'
        IP = 13
        CALL INT_WRITE( S.CMPNO2, OUT_BUFF, IP )
        CALL STR_WRITE( S.CMPTYP2, OUT_BUFF, IP )
        CALL STR_WRITE( S.CMPDSC2, OUT_BUFF, IP )
        CALL STR_WRITE( S.MAKE2, OUT_BUFF, IP )
        CALL STR_WRITE( S.MODEL2, OUT_BUFF, IP )
        CALL INT_WRITE( S.YEAR2, OUT_BUFF, IP )
        CALL STR_WRITE( S.BODY2, OUT_BUFF, IP )
        CALL STR_WRITE( S.ENGINE2, OUT_BUFF, IP )
        CALL REAL_WRITE( S.CMPWGT2, OUT_BUFF, IP )
        CALL REAL_WRITE( S.CMPSPD2, OUT_BUFF, IP )
        CALL STR_WRITE( S.OCCTYP2, OUT_BUFF, IP )
        CALL INT_WRITE( S.OCCAGE2, OUT_BUFF, IP )
        CALL STR_WRITE( S.OCCSEX2, OUT_BUFF, IP )
        CALL REAL_WRITE( S.OCCWT2, OUT_BUFF, IP )
        CALL STR_WRITE( S.DUMSIZ2, OUT_BUFF, IP )
        CALL STR_WRITE( S.RESTR12, OUT_BUFF, IP )
        CALL STR_WRITE( S.RESTR22, OUT_BUFF, IP )
        CALL STR_WRITE( S.CMP2_RESV, OUT_BUFF, IP )
        CALL BUF_WRITE( LUN, OUT_BUFF, IOS )
        IF ( IOS .NE. 0 ) GO TO 201

      END IF


      ! Output curve specification record ______________________________

      ! Adjust output curve specifications
      S.ICHAN = 1
      CALL INT_CONV( S.ICHAN, ENDIAN_P, ENDIAN_I )
      IF ( NFP_I .LT. NFP_L ) THEN
        IF ( MSG ) WRITE( *, '(/2A,I7,A/1X,A/)', IOSTAT=IOS_W )
     &     ' *** UDS_WRITE WARNING - ',
     &     'Array lower index truncated to: ', NFP_L,
     &     ' in write to file:', FILE_NAME(:LF)
        NFP_O = NFP_L
        S.NFP = NFP_L
        CALL INT_CONV( S.NFP, ENDIAN_P, ENDIAN_I )
      END IF
      IF ( NLP_I .GT. NLP_U ) THEN
        IF ( MSG ) WRITE( *, '(/2A,I7,A/1X,A/)', IOSTAT=IOS_W )
     &     ' *** UDS_WRITE WARNING - ',
     &     'Array upper index truncated to: ', NLP_U,
     &     ' in write to file:', FILE_NAME(:LF)
        NLP_O = NLP_U
        S.NLP = NLP_U
        CALL INT_CONV( S.NLP, ENDIAN_P, ENDIAN_I )
      END IF

      ! Byte reordering
      IF ( ENDIAN_I .NE. ENDIAN_O ) THEN
        CALL INT_CONV( S.ICHAN, ENDIAN_I, ENDIAN_O )
        CALL INT_CONV( S.CURNO, ENDIAN_I, ENDIAN_O )
        CALL INT_CONV( S.SENNUM, ENDIAN_I, ENDIAN_O )
        CALL INT_CONV( S.BANDNO, ENDIAN_I, ENDIAN_O )
        CALL INT_CONV( S.GAGNO, ENDIAN_I, ENDIAN_O )
        CALL INT_CONV( S.NFP, ENDIAN_I, ENDIAN_O )
        CALL INT_CONV( S.NLP, ENDIAN_I, ENDIAN_O )
        CALL INT_CONV( S.ID1, ENDIAN_I, ENDIAN_O )
        CALL INT_CONV( S.ID2, ENDIAN_I, ENDIAN_O )
        CALL INT_CONV( S.ID3, ENDIAN_I, ENDIAN_O )
        CALL INT_CONV( S.ID4, ENDIAN_I, ENDIAN_O )
        CALL INT_CONV( S.ID5, ENDIAN_I, ENDIAN_O )
      END IF

      ! Dimensional system conversion
      IF ( DIM_I .NE. DIM_O ) THEN
        CALL DSU_CONV( S.YTYP, S.YUNITS, DIM_I, DIM_O, DD )
        CALL DSU_CONV( S.XTYP, S.XUNITS, DIM_I, DIM_O, DD )
        IF ( S.CHANFORM .EQ. 'Y' )
     &     CALL DSN_CONV( S.DEL, S.XTYP, U.XUNITS, DIM_I, DIM_O,
     &     NUM_I )
        CALL DSN_CONV( S.INIVEL, VEL_TYP, ' ', DIM_I, DIM_O, NUM_I )
      END IF

      ! Number format conversion
      IF ( NUM_I .NE. NUM_O ) THEN
        CALL FPN_CONV( S.DEL, NUM_I, NUM_O )
        CALL FPN_CONV( S.INIVEL, NUM_I, NUM_O )
        CALL FPN_CONV( S.PREF, NUM_I, NUM_O )
        CALL FPN_CONV( S.FCUT, NUM_I, NUM_O )
        CALL FPN_CONV( S.FCOR, NUM_I, NUM_O )
        CALL FPN_CONV( S.FSTP, NUM_I, NUM_O )
        CALL FPN_CONV( S.SCLFAC, NUM_I, NUM_O )
        CALL FPN_CONV( S.RD1, NUM_I, NUM_O )
        CALL FPN_CONV( S.RD2, NUM_I, NUM_O )
        CALL FPN_CONV( S.RD3, NUM_I, NUM_O )
        CALL FPN_CONV( S.RD4, NUM_I, NUM_O )
        CALL FPN_CONV( S.RD5, NUM_I, NUM_O )
      END IF

      ! Write curve specification record
      OUT_BUFF(:12) = 'CURVE HDR'
      IP = 13
      CALL INT_WRITE( S.ICHAN, OUT_BUFF, IP )
      CALL STR_WRITE( S.CHANFORM, OUT_BUFF, IP )
      CALL INT_WRITE( S.CURNO, OUT_BUFF, IP )
      CALL STR_WRITE( S.CURNAM, OUT_BUFF, IP )
      CALL STR_WRITE( S.SENATT, OUT_BUFF, IP )
      CALL STR_WRITE( S.SENLOC, OUT_BUFF, IP )
      CALL INT_WRITE( S.SENNUM, OUT_BUFF, IP )
      CALL INT_WRITE( S.BANDNO, OUT_BUFF, IP )
      CALL INT_WRITE( S.GAGNO, OUT_BUFF, IP )
      CALL STR_WRITE( S.AXIS, OUT_BUFF, IP )
      CALL STR_WRITE( S.YTYP, OUT_BUFF, IP )
      CALL STR_WRITE( S.YUNITS, OUT_BUFF, IP )
      CALL STR_WRITE( S.XTYP, OUT_BUFF, IP )
      CALL STR_WRITE( S.XUNITS, OUT_BUFF, IP )
      CALL STR_WRITE( S.STATUS, OUT_BUFF, IP )
      CALL STR_WRITE( S.CURTYP, OUT_BUFF, IP )
      CALL STR_WRITE( S.CURDSC, OUT_BUFF, IP )
      CALL INT_WRITE( S.NFP, OUT_BUFF, IP )
      CALL INT_WRITE( S.NLP, OUT_BUFF, IP )
      CALL REAL_WRITE( S.DEL, OUT_BUFF, IP )
      CALL REAL_WRITE( S.INIVEL, OUT_BUFF, IP )
      CALL REAL_WRITE( S.PREF, OUT_BUFF, IP )
      CALL REAL_WRITE( S.FCUT, OUT_BUFF, IP )
      CALL REAL_WRITE( S.FCOR, OUT_BUFF, IP )
      CALL REAL_WRITE( S.FSTP, OUT_BUFF, IP )
      CALL REAL_WRITE( S.SCLFAC, OUT_BUFF, IP )
      CALL INT_WRITE( S.ID1, OUT_BUFF, IP )
      CALL INT_WRITE( S.ID2, OUT_BUFF, IP )
      CALL INT_WRITE( S.ID3, OUT_BUFF, IP )
      CALL INT_WRITE( S.ID4, OUT_BUFF, IP )
      CALL INT_WRITE( S.ID5, OUT_BUFF, IP )
      CALL REAL_WRITE( S.RD1, OUT_BUFF, IP )
      CALL REAL_WRITE( S.RD2, OUT_BUFF, IP )
      CALL REAL_WRITE( S.RD3, OUT_BUFF, IP )
      CALL REAL_WRITE( S.RD4, OUT_BUFF, IP )
      CALL REAL_WRITE( S.RD5, OUT_BUFF, IP )
      CALL STR_WRITE( S.CD1, OUT_BUFF, IP )
      CALL STR_WRITE( S.CD2, OUT_BUFF, IP )
      CALL STR_WRITE( S.CURV_RESV, OUT_BUFF, IP )
      CALL BUF_WRITE( LUN, OUT_BUFF, IOS )
      IF ( IOS .NE. 0 ) GO TO 201


      ! Output curve data records ______________________________________

      ! Compute number of data records
      NDREC = ( NLP_O - NFP_O ) / 125 + 1

      ! Write the Y data records
      REC_HDR = 'Y DATA'
      DO IR = 1, NDREC
        IS = NFP_O + 125*(IR-1)
        IE = MIN( IS+124, NLP_O )
        NOUT = IE - IS + 1
        DO I = 1, NOUT
          W_INT(I) = U.Y_INT(IS-1+I)
        END DO
        DO I = NOUT+1, 125
          W(I) = 0.
        END DO

        ! Dimensional system conversion
        IF ( DIM_I .NE. DIM_O ) THEN
          CALL DSA_CONV( W, NOUT, S.YTYP, U.YUNITS,
     &       DIM_I, DIM_O, NUM_I )
        END IF

        ! Number format conversion
        IF ( NUM_I .NE. NUM_O ) THEN
          CALL FPA_CONV( W, NOUT, NUM_I, NUM_O )
        END IF

        ! Write the data
        CALL BUF_WRITE( LUN, OUT_BUFF, IOS )
        IF ( IOS .NE. 0 ) GO TO 201
      END DO

      ! Write X data if X-Y channel
      IF ( S.CHANFORM .EQ. 'X-Y' ) THEN

        ! Write the X data records
        REC_HDR = 'X DATA'
        DO IR = 1, NDREC
          IS = NFP_O + 125*(IR-1)
          IE = MIN( IS+124, NLP_O )
          NOUT = IE - IS + 1
          DO I = 1, NOUT
            W_INT(I) = U.X_INT(IS-1+I)
          END DO
          DO I = NOUT+1, 125
            W(I) = 0.
          END DO

          ! Dimensional system conversion
          IF ( DIM_I .NE. DIM_O ) THEN
            CALL DSA_CONV( W, NOUT, S.XTYP, U.XUNITS,
     &         DIM_I, DIM_O, NUM_I )
          END IF

          ! Number format conversion
          IF ( NUM_I .NE. NUM_O ) THEN
            CALL FPA_CONV( W, NOUT, NUM_I, NUM_O )
          END IF

          ! Write the data
          CALL BUF_WRITE( LUN, OUT_BUFF, IOS )
          IF ( IOS .NE. 0 ) GO TO 201
        END DO

      END IF


      ! Close UDS file and return
  200 CALL UDS_CLOSE( LUN )
      RETURN


      ! Error handlers _________________________________________________

      ! File write error
  201 IF ( MSG ) WRITE( *, '(/2A/1X,A/)', IOSTAT=IOS_W )
     & ' *** UDS_WRITE ERROR - ',
     & 'File write error on file:', FILE_NAME(:LF)
      IOS = -4
      CALL UDS_CLOSE( LUN )
      RETURN


      END
