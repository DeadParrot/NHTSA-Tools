      PROGRAM UDS92

!***********************************************************************
!* Converts UDS-1991 Files to UDS-1992 Format
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/18
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'
      INCLUDE 'file.fi'


      ! Variables ______________________________________________________

      RECORD /UDS/  U
      RECORD /UDS_CONTROL/  CR, CW
      RECORD /FILE/  HIT, REPORT, OUTPUT_LIST

      LOGICAL ALL_BATCH, NO_BATCH, NO_INCR, ONE_PASS, NEW_HIT, EOF,
     & BATCH

      INTEGER NUM_ARGS, MAX_ARGS, IOS, I

      PARAMETER ( MAX_ARGS = 12 )

      CHARACTER CL_ARG(0:MAX_ARGS)*255, PROG_NAME*8,
     & TEMPLATE_STR*255, DIR_OUT*255,
     & FILE_NAME*255, OUT_FILE*255, DEF_FILE*255,
     & PROMPT*79,
     & FN_R, FN_F


      ! Initializations
      PROG_NAME = 'UDS92'
      CALL FILE_INIT( HIT )
      CALL GET_CL( CL_ARG, NUM_ARGS, MAX_ARGS )
      CALL PROC_CL( CL_ARG, NUM_ARGS, PROG_NAME, TEMPLATE_STR,
     & ALL_BATCH, NO_BATCH, NO_INCR, REPORT, OUTPUT_LIST, DIR_OUT,
     & ONE_PASS, .TRUE., .TRUE. )
      PROMPT = 'UDS file name/template/hitlist?'
      PROMPT(74:) = '[done]'
      CALL UDS_CONTROL_INIT( CR )
      CALL UDS_CONTROL_INIT( CW )

      WRITE( *, '(//20X,A//)' )
     & '**  UDS-1991 to UDS-1992 Conversion  **'

      ! Read the file name/template/hitlist
  100 CALL FILE_PROMPT( TEMPLATE_STR, PROMPT, DIR_OUT,
     & ONE_PASS, HIT, NEW_HIT, FILE_NAME, EOF )
      IF ( ( FILE_NAME .EQ. ' ' ) .OR. ( EOF ) ) GO TO 199 ! Done

      ! Batch setup
      CALL BAT_SET( ALL_BATCH, NO_BATCH, NEW_HIT, HIT, BATCH )
      IF ( ( NEW_HIT ) .AND. ( BATCH ) ) THEN ! Batch prompts

        ! Get conversion profile
        CALL UDS92_PROF( FN_R, FN_F, EOF )
        IF ( EOF ) THEN
          CALL MSG_WRITE( '*** Operation skipped', REPORT )
          CALL FILE_DEL( HIT )
          GO TO 190
        END IF

      END IF

      ! Read UDS file
      CALL REP_COLS( REPORT )
      CALL MSG_WRITE( '  Reading: '//FILE_NAME, REPORT )
      CALL UDS_READ_91( FILE_NAME, U, CR, IOS )
      IF ( IOS .NE. 0 ) THEN ! UDS read error
        CALL MSG_WRITE( '*** ERROR - UDS file read failed', REPORT )
        GO TO 190
      END IF
      CALL REP_ENTRY( U, REPORT )

      IF ( .NOT. BATCH ) THEN ! Interactive prompts

        ! Get conversion profile
        CALL UDS92_PROF( FN_R, FN_F, EOF )
        IF ( EOF ) GO TO 190

      END IF

      ! Prepare output fields
      U.FILEVER = 'UDS-1992'

      ! Set default output file name
      OUT_FILE = FILE_NAME
      CALL FN_EXTRACT( OUT_FILE )
      IF ( FN_R .NE. 'I' ) CALL FN_FLAG( OUT_FILE, FN_F, 2, I )
      CALL ADD_PATH( DIR_OUT, OUT_FILE )
      IF ( ( FN_R .NE. 'I' ) .AND. ( .NOT. NO_INCR ) )
     & CALL FN_INCR( OUT_FILE )
      CALL FN_NAME_EXTRACT( OUT_FILE )
      DEF_FILE = OUT_FILE

      ! Set the output UDS file name
      IF ( .NOT. BATCH ) THEN
  120   WRITE( *, '(/A/3A/'' >> '',$)' )
     &     ' Output UDS file name?',
     &     ' [', DEF_FILE(:L_TRIM(DEF_FILE)), ']'
        READ( *, '(A)', END=190 ) OUT_FILE
        IF ( OUT_FILE .EQ. ' ' ) THEN ! Use default file name
          OUT_FILE = DEF_FILE
        ELSE ! Check file name validity
          CALL ADD_PATH( DIR_OUT, OUT_FILE )
          IF ( .NOT. FN_VALID( OUT_FILE ) ) THEN
            WRITE( *, * ) '*** Invalid file name'
            GO TO 120
          END IF
        END IF
      ELSE ! Batch
        OUT_FILE = DEF_FILE
      END IF

      ! Write the output file
      CALL UDS_PUT( OUT_FILE, U, CW, REPORT, OUTPUT_LIST,
     & DIR_OUT, .TRUE., IOS )
      IF ( IOS .NE. 0 ) GO TO 190

      ! Loop for next file
  190 WRITE( *, '(80(''_''))' )
      GO TO 100

      ! Clean up and stop
  199 CALL CLEAN_UP( HIT, REPORT, OUTPUT_LIST, DIR_OUT )

      END



      SUBROUTINE UDS92_PROF( FN_R, FN_F, EOF )

!***********************************************************************
!* Gets Conversion Profile for UDS92
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/18
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER FN_R ! File naming response flag

      CHARACTER FN_F ! File name flag letter

      LOGICAL EOF ! End-of-file (ctrl-Z) handling flag


      ! File naming
  101 WRITE( *, '(/2A,23X,A/'' >> '',$)' )
     & ' Target file naming?',
     & '   ( I - Input,  F - Flagged )', '[Input]'
      READ( *, '(A)', END=199 ) FN_R
      CALL STR_UP( FN_R )
      IF ( .NOT. ANY_CHARS( FN_R, ' IF' ) ) THEN
        WRITE( *, * ) '*** Invalid response'
        GO TO 101
      END IF
      IF ( FN_R .EQ. 'F' ) THEN ! Get flag letter
        WRITE( *, '(/A,64X,A/'' >> '',$)' )
     &     ' Flag letter?', '[2]'
        READ( *, '(A)', END=101 ) FN_F
        IF ( FN_F .EQ. ' ' ) FN_F = '2'
      ELSE ! Use input file name
        FN_R = 'I'
      END IF

      EOF = .FALSE.
      RETURN

      ! Ctrl-Z handler
  199 EOF = .TRUE.
      RETURN

      END



      SUBROUTINE UDS_READ_91( FILE_NAME, U, C, IOS )

!***********************************************************************
!* UDS-1991 File STRUCTURE Read Routine
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/18
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'
      INCLUDE 'platform.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) FILE_NAME ! UDS file name

      RECORD /UDS/  U ! UDS STRUCTURE

      RECORD /UDS_CONTROL/  C ! UDS i/o control STRUCTURE

      INTEGER IOS ! Status flag


      ! Variables ______________________________________________________

      LOGICAL MSG

      INTEGER IOS_W, LF, LUN, I, IR, IS, IE, NFP_I, OCCWT_I

      REAL DEL_P, RD, CMPWGT_R, CMPWGT2_R

      DOUBLE PRECISION DD

      CHARACTER NUM_P*5, NUM_O*5, DIM_I*3, DIM_O*3, YUN_I*20,
     & VEL_TYP*20, WT_TYP*20

      PARAMETER ( VEL_TYP = 'VELOCITY', WT_TYP = 'WEIGHT' )

      PARAMETER ( NUM_P = NUMFORM_P )


      ! Functions ______________________________________________________

      CHARACTER LJUST*12

      EXTERNAL LJUST


      ! Initializations
      IOS = 0
      LF = L_TRIM( FILE_NAME )
      MSG = ( .NOT. C.NO_MSG )


      ! Check array bounds
      IF ( NFP_L .GT. NLP_U ) THEN
        IF ( MSG ) WRITE( *, '(/2A/)' )
     &     ' *** UDS_READ ERROR - ',
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


      ! Open file
      CALL OPEN_US( LUN, FILE_NAME, 'R', IOS )
      IF ( IOS .NE. 0 ) THEN
        IF ( MSG ) WRITE( *, '(/2A/1X,A/)', IOSTAT=IOS_W )
     &     ' *** UDS_READ ERROR - ',
     &     'File open failure on file:', FILE_NAME(:LF)
        GO TO 200
      END IF


      ! Initialize UDS fields
      CALL UDS_INIT( U )


      ! Read header record
      READ( LUN, ERR=201, END=201 )
     & U.TSTSRC, U.TSTNAM(9:12), U.TSTPRF, U.TSTREF,
     & U.TSTCFN, U.CLSSPD,
     & U.CMPNO, U.MAKE, U.MODEL(:20), U.YEAR, U.BODY(:20),
     & U.ENGINE, U.CMPWGT,
     & U.OCCTYP, U.OCCAGE, U.OCCSEX, OCCWT_I, U.DUMSIZ,
     & U.RESTR1, U.RESTR2,
     & U.HIC, U.T1, U.T2, U.CLIP3M, U.CSI, U.AIS,
     & U.CURNAM(10:12), U.YTYP, U.YUNITS(:15), U.AXIS,
     & U.SENLOC(2:2), U.SENATT, U.STATUS(:15), U.CURTYP(:11),
     & U.CURDSC, U.NFP, U.NLP, U.DEL, U.INIVEL, U.PREF,
     & U.FCUT, U.FCOR, U.FSTP, RD, RD, RD,
     & U.ID1, U.ID2, U.ID3, U.ID4, U.ID5,
     & U.RD1, U.RD2, U.RD3, U.RD4, U.RD5, U.CD1, U.CD2
      IF ( ABS( U.CLSSPD ) .EQ. 999.9 ) U.CLSSPD = 0.
      IF ( ABS( U.YEAR ) .EQ. 99 ) U.YEAR = 0
      IF ( ABS( U.CMPWGT ) .EQ. 99999. ) U.CMPWGT = 0.
      IF ( ABS( U.OCCAGE ) .EQ. 99 ) U.OCCAGE = 0
      IF ( ABS( OCCWT_I ) .EQ. 999 ) OCCWT_I = 0
      IF ( ABS( U.HIC ) .EQ. 9999. ) U.HIC = 0.
      IF ( ABS( U.T1 ) .EQ. 999.999 ) U.T1 = 0.
      IF ( ABS( U.T2 ) .EQ. 999.999 ) U.T2 = 0.
      IF ( ABS( U.CLIP3M ) .EQ. 999.9 ) U.CLIP3M = 0.
      IF ( ABS( U.CSI ) .EQ. 9999. ) U.CSI = 0.
      IF ( ABS( U.INIVEL ) .EQ. 999.9 ) U.INIVEL = 0.
      IF ( ABS( U.PREF ) .EQ. 99999. ) U.PREF = 0.
      U.OCCWT = OCCWT_I
      DEL_P = U.DEL ! Save DEL in local number format for later use

      ! Assign UDS-1992 specific fields
      U.FILEVER = 'UDS-1991'
      U.NCHAN = 1
      U.ICHAN = 1
      U.FILEFORM = 'Y'
      U.CHANFORM = 'Y'
      U.NUMFORM = NUM_P
      IF ( U.ID5 .EQ. 1 ) THEN
        U.DIMSYS = 'MET'
        U.ID5 = 0
      ELSE
        U.DIMSYS = 'ENG'
      END IF
      READ( U.TSTNAM, '(BN,I12)', IOSTAT=IOS_W ) U.TSTNO
      IF ( IOS_W .NE. 0 ) U.TSTNAM = LJUST( U.TSTNAM )
      READ( U.CURNAM, '(BN,I12)', IOSTAT=IOS_W ) U.CURNO
      IF ( IOS_W .NE. 0 ) U.CURNAM = LJUST( U.CURNAM )
      IF ( U.CURTYP .EQ. 'TIME SERIES' ) THEN
        U.XTYP = 'TIME'
        U.XUNITS = 'SECONDS'
      ELSE IF ( U.CURTYP(:4) .EQ. 'DFT ' ) THEN
        U.XTYP = 'FREQUENCY'
        U.XUNITS = 'HZ'
      END IF

      ! Check file header fields
      IF ( ( .NOT. BLANK( C.FILEVER ) ) .AND.
     & ( U.FILEVER .NE. C.FILEVER ) ) THEN
        IF ( MSG ) WRITE( *, '(/6A/1X,A/)' )
     &     ' *** UDS_READ ERROR - ',
     &     'File version is ', U.FILEVER(:L_TRIM(U.FILEVER)),
     &     ' not ', C.FILEVER(:L_TRIM(C.FILEVER)), ' in file:',
     &     FILE_NAME(:LF)
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
      IF ( ( .NOT. BLANK( C.CHANFORM ) ) .AND.
     & ( U.CHANFORM .NE. C.CHANFORM ) ) THEN
        IF ( MSG ) WRITE( *, '(/6A/1X,A/)', IOSTAT=IOS_W )
     &     ' *** UDS_READ ERROR - ',
     &     'Channel format is ', U.CHANFORM(:L_TRIM(U.CHANFORM)),
     &     ' not ', C.CHANFORM(:L_TRIM(C.CHANFORM)), ' in file:',
     &     FILE_NAME(:LF)
        IOS = -5
        GO TO 200
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

      ! Dimensional system conversion
      CALL DSU_CONV( U.YTYP, U.YUNITS, DIM_I, DIM_I, DD ) ! Update units
      YUN_I = U.YUNITS
      IF ( DIM_I .NE. DIM_O ) THEN
        CALL DSU_CONV( U.YTYP, U.YUNITS, DIM_I, DIM_O, DD )
        CALL DSN_CONV( U.CLSSPD, VEL_TYP, ' ', DIM_I, DIM_O, NUM_P )
        CALL DSN_CONV( U.CMPWGT, WT_TYP, ' ', DIM_I, DIM_O, NUM_P )
        CALL DSN_CONV( U.OCCWT, WT_TYP, ' ', DIM_I, DIM_O, NUM_P )
        CALL DSN_CONV( U.INIVEL, VEL_TYP, ' ', DIM_I, DIM_O, NUM_P )
      END IF

      ! Number format conversion
      IF ( NUM_P .NE. NUM_O ) THEN
        CALL FPN_CONV( U.CLSSPD, NUM_P, NUM_O )
        CALL FPN_CONV( U.CMPWGT, NUM_P, NUM_O )
        CALL FPN_CONV( U.OCCWT, NUM_P, NUM_O )
        CALL FPN_CONV( U.HIC, NUM_P, NUM_O )
        CALL FPN_CONV( U.T1, NUM_P, NUM_O )
        CALL FPN_CONV( U.T2, NUM_P, NUM_O )
        CALL FPN_CONV( U.CLIP3M, NUM_P, NUM_O )
        CALL FPN_CONV( U.CSI, NUM_P, NUM_O )
        CALL FPN_CONV( U.DEL, NUM_P, NUM_O )
        CALL FPN_CONV( U.INIVEL, NUM_P, NUM_O )
        CALL FPN_CONV( U.PREF, NUM_P, NUM_O )
        CALL FPN_CONV( U.FCUT, NUM_P, NUM_O )
        CALL FPN_CONV( U.FCOR, NUM_P, NUM_O )
        CALL FPN_CONV( U.FSTP, NUM_P, NUM_O )
        CALL FPN_CONV( U.RD1, NUM_P, NUM_O )
        CALL FPN_CONV( U.RD2, NUM_P, NUM_O )
        CALL FPN_CONV( U.RD3, NUM_P, NUM_O )
        CALL FPN_CONV( U.RD4, NUM_P, NUM_O )
        CALL FPN_CONV( U.RD5, NUM_P, NUM_O )
      END IF
      U.SCLFAC = U.RD1


      ! Read component record
      IF ( U.TSTSRC .EQ. 'COMPONENT DB' ) THEN
        READ( LUN, ERR=201, END=201 )
     &     U.IMPANG, U.SENNUM, U.TITLE,
     &     U.CMPDSC, U.CMPSPD, U.CMPTYP, CMPWGT_R, U.BODY2(:20),
     &     U.CMPNO2, U.CMPDSC2, U.CMPSPD2, U.CMPTYP2, CMPWGT2_R,
     &     U.DUMSIZ2, U.ENGINE2, U.MAKE2, U.MODEL2(:20), U.OCCAGE2,
     &     U.OCCSEX2, U.OCCTYP2, U.OCCWT2, U.RESTR12, U.RESTR22,
     &     U.CMPWGT2, U.YEAR2
        IF ( ABS( U.IMPANG ) .EQ. 999 ) U.IMPANG = 0
        IF ( ABS( U.CMPSPD ) .EQ. 999.9 ) U.CMPSPD = 0.
        IF ( ABS( CMPWGT_R ) .EQ. 9999.9 ) CMPWGT_R = 0.
        IF ( ABS( U.CMPSPD2 ) .EQ. 999.9 ) U.CMPSPD2 = 0.
        IF ( ABS( CMPWGT2_R ) .EQ. 9999.9 ) CMPWGT2_R = 0.
        IF ( ABS( U.OCCAGE2 ) .EQ. 99 ) U.OCCAGE2 = 0
        IF ( ABS( U.OCCWT2 ) .EQ. 999. ) U.OCCWT2 = 0.
        IF ( ABS( U.CMPWGT2 ) .EQ. 99999. ) U.CMPWGT2 = 0.
        IF ( ABS( U.YEAR2 ) .EQ. 99 ) U.YEAR2 = 0

        ! Dimensional system conversion
        IF ( DIM_I .NE. DIM_O ) THEN
          CALL DSN_CONV( U.CMPSPD, VEL_TYP, ' ', DIM_I, DIM_O,
     &       NUM_P )
          CALL DSN_CONV( CMPWGT_R, WT_TYP, ' ', DIM_I, DIM_O,
     &       NUM_P )
          CALL DSN_CONV( U.CMPSPD2, VEL_TYP, ' ', DIM_I, DIM_O,
     &       NUM_P )
          CALL DSN_CONV( CMPWGT2_R, WT_TYP, ' ', DIM_I, DIM_O,
     &       NUM_P )
          CALL DSN_CONV( U.OCCWT2, WT_TYP, ' ', DIM_I, DIM_O,
     &       NUM_P )
          CALL DSN_CONV( U.CMPWGT2, WT_TYP, ' ', DIM_I, DIM_O,
     &       NUM_P )
        END IF

        ! Number format conversion
        IF ( NUM_P .NE. NUM_O ) THEN
          CALL FPN_CONV( U.CMPSPD, NUM_P, NUM_O )
          CALL FPN_CONV( CMPWGT_R, NUM_P, NUM_O )
          CALL FPN_CONV( U.CMPSPD2, NUM_P, NUM_O )
          CALL FPN_CONV( CMPWGT2_R, NUM_P, NUM_O )
          CALL FPN_CONV( U.OCCWT2, NUM_P, NUM_O )
          CALL FPN_CONV( U.CMPWGT2, NUM_P, NUM_O )
        END IF

        IF ( ( U.CMPWGT .EQ. 0. ) .AND. ( CMPWGT_R .NE. 0. ) )
     &     U.CMPWGT = CMPWGT_R
        IF ( ( U.CMPWGT2 .EQ. 0. ) .AND. ( CMPWGT2_R .NE. 0. ) )
     &     U.CMPWGT2 = CMPWGT2_R
      END IF


      ! Check array indices
      IF ( U.NFP .GT. U.NLP ) THEN
        IF ( MSG ) WRITE( *, '(/2A/1X,A/)', IOSTAT=IOS_W )
     &     ' *** UDS_READ ERROR - ',
     &     'Crossed array indices: NFP > NLP in file:',
     &     FILE_NAME(:LF)
        IOS = -5
        GO TO 200
      ELSE IF ( U.NFP .EQ. U.NLP ) THEN
        IF ( MSG ) WRITE( *, '(/2A,I7,A/1X,A/)', IOSTAT=IOS_W )
     &     ' *** UDS_READ WARNING - ',
     &     'Equal array indices: NFP=NLP=', U.NLP, ' in file:',
     &     FILE_NAME(:LF)
      END IF

      ! Read curve data
      IF ( .NOT. C.SPECS_ONLY ) THEN

        ! Check array indices against bounds
        IF ( ( U.NLP .LT. NFP_L ) .OR. ( U.NFP .GT. NLP_U ) ) THEN
          IF ( MSG ) WRITE( *, '(/2A/1X,A/)', IOSTAT=IOS_W )
     &       ' *** UDS_READ ERROR - ',
     &       'Array indices outside valid range in file:',
     &       FILE_NAME(:LF)
          IOS = -5
          GO TO 200
        END IF
        NFP_I = U.NFP
        IF ( U.NFP .LT. NFP_L ) THEN
          IF ( MSG ) WRITE( *, '(/2A,I7,A/1X,A/)', IOSTAT=IOS_W )
     &       ' *** UDS_READ WARNING - ',
     &       'Array lower index truncated to: ', NFP_L,
     &       ' in read from file:', FILE_NAME(:LF)
          U.NFP = NFP_L
        END IF
        IF ( U.NLP .GT. NLP_U ) THEN
          IF ( MSG ) WRITE( *, '(/2A,I7,A/1X,A/)', IOSTAT=IOS_W )
     &       ' *** UDS_READ WARNING - ',
     &       'Array upper index truncated to: ', NLP_U,
     &       ' in read from file:', FILE_NAME(:LF)
          U.NLP = NLP_U
        END IF

        ! Assign X values
        IF ( C.FILL_X ) THEN

          ! Set even X values
          DO I = U.NFP, U.NLP
            U.X(I) = I * DEL_P
          END DO

          ! Number format conversion
          IF ( NUM_P .NE. NUM_O ) THEN
            CALL FPA_CONV( U.X_INT(U.NFP), U.NLP-U.NFP+1,
     &         NUM_P, NUM_O )
          END IF

        END IF

        ! Read the Y data records
        DO IR = 1, (U.NLP-NFP_I)/1000+1
          IS = NFP_I + 1000*(IR-1)
          IE = MIN( IS+999, U.NLP )
          READ( LUN, ERR=201, END=201 )
     &       ( RD, I = IS, MIN(IE,U.NFP-1) ),
     &       ( U.Y_INT(I), I = MAX(IS,U.NFP), MIN(IE,U.NLP) )
        END DO

        ! Dimensional system conversion/update
        CALL DSA_CONV( U.Y(U.NFP), U.NLP-U.NFP+1,
     &     U.YTYP, YUN_I, DIM_I, DIM_O, NUM_P )

        ! Number format conversion
        IF ( NUM_P .NE. NUM_O ) THEN
          CALL FPA_CONV( U.Y_INT(U.NFP), U.NLP-U.NFP+1,
     &       NUM_P, NUM_O )
        END IF

      END IF


      ! Close UDS file and return
  200 CLOSE( LUN )
      RETURN


      ! Error handlers _________________________________________________

      ! File read error
  201 IF ( MSG ) WRITE( *, '(/2A/1X,A/)', IOSTAT=IOS_W )
     & ' *** UDS_READ ERROR - ',
     & 'File read error on file:', FILE_NAME(:LF)
      IOS = -4
      CLOSE( LUN )
      RETURN


      END
