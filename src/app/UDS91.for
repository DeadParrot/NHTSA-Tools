      PROGRAM UDS91

!***********************************************************************
!* Converts UDS-1992 Files to UDS-1991 Format
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
      PROG_NAME = 'UDS91'
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
     & '**  UDS-1992 to UDS-1991 Conversion  **'

      ! Read the file name/template/hitlist
  100 CALL FILE_PROMPT( TEMPLATE_STR, PROMPT, DIR_OUT,
     & ONE_PASS, HIT, NEW_HIT, FILE_NAME, EOF )
      IF ( ( FILE_NAME .EQ. ' ' ) .OR. ( EOF ) ) GO TO 199 ! Done

      ! Batch setup
      CALL BAT_SET( ALL_BATCH, NO_BATCH, NEW_HIT, HIT, BATCH )
      IF ( ( NEW_HIT ) .AND. ( BATCH ) ) THEN ! Batch prompts

        ! Get conversion profile
        CALL UDS91_PROF( FN_R, FN_F, EOF )
        IF ( EOF ) THEN
          CALL MSG_WRITE( '*** Operation skipped', REPORT )
          CALL FILE_DEL( HIT )
          GO TO 190
        END IF

      END IF

      ! Read UDS file
      CALL REP_COLS( REPORT )
      CALL UDS_GET( FILE_NAME, U, CR, REPORT, IOS )
      IF ( IOS .NE. 0 ) GO TO 190

      IF ( .NOT. BATCH ) THEN ! Interactive prompts

        ! Get conversion profile
        CALL UDS91_PROF( FN_R, FN_F, EOF )
        IF ( EOF ) GO TO 190

      END IF

      ! Prepare output fields
      U.FILEVER = 'UDS-1991'

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
      CALL ADD_PATH( DIR_OUT, OUT_FILE )
      CALL UDS_WRITE_91( OUT_FILE, U, CW, IOS )
      INQUIRE( FILE=OUT_FILE, NAME=OUT_FILE )
      CALL MSG_WRITE( '  Writing: '//OUT_FILE, REPORT )
      IF ( IOS .NE. 0 ) THEN
        CALL MSG_WRITE( '*** UDS file write failed', REPORT )
        GO TO 190
      END IF
      CALL REP_ENTRY( U, REPORT )
      CALL OUT_ENTRY( OUT_FILE, OUTPUT_LIST )

      ! Loop for next file
  190 WRITE( *, '(80(''_''))' )
      GO TO 100

      ! Clean up and stop
  199 CALL CLEAN_UP( HIT, REPORT, OUTPUT_LIST, DIR_OUT )

      END



      SUBROUTINE UDS91_PROF( FN_R, FN_F, EOF )

!***********************************************************************
!* Gets Conversion Profile for UDS91
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
     &     ' Flag letter?', '[1]'
        READ( *, '(A)', END=101 ) FN_F
        IF ( FN_F .EQ. ' ' ) FN_F = '1'
      ELSE ! Use input file name
        FN_R = 'I'
      END IF

      EOF = .FALSE.
      RETURN

      ! Ctrl-Z handler
  199 EOF = .TRUE.
      RETURN

      END



      SUBROUTINE UDS_WRITE_91( FILE_NAME, U, C, IOS )

!***********************************************************************
!* UDS-1991 File STRUCTURE Write Routine
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

      RECORD /UDS_SPEC/  S ! Output specifications

      LOGICAL MSG

      INTEGER IOS_W, LF, LUN, I, IR, IS, IE, NOUT, ZO(1000)

      REAL ZERO

      PARAMETER ( ZERO = 0. )

      DOUBLE PRECISION DD

      CHARACTER NUM_P*5, NUM_I*5, NUM_O*5, DIM_I*3, DIM_O*3, TEMP*4,
     & VEL_TYP*20, WT_TYP*20

      PARAMETER ( VEL_TYP = 'VELOCITY', WT_TYP = 'WEIGHT' )

      PARAMETER ( NUM_P = NUMFORM_P )


      ! Functions ______________________________________________________

      CHARACTER LJUST*4

      EXTERNAL LJUST


      ! Initializations
      IOS = 0
      LF = L_TRIM( FILE_NAME )
      MSG = ( .NOT. C.NO_MSG )


      ! Check array bounds
      IF ( NFP_L .GT. NLP_U ) THEN
        IF ( MSG ) WRITE( *, '(/2A/)' )
     &     ' *** UDS_WRITE ERROR - ',
     &     'Crossed array bounds: NFP_L > NLP_U'
        IOS = -2
        RETURN
      ELSE IF ( NFP_L .EQ. NLP_U ) THEN
        IF ( MSG ) WRITE( *, '(/2A,I8/)', IOSTAT=IOS_W )
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

      ! Check UDS fields
      IF ( ( .NOT. BLANK( U.CHANFORM ) ) .AND.
     & ( U.CHANFORM .NE. 'Y' ) ) THEN
        IF ( MSG ) WRITE( *, '(/4A/1X,A/)', IOSTAT=IOS )
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
     & ( NUM_I .NE. 'IEEE' ) .AND.
     & ( NUM_I .NE. 'SUN' ) ) THEN
        IF ( MSG ) WRITE( *, '(/4A/1X,A/)', IOSTAT=IOS )
     &     ' *** UDS_WRITE ERROR - ',
     &     'Unsupported real number format: ', NUM_I,
     &     ' in write to file:', FILE_NAME(:LF)
        IOS = -5
        RETURN
      END IF
      IF ( ( U.DIMSYS .NE. 'MET' ) .AND.
     & ( U.DIMSYS .NE. 'SI' ) .AND.
     & ( U.DIMSYS .NE. 'ENG' ) ) THEN
        IF ( MSG ) WRITE( *, '(/4A/1X,A/)', IOSTAT=IOS )
     &     ' *** UDS_WRITE ERROR - ',
     &     'Unsupported dimensional system: ', U.DIMSYS,
     &     ' in write to file:', FILE_NAME(:LF)
        IOS = -5
        RETURN
      END IF
      IF ( U.NCHAN .GT. 1 ) THEN
        IF ( MSG ) WRITE( *, '(/2A/1X,A/)', IOSTAT=IOS )
     &     ' *** UDS_WRITE WARNING - ',
     &     'Only one channel written to file:', FILE_NAME(:LF)
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
      IF ( NUM_O .NE. NUM_P ) THEN
        IF ( MSG ) WRITE( *, '(/2A/1X,A/)', IOSTAT=IOS )
     &     ' *** UDS_WRITE WARNING - ',
     &     'Local real number format used in write to UDS-1991 file:',
     &     FILE_NAME(:LF)
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
      IF ( U.NFP .GT. U.NLP ) THEN
        IF ( MSG ) WRITE( *, '(/2A/1X,A/)', IOSTAT=IOS )
     &     ' *** UDS_WRITE ERROR - ',
     &     'Crossed array indices: NFP > NLP in write to file:',
     &     FILE_NAME(:LF)
        IOS = -5
        RETURN
      ELSE IF ( U.NFP .EQ. U.NLP ) THEN
        IF ( MSG ) WRITE( *, '(/2A,I7,A/1X,A/)', IOSTAT=IOS_W )
     &     ' *** UDS_WRITE WARNING - ',
     &     'Equal array indices: NFP=NLP=', U.NLP,
     &     ' in write to file:', FILE_NAME(:LF)
      END IF
      IF ( ( U.NLP .LT. NFP_L ) .OR. ( U.NFP .GT. NLP_U ) ) THEN
        IF ( MSG ) WRITE( *, '(/2A/1X,A/)', IOSTAT=IOS_W )
     &     ' *** UDS_WRITE ERROR - ',
     &     'Array indices outside valid range in write to file:',
     &     FILE_NAME(:LF)
        IOS = -5
        RETURN
      END IF


      ! Open file
      IF ( C.FN_INCR ) CALL FN_INCR( FILE_NAME )
      CALL OPEN_US( LUN, FILE_NAME, 'W', IOS )
      IF ( IOS .NE. 0 ) CALL OPEN_US( LUN, FILE_NAME, ' ', IOS )
      IF ( IOS .NE. 0 ) THEN
        IF ( MSG ) WRITE( *, '(/2A/1X,A/)', IOSTAT=IOS_W )
     &     ' *** UDS_WRITE ERROR - ',
     &     'File open failure on file:', FILE_NAME(:LF)
        GO TO 200
      END IF


      ! Load output specifications
      CALL UDS_SPEC_COPY( S, U )

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

      ! Dimensional system conversion
      IF ( DIM_I .NE. DIM_O ) THEN
        CALL DSN_CONV( S.CLSSPD, VEL_TYP, ' ', DIM_I, DIM_O, NUM_O )
        CALL DSN_CONV( S.CMPWGT, WT_TYP, ' ', DIM_I, DIM_O, NUM_O )
        CALL DSN_CONV( S.CMPSPD, VEL_TYP, ' ', DIM_I, DIM_O, NUM_O )
        CALL DSN_CONV( S.OCCWT, WT_TYP, ' ', DIM_I, DIM_O, NUM_O )
      END IF


      ! Number format conversion
      IF ( NUM_I .NE. NUM_O ) THEN
        CALL FPN_CONV( S.CMPWGT2, NUM_I, NUM_O )
        CALL FPN_CONV( S.CMPSPD2, NUM_I, NUM_O )
        CALL FPN_CONV( S.OCCWT2, NUM_I, NUM_O )
      END IF

      ! Dimensional system conversion
      IF ( DIM_I .NE. DIM_O ) THEN
        CALL DSN_CONV( S.CMPWGT2, WT_TYP, ' ', DIM_I, DIM_O, NUM_O )
        CALL DSN_CONV( S.CMPSPD2, VEL_TYP, ' ', DIM_I, DIM_O, NUM_O )
        CALL DSN_CONV( S.OCCWT2, WT_TYP, ' ', DIM_I, DIM_O, NUM_O )
      END IF


      ! Check output curve specification fields
      IF ( S.NFP .LT. NFP_L ) THEN
        IF ( MSG ) WRITE( *, '(/2A,I7,A/1X,A/)', IOSTAT=IOS_W )
     &     ' *** UDS_WRITE WARNING - ',
     &     'Array lower index truncated to: ', NFP_L,
     &     ' in write to file:', FILE_NAME(:LF)
        S.NFP = NFP_L
      END IF
      IF ( S.NLP .GT. NLP_U ) THEN
        IF ( MSG ) WRITE( *, '(/2A,I7,A/1X,A/)', IOSTAT=IOS_W )
     &     ' *** UDS_WRITE WARNING - ',
     &     'Array upper index truncated to: ', NLP_U,
     &     ' in write to file:', FILE_NAME(:LF)
        S.NLP = NLP_U
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

      ! Dimensional system conversion
      IF ( DIM_I .NE. DIM_O ) THEN
        CALL DSU_CONV( S.YTYP, S.YUNITS, DIM_I, DIM_O, DD )
        CALL DSU_CONV( S.XTYP, S.XUNITS, DIM_I, DIM_O, DD )
        CALL DSN_CONV( S.DEL, S.XTYP, U.XUNITS, DIM_I, DIM_O, NUM_O )
        CALL DSN_CONV( S.INIVEL, VEL_TYP, ' ', DIM_I, DIM_O, NUM_O )
      END IF

      ! Set UDS-1991 specific fields
      IF ( S.TSTNAM(:8) .EQ. ' ' ) THEN
        TEMP = S.TSTNAM(9:12)
      ELSE
        TEMP = LJUST( S.TSTNAM )
      END IF
      S.TSTNAM = TEMP
      IF ( S.CURNAM(:9) .EQ. ' ' ) THEN
        TEMP = S.CURNAM(10:12)
      ELSE
        TEMP = LJUST( S.CURNAM )
      END IF
      S.CURNAM = TEMP
      IF ( S.CURTYP .EQ. ' ' ) THEN
        IF ( S.XTYP .EQ. 'TIME' ) THEN
          S.CURTYP = 'TIME SERIES'
        ELSE IF ( S.XTYP .EQ. 'FREQUENCY' ) THEN
          S.CURTYP = 'DFT'
        ELSE
          S.CURTYP = ' '
        END IF
      END IF
      IF ( DIM_O .EQ. 'MET' ) THEN
        S.ID5 = 1
      ELSE IF ( ( DIM_O .EQ. 'ENG' ) .AND. ( S.ID5 .EQ. 1 ) ) THEN
        S.ID5 = 0
      END IF
      IF ( ( S.RD1 .EQ. 0. ) .AND. ( S.SCLFAC .NE. 0. ) ) THEN
        S.RD1 = S.SCLFAC
      END IF


      ! Write header record
      WRITE( LUN, ERR=201 )
     & S.TSTSRC, S.TSTNAM(:4), S.TSTPRF, S.TSTREF, S.TSTCFN,
     & S.CLSSPD,
     & S.VEHNO, S.MAKE, S.MODEL(:20), S.YEAR, S.BODY(:20), S.ENGINE,
     & S.VEHTWT,
     & S.OCCTYP, S.OCCAGE, S.OCCSEX, NINT(S.OCCWT), S.DUMSIZ,
     & S.RESTR1, S.RESTR2, S.HIC, S.T1, S.T2, S.CLIP3M, S.CSI,
     & S.AIS,
     & S.CURNAM(:3), S.YTYP, S.YUNITS(:15), S.AXIS, S.SENLOC(2:2),
     & S.SENATT, S.STATUS(:15), S.CURTYP(:11), S.CURDSC,
     & S.NFP, S.NLP, S.DEL, S.INIVEL,
     & S.PREF, S.FCUT, S.FCOR, S.FSTP, ZERO, ZERO, ZERO,
     & S.ID1, S.ID2, S.ID3, S.ID4, S.ID5,
     & S.RD1, S.RD2, S.RD3, S.RD4, S.RD5, S.CD1, S.CD2


      ! Write component record
      IF ( S.TSTSRC .EQ. 'COMPONENT DB' ) THEN
        WRITE( LUN, ERR=201 )
     &     S.IMPANG, S.SENNUM, S.TITLE, S.CMPDSC,
     &     S.CMPSPD, S.CMPTYP, S.CMPWGT, S.BODY2(:20),
     &     S.CMPNO2, S.CMPDSC2, S.CMPSPD2, S.CMPTYP2,
     &     S.CMPWGT2, S.DUMSIZ2, S.ENGINE2, S.MAKE2,
     &     S.MODEL2(:20), S.OCCAGE2, S.OCCSEX2, S.OCCTYP2,
     &     S.OCCWT2, S.RESTR12, S.RESTR22,
     &     S.VEHTWT2, S.YEAR2
      END IF


      ! Write the Y data records
      DO IR = 1, (S.NLP-S.NFP)/1000+1
        IS = S.NFP + 1000*(IR-1)
        IE = MIN( IS+999, S.NLP )
        NOUT = IE - IS + 1
        DO I = 1, NOUT
          ZO(I) = U.Y_INT(IS-1+I)
        END DO

        ! Number format conversion
        IF ( NUM_I .NE. NUM_O ) THEN
          CALL FPA_CONV( ZO, NOUT, NUM_I, NUM_O )
        END IF

        ! Dimensional system conversion
        IF ( DIM_I .NE. DIM_O ) THEN
          CALL DSA_CONV( ZO, NOUT, S.YTYP, U.YUNITS, DIM_I, DIM_O,
     &       NUM_O )
        END IF

        ! Write the data
        WRITE( LUN, ERR=201 ) ( ZO(I), I = 1, NOUT )
      END DO


      ! Close UDS file and return
  200 CLOSE( LUN )
      RETURN


      ! Error handlers _________________________________________________

      ! File write error
  201 IF ( MSG ) WRITE( *, '(/2A/1X,A/)', IOSTAT=IOS_W )
     & ' *** UDS_WRITE ERROR - ',
     & 'File write error on file:', FILE_NAME(:LF)
      IOS = -4
      CLOSE( LUN )
      RETURN

      END
