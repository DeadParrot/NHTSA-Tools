      PROGRAM LJPLOT

!***********************************************************************
!* LaserJet Plots of UDS and ASCII Files
!*
!* Language: Fortran
!*
!* Platform: Windows
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2004/10/25
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'
      INCLUDE 'file.fi'


      ! Variables ______________________________________________________

      RECORD /UDS/  U
      RECORD /FILE/  HIT, REPORT, OUTPUT_LIST

      LOGICAL ALL_BATCH, NO_BATCH, NO_INCR, ONE_PASS, NEW_HIT, EOF,
     & DONE, SYM_ON, SYM_PUT, MAN_FEED, CL_TEMP,
     & XUL, YUL, XTI, XUI, YTI, YUI

      INTEGER NUM_ARGS, MAX_ARGS, IOS, I,
     & NFIL_U, NFIL, NPER_CL, DPI, THICK_FAC,
     & NPER, LSYM(20), PLOT_SIZE, ROW_SIZE, PRN_UNIT

      PARAMETER ( MAX_ARGS = 18, NFIL_U = 20 )

      REAL XL, XLM, XU, YL, YLM, YU

      DOUBLE PRECISION THICK, XLP, XUP, XPD, YLP, YUP, YPD

      CHARACTER CL_ARG(0:MAX_ARGS)*255, PROG_NAME*8,
     & TEMPLATE_STR*255, DIR_OUT*255,
     & FILE_NAME*255,
     & PROMPT_1*79, PROMPT_2*79,
     & FNAME(NFIL_U)*255, FTYP(NFIL_U)*5,
     & FINFO(NFIL_U)*100, DIMSYS_P*3, PLOT_TITLE*80,
     & XTYPE*20, XUNIT*20, XTRAN*9,
     & YTYPE*20, YUNIT*20, YTRAN*9,
     & PLOT_DEV*255,
     & SYM(20)*120, PL(375,3000)


      ! Initializations
      PROG_NAME = 'LJPlot'
      CALL FILE_INIT( HIT )
      CALL GET_CL( CL_ARG, NUM_ARGS, MAX_ARGS )
      CALL LJPLOT_CL( CL_ARG, NUM_ARGS, PROG_NAME,
     & NPER_CL, XTRAN, YTRAN,
     & DPI, THICK_FAC, SYM_ON, MAN_FEED, PLOT_DEV )
      CALL PROC_CL( CL_ARG, NUM_ARGS, PROG_NAME, TEMPLATE_STR,
     & ALL_BATCH, NO_BATCH, NO_INCR, REPORT, OUTPUT_LIST, DIR_OUT,
     & ONE_PASS, .TRUE., .FALSE. )
      PROMPT_1 = 'First file name/template/hitlist?'
      PROMPT_1(74:) = '[done]'
      PROMPT_2 = 'Next file name/template/hitlist?'
      PROMPT_2(71:) = '[no more]'
      IF ( NPER_CL .EQ. 0 ) THEN
        NPER = NFIL_U
      ELSE
        NPER = NPER_CL
      END IF
      CL_TEMP = ( .NOT. BLANK( TEMPLATE_STR ) )

      WRITE( *, '(//20X,A//)' )
     & '**  LaserJet Plot UDS & ASCII Files  **'

      ! Set up symbols and plot size constants
      IF ( SYM_ON ) CALL LJ_SYM_SET( SYM, LSYM )
      PLOT_SIZE = 5 * DPI
      ROW_SIZE = ( PLOT_SIZE + 7 ) / 8
      IF ( DPI .EQ. 300 ) THICK_FAC = MAX( THICK_FAC, 1 )
      THICK = ( DPI / 300.D0 ) * THICK_FAC

      ! Read the first file name/template/hitlist
  100 CALL FILE_PROMPT( TEMPLATE_STR, PROMPT_1, DIR_OUT,
     & ONE_PASS, HIT, NEW_HIT, FILE_NAME, EOF )
      IF ( ( FILE_NAME .EQ. ' ' ) .OR. ( EOF ) ) GO TO 199 ! Done

      ! Read/process first file
      CALL REP_COLS( REPORT )
      NFIL = 1
      FNAME(NFIL) = FILE_NAME
      CALL PLOT_FILE_SETUP( NFIL, FNAME(NFIL),
     & FTYP(NFIL), FINFO(NFIL), DIMSYS_P,
     & XTYPE, XTI, XUNIT, XUI, XL, XLM, XU,
     & YTYPE, YTI, YUNIT, YUI, YL, YLM, YU,
     & U, REPORT, IOS )
      IF ( IOS .NE. 0 ) GO TO 100

      ! Read/process the other files
  102 DONE = .FALSE.
      DO WHILE ( ( .NOT. DONE ) .AND. ( NFIL .LT. NPER ) )

        ! Read another file name/template/hitlist
  110   CALL FILE_PROMPT( TEMPLATE_STR, PROMPT_2, DIR_OUT,
     &   ( CL_TEMP .OR. HIT.OPEN ), HIT, NEW_HIT, FILE_NAME, EOF )

        IF ( EOF ) THEN
          CL_TEMP = .FALSE.
          GO TO 190
        ELSE IF ( FILE_NAME .EQ. ' ' ) THEN ! No more files
          DONE = .TRUE.
        ELSE ! Read/process a file
          NFIL = NFIL + 1
          FNAME(NFIL) = FILE_NAME
          CALL PLOT_FILE_SETUP( NFIL, FNAME(NFIL),
     &     FTYP(NFIL), FINFO(NFIL), DIMSYS_P,
     &     XTYPE, XTI, XUNIT, XUI, XL, XLM, XU,
     &     YTYPE, YTI, YUNIT, YUI, YL, YLM, YU,
     &     U, REPORT, IOS )
          IF ( IOS .NE. 0 ) GO TO 110
        END IF

      END DO
      CL_TEMP = .FALSE.

      ! Set up plot info
      CALL PLOT_SETUP( NFIL, FNAME, FTYP, DIMSYS_P,
     & XTYPE, XTI, XUNIT, XUI, XL, XLM, XU, XTRAN, XUL, XLP, XUP,
     & YTYPE, YTI, YUNIT, YUI, YL, YLM, YU, YTRAN, YUL, YLP, YUP, U )
      SYM_PUT = ( ( SYM_ON ) .AND. ( NFIL .GT. 1 ) )

      ! Get plot header
      WRITE( *, '(/A,62X,A/'' >> '',$)' ) ' Plot title?', '[none]'
      READ( *, '(A)', END=190 ) PLOT_TITLE

      ! Report file entry
      IF ( REPORT.OPEN ) THEN
        WRITE( REPORT.UNIT, '(/A)', IOSTAT=IOS )
     &   '  Plotting to printer'
      END IF

      ! Open the printer
      CALL OPEN_PRN( PRN_UNIT, PLOT_DEV, IOS )
      IF ( IOS .NE. 0 ) THEN
        WRITE( *, * ) '*** Printer connection open failed'
        GO TO 199
      END IF

      ! Initialize the printer
      CALL LJ_PRN_INIT( DPI, MAN_FEED, PRN_UNIT )

      ! Draw the plot box
      CALL LJ_PLOT_BOX( XTRAN, XUL, XLP, XUP,
     & YTRAN, YUL, YLP, YUP, DPI, PRN_UNIT )

      ! Write the plot header
      CALL LJ_HEADER( NFIL, FNAME, FINFO, SYM_PUT, SYM, LSYM,
     & THICK_FAC, PLOT_TITLE, PRN_UNIT )

      ! Write the y-axis labels
      CALL LJ_LABEL_Y( YTYPE, YUNIT, YTRAN, YUL, YLP, YUP, PRN_UNIT )

      ! Write the x-axis labels
      CALL LJ_LABEL_X( XTYPE, XUNIT, XTRAN, XUL, XLP, XUP, PRN_UNIT )

      ! Build the plot raster array
      CALL LJ_PLOT_BLANK( PL, PLOT_SIZE, ROW_SIZE, PRN_UNIT )
      XPD = ( XUP - XLP ) / PLOT_SIZE
      YPD = ( YUP - YLP ) / PLOT_SIZE
      DO I = 1, NFIL
        CALL LJ_DRAW_CURVE( I, FNAME(I), FTYP(I), DIMSYS_P,
     &   XLP, XPD, XTRAN, XUL, YLP, YPD, YTRAN, YUL, U, PL,
     &   DPI, PLOT_SIZE, THICK, SYM_PUT, SYM(I), LSYM(I), PRN_UNIT )
      END DO

      ! Output the plot raster array
      CALL LJ_PLOT_OUT( PL, PLOT_SIZE, ROW_SIZE, PRN_UNIT )

      ! Reset the printer & close the printer device
      CALL LJ_PLOT_RESET( PRN_UNIT )
      CLOSE( PRN_UNIT )

      ! See if same file list is desired
      IF ( ( HIT.OPEN ) .AND. ( HIT.N_REC .GT. 0 ) ) THEN
        GO TO 100
      ELSE IF ( ONE_PASS ) THEN
        GO TO 199
      END IF
      WRITE( *, '(/A,20X,A/'' >> '',$)' )
     & ' First file name/template/hitlist?   ( S - Same list )',
     & '[done]'
      READ( *, '(A)', END=199 ) FILE_NAME
      IF ( FILE_NAME .EQ. ' ' ) THEN ! Done
        GO TO 199
      ELSE IF ( ( FILE_NAME .EQ. 'S' ) .OR.
     & ( FILE_NAME .EQ. 's' ) ) THEN ! Add to current list
        GO TO 102
      ELSE ! Treat as a file name/template/hitlist
        TEMPLATE_STR = FILE_NAME
        GO TO 100
      END IF

      ! Loop for next plot
  190 GO TO 100

      ! Clean up and stop
  199 CALL CLEAN_UP( HIT, REPORT, OUTPUT_LIST, DIR_OUT )

      END



      SUBROUTINE LJPLOT_CL( CL_ARG, NUM_ARGS, PROG_NAME,
     & NPER_CL, XTRAN, YTRAN,
     & DPI, THICK_FAC, SYM_ON, MAN_FEED, PLOT_DEV )

!***********************************************************************
!* Processes the Program-Specific Command Line Arguments
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) CL_ARG(0:*) ! Command line argument array

      INTEGER NUM_ARGS ! Number of arguments

      CHARACTER*(*) PROG_NAME ! Program name

      INTEGER NPER_CL ! Number of curves per plot (<=20) [all]

      CHARACTER*(*) XTRAN ! X-axis transform

      CHARACTER*(*) YTRAN ! Y-axis transform

      INTEGER DPI ! Dots per inch plot resolution

      INTEGER THICK_FAC ! Curve thickness factor (0,1,2,...) [1]

      LOGICAL SYM_ON ! Indicates symbols desired on overlay plots

      LOGICAL MAN_FEED ! Indicates manual feed paper source

      CHARACTER*(*) PLOT_DEV ! Plot device/fil name


      ! Variables ______________________________________________________

      INTEGER I, NS, IOS

      CHARACTER ARG*255, C


      ! Functions ______________________________________________________

      CHARACTER PLOT_TRANSFORM*9


      ! Process command line arguments
      NPER_CL = 0
      XTRAN = ' '
      YTRAN = ' '
      SYM_ON = .TRUE.
      MAN_FEED = .FALSE.
      DPI = 300
      THICK_FAC = 1
      PLOT_DEV = 'PRN'
      DO I = 1, NUM_ARGS
        CALL STR_UPCASE( ARG, CL_ARG(I) )
        C = ARG(1:1)
        IF ( ( C .EQ. '-' ) .OR. ( C .EQ. '/' ) ) THEN
          NS = 2
        ELSE
          NS = 1
        END IF
        IF ( ( ARG(NS:) .EQ. '?' ) .OR.
     &   ( ARG(NS:) .EQ. '??' ) .OR.
     &   ( ARG(NS:) .EQ. 'HELP' ) ) THEN ! Show command line syntax
          CALL SYNTAX_CL( PROG_NAME )
          WRITE( *, '(10X,A/10X,A)' )
     &     '[NPER=<num_per>] [X=LOG] [Y=LOG]',
     &     '[600] [LINE=<thickness>] [NO_SYM] [MF] [PRN=<prn_dev>]'
          IF ( ( ARG(NS:) .EQ. '??' ) .OR.
     &     ( ARG(NS:) .EQ. 'HELP' ) ) THEN ! Show command line help
            CALL HELP_CL( PROG_NAME )
            WRITE( *, '(6(10X,A/),10X,A)' )
     &       '<num_per> :  Number of curves per plot [all (max 20)]',
     &       'X/Y=LOG :  Use logarithmic X/Y axis',
     &       '600 :  600 dpi resolution',
     &       '<thickness> :  Line thickness (0,1,2,...) [1]',
     &       'NO_SYM :  No plot symbols',
     &       'MF :  Manual feed',
     &       '<prn_dev> :  Printer device [PRN]'
          END IF
          STOP ' '
        ELSE IF ( ( ARG(NS:NS+4) .EQ. 'NPER=' ) .OR.
     &   ( ARG(NS:NS+4) .EQ. 'NPER#' ) ) THEN
          READ( ARG(NS+5:), '(BN,I25)', IOSTAT=IOS ) NPER_CL
          IF ( ( IOS .NE. 0 ) .OR. ( NPER_CL .LE. 0 ) ) THEN
            WRITE( *, '(/2A)', IOSTAT=IOS )
     &       ' *** Illegal number of curves per plot: ',
     &       ARG(NS+5:L_TRIM(ARG))
            NPER_CL = 0
          END IF
          CL_ARG(I) = ' '
        ELSE IF ( ( ARG(NS:NS+1) .EQ. 'X=' ) .OR.
     &   ( ARG(NS:NS+1) .EQ. 'X#' ) ) THEN
          XTRAN = PLOT_TRANSFORM( 'X', ARG(NS+2:) )
          CL_ARG(I) = ' '
        ELSE IF ( ( ARG(NS:NS+1) .EQ. 'Y=' ) .OR.
     &   ( ARG(NS:NS+1) .EQ. 'Y#' ) ) THEN
          YTRAN = PLOT_TRANSFORM( 'Y', ARG(NS+2:) )
          CL_ARG(I) = ' '
        ELSE IF ( ( ARG(NS:) .EQ. '600' ) .OR.
     &   ( ARG(NS:) .EQ. '600DPI' ) ) THEN
          DPI = 600
          CL_ARG(I) = ' '
        ELSE IF ( ( ARG(NS:) .EQ. '300' ) .OR.
     &   ( ARG(NS:) .EQ. '300DPI' ) ) THEN
          DPI = 300
          CL_ARG(I) = ' '
        ELSE IF ( ( ARG(NS:NS+4) .EQ. 'LINE=' ) .OR.
     &   ( ARG(NS:NS+4) .EQ. 'LINE#' ) ) THEN
          READ( ARG(NS+5:), '(BN,I25)', IOSTAT=IOS ) THICK_FAC
          IF ( ( IOS .NE. 0 ) .OR. ( THICK_FAC .LT. 0 ) ) THEN
            WRITE( *, '(/A)' )
     &       ' *** Illegal line thickness specified'
            THICK_FAC = 1
          END IF
          CL_ARG(I) = ' '
        ELSE IF ( ( ARG(NS:) .EQ. 'NO_SYM' ) .OR.
     &   ( ARG(NS:) .EQ. 'NOSYM' ) ) THEN
          SYM_ON = .FALSE.
          CL_ARG(I) = ' '
        ELSE IF ( ARG(NS:) .EQ. 'MF' ) THEN
          MAN_FEED = .TRUE.
          CL_ARG(I) = ' '
        ELSE IF ( ( ARG(NS:NS+3) .EQ. 'PRN=' ) .OR.
     &   ( ARG(NS:NS+3) .EQ. 'PRN#' ) ) THEN
          PLOT_DEV = ARG(NS+4:)
          CL_ARG(I) = ' '
        END IF
      END DO

      RETURN
      END



      SUBROUTINE LJ_SYM_SET( SYM, LSYM )

!***********************************************************************
!* Creates the Plot Symbols
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) SYM(*) ! Symbol array

      INTEGER LSYM(*) ! Symbol length array


      ! Variables ______________________________________________________

      INTEGER I


      ! Define plot symbols ____________________________________________

      ! Circle
      SYM(1) = 'PM0CI13,10PM2EP'

      ! Square
      SYM(2) = 'PR11,11PM0PDPR0,-22PR-22,0,PR0,22,PR22,0PM2EP'

      ! Diamond
      SYM(3) = 'PR0,15PM0PDPR15,-15PR-15,-15PR-15,15PR15,15PM2EP'

      ! Hexagon
      SYM(4) = 'PR15,0PM0PDPR-7,12.124PR-14,0PR-7,-12.124'
      SYM(4)(LEN_TRIM(SYM(4))+1:) = 'PR7,-12.124PR14,0PR7,12.124'
      SYM(4)(LEN_TRIM(SYM(4))+1:) = 'PM2EP'

      ! Plus
      SYM(5) = 'PR4,4PM0PDPR0,9PR-8,0,PR0,-9,PR-9,0PR0,-8PR9,0PR0,-9'
      SYM(5)(LEN_TRIM(SYM(5))+1:) = 'PR8,0PR0,9PR9,0PR0,8PR-9,0PM2EP'

      ! Scalloped X plus
      SYM(6) = 'PR10,14PM0PDPR4,-4PR-6,-6PR0,-8PR6,-6PR-4,-4'
      SYM(6)(LEN_TRIM(SYM(6))+1:) = 'PR-6,6PR-8,0PR-6,-6PR-4,4'
      SYM(6)(LEN_TRIM(SYM(6))+1:) = 'PR6,6PR0,8PR-6,6PR4,4'
      SYM(6)(LEN_TRIM(SYM(6))+1:) = 'PR6,-6PR8,0PR6,6PM2EP'

      ! Clover
      SYM(7) = 'PR4,4PM0PDAR-4,4,270,20AR-4,-4,270,20AR4,-4,270,20'
      SYM(7)(LEN_TRIM(SYM(7))+1:) = 'AR4,4,270,20PM2EP'

      ! Four-pointed X star
      SYM(8) = 'PR13,13PM0PDPR-7,-13,PR7,-13,PR-13,7PR-13,-7PR7,13'
      SYM(8)(LEN_TRIM(SYM(8))+1:) = 'PR-7,13PR13,-7PR13,7PM2EP'

      ! Iron cross
      SYM(9) = 'PR3,3PM0PDPR3,10PR-12,0,PR3,-10,PR-10,3PR0,-12PR10,3'
      SYM(9)(LEN_TRIM(SYM(9))+1:) = 'PR-3,-10PR12,0PR-3,10PR10,-3'
      SYM(9)(LEN_TRIM(SYM(9))+1:) = 'PR0,12PR-10,-3PM2EP'

      ! Four-pointed star
      SYM(10) = 'PR4,4PM0PDPR-4,12PR-4,-12PR-12,-4PR12,-4PR4,-12'
      SYM(10)(LEN_TRIM(SYM(10))+1:) = 'PR4,12PR12,4PR-12,4PM2EP'

      ! Filled symbols
      DO I = 11, 20
        SYM(I) = SYM(I-10)(:LEN_TRIM(SYM(I-10)))//'FP'
      END DO

      ! Symbol string lengths
      DO I = 1, 20
        LSYM(I) = LEN_TRIM(SYM(I))
      END DO

      RETURN
      END



      SUBROUTINE LJ_PRN_INIT( DPI, MAN_FEED, PRN_UNIT )

!***********************************************************************
!* Initializes the Printer Device
!***********************************************************************


      ! Arguments ______________________________________________________

      INTEGER DPI ! Dots per inch

      LOGICAL MAN_FEED ! Manual feed flag

      INTEGER PRN_UNIT ! Printer logical unit


      ! Variables ______________________________________________________

      CHARACTER ESC

      PARAMETER ( ESC = CHAR(27) )


      ! Reset the printer and set letter size, portrait mode
      WRITE( PRN_UNIT ) ESC, '%-12345X', ESC, 'E', ESC, '&l2a0O'

      ! Set graphics resolution
      IF ( DPI .EQ. 300 ) THEN
        WRITE( PRN_UNIT ) ESC, '*t300R'
      ELSE IF ( DPI .EQ. 600 ) THEN
        WRITE( PRN_UNIT ) ESC, '*t600R'
      END IF

      ! Set manual feed if requested on command line
      IF ( MAN_FEED ) WRITE( PRN_UNIT ) ESC, '&l2H'

      RETURN
      END



      SUBROUTINE LJ_PLOT_BOX( XTRAN, XUL, XLP, XUP,
     & YTRAN, YUL, YLP, YUP, DPI, PRN_UNIT )

!***********************************************************************
!* Draws the Plot Box
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) XTRAN ! X-axis transform flag

      LOGICAL XUL ! X-axis unitless flag

      DOUBLE PRECISION XLP ! X-axis lower plot range

      DOUBLE PRECISION XUP ! X-axis upper plot range

      CHARACTER*(*) YTRAN ! Y-axis transform flag

      LOGICAL YUL ! Y-axis unitless flag

      DOUBLE PRECISION YLP ! Y-axis lower plot range

      DOUBLE PRECISION YUP ! Y-axis upper plot range

      INTEGER DPI ! Dots per inch

      INTEGER PRN_UNIT ! Printer logical unit


      ! Variables ______________________________________________________

      INTEGER I

      CHARACTER ESC, ZPOS*4

      PARAMETER ( ESC = CHAR(27) )


      ! PLOT BORDERS ___________________________________________________

      ! Move to the upper left corner of the plot box
      WRITE( PRN_UNIT ) ESC, '*p599x1299Y'

      ! Top and left borders
      WRITE( PRN_UNIT ) ESC, '*c1503a2b0P', ESC, '*c2a1503b0P'

      ! Right border
      WRITE( PRN_UNIT ) ESC, '*p+1501X', ESC, '*c2a1503b0P'

      ! Bottom border
      WRITE( PRN_UNIT ) ESC, '*p-1501x+1501Y', ESC, '*c1503a2b0P'


      ! TIC MARKS ______________________________________________________

      ! Move to the upper left corner of the plot
      WRITE( PRN_UNIT ) ESC, '*p600x1300Y'

      ! Top and bottom tic marks
      IF ( .NOT. XUL ) THEN
        DO I = 1, 4
          WRITE( PRN_UNIT ) ESC, '*p+300X', ESC, '*c2a20b0P'
        END DO
        WRITE( PRN_UNIT ) ESC, '*p600X'
        DO I = 1, 24
          WRITE( PRN_UNIT ) ESC, '*p+60X', ESC, '*c1a10b0P'
        END DO
        WRITE( PRN_UNIT ) ESC, '*p600x+1480Y'
        DO I = 1, 4
          WRITE( PRN_UNIT ) ESC, '*p+300X', ESC, '*c2a20b0P'
        END DO
        WRITE( PRN_UNIT ) ESC, '*p600x+10Y'
        DO I = 1, 24
          WRITE( PRN_UNIT ) ESC, '*p+60X', ESC, '*c1a10b0P'
        END DO
      END IF

      ! Left and right tic marks
      IF ( .NOT. YUL ) THEN
        WRITE( PRN_UNIT ) ESC, '*p600x1300Y'
        DO I = 1, 4
          WRITE( PRN_UNIT ) ESC, '*p+300Y', ESC, '*c20a2b0P'
        END DO
        WRITE( PRN_UNIT ) ESC, '*p1300Y'
        DO I = 1, 24
          WRITE( PRN_UNIT ) ESC, '*p+60Y', ESC, '*c10a1b0P'
        END DO
        WRITE( PRN_UNIT ) ESC, '*p2080x1300Y'
        DO I = 1, 4
          WRITE( PRN_UNIT ) ESC, '*p+300Y', ESC, '*c20a2b0P'
        END DO
        WRITE( PRN_UNIT ) ESC, '*p+10x1300Y'
        DO I = 1, 24
          WRITE( PRN_UNIT ) ESC, '*p+60Y', ESC, '*c10a1b0P'
        END DO
      END IF


      ! ZERO LINES _____________________________________________________

      ! X = 0 line
      IF ( ( XLP .LT. 0.D0 ) .AND. ( XUP .GT. 0.D0 ) .AND.
     & ( BLANK( XTRAN ) ) ) THEN
        WRITE( PRN_UNIT ) ESC, '*p600x1300Y' ! Move to plot upper left
        WRITE( ZPOS, '(I4)' ) NINT( -XLP / ( ( XUP - XLP ) / 1500 ) )
        WRITE( PRN_UNIT ) ESC, '*p+', ZPOS, 'X', ESC, '*c1a1501b0P'
      END IF

      ! Y = 0 line
      IF ( ( YLP .LT. 0.D0 ) .AND. ( YUP .GT. 0.D0 ) .AND.
     & ( BLANK( YTRAN ) ) ) THEN
        WRITE( PRN_UNIT ) ESC, '*p600x1300Y' ! Move to plot upper left
        WRITE( ZPOS, '(I4)' ) NINT( YUP / ( ( YUP - YLP ) / 1500 ) )
        WRITE( PRN_UNIT ) ESC, '*p+', ZPOS, 'Y', ESC, '*c1501a1b0P'
      END IF

      ! Set raster box size
      IF ( DPI .EQ. 300 ) THEN
        WRITE( PRN_UNIT ) ESC, '*r1500t1500S'
      ELSE IF ( DPI .EQ. 600 ) THEN
        WRITE( PRN_UNIT ) ESC, '*r3000t3000S'
      END IF


      RETURN
      END



      SUBROUTINE LJ_HEADER( NFIL, FNAME, FINFO, SYM_PUT, SYM, LSYM,
     & THICK_FAC, PLOT_TITLE, PRN_UNIT )

!***********************************************************************
!* Creates the Plot Header
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      INTEGER NFIL ! Number of files

      CHARACTER*(*) FNAME(*) ! File name array

      CHARACTER*(*) FINFO(*) ! File info array

      LOGICAL SYM_PUT ! Symbols desired flag

      CHARACTER*(*) SYM(*) ! Symbol array

      INTEGER LSYM(*) ! Symbol length array

      INTEGER THICK_FAC ! Line thickness factor

      CHARACTER*(*) PLOT_TITLE ! Plot title

      INTEGER PRN_UNIT ! Printer logical unit


      ! Variables ______________________________________________________

      INTEGER I, IOS, NSPT, NSPF

      REAL PEN_WIDTH

      CHARACTER CURDAT*10, CURTIM*8, SPT*4, SPF*2, PEN_WID*4,
     & ESC, SO, SI, ETX

      PARAMETER ( ESC = CHAR(27), SO = CHAR(14), SI = CHAR(15),
     & ETX = CHAR(3) )


      ! Set Univers fonts
      WRITE( PRN_UNIT ) ESC, '(s1p8v0s3b4148T' ! Primary
      WRITE( PRN_UNIT ) ESC, ')s1p7v0s0b4148T' ! Secondary

      ! Write the date/time stamp
      CALL DATEISO( CURDAT )
      CALL TIMEISO( CURTIM )
      WRITE( PRN_UNIT ) ESC, '*p0x0Y', CURDAT, '  ', CURTIM(:5)

      ! Set inter-line spacing
      NSPT = MAX( 1200 - 107 * NFIL, 0 )
      WRITE( SPT, '(I4)' ) NSPT
      NSPF = INT( 76 - 4.5 * MAX( NFIL-10, 0 ) )
      WRITE( SPF, '(I2)' ) NSPF

      ! Write the filenames and file info
      WRITE( PRN_UNIT ) ESC, '*p', SPT, 'Y'
      DO I = 1, NFIL
        WRITE( PRN_UNIT ) ESC, '*p600X',
     &   FNAME(I)(:L_TRIM(FNAME(I)))
        ! ESC)(s to work around NEC SilentWriter 95 bug
        WRITE( PRN_UNIT ) ESC, '*p620x+31Y', ESC, ')s7V', SO, FINFO(I)
        WRITE( PRN_UNIT ) ESC, '(s8V', SI, ESC, '*p+', SPF, 'Y'
      END DO

      ! Turn on HP-GL/2 mode with PCL coordinate system
      WRITE( PRN_UNIT ) ESC, '*c5760x7272Y'
      WRITE( PRN_UNIT ) ESC, '*p0x0y-30Y'
      WRITE( PRN_UNIT ) ESC, '*c0T'
      WRITE( PRN_UNIT ) ESC, '%0B'
      WRITE( PRN_UNIT ) 'INSP1'
      IF ( THICK_FAC .EQ. 0 ) THEN
        PEN_WIDTH = .883 * .08467
      ELSE IF ( THICK_FAC .LE. 3 ) THEN
        PEN_WIDTH = 1.751 * .08467
      ELSE
        PEN_WIDTH = ( MIN( THICK_FAC - .49, 6. ) * .08467 )
      END IF
      WRITE( PEN_WID, '(F4.3)', IOSTAT=IOS ) PEN_WIDTH
      WRITE( PRN_UNIT ) 'PW', PEN_WID
      WRITE( PRN_UNIT ) 'SC0,3.386667,0,-3.386667,2'
      WRITE( PRN_UNIT ) 'IR0,99,100,0'

      ! Write the plot heading and symbols
      IF ( SYM_PUT ) THEN ! Add symbols
        NSPF = INT( 107 - 4.5 * MAX( NFIL-10, 0 ) )
        DO I = 1, NFIL
          WRITE( SPT, '(I4)' ) NSPT - 11 + ( I - 1 ) * NSPF
          WRITE( PRN_UNIT ) 'PA563,', SPT
          WRITE( PRN_UNIT ) SYM(I)(:LSYM(I))
        END DO
      END IF

      ! Write plot heading
      WRITE( PRN_UNIT ) 'SD2,1,4,10,5,0,6,3,7,4148SS'
      WRITE( PRN_UNIT ) 'PA1350,1280LO4'
      WRITE( PRN_UNIT ) 'LB', PLOT_TITLE(:L_TRIM(PLOT_TITLE)), ETX

      RETURN
      END



      SUBROUTINE LJ_LABEL_Y( YTYPE, YUNIT, YTRAN, YUL, YLP, YUP,
     & PRN_UNIT )

!***********************************************************************
!* Writes the Y-Axis Labels
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) YTYPE ! Y-axis data type

      CHARACTER*(*) YUNIT ! Y-axis data units

      CHARACTER*(*) YTRAN ! Y-axis transform flag

      LOGICAL YUL ! Y-axis unitless flag

      DOUBLE PRECISION YLP ! Y-axis lower plot range

      DOUBLE PRECISION YUP ! Y-axis upper plot range

      INTEGER PRN_UNIT ! Printer logical unit


      ! Variables ______________________________________________________

      DOUBLE PRECISION YINC

      CHARACTER ETX, NUM*15

      PARAMETER ( ETX = CHAR(3) )


      ! Functions ______________________________________________________

      CHARACTER NUM_REV*15


      ! Y-axis labels
      WRITE( PRN_UNIT ) 'SD2,1,4,10,5,0,6,3,7,4148SSDI0,1'
      WRITE( PRN_UNIT ) 'LO6PA200,2300'
      WRITE( PRN_UNIT ) 'LB', YTYPE(:L_TRIM(YTYPE)), ETX
      IF ( .NOT. YUL ) THEN ! Write y-axis units
        WRITE( PRN_UNIT ) 'AD2,1,4,9,5,1,6,0,7,4148SA'
        WRITE( PRN_UNIT ) 'LO6PA200,1700'
        WRITE( PRN_UNIT ) 'LB', YUNIT(:L_TRIM(YUNIT)), ETX
      END IF
      WRITE( PRN_UNIT ) 'DI1,0'

      ! Y-axis tic values
      IF ( .NOT. YUL ) THEN

        WRITE( PRN_UNIT ) 'SD2,1,4,8,5,0,6,0,7,4148SSDV2LO8'
        YINC = ( YUP - YLP ) / 5

        NUM = NUM_REV( YUP, YINC )
        WRITE( PRN_UNIT ) 'PA582,1300LB', NUM(:L_TRIM(NUM)), ETX
        IF ( YTRAN .EQ. 'LOG' ) WRITE( PRN_UNIT ) 'PR0,16LB01', ETX

        NUM = NUM_REV( YUP - YINC, YINC )
        WRITE( PRN_UNIT ) 'PA582,1600LB', NUM(:L_TRIM(NUM)), ETX
        IF ( YTRAN .EQ. 'LOG' ) WRITE( PRN_UNIT ) 'PR0,16LB01', ETX

        NUM = NUM_REV( YUP - 2 * YINC, YINC )
        WRITE( PRN_UNIT ) 'PA582,1900LB', NUM(:L_TRIM(NUM)), ETX
        IF ( YTRAN .EQ. 'LOG' ) WRITE( PRN_UNIT ) 'PR0,16LB01', ETX

        NUM = NUM_REV( YLP + 2 * YINC, YINC )
        WRITE( PRN_UNIT ) 'PA582,2200LB', NUM(:L_TRIM(NUM)), ETX
        IF ( YTRAN .EQ. 'LOG' ) WRITE( PRN_UNIT ) 'PR0,16LB01', ETX

        NUM = NUM_REV( YLP + YINC, YINC )
        WRITE( PRN_UNIT ) 'PA582,2500LB', NUM(:L_TRIM(NUM)), ETX
        IF ( YTRAN .EQ. 'LOG' ) WRITE( PRN_UNIT ) 'PR0,16LB01', ETX

        NUM = NUM_REV( YLP, YINC )
        IF ( YTRAN .NE. 'LOG' ) THEN
          WRITE( PRN_UNIT ) 'PA582,2800LB', NUM(:L_TRIM(NUM)), ETX
        ELSE
          WRITE( PRN_UNIT ) 'PA582,2784LB', NUM(:L_TRIM(NUM)), ETX
          WRITE( PRN_UNIT ) 'PR0,16LB01', ETX
        END IF

        WRITE( PRN_UNIT ) 'DVLO'

      END IF

      RETURN
      END



      SUBROUTINE LJ_LABEL_X( XTYPE, XUNIT, XTRAN, XUL, XLP, XUP,
     & PRN_UNIT )

!***********************************************************************
!* Writes the X-Axis Labels
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) XTYPE ! X-axis data type

      CHARACTER*(*) XUNIT ! X-axis data units

      CHARACTER*(*) XTRAN ! X-axis transform flag

      LOGICAL XUL ! X-axis unitless flag

      DOUBLE PRECISION XLP ! X-axis lower plot range

      DOUBLE PRECISION XUP ! X-axis upper plot range

      INTEGER PRN_UNIT ! Printer logical unit


      ! Variables ______________________________________________________

      INTEGER IOS, LN

      DOUBLE PRECISION XINC

      CHARACTER ETX, OFFSET, NUM*15

      PARAMETER ( ETX = CHAR(3) )


      ! Functions ______________________________________________________

      CHARACTER NUM_REV*15


      ! X-axis labels
      WRITE( PRN_UNIT ) 'SD2,1,4,10,5,0,6,3,7,4148SS'
      WRITE( PRN_UNIT ) 'LO6PA1100,2950'
      WRITE( PRN_UNIT ) 'LB', XTYPE(:L_TRIM(XTYPE)), ETX
      IF ( .NOT. XUL ) THEN ! Write x-axis units
        WRITE( PRN_UNIT ) 'AD2,1,4,9,5,1,6,0,7,4148SA'
        WRITE( PRN_UNIT ) 'LO6PA1700,2950'
        WRITE( PRN_UNIT ) 'LB', XUNIT(:L_TRIM(XUNIT)), ETX
      END IF

      ! X-axis tic values
      IF ( .NOT. XUL ) THEN

        WRITE( PRN_UNIT ) 'SD2,1,4,8,5,0,6,0,7,4148SSDV2LO6'
        XINC = ( XUP - XLP ) / 5

        NUM = NUM_REV( XLP, XINC )
        WRITE( PRN_UNIT ) 'PA600,2818LB', NUM(:L_TRIM(NUM)), ETX
        IF ( XTRAN .EQ. 'LOG' ) THEN
          LN = ( L_TRIM(NUM) / 2 ) + 3
          WRITE( OFFSET, '(I1)', IOSTAT=IOS ) LN
          WRITE( PRN_UNIT ) 'CP', OFFSET,',0PR0,17LB01', ETX
        END IF

        NUM = NUM_REV( XLP + XINC, XINC )
        WRITE( PRN_UNIT ) 'PA900,2818LB', NUM(:L_TRIM(NUM)), ETX
        IF ( XTRAN .EQ. 'LOG' ) THEN
          LN = ( L_TRIM(NUM) / 2 ) + 3
          WRITE( OFFSET, '(I1)', IOSTAT=IOS ) LN
          WRITE( PRN_UNIT ) 'CP', OFFSET,',0PR0,17LB01', ETX
        END IF

        NUM = NUM_REV( XLP + 2 * XINC, XINC )
        WRITE( PRN_UNIT ) 'PA1200,2818LB', NUM(:L_TRIM(NUM)), ETX
        IF ( XTRAN .EQ. 'LOG' ) THEN
          LN = ( L_TRIM(NUM) / 2 ) + 3
          WRITE( OFFSET, '(I1)', IOSTAT=IOS ) LN
          WRITE( PRN_UNIT ) 'CP', OFFSET,',0PR0,17LB01', ETX
        END IF

        NUM = NUM_REV( XUP - 2 * XINC, XINC )
        WRITE( PRN_UNIT ) 'PA1500,2818LB', NUM(:L_TRIM(NUM)), ETX
        IF ( XTRAN .EQ. 'LOG' ) THEN
          LN = ( L_TRIM(NUM) / 2 ) + 3
          WRITE( OFFSET, '(I1)', IOSTAT=IOS ) LN
          WRITE( PRN_UNIT ) 'CP', OFFSET,',0PR0,17LB01', ETX
        END IF

        NUM = NUM_REV( XUP - XINC, XINC )
        WRITE( PRN_UNIT ) 'PA1800,2818LB', NUM(:L_TRIM(NUM)), ETX
        IF ( XTRAN .EQ. 'LOG' ) THEN
          LN = ( L_TRIM(NUM) / 2 ) + 3
          WRITE( OFFSET, '(I1)', IOSTAT=IOS ) LN
          WRITE( PRN_UNIT ) 'CP', OFFSET,',0PR0,17LB01', ETX
        END IF

        NUM = NUM_REV( XUP, XINC )
        WRITE( PRN_UNIT ) 'PA2100,2818LB', NUM(:L_TRIM(NUM)), ETX
        IF ( XTRAN .EQ. 'LOG' ) THEN
          LN = ( L_TRIM(NUM) / 2 ) + 3
          WRITE( OFFSET, '(I1)', IOSTAT=IOS ) LN
          WRITE( PRN_UNIT ) 'CP', OFFSET,',0PR0,17LB01', ETX
        END IF

        WRITE( PRN_UNIT ) 'DVLO'

      END IF

      RETURN
      END



      SUBROUTINE LJ_PLOT_BLANK( PL, PLOT_SIZE, ROW_SIZE, PRN_UNIT )

!***********************************************************************
!* Blanks the Plot Raster Array
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER PL(375,3000) ! Plot raster matrix

      INTEGER PLOT_SIZE ! Plot size in laser dots

      INTEGER ROW_SIZE ! Plot row length in characters

      INTEGER PRN_UNIT ! Printer logical unit


      ! Variables ______________________________________________________

      INTEGER I, J

      CHARACTER ZERO_C

      PARAMETER ( ZERO_C = CHAR(0) )


      ! Blank out the plot raster array
      DO J = 1, PLOT_SIZE
        DO I = 1, ROW_SIZE
          PL(I,J) = ZERO_C
        END DO
      END DO

      ! Set HP-GL/2 clipped plot box
      WRITE( PRN_UNIT ) 'IP2032,677,7112,5757'
      WRITE( PRN_UNIT ) 'SC0,1500,0,1500'
      WRITE( PRN_UNIT ) 'IW0,0,1500,1500'

      RETURN
      END



      SUBROUTINE LJ_DRAW_CURVE( IFIL, FNAME, FTYP, DIMSYS_P,
     & XLP, XPD, XTRAN, XUL, YLP, YPD, YTRAN, YUL, U, PL,
     & DPI, PLOT_SIZE, THICK, SYM_PUT, SYM, LSYM, PRN_UNIT )

!***********************************************************************
!* Creates the Raster Image for One Curve
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'


      ! Arguments ______________________________________________________

      INTEGER IFIL ! Index of file

      CHARACTER*(*) FNAME ! File name

      CHARACTER*(*) FTYP ! File type

      CHARACTER*(*) DIMSYS_P ! Plot dimensional system

      DOUBLE PRECISION XLP ! Lower X-axis value of plot box

      DOUBLE PRECISION XPD ! X-axis value increment per laser dot

      CHARACTER*(*) XTRAN ! X-axis transform flag

      LOGICAL XUL ! X-axis unitless flag

      DOUBLE PRECISION YLP ! Lower Y-axis value of plot box

      DOUBLE PRECISION YPD ! X-axis value increment per laser dot

      CHARACTER*(*) YTRAN ! Y-axis transform flag

      LOGICAL YUL ! Y-axis unitless flag

      RECORD /UDS/  U ! UDS structure

      CHARACTER PL(375,3000) ! Plot raster matrix

      INTEGER DPI ! Dots per inch plot resolution

      INTEGER PLOT_SIZE ! Plot size in laser dots

      DOUBLE PRECISION THICK ! Line thickness

      LOGICAL SYM_PUT ! Symbols desired flag

      CHARACTER*(*) SYM ! Symbol (character encoded)

      INTEGER LSYM ! Symbol length

      INTEGER PRN_UNIT ! Printer logical unit


      ! Variables ______________________________________________________

      LOGICAL X1FIN, X2FIN, Y1FIN, Y2FIN, P1FIN, P2FIN, IN_BOX

      INTEGER I, IOS, SSYM, NSYM

      DOUBLE PRECISION X1, X2, Y1, Y2, XLC, XUC, XLS, YLC, YUC, YLS,
     & HTHICK, THICK_SQ, HTHICK_SQ, FAC, ZERO

      PARAMETER ( ZERO = 0.D0 )


      ! Read the file
      CALL PLOT_FILE_READ( IFIL, FNAME, FTYP, DIMSYS_P, U, IOS )
      IF ( IOS .NE. 0 ) RETURN

      ! Find range for unitless axes
      IF ( XUL ) THEN ! Unitless X-axis
        X1FIN = .FALSE.
        X2FIN = .TRUE.
        DO I = U.NFP, U.NLP
          CALL PLOT_WIN_PT( XTRAN, U.X(I), X2, X2FIN )
          IF ( X2FIN ) THEN
            IF ( .NOT. X1FIN ) THEN
              X1FIN = .TRUE.
              XLC = X2
              XUC = X2
            ELSE
              XLC = MIN( XLC, X2 )
              XUC = MAX( XUC, X2 )
            END IF
          END IF
        END DO
        IF ( .NOT. X1FIN ) RETURN ! No values to plot
        XUC = XUC - XLC
        IF ( XUC .EQ. ZERO ) XUC = 1.D0
      END IF
      IF ( YUL ) THEN ! Unitless Y-axis
        Y1FIN = .FALSE.
        Y2FIN = .TRUE.
        DO I = U.NFP, U.NLP
          CALL PLOT_WIN_PT( YTRAN, U.Y(I), Y2, Y2FIN )
          IF ( Y2FIN ) THEN
            IF ( .NOT. Y1FIN ) THEN
              Y1FIN = .TRUE.
              YLC = Y2
              YUC = Y2
            ELSE
              YLC = MIN( YLC, Y2 )
              YUC = MAX( YUC, Y2 )
            END IF
          END IF
        END DO
        IF ( .NOT. Y1FIN ) RETURN ! No values to plot
        YUC = YUC - YLC
        IF ( YUC .EQ. ZERO ) YUC = 1.D0
      END IF

      ! Initialize for first point
      IN_BOX = .FALSE.
      SSYM = 1
  101 NSYM = 0
      X2FIN = .TRUE.
      Y2FIN = .TRUE.
      CALL LJ_WIN_PT( XTRAN, XUL, XLC, XUC, U.X(U.NFP), XLP, XPD, X2,
     & X2FIN )
      CALL LJ_WIN_PT( YTRAN, YUL, YLC, YUC, U.Y(U.NFP), YLP, YPD, Y2,
     & Y2FIN )
      XLS = X2
      YLS = Y2
      P2FIN = ( X2FIN .AND. Y2FIN )

      ! Set line thickness constants
      HTHICK = THICK / 2.D0
      THICK_SQ = THICK**2
      HTHICK_SQ = HTHICK**2

      ! Add the curve to the plot raster array
      DO I = U.NFP+1, U.NLP
        X1 = X2
        X1FIN = X2FIN
        Y1 = Y2
        Y1FIN = Y2FIN
        P1FIN = P2FIN
        CALL LJ_WIN_PT( XTRAN, XUL, XLC, XUC, U.X(I), XLP, XPD, X2,
     &   X2FIN )
        CALL LJ_WIN_PT( YTRAN, YUL, YLC, YUC, U.Y(I), YLP, YPD, Y2,
     &   Y2FIN )
        P2FIN = ( X2FIN .AND. Y2FIN )

        IF ( P1FIN .AND. P2FIN ) THEN ! Both points are finite
          CALL LJ_DRAW_LINE( X1, Y1, X2, Y2, XLS, YLS,
     &     PL, DPI, PLOT_SIZE, HTHICK, THICK_SQ, HTHICK_SQ,
     &     IN_BOX, IFIL, SYM_PUT, SYM, LSYM, SSYM, NSYM, PRN_UNIT )
        ELSE IF ( P1FIN ) THEN ! P1 finite, P2 = -infinity
          IF ( ( .NOT. X2FIN ) .AND. ( .NOT. Y2FIN ) )
     &     THEN ! P2=(-inf,-inf)
            FAC = MAX( X1 * XPD, Y1 * YPD ) * 2.D0
            CALL LJ_DRAW_LINE( X1, Y1, X1-(FAC/XPD), Y1-(FAC/YPD),
     &       XLS, YLS,
     &       PL, DPI, PLOT_SIZE, HTHICK, THICK_SQ, HTHICK_SQ,
     &       IN_BOX, IFIL, SYM_PUT, SYM, LSYM, SSYM, NSYM,
     &       PRN_UNIT )
          ELSE IF ( .NOT. X2FIN ) THEN ! X2 = -infinity
            CALL LJ_DRAW_LINE( X1, Y1, ZERO, Y1, XLS, YLS,
     &       PL, DPI, PLOT_SIZE, HTHICK, THICK_SQ, HTHICK_SQ,
     &       IN_BOX, IFIL, SYM_PUT, SYM, LSYM, SSYM, NSYM,
     &       PRN_UNIT )
          ELSE ! Y2 = -infinity
            CALL LJ_DRAW_LINE( X1, Y1, X1, ZERO, XLS, YLS,
     &       PL, DPI, PLOT_SIZE, HTHICK, THICK_SQ, HTHICK_SQ,
     &       IN_BOX, IFIL, SYM_PUT, SYM, LSYM, SSYM, NSYM,
     &       PRN_UNIT )
          END IF
        ELSE IF ( P2FIN ) THEN ! P2 finite, P1 = -infinity
          IF ( ( .NOT. X1FIN ) .AND. ( .NOT. Y1FIN ) )
     &     THEN ! P1=(-inf,-inf)
            FAC = MAX( X2 * XPD, Y2 * YPD ) * 2.D0
            CALL LJ_DRAW_LINE( X2, Y2, X2-(FAC/XPD), Y2-(FAC/YPD),
     &       XLS, YLS,
     &       PL, DPI, PLOT_SIZE, HTHICK, THICK_SQ, HTHICK_SQ,
     &       IN_BOX, IFIL, SYM_PUT, SYM, LSYM, SSYM, NSYM,
     &       PRN_UNIT )
          ELSE IF ( .NOT. X1FIN ) THEN ! X1 = -infinity
            CALL LJ_DRAW_LINE( X2, Y2, ZERO, Y2, XLS, YLS,
     &       PL, DPI, PLOT_SIZE, HTHICK, THICK_SQ, HTHICK_SQ,
     &       IN_BOX, IFIL, SYM_PUT, SYM, LSYM, SSYM, NSYM,
     &       PRN_UNIT )
          ELSE ! Y1 = -infinity
            CALL LJ_DRAW_LINE( X2, Y2, X2, ZERO, XLS, YLS,
     &       PL, DPI, PLOT_SIZE, HTHICK, THICK_SQ, HTHICK_SQ,
     &       IN_BOX, IFIL, SYM_PUT, SYM, LSYM, SSYM, NSYM,
     &       PRN_UNIT )
          END IF
        END IF
      END DO

      ! Add more symbols if necessary
      IF ( ( SYM_PUT ) .AND. ( IN_BOX ) .AND. ( NSYM .LE. 2 ) .AND.
     & ( SSYM .LE. 2 ) ) THEN
        SSYM = SSYM + 1
        GO TO 101
      END IF

      RETURN
      END



      SUBROUTINE LJ_WIN_PT( ZTRAN, ZUL, ZLC, ZUC, ZVAL, ZLP, ZPD, Z,
     & ZFIN )

!***********************************************************************
!* Assigns Scaled Window Point Values
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) ZTRAN ! Z-axis transform flag

      LOGICAL ZUL ! Z-axis unitless flag

      DOUBLE PRECISION ZLC ! Z-axis unitless mode lower value

      DOUBLE PRECISION ZUC ! Z-axis unitless mode upper value

      REAL ZVAL ! Actual Z-axis point value

      DOUBLE PRECISION ZLP ! Z-axis lower plot value

      DOUBLE PRECISION ZPD ! Z-axis value increment per laser dot

      DOUBLE PRECISION Z ! Z-axis window point value

      LOGICAL ZFIN ! Z-axis finite flag


      ! Assign point
      IF ( BLANK( ZTRAN ) ) THEN
        Z = ZVAL
      ELSE IF ( ZTRAN .EQ. 'LOG' ) THEN
        IF ( ZVAL .NE. 0.D0 ) THEN
          Z = LOG10( ABS( ZVAL ) )
          ZFIN = .TRUE.
        ELSE
          ZFIN = .FALSE.
        END IF
      END IF

      ! Perform unitless re-scale and pixel scaling
      IF ( ZUL .AND. ZFIN ) Z = ( Z - ZLC ) / ZUC
      IF ( ZFIN ) Z = ( Z - ZLP ) / ZPD

      RETURN
      END



      SUBROUTINE LJ_DRAW_LINE( X1, Y1, X2, Y2, XLS, YLS,
     & PL, DPI, PLOT_SIZE, HTHICK, THICK_SQ, HTHICK_SQ,
     & IN_BOX, IFIL, SYM_PUT, SYM, LSYM, SSYM, NSYM, PRN_UNIT )

!***********************************************************************
!* Draws a Line From Point 1 to Point 2 in the Plot Box
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      DOUBLE PRECISION X1 ! X-axis value at point 1

      DOUBLE PRECISION Y1 ! Y-axis value at point 1

      DOUBLE PRECISION X2 ! X-axis value at point 2

      DOUBLE PRECISION Y2 ! Y-axis value at point 2

      DOUBLE PRECISION XLS ! X-axis value at last symbol

      DOUBLE PRECISION YLS ! Y-axis value at last symbol

      CHARACTER PL(375,3000) ! Plot raster matrix

      INTEGER DPI ! Dots per inch plot resolution

      INTEGER PLOT_SIZE ! Plot size in laser dots

      DOUBLE PRECISION HTHICK ! Half line thickness

      DOUBLE PRECISION THICK_SQ ! Line thickness squared

      DOUBLE PRECISION HTHICK_SQ ! Half line thickness squared

      LOGICAL IN_BOX ! Inside plot box flag

      INTEGER IFIL ! Index of file

      LOGICAL SYM_PUT ! Symbols desired flag

      CHARACTER*(*) SYM ! Symbol (character encoded)

      INTEGER LSYM ! Symbol length

      INTEGER SSYM ! Symbol pass counter

      INTEGER NSYM ! Number of symbols

      INTEGER PRN_UNIT ! Printer logical unit


      ! Variables ______________________________________________________

      INTEGER IOS, INC, IS, IX, IXS, IY, IYS, ITHICK, LO, HI,
     & NSYM_OLD, PSYM

      DOUBLE PRECISION XLO, XHI, YLO, YHI,
     & SYM_INC, SYM_INCM, SYM_OFF, XS, YS, XDA, YDA,
     & SLP, SLPI, SLP_SQ, SLP_FAC, DPPS,
     & XTHICK, YTHICK, YDEL,
     & XOH, YOH, XYLO, XYHI, YXLO, YXHI,
     & DLO, DHI, DPDPI, DPIS

      CHARACTER PX*4, PY*4


      ! Functions ______________________________________________________

      CHARACTER LJUST*4

      EXTERNAL LJUST


      ! Draw line from point 1 to point 2 if it intersects box
      XLO = MIN( X1, X2 )
      XHI = MAX( X1, X2 )
      YLO = MIN( Y1, Y2 )
      YHI = MAX( Y1, Y2 )
      DPPS = PLOT_SIZE
      IF ( ( XHI .GE. 0.D0 ) .AND. ( XLO .LE. DPPS ) .AND.
     & ( YHI .GE. 0.D0 ) .AND. ( YLO .LE. DPPS ) ) THEN

        ! Add segment between endpoints
        IF ( SSYM .EQ. 1 ) THEN ! First time through - Draw line
          IN_BOX = .TRUE.

          ! Draw circle line ends
          DO IX = NINT( X1 - HTHICK + .5D0 ), NINT( X1 + HTHICK - .5D0 )
            YDEL = SQRT( MAX( HTHICK_SQ  - ( IX - X1 )**2, 0.D0 ) )
            DO IY = NINT( Y1 - YDEL + .5D0 ), NINT( Y1 + YDEL - .5D0 )
              CALL LJ_PLOT_ADD( IX, IY, PL, PLOT_SIZE )
            END DO
          END DO
          DO IX = NINT( X2 - HTHICK + .5D0 ), NINT( X2 + HTHICK - .5D0 )
            YDEL = SQRT( MAX( HTHICK_SQ  - ( IX - X2 )**2, 0.D0 ) )
            DO IY = NINT( Y2 - YDEL + .5D0 ), NINT( Y2 + YDEL - .5D0 )
              CALL LJ_PLOT_ADD( IX, IY, PL, PLOT_SIZE )
            END DO
          END DO

          IF ( YHI - YLO .LT. XHI - XLO ) THEN ! Shallow segment
            SLP = ( Y2 - Y1 ) / ( X2 - X1 )
            IF ( SLP .NE. 0.D0 ) SLPI = 1.D0 / SLP
            SLP_SQ = SLP**2
            SLP_FAC = SLP_SQ / ( SLP_SQ + 1.D0 )
            YTHICK = SQRT( MAX( ( SLP_SQ + 1 ) * THICK_SQ, 0.D0 ) )
            YTHICK = MAX( YTHICK - 1.D0, 0.D0 )
            ITHICK = INT( YTHICK )
            YTHICK = YTHICK / 2.D0
            XOH = SQRT( HTHICK_SQ * SLP_FAC )
            IF ( YLO .EQ. Y1 ) THEN
              XYLO = X1
              XYHI = X2
            ELSE
              XYLO = X2
              XYHI = X1
            END IF
            DO IX = NINT( MAX( XLO-XOH, 0.D0 ) ),
     &       NINT( MIN( XHI+XOH, DPPS ) )
              LO = NINT( Y1 + ( IX - X1 ) * SLP - YTHICK )
              HI = LO + ITHICK
              IF ( SLP .NE. 0.D0 ) THEN
                DLO = YLO - ( IX - XYLO ) * SLPI + .5D0
                DLO = MIN( MAX( DLO, 0.D0 ), DPPS )
                LO = MAX( LO, NINT( DLO ) )
                DHI = YHI - ( IX - XYHI ) * SLPI - .5D0
                DHI = MIN( MAX( DHI, 0.D0 ), DPPS )
                HI = MIN( HI, NINT( DHI ) )
              END IF
              DO IY = LO, HI
                IF ( ( IY .GT. 0 ) .AND. ( IY .LT. PLOT_SIZE ) )
     &           CALL LJ_PLOT_ADD( IX, IY, PL, PLOT_SIZE )
              END DO
            END DO
          ELSE IF ( Y2 .NE. Y1 ) THEN ! Steep segment
            SLP = ( X2 - X1 ) / ( Y2 - Y1 )
            IF ( SLP .NE. 0.D0 ) SLPI = 1.D0 / SLP
            SLP_SQ = SLP**2
            SLP_FAC = SLP_SQ / ( SLP_SQ + 1.D0 )
            XTHICK = SQRT( MAX( ( SLP**2 + 1 ) * THICK_SQ, 0.D0 ) )
            XTHICK = MAX( XTHICK - 1.D0, 0.D0 )
            ITHICK = INT( XTHICK )
            XTHICK = XTHICK / 2.D0
            YOH = SQRT( HTHICK_SQ * SLP_FAC )
            IF ( XLO .EQ. X1 ) THEN
              YXLO = Y1
              YXHI = Y2
            ELSE
              YXLO = Y2
              YXHI = Y1
            END IF
            DO IY = NINT( MAX( YLO-YOH, 0.D0 ) ),
     &       NINT( MIN( YHI+YOH, DPPS ) )
              LO = NINT( X1 + ( IY - Y1 ) * SLP - XTHICK )
              HI = LO + ITHICK
              IF ( SLP .NE. 0.D0 ) THEN
                LO = MAX( LO, NINT( XLO - ( IY - YXLO ) * SLPI
     &           + .5D0 ) )
                HI = MIN( HI, NINT( XHI - ( IY - YXHI ) * SLPI
     &           - .5D0 ) )
              END IF
              DO IX = LO, HI
                IF ( ( IX .GE. 0 ) .AND. ( IX .LE. PLOT_SIZE ) )
     &           CALL LJ_PLOT_ADD( IX, IY, PL, PLOT_SIZE )
              END DO
            END DO
          END IF
        END IF

        ! Add plot symbol if on segment
        IF ( SYM_PUT ) THEN

          ! Set up symbol-related values
          NSYM_OLD = NSYM
          PSYM = 2**( SSYM + 2 )
          DPDPI = DBLE( DPI )
          SYM_INC = DPDPI / PSYM
          SYM_INCM = SYM_INC * ( 1.D0 - 2.D-7 )
          IF ( IFIL .LE. 10 ) THEN
            SYM_OFF = DPDPI * ( IFIL - 1 ) / ( 10 * PSYM/4 )
          ELSE
            SYM_OFF = ( DPDPI * ( IFIL - 11 ) + ( DPDPI / 20 ) ) /
     &       ( 10 * PSYM/4 )
          END IF
          DPIS = 300.D0 / DPDPI

          ! Check x-increment symbol positions
          LO = INT( ( X1 - SYM_OFF ) / SYM_INC )
          HI = INT( ( X2 - SYM_OFF ) / SYM_INC + 1 )
          INC = SIGN( 1, HI-LO )
          DO IS = LO, HI, INC
            XS = SYM_INC * IS + SYM_OFF
            IF ( ( XLO .LE. XS ) .AND. ( XS .LE. XHI ) ) THEN
              IF ( X1 .NE. X2 ) THEN
                SLP = ( Y2 - Y1 ) / ( X2 - X1 )
                YS = Y1 + ( XS - X1 ) * SLP
              ELSE
                YS = ( Y1 + Y2 ) / 2.D0
              END IF
              XDA = ABS( XS - XLS )
              YDA = ABS( YS - YLS )
              IF ( ( XDA .GE. 4 * SYM_INCM ) .OR.
     &         ( ( YDA .GE. 4 * SYM_INCM ) .AND.
     &         ( XDA .GE. 2 * SYM_INCM ) ) .OR.
     &         ( ( YDA .GE. 8 * SYM_INCM ) .AND.
     &         ( XDA .GE. SYM_INCM ) ) ) THEN
                IYS = NINT( YS )
                IF ( ( IYS .GE. 0 ) .AND. ( IYS .LE. PLOT_SIZE ) ) THEN
                  WRITE( PX, '(I4)', IOSTAT=IOS ) NINT( XS * DPIS )
                  PX = LJUST( PX )
                  WRITE( PY, '(I4)', IOSTAT=IOS ) NINT( YS * DPIS )
                  PY = LJUST( PY )
                  WRITE( PRN_UNIT ) 'PA', PX(:L_TRIM(PX)), ','
                  WRITE( PRN_UNIT ) PY(:L_TRIM(PY)), SYM(:LSYM)
                  NSYM = NSYM + 1
                  XLS = XS
                  YLS = YS
                END IF
              END IF
            END IF
          END DO

          ! Check y-increment symbol positions if none applied yet
          IF ( ( NSYM .EQ. NSYM_OLD ) .AND.
     &     ( MAX( ABS( Y1 - YLS ), ABS( Y2 - YLS ) ) .GE.
     &     8 * SYM_INCM ) ) THEN ! Use y-increment symbol positions
            LO = INT( ( Y1 - SYM_OFF ) / SYM_INC )
            HI = INT( ( Y2 - SYM_OFF ) / SYM_INC + 1 )
            INC = SIGN( 1, HI-LO )
            DO IS = LO, HI, INC
              YS = SYM_INC * IS + SYM_OFF
              IF ( ( YLO .LE. YS ) .AND. ( YS .LE. YHI ) ) THEN
                IF ( Y1 .NE. Y2 ) THEN
                  SLP = ( X2 - X1 ) / ( Y2 - Y1 )
                  XS = X1 + ( YS - Y1 ) * SLP
                ELSE
                  SLP = 1.D38
                  XS = ( X1 + X2 ) / 2.D0
                END IF
                XDA = ABS( XS - XLS )
                YDA = ABS( YS - YLS )
                IF ( ( YDA .GE. 8 * SYM_INC ) .AND.
     &           ( XDA .GE. SYM_INCM ) ) THEN
                  IXS = NINT( XS )
                  IF ( ( IXS .GE. 0 ) .AND.
     &             ( IXS .LE. PLOT_SIZE ) ) THEN
                    WRITE( PX, '(I4)', IOSTAT=IOS ) NINT( XS * DPIS )
                    PX = LJUST( PX )
                    WRITE( PY, '(I4)', IOSTAT=IOS ) NINT( YS * DPIS )
                    PY = LJUST( PY )
                    WRITE( PRN_UNIT ) 'PA', PX(:L_TRIM(PX)), ','
                    WRITE( PRN_UNIT ) PY(:L_TRIM(PY)), SYM(:LSYM)
                    NSYM = NSYM + 1
                    XLS = XS
                    YLS = YS
                  END IF
                END IF
              END IF
            END DO
          END IF

        END IF

      END IF

      RETURN
      END



      SUBROUTINE LJ_PLOT_ADD( IX, IY, PL, PLOT_SIZE )

!***********************************************************************
!* Adds a Point to the Plot Raster Array
!***********************************************************************


      ! Arguments ______________________________________________________

      INTEGER IX ! X-axis raster point value

      INTEGER IY ! Y-axis raster point value

      CHARACTER PL(375,3000) ! Plot raster matrix

      INTEGER PLOT_SIZE ! Plot size in laser dots


      ! Variables ______________________________________________________

      INTEGER IXB, PBIT, PLI


      ! Add the point if it falls in interior of plot box
      IF ( ( IX .GT. 0 ) .AND. ( IX .LT. PLOT_SIZE ) .AND.
     & ( IY .GT. 0 ) .AND. ( IY .LT. PLOT_SIZE ) ) THEN
        ! Point falls inside plot box
        IXB = IX / 8 + 1
        PBIT = 2**( 7 - MOD( IX, 8 ) )
        PLI = ICHAR( PL(IXB,IY) )
        IF ( MOD( PLI / PBIT, 2 ) .EQ. 0 ) THEN
          PLI = MIN( PLI + PBIT, 255 )
          PL(IXB,IY) = CHAR( PLI )
        END IF
      END IF

      RETURN
      END



      SUBROUTINE LJ_PLOT_OUT( PL, PLOT_SIZE, ROW_SIZE, PRN_UNIT )

!***********************************************************************
!* Outputs the Plot Raster Array
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER PL(375,3000) ! Plot raster matrix

      INTEGER PLOT_SIZE ! Plot size in laser dots

      INTEGER ROW_SIZE ! Plot row length in characters

      INTEGER PRN_UNIT ! Printer logical unit


      ! Variables ______________________________________________________

      INTEGER J, I, IOS, I1, I2, LROW, CMD_BYTE, LAST, OFFSET

      CHARACTER PLROW(575), LROW_C*3, ESC, FF

      PARAMETER ( ESC = CHAR(27), FF = CHAR(12) )


      ! Functions ______________________________________________________

      CHARACTER LJUST*3

      EXTERNAL LJUST


      ! Turn off HP-GL/2 / Move to the upper left corner of the plot
      WRITE( PRN_UNIT ) ';', ESC, '%0A', ESC, '*p600x1301Y'

      ! Start graphics command and set delta row compression mode
      WRITE( PRN_UNIT ) ESC, '*r1A', ESC, '*b3M'

      ! Output each dot row from the top down
      DO J = PLOT_SIZE-1, 1, -1

        I1 = 1
        LAST = 0
        LROW = 0
        DO WHILE ( I1 .LE. ROW_SIZE )

          ! Scan for changed bytes
          DO WHILE ( ( I1 .LE. ROW_SIZE ) .AND.
     &     ( PL(I1,J) .EQ. PL(I1,J+1) ) ) ! Skip same bytes
            I1 = I1 + 1
          END DO
          I2 = I1
          DO WHILE ( ( I2 .LT. ROW_SIZE ) .AND.
     &     ( I2 .LT. I1+7 ) .AND.
     &     ( PL(I2+1,J) .NE. PL(I2+1,J+1) ) ) ! Different bytes
            I2 = I2 + 1
          END DO

          ! Set up delta row commands
          IF ( I1 .LE. ROW_SIZE ) THEN ! Changed bytes to output
            OFFSET = I1 - ( LAST + 1 )
            CMD_BYTE = ( I2 - I1 ) * (2**5) + MIN( OFFSET, 31 )
            LROW = LROW + 1
            PLROW(LROW) = CHAR( CMD_BYTE ) ! Command byte
            IF ( OFFSET .GE. 31 ) THEN ! More offset bytes
              OFFSET = OFFSET - 31
              DO WHILE ( OFFSET .GE. 0 )
                LROW = LROW + 1
                PLROW(LROW) = CHAR( MIN( OFFSET, 255 ) ) ! Offset
                OFFSET = OFFSET - 255
              END DO
            END IF
            DO  I = I1, I2
              LROW = LROW + 1
              PLROW(LROW) = PL(I,J)
            END DO
            LAST = I2
            I1 = I2 + 1
          END IF

        END DO

        ! Output graphics row
        WRITE( LROW_C, '(I3)', IOSTAT=IOS ) LROW
        LROW_C = LJUST( LROW_C )
        WRITE( PRN_UNIT ) ESC, '*b', LROW_C(:LEN_TRIM(LROW_C)), 'W',
     &   ( PLROW(I), I = 1, LROW )

      END DO

      ! End graphics command
      WRITE( PRN_UNIT ) ESC, '*rB'

      ! Eject the plot
      WRITE( PRN_UNIT ) FF


      RETURN
      END



      SUBROUTINE LJ_PLOT_RESET( PRN_UNIT )

!***********************************************************************
!* Outputs the Plot Raster Array
!***********************************************************************


      ! Arguments ______________________________________________________

      INTEGER PRN_UNIT ! Printer logical unit


      ! Variables ______________________________________________________

      CHARACTER ESC

      PARAMETER ( ESC = CHAR(27) )


      ! Reset/eject
      WRITE( PRN_UNIT ) ESC, '&l1T', ESC, 'E', ESC, '%-12345X'

      RETURN
      END



      CHARACTER*15 FUNCTION NUM_REV( Z, ZINC )

!***********************************************************************
!* Reverse Formats Numeric String
!***********************************************************************


      ! Arguments ______________________________________________________

      DOUBLE PRECISION Z ! Value to reverse format

      DOUBLE PRECISION ZINC ! Axis increment


      ! Variables ______________________________________________________

      INTEGER I

      CHARACTER*15 NUM, NUM_TMP


      ! Functions ______________________________________________________

      CHARACTER*15 NUM_FMT


      ! Reverse format the numeric string
      NUM = NUM_FMT( Z, ZINC )
      DO I = 1, 15
        NUM_TMP(I:I) = NUM(16-I:16-I)
      END DO
      NUM_REV = NUM_TMP

      RETURN
      END



      CHARACTER*15 FUNCTION NUM_FMT( VAL, MAG )

!***********************************************************************
!* Formats a Number for Output in a 15 Character String
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      DOUBLE PRECISION VAL ! Value

      DOUBLE PRECISION MAG ! Magnitude for zero-trimming check


      ! Variables ______________________________________________________

      LOGICAL DONE

      INTEGER L10, NLD, I, INL, IOS

      DOUBLE PRECISION AVAL, VALLG

      CHARACTER NUM*15, NUM2*15, FFMT*7, C


      ! Check for value=zero case
      IF ( ABS( VAL ) .LT. 1.D-8 * MAG ) THEN
        NUM_FMT = ' '
        NUM_FMT(15:15) = '0'
        RETURN
      END IF

      ! Find base 10 position of leading digit
      AVAL = ABS( VAL )
      VALLG = LOG10( AVAL )
      L10 = INT( ABS( VALLG ) + 1 )
      NLD = INT( VALLG + L10 ) - L10

      ! Write number in appropriate format
      IF ( ( NLD .GT. -4 ) .AND. ( NLD .LT. 7 ) ) THEN ! Use F fmt
        FFMT = '(F15._)'
        WRITE( FFMT(6:6), '(I1)' ) MAX( 6-NLD, 0 )
        WRITE( NUM, FFMT, IOSTAT=IOS ) VAL
        DO WHILE ( NUM(15:15) .EQ. '0' )
          NUM2 = ' '//NUM(1:14)
          NUM = NUM2
        END DO
        IF ( NUM(15:15) .EQ. '.' ) THEN
          NUM2 = ' '//NUM(1:14)
          NUM = NUM2
        END IF
        IF ( INDEX( NUM, '.' ) .NE. 0 ) THEN ! Remove leading zero
          DONE = .FALSE.
          I = 1
          DO WHILE ( ( .NOT. DONE ) .AND. ( I .LT. 15 ) )
            C = NUM(I:I)
            IF ( C .EQ. '.' ) THEN
              DONE = .TRUE.
            ELSE IF ( C .EQ. '0' ) THEN
              NUM2 = ' '//NUM(:I-1)//NUM(I+1:)
              NUM = NUM2
              DONE = .TRUE.
            ELSE IF ( ANY_CHARS( C, '123456789' ) ) THEN
              DONE = .TRUE.
            END IF
            I = I + 1
          END DO
        END IF
      ELSE ! Use E formatting
        WRITE( NUM, '(1PE15.5)', IOSTAT=IOS ) VAL
        IF ( NUM(14:14) .EQ. '0' ) THEN
          NUM2 = ' '//NUM(1:13)//NUM(15:15)
          NUM = NUM2
        END IF
        INL = INDEX( NUM, 'E' ) - 1
        DO WHILE ( NUM(INL:INL) .EQ. '0' )
          NUM2 = ' '//NUM(1:INL-1)//NUM(INL+1:)
          NUM = NUM2
        END DO
        INL = INDEX( NUM, 'E' ) + 1
        IF ( NUM(INL:INL) .EQ. '+' ) THEN
          NUM2 = NUM(1:INL-1)//NUM(INL+1:)
          NUM = NUM2
        END IF
        DO WHILE ( NUM(INL:INL) .EQ. '0' )
          NUM2 = NUM(1:INL-1)//NUM(INL+1:)
          NUM = NUM2
        END DO
      END IF

      NUM_FMT = NUM

      RETURN
      END



      SUBROUTINE PLOT_FILE_SETUP( IFIL, FNAME,
     & FTYP, FINFO, DIMSYS_P,
     & XTYPE, XTI, XUNIT, XUI, XL, XLM, XU,
     & YTYPE, YTI, YUNIT, YUI, YL, YLM, YU,
     & U, REPORT, IOS )

!***********************************************************************
!* Reads the Specified File and Checks Consistency
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'
      INCLUDE 'file.fi'


      ! Arguments ______________________________________________________

      INTEGER IFIL ! Index of file

      CHARACTER*(*) FNAME ! File name

      CHARACTER*(*) FTYP ! File type

      CHARACTER*(*) FINFO ! File info

      CHARACTER*(*) DIMSYS_P ! Plot dimensional system

      CHARACTER*(*) XTYPE ! X-axis data type

      LOGICAL XTI ! X-axis type inconsistent flag

      CHARACTER*(*) XUNIT ! X-axis data units

      LOGICAL XUI ! X-axis units inconsistent flag

      REAL XL ! X-axis min value

      REAL XLM ! X-axis min non-zero absolute magnitude

      REAL XU ! X-axis max value

      CHARACTER*(*) YTYPE ! Y-axis data type

      LOGICAL YTI ! Y-axis type inconsistent flag

      CHARACTER*(*) YUNIT ! Y-axis data units

      LOGICAL YUI ! Y-axis units inconsistent flag

      REAL YL ! Y-axis min value

      REAL YLM ! Y-axis min non-zero absolute magnitude

      REAL YU ! Y-axis max value

      RECORD /UDS/  U ! UDS structure

      RECORD /FILE/  REPORT ! Report file structure

      INTEGER IOS ! Status value


      ! Variables ______________________________________________________

      INTEGER I

      REAL XI, YI

      CHARACTER FILE_NAME*256


      ! Initialize labels and label flags if first file
      FTYP = ' '
      FINFO = ' '
      IF ( IFIL .EQ. 1 ) THEN
        XTYPE = ' '
        XTI = .FALSE.
        XUNIT = ' '
        XUI = .FALSE.
        YTYPE = ' '
        YTI = .FALSE.
        YUNIT = ' '
        YUI = .FALSE.
        DIMSYS_P = ' '
      END IF

      ! Read the file
      FILE_NAME = FNAME
      CALL REP_COLS( REPORT )
      CALL MSG_WRITE( '  Reading: '//FILE_NAME, REPORT )
      CALL PLOT_FILE_READ( IFIL, FNAME, FTYP, DIMSYS_P, U, IOS )
      IF ( IOS .NE. 0 ) THEN ! UDS read error
        CALL MSG_WRITE( '*** ERROR - UDS file read failed', REPORT )
        RETURN
      ELSE
        CALL REP_ENTRY( U, REPORT )
      END IF

      ! Set up file info
      IF ( FTYP .EQ. 'UDS' ) THEN ! UDS File

        CALL PLOT_UDS_INFO( U, FINFO )

        ! Assign and check labels

        IF ( XUNIT .EQ. ' ' ) THEN
          XUNIT = U.XUNITS
        ELSE IF ( ( .NOT. BLANK( U.XUNITS ) ) .AND.
     &   ( U.XUNITS .NE. XUNIT ) ) THEN
          WRITE( *, * ) '*** Inconsistent X-axis units: ',
     &     U.XUNITS(:L_TRIM(U.XUNITS)),'  (vs. ',
     &     XUNIT(:L_TRIM(XUNIT)),')'
          XUI = .TRUE.
        END IF

        IF ( XTYPE .EQ. ' ' ) THEN
          XTYPE = U.XTYP
        ELSE IF ( ( .NOT. BLANK( U.XTYP ) ) .AND.
     &   ( U.XTYP .NE. XTYPE ) ) THEN
          WRITE( *, * ) '*** Inconsistent X-axis type: ',
     &     U.XTYP(:L_TRIM(U.XTYP)),'  (vs. ',
     &     XTYPE(:L_TRIM(XTYPE)),')'
          XTI = .TRUE.
        END IF

        IF ( YUNIT .EQ. ' ' ) THEN
          YUNIT = U.YUNITS
        ELSE IF ( ( .NOT. BLANK( U.YUNITS ) ) .AND.
     &   ( U.YUNITS .NE. YUNIT ) ) THEN
          WRITE( *, * ) '*** Inconsistent Y-axis units: ',
     &     U.YUNITS(:L_TRIM(U.YUNITS)),'  (vs. ',
     &     YUNIT(:L_TRIM(YUNIT)),')'
          YUI = .TRUE.
        END IF

        IF ( YTYPE .EQ. ' ' ) THEN
          YTYPE = U.YTYP
        ELSE IF ( ( .NOT. BLANK( U.YTYP ) ) .AND.
     &   ( U.YTYP .NE. YTYPE ) ) THEN
          WRITE( *, * ) '*** Inconsistent Y-axis type: ',
     &     U.YTYP(:L_TRIM(U.YTYP)),'  (vs. ',
     &     YTYPE(:L_TRIM(YTYPE)),')'
          YTI = .TRUE.
        END IF
      ELSE IF ( FTYP .EQ. 'ASCII' ) THEN
        ! No info to set up
      END IF

      ! Initialize axis ranges and dimensional system if first plot
      IF ( IFIL .EQ. 1 ) THEN
        XL = U.X(U.NFP)
        XLM = 0.
        XU = XL
        YL = U.Y(U.NFP)
        YLM = 0.
        YU = YL
        DIMSYS_P = U.DIMSYS
      END IF

      ! Update plot axis ranges
      DO I = U.NFP, U.NLP
        XI = U.X(I)
        XL = MIN( XL, XI )
        IF ( XI .NE. 0. ) THEN
          IF ( XLM .NE. 0. ) THEN
            XLM = MIN( XLM, ABS( XI ) )
          ELSE
            XLM = ABS( XI )
          END IF
        END IF
        XU = MAX( XU, XI )
        YI = U.Y(I)
        YL = MIN( YL, YI )
        IF ( YI .NE. 0. ) THEN
          IF ( YLM .NE. 0. ) THEN
            YLM = MIN( YLM, ABS( YI ) )
          ELSE
            YLM = ABS( YI )
          END IF
        END IF
        YU = MAX( YU, YI )
      END DO

      RETURN
      END



      SUBROUTINE PLOT_FILE_READ( IFIL, FNAME, FTYP, DIMSYS_P, U, IOS )

!***********************************************************************
!* Reads the Specified File
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      INTEGER IFIL ! Index of file

      CHARACTER*(*) FNAME ! File name

      CHARACTER*(*) FTYP ! File type

      CHARACTER*(*) DIMSYS_P ! Plot dimensional system

      RECORD /UDS/  U ! UDS structure

      INTEGER IOS ! Status value


      ! Variables ______________________________________________________

      RECORD /UDS_CONTROL/  C

      INTEGER ASCII_UNIT, I, L, LF, NCOL, NNCOL

      DOUBLE PRECISION DDREC(2)

      CHARACTER DATA_REC*256


      ! Open the file and read the curve
      IF ( ( FTYP .EQ. 'UDS' ) .OR. ( BLANK( FTYP ) ) ) THEN ! UDS
        CALL UDS_CONTROL_INIT( C )
        C.DIMSYS = DIMSYS_P
        C.NO_MSG = .TRUE.
        C.FILL_X = .TRUE.
        CALL UDS_READ( FNAME, U, C, IOS )
        IF ( IOS .EQ. 0 ) FTYP = 'UDS'
      END IF
      IF ( ( FTYP .EQ. 'ASCII' ) .OR. ( BLANK( FTYP ) ) ) THEN ! ASCII

        ! Open the file
        CALL OPEN_FS( ASCII_UNIT, FNAME, 'R', IOS )
        IF ( IOS .NE. 0 ) THEN
          WRITE( *, * ) '*** File open failed'
          IFIL = IFIL - 1
          RETURN
        END IF

        ! Find first numerical record
        NNCOL = 0
        DO WHILE ( NNCOL .EQ. 0 )
          READ( ASCII_UNIT, '(A)', IOSTAT=IOS ) DATA_REC
          IF ( IOS .NE. 0 ) THEN ! File end or error
            WRITE( *, * ) '*** Unacceptable ASCII file - ',
     &       'No records start with numeric data'
            IFIL = IFIL - 1
            RETURN
          END IF
          L = 1
          NCOL = 0
          CALL GET_ASCII_COL( DATA_REC, L, NCOL, NNCOL, DDREC )
        END DO

        ! Determine file column structure
        L = 1
        LF = LEN_TRIM( DATA_REC )
        NCOL = 0
        NNCOL = 0
        DO WHILE ( ( L .LE. LF ) .AND. ( NNCOL .LT. 2 ) )
          CALL GET_ASCII_COL( DATA_REC, L, NCOL, NNCOL, DDREC )
        END DO
        IF ( NNCOL .LT. 2 ) THEN
          WRITE( *, * ) '*** Unacceptable ASCII file - ',
     &     'Fewer than 2 numeric columns'
          IFIL = IFIL - 1
          IOS = -3
          RETURN
        END IF

        ! Read the data
        BACKSPACE( ASCII_UNIT )
        I = NFP_L
        CALL UDS_INIT( U )
        DO WHILE ( I .LT. NLP_U )
          READ( ASCII_UNIT, *, ERR=101, END=101, IOSTAT=IOS )
     &     U.X(I), U.Y(I)
          IF ( IOS .EQ. 0 ) I = I + 1
        END DO
  101   CLOSE( ASCII_UNIT )
        IF ( ( IOS .EQ. 0 ) .OR. ( IOS .EQ. -1 ) ) THEN ! OK as ASCII
          FTYP = 'ASCII'
          U.NFP = NFP_L
          U.NLP = I - 1
          IOS = 0
        ELSE ! Read error occurred
          WRITE( *, * ) '*** Unacceptable ASCII file - Read error'
          IFIL = IFIL - 1
          IOS = -3
          RETURN
        END IF

      END IF

      RETURN
      END



      SUBROUTINE GET_ASCII_COL( DATA_REC, L, NCOL, NNCOL, DDREC )

!***********************************************************************
!* Extracts Next Data Column from String
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) DATA_REC ! Data record

      INTEGER L ! Record column number

      INTEGER NCOL ! Number of columns

      INTEGER NNCOL ! Number of numeric columns

      DOUBLE PRECISION DDREC(2) ! Numbers extracted from record


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
          IF ( NNCOL .LE. 2 ) THEN
            DDREC( NNCOL ) = DD
          ELSE
            WRITE( *, * ) '*** Numeric columns past 2 ignored'
          END IF
        END IF

        ! Advance column index
        L = IE + 1

      ELSE ! Past end of record

        ! Set column index past end of data record
        L = I

      END IF

      RETURN
      END



      SUBROUTINE PLOT_UDS_INFO( U, FINFO )

!***********************************************************************
!* Assigns UDS File Info String for Display
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      RECORD /UDS/  U ! UDS structure

      CHARACTER*(*) FINFO ! File info


      ! Variables ______________________________________________________

      INTEGER IOS

      CHARACTER CURNAM_O*12, FREQ*8


      ! Functions ______________________________________________________

      CHARACTER LJUST*12, STR_APPEND*100

      EXTERNAL LJUST, STR_APPEND


      ! Set up file info string
      FINFO = STR_APPEND( FINFO, U.MODEL )
      IF ( .NOT. BLANK( U.OCCTYP ) )
     & FINFO = STR_APPEND( FINFO, U.OCCTYP//' Loc. '//U.SENLOC )
      FINFO = STR_APPEND( FINFO, U.SENATT(:L_TRIM(U.SENATT)) )
      CURNAM_O = LJUST( U.CURNAM )
      FINFO = STR_APPEND( FINFO,
     & '('//CURNAM_O(:L_TRIM(CURNAM_O))//')' )
      FINFO = STR_APPEND( FINFO, U.AXIS//' Axis' )
      IF ( U.FCUT .GT. 0. ) THEN
        WRITE( FREQ, '(F8.2)', IOSTAT=IOS ) U.FCUT
        IF ( FREQ(8:8) .EQ. '0' ) THEN
          FREQ(8:8) = ' '
          IF ( FREQ(7:7) .EQ. '0' ) FREQ(6:7) = ' '
        END IF
        FREQ = LJUST( FREQ )
        FINFO = STR_APPEND( FINFO, FREQ(:L_TRIM(FREQ))//' Hz' )
      END IF
      FINFO = STR_APPEND( FINFO, U.STATUS )

      RETURN
      END



      SUBROUTINE PLOT_SETUP( NFIL, FNAME, FTYP, DIMSYS_P,
     & XTYPE, XTI, XUNIT, XUI, XL, XLM, XU, XTRAN, XUL, XLP, XUP,
     & YTYPE, YTI, YUNIT, YUI, YL, YLM, YU, YTRAN, YUL, YLP, YUP, U )

!***********************************************************************
!* Set Up the Plot Info
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'


      ! Arguments ______________________________________________________

      INTEGER NFIL ! Number of files

      CHARACTER*(*) FNAME(*) ! File name

      CHARACTER*(*) FTYP(*) ! File type

      CHARACTER*(*) DIMSYS_P ! Plot dimensional system

      CHARACTER*(*) XTYPE ! X-axis data type

      LOGICAL XTI ! X-axis type inconsistent flag

      CHARACTER*(*) XUNIT ! X-axis data units

      LOGICAL XUI ! X-axis units inconsistent flag

      REAL XL ! X-axis min value

      REAL XLM ! X-axis min non-zero absolute magnitude

      REAL XU ! X-axis max value

      CHARACTER*(*) XTRAN ! X-axis transform

      LOGICAL XUL ! X-axis unitless flag

      DOUBLE PRECISION XLP ! Lower X-axis value of plot box

      DOUBLE PRECISION XUP ! Upper X-axis value of plot box

      CHARACTER*(*) YTYPE ! Y-axis data type

      LOGICAL YTI ! Y-axis type inconsistent flag

      CHARACTER*(*) YUNIT ! Y-axis data units

      LOGICAL YUI ! Y-axis units inconsistent flag

      REAL YL ! Y-axis min value

      REAL YLM ! Y-axis min non-zero absolute magnitude

      REAL YU ! Y-axis max value

      CHARACTER*(*) YTRAN ! Y-axis transform

      LOGICAL YUL ! Y-axis unitless flag

      DOUBLE PRECISION YLP ! Lower Y-axis value of plot box

      DOUBLE PRECISION YUP ! Upper Y-axis value of plot box

      RECORD /UDS/  U ! UDS structure


      ! Set up default unitless mode(s)
      IF ( XUI ) THEN
        XUL = .TRUE.
        WRITE( *, * ) '*** Unitless mode set for X-axis'
      ELSE
        XUL = .FALSE.
      END IF
      IF ( YUI ) THEN
        YUL = .TRUE.
        WRITE( *, * ) '*** Unitless mode set for Y-axis'
      ELSE
        YUL = .FALSE.
      END IF

      ! Assign axis labels
      CALL PLOT_AXIS_LBL( XTYPE, XTI, XUNIT, XUI,
     & YTYPE, YTI, YUNIT, YUI )

      ! Set up plot scaling
      CALL PLOT_SCALE( NFIL, FNAME, FTYP, DIMSYS_P,
     & XTYPE, XTRAN, XUL, XL, XLM, XU, XLP, XUP,
     & YTYPE, YTRAN, YUL, YL, YLM, YU, YLP, YUP, U )

      RETURN
      END



      CHARACTER*(*) FUNCTION PLOT_TRANSFORM( AXIS, TRAN )

!***********************************************************************
!* Checks, Reports, and Assigns Axis Transform Flag
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER AXIS ! Axis flag

      CHARACTER*(*) TRAN ! Transform flag


      ! Check and assign TRAN
      IF ( TRAN .EQ. 'LOG' ) THEN
        PLOT_TRANSFORM = TRAN
        WRITE( *, * ) '*** ', AXIS, '-axis set to ',
     &   TRAN(:L_TRIM(TRAN)), ' transform mode'
      ELSE
        PLOT_TRANSFORM = ' '
        IF ( .NOT. BLANK( TRAN ) ) THEN
          WRITE( *, * ) '*** WARNING - Transform not recognized - ',
     &     'Legal values are: LOG'
        END IF
      END IF

      RETURN
      END



      SUBROUTINE PLOT_AXIS_LBL( XTYPE, XTI, XUNIT, XUI,
     & YTYPE, YTI, YUNIT, YUI )

!***********************************************************************
!* Queries for Blank Axis Labels
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) XTYPE ! X-axis data type

      LOGICAL XTI ! X-axis type inconsistent flag

      CHARACTER*(*) XUNIT ! X-axis data units

      LOGICAL XUI ! X-axis units inconsistent flag

      CHARACTER*(*) YTYPE ! Y-axis data type

      LOGICAL YTI ! Y-axis type inconsistent flag

      CHARACTER*(*) YUNIT ! Y-axis data units

      LOGICAL YUI ! Y-axis units inconsistent flag


      ! Variables ______________________________________________________

      INTEGER IEOF

      CHARACTER ZUNIT*20, ZTYPE*20


      ! Functions ______________________________________________________

      CHARACTER UPCASE*20

      EXTERNAL UPCASE


      ! Initializations
      WRITE( *, * )
      IEOF = 0

      ! X-axis units query
  101 IF ( ( BLANK( XUNIT ) ) .OR. ( XUI ) ) THEN
        ZUNIT = XUNIT
        WRITE( *, '(A,29X,A/'' >> '',$)' )
     &   ' X-axis units?   ( N - None )', '['//ZUNIT//']'
        READ( *, '(A)', END=201 ) XUNIT
        IF ( BLANK( XUNIT ) ) THEN
          XUNIT = ZUNIT
        ELSE IF ( UPCASE( XUNIT ) .EQ. 'N' ) THEN
          XUNIT = ' '
        END IF
        XUI = .FALSE.
        IEOF = 0
      END IF

      ! X-axis type query
      IF ( ( BLANK( XTYPE ) ) .OR. ( XTI ) ) THEN
        ZTYPE = XTYPE
        WRITE( *, '(A,30X,A/'' >> '',$)' )
     &   ' X-axis type?   ( N - None )', '['//ZTYPE//']'
        READ( *, '(A)', END=201 ) XTYPE
        IF ( BLANK( XTYPE ) ) THEN
          XTYPE = ZTYPE
        ELSE IF ( UPCASE( XTYPE ) .EQ. 'N' ) THEN
          XTYPE = ' '
        END IF
        XTI = .FALSE.
        IEOF = 0
      END IF

      ! Y-axis units query
      IF ( ( BLANK( YUNIT ) ) .OR. ( YUI ) ) THEN
        ZUNIT = YUNIT
        WRITE( *, '(A,29X,A/'' >> '',$)' )
     &   ' Y-axis units?   ( N - None )', '['//ZUNIT//']'
        READ( *, '(A)', END=201 ) YUNIT
        IF ( BLANK( YUNIT ) ) THEN
          YUNIT = ZUNIT
        ELSE IF ( UPCASE( YUNIT ) .EQ. 'N' ) THEN
          YUNIT = ' '
        END IF
        YUI = .FALSE.
        IEOF = 0
      END IF

      ! Y-axis type query
      IF ( ( BLANK( YTYPE ) ) .OR. ( YTI ) ) THEN
        ZTYPE = YTYPE
        WRITE( *, '(A,30X,A/'' >> '',$)' )
     &   ' Y-axis type?   ( N - None )', '['//ZTYPE//']'
        READ( *, '(A)', END=201 ) YTYPE
        IF ( BLANK( YTYPE ) ) THEN
          YTYPE = ZTYPE
        ELSE IF ( UPCASE( YTYPE ) .EQ. 'N' ) THEN
          YTYPE = ' '
        END IF
        YTI = .FALSE.
        IEOF = 0
      END IF

      RETURN

      ! EOF handler
  201 IEOF = IEOF + 1
      IF ( IEOF .LE. 1 ) GO TO 101
      RETURN

      END



      SUBROUTINE PLOT_SCALE( NFIL, FNAME, FTYP, DIMSYS_P,
     & XTYPE, XTRAN, XUL, XL, XLM, XU, XLP, XUP,
     & YTYPE, YTRAN, YUL, YL, YLM, YU, YLP, YUP, U )

!***********************************************************************
!* Computes the Plot Axis Scaling and Ranges
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      INTEGER NFIL ! Number of files

      CHARACTER*(*) FNAME(*) ! File name

      CHARACTER*(*) FTYP(*) ! File type

      CHARACTER*(*) DIMSYS_P ! Plot dimensional system

      CHARACTER*(*) XTYPE ! X-axis data type

      CHARACTER*(*) XTRAN ! X-axis transform

      LOGICAL XUL ! X-axis unitless flag

      REAL XL ! X-axis min value

      REAL XLM ! X-axis min non-zero absolute magnitude

      REAL XU ! X-axis max value

      DOUBLE PRECISION XLP ! Lower X-axis value of plot box

      DOUBLE PRECISION XUP ! Upper X-axis value of plot box

      CHARACTER*(*) YTYPE ! Y-axis data type

      CHARACTER*(*) YTRAN ! Y-axis transform

      LOGICAL YUL ! Y-axis unitless flag

      REAL YL ! Y-axis min value

      REAL YLM ! Y-axis min non-zero absolute magnitude

      REAL YU ! Y-axis max value

      DOUBLE PRECISION YLP ! Lower Y-axis value of plot box

      DOUBLE PRECISION YUP ! Upper Y-axis value of plot box

      RECORD /UDS/  U ! UDS structure


      ! Variables ______________________________________________________

      LOGICAL FOUND

      INTEGER NINC, I, IOS

      PARAMETER ( NINC = 5 )

      DOUBLE PRECISION XLW, XUW, YLW, YUW

      CHARACTER XBAS*3, YBAS*3, RESP,
     & XLP_C*25, XUP_C*25, YLP_C*25, YUP_C*25


      ! Set default plot axis ranges
  101 CALL PLOT_AXIS_RANGE( NINC, XTRAN, XUL, XL, XLM,
     & XU, XLP, XUP, XBAS )
      CALL PLOT_AXIS_RANGE( NINC, YTRAN, YUL, YL, YLM,
     & YU, YLP, YUP, YBAS )

      ! Prompt for range/mode set
  102 WRITE( *, '(/A,18X,A/'' >> '',$)' )
     & ' Set?   ( R - Range,  L - Log transform,  U - Unitless )',
     & '[none]'
      READ( *, '(A)', END=201 ) RESP
      CALL STR_UP( RESP )

      ! Perform range/mode requests
      IF ( RESP .EQ. 'R' ) THEN ! Set range
        IF ( ( .NOT. YUL ) .OR. ( .NOT. XUL ) )
     &   WRITE( *, * ) '   Plot range:'
        IF ( .NOT. XUL ) WRITE( *, 601, IOSTAT=IOS )
     &   XTYPE, 'X', XBAS, XLP, XBAS, XUP
        IF ( .NOT. YUL ) WRITE( *, 601, IOSTAT=IOS )
     &   YTYPE, 'Y', YBAS, YLP, YBAS, YUP
  601   FORMAT(6X,A,' (',A1,'):  ',A,G15.6,' to  ',A,G15.6)

        WRITE( *, '(/A,45X,A/'' >> '',$)' )
     &   ' Axis?   ( X,  Y,  B - Both )', '[none]'
        READ( *, '(A)', END=101 ) RESP
        CALL STR_UP( RESP )

        IF ( ( RESP .EQ. 'X' ) .AND. ( .NOT. XUL ) ) THEN ! X-axis range
  103     WRITE( *, 602 ) XTYPE, XTRAN
  602     FORMAT(/1X,A,1X,A,' lower bound?',25X,'[no change]'/
     &     ' >> ',$)
          READ( *, '(A)', END=101 ) XLP_C
          IF ( XLP_C .NE. ' ' ) READ( XLP_C, '(BN,F25.0)', ERR=103 ) XLP
  104     WRITE( *, 603 ) XTYPE, XTRAN
  603     FORMAT(/1X,A,1X,A,' upper bound?',25X,'[no change]'/
     &     ' >> ',$)
          READ( *, '(A)', END=101 ) XUP_C
          IF ( XUP_C .NE. ' ' ) READ( XUP_C, '(BN,F25.0)', ERR=104 ) XUP
          IF ( XLP .GE. XUP ) THEN
            WRITE( *, * ) '*** Null X range'
            GO TO 103
          END IF
          IF ( ( ( XLP_C .NE. ' ' ) .OR. ( XUP_C .NE. ' ' ) ) .AND.
     &     ( .NOT. YUL ) ) THEN
            FOUND = .FALSE.
            DO I = 1, NFIL
              CALL PLOT_FILE_READ( I, FNAME(I), FTYP(I), DIMSYS_P,
     &         U, IOS )
              CALL PLOT_WINDOW( FOUND, XLP, XUP, XTRAN,
     &         YLW, YUW, YTRAN, U, U.X, U.Y )
            END DO
            IF ( FOUND ) THEN
              IF ( BLANK( YTRAN ) ) THEN
                CALL PLOT_AUTO_SCALE( NINC, YLW, YUW, YLP, YUP )
              ELSE IF ( YTRAN .EQ. 'LOG' ) THEN
                CALL PLOT_AUTO_SCALE( NINC, YLW, YUW, YLP, YUP )
              END IF
            ELSE
              WRITE( *, 901 ) XTYPE(:L_TRIM(XTYPE))
  901         FORMAT(' *** Empty plot within specified ',A,' range')
              GO TO 101
            END IF
          END IF
        ELSE IF ( ( RESP .EQ. 'Y' ) .AND. ( .NOT. YUL ) )
     &   THEN ! Y-axis range
  105     WRITE( *, 602 ) YTYPE, YTRAN
          READ( *, '(A)', END=101 ) YLP_C
          IF ( YLP_C .NE. ' ' ) READ( YLP_C, '(BN,F25.0)', ERR=105 )
     &     YLP
  106     WRITE( *, 603 ) YTYPE, YTRAN
          READ( *, '(A)', END=101 ) YUP_C
          IF ( YUP_C .NE. ' ' ) READ( YUP_C, '(BN,F25.0)', ERR=106 )
     &     YUP
          IF ( YLP .GE. YUP ) THEN
            WRITE( *, * ) '*** Null Y range'
            GO TO 105
          END IF
          IF ( ( ( YLP_C .NE. ' ' ) .OR. ( YUP_C .NE. ' ' ) ) .AND.
     &     ( .NOT. XUL ) ) THEN
            FOUND = .FALSE.
            DO I = 1, NFIL
              CALL PLOT_FILE_READ( I, FNAME(I), FTYP(I), DIMSYS_P,
     &         U, IOS )
              CALL PLOT_WINDOW( FOUND, YLP, YUP, YTRAN,
     &         XLW, XUW, XTRAN, U, U.Y, U.X )
            END DO
            IF ( FOUND ) THEN
              IF ( BLANK( XTRAN ) ) THEN
                CALL PLOT_AUTO_SCALE( NINC, XLW, XUW, XLP, XUP )
              ELSE IF ( XTRAN .EQ. 'LOG' ) THEN
                CALL PLOT_AUTO_SCALE( NINC, XLW, XUW, XLP, XUP )
              END IF
            ELSE
              WRITE( *, 901 ) YTYPE(:L_TRIM(YTYPE))
              GO TO 101
            END IF
          END IF
        ELSE IF ( ( RESP .EQ. 'B' ) .AND. ( .NOT. XUL ) .AND.
     &   ( .NOT. YUL ) ) THEN ! Both
  107     WRITE( *, 602 ) XTYPE, XTRAN
          READ( *, '(A)', END=101 ) XLP_C
          IF ( XLP_C .NE. ' ' ) READ( XLP_C, '(BN,F25.0)', ERR=107 ) XLP
  108     WRITE( *, 603 ) XTYPE, XTRAN
          READ( *, '(A)', END=101 ) XUP_C
          IF ( XUP_C .NE. ' ' ) READ( XUP_C, '(BN,F25.0)', ERR=108 ) XUP
  109     WRITE( *, 602 ) YTYPE, YTRAN
          READ( *, '(A)', END=101 ) YLP_C
          IF ( YLP_C .NE. ' ' ) READ( YLP_C, '(BN,F25.0)', ERR=109 ) YLP
  110     WRITE( *, 603 ) YTYPE, YTRAN
          READ( *, '(A)', END=101 ) YUP_C
          IF ( YUP_C .NE. ' ' ) READ( YUP_C, '(BN,F25.0)', ERR=110 ) YUP
          IF ( ( XLP .GE. XUP ) .OR. ( YLP .GE. YUP ) ) THEN
            WRITE( *, * ) '*** Null plot range'
            GO TO 107
          END IF
        END IF
        GO TO 102
      ELSE IF ( RESP .EQ. 'L' ) THEN ! Set log transform mode(s)
        ! X-axis transform
        WRITE( *, '(/A,45X,A/'' >> '',$)' )
     &   ' Axis?   ( X,  Y,  B - Both )', '[none]'
        READ( *, '(A)', END=101 ) RESP
        CALL STR_UP( RESP )

        IF ( RESP .EQ. 'X' ) THEN ! Set X-axis to LOG mode
          XTRAN = 'LOG'
          CALL PLOT_AXIS_RANGE( NINC, XTRAN, XUL, XL, XLM,
     &     XU, XLP, XUP, XBAS )
          IF ( .NOT. BLANK( YTRAN ) ) THEN
            YTRAN = ' '
            CALL PLOT_AXIS_RANGE( NINC, YTRAN, YUL, YL, YLM,
     &       YU, YLP, YUP, YBAS )
          END IF
        ELSE IF ( RESP .EQ. 'Y' ) THEN ! Set Y-axis to LOG mode
          YTRAN = 'LOG'
          CALL PLOT_AXIS_RANGE( NINC, YTRAN, YUL, YL, YLM,
     &     YU, YLP, YUP, YBAS )
          IF ( .NOT. BLANK( XTRAN ) ) THEN
            XTRAN = ' '
            CALL PLOT_AXIS_RANGE( NINC, XTRAN, XUL, XL, XLM,
     &       XU, XLP, XUP, XBAS )
          END IF
        ELSE IF ( RESP .EQ. 'B' ) THEN ! Set both axes to LOG mode
          XTRAN = 'LOG'
          CALL PLOT_AXIS_RANGE( NINC, XTRAN, XUL, XL, XLM,
     &     XU, XLP, XUP, XBAS )
          YTRAN = 'LOG'
          CALL PLOT_AXIS_RANGE( NINC, YTRAN, YUL, YL, YLM,
     &     YU, YLP, YUP, YBAS )
        ELSE IF ( RESP .EQ. ' ' ) THEN ! Turn off all LOG modes
          IF ( .NOT. BLANK( XTRAN ) ) THEN
            XTRAN = ' '
            CALL PLOT_AXIS_RANGE( NINC, XTRAN, XUL, XL, XLM,
     &       XU, XLP, XUP, XBAS )
          END IF
          IF ( .NOT. BLANK( YTRAN ) ) THEN
            YTRAN = ' '
            CALL PLOT_AXIS_RANGE( NINC, YTRAN, YUL, YL, YLM,
     &       YU, YLP, YUP, YBAS )
          END IF
        END IF

        GO TO 102
      ELSE IF ( RESP .EQ. 'U' ) THEN ! Set unitless mode(s)
        WRITE( *, '(/A,45X,A/'' >> '',$)' )
     &   ' Axis?   ( X,  Y,  B - Both )', '[none]'
        READ( *, '(A)', END=101 ) RESP
        CALL STR_UP( RESP )

        IF ( RESP .EQ. 'X' ) THEN
          XUL = .TRUE.
          CALL PLOT_AXIS_RANGE( NINC, XTRAN, XUL, XL, XLM,
     &     XU, XLP, XUP, XBAS )
          IF ( YUL ) THEN
            YUL = .FALSE.
            CALL PLOT_AXIS_RANGE( NINC, YTRAN, YUL, YL, YLM,
     &       YU, YLP, YUP, YBAS )
          END IF
        ELSE IF ( RESP .EQ. 'Y' ) THEN
          YUL = .TRUE.
          CALL PLOT_AXIS_RANGE( NINC, YTRAN, YUL, YL, YLM,
     &     YU, YLP, YUP, YBAS )
          IF ( XUL ) THEN
            XUL = .FALSE.
            CALL PLOT_AXIS_RANGE( NINC, XTRAN, XUL, XL, XLM,
     &       XU, XLP, XUP, XBAS )
          END IF
        ELSE IF ( RESP .EQ. 'B' ) THEN
          XUL = .TRUE.
          YUL = .TRUE.
          CALL PLOT_AXIS_RANGE( NINC, XTRAN, XUL, XL, XLM,
     &     XU, XLP, XUP, XBAS )
          CALL PLOT_AXIS_RANGE( NINC, YTRAN, YUL, YL, YLM,
     &     YU, YLP, YUP, YBAS )
        ELSE IF ( RESP .EQ. ' ' ) THEN
          IF ( XUL ) THEN
            XUL = .FALSE.
            CALL PLOT_AXIS_RANGE( NINC, XTRAN, XUL, XL, XLM,
     &       XU, XLP, XUP, XBAS )
          END IF
          IF ( YUL ) THEN
            YUL = .FALSE.
            CALL PLOT_AXIS_RANGE( NINC, YTRAN, YUL, YL, YLM,
     &       YU, YLP, YUP, YBAS )
          END IF
        END IF

        GO TO 102
      END IF

  201 RETURN
      END



      SUBROUTINE PLOT_AXIS_RANGE( NINC, ZTRAN, ZUL, ZL, ZLM,
     & ZU, ZLP, ZUP, ZBAS )

!***********************************************************************
!* Sets Plot Range Based on Value Range
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      INTEGER NINC ! Number of plot major tic increments

      CHARACTER*(*) ZTRAN ! Z-axis transform flag

      LOGICAL ZUL ! Z-axis unitless mode flag

      REAL ZL ! Z-axis lower value

      REAL ZLM ! Z-axis min non-zero absolute magnitude

      REAL ZU ! Z-axis upper value

      DOUBLE PRECISION ZLP ! Z-axis lower plot value

      DOUBLE PRECISION ZUP ! Z-axis upper plot value

      CHARACTER*3 ZBAS ! Z-axis log display base string, if any


      ! Variables ______________________________________________________

      DOUBLE PRECISION ZLW, ZUW


      ! Set plot range
      IF ( ZUL ) THEN
        ZLP = 0.D0
        ZUP = 1.D0
      ELSE IF ( BLANK( ZTRAN ) ) THEN
        CALL PLOT_AUTO_SCALE( NINC, DBLE( ZL ), DBLE( ZU ), ZLP, ZUP )
        ZBAS = ' '
      ELSE IF ( ZTRAN .EQ. 'LOG' ) THEN
        IF ( ( ZL .EQ. 0. ) .AND. ( ZU .EQ. 0. ) ) THEN
          WRITE( *, * ) '*** Empty Plot - Z values are all zero'
          ZUP = 0.D0
          ZLP = -5.D0
        ELSE
          ZUW = LOG10( MAX( ABS( ZL ), ABS( ZU ) ) )
          ZLW = LOG10( ZLM )
          CALL PLOT_AUTO_SCALE( NINC, ZLW, ZUW, ZLP, ZUP )
        END IF
        ZBAS = '10^'
      END IF

      RETURN
      END



      SUBROUTINE PLOT_AUTO_SCALE( NINC, ZL, ZU, ZLP, ZUP )

!***********************************************************************
!* Computes Automatic Rounded Axis Ranges
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      INTEGER NINC ! Number of plot major tic increments

      DOUBLE PRECISION ZL ! Z-axis lower value

      DOUBLE PRECISION ZU ! Z-axis upper value

      DOUBLE PRECISION ZLP ! Z-axis lower plot value

      DOUBLE PRECISION ZUP ! Z-axis upper plot value


      ! Variables ______________________________________________________

      INTEGER NINC2, NZERO, NDIR, L10, NLZD

      DOUBLE PRECISION ZD, ZDL, ZDN, ZR, ZDS, ZMAG, FAC, ONE_PLUS

      PARAMETER ( ONE_PLUS = 1.D0 + 2.D-7 )


      ! Check for null plot range
      IF ( ZL .GE. ZU ) THEN ! Null plot range
        ZLP = ZL - 1.D0
        ZUP = ZU + 1.D0
        RETURN
      END IF

      ! Initialize range and increment
      NINC2 = NINC * 2
      ZLP = ZL
      ZUP = ZU
      ZD = ( ZU - ZL ) / NINC2
      NZERO = -1

      ! Position zero if in or close to data range
      IF ( ( ZLP .GE. 0.D0 ) .OR.
     & ( EQUAL_DP( ZLP, 0.D0, 2.D-7 ) ) ) THEN
        IF ( ABS( ZLP ) .LT. ZD ) THEN
          ZLP = 0.D0
          ZD = ZUP / NINC2
          NZERO = 0
        END IF
      ELSE IF ( ( ZUP .LE. 0.D0 ) .OR.
     & ( EQUAL_DP( ZUP, 0.D0, 2.D-7 ) ) ) THEN
        IF ( ABS( ZUP ) .LT. ZD ) THEN
          ZUP = 0.D0
          ZD = -ZLP / NINC2
          NZERO = NINC2
        END IF
      ELSE ! Assign zero to a major or half-major tic mark
        NZERO = MAX( MIN( NINT( -ZLP * NINC2 / ( ZUP - ZLP ) ),
     &   NINC2 - 1 ), 1 )
        ZD = MAX( ZUP / ( NINC2 - NZERO ), -ZLP / NZERO )
        IF ( NZERO .GT. NINC ) THEN
          NDIR = -1
        ELSE
          NDIR = 1
        END IF
        ZDS = MAX( ZUP / ( NINC2 - ( NZERO + NDIR ) ),
     &   -ZLP / ( NZERO + NDIR ) )
        IF ( ZDS .LT. ZD ) THEN
          ZD = ZDS
          NZERO = NZERO + NDIR
        END IF
      END IF

      ! Select rounding granularity
      ZDL = LOG10( ZD )
      L10 = INT( ABS( ZDL ) + 1 )
      NLZD = INT( ZDL + L10 ) - L10 - 1
      FAC = 10.**NLZD
      ZDN = ZD / FAC ! ZDN is >= 10 and < 100
  101 IF ( ZDN .GT. 120.D0 * ONE_PLUS ) THEN
        ZR = 125.D0
      ELSE IF ( ZDN .GT. 100.D0 * ONE_PLUS ) THEN
        ZR = 120.D0
      ELSE IF ( ZDN .GT. 75.D0 * ONE_PLUS ) THEN
        ZR = 100.D0
      ELSE IF ( ZDN .GT. 60.D0 * ONE_PLUS ) THEN
        ZR = 75.D0
      ELSE IF ( ZDN .GT. 50.D0 * ONE_PLUS ) THEN
        ZR = 60.D0
      ELSE IF ( ZDN .GT. 40.D0 * ONE_PLUS ) THEN
        ZR = 50.D0
      ELSE IF ( ZDN .GT. 35.D0 * ONE_PLUS ) THEN
        ZR = 40.D0
      ELSE IF ( ZDN .GT. 30.D0 * ONE_PLUS ) THEN
        ZR = 35.D0
      ELSE IF ( ZDN .GT. 25.D0 * ONE_PLUS ) THEN
        ZR = 30.D0
      ELSE IF ( ZDN .GT. 20.D0 * ONE_PLUS ) THEN
        ZR = 25.D0
      ELSE IF ( ZDN .GT. 15.D0 * ONE_PLUS ) THEN
        ZR = 20.D0
      ELSE IF ( ZDN .GT. 12.5D0 * ONE_PLUS ) THEN
        ZR = 15.D0
      ELSE IF ( ZDN .GT. 10.D0 * ONE_PLUS ) THEN
        ZR = 12.5D0
      ELSE
        ZR = 10.D0
      END IF
      ZDS = ZR * FAC

      ! Select plot range
      IF ( NZERO .GE. 0 ) THEN ! Zero is on plot
        ZLP = -NZERO * ZDS
        ZUP = ( NINC2 - NZERO ) * ZDS
      ELSE ! Zero is not on plot
        IF ( ZLP .GE. 0.D0 ) THEN
          ZLP = INT( ( ZLP / ZDS ) * (1.D0+1.D-12) ) * ZDS
        ELSE
          ZLP = INT( ( ZLP / ZDS - 1.D0 ) * (1.D0-1.D-12) ) * ZDS
        END IF
        ZUP = ZLP + NINC2 * ZDS
        IF ( ( ZUP .LT. ZU ) .AND. ( ZDN .LE. 120.D0 ) ) THEN
          ZDN = ZDN * 1.05D0
          GO TO 101
        END IF
      END IF

      ZMAG = MAX( ABS( ZLP ), ABS( ZUP ) )
      IF ( ABS( ZLP ) .LT. ZMAG * 1.D-8 ) ZLP = 0.D0
      IF ( ABS( ZUP ) .LT. ZMAG * 1.D-8 ) ZUP = 0.D0

      RETURN
      END



      SUBROUTINE PLOT_WINDOW( FOUND, XLP, XUP, XTRAN,
     & YLW, YUW, YTRAN, U, X, Y )

!***********************************************************************
!* Computes the Y-Axis Range for a Given X Window
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      LOGICAL FOUND ! Point in window found flag

      DOUBLE PRECISION XLP ! X-axis lower plot range

      DOUBLE PRECISION XUP ! X-axis upper plot range

      CHARACTER*(*) XTRAN ! X-axis transform flag

      DOUBLE PRECISION YLW ! Y-axis lower window value

      DOUBLE PRECISION YUW ! Y-axis upper window value

      CHARACTER*(*) YTRAN ! Y-axis transform flag

      RECORD /UDS/  U ! UDS structure

      REAL X(NFP_L:NLP_U) ! X-axis value array

      REAL Y(NFP_L:NLP_U) ! Y-axis value array


      ! Variables ______________________________________________________

      LOGICAL XIFIN, XPFIN, YIFIN, YPFIN, IWIN, PWIN

      INTEGER P

      DOUBLE PRECISION XI, XP, YI, YP, SLOPE, YSTAR


      ! Set initial point
      IF ( BLANK( XTRAN ) ) XPFIN = .TRUE.
      IF ( BLANK( YTRAN ) ) YPFIN = .TRUE.
      CALL PLOT_WIN_PT( XTRAN, X(U.NFP), XP, XPFIN )
      CALL PLOT_WIN_PT( YTRAN, Y(U.NFP), YP, YPFIN )
      IF ( ( XPFIN ) .AND. ( YPFIN ) .AND. ( XP .GE. XLP ) .AND.
     & ( XP .LE. XUP ) ) THEN
        CALL PLOT_WIN_EXTEND( FOUND, YLW, YUW, YP )
        PWIN = .TRUE.
      ELSE
        PWIN = .FALSE.
      END IF

      ! Scan X array for points inside window
      DO P = U.NFP+1, U.NLP
        XI = XP
        XIFIN = XPFIN
        YI = YP
        YIFIN = YPFIN
        IWIN = PWIN
        CALL PLOT_WIN_PT( XTRAN, X(P), XP, XPFIN )
        CALL PLOT_WIN_PT( YTRAN, Y(P), YP, YPFIN )

        ! Check if point P is in window
        IF ( ( XPFIN ) .AND. ( YPFIN ) .AND. ( XP .GE. XLP ) .AND.
     &   ( XP .LE. XUP ) ) THEN
          CALL PLOT_WIN_EXTEND( FOUND, YLW, YUW, YP )
          PWIN = .TRUE.
        ELSE
          PWIN = .FALSE.
        END IF

        ! Check line between I and P
        IF ( ( IWIN ) .AND. ( PWIN ) ) THEN ! Done - Both pts in window
          ! Do nothing
        ELSE IF ( ( YIFIN ) .AND. ( YPFIN ) .AND.
     &   ( ( XIFIN ) .OR. ( XPFIN ) ) ) THEN
          IF ( .NOT. XIFIN ) THEN ! Check horizontal line from P
            IF ( XP .GT. XUP )
     &       CALL PLOT_WIN_EXTEND( FOUND, YLW, YUW, YP )
          ELSE IF ( .NOT.XPFIN ) THEN ! Check horizontal line from I
            IF ( XI .GT. XUP )
     &       CALL PLOT_WIN_EXTEND( FOUND, YLW, YUW, YI )
          ELSE IF ( ( MIN( XI, XP ) .LE. XUP ) .AND.
     &       ( MAX( XI, XP ) .GE. XLP ) ) THEN
            IF ( XI .EQ. XP ) THEN ! Vertical segment
              CALL PLOT_WIN_EXTEND( FOUND, YLW, YUW, YI )
              CALL PLOT_WIN_EXTEND( FOUND, YLW, YUW, YP )
            ELSE ! Check segment between I and P
              SLOPE = ( YP - YI ) / ( XP - XI )
              YSTAR = YI + ( XLP - XI ) * SLOPE
              IF ( ( YSTAR .GT. MIN( YI, YP ) ) .AND.
     &         ( YSTAR .LT. MAX( YI, YP ) ) )
     &         CALL PLOT_WIN_EXTEND( FOUND, YLW, YUW, YSTAR )
              YSTAR = YI + ( XUP - XI ) * SLOPE
              IF ( ( YSTAR .GT. MIN( YI, YP ) ) .AND.
     &         ( YSTAR .LT. MAX( YI, YP ) ) )
     &         CALL PLOT_WIN_EXTEND( FOUND, YLW, YUW, YSTAR )
            END IF
          END IF
        END IF
      END DO

      RETURN
      END



      SUBROUTINE PLOT_WIN_PT( ZTRAN, ZVAL, Z, ZFIN )

!***********************************************************************
!* Assigns Window Point Values
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) ZTRAN ! Z-axis transform flag

      REAL ZVAL ! Actual Z-axis point value

      DOUBLE PRECISION Z ! Z-axis window point value

      LOGICAL ZFIN ! Z-axis finite flag


      ! Assign point
      IF ( BLANK( ZTRAN ) ) THEN
        Z = ZVAL
      ELSE IF ( ZTRAN .EQ. 'LOG' ) THEN
        IF ( ZVAL .NE. 0. ) THEN
          Z = LOG10( ABS( ZVAL ) )
          ZFIN = .TRUE.
        ELSE
          ZFIN = .FALSE.
        END IF
      END IF

      RETURN
      END



      SUBROUTINE PLOT_WIN_EXTEND( FOUND, ZLW, ZUW, ZVAL )

!***********************************************************************
!* Extends Window Range by Specified Value
!***********************************************************************


      ! Arguments ______________________________________________________

      LOGICAL FOUND ! Point in window found flag

      DOUBLE PRECISION ZLW ! Lower window value

      DOUBLE PRECISION ZUW ! Upper window value

      DOUBLE PRECISION ZVAL ! Point value


      ! Update window range for Zval
      IF ( .NOT. FOUND ) THEN ! First point in window
        FOUND = .TRUE.
        ZLW = ZVAL
        ZUW = ZVAL
      ELSE ! Extend window range
        ZLW = MIN( ZLW, ZVAL )
        ZUW = MAX( ZUW, ZVAL )
      END IF

      RETURN
      END



      CHARACTER*(*) FUNCTION STR_APPEND( STR1, STR2 )

!***********************************************************************
!* Appends String2 Onto String1
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) STR1 ! First string

      CHARACTER*(*) STR2 ! Second string


      ! Append the string
      IF ( LEN_TRIM( STR1 ) .EQ. 0 ) THEN
        STR_APPEND = STR2
      ELSE
        STR_APPEND = STR1(:LEN_TRIM(STR1))//'  '//STR2
      END IF

      RETURN
      END
