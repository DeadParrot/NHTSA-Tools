      PROGRAM UDS2WKS

!***********************************************************************
!* UDS File Conversion to Lotus 123 WKS Format
!*
!* Author: S. Summers
!*         Stuart G. Mentzer - Tools framework
!*
!* Language: Fortran
!*
!* Date: 2003/07/27
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'
      INCLUDE 'file.fi'


      ! Variables ______________________________________________________

      RECORD /UDS/  U
      RECORD /UDS_CONTROL/  C
      RECORD /FILE/  HIT, REPORT, OUTPUT_LIST

      LOGICAL ALL_BATCH, NO_BATCH, NO_INCR, ONE_PASS, NEW_HIT, EOF,
     & BATCH

      INTEGER NUM_ARGS, MAX_ARGS, IOS, I, IT, WKS_UNIT

      PARAMETER ( MAX_ARGS = 12 )

      CHARACTER CL_ARG(0:MAX_ARGS)*255, PROG_NAME*8,
     & TEMPLATE_STR*255, DIR_OUT*255,
     & FILE_NAME*255, OUT_FILE*255, DEF_FILE*255,
     & PROMPT*79

      INTEGER nr, nc, Ncols, Ngraph, MaxRow, FirstRow

      PARAMETER ( FirstRow = 8 )


      ! Initializations
      PROG_NAME = 'UDS2WKS'
      CALL FILE_INIT( HIT )
      CALL GET_CL( CL_ARG, NUM_ARGS, MAX_ARGS )
      CALL PROC_CL( CL_ARG, NUM_ARGS, PROG_NAME, TEMPLATE_STR,
     & ALL_BATCH, NO_BATCH, NO_INCR, REPORT, OUTPUT_LIST, DIR_OUT,
     & ONE_PASS, .TRUE., .TRUE. )
      PROMPT = 'UDS file name/template/hitlist?'
      PROMPT(74:) = '[done]'
      CALL UDS_CONTROL_INIT( C )
      C.FILL_X = .TRUE.

      WRITE( *, '(//24X,A//)' ) '**  UDS To WKS Conversion  **'

      ! Read the file name/template/hitlist
  100 CALL FILE_PROMPT( TEMPLATE_STR, PROMPT, DIR_OUT,
     & ONE_PASS, HIT, NEW_HIT, FILE_NAME, EOF )
      IF ( ( FILE_NAME .EQ. ' ' ) .OR. ( EOF ) ) GO TO 199 ! Done

      ! Batch setup
      CALL BAT_SET( ALL_BATCH, NO_BATCH, NEW_HIT, HIT, BATCH )

      ! Read UDS file
      CALL REP_COLS( REPORT )
      CALL UDS_GET( FILE_NAME, U, C, REPORT, IOS )
      IF ( IOS .NE. 0 ) GO TO 190

      ! Set default output file name
      OUT_FILE = FILE_NAME
      CALL FN_EXTRACT( OUT_FILE )
      IT = FT_POSN_S( OUT_FILE, '.uds' )
      IF ( IT .EQ. 0 ) THEN
        IT = MIN( LEN_TRIM( OUT_FILE ) + 1, LEN( OUT_FILE ) - 3 )
      END IF
      OUT_FILE(IT:) = '.wks'
      CALL ADD_PATH( DIR_OUT, OUT_FILE )
      IF ( .NOT. NO_INCR ) CALL FN_NINC( OUT_FILE )
      CALL FN_NAME_EXTRACT( OUT_FILE )
      DEF_FILE = OUT_FILE

      ! Get the output WKS file name
      IF ( .NOT. BATCH ) THEN
  120   WRITE( *, '(/A/3A/'' >> '',$)' )
     &     ' Output WKS file name?',
     &     ' [', DEF_FILE(:L_TRIM(DEF_FILE)), ']'
        READ( *, '(A)', END=190 ) OUT_FILE
        IF ( OUT_FILE .EQ. ' ' ) THEN
          OUT_FILE = DEF_FILE
        ELSE IF ( .NOT. FN_VALID( OUT_FILE ) ) THEN
          WRITE( *, * ) '*** Invalid file name'
          GO TO 120
        END IF
      ELSE ! Batch
        OUT_FILE = DEF_FILE
      END IF

      ! Write the output WKS file
      CALL ADD_PATH( DIR_OUT, OUT_FILE )
      Call OpenWKS( WKS_UNIT, OUT_FILE )
      INQUIRE( FILE=OUT_FILE, NAME=OUT_FILE, IOSTAT=IOS )
      CALL MSG_WRITE( '  Writing: '//OUT_FILE, REPORT )
      IF ( WKS_UNIT .LE. 0 ) THEN
        CALL MSG_WRITE( '*** WKS file write failed', REPORT )
        GO TO 190
      END IF

      ! Set relevant values
      Ncols = 2
      NGraph = 1
      MaxRow = FirstRow + U.NLP - U.NFP + 1

      ! Set WKS dimensions
      Call DimWKS( WKS_UNIT, Ncols, MaxRow )

      ! define the default graph to be the first UDS column of data
      Call GraphWKS( WKS_UNIT, FirstRow, Ngraph, U )

      ! Write the X column
      nc = 1
      nr = FirstRow
      Do i = U.NFP, U.NLP
        Call WrtFloat( nr, nc, WKS_UNIT, U.X(I) )
        nr = nr + 1
      End do

      ! write Units to row 6
      Call WrtLabel( 6, Nc, WKS_UNIT, U.XUNITS )

      ! Set to Y column
      nc = nc + 1

      ! write file name to row 1
      Call FN_Extract( FILE_NAME )
      Call WrtLabel( 1, Nc, WKS_UNIT, FILE_NAME )

      ! write senatt to row 2
      Call WrtLabel( 2, Nc, WKS_UNIT, U.SenAtt )

      ! write Axis and Senloc
      Call WrtLabel( 3, Nc, WKS_UNIT, U.Axis//' Axis' )
      IF ( U.SENLOC .NE. ' ' )
     & Call WrtLabel( 4, Nc, WKS_UNIT, U.SenLoc//' Location' )

      ! write Units to row 6
      Call WrtLabel( 6, Nc, WKS_UNIT, U.YUNITS )

      ! Write the Y column
      nr = FirstRow
      Do i = U.NFP, U.NLP
        Call WrtFloat( nr, nc, WKS_UNIT, U.Y(i) )
        nr = nr + 1
      End do

      Call CloseWKS( WKS_UNIT )

      CALL OUT_ENTRY( OUT_FILE, OUTPUT_LIST )

      ! Loop for next file
  190 WRITE( *, '(80(''_''))' )
      GO TO 100

      ! Clean up and stop
  199 CALL CLEAN_UP( HIT, REPORT, OUTPUT_LIST, DIR_OUT )

      END



      Subroutine OpenWKS( LUN, WKS_File_Name )
!***********************************************************************

      Implicit None
      Integer Lun, IOS
      Integer*2 Code/0/, Length/2/
      Character char4
      PARAMETER ( char4 = char(4) )
      Character Products(2)/char4,char4/
      Character*(*) WKS_File_Name

      CALL OPEN_BIN( LUN, WKS_File_Name, 'W', IOS )
      IF ( IOS .NE. 0 ) THEN
        WRITE( *, * ) '*** WKS file open failed'
        RETURN
      END IF
      Write( Lun ) Code
      Write( Lun ) Length
      Write( Lun ) Products

      return
      end



      Subroutine CloseWKS( Lun )
!***********************************************************************

      Implicit None
      Integer Lun
      Integer*2 Code/01/, Length/0/
      Write( Lun ) Code
      Write( Lun ) Length
      Close( Lun )

      return
      end



      Subroutine Wrtlabel( nr, nc, Lun, String )
!***********************************************************************

      Implicit None
      Character*(*) String
      Integer*2 code/15/
      Integer*2 Length
      Character char255
      PARAMETER ( char255 = char(255) )
      Character Format/char255/
      Integer*2 Column
      Integer*2 Row
      Character Comma
      PARAMETER ( Comma = char(44) )
      Integer Lun, i, nr, nc, l

      INTEGER L_TRIM
      EXTERNAL L_TRIM

      ! fill out the structure and write the Cell Header
      l = L_TRIM( String )
      Length = INT2( l + 7 )
      Row = INT2( nr - 1 )
      Column = INT2( nc - 1 )
      Write( Lun ) code
      Write( Lun ) Length
      Write( Lun ) Format
      Write( Lun ) Column
      Write( Lun ) Row
      Write( Lun ) Comma

      ! remove any nulls from the string, replace with spaces
      i = index( String(:l), char(0) )
      Do While ( i .gt. 0 )
        String(i:i) = ' '
        i = index( String(:l), char(0) )
      End do

      ! write the String and terminate it with a Null
      Write( Lun ) String(:l)
      Write( Lun ) Char(0)

      return
      end


      Subroutine WrtFloat( nr, nc, Lun, Val )
!***********************************************************************

      Implicit None
      Integer nr, nc, Lun
      Real Val
      integer*2 code /14/
      integer*2 Length /13/
      Character char255
      PARAMETER ( char255 = char(255) )
      Character Format/char255/
      Integer*2 Column
      Integer*2 Row
      Real*8 Fvalue

      Column = INT2( nc - 1 )
      Row = INT2( nr - 1 )
      Fvalue = Val
      Write( Lun ) Code
      Write( Lun ) Length
      Write( Lun ) Format
      Write( Lun ) Column
      Write( Lun ) Row
      Write( Lun ) Fvalue

      return
      end


      Subroutine GraphWKS( Lun, nr, Ngraph, U )
!***********************************************************************

      Implicit None

      ! Headers
      INCLUDE 'uds_str.fi'
      RECORD /UDS/  U

      Integer Lun, nr, Ngraph, i, j
      Integer*2 Code/45/
      Integer*2 Length /437/
      Integer*2 Xrange(4)
      Integer*2 Yrange(4,6)
      Integer*2 label(4,6)
      Character Gtype
      Character Grid
      Character Color
      Character line(6)
      Character Algn(6)
      Character Zscale
      real*8 Xlow
      real*8 Xup
      Character Yscale
      real*8    Ylow
      real*8 Yup
      Character*40 Title1
      Character*40 Title2
      Character*40 Xtitle
      Character*40 Ytitle
      Character*20 legend(6)
      Character    Xformat
      Character    Yformat
      Integer*2    Skip

      ! write the graph code, and the length in bytes
      Write( Lun ) Code,Length
      Xrange(1) = 0
      Xrange(2) = INT2( nr - 1 )
      Xrange(3) = 0
      Xrange(4) = INT2( nr - 1 + (U.NLP-U.NFP) )
      Write( Lun ) Xrange

      ! set defaults for the six Y ranges
      do i = 1, 3, 2
        do j = 1, 6
          Yrange(i,j) = 32767
          label(i,j) = 32767
        end do
      end do
      do i = 2, 4, 2
        do j = 1, 6
          Yrange(i,j) = 0
          label(i,j) = 0
        end do
      end do

      ! set range for columns to be graphed
      Do i = 1, Ngraph
        Yrange(1,i) = INT2( i )
        Yrange(2,i) = INT2( nr - 1 )
        Yrange(3,i) = INT2( i )
        Yrange(4,i)= Xrange(4)
      End do

      Write( Lun ) Yrange
      Write( Lun ) Label
      Gtype = char(0)
      Write( Lun ) Gtype
      grid = char(0)
      Write( Lun ) Grid
      color = char(255)
      Write( Lun ) Color
      do i = 1, 6
        line(i) = char(1)
        Algn(i) = char(0)
        Call PadNUll( legend(i) )
      end do
      Write( Lun ) Line
      Write( Lun ) Algn
      Zscale = char(0)
      Write( Lun ) Zscale
      xlow = 0.
      Write( Lun ) xlow
      Xup = 0.
      Write( Lun ) Xup
      Yscale = char(0)
      Write( Lun ) Yscale
      yLow = 0.
      Write( Lun ) Ylow
      Yup = 0.
      Write( Lun ) Yup
      title1 = U.make//'  '//U.model
      ! title1 = 'this is the first title'
      Call PadNUll( title1 )
      Write( Lun ) Title1
      title2 = U.SenAtt//' '//U.Axis//' '//U.SenLoc
      ! title2 = 'this is the second title'
      Call PadNUll( title2 )
      Write( Lun ) Title2
      Xtitle = U.XUNITS
      Call PadNUll( Xtitle )
      Write( Lun ) Xtitle
      Ytitle = U.YUNITS
      Call PadNUll( Ytitle )
      Write( Lun ) Ytitle
      Write( Lun ) Legend
      Xformat = char(5)
      Write( Lun ) Xformat
      Yformat = char(5)
      Write( Lun ) Yformat
      skip = 1
      Write( Lun ) Skip

      return
      end



      Subroutine PadNUll( String )
!***********************************************************************

      Implicit None
      Character*(*) String
      Integer l

      l = len( String )
      Do While ( ( String(l:l) .eq. ' ' ) .and. ( l .gt. 0 ) )
        string(l:l) = char(0)
        l = l - 1
      End do

      return
      end



      Subroutine DimWKS( Lun, Ncols, MaxRow )
!***********************************************************************

      Implicit None
      Integer Lun, Ncols, MaxRow
      Integer*2 Code/6/
      Integer*2 Length/8/
      Integer*2 Sc/0/, Sr/0/, Ec, Er

      Ec = INT2( Ncols - 1 )
      Er = INT2( MaxRow - 1 )
      Write( Lun ) Code
      Write( Lun ) length
      Write( Lun ) Sc, Sr, Ec, Er

      return
      end
