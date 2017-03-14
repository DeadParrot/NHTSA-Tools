      SUBROUTINE UDS_SPEC_CHK( NAME_IN, MSG, IOS )

!***********************************************************************
!* Checks a Specified UDS File Field Name
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/17
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) NAME_IN ! UDS field name to check

      LOGICAL MSG ! Specifies messages allowed

      INTEGER IOS ! Status flag returned


      ! Variables ______________________________________________________

      CHARACTER NAME*25


      ! Convert field name to uppercase
      CALL STR_UPCASE( NAME, NAME_IN )

      ! Check for specified field name
      IOS = 0
      IF ( NAME .EQ. 'FILEVER' ) THEN
      ELSE IF ( NAME .EQ. 'NCHAN' ) THEN
      ELSE IF ( NAME .EQ. 'FILEFORM' ) THEN
      ELSE IF ( NAME .EQ. 'NUMFORM' ) THEN
      ELSE IF ( NAME .EQ. 'DIMSYS' ) THEN
      ELSE IF ( NAME .EQ. 'TSTSRC' ) THEN
      ELSE IF ( NAME .EQ. 'TSTNO' ) THEN
      ELSE IF ( NAME .EQ. 'TSTNAM' ) THEN
      ELSE IF ( NAME .EQ. 'TITLE' ) THEN
      ELSE IF ( NAME .EQ. 'TSTPRF' ) THEN
      ELSE IF ( NAME .EQ. 'TSTREF' ) THEN
      ELSE IF ( NAME .EQ. 'TSTCFN' ) THEN
      ELSE IF ( NAME .EQ. 'IMPANG' ) THEN
      ELSE IF ( NAME .EQ. 'CLSSPD' ) THEN
      ELSE IF ( NAME .EQ. 'CMPNO' ) THEN
      ELSE IF ( NAME .EQ. 'VEHNO' ) THEN ! Equivalent to CMPNO
      ELSE IF ( NAME .EQ. 'CMPTYP' ) THEN
      ELSE IF ( NAME .EQ. 'CMPDSC' ) THEN
      ELSE IF ( NAME .EQ. 'VEHDSC' ) THEN ! Equivalent to CMPDSC
      ELSE IF ( NAME .EQ. 'MAKE' ) THEN
      ELSE IF ( NAME .EQ. 'MODEL' ) THEN
      ELSE IF ( NAME .EQ. 'YEAR' ) THEN
      ELSE IF ( NAME .EQ. 'BODY' ) THEN
      ELSE IF ( NAME .EQ. 'ENGINE' ) THEN
      ELSE IF ( NAME .EQ. 'CMPWGT' ) THEN
      ELSE IF ( NAME .EQ. 'VEHTWT' ) THEN ! Equivalent to CMPWGT
      ELSE IF ( NAME .EQ. 'CMPSPD' ) THEN
      ELSE IF ( NAME .EQ. 'VEHSPD' ) THEN ! Equivalent to CMPSPD2
      ELSE IF ( NAME .EQ. 'OCCTYP' ) THEN
      ELSE IF ( NAME .EQ. 'OCCAGE' ) THEN
      ELSE IF ( NAME .EQ. 'OCCSEX' ) THEN
      ELSE IF ( NAME .EQ. 'OCCWT' ) THEN
      ELSE IF ( NAME .EQ. 'DUMSIZ' ) THEN
      ELSE IF ( NAME .EQ. 'RESTR1' ) THEN
      ELSE IF ( NAME .EQ. 'RESTR2' ) THEN
      ELSE IF ( NAME .EQ. 'HIC' ) THEN
      ELSE IF ( NAME .EQ. 'T1' ) THEN
      ELSE IF ( NAME .EQ. 'T2' ) THEN
      ELSE IF ( NAME .EQ. 'HICDTUP' ) THEN
      ELSE IF ( NAME .EQ. 'CLIP3M' ) THEN
      ELSE IF ( NAME .EQ. 'CSI' ) THEN
      ELSE IF ( NAME .EQ. 'AIS' ) THEN
      ELSE IF ( NAME .EQ. 'HDR_RESV' ) THEN
      ELSE IF ( NAME .EQ. 'CMPNO2' ) THEN
      ELSE IF ( NAME .EQ. 'VEHNO2' ) THEN ! Equivalent to CMPNO2
      ELSE IF ( NAME .EQ. 'CMPTYP2' ) THEN
      ELSE IF ( NAME .EQ. 'CMPDSC2' ) THEN
      ELSE IF ( NAME .EQ. 'VEHDSC2' ) THEN ! Equivalent to CMPDSC2
      ELSE IF ( NAME .EQ. 'MAKE2' ) THEN
      ELSE IF ( NAME .EQ. 'MODEL2' ) THEN
      ELSE IF ( NAME .EQ. 'YEAR2' ) THEN
      ELSE IF ( NAME .EQ. 'BODY2' ) THEN
      ELSE IF ( NAME .EQ. 'ENGINE2' ) THEN
      ELSE IF ( NAME .EQ. 'CMPWGT2' ) THEN
      ELSE IF ( NAME .EQ. 'VEHTWT2' ) THEN ! Equivalent to CMPWGT2
      ELSE IF ( NAME .EQ. 'CMPSPD2' ) THEN
      ELSE IF ( NAME .EQ. 'VEHSPD2' ) THEN ! Equivalent to CMPSPD2
      ELSE IF ( NAME .EQ. 'OCCTYP2' ) THEN
      ELSE IF ( NAME .EQ. 'OCCAGE2' ) THEN
      ELSE IF ( NAME .EQ. 'OCCSEX2' ) THEN
      ELSE IF ( NAME .EQ. 'OCCWT2' ) THEN
      ELSE IF ( NAME .EQ. 'DUMSIZ2' ) THEN
      ELSE IF ( NAME .EQ. 'RESTR12' ) THEN
      ELSE IF ( NAME .EQ. 'RESTR22' ) THEN
      ELSE IF ( NAME .EQ. 'CMP2_RESV' ) THEN
      ELSE IF ( NAME .EQ. 'ICHAN' ) THEN
      ELSE IF ( NAME .EQ. 'CHANFORM' ) THEN
      ELSE IF ( NAME .EQ. 'CURNO' ) THEN
      ELSE IF ( NAME .EQ. 'CURNAM' ) THEN
      ELSE IF ( NAME .EQ. 'SENATT' ) THEN
      ELSE IF ( NAME .EQ. 'SENLOC' ) THEN
      ELSE IF ( NAME .EQ. 'SENNUM' ) THEN
      ELSE IF ( NAME .EQ. 'BANDNO' ) THEN
      ELSE IF ( NAME .EQ. 'GAGNO' ) THEN
      ELSE IF ( NAME .EQ. 'AXIS' ) THEN
      ELSE IF ( NAME .EQ. 'YTYP' ) THEN
      ELSE IF ( NAME .EQ. 'YUNITS' ) THEN
      ELSE IF ( NAME .EQ. 'XTYP' ) THEN
      ELSE IF ( NAME .EQ. 'XUNITS' ) THEN
      ELSE IF ( NAME .EQ. 'STATUS' ) THEN
      ELSE IF ( NAME .EQ. 'CURTYP' ) THEN
      ELSE IF ( NAME .EQ. 'CURDSC' ) THEN
      ELSE IF ( NAME .EQ. 'NFP' ) THEN
      ELSE IF ( NAME .EQ. 'NLP' ) THEN
      ELSE IF ( NAME .EQ. 'DEL' ) THEN
      ELSE IF ( NAME .EQ. 'INIVEL' ) THEN
      ELSE IF ( NAME .EQ. 'PREF' ) THEN
      ELSE IF ( NAME .EQ. 'FCUT' ) THEN
      ELSE IF ( NAME .EQ. 'FCOR' ) THEN
      ELSE IF ( NAME .EQ. 'FSTP' ) THEN
      ELSE IF ( NAME .EQ. 'SCLFAC' ) THEN
      ELSE IF ( NAME .EQ. 'ID1' ) THEN
      ELSE IF ( NAME .EQ. 'ID2' ) THEN
      ELSE IF ( NAME .EQ. 'ID3' ) THEN
      ELSE IF ( NAME .EQ. 'ID4' ) THEN
      ELSE IF ( NAME .EQ. 'ID5' ) THEN
      ELSE IF ( NAME .EQ. 'RD1' ) THEN
      ELSE IF ( NAME .EQ. 'RD2' ) THEN
      ELSE IF ( NAME .EQ. 'RD3' ) THEN
      ELSE IF ( NAME .EQ. 'RD4' ) THEN
      ELSE IF ( NAME .EQ. 'RD5' ) THEN
      ELSE IF ( NAME .EQ. 'CD1' ) THEN
      ELSE IF ( NAME .EQ. 'CD2' ) THEN
      ELSE IF ( NAME .EQ. 'CURV_RESV' ) THEN
      ELSE IF ( NAME .EQ. '?' ) THEN
        IOS = 2
        IF ( MSG ) CALL UDS_SPEC_LIST()
      ELSE ! Not a valid UDS field name
        IOS = 1
        IF ( MSG ) WRITE( *, '(/2A)' )
     &     ' *** Unrecognized UDS field name: ', NAME(:L_TRIM(NAME))
      END IF

      RETURN
      END



      SUBROUTINE UDS_SPEC_SHOW( U, NAME_IN, MSG, IOS )

!***********************************************************************
!* Displays a Specified UDS File Field
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/17
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      RECORD /UDS/  U ! UDS object

      CHARACTER*(*) NAME_IN ! UDS field name to display

      LOGICAL MSG ! Specifies messages allowed

      INTEGER IOS ! Status flag returned


      ! Variables ______________________________________________________

      LOGICAL SHOW

      PARAMETER ( SHOW = .TRUE. )

      INTEGER VAL_INT

      REAL VAL_REAL

      CHARACTER VAL_TYP, VAL_STR*15


      ! Display field value
      CALL UDS_SPEC_GET( U, NAME_IN, VAL_TYP,
     & VAL_INT, VAL_REAL, VAL_STR, SHOW, MSG, IOS )

      RETURN
      END



      SUBROUTINE UDS_SPEC_GET( U, NAME_IN, VAL_TYP,
     & VAL_INT, VAL_REAL, VAL_STR, SHOW, MSG, IOS )

!***********************************************************************
!* Performs Specified UDS File Field Check/Display
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/17
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      RECORD /UDS/  U ! UDS object

      CHARACTER*(*) NAME_IN ! UDS field name to check/display

      CHARACTER VAL_TYP ! UDS value type

      INTEGER VAL_INT ! UDS integer value

      REAL VAL_REAL ! UDS real value

      CHARACTER*(*) VAL_STR ! UDS string value

      LOGICAL SHOW ! Specifies to display value

      LOGICAL MSG ! Specifies messages allowed

      INTEGER IOS ! Status flag returned


      ! Variables ______________________________________________________

      INTEGER IOS_W

      CHARACTER NAME*25


      ! Convert field name to uppercase
      CALL STR_UPCASE( NAME, NAME_IN )

      ! Initialize value variables
      VAL_TYP = ' '
      VAL_INT = 0
      VAL_REAL = 0.
      VAL_STR = ' '

      ! Get specified field value
      IOS = 0
      IF ( SHOW ) WRITE( *, * )
      IF ( NAME .EQ. 'FILEVER' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.FILEVER
        IF ( SHOW ) WRITE( *, * )
     &     'FILEVER = ', U.FILEVER(:L_TRIM(U.FILEVER))
      ELSE IF ( NAME .EQ. 'NCHAN' ) THEN
        VAL_TYP = 'I'
        VAL_INT = U.NCHAN
        IF ( SHOW ) WRITE( *, * ) 'NCHAN = ', U.NCHAN
      ELSE IF ( NAME .EQ. 'FILEFORM' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.FILEFORM
        IF ( SHOW ) WRITE( *, * )
     &     'FILEFORM = ', U.FILEFORM(:L_TRIM(U.FILEFORM))
      ELSE IF ( NAME .EQ. 'NUMFORM' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.NUMFORM
        IF ( SHOW ) WRITE( *, * )
     &     'NUMFORM = ', U.NUMFORM(:L_TRIM(U.NUMFORM))
      ELSE IF ( NAME .EQ. 'DIMSYS' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.DIMSYS
        IF ( SHOW ) WRITE( *, * )
     &     'DIMSYS = ', U.DIMSYS(:L_TRIM(U.DIMSYS))
      ELSE IF ( NAME .EQ. 'TSTSRC' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.TSTSRC
        IF ( SHOW ) WRITE( *, * )
     &     'TSTSRC = ', U.TSTSRC(:L_TRIM(U.TSTSRC))
      ELSE IF ( NAME .EQ. 'TSTNO' ) THEN
        VAL_TYP = 'I'
        VAL_INT = U.TSTNO
        IF ( SHOW ) WRITE( *, * ) 'TSTNO = ', U.TSTNO
      ELSE IF ( NAME .EQ. 'TSTNAM' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.TSTNAM
        IF ( SHOW ) WRITE( *, * )
     &     'TSTNAM = ', U.TSTNAM(:L_TRIM(U.TSTNAM))
      ELSE IF ( NAME .EQ. 'TITLE' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.TITLE
        IF ( SHOW ) WRITE( *, * )
     &     'TITLE = ', U.TITLE(:L_TRIM(U.TITLE))
      ELSE IF ( NAME .EQ. 'TSTPRF' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.TSTPRF
        IF ( SHOW ) WRITE( *, * )
     &     'TSTPRF = ', U.TSTPRF(:L_TRIM(U.TSTPRF))
      ELSE IF ( NAME .EQ. 'TSTREF' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.TSTREF
        IF ( SHOW ) WRITE( *, * )
     &     'TSTREF = ', U.TSTREF
      ELSE IF ( NAME .EQ. 'TSTCFN' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.TSTCFN
        IF ( SHOW ) WRITE( *, * )
     &     'TSTCFN = ', U.TSTCFN
      ELSE IF ( NAME .EQ. 'IMPANG' ) THEN
        VAL_TYP = 'I'
        VAL_INT = U.IMPANG
        IF ( SHOW ) WRITE( *, * ) 'IMPANG = ', U.IMPANG
      ELSE IF ( NAME .EQ. 'CLSSPD' ) THEN
        VAL_TYP = 'R'
        VAL_REAL = U.CLSSPD
        IF ( SHOW ) WRITE( *, * ) 'CLSSPD = ', U.CLSSPD
      ELSE IF ( NAME .EQ. 'CMPNO' ) THEN
        VAL_TYP = 'I'
        VAL_INT = U.CMPNO
        IF ( SHOW ) WRITE( *, * ) 'CMPNO = ', U.CMPNO
      ELSE IF ( NAME .EQ. 'VEHNO' ) THEN ! Equivalent to CMPNO
        VAL_TYP = 'I'
        VAL_INT = U.VEHNO
        IF ( SHOW ) WRITE( *, * ) 'VEHNO = ', U.VEHNO
      ELSE IF ( NAME .EQ. 'CMPTYP' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.CMPTYP
        IF ( SHOW ) WRITE( *, * )
     &     'CMPTYP = ', U.CMPTYP(:L_TRIM(U.CMPTYP))
      ELSE IF ( NAME .EQ. 'CMPDSC' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.CMPDSC
        IF ( SHOW ) WRITE( *, * )
     &     'CMPDSC = ', U.CMPDSC(:L_TRIM(U.CMPDSC))
      ELSE IF ( NAME .EQ. 'VEHDSC' ) THEN ! Equivalent to CMPDSC
        VAL_TYP = 'S'
        VAL_STR = U.VEHDSC
        IF ( SHOW ) WRITE( *, * )
     &     'VEHDSC = ', U.VEHDSC(:L_TRIM(U.VEHDSC))
      ELSE IF ( NAME .EQ. 'MAKE' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.MAKE
        IF ( SHOW ) WRITE( *, * )
     &     'MAKE = ', U.MAKE(:L_TRIM(U.MAKE))
      ELSE IF ( NAME .EQ. 'MODEL' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.MODEL
        IF ( SHOW ) WRITE( *, * )
     &     'MODEL = ', U.MODEL(:L_TRIM(U.MODEL))
      ELSE IF ( NAME .EQ. 'YEAR' ) THEN
        VAL_TYP = 'I'
        VAL_INT = U.YEAR
        IF ( SHOW ) WRITE( *, * ) 'YEAR = ', U.YEAR
      ELSE IF ( NAME .EQ. 'BODY' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.BODY
        IF ( SHOW ) WRITE( *, * )
     &     'BODY = ', U.BODY(:L_TRIM(U.BODY))
      ELSE IF ( NAME .EQ. 'ENGINE' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.ENGINE
        IF ( SHOW ) WRITE( *, * )
     &     'ENGINE = ', U.ENGINE(:L_TRIM(U.ENGINE))
      ELSE IF ( NAME .EQ. 'CMPWGT' ) THEN
        VAL_TYP = 'R'
        VAL_REAL = U.CMPWGT
        IF ( SHOW ) WRITE( *, * ) 'CMPWGT = ', U.CMPWGT
      ELSE IF ( NAME .EQ. 'VEHTWT' ) THEN ! Equivalent to CMPWGT
        VAL_TYP = 'R'
        VAL_REAL = U.VEHTWT
        IF ( SHOW ) WRITE( *, * ) 'VEHTWT = ', U.VEHTWT
      ELSE IF ( NAME .EQ. 'CMPSPD' ) THEN
        VAL_TYP = 'R'
        VAL_REAL = U.CMPSPD
        IF ( SHOW ) WRITE( *, * ) 'CMPSPD = ', U.CMPSPD
      ELSE IF ( NAME .EQ. 'VEHSPD' ) THEN ! Equivalent to CMPSPD2
        VAL_TYP = 'R'
        VAL_REAL = U.VEHSPD
        IF ( SHOW ) WRITE( *, * ) 'VEHSPD = ', U.VEHSPD
      ELSE IF ( NAME .EQ. 'OCCTYP' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.OCCTYP
        IF ( SHOW ) WRITE( *, * )
     &     'OCCTYP = ', U.OCCTYP(:L_TRIM(U.OCCTYP))
      ELSE IF ( NAME .EQ. 'OCCAGE' ) THEN
        VAL_TYP = 'I'
        VAL_INT = U.OCCAGE
        IF ( SHOW ) WRITE( *, * ) 'OCCAGE = ', U.OCCAGE
      ELSE IF ( NAME .EQ. 'OCCSEX' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.OCCSEX
        IF ( SHOW ) WRITE( *, * ) 'OCCSEX = ', U.OCCSEX
      ELSE IF ( NAME .EQ. 'OCCWT' ) THEN
        VAL_TYP = 'R'
        VAL_REAL = U.OCCWT
        IF ( SHOW ) WRITE( *, * ) 'OCCWT = ', U.OCCWT
      ELSE IF ( NAME .EQ. 'DUMSIZ' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.DUMSIZ
        IF ( SHOW ) WRITE( *, * )
     &     'DUMSIZ = ', U.DUMSIZ(:L_TRIM(U.DUMSIZ))
      ELSE IF ( NAME .EQ. 'RESTR1' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.RESTR1
        IF ( SHOW ) WRITE( *, * )
     &     'RESTR1 = ', U.RESTR1(:L_TRIM(U.RESTR1))
      ELSE IF ( NAME .EQ. 'RESTR2' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.RESTR2
        IF ( SHOW ) WRITE( *, * )
     &     'RESTR2 = ', U.RESTR2(:L_TRIM(U.RESTR2))
      ELSE IF ( NAME .EQ. 'HIC' ) THEN
        VAL_TYP = 'R'
        VAL_REAL = U.HIC
        IF ( SHOW ) WRITE( *, * ) 'HIC = ', U.HIC
      ELSE IF ( NAME .EQ. 'T1' ) THEN
        VAL_TYP = 'R'
        VAL_REAL = U.T1
        IF ( SHOW ) WRITE( *, * ) 'T1 = ', U.T1
      ELSE IF ( NAME .EQ. 'T2' ) THEN
        VAL_TYP = 'R'
        VAL_REAL = U.T2
        IF ( SHOW ) WRITE( *, * ) 'T2 = ', U.T2
      ELSE IF ( NAME .EQ. 'HICDTUP' ) THEN
        VAL_TYP = 'R'
        VAL_REAL = U.HICDTUP
        IF ( SHOW ) WRITE( *, * ) 'HICDTUP = ', U.HICDTUP
      ELSE IF ( NAME .EQ. 'CLIP3M' ) THEN
        VAL_TYP = 'R'
        VAL_REAL = U.CLIP3M
        IF ( SHOW ) WRITE( *, * ) 'CLIP3M = ', U.CLIP3M
      ELSE IF ( NAME .EQ. 'CSI' ) THEN
        VAL_TYP = 'R'
        VAL_REAL = U.CSI
        IF ( SHOW ) WRITE( *, * ) 'CSI = ', U.CSI
      ELSE IF ( NAME .EQ. 'AIS' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.AIS
        IF ( SHOW ) WRITE( *, * ) 'AIS = ', U.AIS
      ELSE IF ( NAME .EQ. 'HDR_RESV' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.HDR_RESV
        IF ( SHOW ) WRITE( *, *, IOSTAT=IOS_W )
     &     'HDR_RESV = ', U.HDR_RESV(:L_TRIM(U.HDR_RESV))
      ELSE IF ( NAME .EQ. 'CMPNO2' ) THEN
        VAL_TYP = 'I'
        VAL_INT = U.CMPNO2
        IF ( SHOW ) WRITE( *, * ) 'CMPNO2 = ', U.CMPNO2
      ELSE IF ( NAME .EQ. 'VEHNO2' ) THEN ! Equivalent to CMPNO2
        VAL_TYP = 'I'
        VAL_INT = U.VEHNO2
        IF ( SHOW ) WRITE( *, * ) 'VEHNO2 = ', U.VEHNO2
      ELSE IF ( NAME .EQ. 'CMPTYP2' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.CMPTYP2
        IF ( SHOW ) WRITE( *, * )
     &     'CMPTYP2 = ', U.CMPTYP2(:L_TRIM(U.CMPTYP2))
      ELSE IF ( NAME .EQ. 'CMPDSC2' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.CMPDSC2
        IF ( SHOW ) WRITE( *, * )
     &     'CMPDSC2 = ', U.CMPDSC2(:L_TRIM(U.CMPDSC2))
      ELSE IF ( NAME .EQ. 'VEHDSC2' ) THEN ! Equivalent to CMPDSC2
        VAL_TYP = 'S'
        VAL_STR = U.VEHDSC2
        IF ( SHOW ) WRITE( *, * )
     &     'VEHDSC2 = ', U.VEHDSC2(:L_TRIM(U.VEHDSC2))
      ELSE IF ( NAME .EQ. 'MAKE2' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.MAKE2
        IF ( SHOW ) WRITE( *, * )
     &     'MAKE2 = ', U.MAKE2(:L_TRIM(U.MAKE2))
      ELSE IF ( NAME .EQ. 'MODEL2' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.MODEL2
        IF ( SHOW ) WRITE( *, * )
     &     'MODEL2 = ', U.MODEL2(:L_TRIM(U.MODEL2))
      ELSE IF ( NAME .EQ. 'YEAR2' ) THEN
        VAL_TYP = 'I'
        VAL_INT = U.YEAR2
        IF ( SHOW ) WRITE( *, * ) 'YEAR2 = ', U.YEAR2
      ELSE IF ( NAME .EQ. 'BODY2' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.BODY2
        IF ( SHOW ) WRITE( *, * )
     &     'BODY2 = ', U.BODY2(:L_TRIM(U.BODY2))
      ELSE IF ( NAME .EQ. 'ENGINE2' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.ENGINE2
        IF ( SHOW ) WRITE( *, * )
     &     'ENGINE2 = ', U.ENGINE2(:L_TRIM(U.ENGINE2))
      ELSE IF ( NAME .EQ. 'CMPWGT2' ) THEN
        VAL_TYP = 'R'
        VAL_REAL = U.CMPWGT2
        IF ( SHOW ) WRITE( *, * ) 'CMPWGT2 = ', U.CMPWGT2
      ELSE IF ( NAME .EQ. 'VEHTWT2' ) THEN ! Equivalent to CMPWGT2
        VAL_TYP = 'R'
        VAL_REAL = U.VEHTWT2
        IF ( SHOW ) WRITE( *, * ) 'VEHTWT2 = ', U.VEHTWT2
      ELSE IF ( NAME .EQ. 'CMPSPD2' ) THEN
        VAL_TYP = 'R'
        VAL_REAL = U.CMPSPD2
        IF ( SHOW ) WRITE( *, * ) 'CMPSPD2 = ', U.CMPSPD2
      ELSE IF ( NAME .EQ. 'VEHSPD2' ) THEN ! Equivalent to CMPSPD2
        VAL_TYP = 'R'
        VAL_REAL = U.VEHSPD2
        IF ( SHOW ) WRITE( *, * ) 'VEHSPD2 = ', U.VEHSPD2
      ELSE IF ( NAME .EQ. 'OCCTYP2' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.OCCTYP2
        IF ( SHOW ) WRITE( *, * )
     &     'OCCTYP2 = ', U.OCCTYP2(:L_TRIM(U.OCCTYP2))
      ELSE IF ( NAME .EQ. 'OCCAGE2' ) THEN
        VAL_TYP = 'I'
        VAL_INT = U.OCCAGE2
        IF ( SHOW ) WRITE( *, * ) 'OCCAGE2 = ', U.OCCAGE2
      ELSE IF ( NAME .EQ. 'OCCSEX2' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.OCCSEX2
        IF ( SHOW ) WRITE( *, * ) 'OCCSEX2 = ', U.OCCSEX2
      ELSE IF ( NAME .EQ. 'OCCWT2' ) THEN
        VAL_TYP = 'R'
        VAL_REAL = U.OCCWT2
        IF ( SHOW ) WRITE( *, * ) 'OCCWT2 = ', U.OCCWT2
      ELSE IF ( NAME .EQ. 'DUMSIZ2' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.DUMSIZ2
        IF ( SHOW ) WRITE( *, * )
     &     'DUMSIZ2 = ', U.DUMSIZ2(:L_TRIM(U.DUMSIZ2))
      ELSE IF ( NAME .EQ. 'RESTR12' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.RESTR12
        IF ( SHOW ) WRITE( *, * )
     &     'RESTR12 = ', U.RESTR12(:L_TRIM(U.RESTR12))
      ELSE IF ( NAME .EQ. 'RESTR22' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.RESTR22
        IF ( SHOW ) WRITE( *, * )
     &     'RESTR22 = ', U.RESTR22(:L_TRIM(U.RESTR22))
      ELSE IF ( NAME .EQ. 'CMP2_RESV' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.CMP2_RESV
        IF ( SHOW ) WRITE( *, *, IOSTAT=IOS_W )
     &     'CMP2_RESV = ', U.CMP2_RESV(:L_TRIM(U.CMP2_RESV))
      ELSE IF ( NAME .EQ. 'ICHAN' ) THEN
        VAL_TYP = 'I'
        VAL_INT = U.ICHAN
        IF ( SHOW ) WRITE( *, * ) 'ICHAN = ', U.ICHAN
      ELSE IF ( NAME .EQ. 'CHANFORM' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.CHANFORM
        IF ( SHOW ) WRITE( *, * )
     &     'CHANFORM = ', U.CHANFORM(:L_TRIM(U.CHANFORM))
      ELSE IF ( NAME .EQ. 'CURNO' ) THEN
        VAL_TYP = 'I'
        VAL_INT = U.CURNO
        IF ( SHOW ) WRITE( *, * ) 'CURNO = ', U.CURNO
      ELSE IF ( NAME .EQ. 'CURNAM' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.CURNAM
        IF ( SHOW ) WRITE( *, * )
     &     'CURNAM = ', U.CURNAM(:L_TRIM(U.CURNAM))
      ELSE IF ( NAME .EQ. 'SENATT' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.SENATT
        IF ( SHOW ) WRITE( *, * )
     &     'SENATT = ', U.SENATT(:L_TRIM(U.SENATT))
      ELSE IF ( NAME .EQ. 'SENLOC' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.SENLOC
        IF ( SHOW ) WRITE( *, * )
     &     'SENLOC = ', U.SENLOC(:L_TRIM(U.SENLOC))
      ELSE IF ( NAME .EQ. 'SENNUM' ) THEN
        VAL_TYP = 'I'
        VAL_INT = U.SENNUM
        IF ( SHOW ) WRITE( *, * ) 'SENNUM = ', U.SENNUM
      ELSE IF ( NAME .EQ. 'BANDNO' ) THEN
        VAL_TYP = 'I'
        VAL_INT = U.BANDNO
        IF ( SHOW ) WRITE( *, * ) 'BANDNO = ', U.BANDNO
      ELSE IF ( NAME .EQ. 'GAGNO' ) THEN
        VAL_TYP = 'I'
        VAL_INT = U.GAGNO
        IF ( SHOW ) WRITE( *, * ) 'GAGNO = ', U.GAGNO
      ELSE IF ( NAME .EQ. 'AXIS' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.AXIS
        IF ( SHOW ) WRITE( *, * )
     &     'AXIS = ', U.AXIS(:L_TRIM(U.AXIS))
      ELSE IF ( NAME .EQ. 'YTYP' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.YTYP
        IF ( SHOW ) WRITE( *, * )
     &     'YTYP = ', U.YTYP(:L_TRIM(U.YTYP))
      ELSE IF ( NAME .EQ. 'YUNITS' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.YUNITS
        IF ( SHOW ) WRITE( *, * )
     &     'YUNITS = ', U.YUNITS(:L_TRIM(U.YUNITS))
      ELSE IF ( NAME .EQ. 'XTYP' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.XTYP
        IF ( SHOW ) WRITE( *, * )
     &     'XTYP = ', U.XTYP(:L_TRIM(U.XTYP))
      ELSE IF ( NAME .EQ. 'XUNITS' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.XUNITS
        IF ( SHOW ) WRITE( *, * )
     &     'XUNITS = ', U.XUNITS(:L_TRIM(U.XUNITS))
      ELSE IF ( NAME .EQ. 'STATUS' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.STATUS
        IF ( SHOW ) WRITE( *, * )
     &     'STATUS = ', U.STATUS(:L_TRIM(U.STATUS))
      ELSE IF ( NAME .EQ. 'CURTYP' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.CURTYP
        IF ( SHOW ) WRITE( *, * )
     &     'CURTYP = ', U.CURTYP(:L_TRIM(U.CURTYP))
      ELSE IF ( NAME .EQ. 'CURDSC' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.CURDSC
        IF ( SHOW ) WRITE( *, * )
     &     'CURDSC = ', U.CURDSC(:L_TRIM(U.CURDSC))
      ELSE IF ( NAME .EQ. 'NFP' ) THEN
        VAL_TYP = 'I'
        VAL_INT = U.NFP
        IF ( SHOW ) WRITE( *, * ) 'NFP = ', U.NFP
      ELSE IF ( NAME .EQ. 'NLP' ) THEN
        VAL_TYP = 'I'
        VAL_INT = U.NLP
        IF ( SHOW ) WRITE( *, * ) 'NLP = ', U.NLP
      ELSE IF ( NAME .EQ. 'DEL' ) THEN
        VAL_TYP = 'R'
        VAL_REAL = U.DEL
        IF ( SHOW ) WRITE( *, * ) 'DEL = ', U.DEL
      ELSE IF ( NAME .EQ. 'INIVEL' ) THEN
        VAL_TYP = 'R'
        VAL_REAL = U.INIVEL
        IF ( SHOW ) WRITE( *, * ) 'INIVEL = ', U.INIVEL
      ELSE IF ( NAME .EQ. 'PREF' ) THEN
        VAL_TYP = 'R'
        VAL_REAL = U.PREF
        IF ( SHOW ) WRITE( *, * ) 'PREF = ', U.PREF
      ELSE IF ( NAME .EQ. 'FCUT' ) THEN
        VAL_TYP = 'R'
        VAL_REAL = U.FCUT
        IF ( SHOW ) WRITE( *, * ) 'FCUT = ', U.FCUT
      ELSE IF ( NAME .EQ. 'FCOR' ) THEN
        VAL_TYP = 'R'
        VAL_REAL = U.FCOR
        IF ( SHOW ) WRITE( *, * ) 'FCOR = ', U.FCOR
      ELSE IF ( NAME .EQ. 'FSTP' ) THEN
        VAL_TYP = 'R'
        VAL_REAL = U.FSTP
        IF ( SHOW ) WRITE( *, * ) 'FSTP = ', U.FSTP
      ELSE IF ( NAME .EQ. 'SCLFAC' ) THEN
        VAL_TYP = 'R'
        VAL_REAL = U.SCLFAC
        IF ( SHOW ) WRITE( *, * ) 'SCLFAC = ', U.SCLFAC
      ELSE IF ( NAME .EQ. 'ID1' ) THEN
        VAL_TYP = 'I'
        VAL_INT = U.ID1
        IF ( SHOW ) WRITE( *, * ) 'ID1 = ', U.ID1
      ELSE IF ( NAME .EQ. 'ID2' ) THEN
        VAL_TYP = 'I'
        VAL_INT = U.ID2
        IF ( SHOW ) WRITE( *, * ) 'ID2 = ', U.ID2
      ELSE IF ( NAME .EQ. 'ID3' ) THEN
        VAL_TYP = 'I'
        VAL_INT = U.ID3
        IF ( SHOW ) WRITE( *, * ) 'ID3 = ', U.ID3
      ELSE IF ( NAME .EQ. 'ID4' ) THEN
        VAL_TYP = 'I'
        VAL_INT = U.ID4
        IF ( SHOW ) WRITE( *, * ) 'ID4 = ', U.ID4
      ELSE IF ( NAME .EQ. 'ID5' ) THEN
        VAL_TYP = 'I'
        VAL_INT = U.ID5
        IF ( SHOW ) WRITE( *, * ) 'ID5 = ', U.ID5
      ELSE IF ( NAME .EQ. 'RD1' ) THEN
        VAL_TYP = 'R'
        VAL_REAL = U.RD1
        IF ( SHOW ) WRITE( *, * ) 'RD1 = ', U.RD1
      ELSE IF ( NAME .EQ. 'RD2' ) THEN
        VAL_TYP = 'R'
        VAL_REAL = U.RD2
        IF ( SHOW ) WRITE( *, * ) 'RD2 = ', U.RD2
      ELSE IF ( NAME .EQ. 'RD3' ) THEN
        VAL_TYP = 'R'
        VAL_REAL = U.RD3
        IF ( SHOW ) WRITE( *, * ) 'RD3 = ', U.RD3
      ELSE IF ( NAME .EQ. 'RD4' ) THEN
        VAL_TYP = 'R'
        VAL_REAL = U.RD4
        IF ( SHOW ) WRITE( *, * ) 'RD4 = ', U.RD4
      ELSE IF ( NAME .EQ. 'RD5' ) THEN
        VAL_TYP = 'R'
        VAL_REAL = U.RD5
        IF ( SHOW ) WRITE( *, * ) 'RD5 = ', U.RD5
      ELSE IF ( NAME .EQ. 'CD1' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.CD1
        IF ( SHOW ) WRITE( *, * )
     &     'CD1 = ', U.CD1(:L_TRIM(U.CD1))
      ELSE IF ( NAME .EQ. 'CD2' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.CD2
        IF ( SHOW ) WRITE( *, * )
     &     'CD2 = ', U.CD2(:L_TRIM(U.CD2))
      ELSE IF ( NAME .EQ. 'CURV_RESV' ) THEN
        VAL_TYP = 'S'
        VAL_STR = U.CURV_RESV
        IF ( SHOW ) WRITE( *, *, IOSTAT=IOS_W )
     &     'CURV_RESV = ', U.CURV_RESV(:L_TRIM(U.CURV_RESV))
      ELSE IF ( NAME .EQ. '?' ) THEN
        IOS = 2
        IF ( MSG ) CALL UDS_SPEC_LIST()
      ELSE ! Not a valid UDS field name
        IOS = 1
        IF ( MSG ) THEN
          IF ( .NOT. SHOW ) WRITE( *, * )
          WRITE( *, '(2A)' )
     &       ' *** Unrecognized UDS field name: ', NAME(:L_TRIM(NAME))
        END IF
      END IF

      ! Write numbers to string
      IF ( LEN( VAL_STR ) .GE. 15 ) THEN
        IF ( VAL_TYP .EQ. 'I' ) THEN
          WRITE( VAL_STR, '(I15)', IOSTAT=IOS_W ) VAL_INT
        ELSE IF ( VAL_TYP .EQ. 'R' ) THEN
          WRITE( VAL_STR, '(1PG15.7)', IOSTAT=IOS_W ) VAL_REAL
        END IF
      END IF

      RETURN
      END



      SUBROUTINE UDS_SPEC_SET( U, NAME_IN, VALUE, MSG, IOS )

!***********************************************************************
!* Performs Specified UDS File Specification Field Set
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/17
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      RECORD /UDS/  U ! UDS object

      CHARACTER*(*) NAME_IN ! UDS field name to set

      CHARACTER*(*) VALUE ! UDS field value (in string form) to set

      LOGICAL MSG ! Specifies messages allowed

      INTEGER IOS ! Status flag returned


      ! Variables ______________________________________________________

      CHARACTER NAME*25


      ! Functions ______________________________________________________

      CHARACTER RJUST*12

      EXTERNAL RJUST


      ! Convert field name to uppercase
      CALL STR_UPCASE( NAME, NAME_IN )

      ! Set specified field
      IOS = 0
      IF ( NAME .EQ. 'FILEVER' ) THEN
        U.FILEVER = VALUE
        IF ( ( MSG ) .AND. ( IOS .EQ. 0 ) ) THEN
          IF ( U.FILEVER .NE. 'UDS-1992' ) WRITE( *, 901 )
        END IF
      ELSE IF ( NAME .EQ. 'NCHAN' ) THEN
        CALL INT_ASSIGN( VALUE, U.NCHAN, MSG, IOS )
        IF ( ( MSG ) .AND. ( IOS .EQ. 0 ) ) THEN
          IF ( U.NCHAN .NE. 1 ) WRITE( *, 901 )
        END IF
      ELSE IF ( NAME .EQ. 'FILEFORM' ) THEN
        U.FILEFORM = VALUE
        IF ( ( MSG ) .AND. ( IOS .EQ. 0 ) ) THEN
          IF ( ( U.FILEFORM .NE. 'Y' ) .AND.
     &       ( U.FILEFORM .NE. 'X-Y' ) ) WRITE( *, 901 )
        END IF
      ELSE IF ( NAME .EQ. 'NUMFORM' ) THEN
        U.NUMFORM = VALUE
        IF ( ( MSG ) .AND. ( IOS .EQ. 0 ) ) THEN
          IF ( ( U.NUMFORM .NE. 'VAX' ) .AND.
     &       ( U.NUMFORM .NE. 'IEEE' ) .AND.
     &       ( U.NUMFORM .NE. 'SUN' ) ) WRITE( *, 901 )
        END IF
      ELSE IF ( NAME .EQ. 'DIMSYS' ) THEN
        U.DIMSYS = VALUE
        IF ( ( MSG ) .AND. ( IOS .EQ. 0 ) ) THEN
          IF ( ( U.DIMSYS .NE. 'MET' ) .AND.
     &       ( U.DIMSYS .NE. 'SI' ) .AND.
     &       ( U.DIMSYS .NE. 'ENG' ) ) WRITE( *, 901 )
        END IF
      ELSE IF ( NAME .EQ. 'TSTSRC' ) THEN
        U.TSTSRC = VALUE
      ELSE IF ( NAME .EQ. 'TSTNO' ) THEN
        CALL INT_ASSIGN( VALUE, U.TSTNO, MSG, IOS )
        IF ( ( MSG ) .AND. ( IOS .EQ. 0 ) ) THEN
          IF ( U.TSTNO .LT. 0 ) WRITE( *, 901 )
        END IF
      ELSE IF ( NAME .EQ. 'TSTNAM' ) THEN
        U.TSTNAM = VALUE
        IF ( INT_CHK( VALUE ) ) U.TSTNAM = RJUST( U.TSTNAM )
      ELSE IF ( NAME .EQ. 'TITLE' ) THEN
        U.TITLE = VALUE
      ELSE IF ( NAME .EQ. 'TSTPRF' ) THEN
        U.TSTPRF = VALUE
      ELSE IF ( NAME .EQ. 'TSTREF' ) THEN
        U.TSTREF = VALUE
      ELSE IF ( NAME .EQ. 'TSTCFN' ) THEN
        U.TSTCFN = VALUE
      ELSE IF ( NAME .EQ. 'IMPANG' ) THEN
        CALL INT_ASSIGN( VALUE, U.IMPANG, MSG, IOS )
        IF ( ( MSG ) .AND. ( IOS .EQ. 0 ) ) THEN
          IF ( ABS( U.IMPANG ) .GT. 360 ) WRITE( *, 901 )
        END IF
      ELSE IF ( NAME .EQ. 'CLSSPD' ) THEN
        CALL REAL_ASSIGN( VALUE, U.CLSSPD, MSG, IOS )
      ELSE IF ( NAME .EQ. 'CMPNO' ) THEN
        CALL INT_ASSIGN( VALUE, U.CMPNO, MSG, IOS )
        IF ( ( MSG ) .AND. ( IOS .EQ. 0 ) ) THEN
          IF ( U.CMPNO .LT. 0 ) WRITE( *, 901 )
        END IF
      ELSE IF ( NAME .EQ. 'VEHNO' ) THEN ! Equivalent to CMPNO
        CALL INT_ASSIGN( VALUE, U.VEHNO, MSG, IOS )
        IF ( ( MSG ) .AND. ( IOS .EQ. 0 ) ) THEN
          IF ( U.VEHNO .LT. 0 ) WRITE( *, 901 )
        END IF
      ELSE IF ( NAME .EQ. 'CMPTYP' ) THEN
        U.CMPTYP = VALUE
      ELSE IF ( NAME .EQ. 'CMPDSC' ) THEN
        U.CMPDSC = VALUE
      ELSE IF ( NAME .EQ. 'VEHDSC' ) THEN ! Equivalent to CMPDSC
        U.VEHDSC = VALUE
      ELSE IF ( NAME .EQ. 'MAKE' ) THEN
        U.MAKE = VALUE
      ELSE IF ( NAME .EQ. 'MODEL' ) THEN
        U.MODEL = VALUE
      ELSE IF ( NAME .EQ. 'YEAR' ) THEN
        CALL INT_ASSIGN( VALUE, U.YEAR, MSG, IOS )
        IF ( ( MSG ) .AND. ( IOS .EQ. 0 ) ) THEN
          IF ( U.YEAR .LT. 0 ) WRITE( *, 901 )
        END IF
      ELSE IF ( NAME .EQ. 'BODY' ) THEN
        U.BODY = VALUE
      ELSE IF ( NAME .EQ. 'ENGINE' ) THEN
        U.ENGINE = VALUE
      ELSE IF ( NAME .EQ. 'CMPWGT' ) THEN
        CALL REAL_ASSIGN( VALUE, U.CMPWGT, MSG, IOS )
        IF ( ( MSG ) .AND. ( IOS .EQ. 0 ) ) THEN
          IF ( U.CMPWGT .LT. 0. ) WRITE( *, 901 )
        END IF
      ELSE IF ( NAME .EQ. 'VEHTWT' ) THEN ! Equivalent to CMPWGT
        CALL REAL_ASSIGN( VALUE, U.VEHTWT, MSG, IOS )
        IF ( ( MSG ) .AND. ( IOS .EQ. 0 ) ) THEN
          IF ( U.VEHTWT .LT. 0. ) WRITE( *, 901 )
        END IF
      ELSE IF ( NAME .EQ. 'CMPSPD' ) THEN
        CALL REAL_ASSIGN( VALUE, U.CMPSPD, MSG, IOS )
      ELSE IF ( NAME .EQ. 'VEHSPD' ) THEN ! Equivalent to CMPSPD
        CALL REAL_ASSIGN( VALUE, U.VEHSPD, MSG, IOS )
      ELSE IF ( NAME .EQ. 'OCCTYP' ) THEN
        U.OCCTYP = VALUE
      ELSE IF ( NAME .EQ. 'OCCAGE' ) THEN
        CALL INT_ASSIGN( VALUE, U.OCCAGE, MSG, IOS )
        IF ( ( MSG ) .AND. ( IOS .EQ. 0 ) ) THEN
          IF ( U.OCCAGE .LT. 0 ) WRITE( *, 901 )
        END IF
      ELSE IF ( NAME .EQ. 'OCCSEX' ) THEN
        U.OCCSEX = VALUE
        IF ( ( MSG ) .AND. ( IOS .EQ. 0 ) ) THEN
          IF ( .NOT. ANY_CHARS( U.OCCSEX, 'MF ' ) )
     &       WRITE( *, 901 )
        END IF
      ELSE IF ( NAME .EQ. 'OCCWT' ) THEN
        CALL REAL_ASSIGN( VALUE, U.OCCWT, MSG, IOS )
        IF ( ( MSG ) .AND. ( IOS .EQ. 0 ) ) THEN
          IF ( U.OCCWT .LT. 0. ) WRITE( *, 901 )
        END IF
      ELSE IF ( NAME .EQ. 'DUMSIZ' ) THEN
        U.DUMSIZ = VALUE
      ELSE IF ( NAME .EQ. 'RESTR1' ) THEN
        U.RESTR1 = VALUE
      ELSE IF ( NAME .EQ. 'RESTR2' ) THEN
        U.RESTR2 = VALUE
      ELSE IF ( NAME .EQ. 'HIC' ) THEN
        CALL REAL_ASSIGN( VALUE, U.HIC, MSG, IOS )
        IF ( ( MSG ) .AND. ( IOS .EQ. 0 ) ) THEN
          IF ( U.HIC .LT. 0. ) WRITE( *, 901 )
        END IF
      ELSE IF ( NAME .EQ. 'T1' ) THEN
        CALL REAL_ASSIGN( VALUE, U.T1, MSG, IOS )
      ELSE IF ( NAME .EQ. 'T2' ) THEN
        CALL REAL_ASSIGN( VALUE, U.T2, MSG, IOS )
      ELSE IF ( NAME .EQ. 'HICDTUP' ) THEN
        CALL REAL_ASSIGN( VALUE, U.HICDTUP, MSG, IOS )
        IF ( ( MSG ) .AND. ( IOS .EQ. 0 ) ) THEN
          IF ( U.HICDTUP .LT. 0. ) WRITE( *, 901 )
        END IF
      ELSE IF ( NAME .EQ. 'CLIP3M' ) THEN
        CALL REAL_ASSIGN( VALUE, U.CLIP3M, MSG, IOS )
        IF ( ( MSG ) .AND. ( IOS .EQ. 0 ) ) THEN
          IF ( U.CLIP3M .LT. 0. ) WRITE( *, 901 )
        END IF
      ELSE IF ( NAME .EQ. 'CSI' ) THEN
        CALL REAL_ASSIGN( VALUE, U.CSI, MSG, IOS )
        IF ( ( MSG ) .AND. ( IOS .EQ. 0 ) ) THEN
          IF ( U.CSI .LT. 0. ) WRITE( *, 901 )
        END IF
      ELSE IF ( NAME .EQ. 'AIS' ) THEN
        U.AIS = VALUE
      ELSE IF ( NAME .EQ. 'HDR_RESV' ) THEN
        U.HDR_RESV = VALUE
      ELSE IF ( NAME .EQ. 'CMPNO2' ) THEN
        CALL INT_ASSIGN( VALUE, U.CMPNO2, MSG, IOS )
        IF ( ( MSG ) .AND. ( IOS .EQ. 0 ) ) THEN
          IF ( U.CMPNO2 .LT. 0 ) WRITE( *, 901 )
        END IF
      ELSE IF ( NAME .EQ. 'VEHNO2' ) THEN ! Equivalent to CMPNO2
        CALL INT_ASSIGN( VALUE, U.VEHNO2, MSG, IOS )
        IF ( ( MSG ) .AND. ( IOS .EQ. 0 ) ) THEN
          IF ( U.VEHNO2 .LT. 0 ) WRITE( *, 901 )
        END IF
      ELSE IF ( NAME .EQ. 'CMPTYP2' ) THEN
        U.CMPTYP2 = VALUE
      ELSE IF ( NAME .EQ. 'CMPDSC2' ) THEN
        U.CMPDSC2 = VALUE
      ELSE IF ( NAME .EQ. 'VEHDSC2' ) THEN ! Equivalent to CMPDSC2
        U.VEHDSC2 = VALUE
      ELSE IF ( NAME .EQ. 'MAKE2' ) THEN
        U.MAKE2 = VALUE
      ELSE IF ( NAME .EQ. 'MODEL2' ) THEN
        U.MODEL2 = VALUE
      ELSE IF ( NAME .EQ. 'YEAR2' ) THEN
        CALL INT_ASSIGN( VALUE, U.YEAR2, MSG, IOS )
        IF ( ( MSG ) .AND. ( IOS .EQ. 0 ) ) THEN
          IF ( U.YEAR2 .LT. 0 ) WRITE( *, 901 )
        END IF
      ELSE IF ( NAME .EQ. 'BODY2' ) THEN
        U.BODY2 = VALUE
      ELSE IF ( NAME .EQ. 'ENGINE2' ) THEN
        U.ENGINE2 = VALUE
      ELSE IF ( NAME .EQ. 'CMPWGT2' ) THEN
        CALL REAL_ASSIGN( VALUE, U.CMPWGT2, MSG, IOS )
        IF ( ( MSG ) .AND. ( IOS .EQ. 0 ) ) THEN
          IF ( U.CMPWGT2 .LT. 0. ) WRITE( *, 901 )
        END IF
      ELSE IF ( NAME .EQ. 'VEHTWT2' ) THEN ! Equivalent to CMPWGT2
        CALL REAL_ASSIGN( VALUE, U.VEHTWT2, MSG, IOS )
        IF ( ( MSG ) .AND. ( IOS .EQ. 0 ) ) THEN
          IF ( U.VEHTWT2 .LT. 0. ) WRITE( *, 901 )
        END IF
      ELSE IF ( NAME .EQ. 'CMPSPD2' ) THEN
        CALL REAL_ASSIGN( VALUE, U.CMPSPD2, MSG, IOS )
      ELSE IF ( NAME .EQ. 'VEHSPD2' ) THEN ! Equivalent to CMPSPD2
        CALL REAL_ASSIGN( VALUE, U.VEHSPD2, MSG, IOS )
      ELSE IF ( NAME .EQ. 'OCCTYP2' ) THEN
        U.OCCTYP2 = VALUE
      ELSE IF ( NAME .EQ. 'OCCAGE2' ) THEN
        CALL INT_ASSIGN( VALUE, U.OCCAGE2, MSG, IOS )
        IF ( ( MSG ) .AND. ( IOS .EQ. 0 ) ) THEN
          IF ( U.OCCAGE2 .LT. 0 ) WRITE( *, 901 )
        END IF
      ELSE IF ( NAME .EQ. 'OCCSEX2' ) THEN
        U.OCCSEX2 = VALUE
        IF ( ( MSG ) .AND. ( IOS .EQ. 0 ) ) THEN
          IF ( .NOT. ANY_CHARS( U.OCCSEX2, 'MF ' ) )
     &       WRITE( *, 901 )
        END IF
      ELSE IF ( NAME .EQ. 'OCCWT2' ) THEN
        CALL REAL_ASSIGN( VALUE, U.OCCWT2, MSG, IOS )
        IF ( ( MSG ) .AND. ( IOS .EQ. 0 ) ) THEN
          IF ( U.OCCWT2 .LT. 0. ) WRITE( *, 901 )
        END IF
      ELSE IF ( NAME .EQ. 'DUMSIZ2' ) THEN
        U.DUMSIZ2 = VALUE
      ELSE IF ( NAME .EQ. 'RESTR12' ) THEN
        U.RESTR12 = VALUE
      ELSE IF ( NAME .EQ. 'RESTR22' ) THEN
        U.RESTR22 = VALUE
      ELSE IF ( NAME .EQ. 'CMP2_RESV' ) THEN
        U.CMP2_RESV = VALUE
      ELSE IF ( NAME .EQ. 'ICHAN' ) THEN
        CALL INT_ASSIGN( VALUE, U.ICHAN, MSG, IOS )
        IF ( ( MSG ) .AND. ( IOS .EQ. 0 ) ) THEN
          IF ( U.ICHAN .NE. 1 ) WRITE( *, 901 )
        END IF
      ELSE IF ( NAME .EQ. 'CHANFORM' ) THEN
        U.CHANFORM = VALUE
        IF ( ( MSG ) .AND. ( IOS .EQ. 0 ) ) THEN
          IF ( ( U.CHANFORM .NE. 'Y' ) .AND.
     &       ( U.CHANFORM .NE. 'X-Y' ) ) WRITE( *, 901 )
        END IF
      ELSE IF ( NAME .EQ. 'CURNO' ) THEN
        CALL INT_ASSIGN( VALUE, U.CURNO, MSG, IOS )
        IF ( ( MSG ) .AND. ( IOS .EQ. 0 ) ) THEN
          IF ( U.CURNO .LT. 0 ) WRITE( *, 901 )
        END IF
      ELSE IF ( NAME .EQ. 'CURNAM' ) THEN
        U.CURNAM = VALUE
        IF ( INT_CHK( VALUE ) ) U.CURNAM = RJUST( U.CURNAM )
      ELSE IF ( NAME .EQ. 'SENATT' ) THEN
        U.SENATT = VALUE
      ELSE IF ( NAME .EQ. 'SENLOC' ) THEN
        U.SENLOC = VALUE
      ELSE IF ( NAME .EQ. 'SENNUM' ) THEN
        CALL INT_ASSIGN( VALUE, U.SENNUM, MSG, IOS )
        IF ( ( MSG ) .AND. ( IOS .EQ. 0 ) ) THEN
          IF ( U.SENNUM .LT. 0 ) WRITE( *, 901 )
        END IF
      ELSE IF ( NAME .EQ. 'BANDNO' ) THEN
        CALL INT_ASSIGN( VALUE, U.BANDNO, MSG, IOS )
        IF ( ( MSG ) .AND. ( IOS .EQ. 0 ) ) THEN
          IF ( U.BANDNO .LT. 0 ) WRITE( *, 901 )
        END IF
      ELSE IF ( NAME .EQ. 'GAGNO' ) THEN
        CALL INT_ASSIGN( VALUE, U.GAGNO, MSG, IOS )
        IF ( ( MSG ) .AND. ( IOS .EQ. 0 ) ) THEN
          IF ( U.GAGNO .LT. 0 ) WRITE( *, 901 )
        END IF
      ELSE IF ( NAME .EQ. 'AXIS' ) THEN
        U.AXIS = VALUE
        IF ( ( MSG ) .AND. ( IOS .EQ. 0 ) ) THEN
          IF ( ( ( .NOT. ANY_CHARS( U.AXIS(1:1), 'XYZ' ) ) .OR.
     &       ( .NOT. ANY_CHARS( U.AXIS(2:2), 'GL' ) ) ) .AND.
     &       ( U.AXIS .NE. 'RS' ) .AND.
     &       ( .NOT. BLANK( U.AXIS ) ) ) WRITE( *, 901 )
        END IF
      ELSE IF ( NAME .EQ. 'YTYP' ) THEN
        U.YTYP = VALUE
      ELSE IF ( NAME .EQ. 'YUNITS' ) THEN
        U.YUNITS = VALUE
      ELSE IF ( NAME .EQ. 'XTYP' ) THEN
        U.XTYP = VALUE
      ELSE IF ( NAME .EQ. 'XUNITS' ) THEN
        U.XUNITS = VALUE
      ELSE IF ( NAME .EQ. 'STATUS' ) THEN
        U.STATUS = VALUE
      ELSE IF ( NAME .EQ. 'CURTYP' ) THEN
        U.CURTYP = VALUE
      ELSE IF ( NAME .EQ. 'CURDSC' ) THEN
        U.CURDSC = VALUE
      ELSE IF ( NAME .EQ. 'NFP' ) THEN
        CALL INT_ASSIGN( VALUE, U.NFP, MSG, IOS )
        IF ( ( MSG ) .AND. ( IOS .EQ. 0 ) ) THEN
          IF ( U.NFP .LT. NFP_L ) THEN
            WRITE( *, * )
     &         '*** WARNING - NFP limited to array bound: ', NFP_L
            U.NFP = NFP_L
          END IF
        END IF
      ELSE IF ( NAME .EQ. 'NLP' ) THEN
        CALL INT_ASSIGN( VALUE, U.NLP, MSG, IOS )
        IF ( ( MSG ) .AND. ( IOS .EQ. 0 ) ) THEN
          IF ( U.NLP .GT. NLP_U ) THEN
            WRITE( *, * )
     &         '*** WARNING - NLP limited to array bound: ', NLP_U
            U.NLP = NLP_U
          END IF
        END IF
      ELSE IF ( NAME .EQ. 'DEL' ) THEN
        CALL REAL_ASSIGN( VALUE, U.DEL, MSG, IOS )
        IF ( ( MSG ) .AND. ( IOS .EQ. 0 ) ) THEN
          IF ( U.DEL .LT. 0. ) WRITE( *, 901 )
        END IF
      ELSE IF ( NAME .EQ. 'INIVEL' ) THEN
        CALL REAL_ASSIGN( VALUE, U.INIVEL, MSG, IOS )
      ELSE IF ( NAME .EQ. 'PREF' ) THEN
        CALL REAL_ASSIGN( VALUE, U.PREF, MSG, IOS )
        IF ( ( MSG ) .AND. ( IOS .EQ. 0 ) ) THEN
          IF ( U.PREF .LT. 0. ) WRITE( *, 901 )
        END IF
      ELSE IF ( NAME .EQ. 'FCUT' ) THEN
        CALL REAL_ASSIGN( VALUE, U.FCUT, MSG, IOS )
        IF ( ( MSG ) .AND. ( IOS .EQ. 0 ) ) THEN
          IF ( U.FCUT .LT. 0. ) WRITE( *, 901 )
        END IF
      ELSE IF ( NAME .EQ. 'FCOR' ) THEN
        CALL REAL_ASSIGN( VALUE, U.FCOR, MSG, IOS )
        IF ( ( MSG ) .AND. ( IOS .EQ. 0 ) ) THEN
          IF ( U.FCOR .LT. 0. ) WRITE( *, 901 )
        END IF
      ELSE IF ( NAME .EQ. 'FSTP' ) THEN
        CALL REAL_ASSIGN( VALUE, U.FSTP, MSG, IOS )
        IF ( ( MSG ) .AND. ( IOS .EQ. 0 ) ) THEN
          IF ( U.FSTP .LT. 0. ) WRITE( *, 901 )
        END IF
      ELSE IF ( NAME .EQ. 'SCLFAC' ) THEN
        CALL REAL_ASSIGN( VALUE, U.SCLFAC, MSG, IOS )
      ELSE IF ( NAME .EQ. 'ID1' ) THEN
        CALL INT_ASSIGN( VALUE, U.ID1, MSG, IOS )
      ELSE IF ( NAME .EQ. 'ID2' ) THEN
        CALL INT_ASSIGN( VALUE, U.ID2, MSG, IOS )
      ELSE IF ( NAME .EQ. 'ID3' ) THEN
        CALL INT_ASSIGN( VALUE, U.ID3, MSG, IOS )
      ELSE IF ( NAME .EQ. 'ID4' ) THEN
        CALL INT_ASSIGN( VALUE, U.ID4, MSG, IOS )
      ELSE IF ( NAME .EQ. 'ID5' ) THEN
        CALL INT_ASSIGN( VALUE, U.ID5, MSG, IOS )
      ELSE IF ( NAME .EQ. 'RD1' ) THEN
        CALL REAL_ASSIGN( VALUE, U.RD1, MSG, IOS )
      ELSE IF ( NAME .EQ. 'RD2' ) THEN
        CALL REAL_ASSIGN( VALUE, U.RD2, MSG, IOS )
      ELSE IF ( NAME .EQ. 'RD3' ) THEN
        CALL REAL_ASSIGN( VALUE, U.RD3, MSG, IOS )
      ELSE IF ( NAME .EQ. 'RD4' ) THEN
        CALL REAL_ASSIGN( VALUE, U.RD4, MSG, IOS )
      ELSE IF ( NAME .EQ. 'RD5' ) THEN
        CALL REAL_ASSIGN( VALUE, U.RD5, MSG, IOS )
      ELSE IF ( NAME .EQ. 'CD1' ) THEN
        U.CD1 = VALUE
      ELSE IF ( NAME .EQ. 'CD2' ) THEN
        U.CD2 = VALUE
      ELSE IF ( NAME .EQ. 'CURV_RESV' ) THEN
        U.CURV_RESV = VALUE
      ELSE IF ( NAME .EQ. '?' ) THEN
        IOS = 2
        IF ( MSG ) CALL UDS_SPEC_LIST()
      ELSE ! Not a valid UDS field name
        IOS = 1
        IF ( MSG ) WRITE( *, '(/2A)' )
     &     ' *** Unrecognized UDS field name: ', NAME(:L_TRIM(NAME))
      END IF

  901 FORMAT(/' *** WARNING - Nonstandard field value')

      RETURN
      END



      SUBROUTINE UDS_SPEC_LIST()

!***********************************************************************
!* Lists UDS File Specification Field Names
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/17
!***********************************************************************


      ! List UDS field names
      WRITE( *, * )
      WRITE( *, * ) 'UDS Field List:   '//
     & '(strings shown as <name>*<length>)'

      WRITE( *, * )
      WRITE( *, * ) 'Header Record:'
      WRITE( *, * ) 'FILEVER*12, NCHAN, FILEFORM*10, '//
     & 'NUMFORM*5, DIMSYS*3,'
      WRITE( *, * ) 'TSTSRC*15, TSTNO, TSTNAM*12, TITLE*70, '//
     & 'TSTPRF*25, TSTREF*10, TSTCFN*3,'
      WRITE( *, * ) 'IMPANG, CLSSPD, CMPNO, CMPTYP*2, CMPDSC*30,'
      WRITE( *, * ) 'MAKE*15, MODEL*25, YEAR, BODY*25, '//
     & 'ENGINE*4, CMPWGT, CMPSPD,'
      WRITE( *, * ) 'OCCTYP*6, OCCAGE, OCCSEX*1, OCCWT, '//
     & 'DUMSIZ*2, RESTR1*20, RESTR2*20,'
      WRITE( *, * ) 'HIC, T1, T2, HICDTUP, CLIP3M, CSI, '//
     & 'AIS*1, HDR_RESV*132'

      WRITE( *, * )
      WRITE( *, * ) 'Component-2 Record:'
      WRITE( *, * ) 'CMPNO2, CMPTYP2*2, CMPDSC2*30, MAKE2*15, '//
     & 'MODEL2*25, YEAR2, BODY2*25,'
      WRITE( *, * ) 'ENGINE2*4, CMPWGT2, CMPSPD2, '//
     & 'OCCTYP2*6, OCCAGE2, OCCSEX2*1, OCCWT2,'
      WRITE( *, * ) 'DUMSIZ2*2, RESTR12*20, RESTR22*20, '//
     & 'CMP2_RESV*326'

      WRITE( *, * )
      WRITE( *, * ) 'Curve Header Record:'
      WRITE( *, * ) 'ICHAN, CHANFORM*10, '//
     & 'CURNO, CURNAM*12, SENATT*30, SENLOC*2, '//
     & 'SENNUM, BANDNO,'
      WRITE( *, * ) 'GAGNO, AXIS*2, YTYP*20, YUNITS*20, '//
     & 'XTYP*20, XUNITS*20, STATUS*20, CURTYP*20,'
      WRITE( *, * ) 'CURDSC*70, NFP, NLP, DEL, INIVEL, PREF, '//
     & 'FCUT, FCOR, FSTP, SCLFAC,'
      WRITE( *, * ) 'ID1, ID2, ID3, ID4, ID5, '//
     & 'RD1, RD2, RD3, RD4, RD5, CD1*20, CD2*70, CURV_RESV*68'

      RETURN
      END
