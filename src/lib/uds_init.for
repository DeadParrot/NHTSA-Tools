      SUBROUTINE UDS_INIT( U )

!***********************************************************************
!* Initializes UDS-1992 File Fields
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2001/03/03
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'


      ! Arguments ______________________________________________________

      RECORD /UDS_SPEC/  U ! UDS_SPEC object


      ! Header record
      U.FILEVER = ' '
      U.NCHAN = 0
      U.FILEFORM = ' '
      U.NUMFORM = ' '
      U.DIMSYS = ' '
      U.TSTSRC = ' '
      U.TSTNO = 0
      U.TSTNAM = ' '
      U.TITLE = ' '
      U.TSTPRF = ' '
      U.TSTREF = ' '
      U.TSTCFN = ' '
      U.IMPANG = 0
      U.CLSSPD = 0.
      U.CMPNO = 0
      U.CMPTYP = ' '
      U.CMPDSC = ' '
      U.MAKE = ' '
      U.MODEL = ' '
      U.YEAR = 0
      U.BODY = ' '
      U.ENGINE = ' '
      U.CMPWGT = 0.
      U.CMPSPD = 0.
      U.OCCTYP = ' '
      U.OCCAGE = 0
      U.OCCSEX = ' '
      U.OCCWT = 0.
      U.DUMSIZ = ' '
      U.RESTR1 = ' '
      U.RESTR2 = ' '
      U.HIC = 0.
      U.T1 = 0.
      U.T2 = 0.
      U.HICDTUP = 0.
      U.CLIP3M = 0.
      U.CSI = 0.
      U.AIS = ' '
      U.HDR_RESV = ' '

      ! Component-2 record
      U.CMPNO2 = 0
      U.CMPTYP2 = ' '
      U.CMPDSC2 = ' '
      U.MAKE2 = ' '
      U.MODEL2 = ' '
      U.YEAR2 = 0
      U.BODY2 = ' '
      U.ENGINE2 = ' '
      U.CMPWGT2 = 0.
      U.CMPSPD2 = 0.
      U.OCCTYP2 = ' '
      U.OCCAGE2 = 0
      U.OCCSEX2 = ' '
      U.OCCWT2 = 0.
      U.DUMSIZ2 = ' '
      U.RESTR12 = ' '
      U.RESTR22 = ' '
      U.CMP2_RESV = ' '

      ! Curve header record
      U.ICHAN = 0
      U.CHANFORM = ' '
      U.CURNO = 0
      U.CURNAM = ' '
      U.SENATT = ' '
      U.SENLOC = ' '
      U.SENNUM = 0
      U.BANDNO = 0
      U.GAGNO = 0
      U.AXIS = ' '
      U.YTYP = ' '
      U.YUNITS = ' '
      U.XTYP = ' '
      U.XUNITS = ' '
      U.STATUS = ' '
      U.CURTYP = ' '
      U.CURDSC = ' '
      U.NFP = 0
      U.NLP = 0
      U.DEL = 0.
      U.INIVEL = 0.
      U.PREF = 0.
      U.FCUT = 0.
      U.FCOR = 0.
      U.FSTP = 0.
      U.SCLFAC = 0.
      U.ID1 = 0
      U.ID2 = 0
      U.ID3 = 0
      U.ID4 = 0
      U.ID5 = 0
      U.RD1 = 0.
      U.RD2 = 0.
      U.RD3 = 0.
      U.RD4 = 0.
      U.RD5 = 0.
      U.CD1 = ' '
      U.CD2 = ' '
      U.CURV_RESV = ' '

      RETURN
      END



      SUBROUTINE UDS_CONTROL_INIT( C )

!***********************************************************************
!* Initializes UDS-1992 File Fields
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2001/03/03
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'


      ! Arguments ______________________________________________________

      RECORD /UDS_CONTROL/  C ! UDS_CONTROL object


      ! Initialize fields
      C.FILEVER = ' '
      C.FILEFORM = ' '
      C.NUMFORM = ' '
      C.DIMSYS = ' '
      C.CHANFORM = ' '
      C.NO_MSG = .FALSE.
      C.SPECS_ONLY = .FALSE.
      C.FILL_X = .FALSE.
      C.FN_INCR = .FALSE.

      RETURN
      END
