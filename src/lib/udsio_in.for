      SUBROUTINE UDSIO_INIT()

!***********************************************************************
!* Initializes UDS-1992 File COMMON Fields Before File Read
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************

      ! Headers
      INCLUDE 'udsiox.fi'


      ! Header initializations
      FILEVER = ' '
      NCHAN = 0
      FILEFORM = ' '
      NUMFORM = ' '
      DIMSYS = ' '
      TSTSRC = ' '
      TSTNO = 0
      TSTNAM = ' '
      TITLE = ' '
      TSTPRF = ' '
      TSTREF = ' '
      TSTCFN = ' '
      IMPANG = 0
      CLSSPD = 0.
      CMPNO = 0
      CMPTYP = ' '
      CMPDSC = ' '
      MAKE = ' '
      MODEL = ' '
      YEAR = 0
      BODY = ' '
      ENGINE = ' '
      CMPWGT = 0.
      CMPSPD = 0.
      OCCTYP = ' '
      OCCAGE = 0
      OCCSEX = ' '
      OCCWT = 0.
      DUMSIZ = ' '
      RESTR1 = ' '
      RESTR2 = ' '
      HIC = 0.
      T1 = 0.
      T2 = 0.
      HICDTUP = 0.
      CLIP3M = 0.
      CSI = 0.
      AIS = ' '
      HDR_RESV = ' '

      ! Component-2 initializations
      CMPNO2 = 0
      CMPDSC2 = ' '
      CMPTYP2 = ' '
      MAKE2 = ' '
      MODEL2 = ' '
      YEAR2 = 0
      BODY2 = ' '
      ENGINE2 = ' '
      CMPWGT2 = 0.
      CMPSPD2 = 0.
      OCCTYP2 = ' '
      OCCAGE2 = 0
      OCCSEX2 = ' '
      OCCWT2 = 0.
      DUMSIZ2 = ' '
      RESTR12 = ' '
      RESTR22 = ' '
      CMP2_RESV = ' '

      ! Curve header initializations
      ICHAN = 0
      CHANFORM = ' '
      CURNO = 0
      CURNAM = ' '
      SENATT = ' '
      SENLOC = ' '
      SENNUM = 0
      BANDNO = 0
      GAGNO = 0
      AXIS = ' '
      YTYP = ' '
      YUNITS = ' '
      XTYP = ' '
      XUNITS = ' '
      STATUS = ' '
      CURTYP = ' '
      CURDSC = ' '
      NFP = 0
      NLP = 0
      DEL = 0.
      INIVEL = 0.
      PREF = 0.
      FCUT = 0.
      FCOR = 0.
      FSTP = 0.
      SCLFAC = 0.
      ID1 = 0
      ID2 = 0
      ID3 = 0
      ID4 = 0
      ID5 = 0
      RD1 = 0.
      RD2 = 0.
      RD3 = 0.
      RD4 = 0.
      RD5 = 0.
      CD1 = ' '
      CD2 = ' '
      CURV_RESV = ' '

      RETURN
      END
