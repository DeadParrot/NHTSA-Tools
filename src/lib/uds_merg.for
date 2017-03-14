      SUBROUTINE UDS_MERGE( U, M )

!***********************************************************************
!* Merges Two UDS File Specifications
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      RECORD /UDS_SPEC/  U ! UDS_SPEC object to merge from

      RECORD /UDS_SPEC/  M ! UDS_SPEC object to merge to


      ! Variables ______________________________________________________

      INTEGER I


      ! Header fields
      IF ( U.FILEVER .NE. M.FILEVER ) M.FILEVER = 'UDS-1992'
      IF ( U.NCHAN .NE. M.NCHAN ) M.NCHAN = 0
      IF ( U.FILEFORM .NE. M.FILEFORM ) M.FILEFORM = ' '
      IF ( U.NUMFORM .NE. M.NUMFORM ) M.NUMFORM = ' '
      IF ( U.DIMSYS .NE. M.DIMSYS ) M.DIMSYS = ' '
      IF ( U.TSTSRC .NE. M.TSTSRC ) M.TSTSRC = ' '
      IF ( U.TSTNO .NE. M.TSTNO ) M.TSTNO = 0
      IF ( U.TSTNAM .NE. M.TSTNAM ) M.TSTNAM = ' '
      IF ( U.TITLE .NE. M.TITLE ) M.TITLE = ' '
      IF ( U.TSTPRF .NE. M.TSTPRF ) M.TSTPRF = ' '
      IF ( U.TSTREF .NE. M.TSTREF ) M.TSTREF = ' '
      IF ( U.TSTCFN .NE. M.TSTCFN ) M.TSTCFN = ' '
      IF ( U.IMPANG .NE. M.IMPANG ) M.IMPANG = 0
      CALL MERGE_SP( U.CLSSPD, M.CLSSPD )
      IF ( U.CMPNO .NE. M.CMPNO ) M.CMPNO = 0
      IF ( U.CMPTYP .NE. M.CMPTYP ) M.CMPTYP = ' '
      IF ( U.CMPDSC .NE. M.CMPDSC ) M.CMPDSC = ' '
      IF ( U.MAKE .NE. M.MAKE ) M.MAKE = ' '
      IF ( U.MODEL .NE. M.MODEL ) M.MODEL = ' '
      IF ( U.YEAR .NE. M.YEAR ) M.YEAR = 0
      IF ( U.BODY .NE. M.BODY ) M.BODY = ' '
      IF ( U.ENGINE .NE. M.ENGINE ) M.ENGINE = ' '
      CALL MERGE_SP( U.CMPWGT, M.CMPWGT )
      CALL MERGE_SP( U.CMPSPD, M.CMPSPD )
      IF ( U.OCCTYP .NE. M.OCCTYP ) M.OCCTYP = ' '
      IF ( U.OCCAGE .NE. M.OCCAGE ) M.OCCAGE = 0
      IF ( U.OCCSEX .NE. M.OCCSEX ) M.OCCSEX = ' '
      CALL MERGE_SP( U.OCCWT, M.OCCWT )
      IF ( U.DUMSIZ .NE. M.DUMSIZ ) M.DUMSIZ = ' '
      IF ( U.RESTR1 .NE. M.RESTR1 ) M.RESTR1 = ' '
      IF ( U.RESTR2 .NE. M.RESTR2 ) M.RESTR2 = ' '
      ! Set computed injury measures to zero
      M.HIC = 0.
      M.T1 = 0.
      M.T2 = 0.
      M.HICDTUP = 0.
      M.CLIP3M = 0.
      M.CSI = 0.
      IF ( U.AIS .NE. M.AIS ) M.AIS = ' '
      IF ( U.HDR_RESV .NE. M.HDR_RESV ) M.HDR_RESV = ' '

      ! Component-2 fields
      IF ( U.CMPNO2 .NE. M.CMPNO2 ) M.CMPNO2 = 0
      IF ( U.CMPTYP2 .NE. M.CMPTYP2 ) M.CMPTYP2 = ' '
      IF ( U.CMPDSC2 .NE. M.CMPDSC2 ) M.CMPDSC2 = ' '
      IF ( U.MAKE2 .NE. M.MAKE2 ) M.MAKE2 = ' '
      IF ( U.MODEL2 .NE. M.MODEL2 ) M.MODEL2 = ' '
      IF ( U.YEAR2 .NE. M.YEAR2 ) M.YEAR2 = 0
      IF ( U.BODY2 .NE. M.BODY2 ) M.BODY2 = ' '
      IF ( U.ENGINE2 .NE. M.ENGINE2 ) M.ENGINE2 = ' '
      CALL MERGE_SP( U.CMPWGT2, M.CMPWGT2 )
      CALL MERGE_SP( U.CMPSPD2, M.CMPSPD2 )
      IF ( U.OCCTYP2 .NE. M.OCCTYP2 ) M.OCCTYP2 = ' '
      IF ( U.OCCAGE2 .NE. M.OCCAGE2 ) M.OCCAGE2 = 0
      IF ( U.OCCSEX2 .NE. M.OCCSEX2 ) M.OCCSEX = ' '
      CALL MERGE_SP( U.OCCWT2, M.OCCWT2 )
      IF ( U.DUMSIZ2 .NE. M.DUMSIZ2 ) M.DUMSIZ2 = ' '
      IF ( U.RESTR12 .NE. M.RESTR12 ) M.RESTR12 = ' '
      IF ( U.RESTR22 .NE. M.RESTR22 ) M.RESTR22 = ' '
      IF ( U.CMP2_RESV .NE. M.CMP2_RESV ) M.CMP2_RESV = ' '

      ! Curve specification fields
      IF ( U.ICHAN .NE. M.ICHAN ) M.ICHAN = 0
      IF ( U.CHANFORM .NE. M.CHANFORM ) M.CHANFORM = ' '
      IF ( U.CURNO .NE. M.CURNO ) M.CURNO = 0
      IF ( U.CURNAM .NE. M.CURNAM ) M.CURNAM = ' '
      IF ( U.SENATT .NE. M.SENATT ) THEN ! Use common leading "words"
        I = 0
        DO WHILE ( ( I .LT. LEN( U.SENATT ) ).AND.
     &     ( U.SENATT(I+1:I+1) .EQ. M.SENATT(I+1:I+1) ) )
          I = I + 1
        END DO
        I = LEN_TRIM( U.SENATT(:I) )
        IF ( I .LT. LEN( U.SENATT ) ) THEN
          IF ( ( U.SENATT(I+1:I+1) .NE. ' ' ) .OR.
     &       ( M.SENATT(I+1:I+1) .NE. ' ' ) ) THEN
            DO WHILE ( ( I .GT. 0 ) .AND.
     &         ( U.SENATT(MAX(I,1):MAX(I,1)) .NE. ' ' ) )
              I = I - 1
            END DO
            I = LEN_TRIM( U.SENATT(:I) )
          END IF
        END IF
        IF ( I .GT. 0 ) THEN
          IF ( U.SENATT(I:I) .EQ. '-' ) I = I - 1
        END IF
        M.SENATT = U.SENATT(:I)
      END IF
      IF ( U.SENLOC .NE. M.SENLOC ) M.SENLOC = ' '
      IF ( U.SENNUM .NE. M.SENNUM ) M.SENNUM = 0
      IF ( U.BANDNO .NE. M.BANDNO ) M.BANDNO = 0
      IF ( U.GAGNO .NE. M.GAGNO ) M.GAGNO = 0
      IF ( U.AXIS .NE. M.AXIS ) THEN
        M.AXIS = ' '
        M.INIVEL = 0.
      END IF
      IF ( U.YTYP .NE. M.YTYP ) M.YTYP = ' '
      IF ( U.YUNITS .NE. M.YUNITS ) M.YUNITS = ' '
      IF ( U.XTYP .NE. M.XTYP ) M.XTYP = ' '
      IF ( U.XUNITS .NE. M.XUNITS ) M.XUNITS = ' '
      IF ( U.STATUS .NE. M.STATUS ) M.STATUS = ' '
      IF ( U.CURTYP .NE. M.CURTYP ) M.CURTYP = ' '
      IF ( U.CURDSC .NE. M.CURDSC ) M.CURDSC = ' '
      IF ( U.NFP .NE. M.NFP ) M.NFP = MAX( U.NFP, M.NFP )
      IF ( U.NLP .NE. M.NLP ) M.NLP = MIN( U.NLP, M.NLP )
      CALL MERGE_SP( U.DEL, M.DEL )
      CALL MERGE_SP( U.INIVEL, M.INIVEL )
      CALL MERGE_SP( U.PREF, M.PREF )
      CALL MERGE_SP( U.FCUT, M.FCUT )
      CALL MERGE_SP( U.FCOR, M.FCOR )
      CALL MERGE_SP( U.FSTP, M.FSTP )
      CALL MERGE_SP( U.SCLFAC, M.SCLFAC )
      IF ( U.ID1 .NE. M.ID1 ) M.ID1 = 0
      IF ( U.ID2 .NE. M.ID2 ) M.ID2 = 0
      IF ( U.ID3 .NE. M.ID3 ) M.ID3 = 0
      IF ( U.ID4 .NE. M.ID4 ) M.ID4 = 0
      IF ( U.ID5 .NE. M.ID5 ) M.ID5 = 0
      CALL MERGE_SP( U.RD1, M.RD1 )
      CALL MERGE_SP( U.RD2, M.RD2 )
      CALL MERGE_SP( U.RD3, M.RD3 )
      CALL MERGE_SP( U.RD4, M.RD4 )
      CALL MERGE_SP( U.RD5, M.RD5 )
      IF ( U.CD1 .NE. M.CD1 ) M.CD1 = ' '
      IF ( U.CD2 .NE. M.CD2 ) M.CD2 = ' '
      IF ( U.CURV_RESV .NE. M.CURV_RESV ) M.CURV_RESV = ' '

      RETURN
      END



      SUBROUTINE MERGE_SP( VAL_U, VAL_M )

!***********************************************************************
!* Merges Two Single Precision Values
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      REAL VAL_U ! Value to merge from

      REAL VAL_M ! Value to merge to


      ! Merge value
      IF ( EQUAL_SP( VAL_U, VAL_M, 2.E-7 ) ) THEN ! Essentially equal
        ! No change
      ELSE ! Different - Set to zero
        VAL_M = 0.
      END IF

      RETURN
      END
