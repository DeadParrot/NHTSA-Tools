      SUBROUTINE REP_ENTRY( S, REPORT )

!***********************************************************************
!* Writes Standard UDS Entry to Report File
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
      INCLUDE 'file.fi'


      ! Arguments ______________________________________________________

      RECORD /UDS_SPEC/  S ! UDS_SPEC object

      RECORD /FILE/  REPORT ! Report file object


      ! Variables ______________________________________________________

      INTEGER IOS

      CHARACTER VEHICLE*30, CUTOFF*20


      ! Functions ______________________________________________________

      CHARACTER RJUST*12

      EXTERNAL RJUST


      ! Return if report file not open
      IF ( .NOT. REPORT.OPEN ) RETURN

      ! Set up entries
      VEHICLE = ' '
      WRITE( VEHICLE(1:4), '(I4)', IOSTAT=IOS ) S.YEAR
      VEHICLE(6:) = S.MAKE(:L_TRIM(S.MAKE))//' '//S.MODEL
      CUTOFF = ' '
      IF ( S.FCUT .GT. 0. ) THEN
        WRITE( CUTOFF, '(G20.6)', IOSTAT=IOS ) S.FCUT
      ELSE IF ( S.PREF .GT. 0. ) THEN
        WRITE( CUTOFF, '(G20.6)', IOSTAT=IOS ) S.PREF
      END IF

      ! Write the UDS entry
      WRITE( REPORT.UNIT,
     & '(A12,2X,5X,I1,5X,2X,A30,3X,A2,3X,A20,2X,A20,2X,A20)',
     & IOSTAT=IOS )
     & RJUST( S.TSTNAM ), S.VEHNO, VEHICLE, S.AXIS,
     & S.YTYP, S.XTYP, CUTOFF
      WRITE( REPORT.UNIT,
     & '(A12,2X,4X,A2,5X,2X,A30,3X,2X,3X,A20,2X,A20,2X,A20)',
     & IOSTAT=IOS )
     & RJUST( S.CURNAM ), S.SENLOC, S.SENATT,
     & S.YUNITS, S.XUNITS, S.STATUS

      RETURN
      END
