      PROGRAM UDSMAKE

!***********************************************************************
!* This is a program template for converting time series files to the
!* NHTSA UDS-1992 file format.  Link with the NHTSA UDS library.
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/18
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi' ! UDS STRUCTURE declarations
      INCLUDE 'platform.fi' ! UDS STRUCTURE declarations


      ! Variables ______________________________________________________

      RECORD /UDS_Y/  U
      RECORD /UDS_CONTROL/  C

      INTEGER IOS

      CHARACTER IN_FILE*255, OUT_FILE*255


      ! Initializations
      CALL UDS_CONTROL_INIT( C )

      ! Get file name
  101 WRITE( *, '(/A,45X,A/'' >> '',$)' )
     & ' Time series file name?', '[done]'
      READ( *, '(A)', END=201 ) IN_FILE
      IF ( IN_FILE .EQ. ' ' ) GO TO 101

      ! Read the time series file here

      ! Load the data into the U.Y array here
      ! U.Y(0) should be the time zero value


      ! Set UDS fields
      CALL UDS_INIT( U )
      U.FILEVER = 'UDS-1992'
      U.NCHAN = 1
      U.FILEFORM = 'Y' ! Y-series file
      U.NUMFORM = NUMFORM_P ! Use local platform floating point format
      U.DIMSYS = 'MET' ! Can also use 'SI' for SI or 'ENG' for English
      U.ICHAN = 1
      U.CHANFORM = 'Y' ! Y-series channel

      ! Set AXIS to 'XG' for X-global, 'XL' for X-local, and so forth
      U.AXIS = 'XG'

      ! Set YTYP to 'ACCELERATION', 'VELOCITY', 'DISPLACEMENT', etc.
      U.YTYP = 'ACCELERATION'
      U.YUNITS = 'G''S'
      U.XTYP = 'TIME'
      U.XUNITS = 'SECONDS'
      U.CURTYP = 'TIME SERIES'

      ! Set NFP to the index of the first point (NFP_L <= NFP <= NLP)
      U.NFP = 0

      ! Set NLP to the index of the last point (NFP <= NLP <= NLP_U)
      U.NLP = 1000

      ! Time step (sec)
      U.DEL = .0001

      ! Set INIVEL to the initial velocity along the data axis
      !  DIMSYS = 'MET' => km/h
  !  DIMSYS = 'SI' => m/s
  !  DIMSYS = 'ENG' => mi/h
      U.INIVEL = 56.

      ! See the UDS_VARS.DOC file for meanings of the other UDS fields


      ! Write the UDS file
  102 WRITE( *, '(/A/'' >> '',$)' ) ' Output UDS file name?'
      READ( *, '(A)', END=101 ) OUT_FILE
      CALL UDS_WRITE( OUT_FILE, U, C, IOS )
      IF ( IOS .NE. 0 ) GO TO 102


      ! Loop for next file
      GO TO 101

  201 END
