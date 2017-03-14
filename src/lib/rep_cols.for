      SUBROUTINE REP_COLS( REPORT )

!***********************************************************************
!* Writes Standard UDS Entry Column Headings to Report File
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/10/18
!***********************************************************************

      ! Headers
      INCLUDE 'file.fi'


      ! Arguments ______________________________________________________

      RECORD /FILE/  REPORT ! Report file object


      ! Variables ______________________________________________________

      INTEGER IOS


      ! Write the column headings
      IF ( REPORT.OPEN )
     & WRITE( REPORT.UNIT, '(//A,6(2X,A)/A,6(2X,A))', IOSTAT=IOS )
     & 'Test / Curve', 'Veh. / Occ.',
     & 'Veh./Comp. / Sensor Attachment', 'Axis',
     & '  Independent Data  ', '   Dependent Data   ',
     & '  Cutoff / Status   ',
     & '------------', '-----------',
     & '------------------------------', '----',
     & '--------------------', '--------------------',
     & '--------------------'

      RETURN
      END
