      PROGRAM MADY2UDS

!***********************************************************************
!* Convert MADYMO time history file to a UDS file
!*
!* Language: Fortran
!*
!* Author: S. Summers
!*
!* History:
!*  1992/08/30  First version
!*  1993/02/??  Added support for file name Ranges
!*  1994/01/??  Stuart G. Mentzer: Added Tools framework
!*
!* Date: 2001/03/03
!*
!* Notes:
!* . Some of the output data units are not standard NHTSA units
!***********************************************************************

      IMPLICIT NONE

      ! Headers
      INCLUDE 'platform.fi'
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'


      ! Variables ______________________________________________________

      RECORD /UDS_Y/  U
      RECORD /UDS_CONTROL/  C

      Integer nlp_m, MaxLocn, MaxMeas, Maxfiles, IN_UNIT
      Parameter ( nlp_m=nlp_u, MaxLocn=20, maxmeas=20, Maxfiles=20 )

      Character*80 Mname, Uname, Mtitle(2)
      Character*80 LocName(150), cjunk
      Character*80 MeasName(20)
      Integer Nloc, Nlocout, locout(MaxLocn)
      Integer Nmeas, Nmeasout, measout(Maxmeas)
      Integer i, j, k, IOS, nout, maxrow, ierr
      Real udata(0:nlp_m,Maxfiles)
      real time, tmin, tmax, tprev, tnext, scale_fac
      real mdata(150,20), pdata(150,20)


      ! Initializations
      CALL UDS_CONTROL_INIT( C )
      tmin = HUGE( tmin )
      tmax = 0.0


!  read in the name for the MADYMO Data file
5     IOS = 1
      Do While ( IOS .ne. 0 )
        Write( *, 10 )
10      format(/,' Enter MADYMO File Name, CR or ^Z to end >> '$)
        read( *, '(a)', end=10000 ) Mname
        If ( LEN_TRIM( Mname ) .le. 0 ) goto 10000
        CALL OPEN_FS( IN_UNIT, Mname, 'R', ios )
        if ( IOS .ne. 0 ) print *, ' >>> ERROR Opening File <<<'
      End Do
      read( IN_UNIT, '(a80)' ) Mtitle(1)
      read( IN_UNIT, '(a80)' ) Mtitle(2)
      read( IN_UNIT, * ) Nloc, Nmeas
      if ( Nloc .gt. 150 ) stop ' >>> Max of 150 Locations <<<'
      if ( Nmeas .gt. 20 ) stop ' >>> Max of 20 measurements <<<'
      do i = 1, Nloc
        read( IN_UNIT, '(a)' ) LocName(i)
        call str_upcase( LocName(I), LocName(i) )
      end do
      do i = 1, Nmeas
        read( IN_UNIT, '(a)' ) MeasName(i)
        call str_upcase( MeasName(I), MeasName(i) )
      end do

!  ask Which Rows and columns to output in the UDS file
50    Call madyloc( LocName, Nloc, Nlocout, Locout, ierr )
      if ( ierr .ne. 0 ) goto 5
      Call madymeas( MeasName, Nmeas, Nmeasout, Measout, ierr )
      if ( ierr .ne. 0 ) goto 5
      if ( Nlocout*Nmeasout .gt. Maxfiles ) then
        write( *, 60 ) NlocOut*NmeasOut, MaxFiles
60      format(/,10x,' >>> Too Many files, ',i4,
     &   ',  Max is ',i3,/)
        goto 50
      end if

!  search through the file to find the minimum time increment.
      Maxrow = 0
      do While ( Maxrow .lt. nlp_m )
        read( IN_UNIT, *, end=100, err=100 ) time
        if ( Maxrow .gt. 1 ) then
          U.del = time - tprev
          if ( U.del .lt. tmin ) tmin = U.del
        else if ( Maxrow .eq. 1 ) then
          tmin = time - tprev
        end if
        Tprev = time

!    read in the data for time zero
        do i = 1, Nloc
          read( IN_UNIT, *, end=100, err=100 )
     &     ( mdata(i,j), j = 1, Nmeas )
        end do
        maxrow = maxrow + 1
        tmax = time
      end do

!  Try to use the minimum time increment for the UDS file
!  If this results in too many data points, then cut it back appropriately
100   U.NLP = INT( tmax / tmin - 1 )
      If ( U.NLP .gt. nlp_m ) then
        print *, ' >>> Too many data points - increasing time step <<'
        tmin = tmax / nlp_m
      end if
      U.del = tmin * 0.001

! determine if we should use a scale factor based on the measurement selected
! also set some of the UDS Parameters to the appropriate units

!  go back to the beginning of the file and prepare to read the data
      rewind IN_UNIT
      do i = 1, 3
        read( IN_UNIT, '(a)') Cjunk
      end do
      do i = 1, Nloc
        read( IN_UNIT, '(a)') Cjunk
      end do
      do i = 1, Nmeas
        read( IN_UNIT, '(a)') Cjunk
      end do

!  read the data from the madymo file
      read( IN_UNIT, *, end=200, err=200 ) tprev
      do i = 1, Nloc
        read( IN_UNIT, *, end=200, err=200 ) (pdata(i,j),j=1,Nmeas)
      end do
      nOut = 0
      do i = 1, NlocOUt
        do j = 1, NmeasOut
          Nout = Nout + 1
          udata(0,Nout) = Pdata(LocOut(i),MeasOut(j))
        end do
      end do

      read( IN_UNIT, *, end=200, err=200 ) tnext
      do i = 1, Nloc
        read( IN_UNIT, *, end=200, err=200 ) (mdata(i,j),j=1,Nmeas)
      end do

      Maxrow = 1
      time = 0
      Do While ( time .lt. tmax )
        time = time + tmin
        If ( time .gt. Tnext ) then
          tprev = tnext
          read( IN_UNIT, *, end=200, err=200 ) tnext
          do i = 1, Nloc
            do j = 1, Nmeas
              Pdata(i,j) = mdata(i,j)
            end do
            read( IN_UNIT, *, end=200, err=200 )
     &       ( mdata(i,j), j=1, Nmeas )
          end do
        end if

        Nout = 0
        do i = 1, NLocOut
          do j = 1, Nmeasout
            nout = nout + 1
            Udata(MaxRow,nout) =  Pdata(locOut(i),measout(j)) +
     &       ( (time-tprev) / (tnext-tprev)
     &       * ( Mdata(locout(i),measOut(j) ) -
     &       Pdata(locOut(i),measout(j)) ) )
          end do
        end do
        Maxrow = Maxrow + 1
      end do

200   close(1)
      If ( Maxrow .le. 0 ) then
        print *, ' >>> ERROR reading data file <<<'
        go to 5
      end if

!  define UDS parameters
300   nout = 0
      do i = 1, NlocOut
        do j = 1, NmeasOut
          Nout = nout + 1
305     write( *, 310 ) Locname(LocOut(i)), MeasName(MeasOut(j))
310     format(/,a,/,a,/,/,' UDS File Name : ',$)
          read( *, '(a)', end=400, err=305 ) Uname
          if ( LEN_TRIM( Uname ) .le. 0 ) goto 400

          U.YUNITS = 'UNKNOWN'
          scale_fac = 1.0
          if ( index( measname(MeasOut(j)), 'ACCELER' ) .gt. 0 ) then
            U.YTYP = 'ACCELERATION'
            U.YUNITS = 'G''S'
            scale_fac = 0.101832994
          else if ( index( measname(measOut(j)), 'VELOCITY' ) .gt. 0 )
     &     then
            U.YTYP = 'VELOCITY'
            U.YUNITS = 'METERS/SEC'
          else if ( index( measname(MeasOut(j)), 'DISPL' ) .gt. 0 )
     &     then
            U.YTYP = 'DISPLACEMENT'
            U.YUNITS = 'METERS'
          else if ( index( measname(MeasOut(j)), 'PENETR' ) .gt. 0 )
     &     then
            U.YTYP = 'DISPLACEMENT'
            U.YUNITS = 'METERS'
          else if ( index( measname(MeasOut(j)), 'FORCE' ) .gt. 0 )
     &     then
            U.YTYP = 'FORCE'
            U.YUNITS = 'NEWTONS'
          end if
          U.nfp = 0
          U.nlp = MaxRow - 1
          call str_upcase( locname(LocOut(i)), locname(LocOut(i)) )
          call str_upcase( measname(MeasOut(j)),
     &     measname(MeasOut(j)) )
          U.YTYP = MeasName(MeasOut(j))(1:15)
          U.CURTYP = 'TIME SERIES'
          U.SENATT = LocName(LocOut(i))(1:25)
          U.TITLE = Mtitle(1)(1:70)
          U.TSTSRC = 'SIMULATION'
          U.TSTPRF = 'MADYMO 3D'
          U.TSTNAM = Mtitle(2)(1:12)
          U.TSTCFN = 'SIM'
          U.TSTNO = 0
          U.IMPANG = 0
          U.CLSSPD = 0.
          U.DIMSYS = 'MET'
          U.FILEVER = 'UDS-1992'
          U.NCHAN = 1
          U.ICHAN = 1
          U.CHANFORM = 'Y'
          U.FILEFORM = 'Y'
          U.NUMFORM = NUMFORM_P
          U.XTYP = 'TIME'
          U.XUNITS = 'SECONDS'
          do k = U.nfp, U.nlp
            U.Y(k) = scale_fac * udata(k,nout)
          end do

          CALL UDS_WRITE( Uname, U, C, IOS )
400     continue
        end do
      end do
5000  goto 5

10000 end



!***********************************************************************
      Subroutine madyloc(LocName, Nloc, Nlocout, Locout, ierr)
!***********************************************************************
      Implicit None
      INCLUDE 'uds_fxn.fi'

      character LocName(*)*80
      character*80 string, substring
      Integer Nloc, Nlocout, LocOut(*), i, ierr
      Integer Nstart, Nstop
      Logical GetSub
      external getsub

10    ierr = 0
      NlocOut = 0
      if ( Nloc .eq. 1 ) then
        write( *, 20 ) LocName(1)(:l_trim(locName(1)))
20      format(/,' >>> Using Location ',a,' <<<')
        NLocOut = 1
        LocOut(1) = 1
        goto 60
      end if

30    write( *, 40 )
40    format(/,' Enter location(s) for UDS Data (ie: 1,2,5-8,...)')
      do i = 1, Nloc
        write( *, 45 ) i, LocName(i)(:l_trim(LocName(i)))
      end do
45    format(10x,i2,2x,a)
      write( *, 50 )
50    format(2x,' Location >> ',$)
      read( *, '(a)', err=30, end=100 ) string
      if ( LEN_TRIM( string ) .le. 0 ) goto 100

!  check for wildcard reply
      if ( index( string, '*' ) .gt. 0 ) then
        NLocOut = Nloc
        do i =1, Nloc
          LocOut(i) = i
        end do
        return
      end if

      do while ( GetSub( string, substring ) )
        if ( LEN_TRIM( substring ) .le. 0 ) goto 60
        if (index(substring,'-').gt.0) then
          i = index(substring,'-')
          read( substring(1:i-1), * ) nstart
          read( substring(i+1:), * ) nstop
          do i = nstart, nstop
            NlocOut = NlocOut + 1
            LocOut(NlocOut) = i
          end do
        else
          read( substring, * ) Nstart
          NlocOut = NlocOut + 1
          LocOut(NlocOut) = Nstart
        end if
      end do
60    return

100   ierr=1
      return
      end



!***********************************************************************
      logical function Getsub(string, substring)
!***********************************************************************
      implicit none
      INCLUDE 'uds_fxn.fi'

      Character string*(*), substring*(*)
      integer i

      if ( LEN_TRIM( string ) .le. 0 ) then
        Getsub = .false.
        return
      end if
      i = index( string, ',' )
      if ( i .gt. 0 ) then
        substring = string(1:i-1)
        string = string(i+1:)
      else
        substring = string
        string = ' '
      end if
      GetSub = .true.
      return
      end



!***********************************************************************
      Subroutine madyMeas(MeasName, NMeas, NMeasout, Measout, ierr)
!***********************************************************************
      Implicit None
      INCLUDE 'uds_fxn.fi'

      character MeasName(*)*(*)
      character*80 string, substring
      Integer NMeas, NMeasout, MeasOut(*), i, ierr
      Integer Nstart, Nstop
      Logical GetSub
      external getsub

10    ierr = 0
      NMeasOut = 0
      if ( NMeas .eq. 1 ) then
        write( *, 20 ) MeasName(1)(:l_trim(MeasName(1)))
20    format(/,' >>> Using Measurement ',a,' <<<')
        NMeasOut = 1
        MeasOut(1) = 1
        goto 150
      end if
70    write( *, 75 )
75    format(/,' Enter Measurement(s) for UDS Data (ie: 1,2,5-8...)')
      do i = 1, Nmeas
        write( *, 80 ) i, MeasName(i)(:l_trim(MeasName(i)))
      end do
80    format(10x,i2,2x,a)
      write( *, 85 )
85    format(2x,' Measurement >> ',$)
      read( *, '(a)', err=70, end=200 ) string
      if ( LEN_TRIM( string ) .le. 0 )  goto 200

!  check for wildcard reply
      if ( index( string, '*' ) .gt. 0 ) then
        NMeasOut = NMeas
        do i =1, NMeas
          MeasOut(i) = i
        end do
        return
      end if

      do while ( GetSub( string, substring ) )
        if ( LEN_TRIM( substring ) .le. 0 ) goto 150
        if ( index( substring, '-' ) .gt. 0 ) then
          i = index( substring, '-' )
          read( substring(1:i-1), * ) nstart
          read( substring(i+1:), * ) nstop
          do i = nstart, nstop
            NMeasOut = NMeasOut + 1
            MeasOut(NMeasOut) = i
          end do
        else
          read( substring, * ) Nstart
          NMeasOut = NMeasOut + 1
          MeasOut(NMeasOut) = Nstart
        end if
      end do
150   return

200   ierr=1
      return
      end
