      PROGRAM ATB2UDS

!***********************************************************************
!* Name    :  ATB2UDS
!*
!* Purpose :  Extract motion/force histories from the ATB (CVS 20)
!*            post-processing time history data file and output these
!*            to UDS files.
!*
!* Inputs  :  The user is prompted for the following:
!*            . Name of ATB output file
!*            . ATB run number to be associated with the UDS output files
!*            . A yes/no reply to output groups of corresponding time histories
!*
!* Outputs :  The requested histories are output to UDS files.  Please
!*            refer to the external ATB2UDS.DOC file for output file
!*            naming convention.
!*
!* Pertinent Information:
!*            This program is intended as a post-processor to ATB, CVS
!*            version 20. The structure of the unformatted time history
!*            data file output by ATB was deduced from looking at the
!*            OUTPUT and HEDING CVS subroutines. With later versions of
!*            CVS, the structure of this file may be changed, without
!*            notice in the external documentation, as this file is
!*            intended for internal post-processing by the CVS software.
!*            Thus, it would be wise, actually necessary, to look at the
!*            above mentioned CVS subroutines and possibly others for
!*            future upgrades of the ATB2UDS program.
!*
!* Language: Fortran
!*
!* Author :   Randa Radwan Samaha - 1987/03
!*            Seth Grimes: Update to ATB v.4.2 - 1992/03
!*            Stuart G. Mentzer: Tools integration - 1994/03, 1995/05
!*
!* Date: 2001/03/03
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'


      ! Variables ______________________________________________________

      LOGICAL exit_flag

      INTEGER ncurve, NFP_P, NLP_P, LIST_UNIT, ATB_UNIT, IOS

      PARAMETER ( NFP_P = -2000, NLP_P = 50000 )

      REAL y1(NFP_P:NLP_P), y2(NFP_P:NLP_P), y3(NFP_P:NLP_P),
     & y4(NFP_P:NLP_P)

      CHARACTER run_date*10, run_time*8

      RECORD /UDS_Y/  U

      COMMON / UNITS / LIST_UNIT, ATB_UNIT


      WRITE( *, '(//20X,A//)' )
     & '**  ATB (v.4.2) to UDS File Conversion  **'

!... Open output listing file. This file provides a cross-reference
!    between the time history curves and the corresponding UDS files.
!
      CALL OPEN_FS( LIST_UNIT, 'ATB2UDS.out', 'W', IOS )
      IF ( IOS .NE. 0 ) THEN
        WRITE( *, * ) '*** Listing file open failed'
        STOP ' '
      END IF
      call DATEISO( run_date )
      call TIMEISO( run_time )
      WRITE( LIST_UNIT, '(//20X,A//)' )
     & '**  ATB (v.4.2) to UDS File Conversion  **'
      write( LIST_UNIT, 20 ) run_date, run_time
  20  format(////,' Run Date: ', a, ' at ', a)


      exit_flag = .false. ! error status flag
      ncurve    = 0 ! number of curves output

      call read_atb( exit_flag, y1(0), NLP_P )
      if ( .not. exit_flag ) call set_indices
      if ( .not. exit_flag ) call set_uds_header( U, exit_flag )
      if ( .not. exit_flag ) then
        call write_motion_data( ncurve, y1, y2, y3, y4,
     &     NFP_P, NLP_P, U )
        call write_plane_forces( ncurve, y1, y2, y3, y4,
     &     NFP_P, NLP_P, U )
      end if

      if ( ncurve .eq. 0 ) write( LIST_UNIT,  30)
   30 format(//,' No UDS files generated for this run !!')
      close( LIST_UNIT )
      end


!***********************************************************************
!*
!* Name       :   read_ATB
!*
!* Purpose    :   Read the ATB Fortran unit 8 output file and compute the
!*                the time increment at which data is available.
!*
!* Called By  :   ATB2UDS main
!*
!* Subroutines Called : None
!*
!* Inputs :
!*      From User -   the name of the ATB output file
!*
!* Outputs    :
!*      Argument -  Exit_flag set if user can't provide a valid ATB output file
!*
!*      To Commons -   The elements of the following commons are set:
!*                      ATB_control
!*                      ATB_titles
!*                      ATB_data (except index_nt array)
!*
!* Programmer :      ASGI - Randa Radwan  1987/03
!*
!***********************************************************************

      SUBROUTINE read_atb( exit_flag, time, NLP_P )

      ! Headers
      INCLUDE 'atb2uds.fi'
      INCLUDE 'uds_fxn.fi'

      logical exit_flag, done, valid

      character r*1

      integer NLP_P, npts_total, factor, IOS, I, J, K,
     & LIST_UNIT, ATB_UNIT

      real del, del_max, del_tmp, rem, time(0:NLP_P)

!4.2    50 changed to 65 for ATB version 4.2 vvvvvvvvvvvvvvvvvvvvvvvvvvv
      real*8 usec, Dummy(14,65)

      COMMON / UNITS / LIST_UNIT, ATB_UNIT


!... Get the ATB output file name and make sure that it is readable
!
 100  WRITE( *, 10 )
  10  format(/, ' Please enter ATB output file name  [FOR008.dat]',
     & t50, ': ',$)

      done = .false.
      do while ( .not. done )
        READ( *, 20, end=11 ) atb_file
  20   format(a)
        if ( atb_file .eq. ' ' ) atb_file = 'FOR008.dat'
        CALL OPEN_US( ATB_UNIT, atb_file, 'R', IOS )
        if ( ios .eq. 0 ) then
          done = .true.
        else
          WRITE( *, 40 )
   40     format(' *** Open failure , please reenter file name',
     &       t50, ': ',$)
        end if
      end do
!
!... Read ATB controlling parameters.
!
!4.2    This read is OK for ATB version 4.2.
      read( ATB_UNIT, err=22 ) nseg,njnt,npl,nblt,nbag,nveh,ngrnd,
     & npanel,mnpl,mnblt,mnseg,mnbag,mpl,mblt,mseg,mbag

!2.0    ATB version 2.0 vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
!2.0       read( ATB_UNIT, err=22) date,coment,vpsttl,bdyttl,bltttl,
!2.0     &  plttl,bagttl,seg,joint,unitl,unitm,unitt,nsg,msg,xsg,
!2.0     &  nhrnss,nbltph,nptspb,nsd,msdm,msdn
!2.0    ATB version 2.0 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!4.2    Read modified for ATB version 4.2 vvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
      read( ATB_UNIT, err=22 ) date,coment,vpsttl,bdyttl,bltttl,
     & plttl,bagttl,seg,joint,unitl,unitm,unitt,nsg,msg,xsg,MCG,
     & MCGIN,KREF,nhrnss,nbltph,nptspb,nsd,msdm,msdn
!
!... Skim over the data to verify that the file is uncorrupted.
!... Find the maximum time increment and the number of available points.
!... These points are output at each CVS/ATB integrator step, rather
!... than every delta_t as specified by the user in DT (card A.4).
!
      npts_total = 0
      del_max = 0.0
      i = 1
      read( ATB_UNIT, err=22, end=300 ) nt, usec,
     & ( ( dummy(j,k), j=1, 14 ), k=1, nt )
      time(i) = REAL( usec )
  200 i = i + 1
      if ( i .gt. nlp_p ) go to 300
      read( ATB_UNIT, err=22, end=300 ) nt, usec,
     & ( ( dummy(j,k), j=1, 14 ), k=1, nt )
      time(i) = REAL( usec )
      del = time(i) - time(i-1)
      if ( del_max .lt. del ) del_max = del
      goto 200
  300 npts_total = i - 1
      close( ATB_UNIT )

!...  Make sure it is not empty
!
      if ( npts_total .eq. 0 ) then
        WRITE( *, 50 ) atb_file(:L_TRIM(atb_file))
   50   format(' *** ',a,' file does not contain any data!')
        WRITE( *,60 )
   60   format(/' Would you like to specify another file ??',
     &          '(y,<n>)',t50,': ',$)
        READ( *, 20 ) r
        if ( ( r .eq. 'Y' ) .or. ( r .eq. 'y' ) ) goto 100
        exit_flag = .true.
        return
      end if

!...    Echo ATB file name to output listing file (unit 1)
!
      write( LIST_UNIT, 65 ) atb_file(:L_TRIM(atb_file))
   65 format(//' *** ',a,' ATB output file is input.',/)

!
!...    Find the actual time increment (delta_t), at which data is available,
!...    and the number of resulting data points.
!
!............  NOTE :: The actual delta_t has to be equal or greater than
!............          the maximum time increment.
!


!.....  Find the index of the first time value greater or equal to del_max
!
      i = 0
      done = .false.
      do while ( ( .not. done ) .and. ( i .lt. npts_total ) )
        i = i + 1
        if ( ( time(i) .gt. del_max ) .or.
     &     ( abs(time(i) - del_max ) .lt. 1.0e-4 ) ) done = .true.
      end do

      done = .false.
      do while ( ( .not. done ) .and. ( i .le. npts_total ) )
        del_tmp = time(i)
        rem = MOD( time(npts_total) + 1.e-5, del_tmp )
        if ( abs(rem) .gt. 1.0e-4 ) then
          i = i + 1
        else
          factor = NINT( time(npts_total) / del_tmp )
          j = 1
          valid = .true.

          do while ( valid .and. ( j .le. factor ) )
            k = i

            do while ( valid .and. ( k .le. npts_total ) )
              rem = MOD( time(k) + 1.e-5, del_tmp )
                if ( abs(rem) .gt. 1.e-4 ) then
                if ( time(k) .lt. del_tmp ) then
                  k = k + 1
                else
                  valid = .false.
                end if
              else
                j = j + 1
                del_tmp = j * time(i)
              end if
            end do

          end do

          if ( valid ) then
            delta_t = time(i)
            npts = factor + 1
            done = .true.
          else
            i = i + 1
          end if

        end if
      end do
      IF ( NPTS .GT. NLP_P+1 ) THEN
        WRITE( *, * )
     &     '*** Number of points limited by ATB2UDS array size'
        NPTS = NLP_P + 1
      END IF

      return

!..............................................................................
!
!...    User wants out!
!
   11 exit_flag = .true.
      WRITE( *, 80 )
   80 format(/' Bye...')
      return

!...    Error during reading ATB output file!
!
   22 close( ATB_UNIT )
      WRITE( *, 70 ) atb_file(:L_TRIM(atb_file))
   70 format(' *** Error reading ',a,
     & ' file. It is not in ATB output format!')
      WRITE( *, 60 )
      read( *, 20 ) r
      if ( ( r .eq. 'Y' ) .or. ( r .eq. 'y' ) ) goto 100
      exit_flag = .true.
      return

      end


!***********************************************************************
!*
!* Name       :   SET_INDICES
!*
!* Purpose    :   Set the start indices, for the motion/joint/force
!*                histories, in the DATA array of the write routines.
!*
!* Called By  :   ATB2UDS main
!*
!* Subroutines called: None
!*
!* Inputs :
!*      From ATB_control common: nsg, npl, and mnpl
!*
!* Outputs    :
!*      To ATB_data common  index_nt
!*
!*
!* Programmer :      ASGI - Randa Radwan  1987/03
!*
!***********************************************************************

      SUBROUTINE set_indices

      ! Headers
      INCLUDE 'atb2uds.fi'

      integer ind, ncontact, I

!
!...    Set the start indices for the different histories in the DATA
!...    array (See the WRITE routines).
!
!...........................................................................
!
!...    The different histories, if available, are stored consecutively in
!...    the columns of the DATA array. They are stored in the following order:
!
!      1 -     linear accelerations
!      2 -     linear velocities
!      3 -     linear displacements
!      4 -     angular accelerations
!      5 -     angular velocities
!      6 -     angular displacements
!      7 -     joint parameters
!4.2    ATB version 4.2 vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
!      8 -     segment unit traces ???
!      9 -     joint forces & torques
!      10 -    body properties
!      11 -    plane forces
!      12 -    belt forces
!      13 -    harness -belt endpoint forces
!      14 -    spring damper forces
!      15 -    segment contact forces
!      16 -    airbag forces
!4.2    ATB version 4.2 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!2.0    ATB version 2.0 vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
!      8 -     plane forces
!      9 -     belt forces
!      10 -    harness -belt endpoint forces
!      11 -    spring damper forces
!      12 -    segment contact forces
!      13 -    airbag forces
!2.0    ATB version 2.0 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!...
!...    Histories for 2 or 3 points/segments/joints/contacts etc. are stored
!...    in a single column of the DATA array.
!...
!...    The INDEX_NT values correspond to the second dimension (column index)
!...    of the DATA array.
!
!...........................................................................
!
!
!...    Start index to motion histories. Linear/angular histories for 3
!..     points/segments are stored in one column.

      ind = 0
      do i = 1, 6
        if ( nsg(i) .eq. 0 ) then ! no ith history is output
          index_nt (i) = -1
        else
          index_nt (i) = ind + 1
          if ( mod(nsg(i),3) .eq. 0 ) then
            ind = ind + nsg(i)/3 ! integer divide
          else
            ind = ind + nsg(i)/3 + 1
          end if
        end if
      end do

!...    Start index to joint parameters. Histories for 2 joints are stored in
!...    one column.
!
      if ( nsg(7) .eq. 0 ) then
        index_nt(7) = -1
      else
        index_nt(7) = ind + 1
        ind = ind + nsg(7)/2 + mod( nsg(7), 2 )
      end if

!...    Start index to plane contact forces histories. Histories for 2
!...    contacts are stored in one column.
!
      if ( npl .eq. 0 ) then
        index_nt(8) = -1
      else
        ncontact = 0
        do i = 1, npl
          ncontact = ncontact + mnpl(i)
        end do
        if ( ncontact .eq. 0 ) then
          index_nt(8) = -1
        else
          index_nt(8) = ind + 1
          ind = ind + ncontact/2 + mod( ncontact, 2 )
        end if
      end if

      return
      end


!***********************************************************************
!*
!* Name       :   set_uds_header
!*
!* Purpose    :   Set the values of UDS header variables that
!*                are common to all the UDS files to be output.
!*
!* Called By  :   ATB2UDS main
!*
!* Subroutines called:
!*
!* - UDSINIT
!*
!* Inputs     :   The user is prompted for a run identification number
!* Outputs    :   Various variables in the UDS commons are set.
!*
!*
!* Programmer :      ASGI - Randa Radwan  1987/03
!*
!***********************************************************************

      SUBROUTINE set_uds_header( U, exit_flag )

      ! Headers
      INCLUDE 'atb2uds.fi'
      INCLUDE 'platform.fi'
      INCLUDE 'uds_str.fi'


      ! Variables ______________________________________________________

      RECORD /UDS_Y/  U

      logical exit_flag

      integer I, IOS, IOS_W


!...  Set up general UDS header variables.
!
      CALL UDS_INIT( U )
      U.FILEVER = 'UDS-1992'
      U.NCHAN = 1
      U.FILEFORM = 'Y'
      U.NUMFORM = NUMFORM_P
      U.DIMSYS = 'ENG'
      U.TSTPRF = 'ATB'
      U.TSTSRC = 'SIMULATION'
      WRITE( U.TSTREF, '(2a4,a2)' ) ( coment(i), i=1, 3 )
      U.ICHAN = 1
      U.CHANFORM = 'Y'
      U.XTYP = 'TIME'
      U.XUNITS = 'SECONDS'
      U.STATUS = 'COMPUTED'
      U.CURTYP = 'TIME SERIES'
      U.NFP = 0
      U.NLP = NPTS - 1
      U.DEL = 0.001 * delta_t

!... Get run identification number
!
      WRITE( *, 10 )
  10  format(/' Please enter an ATB identification number - to be',
     &       ' placed',/,' in the TSTNO field (integer)',t50,': ',$)
 100  READ( *, 20, iostat=ios ) U.TSTNO
  20  format(BN,i4)
      if ( ( ios .ne. 0 ) .or. ( U.TSTNO .eq. 0 ) ) then
        if ( ios .eq. -1 ) then
          WRITE( *, * ) ' Bye.. Bye..'
          exit_flag = .true.
          return
        else
          WRITE( *, 30 )
  30     format(' *** Invalid entry !! Try again',t50,': ',$)
          goto 100
        end if
      end if
      write( U.TSTNAM, '(I12)', iostat=IOS_W ) U.TSTNO

      return

      end


!***********************************************************************
!*
!* Name       :   write_motion_data
!*
!* Purpose    :   Read the motion data histories from the ATB output
!*                file, set the UDS header variables for each curve
!*                and output the corresponding UDS files.
!*
!* Called By  :   ATB2UDS main
!*
!* Inputs :
!*      From user :-  user is prompted if he/she wishes to output
!*                    the different groups of motion histories if
!*                    available from the ATB file
!*      From Commons :-  various elements from the ATB and UDS commons
!*                       are accessed
!*
!* Outputs    :
!*      Argument :-  NCURVE contains the number of curves (i.e.
!*                   UDS files) output so far.
!*      Files    the requested UDS files are output to the
!*               user's current directory!
!*
!* Programmer :      ASGI - Randa Radwan  1987/03
!*
!***********************************************************************

      SUBROUTINE write_motion_data( ncurve, x_out, y_out,
     & z_out, res_out, NFP_P, NLP_P, U )

      ! Headers
      INCLUDE 'atb2uds.fi'
      INCLUDE 'uds_str.fi'


      ! Variables ______________________________________________________

      RECORD /UDS_Y/  U
      RECORD /UDS_CONTROL/  C

      integer ncurve, idummy, ind_desc, NFP_P, NLP_P, IOS,
     & I, I1, I2, J, J1, J2, K, KK,
     & LIST_UNIT, ATB_UNIT

      character r*1, file_out*20, motion(6)*20,
     & mn_desc(0:2)*12

      real x_out(NFP_P:NLP_P), y_out(NFP_P:NLP_P),
     & z_out(NFP_P:NLP_P), res_out(NFP_P:NLP_P)

!4.2    50 changed to 65 for ATB version 4.2 vvvvvvvvvvvvvvvvvvvvvvvvvvv
      real*8 dummy, DP_data(14,65)

      COMMON / UNITS / LIST_UNIT, ATB_UNIT

      SAVE motion, mn_desc

      data  motion / 'Linear Acceleration', 'Linear Velocity',
     & 'Linear Displacement', 'Angular Acceleration',
     & 'Angular Velocity', 'Angular Displacement' /

      data  mn_desc / 'DISPLACEMENT', 'ACCELERATION', 'VELOCITY' /


      CALL UDS_CONTROL_INIT( C )

      CALL OPEN_US( ATB_UNIT, atb_file, 'R', IOS )

!.................................................................
!.................................................................
!
!...    Loop for the 6 group of motion histories:
!...
!...   kk = 1 linear accelerations
!...   kk = 2 linear velocities
!...   kk = 3 linear displacements
!...   kk = 4 angular accelerations
!...   kk = 5 angular velocities
!...   kk = 6 angular displacements

      do kk = 1, 6

        U.YTYP = ' '
        U.YUNITS = ' '
        if ( index_nt(kk) .eq. -1 ) then
          WRITE( *, 10 ) motion (kk)
  10     format(/' No ',a,' histories output by ATB !')
        else
          WRITE( *, 20 ) motion(kk)
  20     format(/' Would you like ',a,' data written to UDS ',
     &       'files ?  (<y>,n)',t75,': ',$)
          READ( *, 30 ) r
  30     format(a)

          if ( ( r .ne. 'n' ) .and. ( r .ne. 'N' ) ) then

!.................................................................
!.... User wants this group output
!.................................................................
!
!
!.............  Set UDS units and data type for the given group
!
            U.YTYP = mn_desc(mod(kk,3))
            if ( kk .eq. 1 ) then
              U.YUNITS = 'G''S'
            else if ( kk .eq. 2 ) then
              WRITE( U.YUNITS, 40 ) unitl, unitt
  40         format(a4,'/',a4)
            else if ( kk .eq. 3 ) then
              WRITE( U.YUNITS, 45 ) unitl
  45         format(a4)
            else if ( kk .eq. 4 ) then
              WRITE( U.YUNITS, 50 ) unitt
  50         format('DEG/',a4,'**2')
            else if ( kk .eq. 5 ) then
              WRITE( U.YUNITS, 60 ) unitt
  60         format('DEGREES/',a4)
            else ! kk=6
              U.YUNITS = 'DEGREES'
            end if

!.............  Loop for sets of three histories (as available in ATB file)
!
            k = index_nt(kk) - 1
            do j1 = 1, nsg(kk), 3

              k = k + 1
              j2 = MIN( j1+2, nsg(kk) )

!................ Loop for each history
!
              do j = j1, j2

                rewind( ATB_UNIT )
                read( ATB_UNIT ) ! skip over controlling
                read( ATB_UNIT ) ! parameters

!.....................Loop to read history for given point/segment
!
                do i = 0, npts-1
 100             read( ATB_UNIT ) idummy, dummy,
     &               ( ( DP_data(i1,i2), i1=1, 14 ), i2=1, nt )

!...................... We want the data at delta_t intervals only!
                  if ( abs(dummy-i*delta_t) .gt. 1.d-4 ) goto 100
                  x_out(i) = REAL( DP_data(4*(j-j1)+1,k) )
                  y_out(i) = REAL( DP_data(4*(j-j1)+2,k) )
                  z_out(i) = REAL( DP_data(4*(j-j1)+3,k) )
                  res_out(i) = REAL( DP_data(4*(j-j1)+4,k) )
                end do

!.................... Set UDS header variables/file name particular
!.................... to a single history
!
                WRITE( U.SENATT, 45 ) seg(iabs(msg(j,kk)))
                if ( kk .le. 3 ) then
                  WRITE( U.CURDSC, 70 ) motion(kk),
     &               (xsg(i,j,kk),i=1,3), U.SENATT
  70             format(a,' of pt (',f7.3,',',f7.3,',',f7.3,
     &               ') of ',a4)
                  ind_desc = 62
                  U.rd1 = REAL( xsg(1,j,kk) )
                  U.rd2 = REAL( xsg(2,j,kk) )
                  U.rd3 = REAL( xsg(3,j,kk) )
                else
                  WRITE( U.CURDSC, 80 ) motion(kk), U.SENATT,
     &               ( date(i), i = 1, 3 )
  80             format(a,' of ',a4,' *** ',3a4)
                  ind_desc = 27
                end if

!.................... X - COMPONENT
                if ( ( kk .eq. 1 ) .or. ( kk .eq. 4 ) ) then
                  U.AXIS = 'XL'
                else
                  U.AXIS = 'XG'
                end if
                U.CURNO = NCURVE + 1
                WRITE( U.CURNAM, '(I12)' ) U.CURNO

!.................... Output curve
!
                DO I = U.NFP, U.NLP
                  U.Y(I) = x_out(I)
                END DO
                CALL FN_DEF( U, file_out )
                file_out(8:8) = 'X'
                CALL FN_INCR( file_out )
                call UDS_WRITE( file_out, U, C, IOS )
                WRITE( *, 95 ) file_out
  95           format(' .... Data written to ',a,'.')

!... ................ Echo to cross_reference listing file
                write( LIST_UNIT, * )
                write( LIST_UNIT, 105 ) file_out
 105           format(1x,a)
!
!.................... Y - COMPONENT
                U.AXIS(1:1) = 'Y'
                U.CURNO = NCURVE + 2
                WRITE( U.CURNAM, '(I12)' ) U.CURNO
!
!.................... Output curve
!
                DO I = U.NFP, U.NLP
                  U.Y(I) = y_out(I)
                END DO
                CALL FN_DEF( U, file_out )
                file_out(8:8) = 'Y'
                CALL FN_INCR( file_out )
                call UDS_WRITE( file_out, U, C, IOS )
                WRITE( *, 95 ) file_out
!
!.....................Echo to listing file
                write( LIST_UNIT, 105 ) file_out
!
!.................... Z - COMPONENT
                U.AXIS(1:1) = 'Z'
                U.CURNO = NCURVE + 3
                WRITE( U.CURNAM, '(I12)' ) U.CURNO
!
!.................... Output curve
!
                DO I = U.NFP, U.NLP
                  U.Y(I) = z_out(I)
                END DO
                CALL FN_DEF( U, file_out )
                file_out(8:8) = 'Z'
                CALL FN_INCR( file_out )
                call UDS_WRITE( file_out, U, C, IOS )
                WRITE( *, 95 ) file_out
!
!.................... Echo to listing file
                write( LIST_UNIT, 105 ) file_out

!.................... RESULTANT
                U.AXIS = 'RS'
                U.CURNO = NCURVE + 4
                WRITE( U.CURNAM, '(I12)' ) U.CURNO
!
!.................... Output curve
!
                DO I = U.NFP, U.NLP
                  U.Y(I) = res_out(I)
                END DO
                CALL FN_DEF( U, file_out )
                file_out(8:8) = 'R'
                CALL FN_INCR( file_out )
                call UDS_WRITE( file_out, U, C, IOS )
                WRITE( *, 95 ) file_out
!
!.................. Echo to listing file
                write( LIST_UNIT, 110 ) file_out,
     &             U.CURDSC(1:ind_desc), msg(j,kk)
 110           format(1x,a,' --> ',a,/,1x,t20,
     &             '(segment #',i3,')')
!
!...................  Update the number of curve output so far !
!
                ncurve = ncurve + 4
              end do
            end do
          end if
        end if
      end do

      return

      end



!***********************************************************************
!* Name       :   write_plane_forces
!*
!* Purpose    :   Read the plane forces histories from the ATB output file,
!*                set the UDS header variables for each curve and output
!*                the corresponding UDS files.
!*
!* Called By  :   ATB2UDS main
!*
!* Inputs :
!*      Argument :- NCURVE contains the number of curves (i.e.
!*                  UDS files) output so far.
!*      From user :-  user is prompted if he/she wishes to output
!*                    plane forces data available from the ATB file
!*
!*      From Commons :-  various elements from the ATB and UDS commons
!*                       are accessed
!*
!* Outputs    :
!*      Argument :- NCURVE contains the updated number of curves
!*                  output so far.
!*      Files :- the requested UDS files are output to the
!*               user's default directory!
!*
!* Programmer :      ASGI - Randa Radwan  1987/03
!***********************************************************************

      SUBROUTINE write_plane_forces( ncurve, defl_out, norm_out,
     & fric_out, res_out, NFP_P, NLP_P, U )

      ! Headers
      INCLUDE 'atb2uds.fi'
      INCLUDE 'uds_str.fi'


      ! Variables ______________________________________________________

      RECORD /UDS_Y/  U
      RECORD /UDS_CONTROL/  C

      logical done

      integer ncurve, idummy, nplane(150), nsegment(150),
     & ncontact, endp, NFP_P, NLP_P, IOS,
     & I, I1, I2, J, J1, J2, K, LIST_UNIT, ATB_UNIT

      character r*1, file_out*20, plane*20

      real defl_out(NFP_P:NLP_P), norm_out(NFP_P:NLP_P),
     & fric_out(NFP_P:NLP_P), res_out(NFP_P:NLP_P), contact_check

!4.2    50 changed to 65 for ATB version 4.2 vvvvvvvvvvvvvvvvvvvvvvvvvvv
      real*8 dummy, DP_data(14,65)

      COMMON / UNITS / LIST_UNIT, ATB_UNIT

      CALL UDS_CONTROL_INIT( C )

      CALL OPEN_US( ATB_UNIT, atb_file, 'R', IOS )


      if ( index_nt(8) .eq. -1 ) then
        WRITE( *, 10 )
  10   format(/' No plane contact forces output by ATB !')
      else
        WRITE( *, 20 )
  20   format(/' Would you like plane contact forces data ',
     &          'written to UDS files ?  (<y>,n)',t75,': ',$)
        READ( *, 30 ) r
  30   format(a)

        if ( ( r .ne. 'n' ) .and. ( r .ne. 'N' ) ) then

!.................................................................
!.... User wants the plane forces output
!.................................................................
!
          k = index_nt(8) - 1 ! start index for the plane forces
!
!.............. Find the total number of contacts and the segment and
!.............. plane number for each contact.
!
          ncontact  = 0
          do i = 1, npl
            if ( mnpl(i) .ne. 0 ) then
              do j = 1, mnpl(i)
                ncontact = ncontact + 1
                nplane(ncontact) = i
                nsegment(ncontact) = mpl(2,j,i)
              end do
            end if
          end do
!
!............ Loop for sets of two contacts (as available in ATB file)
!
          do i1 = 1, ncontact, 2
            k = k + 1
            i2 = MIN( i1+1, ncontact )
!
!............... Loop for each contact
!
            do i = i1, i2

              rewind( ATB_UNIT )
              read( ATB_UNIT )  ! Skip over
              read( ATB_UNIT )  ! controlling parameters

!.................. Encode contact plane name and find out its length
!
              WRITE( plane, 35 ) ( plttl(j,nplane(i)), j=1,5 )
  35         format(5a4)
              done = .false.
              j = 20
              endp = j
              do while ( ( .not. done ) .and. ( j .gt. 1 ) )
                if ( plane(j:j) .eq. ' ' ) then
                  endp = j - 1
                  j = j - 1
                else
                  done = .true.
                end if
              end do

!.................. Set some UDS header variables.
              WRITE( U.CURDSC, 40 ) plane(1:endp), seg(nsegment(i))
  40          format(20x, a, ' and ', a4, ' contact force')
              write( U.SENATT, 50 ) seg(nsegment(i))
  50          format(a4)

!.................. Loop to read force history for given contact.
!
              contact_check = 0.
              do j = 0, npts-1
 100           read( ATB_UNIT ) idummy, dummy,
     &          ((DP_data(j1,j2), j1 = 1, 14), j2 = 1, nt)

!..................... We want the data at delta_t intervals only!
                if ( abs(dummy-j*delta_t) .gt. 1.d-4 ) goto 100
                defl_out(j) = REAL( DP_data(7*(i-i1)+1,k) )
                norm_out(j) = REAL( DP_data(7*(i-i1)+2,k) )
                fric_out(j) = REAL( DP_data(7*(i-i1)+3,k) )
                res_out(j)  = REAL( DP_data(7*(i-i1)+4,k) )
                contact_check = contact_check + res_out(j)
              end do

!.................. If data is zero, do not output a UDS file
!
              if ( contact_check .lt. 1.0e-4 ) then
                write( LIST_UNIT, 55) U.CURDSC(21:65), nplane(i),
     &             nsegment(i)
  55           format(/,' ### No contact - zero data ---> ',
     &             a,/,1x,t33,'(panel #',i3,' vs segment #',i3,')')
              else

!..................... Set UDS header variables/file name particular to
!..................... a single curve
!
!..................... DEFLECTION
                U.AXIS = 'XG'
                U.YTYP = 'DEFLECTION'
                U.CURDSC(1:19) = 'DEFLECTION'
                WRITE( U.YUNITS, 50 ) unitl
                U.CURNO = NCURVE + 1
                WRITE( U.CURNAM, '(I12)' ) U.CURNO

!...................... Output curve
!
                DO J = U.NFP, U.NLP
                  U.Y(J) = defl_out(J)
                END DO
                CALL FN_DEF( U, file_out )
                file_out(8:8) = 'P'
                CALL FN_INCR( file_out )
                call UDS_WRITE( file_out, U, C, IOS )
                WRITE( *, 70 ) file_out
  70           format(' .... Data written to ',a,'.')

!...................... Echo to cross-reference listing file
                write( LIST_UNIT, * )
                write( LIST_UNIT, 80 ) file_out
  80           format(1x,a,5x,'deflection data')

!.....................  NORMAL FORCE COMPONENT
                U.YTYP = 'FORCE'
                U.CURDSC(1:19) = 'NORMAL-COMPONENT'
                WRITE( U.YUNITS, 50 ) unitm
                U.CURNO = NCURVE + 2
                WRITE( U.CURNAM, '(I12)' ) U.CURNO

!.................... Output curve
!
                DO J = U.NFP, U.NLP
                  U.Y(J) = norm_out(J)
                END DO
                CALL FN_DEF( U, file_out )
                file_out(8:8) = 'P'
                CALL FN_INCR( file_out )
                call UDS_WRITE( file_out, U, C, IOS )
                WRITE( *, 70 ) file_out
!...................... Echo to listing file
                write( LIST_UNIT, 90 ) file_out
  90           format(1x,a,5x,'normal force component')

!.....................  FRICTION FORCE COMPONENT
                U.AXIS(1:1) = 'Y'
                U.CURDSC(1:19) = 'FRICTION-COMPONENT'
                U.CURNO = NCURVE + 3
                WRITE( U.CURNAM, '(I12)' ) U.CURNO

!.................... Output curve
!
                DO J = U.NFP, U.NLP
                  U.Y(J) = fric_out(J)
                END DO
                CALL FN_DEF( U, file_out )
                file_out(8:8) = 'P'
                CALL FN_INCR( file_out )
                call UDS_WRITE( file_out, U, C, IOS )
                WRITE( *, 70 ) file_out
!...................... Echo to listing file
                write( LIST_UNIT, 110 ) file_out
 110           format(1x,a,5x,'friction force component')
!
!.....................  RESULTANT
                U.AXIS = 'RS'
                U.CURDSC(1:19) = 'RESULTANT'
                U.CURNO = NCURVE + 4
                WRITE( U.CURNAM, '(I12)' ) U.CURNO

!.................... Output curve
!
                DO J = U.NFP, U.NLP
                  U.Y(J) = res_out(J)
                END DO
                CALL FN_DEF( U, file_out )
                file_out(8:8) = 'P'
                CALL FN_INCR( file_out )
                call UDS_WRITE( file_out, U, C, IOS )
                WRITE( *, 70 ) file_out
!...................... Echo to listing file
                write( LIST_UNIT, 120 ) file_out, U.CURDSC(1:10),
     &             U.CURDSC(21:65), nplane(i), nsegment(i)
 120           format(1x,a,' --> ',a,' - ',a,/,1x,t20,
     &             '(panel #',i3,' vs segment #',i3,')')

!...................... Update the number of curve output so far !
                ncurve = ncurve + 4
              end if
            end do
          end do
        end if
      end if

      return

      end
