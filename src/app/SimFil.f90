      PROGRAM SIMFIL

!***********************************************************************
!* Simultaneous Filtering of Relevant Pulse Integrals/Derivatives
!*
!* Developed by Stuart G. Mentzer for:
!*
!*   National Highway Traffic Safety Administration
!*   U.S. Department of Transportation
!*   Washington, DC  20590
!*   U.S.A.
!*
!* Documentation:
!*
!*   "The SIMFIL Program: Simultaneous Filtering"
!*   Stuart G. Mentzer
!*   U.S. Dept. of Transportation Report No. DOT HS 807 731
!*   (February 1991)
!*   (available from NTIS)
!*  and
!*   "The SIMFIL Program v.2: Simultaneous Filtering"
!*   Stuart G. Mentzer
!*   U.S. Dept. of Transportation Draft Report
!*   (updated version of above report)
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* History:
!*  SimFil v.2: Added enhanced 3-domain tail smoothing and full-range
!*   subampled/interpolated (unfiltered) outputs
!*  This version supports template/hitlist and batch processing
!*
!* Date: 2017/02/06
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'
      INCLUDE 'file.fi'

      RECORD /UDS_Y/  U
      RECORD /UDS_CONTROL/  C
      RECORD /FILE/  HIT, REPORT, OUTPUT_LIST

      LOGICAL ALL_BATCH, NO_BATCH, NO_INCR, ONE_PASS, NEW_HIT, EOF, &
       BATCH, GROUP, NEW_GROUP, INI_VAL

      INTEGER NUM_ARGS, MAX_ARGS, IOS, I, NDD, NDDH, NDDHM, NDD2

      PARAMETER ( MAX_ARGS = 12 )

      CHARACTER CL_ARG(0:MAX_ARGS)*255, PROG_NAME*8, &
       TEMPLATE_STR*255, DIR_OUT*255, &
       FILE_NAME*255, &
       PROMPT*79

      LOGICAL ASM, VSM, DSM

      INTEGER NFP_S, NLP_S, NFP_I, NLP_I, NFREQ, KC, KS, KS_MAX, NSUB

      ! Array range - can be adjusted as desired
      PARAMETER ( NFP_S = -10000, NLP_S = 100000 )

      REAL A(NFP_S:NLP_S), V(NFP_S:NLP_S), D(NFP_S:NLP_S), &
       AF(NFP_S:NLP_S), VF(NFP_S:NLP_S), DF(NFP_S:NLP_S), &
       SC(0:2*NLP_S), SINF(0:4*NLP_S), COSF(0:4*NLP_S), &
       BA(0:NLP_S), BAW(0:NLP_S), BVW(0:NLP_S), BDW(0:NLP_S), &
       H(0:NLP_S), YO(0:NLP_S/2), YE(0:NLP_S/2), DEL_I

      EQUIVALENCE ( YO, AF(0) ) ! Space
      EQUIVALENCE ( YE, VF(0) ) ! saving
      EQUIVALENCE (  H, DF(0) ) ! device

      DOUBLE PRECISION PI, ALPHA, AVFAC, VDFAC, ADFAC, FC, CF, SF, &
       FSTOP, FCUT_DEF, FSTP_DEF, DTT, STT, FTT, &
       FTD, FS0, FSN, FREQ(100), STF, FTF, DTF, &
       TWON, PIT, PIT2, BAVFAC, BADFAC, W0, FMD, FMC, &
       A0, AN, V0, VN,  D0, DN, A0A, ASA, B1A, B2A, V0V, A0V, ASV, &
       B1V, B2V, D0D, V0D, A0D, ASD, B1D, B2D, CORF, STPF, &
       FCORI, FSTPI

      CHARACTER ATYP*25, VTYP*25, DTYP*25, AFL, VFL, DFL, &
       AUN*20, VUN*20, DUN*20, YUNITS_I*20, V0_C*25, D0_C*25, &
       FCUT_DEF_C*7, FSTP_DEF_C*7, PTYP, FS0_C*25, FSN_C*25, &
       FREQ_C(100)*25, FTD_C*25, STF_C*25, FTF_C*25, DTF_C*25, &
       DFTTYP, OF_CTRL, AFIL*255, VFIL*255, DFIL*255, &
       DEF_FILE*255, OUT_FILE*255, FILMOD, SHOW_INT*8


      ! Initializations
      PROG_NAME = 'SimFil'
      CALL FILE_INIT( HIT )
      CALL GET_CL( CL_ARG, NUM_ARGS, MAX_ARGS )
      CALL SIMFIL_CL( CL_ARG, NUM_ARGS, PROG_NAME, INI_VAL )
      CALL PROC_CL( CL_ARG, NUM_ARGS, PROG_NAME, TEMPLATE_STR, &
       ALL_BATCH, NO_BATCH, NO_INCR, REPORT, OUTPUT_LIST, DIR_OUT, &
       ONE_PASS, .TRUE., .TRUE. )
      PROMPT = 'UDS file name/template/hitlist?'
      PROMPT(74:) = '[done]'
      CALL UDS_CONTROL_INIT( C )
      C.CHANFORM = 'Y' ! Only accept Y-series file input
      PI = 4.D0 * DATAN( 1.D0 )
      ALPHA = DLOG( PI / DACOS( 2. * ( 10.D0**(-.15D0) ) - 1.D0 ) ) / DLOG( 4.D0 )
      BA(0) = 0.
      BAW(0) = 0.
      BVW(0) = 0.
      BDW(0) = 0.

      WRITE( *, '(//14X,A//)' ) '**  SimFil v.2 - Simultaneous Filtering Program  **'

      ! Read the file name/template/hitlist
  100 CALL FILE_PROMPT( TEMPLATE_STR, PROMPT, DIR_OUT, &
       ONE_PASS, HIT, NEW_HIT, FILE_NAME, EOF )
      IF ( ( FILE_NAME == ' ' ) .OR. ( EOF ) ) GO TO 199 ! Done

      ! Batch setup
      CALL BAT_SET( ALL_BATCH, NO_BATCH, NEW_HIT, HIT, BATCH )
      CALL GROUP_SET( HIT, NEW_HIT, BATCH, 2, GROUP, NEW_GROUP )

      ! Group prompts
      IF ( NEW_GROUP ) THEN

        ! Set up generic info for group runs
  101   CALL GROUP_SETUP( ATYP, VTYP, DTYP, AFL, VFL, DFL, &
         INI_VAL, V0_C, V0, D0_C, D0, FCUT_DEF_C, FSTP_DEF_C, EOF )
        IF ( EOF ) THEN
          CALL MSG_WRITE( '*** Operation skipped', REPORT )
          CALL FILE_DEL( HIT )
          GO TO 190
        END IF

        ! Get process specifications
        CALL PROCESS_SETUP( ATYP, VTYP, DTYP, AFL, VFL, DFL, &
         FCUT_DEF_C, FSTP_DEF_C, DTT, STT, FTT, PTYP, FTD, FTD_C, &
         ASM, VSM, DSM, FS0, FS0_C, FSN, FSN_C, &
         NFREQ, FREQ, FREQ_C, STF, STF_C, FTF, FTF_C, DTF, DTF_C, &
         DFTTYP, OF_CTRL, AFIL, VFIL, DFIL, BATCH, GROUP, EOF )
        IF ( EOF ) THEN
          CALL MSG_WRITE( '*** Operation skipped', REPORT )
          CALL FILE_DEL( HIT )
          GO TO 190
        END IF

      END IF

      ! Read UDS file
      CALL REP_COLS( REPORT )
      CALL UDS_GET( FILE_NAME, U, C, REPORT, IOS )
      IF ( IOS /= 0 ) GO TO 190

      ! Check file fields
      IF ( ( U.XTYP /= 'TIME' ) .OR. &
       ( U.XUNITS /= 'SECONDS' ) ) THEN
        CALL MSG_WRITE( '*** ERROR - Not a time series', REPORT )
        GO TO 190
      END IF
      IF ( U.NFP < NFP_S ) THEN
        WRITE( SHOW_INT, '(I8)', IOSTAT=IOS ) NFP_S
        CALL MSG_WRITE( '*** Lower array index '// &
         'limited to SimFil bound of: '//SHOW_INT, REPORT )
        U.NFP = NFP_S
      END IF
      IF ( U.NFP > 0 ) THEN
        CALL MSG_WRITE( '*** ERROR - Lower array index > 0', REPORT )
        GO TO 190
      END IF
      IF ( U.NLP > NLP_S ) THEN
        WRITE( SHOW_INT, '(I8)', IOSTAT=IOS ) NLP_S
        CALL MSG_WRITE( '*** Upper array index '// &
         'limited to SimFil bound of: '//SHOW_INT, REPORT )
        U.NLP = NLP_S
      END IF
      IF ( U.NLP < 3 ) THEN
        CALL MSG_WRITE( '*** ERROR - Upper array index < 3 ', REPORT )
        GO TO 190
      END IF

      ! Set up file information
      CALL SETUP( U, AVFAC, VDFAC, ADFAC, ATYP, VTYP, DTYP, &
       AFL, VFL, DFL, AUN, VUN, DUN, YUNITS_I, FC, CF, SF, &
       NFP_I, NLP_I, DEL_I, DTT, STT, FTT, FSTOP, &
       FCUT_DEF, FCUT_DEF_C, FSTP_DEF, FSTP_DEF_C, INI_VAL, &
       A, A0, V, V0_C, V0, D, D0_C, D0, NFP_S, NLP_S, &
       REPORT, GROUP, EOF )
      IF ( EOF ) THEN
        CALL MSG_WRITE( '*** File skipped', REPORT )
        GO TO 190
      END IF

      IF ( .NOT. GROUP ) THEN ! Interactive prompts

        ! Get process specifications
        CALL PROCESS_SETUP( ATYP, VTYP, DTYP, AFL, VFL, DFL, &
         FCUT_DEF_C, FSTP_DEF_C, DTT, STT, FTT, PTYP, FTD, FTD_C, &
         ASM, VSM, DSM, FS0, FS0_C, FSN, FSN_C, &
         NFREQ, FREQ, FREQ_C, STF, STF_C, FTF, FTF_C, DTF, DTF_C, &
         DFTTYP, OF_CTRL, AFIL, VFIL, DFIL, BATCH, GROUP, EOF )
        IF ( EOF ) THEN
          CALL MSG_WRITE( '*** File skipped', REPORT )
          GO TO 190
        END IF

      END IF

      ! Tail smoothing & baseline function
      IF ( ( PTYP == 'F' ) .OR. ( PTYP == 'D' ) ) THEN

        ! Set up DFT timing values
        CALL TIMING( PI, AVFAC, ADFAC, NLP_I, DTT, FTT, FTD, FTD_C, &
         NDD, NDDH, NDDHM, NDD2, TWON, PIT, PIT2, BAVFAC, BADFAC, &
         W0, FMD, FMC, A, AN, V, VN, D, DN, NFP_S, NLP_S )

        ! Set up default frequencies with actual FSTOP value
        IF ( FSTOP == 0.D0 ) FSTOP = FMD
        FSTOP = MIN( FSTOP, FMD )
        CALL FREQ_DEF( U, FMD, FSTOP, FCUT_DEF, FCUT_DEF_C, &
         FSTP_DEF, FSTP_DEF_C )

        ! Perform tail smoothing
        CALL SMOOTH( PI, AVFAC, VDFAC, ADFAC, ATYP, DTYP, &
         AFL, VFL, FCUT_DEF, FCUT_DEF_C, NFP_I, NLP_I, DTT, NDD, &
         ASM, VSM, DSM, FS0, FS0_C, FSN, FSN_C, &
         A, A0, AN, V, V0, VN, D, D0, DN, &
         H, NFP_S, NLP_S, REPORT )

        ! Compute baseline parameters
        CALL BLINE( PI, AVFAC, VDFAC, ADFAC, FTD, &
         A0, AN, V0, VN, D0, DN, A0A, ASA, B1A, B2A, &
         V0V, A0V, ASV, B1V, B2V, D0D, V0D, A0D, ASD, B1D, B2D )

      END IF

      ! Find max frequency needed for DFT and set KS_MAX and compute DFT
      IF ( ( PTYP == 'F' ) .OR. ( PTYP == 'D' ) ) THEN

        ! Find KS_MAX
        KS_MAX = 0
        DO I = 1, NFREQ
          IF ( PTYP == 'F' ) THEN
            IF ( FREQ_C(I) == ' ' ) THEN
              IF ( FCUT_DEF_C(1:1) == 'N' ) THEN
                FCORI = FCUT_DEF
              ELSE
                FCORI = FCUT_DEF * .5D0
              END IF
              FSTPI = FSTP_DEF
            ELSE IF ( FREQ_C(I) == 'S' ) THEN
              FCORI = FSTOP
              FSTPI = FSTOP
            ELSE IF ( FREQ_C(I) == 'M' ) THEN
              FCORI = FMD
              FSTPI = FMD
            ELSE
              FCORI = FREQ(I) * .5D0
              FSTPI = FREQ(I) * 2.5D0
            END IF
            IF ( FCORI >= FMD ) THEN ! Check for subsampling
              IF ( ( DTF_C == ' ' ) .OR. ( DTF_C(1:1) == 'I' ) ) THEN
                FSTPI = 0.D0
              ELSE
                NSUB = NINT( DTF / DTT )
                IF ( EQUAL_DP( DTF, NSUB*DTT, 1.D-15 ) ) FSTPI = 0.D0
              END IF
            END IF
          ELSE ! PTYP = 'D'
            IF ( FREQ_C(I) == ' ' ) THEN
              FSTPI = FSTP_DEF
            ELSE IF ( FREQ_C(I) == 'S' ) THEN
              FSTPI = FSTOP
            ELSE IF ( FREQ_C(I) == 'M' ) THEN
              FSTPI = FMD
            ELSE
              FSTPI = FREQ(I) * 2.5D0
            END IF
          END IF
          KS = MIN( INT( ( FSTPI / W0 ) * (1.+1.D-15) ), NDD-1 )
          KS_MAX = MAX( KS_MAX, KS )
        END DO

        ! Compute DFT
        IF ( YUNITS_I == AUN ) THEN
          CALL ADFT( DTT, NDD, NDDH, NDDHM, NDD2, TWON, PIT, PIT2, &
           A(0), A0A, ASA, B1A, B2A, YO, YE, SC, KS_MAX, BA, NLP_S )
        ELSE IF ( YUNITS_I == VUN ) THEN
          CALL VDFT( DTT, NDD, NDDH, NDDHM, NDD2, TWON, PIT, PIT2, &
           BAVFAC, V(0), V0V, A0V, ASV, B1V, B2V, YO, YE, SC, &
           KS_MAX, BA, NLP_S )
        ELSE IF ( YUNITS_I == DUN ) THEN
          CALL DDFT( DTT, NDD, NDDH, NDDHM, NDD2, TWON, PIT, PIT2, &
           BADFAC, D(0), D0D, V0D, A0D, ASD, B1D, B2D, YO, YE, SC, &
           KS_MAX, BA, NLP_S )
        END IF

      END IF

      ! Perform filtering or DFT output
      IF ( PTYP == 'F' ) THEN ! Filter and output
        DO I = 1, NFREQ
          CALL GET_CUT( U, FSTOP, FCUT_DEF, FCUT_DEF_C, &
           NDD, W0, FMD, CF, FC, SF, FREQ(I), FREQ_C(I), &
           CORF, STPF, KC, KS, FILMOD )
          CALL FILTER( FILE_NAME, U, PI, ALPHA, ATYP, VTYP, DTYP, &
           AFL, VFL, DFL, AUN, VUN, DUN, FC, CF, DEL_I, DTT, FTD, &
           FTF, FTF_C, DTF, DTF_C, BAVFAC, BADFAC, W0, FMD, FMC, &
           A, A0, AN, V, V0, VN, D, D0, DN, AF, VF, DF, &
           A0A, ASA, B1A, B2A, V0V, A0V, ASV, B1V, B2V, &
           D0D, V0D, A0D, ASD, B1D, B2D, FREQ(I), CORF, STPF, &
           KC, KS, BA, BAW, BVW, BDW, H, SINF, COSF, NFP_S, NLP_S, &
           OF_CTRL, AFIL, VFIL, DFIL, DEF_FILE, OUT_FILE, FILMOD, &
           DIR_OUT, NO_INCR, REPORT, OUTPUT_LIST, BATCH, NFREQ )
        END DO
      ELSE IF ( PTYP == 'D' ) THEN ! Output DFT's
        DO I = 1, NFREQ
          CALL GET_STOP( FSTOP, FSTP_DEF, FSTP_DEF_C, &
           NDD, W0, FMD, FREQ(I), FREQ_C(I), KS )
          CALL DFT_OUT( FILE_NAME, U, ATYP, VTYP, DTYP, &
           AFL, VFL, DFL, AUN, VUN, DUN, FC, CF, &
           BAVFAC, BADFAC, W0, B1A, B2A, B1V, B2V, B1D, B2D, &
           DFTTYP, FREQ(I), KS, BA, BAW, BVW, BDW, &
           NLP_S, OF_CTRL, AFIL, VFIL, DFIL, DEF_FILE, OUT_FILE, &
           DIR_OUT, NO_INCR, REPORT, OUTPUT_LIST, BATCH, NFREQ )
        END DO
      ELSE IF ( PTYP == 'R' ) THEN ! Raw output
        CALL RAW_OUT( FILE_NAME, U, ATYP, VTYP, DTYP, &
         AFL, VFL, DFL, AUN, VUN, DUN, &
         NLP_I, DEL_I, DTT, STT, FTT, &
         STF, STF_C, FTF, FTF_C, DTF, DTF_C, &
         A, V, D, AF, VF, DF, NFP_S, NLP_S, &
         AFIL, VFIL, DFIL, DEF_FILE, OUT_FILE, &
         DIR_OUT, NO_INCR, REPORT, OUTPUT_LIST, BATCH )
      END IF

      ! Loop for next file
  190 WRITE( *, '(80(''_''))' )
      GO TO 100

      ! Clean up and stop
  199 CALL CLEAN_UP( HIT, REPORT, OUTPUT_LIST, DIR_OUT )

      END



      SUBROUTINE SIMFIL_CL( CL_ARG, NUM_ARGS, PROG_NAME, INI_VAL )

!***********************************************************************
!* Processes the Program-Specific Command Line Arguments
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) CL_ARG(0:*) ! Command line argument array

      INTEGER NUM_ARGS ! Number of arguments

      CHARACTER*(*) PROG_NAME ! Program name

      LOGICAL INI_VAL ! Indicates to use specified/file initial value


      ! Variables ______________________________________________________

      INTEGER I, NS

      CHARACTER ARG*255, C


      ! Process command line arguments
      INI_VAL = .FALSE.
      DO I = 1, NUM_ARGS
        CALL STR_UPCASE( ARG, CL_ARG(I) )
        C = ARG(1:1)
        IF ( ( C == '-' ) .OR. ( C == '/' ) ) THEN
          NS = 2
        ELSE
          NS = 1
        END IF
        IF ( ( ARG(NS:) == '?' ) .OR. &
         ( ARG(NS:) == '??' ) .OR. &
         ( ARG(NS:) == 'HELP' ) ) THEN ! Show command line syntax
          CALL SYNTAX_CL( PROG_NAME )
          WRITE( *, '(10X,A)' ) '[INI_VAL]'
          IF ( ( ARG(NS:) == '??' ) .OR. &
           ( ARG(NS:) == 'HELP' ) ) THEN ! Show command line help
            CALL HELP_CL( PROG_NAME )
            WRITE( *, '(10X,A)' ) 'INI_VAL :  Allow initial value control'
          END IF
          STOP ' '
        ELSE IF ( ( ARG(NS:) == 'INI_VAL' ) .OR. &
         ( ARG(NS:) == 'INIVAL' ) ) THEN
          INI_VAL = .TRUE.
          CL_ARG(I) = ' '
        END IF
      END DO

      RETURN
      END



      SUBROUTINE GROUP_SETUP( ATYP, VTYP, DTYP, AFL, VFL, DFL, &
       INI_VAL, V0_C, V0, D0_C, D0, FCUT_DEF_C, FSTP_DEF_C, EOF )

!***********************************************************************
!* Sets up for Group Processing
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) ATYP ! A data type

      CHARACTER*(*) VTYP ! V data type

      CHARACTER*(*) DTYP ! D data type

      CHARACTER AFL ! A data file flag

      CHARACTER VFL ! V data file flag

      CHARACTER DFL ! D data file flag

      LOGICAL INI_VAL ! Specifies to prompt for initial values

      CHARACTER*(*) V0_C ! Initial V string

      DOUBLE PRECISION V0 ! Initial V

      CHARACTER*(*) D0_C ! Initial D string

      DOUBLE PRECISION D0 ! Initial D

      CHARACTER*(*) FCUT_DEF_C ! Default cutoff frequency string

      CHARACTER*(*) FSTP_DEF_C ! Default stop frequency string

      LOGICAL EOF ! Indicates user-entered EOF


      ! Variables ______________________________________________________

      INTEGER IOS


      ! Set up generic data type strings
      ATYP = 'ACCELERATION, FORCE, ...'
      VTYP = 'VELOCITY, IMPULSE, ...'
      DTYP = 'DISPLACEMENT, ...'
      AFL = 'A'
      VFL = 'V'
      DFL = 'D'

  101 V0_C = ' '
      IF ( INI_VAL ) THEN
        WRITE( *, '(/A,16X,A/'' >> '',$)' ) ' Time-zero 1st integral (VELOCITY, IMPULSE, ...) value?', '[default]'
        READ( *, '(A)', END=199 ) V0_C
        IF ( V0_C /= ' ' ) THEN
          READ( V0_C, '(BN,F25.0)', IOSTAT=IOS ) V0
          IF ( IOS /= 0 ) THEN
            WRITE( *, '(/A)' ) ' *** Unacceptable value'
            GO TO 101
          END IF
        END IF
      END IF

  102 D0_C = ' '
      IF ( INI_VAL ) THEN
        WRITE( *, '(/A,5X,A/'' >> '',$)' ) ' Time-zero 2nd integral (DISPLACEMENT, IMPULSE X TIME, ...) value?', '[default]'
        READ( *, '(A)', END=101 ) D0_C
        IF ( D0_C /= ' ' ) THEN
          READ( D0_C, '(BN,F25.0)', IOSTAT=IOS ) D0
          IF ( IOS /= 0 ) THEN
            WRITE( *, '(/A)' ) ' *** Unacceptable value'
            GO TO 102
          END IF
        END IF
      END IF

      ! Set default frequency display
      FCUT_DEF_C = 'default'
      FSTP_DEF_C = 'default'

      EOF = .FALSE.
      RETURN

      ! Ctrl-Z handler
  199 EOF = .TRUE.
      RETURN

      END



      SUBROUTINE SETUP( U, AVFAC, VDFAC, ADFAC, ATYP, VTYP, DTYP, &
       AFL, VFL, DFL, AUN, VUN, DUN, YUNITS_I, FC, CF, SF, &
       NFP_I, NLP_I, DEL_I, DTT, STT, FTT, FSTOP, &
       FCUT_DEF, FCUT_DEF_C, FSTP_DEF, FSTP_DEF_C, INI_VAL, &
       A, A0, V, V0_C, V0, D, D0_C, D0, NFP_S, NLP_S, &
       REPORT, GROUP, EOF )

!***********************************************************************
!* Sets up File Information for Processing
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'
      INCLUDE 'file.fi'

      RECORD /UDS_Y/  U
      RECORD /FILE/  REPORT

      LOGICAL INI_VAL, GROUP, EOF

      INTEGER NFP_I, NLP_I, NFP_S, NLP_S, IOS, IOS_W, I

      REAL DEL_I, A(NFP_S:NLP_S), V(NFP_S:NLP_S), D(NFP_S:NLP_S)

      DOUBLE PRECISION AVFAC, VDFAC, ADFAC, FC, CF, SF, &
       DTT, STT, FTT, FSTOP, FCUT_DEF, FSTP_DEF, &
       A0, V0, D0, V0D, A0_D, V0_D, D0_D

      CHARACTER ATYP*(*), VTYP*(*), DTYP*(*), &
       AFL, VFL, DFL, AUN*(*), VUN*(*), DUN*(*), &
       YUNITS_I*(*), FCUT_DEF_C*(*), FSTP_DEF_C*(*), &
       V0_C*(*), D0_C*(*)


      ! Functions ______________________________________________________

      CHARACTER UPCASE

      EXTERNAL UPCASE


      ! Print curve info
      IF ( .NOT. GROUP ) &
       WRITE( *, '(/4A,2X,A,3X,A,3X,A/8X,A,F8.2,A,8X,A,F8.2,A)', &
       IOSTAT=IOS_W ) ' Curve: ', U.CURNAM, ' - ', &
       U.SENLOC, U.SENATT, U.AXIS, U.YTYP, &
       'Prefilter: ', U.PREF, ' Hz', 'Cutoff: ', U.FCUT, ' Hz'

      ! Set output fields
      U.FILEFORM = 'Y'
      U.CHANFORM = 'Y'
      U.HIC = 0.
      U.T1 = 0.
      U.T2 = 0.
      U.HICDTUP = 0.
      U.CLIP3M = 0.
      U.CSI = 0.

      ! Assign data type and units fields
      YUNITS_I = U.YUNITS
      AVFAC = 1.D0
      VDFAC = 1.D0
      ADFAC = 1.D0
      A0_D = 0.D0
      V0_D = 0.D0
      D0_D = 0.D0
      IF ( ( U.DIMSYS == 'ENG' ) .AND. &
       ( ( U.YUNITS == 'G''S' ) .OR. &
       ( U.YUNITS == 'MILES/HOUR' ) .OR. &
       ( U.YUNITS == 'INCHES' ) .OR. &
       ( U.YUNITS == 'FEET' ) ) ) THEN
        ATYP = 'ACCELERATION'
        VTYP = 'VELOCITY'
        DTYP = 'DISPLACEMENT'
        IF ( ( U.YTYP == 'REL. VELOCITY' ) .OR. &
         ( U.YTYP == 'DEFLECTION' ) ) THEN
          VTYP = 'REL. VELOCITY'
          DTYP = 'DEFLECTION'
        ELSE IF ( U.YTYP == 'LENGTH' ) THEN
          DTYP = 'LENGTH'
        ELSE IF ( U.YTYP == 'POSITION' ) THEN
          DTYP = 'POSITION'
        END IF
        AFL = 'A'
        VFL = 'V'
        DFL = DTYP(1:1)
        AUN = 'G''S'
        VUN = 'MILES/HOUR'
        AVFAC = 386.0868D0 * 3600.D0 / ( 5280.D0 * 12.D0 )
        IF ( U.YUNITS == 'FEET' ) THEN
          DUN = U.YUNITS
          VDFAC = 5280.D0 / 3600.D0
          ADFAC = 32.1739D0
        ELSE
          DUN = 'INCHES'
          VDFAC = 5280.D0 * 12.D0 / 3600.D0
          ADFAC = 386.0868D0
        END IF
        V0_D = DP_TRAN( U.INIVEL )
      ELSE IF ( ( U.DIMSYS == 'MET' ) .AND. &
       ( ( U.YUNITS == 'G''S' ) .OR. &
       ( U.YUNITS == 'KILOMETERS/HOUR' ) .OR. &
       ( U.YUNITS == 'MILLIMETERS' ) .OR. &
       ( U.YUNITS == 'METERS' ) .OR. &
       ( U.YUNITS == 'CENTIMETERS' ) ) ) THEN
        ATYP = 'ACCELERATION'
        VTYP = 'VELOCITY'
        DTYP = 'DISPLACEMENT'
        IF ( ( U.YTYP == 'REL. VELOCITY' ) .OR. &
         ( U.YTYP == 'DEFLECTION' ) ) THEN
          VTYP = 'REL. VELOCITY'
          DTYP = 'DEFLECTION'
        ELSE IF ( U.YTYP == 'LENGTH' ) THEN
          DTYP = 'LENGTH'
        ELSE IF ( U.YTYP == 'POSITION' ) THEN
          DTYP = 'POSITION'
        END IF
        AFL = 'A'
        VFL = 'V'
        DFL = DTYP(1:1)
        AUN = 'G''S'
        VUN = 'KILOMETERS/HOUR'
        AVFAC = 9.80665D0 * 3600.D0 / 1000.D0
        IF ( U.YUNITS == 'METERS' ) THEN
          DUN = U.YUNITS
          VDFAC = 1.D3 / 3600.D0
          ADFAC = 9.80665D0
        ELSE IF ( U.YUNITS == 'CENTIMETERS' ) THEN
          DUN = U.YUNITS
          VDFAC = 1.D5 / 3600.D0
          ADFAC = 9.80665D2
        ELSE
          DUN = 'MILLIMETERS'
          VDFAC = 1.D6 / 3600.D0
          ADFAC = 9.80665D3
        END IF
        V0_D = DP_TRAN( U.INIVEL )
      ELSE IF ( ( U.DIMSYS == 'SI' ) .AND. &
       ( ( U.YUNITS == 'METERS/SEC**2' ) .OR. &
       ( U.YUNITS == 'METERS/SEC' ) .OR. &
       ( U.YUNITS == 'METERS' ) .OR. &
       ( U.YUNITS == 'MILLIMETERS' ) .OR. &
       ( U.YUNITS == 'CENTIMETERS' ) ) ) THEN
        ATYP = 'ACCELERATION'
        VTYP = 'VELOCITY'
        DTYP = 'DISPLACEMENT'
        IF ( ( U.YTYP == 'REL. VELOCITY' ) .OR. &
         ( U.YTYP == 'DEFLECTION' ) ) THEN
          VTYP = 'REL. VELOCITY'
          DTYP = 'DEFLECTION'
        ELSE IF ( U.YTYP == 'LENGTH' ) THEN
          DTYP = 'LENGTH'
        ELSE IF ( U.YTYP == 'POSITION' ) THEN
          DTYP = 'POSITION'
        END IF
        AFL = 'A'
        VFL = 'V'
        DFL = DTYP(1:1)
        AUN = 'METERS/SEC**2'
        VUN = 'METERS/SEC'
        AVFAC = 1.D0
        IF ( U.YUNITS == 'MILLIMETERS' ) THEN
          DUN = U.YUNITS
          VDFAC = 1.D3
          ADFAC = 1.D3
        ELSE IF ( U.YUNITS == 'CENTIMETERS' ) THEN
          DUN = U.YUNITS
          VDFAC = 1.D2
          ADFAC = 1.D2
        ELSE
          DUN = 'METERS'
          VDFAC = 1.D0
          ADFAC = 1.D0
        END IF
        V0_D = DP_TRAN( U.INIVEL )
      ELSE IF ( ( U.YUNITS == 'DEGREES/SEC**2' ) .OR. &
       ( U.YUNITS == 'DEGREES/SEC' ) .OR. &
       ( U.YUNITS == 'DEGREES' ) ) THEN
        ATYP = 'ANGULAR ACCELERATION'
        VTYP = 'ANGULAR VELOCITY'
        DTYP = 'ANGULAR DISPLACEMENT'
        AFL = 'A'
        VFL = 'V'
        DFL = 'D'
        AUN = 'DEGREES/SEC**2'
        VUN = 'DEGREES/SEC'
        DUN = 'DEGREES'
      ELSE IF ( ( U.YUNITS == 'RADIANS/SEC**2' ) .OR. &
       ( U.YUNITS == 'RADIANS/SEC' ) .OR. &
       ( U.YUNITS == 'RADIANS' ) ) THEN
        ATYP = 'ANGULAR ACCELERATION'
        VTYP = 'ANGULAR VELOCITY'
        DTYP = 'ANGULAR DISPLACEMENT'
        AFL = 'A'
        VFL = 'V'
        DFL = 'D'
        AUN = 'RADIANS/SEC**2'
        VUN = 'RADIANS/SEC'
        DUN = 'RADIANS'
      ELSE IF ( ( U.YUNITS == 'POUNDS' ) .OR. &
       ( U.YUNITS == 'POUND-SECONDS' ) ) THEN
        ATYP = 'FORCE'
        VTYP = 'IMPULSE'
        DTYP = 'IMPULSE x TIME'
        IF ( U.YTYP == 'STATIC FORCE' ) ATYP = U.YTYP
        AFL = 'F'
        VFL = 'I'
        DFL = '2'
        AUN = 'POUNDS'
        VUN = 'POUND-SECONDS'
        DUN = 'POUND-SEC**2'
      ELSE IF ( ( U.YUNITS == 'NEWTONS' ) .OR. &
       ( U.YUNITS == 'NEWTON-SECONDS' ) ) THEN
        ATYP = 'FORCE'
        VTYP = 'IMPULSE'
        DTYP = 'IMPULSE x TIME'
        IF ( U.YTYP == 'STATIC FORCE' ) ATYP = U.YTYP
        AFL = 'F'
        VFL = 'I'
        DFL = '2'
        AUN = 'NEWTONS'
        VUN = 'NEWTON-SECONDS'
        DUN = 'NEWTON-SEC**2'
      ELSE IF ( ( U.YUNITS == 'FOOT-POUNDS' ) .OR. &
       ( U.YUNITS == 'INCH-POUNDS' ) ) THEN
        ATYP = 'ENERGY'
        VTYP = 'ENERGY x TIME'
        DTYP = 'ENERGY x TIME**2'
        AFL = 'E'
        VFL = '1'
        DFL = '2'
        IF ( U.YUNITS == 'INCH-POUNDS' ) THEN
          AUN = U.YUNITS
          VUN = 'INCH-POUND-SECONDS'
          DUN = 'INCH-POUND-SEC**2'
        ELSE
          AUN = 'FOOT-POUNDS'
          VUN = 'FOOT-POUND-SECONDS'
          DUN = 'FOOT-POUND-SEC**2'
        END IF
      ELSE IF ( ( U.YUNITS == 'JOULES' ) .OR. &
       ( U.YUNITS == 'METER-NEWTONS' ) ) THEN
        ATYP = 'ENERGY'
        VTYP = 'ENERGY x TIME'
        DTYP = 'ENERGY x TIME**2'
        AFL = 'E'
        VFL = '1'
        DFL = '2'
        YUNITS_I = 'JOULES'
        AUN = 'JOULES'
        VUN = 'JOULE-SECONDS'
        DUN = 'JOULE-SEC**2'
      ELSE IF ( ( U.YUNITS == 'POUND-FEET' ) .OR. &
       ( U.YUNITS == 'POUND-FOOT-SECONDS' ) ) THEN
        ATYP = 'TORQUE'
        VTYP = 'ANGULAR IMPULSE'
        DTYP = 'ANG. IMPULSE x TIME'
        AFL = 'T'
        VFL = 'I'
        DFL = '2'
        AUN = 'POUND-FEET'
        VUN = 'POUND-FOOT-SECONDS'
        DUN = 'POUND-FOOT-SEC**2'
      ELSE IF ( ( U.YUNITS == 'POUND-INCHES' ) .OR. &
       ( U.YUNITS == 'POUND-INCH-SECONDS' ) ) THEN
        ATYP = 'TORQUE'
        VTYP = 'ANGULAR IMPULSE'
        DTYP = 'ANG. IMPULSE x TIME'
        AFL = 'T'
        VFL = 'I'
        DFL = '2'
        AUN = 'POUND-INCHES'
        VUN = 'POUND-INCH-SECONDS'
        DUN = 'POUND-INCH-SEC**2'
      ELSE IF ( ( U.YUNITS == 'NEWTON-METERS' ) .OR. &
       ( U.YUNITS == 'NEWTON-METER-SECONDS' ) ) THEN
        ATYP = 'TORQUE'
        VTYP = 'ANGULAR IMPULSE'
        DTYP = 'ANG. IMPULSE x TIME'
        AFL = 'T'
        VFL = 'I'
        DFL = '2'
        AUN = 'NEWTON-METERS'
        VUN = 'NEWTON-METER-SECONDS'
        DUN = 'NEWTON-METER-SEC**2'
      ELSE
        CALL MSG_WRITE( '*** Unsupported units: '//U.YUNITS//' => Generic integrals', REPORT )
        ATYP = U.YTYP
        VTYP = U.YTYP(:L_TRIM(U.YTYP))//' x TIME'
        DTYP = U.YTYP(:L_TRIM(U.YTYP))//' x TIME**2'
        AFL = UPCASE( FN_TYPE( ATYP ) )
        VFL = '1'
        DFL = '2'
        AUN = U.YUNITS
        VUN = U.YUNITS(:L_TRIM(U.YUNITS))//'-SECONDS'
        DUN = U.YUNITS(:L_TRIM(U.YUNITS))//'-SEC**2'
      END IF

      ! Translate filtering info to DP
      FC = DP_TRAN( U.FCUT )
      CF = DP_TRAN( U.FCOR )
      SF = DP_TRAN( U.FSTP )

      ! Set up timing quantities
      NFP_I = U.NFP
      U.NFP = 0
      NLP_I = U.NLP
      DTT = DP_TRAN( U.DEL )
      DEL_I = U.DEL
      STT = NFP_I * DTT
      FTT = NLP_I * DTT

      ! Assign signal array
      IF ( YUNITS_I == AUN ) THEN
        DO I = NFP_I, NLP_I
          A(I) = U.Y(I)
        END DO
      ELSE IF ( YUNITS_I == VUN ) THEN
        DO I = NFP_I, NLP_I
          V(I) = U.Y(I)
        END DO
        IF ( .NOT. EQUAL_DP( V0_D, DBLE( V(0) ), 1.D-6 ) ) THEN
          WRITE( *, '(/3A,6X,1PG16.7)', IOSTAT=IOS_W ) &
           ' *** WARNING - Actual time-zero ', &
           VTYP(:L_TRIM(VTYP)), ' value = ', V(0)
          WRITE( *, '(14X,3A,1PG16.7)', IOSTAT=IOS_W ) &
           ' File/default time-zero ', &
           VTYP(:L_TRIM(VTYP)), ' value = ', V0_D
        END IF
        V0_D = V(0)
      ELSE IF ( YUNITS_I == DUN ) THEN
        DO I = NFP_I, NLP_I
          D(I) = U.Y(I)
        END DO
        D0_D = D(0)
      END IF

      ! Set initial values
      A0 = A0_D
      IF ( GROUP ) THEN ! Use default for unspecified initial values

        IF ( V0_C == ' ' ) V0 = V0_D
        IF ( D0_C == ' ' ) D0 = D0_D

      ELSE IF ( YUNITS_I == AUN ) THEN

  101   V0_C = ' '
        IF ( INI_VAL ) THEN
          WRITE( *, '(/3A,19X,A,1PG16.7,A/'' >> '',$)', IOSTAT=IOS_W ) &
           ' Time-zero ', VTYP, ' value?', '[',V0_D,']'
          READ( *, '(A)', END=199 ) V0_C
          IF ( V0_C == ' ' ) THEN
            V0 = V0_D
          ELSE
            READ( V0_C, '(BN,F25.0)', IOSTAT=IOS ) V0
            IF ( IOS /= 0 ) THEN
              WRITE( *, '(/A)' ) ' *** Unacceptable value'
              GO TO 101
            END IF
          END IF
        ELSE
          V0 = V0_D
        END IF

  102   D0_C = ' '
        IF ( INI_VAL ) THEN
          WRITE( *, '(/3A,19X,A,1PG16.7,A/'' >> '',$)', IOSTAT=IOS_W ) &
           ' Time-zero ', DTYP, ' value?', '[',D0_D,']'
          READ( *, '(A)', END=101 ) D0_C
          IF ( D0_C == ' ' ) THEN
            D0 = D0_D
          ELSE
            READ( D0_C, '(BN,F25.0)', IOSTAT=IOS ) D0
            IF ( IOS /= 0 ) THEN
              WRITE( *, '(/A)' ) ' *** Unacceptable value'
              GO TO 102
            END IF
          END IF
        ELSE
          D0 = D0_D
        END IF

      ELSE IF ( YUNITS_I == VUN ) THEN

        V0 = V0_D
  103   D0_C = ' '
        IF ( INI_VAL ) THEN
          WRITE( *, '(/3A,19X,A,1PG16.7,A/'' >> '',$)', IOSTAT=IOS_W ) &
           ' Time-zero ', DTYP, ' value?', '[',D0_D,']'
          READ( *, '(A)', END=199 ) D0_C
          IF ( D0_C == ' ' ) THEN
            D0 = D0_D
          ELSE
            READ( D0_C, '(BN,F25.0)', IOSTAT=IOS ) D0
            IF ( IOS /= 0 ) THEN
              WRITE( *, '(/A)' ) ' *** Unacceptable value'
              GO TO 103
            END IF
          END IF
        ELSE
          D0 = D0_D
        END IF

      ELSE IF ( YUNITS_I == DUN ) THEN

        D0 = D0_D
  104   V0_C = ' '
        IF ( INI_VAL ) THEN
          WRITE( *, '(/3A,19X,A,1PG16.7,A/'' >> '',$)', IOSTAT=IOS_W ) &
           ' Time-zero ', VTYP, ' value?', '[',V0_D,']'
          READ( *, '(A)', END=199 ) V0_C
          IF ( V0_C == ' ' ) THEN
            V0 = V0_D
          ELSE
            READ( V0_C, '(BN,F25.0)', IOSTAT=IOS ) V0
            IF ( IOS /= 0 ) THEN
              WRITE( *, '(/A)' ) ' *** Unacceptable value'
              GO TO 104
            END IF
          END IF
        ELSE
          V0 = V0_D
        END IF

      END IF

      ! Compute the other two signal domains
      IF ( YUNITS_I == AUN ) THEN

        CALL AIN( AVFAC, VDFAC, ADFAC, DTT, NFP_I, NLP_I, &
         A, A0, V, V0, D, D0, NFP_S, NLP_S )

      ELSE IF ( YUNITS_I == VUN ) THEN

        CALL VIN( AVFAC, VDFAC, ADFAC, DTT, NFP_I, NLP_I, &
         A, A0, V, V0, D, D0, NFP_S, NLP_S )

      ELSE IF ( YUNITS_I == DUN ) THEN

        CALL DIN( VDFAC, AVFAC, DTT, NFP_I, NLP_I, &
         A, A0, V, V0, D, D0, V0D, NFP_S, NLP_S )

        IF ( .NOT. EQUAL_DP( V0, V0D, 1.D-3 ) ) THEN
          WRITE( *, '(/3A,G16.7)', IOSTAT=IOS_W ) &
           ' *** WARNING - Computed initial ', &
           VTYP(:L_TRIM(VTYP)), ' value = ', V0D
        END IF

      END IF

      ! Determine effective current stop frequency in data
      FSTOP = 0.D0
      IF ( U.PREF > 0. ) FSTOP = 2.5D0 * U.PREF
      IF ( FSTOP == 0.D0 ) THEN
        FSTOP = MAX( SF, 2.5D0*FC )
      ELSE IF ( FC > 0.D0 ) THEN
        FSTOP = MIN( FSTOP, MAX( SF, 2.5D0*FC ) )
      END IF

      ! Set up default frequencies
      CALL FREQ_DEF( U, 0.D0, FSTOP, FCUT_DEF, FCUT_DEF_C, &
       FSTP_DEF, FSTP_DEF_C )

      EOF = .FALSE.
      RETURN

      ! Ctrl-Z handler
  199 EOF = .TRUE.
      RETURN

      END



      SUBROUTINE PROCESS_SETUP( ATYP, VTYP, DTYP, AFL, VFL, DFL, &
       FCUT_DEF_C, FSTP_DEF_C, DTT, STT, FTT, PTYP, FTD, FTD_C, &
       ASM, VSM, DSM, FS0, FS0_C, FSN, FSN_C, &
       NFREQ, FREQ, FREQ_C, STF, STF_C, FTF, FTF_C, DTF, DTF_C, &
       DFTTYP, OF_CTRL, AFIL, VFIL, DFIL, BATCH, GROUP, EOF )

!***********************************************************************
!* Sets Up SimFil Process Specifications
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'

      LOGICAL ASM, VSM, DSM, BATCH, GROUP, EOF

      INTEGER NFREQ

      DOUBLE PRECISION DTT, STT, FTT, FTD, FS0, FSN, FREQ(100), &
       STF, FTF, DTF

      CHARACTER ATYP*(*), VTYP*(*), DTYP*(*), AFL, VFL, DFL, &
       PTYP, FTD_C*(*), FCUT_DEF_C*(*), FSTP_DEF_C*(*), &
       FS0_C*(*), FSN_C*(*), FREQ_C(100)*(*), &
       STF_C*(*), FTF_C*(*), DTF_C*(*), DFTTYP, &
       OF_CTRL, AFIL*(*), VFIL*(*), DFIL*(*)


      ! Get process type
  101 EOF = .FALSE.
      WRITE( *, '(/A,24X,A/'' >> '',$)' ) &
       ' Process?   ( F - Filter,   D - DFT,   R - Raw )', &
       '[filter]'
      READ( *, '(A1)', END=199 ) PTYP
      CALL STR_UP( PTYP )
      IF ( PTYP == ' ' ) THEN
        PTYP = 'F'
      ELSE IF ( .NOT. ANY_CHARS( PTYP, 'FDR' ) ) THEN
        WRITE( *, '(/A)' ) ' *** Invalid process'
        GO TO 101
      END IF

      ! Set up timing and tail smoothing
      IF ( ( PTYP == 'F' ) .OR. ( PTYP == 'D' ) ) THEN

        ! Set up DFT timing values
        CALL TIMING_SET( FTT, FTD, FTD_C, GROUP, EOF )
        IF ( EOF ) GO TO 101

        ! Set tail smoothing spec's
        CALL SMOOTH_SET( ATYP, VTYP, DTYP, AFL, VFL, DFL, &
         FCUT_DEF_C, ASM, VSM, DSM, FS0, FS0_C, FSN, FSN_C, EOF )
        IF ( EOF ) GO TO 101

      END IF

      ! Set up frequency info
  103 IF ( PTYP == 'F' ) THEN ! Set cutoff frequency
        CALL FREQ_SET( 'Cutoff ', FCUT_DEF_C, NFREQ, FREQ_C, FREQ, EOF )
      ELSE IF ( PTYP == 'D' ) THEN ! Set stop frequency
        CALL FREQ_SET( 'DFT max', FSTP_DEF_C, NFREQ, FREQ_C, FREQ, EOF )
      END IF
      IF ( EOF ) GO TO 101

      ! Set up filtering or DFT spec's
  104 IF ( PTYP == 'F' ) THEN ! Filter
        CALL FILTER_SET( ATYP, VTYP, DTYP, DTT, FTD, &
         FTF, FTF_C, DTF, DTF_C, NFREQ, OF_CTRL, &
         AFIL, VFIL, DFIL, BATCH, GROUP, EOF )
      ELSE IF ( PTYP == 'D' ) THEN ! DFT
        CALL DFT_SET( ATYP, VTYP, DTYP, DFTTYP, NFREQ, OF_CTRL, &
         AFIL, VFIL, DFIL, BATCH, EOF )
      ELSE IF ( PTYP == 'R' ) THEN ! Raw
        CALL RAW_SET( ATYP, VTYP, DTYP, DTT, STT, FTT, &
         STF, STF_C, FTF, FTF_C, DTF, DTF_C, &
         AFIL, VFIL, DFIL, BATCH, GROUP, EOF )
      END IF
      IF ( EOF ) GO TO 101

      RETURN

      ! Ctrl-Z handler
  199 EOF = .TRUE.
      RETURN

      END



      SUBROUTINE TIMING_SET( FTT, FTD, FTD_C, GROUP, EOF )

!***********************************************************************
!* Sets Up Timing Values for DFT
!***********************************************************************

      LOGICAL GROUP, EOF

      INTEGER IOS, IOS_W

      DOUBLE PRECISION FTT, FTD

      CHARACTER FTD_C*(*)


      ! Get DFT final time value
  101 IF ( GROUP ) THEN
        WRITE( *, '(/A,52X,A/'' >> '',$)' ) &
         ' DFT final time (sec)?', '[tail]'
      ELSE
        WRITE( *, '(/A,41X,A,F15.6,A/'' >> '',$)', IOSTAT=IOS_W ) &
         ' DFT final time (sec)?', '[',FTT,']'
      END IF
      READ( *, '(A)', END=199 ) FTD_C
      IF ( FTD_C == ' ' ) THEN ! Use tail
        FTD = FTT
      ELSE ! Assign value
        READ( FTD_C, '(BN,F25.0)', IOSTAT=IOS ) FTD
        IF ( ( IOS /= 0 ) .OR. ( FTD < 0.D0 ) ) THEN
          WRITE( *, '(/A)' ) ' *** Invalid DFT final time'
          GO TO 101
        END IF
      END IF

      EOF = .FALSE.
      RETURN

      ! Ctrl-Z handler
  199 EOF = .TRUE.
      RETURN

      END



      SUBROUTINE SMOOTH_SET( ATYP, VTYP, DTYP, AFL, VFL, DFL, &
       FCUT_DEF_C, ASM, VSM, DSM, FS0, FS0_C, FSN, FSN_C, EOF )

!***********************************************************************
!* Sets Up for Tail Smoothing
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'

      LOGICAL ASM, VSM, DSM, EOF

      INTEGER IOS, I

      DOUBLE PRECISION FS0, FSN

      CHARACTER ATYP*(*), VTYP*(*), DTYP*(*), AFL, VFL, DFL, &
       FCUT_DEF_C*7, FS0_C*(*), FSN_C*(*), &
       SMDOM*7, C, TAB

      PARAMETER ( TAB = CHAR(9) )


      ! Functions ______________________________________________________

      CHARACTER LJUST*25

      EXTERNAL LJUST


      ! Initializations
      EOF = .FALSE.

      ! Get desired tail smoothing specifications
  101 WRITE( *, '(/3A,11X,A/29X,5A/31X,5A/33X,5A/'' >> '',$)' ) &
       ' Tail smoothing domain(s)?   ', &
       '( '//AFL//'/'//VFL//'/'//DFL//' in any combo )   ', &
       '( N - None )', &
       '['//AFL//']', &
       '( ',AFL,' - ',ATYP(:L_TRIM(ATYP)),' )', &
       '( ',VFL,' - ',VTYP(:L_TRIM(VTYP)),' )', &
       '( ',DFL,' - ',DTYP(:L_TRIM(DTYP)),' )'
      READ( *, '(A)', END=199 ) SMDOM
      CALL STR_UP( SMDOM )
      ASM = .FALSE.
      VSM = .FALSE.
      DSM = .FALSE.
      IF ( SMDOM == ' ' ) THEN ! Default A-domain smoothing
        ASM = .TRUE.
      ELSE IF ( SMDOM == 'N' ) THEN ! No smoothing
        RETURN
      ELSE ! Check assign smoothing domain flags
        DO I = 1, LEN_TRIM( SMDOM )
          C = SMDOM(I:I)
          IF ( C == AFL ) THEN
            ASM = .TRUE.
          ELSE IF ( C == VFL ) THEN
            VSM = .TRUE.
          ELSE IF ( C == DFL ) THEN
            DSM = .TRUE.
          ELSE IF ( ( C /= ' ' ) .AND. ( C /= ',' ) .AND. &
           ( C /= '/' ) .AND.  ( C /= TAB ) ) THEN
            WRITE( *, '(/2A)' ) ' *** Invalid domain letter: ', C
            GO TO 101
          END IF
        END DO
      END IF

      ! Time zero tail smoothing
  102 WRITE( *, '(/2A,15X,A/'' >> '',$)' ) ' Time-zero tail', &
       ' smoothing frequency (Hz)?   ( N - None )', &
       '['//FCUT_DEF_C//']'
      READ( *, '(A)', END=101 ) FS0_C
      CALL STR_UP( FS0_C )
      FS0_C = LJUST( FS0_C )
      FS0 = 0.D0
      IF ( FS0_C == ' ' ) THEN
        ! Use default
      ELSE IF ( FS0_C /= 'N' ) THEN ! Use specified frequency
        READ( FS0_C, '(BN,F25.0)', IOSTAT=IOS ) FS0
        IF ( ( IOS /= 0 ) .OR. ( FS0 <= 0.D0 ) ) THEN
          WRITE( *, '(/A)' ) ' *** Invalid frequency value'
          GO TO 102
        END IF
      END IF

      ! End tail smoothing
  103 WRITE( *, '(/2A,21X,A/'' >> '',$)' ) ' End tail', &
       ' smoothing frequency (Hz)?   ( N - None )', &
       '['//FCUT_DEF_C//']'
      READ( *, '(A)', END=101 ) FSN_C
      CALL STR_UP( FSN_C )
      FSN_C = LJUST( FSN_C )
      FSN = 0.D0
      IF ( FSN_C == ' ' ) THEN
        ! Use default
      ELSE IF ( FSN_C /= 'N' ) THEN ! Use specified frequency
        READ( FSN_C, '(BN,F25.0)', IOSTAT=IOS ) FSN
        IF ( ( IOS /= 0 ) .OR. ( FSN <= 0.D0 ) ) THEN
          WRITE( *, '(/A)' ) ' *** Invalid frequency value'
          GO TO 103
        END IF
      END IF

      RETURN

      ! Ctrl-Z handler
  199 EOF = .TRUE.
      RETURN

      END



      SUBROUTINE FREQ_SET( FREQ_NAME, FREQ_DEF_C, NFREQ, FREQ_C, FREQ, EOF )

!***********************************************************************
!* Sets Up for Specified Frequency and Mode
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'

      LOGICAL EOF, DEF_FR, STOP_FR, MAX_FR

      INTEGER NFREQ, IOS, I

      DOUBLE PRECISION FREQ(100), FRS

      CHARACTER FREQ_NAME*(*), FREQ_DEF_C*(*), FREQ_C(100)*(*), &
       FRS_C*25, DEF_ACT*7, FREQ_DEF_O*15


      ! Functions ______________________________________________________

      CHARACTER LJUST*25

      EXTERNAL LJUST


      ! Get frequency values
      EOF = .FALSE.
      DEF_FR = .FALSE.
      STOP_FR = .FALSE.
      MAX_FR = .FALSE.
      NFREQ = 0
      DEF_ACT = FREQ_DEF_C
      READ( FREQ_DEF_C, '(BN,F7.0)', IOSTAT=IOS ) FRS
      IF ( IOS == 0 ) THEN
        FREQ_DEF_O = ' = '//FREQ_DEF_C(:L_TRIM(FREQ_DEF_C))//' (Hz)'
      ELSE
        FREQ_DEF_O = ' '
      END IF
      WRITE( *, '(/3A)' ) ' Enter up to 100 ', FREQ_NAME, ' frequencies:'
  101 WRITE( *, '(/1X,2A,I3,A,41X,A/9X,3A/9X,A/9X,A/'' >> '',$)' ) &
       FREQ_NAME, ' frequency # ', NFREQ+1, ' (Hz)?', &
       '['//DEF_ACT//']', &
       '( D - Default frequency', FREQ_DEF_O(:L_TRIM(FREQ_DEF_O)), ' )', &
       '( S - Stop frequency )', &
       '( M - Max frequency )'
      READ( *, '(A)', END=199 ) FRS_C
      CALL STR_UP( FRS_C )
      FRS_C = LJUST( FRS_C )
      FRS = 0.D0
      IF ( FRS_C == ' ' ) THEN ! Default frequency
        IF ( NFREQ == 0 ) THEN
          IF ( .NOT. DEF_FR ) THEN
            DEF_FR = .TRUE.
          ELSE
            WRITE( *, '(/A)' ) ' *** Repeat frequency ignored'
            GO TO 101
          END IF
        ELSE
          RETURN
        END IF
      ELSE IF ( FRS_C == 'D' ) THEN ! Default frequency
        IF ( .NOT. DEF_FR ) THEN
          DEF_FR = .TRUE.
        ELSE
          WRITE( *, '(/A)' ) ' *** Repeat frequency ignored'
          GO TO 101
        END IF
      ELSE IF ( FRS_C == 'S' ) THEN ! Stop frequency
        IF ( .NOT. STOP_FR ) THEN
          STOP_FR = .TRUE.
        ELSE
          WRITE( *, '(/A)' ) ' *** Repeat frequency ignored'
          GO TO 101
        END IF
      ELSE IF ( FRS_C == 'M' ) THEN ! Max frequency
        IF ( .NOT. MAX_FR ) THEN
          MAX_FR = .TRUE.
        ELSE
          WRITE( *, '(/A)' ) ' *** Repeat frequency ignored'
          GO TO 101
        END IF
      ELSE ! Specified frequency
        READ( FRS_C, '(BN,F25.0)', IOSTAT=IOS ) FRS
        IF ( ( IOS .NE.0 ) .OR. ( FRS < 0.D0 ) ) THEN
          WRITE( *, '(/A)' ) ' *** Invalid frequency'
          GO TO 101
        END IF
        DO I = 1, NFREQ-1
          IF ( FRS == FREQ(I) ) THEN
            WRITE( *, '(/A)' ) ' *** Repeat frequency ignored'
            GO TO 101
          END IF
        END DO
      END IF
      NFREQ = NFREQ + 1
      FREQ_C(NFREQ) = FRS_C
      FREQ(NFREQ) = FRS
      DEF_ACT = 'no more'
      IF ( NFREQ < 100 ) GO TO 101

      RETURN

      ! Ctrl-Z handler
  199 EOF = .TRUE.
      RETURN

      END



      SUBROUTINE FILTER_SET( ATYP, VTYP, DTYP, DTT, FTD, &
       FTF, FTF_C, DTF, DTF_C, NFREQ, OF_CTRL, &
       AFIL, VFIL, DFIL, BATCH, GROUP, EOF )

!***********************************************************************
!* Sets Up Filtering Output Specifications
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'

      LOGICAL BATCH, GROUP, EOF

      INTEGER NFREQ, IOS, IOS_W, ISUB

      DOUBLE PRECISION DTT, FTD, FTF, DTF

      CHARACTER ATYP*(*), VTYP*(*), DTYP*(*), FTF_C*(*), DTF_C*(*), &
       OF_CTRL, AFIL*(*), VFIL*(*), DFIL*(*)


      ! Functions ______________________________________________________

      CHARACTER UPCASE*25, LJUST*25

      EXTERNAL UPCASE, LJUST


      ! Get final time value
  101 IF ( GROUP ) THEN
        WRITE( *, '(/A,56X,A/'' >> '',$)' ) ' Final time (sec)?', '[tail]'
      ELSE
        WRITE( *, '(/A,45X,A,F15.6,A/'' >> '',$)', IOSTAT=IOS_W ) &
         ' Final time (sec)?', '[',FTD,']'
      END IF
      READ( *, '(A)', END=199 ) FTF_C
      IF ( FTF_C == ' ' ) THEN ! Use default
        FTF = FTD
      ELSE ! Specified final time
        READ( FTF_C, '(BN,F25.0)', IOSTAT=IOS ) FTF
        IF ( ( IOS /= 0 ) .OR. ( FTF < 0.D0 ) ) THEN
          WRITE( *, '(/A)' ) ' *** Invalid time value'
          GO TO 101
        END IF
      END IF

      ! Get filtered signal time step
  102 IF ( GROUP ) THEN
        WRITE( *, '(/A,25X,A/'' >> '',$)' ) &
         ' Time step (sec)?   ( I# - Input step x # )', &
         '[input step]'
      ELSE
        WRITE( *, '(/A,20X,A,F15.6,A/'' >> '',$)', IOSTAT=IOS_W ) &
         ' Time step (sec)?   ( I# - Input step x # )', &
         '[',DTT,']'
      END IF
      READ( *, '(A)', END=101 ) DTF_C
      CALL STR_UP( DTF_C )
      DTF_C = LJUST( DTF_C )
      IF ( ( DTF_C == ' ' ) .OR. ( DTF_C == 'I' ) ) THEN ! Use default
        DTF = DTT
      ELSE IF ( DTF_C(1:1) == 'I' ) THEN ! Subsampling requested
        READ( DTF_C(2:), '(BN,I24)', IOSTAT=IOS ) ISUB
        IF ( ( IOS /= 0 ) .OR. ( ISUB < 1 ) ) THEN
          WRITE( *, '(/A)' ) ' *** Invalid subsampling rate'
          GO TO 102
        END IF
      ELSE ! Numerical time step specified
        READ( DTF_C, '(BN,F25.0)', IOSTAT=IOS ) DTF
        IF ( ( IOS /= 0 ) .OR. ( DTF <= 0.D0 ) ) THEN
          WRITE( *, '(/A)' ) ' *** Invalid time step'
          GO TO 102
        END IF
      END IF

      ! Get output signals _____________________________________________

      IF ( ( NFREQ > 1 ) .AND. ( .NOT. BATCH ) ) THEN
  103   WRITE( *, '(/A,25X,A/'' >> '',$)' ) &
         ' Output file control?   ( G - Group,  E - Each )', &
         '[group]'
        READ( *, '(A)', END=101 ) OF_CTRL
        CALL STR_UP( OF_CTRL )
        IF ( OF_CTRL == ' ' ) THEN
          OF_CTRL = 'G'
        ELSE IF ( .NOT. ANY_CHARS( OF_CTRL, 'GE') ) THEN
          WRITE( *, '(/A)' ) ' *** Invalid response'
          GO TO 103
        END IF
      ELSE
        OF_CTRL = 'E'
      END IF

      IF ( ( BATCH ) .OR. ( OF_CTRL == 'G' ) ) THEN
  104   WRITE( *, 601 ) ATYP
        READ( *, '(A)', END=101 ) AFIL
        IF ( ( AFIL == ' ' ) .OR. &
         ( UPCASE( LJUST( AFIL ) ) == 'Y' ) ) THEN
          AFIL = ' '
        ELSE IF ( UPCASE( LJUST( AFIL ) ) == 'N' ) THEN
          AFIL = 'N'
        ELSE
          WRITE( *, '(/A)' ) ' *** Invalid response'
          GO TO 104
        END IF

  105   WRITE( *, 601 ) VTYP
        READ( *, '(A)', END=101 ) VFIL
        IF ( ( VFIL == ' ' ) .OR. &
         ( UPCASE( LJUST( VFIL ) ) == 'Y' ) ) THEN
          VFIL = ' '
        ELSE IF ( UPCASE( LJUST( VFIL ) ) == 'N' ) THEN
          VFIL = 'N'
        ELSE
          WRITE( *, '(/A)' ) ' *** Invalid response'
          GO TO 105
        END IF

  106   WRITE( *, 601 ) DTYP
        READ( *, '(A)', END=101 ) DFIL
        IF ( ( DFIL == ' ' ) .OR. &
         ( UPCASE( LJUST( DFIL ) ) == 'Y' ) ) THEN
          DFIL = ' '
        ELSE IF ( UPCASE( LJUST( DFIL ) ) == 'N' ) THEN
          DFIL = 'N'
        ELSE
          WRITE( *, '(/A)' ) ' *** Invalid response'
          GO TO 106
        END IF

        IF ( ( AFIL == 'N' ) .AND. &
         ( VFIL == 'N' ) .AND. &
         ( DFIL == 'N' ) ) THEN
          WRITE( *, '(/A)' ) ' *** No outputs specified'
          GO TO 104
        END IF
      END IF

      EOF = .FALSE.
      RETURN

      ! Ctrl-Z handler
  199 EOF = .TRUE.
      RETURN

  601 FORMAT(/1X,A,' file output?   (Y/N)',28X,'[yes]'/' >> ',$)

      END



      SUBROUTINE DFT_SET( ATYP, VTYP, DTYP, DFTTYP, NFREQ, OF_CTRL, &
       AFIL, VFIL, DFIL, BATCH, EOF )

!***********************************************************************
!* Sets Up DFT Output Specifications
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'

      LOGICAL BATCH, EOF

      INTEGER NFREQ

      CHARACTER ATYP*(*), VTYP*(*), DTYP*(*), DFTTYP, &
       OF_CTRL, AFIL*(*), VFIL*(*), DFIL*(*)


      ! Functions ______________________________________________________

      CHARACTER UPCASE*25, LJUST*25

      EXTERNAL UPCASE, LJUST


      ! Ask whether signed or magnitude DFT output desired
  101 WRITE( *, '(/A,22X,A/'' >> '',$)' ) &
       ' DFT output type?   ( S - Signed,  M - Magnitude )', &
       '[signed]'
      READ( *, '(A)', END=199 ) DFTTYP
      CALL STR_UP( DFTTYP )
      IF ( .NOT. ANY_CHARS( DFTTYP, ' SM' ) ) THEN
        WRITE( *, '(/A)' ) ' *** Invalid DFT output type'
        GO TO 101
      END IF


      ! Get output DFTs ________________________________________________

      IF ( ( NFREQ > 1 ) .AND. ( .NOT. BATCH ) ) THEN
  102   WRITE( *, '(/A,25X,A/'' >> '',$)' ) &
         ' Output file control?   ( G - Group,  E - Each )', &
         '[group]'
        READ( *, '(A)', END=101 ) OF_CTRL
        CALL STR_UP( OF_CTRL )
        IF ( OF_CTRL == ' ' ) THEN
          OF_CTRL = 'G'
        ELSE IF ( .NOT. ANY_CHARS( OF_CTRL, 'GE') ) THEN
          WRITE( *, '(/A)' ) ' *** Invalid response'
          GO TO 102
        END IF
      ELSE
        OF_CTRL = 'E'
      END IF


      IF ( ( BATCH ) .OR. ( OF_CTRL == 'G' ) ) THEN
  103   WRITE( *, 601 ) ATYP
        READ( *, '(A)', END=101 ) AFIL
        IF ( ( AFIL == ' ' ) .OR. &
         ( UPCASE( LJUST( AFIL ) ) == 'Y' ) ) THEN
          AFIL = ' '
        ELSE IF ( UPCASE( LJUST( AFIL ) ) == 'N' ) THEN
          AFIL = 'N'
        ELSE
          WRITE( *, '(/A)' ) ' *** Invalid response'
          GO TO 103
        END IF

  104   WRITE( *, 601 ) VTYP
        READ( *, '(A)', END=101 ) VFIL
        IF ( ( VFIL == ' ' ) .OR. &
         ( UPCASE( LJUST( VFIL ) ) == 'Y' ) ) THEN
          VFIL = ' '
        ELSE IF ( UPCASE( LJUST( VFIL ) ) == 'N' ) THEN
          VFIL = 'N'
        ELSE
          WRITE( *, '(/A)' ) ' *** Invalid response'
          GO TO 104
        END IF

  105   WRITE( *, 601 ) DTYP
        READ( *, '(A)', END=101 ) DFIL
        IF ( ( DFIL == ' ' ) .OR. &
         ( UPCASE( LJUST( DFIL ) ) == 'Y' ) ) THEN
          DFIL = ' '
        ELSE IF ( UPCASE( LJUST( DFIL ) ) == 'N' ) THEN
          DFIL = 'N'
        ELSE
          WRITE( *, '(/A)' ) ' *** Invalid response'
          GO TO 105
        END IF

        IF ( ( AFIL == 'N' ) .AND. &
         ( VFIL == 'N' ) .AND. &
         ( DFIL == 'N' ) ) THEN
          WRITE( *, '(/A)' ) ' *** No outputs specified'
          GO TO 103
        END IF
      END IF

      EOF = .FALSE.
      RETURN

      ! Ctrl-Z handler
  199 EOF = .TRUE.
      RETURN

  601 FORMAT(/1X,A,' DFT file output?   (Y/N)',24X,'[yes]'/' >> ',$)

      END



      SUBROUTINE RAW_SET( ATYP, VTYP, DTYP, DTT, STT, FTT, &
       STF, STF_C, FTF, FTF_C, DTF, DTF_C, &
       AFIL, VFIL, DFIL, BATCH, GROUP, EOF )

!***********************************************************************
!* Sets Up Raw Output Specifications
!***********************************************************************

      LOGICAL BATCH, GROUP, EOF

      INTEGER IOS, IOS_W, ISUB

      DOUBLE PRECISION DTT, STT, FTT, STF, FTF, DTF

      CHARACTER ATYP*(*), VTYP*(*), DTYP*(*), &
       STF_C*(*), FTF_C*(*), DTF_C*(*), &
       AFIL*(*), VFIL*(*), DFIL*(*)


      ! Functions ______________________________________________________

      CHARACTER UPCASE*25, LJUST*25

      EXTERNAL UPCASE, LJUST


      ! Get initial time value
  101 IF ( GROUP ) THEN
        WRITE( *, '(/A,54X,A/'' >> '',$)' ) ' Initial time (sec)?', '[tail]'
      ELSE
        WRITE( *, '(/A,43X,A,F15.6,A/'' >> '',$)', IOSTAT=IOS_W ) &
         ' Initial time (sec)?', '[',STT,']'
      END IF
      READ( *, '(A)', END=199 ) STF_C
      IF ( STF_C == ' ' ) THEN ! Use default
        STF = STT
      ELSE
        READ( STF_C, '(BN,F25.0)', IOSTAT=IOS ) STF
        IF ( IOS /= 0 ) THEN
          WRITE( *, '(/A)' ) ' *** Invalid time value'
          GO TO 101
        END IF
      END IF

      ! Get final time value
  102 IF ( GROUP ) THEN
        WRITE( *, '(/A,56X,A/'' >> '',$)' ) ' Final time (sec)?', '[tail]'
      ELSE
        WRITE( *, '(/A,45X,A,F15.6,A/'' >> '',$)', IOSTAT=IOS_W ) &
         ' Final time (sec)?', '[',FTT,']'
      END IF
      READ( *, '(A)', END=101 ) FTF_C
      IF ( FTF_C == ' ' ) THEN ! Use default
        FTF = FTT
      ELSE
        READ( FTF_C, '(BN,F25.0)', IOSTAT=IOS ) FTF
        IF ( IOS /= 0 ) THEN
          WRITE( *, '(/A)' ) ' *** Invalid time value'
          GO TO 102
        END IF
      END IF

      ! Get subsampled signal time step
  103 IF ( GROUP ) THEN
        WRITE( *, '(/A,25X,A/'' >> '',$)' ) ' Time step (sec)?   ( I# - Input step x # )', &
         '[input step]'
      ELSE
        WRITE( *, '(/A,20X,A,F15.6,A/'' >> '',$)', IOSTAT=IOS_W ) &
         ' Time step (sec)?   ( I# - Input step x # )', &
         '[',DTT,']'
      END IF
      READ( *, '(A)', END=101 ) DTF_C
      CALL STR_UP( DTF_C )
      DTF_C = LJUST( DTF_C )
      IF ( ( DTF_C == ' ' ) .OR. ( DTF_C == 'I' ) ) THEN ! Use default
        DTF = DTT
      ELSE IF ( DTF_C(1:1) == 'I' ) THEN ! Subsampling requested
        READ( DTF_C(2:), '(BN,I24)', IOSTAT=IOS ) ISUB
        IF ( ( IOS /= 0 ) .OR. ( ISUB < 1 ) ) THEN
          WRITE( *, '(/A)' ) ' *** Invalid subsampling rate'
          GO TO 103
        END IF
      ELSE ! Numerical time step specified
        READ( DTF_C, '(BN,F25.0)', IOSTAT=IOS ) DTF
        IF ( ( IOS /= 0 ) .OR. ( DTF <= 0.D0 ) ) THEN
          WRITE( *, '(/A)' ) ' *** Invalid time step'
          GO TO 103
        END IF
      END IF


      ! Get output signals _____________________________________________

      IF ( BATCH ) THEN
  104   WRITE( *, 601 ) ATYP
        READ( *, '(A)', END=101 ) AFIL
        IF ( ( AFIL == ' ' ) .OR. &
         ( UPCASE( LJUST( AFIL ) ) == 'Y' ) ) THEN
          AFIL = ' '
        ELSE IF ( UPCASE( LJUST( AFIL ) ) == 'N' ) THEN
          AFIL = 'N'
        ELSE
          WRITE( *, '(/A)' ) ' *** Invalid response'
          GO TO 104
        END IF

  105   WRITE( *, 601 ) VTYP
        READ( *, '(A)', END=101 ) VFIL
        IF ( ( VFIL == ' ' ) .OR. &
         ( UPCASE( LJUST( VFIL ) ) == 'Y' ) ) THEN
          VFIL = ' '
        ELSE IF ( UPCASE( LJUST( VFIL ) ) == 'N' ) THEN
          VFIL = 'N'
        ELSE
          WRITE( *, '(/A)' ) ' *** Invalid response'
          GO TO 105
        END IF

  106   WRITE( *, 601 ) DTYP
        READ( *, '(A)', END=101 ) DFIL
        IF ( ( DFIL == ' ' ) .OR. &
         ( UPCASE( LJUST( DFIL ) ) == 'Y' ) ) THEN
          DFIL = ' '
        ELSE IF ( UPCASE( LJUST( DFIL ) ) == 'N' ) THEN
          DFIL = 'N'
        ELSE
          WRITE( *, '(/A)' ) ' *** Invalid response'
          GO TO 106
        END IF

        IF ( ( AFIL == 'N' ) .AND. &
         ( VFIL == 'N' ) .AND. &
         ( DFIL == 'N' ) ) THEN
          WRITE( *, '(/A)' ) ' *** No outputs specified'
          GO TO 104
        END IF
      END IF

      EOF = .FALSE.
      RETURN

      ! Ctrl-Z handler
  199 EOF = .TRUE.
      RETURN

  601 FORMAT(/1X,A,' file output?   (Y/N)',28X,'[yes]'/' >> ',$)

      END



      SUBROUTINE AIN( AVFAC, VDFAC, ADFAC, DTT, NFP, NLP, &
       A, A0, V, V0, D, D0, NFP_S, NLP_S )

!***********************************************************************
!* Integrates Acceleration Input
!***********************************************************************

      INTEGER NFP, NLP, NFP_S, NLP_S, I

      REAL A(NFP_S:NLP_S), V(NFP_S:NLP_S), D(NFP_S:NLP_S)

      DOUBLE PRECISION AVFAC, VDFAC, ADFAC, DTT, A0, V0, D0, &
       DTAV2, DTV0, DTAD6, AI, ASUM, A6, A6SUM


      ! Set up time step constants
      DTAV2 = DTT * AVFAC / 2
      DTV0 = DTT * VDFAC * V0
      DTAD6 = ( DTT**2 ) * ADFAC / 6

      ! Set up tail and integration values
      A0 = A(0)
      V(0) = REAL( V0 )
      D(0) = REAL( D0 )

      ! Pre-zero integration
      ASUM = A0
      A6 = 0.D0
      A6SUM = 0.D0
      DO I = -1, NFP, -1
        AI = A(I)
        V(I) = REAL( V0 - DTAV2 * ( AI + ASUM ) )
        D(I) = REAL( D0 + I * DTV0 + DTAD6 * ( AI + A6SUM - ( 3 * I + 1 ) * A0 ) )
        ASUM = 2 * AI + ASUM
        A6 = 6 * AI + A6
        A6SUM = A6 + A6SUM
      END DO

      ! Post-zero integration
      ASUM = A0
      A6 = 0.D0
      A6SUM = 0.D0
      DO I = 1, NLP
        AI = A(I)
        V(I) = REAL( V0 + DTAV2 * ( AI + ASUM ) )
        D(I) = REAL( D0 + I * DTV0 + DTAD6 * ( AI + A6SUM + ( 3 * I - 1 ) * A0 ) )
        ASUM = 2 * AI + ASUM
        A6 = 6 * AI + A6
        A6SUM = A6 + A6SUM
      END DO

      RETURN
      END



      SUBROUTINE VIN( AVFAC, VDFAC, ADFAC, DTT, NFP, NLP, &
       A, A0, V, V0, D, D0, NFP_S, NLP_S )

!***********************************************************************
!* Integrates/Differentiates Velocity Input
!***********************************************************************

      INTEGER NFP, NLP, NFP_S, NLP_S, I

      REAL A(NFP_S:NLP_S), V(NFP_S:NLP_S), D(NFP_S:NLP_S)

      DOUBLE PRECISION AVFAC, VDFAC, ADFAC, DTT, A0, V0, D0, &
       DTVA, DTVD3, DTAD6, DTA0, VM, VI, VP, VESUM, VOSUM


      ! Set up time step constants
      DTVA = 1.D0 / ( 2 * DTT * AVFAC )
      DTVD3 = DTT * VDFAC / 3
      DTAD6 = ( DTT**2 ) * ADFAC / 6

      ! Pre-zero integration/differentiation
      IF ( NFP >= 0 ) THEN
        A0 = ( -7 * V0 + 6 * V(1) + 3 * V(2) - 2 * V(3) ) * DTVA / 3
        DTA0 = DTAD6 * A0
      ELSE
        A0 = ( V(1) - V(-1) ) * DTVA
        DTA0 = DTAD6 * A0
        VI = V0
        VM = V(-1)
        VESUM = 4 * VM + V0
        VOSUM = 2 * V0
        DO I = -1, NFP+1, -1
          VP = VI
          VI = VM
          VM = V(I-1)
          A(I) = REAL( ( VP - VM ) * DTVA )
          IF ( MOD( I, 2 ) == 0 ) THEN
            D(I) = REAL( D0 - DTVD3 * ( VI + VESUM ) )
            VESUM = VESUM + 4 * VM + 2 * VI
          ELSE
            D(I) = REAL( D0 - DTVD3 * ( VI + VOSUM ) + DTA0 )
            VOSUM = VOSUM + 4 * VM + 2 * VI
          END IF
        END DO
        I = NFP
        VI = VM
        A(I) = REAL( ( -7 * VI + 6 * V(I+1) + 3 * V(I+2) - 2 * V(I+3) ) * DTVA / 3 )
        IF ( MOD( I, 2 ) == 0 ) THEN
          D(I) = REAL( D0 - DTVD3 * ( VI + VESUM ) )
        ELSE
          D(I) = REAL( D0 - DTVD3 * ( VI + VOSUM ) + DTA0 )
        END IF
      END IF

      ! Post-zero integration/differentiation
      A(0) = REAL( A0 )
      D(0) = REAL( D0 )
      VI = V0
      VP = V(1)
      VESUM = 4 * VP + V0
      VOSUM = 2 * V0
      DO I = 1, NLP-1
        VM = VI
        VI = VP
        VP = V(I+1)
        A(I) = REAL( ( VP - VM ) * DTVA )
        IF ( MOD( I, 2 ) == 0 ) THEN
          D(I) = REAL( D0 + DTVD3 * ( VI + VESUM ) )
          VESUM = VESUM + 4 * VP + 2 * VI
        ELSE
          D(I) = REAL( D0 + DTVD3 * ( VI + VOSUM ) + DTA0 )
          VOSUM = VOSUM + 4 * VP + 2 * VI
        END IF
      END DO
      I = NLP
      VI = VP
      A(I) = REAL( ( 7 * VI - 6 * V(I-1) - 3 * V(I-2) + 2 * V(I-3) ) * DTVA / 3 )
      IF ( MOD( I, 2 ) == 0 ) THEN
        D(I) = REAL( D0 + DTVD3 * ( VI + VESUM ) )
      ELSE
        D(I) = REAL( D0 + DTVD3 * ( VI + VOSUM ) + DTA0 )
      END IF

      RETURN
      END



      SUBROUTINE DIN( VDFAC, AVFAC, DTT, NFP, NLP, &
       A, A0, V, V0, D, D0, V0D, NFP_S, NLP_S )

!***********************************************************************
!* Differentiates Displacement Input
!***********************************************************************

      INTEGER NFP, NLP, NFP_S, NLP_S, I

      REAL A(NFP_S:NLP_S), V(NFP_S:NLP_S), D(NFP_S:NLP_S)

      DOUBLE PRECISION VDFAC, AVFAC, DTT, A0, V0, D0, V0D, &
       DTDV, DTVA, DM2, &
       DM, DI, DP, DP2, DP3, &
       VM2, VM, VI, VP, VP2, VP3


      ! Set up time step constants
      DTDV = 1.D0 / ( 2 * DTT * VDFAC )
      DTVA = 1.D0 / ( 2 * DTT * AVFAC )

      ! First differentiation
      IF ( NFP >= 0 ) THEN
        V0D = ( -7 * D0 + 6 * D(1) + 3 * D(2) - 2 * D(3) ) * DTDV / 3
      ELSE
        V0D = ( D(1) - D(-1) ) * DTDV
      END IF
      I = NFP
      DM = D(I) ! Suppress may be used uninitialized warning
      DI = D(I)
      DP = D(I+1)
      DP2 = D(I+2)
      DP3 = D(I+3)
      V(I) = REAL( ( -7 * DI + 6 * DP + 3 * DP2 - 2 * DP3 ) * DTDV / 3 )
      DM2 = D(NLP-3)
      DO I = NFP+1, NLP-1
        DM = DI
        DI = DP
        DP = D(I+1)
        V(I) = REAL( ( DP - DM ) * DTDV )
      END DO
      V(NLP) = REAL( ( 7 * DP - 6 * DI - 3 * DM + 2 * DM2 ) * DTDV / 3 )

      ! Second differentiation
      IF ( NFP >= 0 ) THEN
        A0 = ( -7 * V0 + 6 * V(1) + 3 * V(2) - 2 * V(3) ) * DTVA / 3
      ELSE
        A0 = ( V(1) - V(-1) ) * DTVA
      END IF
      I = NFP
      VM = V(I) ! Suppress may be used uninitialized warning
      VI = V(I)
      VP = V(I+1)
      VP2 = V(I+2)
      VP3 = V(I+3)
      A(I) = REAL( ( -7 * VI + 6 * VP + 3 * VP2 - 2 * VP3 ) * DTVA / 3 )
      VM2 = V(NLP-3)
      DO I = NFP+1, NLP-1
        VM = VI
        VI = VP
        VP = V(I+1)
        A(I) = REAL( ( VP - VM ) * DTVA )
      END DO
      A(NLP) = REAL( ( 7 * VP - 6 * VI - 3 * VM + 2 * VM2 ) * DTVA / 3 )

      RETURN
      END



      SUBROUTINE TIMING( PI, AVFAC, ADFAC, NLP_I, DTT, FTT, FTD, FTD_C, &
       NDD, NDDH, NDDHM, NDD2, TWON, PIT, PIT2, BAVFAC, BADFAC, &
       W0, FMD, FMC, A, AN, V, VN, D, DN, NFP_S, NLP_S )

!***********************************************************************
!* Sets Up Timing Values for DFT
!***********************************************************************

      INTEGER NLP_I, NFP_S, NLP_S, NDD, NDDH, NDDHM, NDD2

      REAL A(NFP_S:NLP_S), V(NFP_S:NLP_S), D(NFP_S:NLP_S)

      DOUBLE PRECISION PI, AVFAC, ADFAC, DTT, FTT, FTD, &
       TWON, PIT, PIT2, BAVFAC, BADFAC, W0, FMD, FMC, AN, VN, DN

      CHARACTER FTD_C*(*)


      ! Set DFT final time value
      IF ( FTD_C == ' ' ) THEN ! Use signal tail
        NDD = NLP_I
        FTD = FTT
      ELSE ! Assign specified value
        NDD = MAX( MIN( INT( (FTD/DTT+1.D0)*(1.D0-1.D-14) ), NLP_I), 2 )
        FTD = NDD * DTT
      END IF

      ! Set up timing related constants
      NDDH = NDD / 2
      NDDHM = ( NDD - 1 ) / 2
      NDD2 = NDD * 2
      TWON = 2.D0 / NDD
      PIT = PI / NDD
      PIT2 = PIT * 2
      BAVFAC = -( FTD / PI ) * AVFAC
      BADFAC = -( ( FTD / PI )**2 ) * ADFAC
      W0 = 1.D0 / ( 2 * FTD )
      FMD = ( NDD - 1 ) * W0
      FMC = NDD * W0

      ! Set end tail values
      AN = A(NDD)
      VN = V(NDD)
      DN = D(NDD)

      RETURN
      END



      SUBROUTINE SMOOTH( PI, AVFAC, VDFAC, ADFAC, ATYP, DTYP, &
       AFL, VFL, FCUT_DEF, FCUT_DEF_C, NFP_I, NLP_I, DTT, NDD, &
       ASM, VSM, DSM, FS0, FS0_C, FSN, FSN_C, &
       A, A0, AN, V, V0, VN, D, D0, DN, &
       H, NFP_S, NLP_S, REPORT )

!***********************************************************************
!* Performs Tail Smoothing
!***********************************************************************

      ! Headers
      INCLUDE 'file.fi'
      INCLUDE 'uds_fxn.fi'

      RECORD /FILE/  REPORT

      LOGICAL ASM, VSM, DSM, H_SET, FAIL

      INTEGER NFP_I, NLP_I, NDD, NFP_S, NLP_S, I, J, NS, NPT, NIP, NI

      REAL A(NFP_S:NLP_S), V(NFP_S:NLP_S), D(NFP_S:NLP_S), H(0:NLP_S)

      DOUBLE PRECISION PI, AVFAC, VDFAC, ADFAC, FCUT_DEF, DTT, &
       A0, AN, V0, VN, D0, DN, &
       FS0, FSN, OMS, RHO, OMSRHO, A0S, ANS, V0S, VNS, &
       D0S, DNS, AC, VC, TP, AP, VP, DP, PIT, PIT2, PITI, PIT2I, &
       A0A, ASA, B1A, B2A, V0V, A0V, ASV, B1V, B2V, &
       D0D, V0D, A0D, ASD, B1D, B2D, &
       T, TS, TC, TPT, ST, SPT, SPPT, VI, DI, VAFAC2

      PARAMETER ( RHO = 2.D0 )

      CHARACTER ATYP*(*), DTYP*(*), AFL, VFL, &
       FCUT_DEF_C*(*), FS0_C*(*), FSN_C*(*), MSG_STR*79


      ! Initializations
      VAFAC2 = 2 / AVFAC

      ! Warn about unsmoothed outputs
      IF ( .NOT. ( ASM .OR. VSM .OR. DSM ) ) RETURN
      IF ( ( VSM ) .AND. ( ATYP == 'ACCELERATION' ) ) THEN
        CALL MSG_WRITE( '*** WARNING - Smoothed time-zero '// &
         'VELOCITY not stored in output file INIVEL field', &
         REPORT )
      END IF
      IF ( ( DSM ) .AND. ( ATYP == 'ACCELERATION' ) ) THEN
        MSG_STR = '*** WARNING - Smoothed time-zero'
        MSG_STR(LEN_TRIM(MSG_STR)+2:) = DTYP
        MSG_STR(LEN_TRIM(MSG_STR)+2:) = 'not stored in'
        MSG_STR(LEN_TRIM(MSG_STR)+2:) = AFL//' or '//VFL//' files'
        CALL MSG_WRITE( MSG_STR, REPORT )
      END IF

      ! Time zero tail smoothing
      H_SET = .FALSE.
      IF ( FS0_C == ' ' ) THEN
        FS0 = FCUT_DEF
        FS0_C = FCUT_DEF_C
      END IF
      IF ( FS0_C /= 'N' ) THEN ! Use specified frequency

        ! Set up smoothing parameters
        OMS = 4 * PI * FS0 * DTT ! Freq factor of 2
        OMSRHO = OMS / RHO
        NS = MIN( INT( RHO / ( 4 * FS0 * DTT ) ), NLP_S )
        H(0) = REAL( 2 * OMS )
        DO I = 1, NS
          H(I) = REAL( ( 1.D0 + DCOS( OMSRHO * I ) ) * DSIN( OMS * I ) / I )
        END DO
        H_SET = .TRUE.

        IF ( NS > 0 ) THEN ! Perform smoothing

          ! Compute smoothed tail value
          J = 0
          CALL PTSMOOTH( NFP_I, NLP_I, A, V, D, H, NFP_S, NLP_S, &
           NS, A0S, V0S, D0S, J, FAIL )
          IF ( FAIL ) GO TO 101
          IF ( .NOT. ASM ) A0S = A0
          IF ( .NOT. VSM ) V0S = V0
          IF ( .NOT. DSM ) D0S = D0

          ! Set up tail smoothing parameters
          NPT = MIN( NS*3, NDD/2 ) ! Use 3x the impulse response span
          TP = NPT * DTT
          VC = V0S - ( D0 - D0S ) / ( TP * VDFAC )
          AC = A0S - 2 * ( V0 - VC ) / ( TP * AVFAC )
          AP = A(NPT)
          VP = V(NPT)
          DP = D(NPT)
          PIT = PI / NPT
          PIT2 = PIT * 2

          ! Compute smoothing-baseline parameters
          CALL BLINE( PI, AVFAC, VDFAC, ADFAC, TP, &
           AC, AP, VC, VP, D0S, DP, &
           A0A, ASA, B1A, B2A, V0V, A0V, ASV, B1V, B2V, &
           D0D, V0D, A0D, ASD, B1D, B2D )

          ! Set tail values
          A0 = A0S
          A(0) = REAL( A0 )
          V0 = V0S
          V(0) = REAL( V0 )
          D0 = D0S
          D(0) = REAL( D0 )

          ! Perform tail smoothing
          DO I = 1, NPT-1
            T = I * DTT
            TS = T**2
            TC = T**3
            TPT = ( NPT - I ) * DTT
            PITI = PIT * I
            PIT2I = PIT2 * I
            ST = D0D + V0D * T + A0D * TS + ASD * TC + B1D * DSIN( PITI ) + B2D * DSIN( PIT2I )
            SPT = V0V + A0V * T + ASV * TS + B1V * DCOS( PITI ) + B2V * DCOS( PIT2I )
            SPPT = A0A + ASA * T + B1A * DSIN( PITI ) + B2A * DSIN( PIT2I )
            VI = V(I)
            DI = D(I)
            A(I) = REAL( ( VI * VAFAC2 + T * A(I) - SPT * VAFAC2 + TPT * SPPT ) / TP )
            V(I) = REAL( ( DI / VDFAC + T * VI - ST / VDFAC + TPT * SPT ) / TP )
            D(I) = REAL( ( T * DI + TPT * ST ) / TP )
          END DO

        END IF

  101 END IF

      ! End tail smoothing
      IF ( FSN_C == ' ' ) THEN
        FSN = FCUT_DEF
        FSN_C = FCUT_DEF_C
      END IF
      IF ( FSN_C /= 'N' ) THEN ! Use specified frequency

        ! Set up smoothing parameters
        IF ( ( FSN /= FS0 ) .OR. ( .NOT. H_SET ) ) THEN
          ! Compute impulse response
          OMS = 4 * PI * FSN * DTT
          OMSRHO = OMS / RHO
          NS = MIN( INT( RHO / ( 4 * FSN * DTT ) ), NLP_S )
          H(0) = REAL( 2 * OMS )
          DO I = 1, NS
            H(I) = REAL( ( 1.D0 + DCOS( OMSRHO * I ) ) * DSIN( OMS * I ) / I )
          END DO
        END IF

        IF ( NS > 0 ) THEN ! Perform smoothing

          ! Compute smoothed tail value
          J = NDD
          CALL PTSMOOTH( NFP_I, NLP_I, A, V, D, H, NFP_S, NLP_S, NS, ANS, VNS, DNS, J, FAIL )
          IF ( FAIL ) RETURN
          IF ( .NOT. ASM ) ANS = AN
          IF ( .NOT. VSM ) VNS = VN
          IF ( .NOT. DSM ) DNS = DN

          ! Set up tail smoothing parameters
          NPT = MIN( NS*3, NDD/2 ) ! Use 3x the impulse response span
          NIP = NDD - NPT
          TP = NPT * DTT
          VC = VNS + ( DN - DNS ) / ( TP * VDFAC )
          AC = ANS + 2 * ( VN - VC ) / ( TP * AVFAC )
          AP = A(NIP)
          VP = V(NIP)
          DP = D(NIP)
          PIT = PI / NPT
          PIT2 = PIT * 2

          ! Compute smoothing-baseline parameters
          CALL BLINE( PI, AVFAC, VDFAC, ADFAC, TP, &
           AC, AP, -VC, -VP, DNS, DP, &
           A0A, ASA, B1A, B2A, V0V, A0V, ASV, B1V, B2V, &
           D0D, V0D, A0D, ASD, B1D, B2D )

          ! Set tail values
          AN = ANS
          A(NDD) = REAL( AN )
          VN = VNS
          V(NDD) = REAL( VN )
          DN = DNS
          D(NDD) = REAL( DN )

          ! Perform tail smoothing
          DO I = 1, NPT-1
            T = I * DTT
            TS = T**2
            TC = T**3
            TPT = ( NPT - I ) * DTT
            PITI = PIT * I
            PIT2I = PIT2 * I
            ST = D0D + V0D * T + A0D * TS + ASD * TC + B1D * DSIN( PITI ) + B2D * DSIN( PIT2I )
            SPT = V0V + A0V * T + ASV * TS + B1V * DCOS( PITI ) + B2V * DCOS( PIT2I )
            SPPT = A0A + ASA * T + B1A * DSIN( PITI ) + B2A * DSIN( PIT2I )
            NI = NDD - I
            VI = V(NI)
            DI = D(NI)
            A(NI) = REAL( ( -VI * VAFAC2 + T * A(NI) - SPT * VAFAC2 + TPT * SPPT ) / TP )
            V(NI) = REAL( -( DI / VDFAC - T * VI - ST / VDFAC + TPT * SPT ) / TP )
            D(NI) = REAL( ( T * DI + TPT * ST ) / TP )
          END DO

        END IF

      END IF

      RETURN
      END



      SUBROUTINE PTSMOOTH( NFP_I, NLP_I, A, V, D, H, NFP_S, NLP_S, &
       NS, AJ, VJ, DJ, J, FAIL )

!***********************************************************************
!* Performs Point Tail Smoothing
!***********************************************************************

      LOGICAL FAIL

      INTEGER NFP_I, NLP_I, NFP_S, NLP_S, NS, J, I, IJ, NL, NR, NT, &
       NLA, NRA, ISUM, IASUM

      REAL A(NFP_S:NLP_S), V(NFP_S:NLP_S), D(NFP_S:NLP_S), H(0:NLP_S)

      DOUBLE PRECISION AJ, VJ, DJ, HI, HSUM, HISUM, SISUM, HA, HB, DEN


      ! Compute smoothed tail
      FAIL = .FALSE.
      AJ = 0.D0
      VJ = 0.D0
      DJ = 0.D0
      HSUM = 0.D0
      HISUM = 0.D0
      NL = MAX( J-NS, NFP_I )
      NR = MIN( J+NS, NLP_I )
      DO I = NL, NR
        IJ = I - J
        HI = H( ABS( IJ ) )
        AJ = AJ + HI * A(I)
        VJ = VJ + HI * V(I)
        DJ = DJ + HI * D(I)
        HSUM = HSUM + HI
        HISUM = HISUM + HI * IJ
      END DO
      IF ( HSUM == 0.D0 ) GO TO 199
      AJ = AJ / HSUM
      VJ = VJ / HSUM
      DJ = DJ / HSUM

      ! Asymmetry correction
      IF ( J-NL /= NR-J ) THEN
        NT = NR - NL + 1
        NLA = J - NL
        NRA = NR - J
        ISUM = ( -NLA * ( NLA + 1 ) + NRA * ( NRA + 1 ) ) / 2
        IASUM = ( NLA * ( NLA + 1 ) + NRA * ( NRA + 1 ) ) / 2
        IF ( IASUM == 0 ) GO TO 199
        SISUM = ( -DBLE( NLA ) * ( NLA + 1 ) * ( 2 * NLA + 1 ) + &
         DBLE( NRA ) * ( NRA + 1 ) * ( 2 * NRA + 1 ) ) / 6
        HISUM = HISUM / HSUM
        DEN = NT * SISUM / IASUM - ISUM
        IF ( DEN == 0.D0 ) GO TO 199
        HA = HISUM / DEN
        DEN = DBLE( ISUM ) * IASUM / NT - SISUM
        IF ( DEN == 0.D0 ) GO TO 199
        HB = HISUM / DEN

        ! Apply correction
        DO I = NL, NR
          AJ = AJ + ( HA + HB * ABS( I - J ) ) * A(I)
          VJ = VJ + ( HA + HB * ABS( I - J ) ) * V(I)
          DJ = DJ + ( HA + HB * ABS( I - J ) ) * D(I)
        END DO
      END IF

      RETURN

      ! Alternate return on zero divisor
  199 WRITE( *, '(/A)' ) ' *** WARNING - Specified tail smoothing is not computable'
      FAIL = .TRUE.
      RETURN

      END



      SUBROUTINE BLINE( PI, AVFAC, VDFAC, ADFAC, TB, &
       A0, AN, V0, VN, D0, DN, A0A, ASA, B1A, B2A, &
       V0V, A0V, ASV, B1V, B2V, D0D, V0D, A0D, ASD, B1D, B2D )

!***********************************************************************
!* Computes Baseline Function Parameters
!***********************************************************************

      DOUBLE PRECISION PI, AVFAC, VDFAC, ADFAC, TB, &
       A0, AN, V0, VN, D0, DN, A0A, ASA, B1A, B2A, &
       V0V, A0V, ASV, B1V, B2V, D0D, V0D, A0D, ASD, B1D, B2D, &
       ADIF, ASUM, A2SUM, VDIF, VSUM, DDIF


      ! Compute signal tail constants
      ADIF = AN - A0
      ASUM = AN + A0
      A2SUM = AN + 2 * A0
      VDIF = VN - V0
      VSUM = VN + V0
      DDIF = DN - D0

      ! Compute acceleration domain baseline parameters
      A0A = A0
      ASA = ( 1.D0 / TB ) * ADIF
      B1A = ( PI / ( 2 * TB ) ) * VDIF / AVFAC - ( PI / 4 ) * ASUM
      B2A = ( ( 2 * PI ) / ( TB**2 ) ) * DDIF / ADFAC - &
       ( PI / TB ) * VSUM / AVFAC + ( PI / 6 ) * ADIF

      ! Compute velocity domain baseline parameters
      V0V = ( 1.D0 / TB ) * DDIF / VDFAC - ( TB / 6 ) * A2SUM * AVFAC
      A0V = A0 * AVFAC
      ASV = ( 1.D0 / ( 2 * TB ) ) * ADIF * AVFAC
      B1V = -( 1.D0 / 2 ) * VDIF + ( TB / 4 ) * ASUM * AVFAC
      B2V = -( 1.D0 / TB ) * DDIF / VDFAC + ( 1.D0 / 2 ) * VSUM - &
       ( TB / 12 ) * ADIF * AVFAC

      ! Compute displacement domain baseline parameters
      D0D = D0
      V0D = ( 1.D0 / TB ) * DDIF - ( TB / 6 ) * A2SUM * ADFAC
      A0D = ( 1.D0 / 2 ) * A0 * ADFAC
      ASD = ( 1.D0 / ( 6 * TB ) ) * ADIF * ADFAC
      B1D = -( TB / ( 2 * PI ) ) * VDIF * VDFAC + &
       ( TB**2 / ( 4 * PI ) ) * ASUM * ADFAC
      B2D = -( 1.D0 / ( 2 * PI ) ) * DDIF + &
       ( TB / ( 4 * PI ) ) * VSUM * VDFAC - &
       ( TB**2 / ( 24 * PI ) ) * ADIF * ADFAC

      RETURN
      END



      SUBROUTINE GET_CUT( U, FSTOP, FCUT_DEF, FCUT_DEF_C, &
       NDD, W0, FMD, CF, FC, SF, CUTF, CUTF_C, &
       CORF, STPF, KC, KS, FILMOD )

!***********************************************************************
!* Set Up for Specified Cutoff Frequency and Filtering Mode
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'

      RECORD /UDS_Y/  U

      INTEGER NDD, KC, KS

      DOUBLE PRECISION FSTOP, FCUT_DEF, W0, FMD, CF, FC, SF, &
       CUTF, CORF, STPF

      CHARACTER FCUT_DEF_C*(*), CUTF_C*(*), FILMOD


      ! Assign cutoff frequency value
      IF ( CUTF_C == ' ' ) THEN ! Default frequency
        IF ( FCUT_DEF_C(1:1) == 'N' ) THEN ! Max frequency
          FILMOD = 'M'
          CUTF = FMD
          CORF = FMD
          STPF = FMD
          U.FCUT = REAL( FC )
          U.FCOR = REAL( CF )
          U.FSTP = REAL( SF )
        ELSE ! Default frequency
          FILMOD = ' '
          CUTF = FCUT_DEF
          CORF = FCUT_DEF * .5D0
          STPF = FCUT_DEF * 2.5D0
          U.FCUT = REAL( FCUT_DEF )
          U.FCOR = REAL( FCUT_DEF * .5D0 )
          U.FSTP = REAL( FCUT_DEF * 2.5D0 )
        END IF
      ELSE IF ( CUTF_C == 'S' ) THEN ! Stop frequency mode
        FILMOD = 'S'
        CUTF = FSTOP
        CORF = FSTOP
        STPF = FSTOP
        U.FCUT = REAL( FC )
        U.FCOR = REAL( CF )
        U.FSTP = REAL( SF )
      ELSE IF ( CUTF_C == 'M' ) THEN ! Max frequency mode
        FILMOD = 'M'
        CUTF = FMD
        CORF = FMD
        STPF = FMD
        U.FCUT = REAL( FC )
        U.FCOR = REAL( CF )
        U.FSTP = REAL( SF )
      ELSE ! Specified cutoff frequency
        FILMOD = ' '
        CORF = .5D0 * CUTF
        STPF = 2.5D0 * CUTF
        U.FCUT = REAL( CUTF )
        U.FCOR = REAL( CORF )
        U.FSTP = REAL( STPF )
      END IF

      ! Compute corner and stop frequency indices
      KS = MIN( INT( ( STPF / W0 ) * (1.+1.D-15) ), NDD-1 )
      KC = MIN( INT( ( CORF / W0 ) * (1.+1.D-15) ), KS )

      RETURN
      END



      SUBROUTINE GET_STOP( FSTOP, FSTP_DEF, FSTP_DEF_C, &
       NDD, W0, FMD, STPF, STPF_C, KS )

!***********************************************************************
!* Set Up for Specified Stop Frequency for DFT Output
!***********************************************************************

      INTEGER NDD, KS, IOS_W

      DOUBLE PRECISION FSTOP, FSTP_DEF, W0, FMD, STPF

      CHARACTER FSTP_DEF_C*(*), STPF_C*(*)


      ! Assign stop frequency value
      IF ( STPF_C == ' ' ) THEN ! Default frequency
        IF ( FSTP_DEF_C(1:1) == 'M' ) THEN ! Max frequency
          STPF = FMD
        ELSE ! Default frequency
          STPF = FSTP_DEF
        END IF
      ELSE IF ( STPF_C == 'S' ) THEN ! Stop frequency
        STPF = FSTOP
      ELSE IF ( STPF_C == 'M' ) THEN ! Max frequency
        STPF = FMD
      ELSE ! Specified stop frequency
        IF ( STPF > FMD ) THEN
          WRITE( *, '(/A,F8.2)', IOSTAT=IOS_W ) &
           ' *** WARNING - DFT max frequency set to Nyquist = ', &
           FMD
          STPF = FMD
        END IF
      END IF

      ! Compute stop frequency index
      KS = MIN( INT( ( STPF / W0 ) * ( 1.+1.D-15 ) ), NDD-1 )

      RETURN
      END



      SUBROUTINE ADFT( DTT, NDD, NDDH, NDDHM, NDD2, TWON, PIT, PIT2, &
       A, A0A, ASA, B1A, B2A, AO, AE, SC, KS, BA, NLP_S )

!***********************************************************************
!* Computes the A-DFT from the Residual Acceleration Signal
!***********************************************************************

      INTEGER NDD, NDDH, NDDHM, NDD2, KS, NLP_S, NO, NE, NI, I, L

      REAL A(0:NLP_S), AO(0:NLP_S/2), AE(0:NLP_S/2), &
       SC(0:2*NLP_S), BA(0:NLP_S), SCL

      DOUBLE PRECISION DTT, TWON, PIT, PIT2, A0A, ASA, B1A, B2A, &
       AI, ANI


      ! Set up odd and even length indices
      NO = NDDH
      NE = NDDHM

      ! Compute the zeroed, folded acceleration
      AO(0) = 0.
      AE(0) = 0.
      DO I = 1, NDDHM
        NI = NDD - I
        AI = A(I) - A0A - ASA * ( I * DTT ) - B1A * DSIN( PIT * I ) - B2A * DSIN( PIT2 * I )
        ANI = A(NI) - A0A - ASA * ( NI * DTT ) - B1A * DSIN( PIT * NI ) - B2A * DSIN( PIT2 * NI )
        AO(I) = REAL( AI + ANI )
        AE(I) = REAL( AI - ANI )
      END DO
      IF ( NO > NDDHM ) THEN
        AO(NO) = REAL( A(NO) - A0A - ASA * ( NO * DTT ) - B1A * DSIN( PIT * NO ) - B2A * DSIN( PIT2 * NO ) )
      END IF

      ! Set up sine array for computing DFT
      DO L = 0, NDDH
        SCL = REAL( SIN( PIT * L ) )
        SC(L) = SCL
        SC(NDD-L) = SCL
        SC(NDD+L) = -SCL
        SC(NDD2-L) = -SCL
      END DO

      ! Compute the residual acceleration DFT
      CALL DFTCAL( NDD2, TWON, NO, AO, NE, AE, SC, KS, BA, NLP_S )

      RETURN
      END



      SUBROUTINE VDFT( DTT, NDD, NDDH, NDDHM, NDD2, TWON, PIT, PIT2, &
       BAVFAC, V, V0V, A0V, ASV, B1V, B2V, VO, VE, SC, KS, BA, NLP_S )

!***********************************************************************
!* Computes the A-DFT from the Residual Velocity Signal
!***********************************************************************

      INTEGER NDD, NDDH, NDDHM, NDD2, KS, NLP_S, NO, NE, NI, I, L, J

      REAL V(0:NLP_S), VO(0:NLP_S/2), VE(0:NLP_S/2), &
       SC(0:2*NLP_S), BA(0:NLP_S), SCL

      DOUBLE PRECISION DTT, TWON, PIT, PIT2, BAVFAC, &
       V0V, A0V, ASV, B1V, B2V, T, TN, VI, VNI


      ! Set up odd and even length indices
      NO = NDDHM
      NE = NDDH

      ! Compute the zeroed, folded velocity
      VO(0) = 0.
      VE(0) = 0.
      DO I = 1, NDDHM
        T = I * DTT
        NI = NDD - I
        TN = NI * DTT
        VI = V(I) - V0V - A0V * T - ASV * T**2 - B1V * DCOS( PIT * I ) - B2V * DCOS( PIT2 * I )
        VNI = V(NI) - V0V - A0V * TN - ASV * TN**2 - B1V * DCOS( PIT * NI ) - B2V * DCOS( PIT2 * NI )
        VO(I) = REAL( VI - VNI )
        VE(I) = REAL( VI + VNI )
      END DO
      IF ( NE > NDDHM ) THEN
        VE(NE) = REAL( V(NE) - V0V - A0V * ( NE * DTT ) - ASV * ( NE * DTT )**2 - B1V * DCOS( PIT * NE ) - B2V * DCOS( PIT2 * NE ) )
      END IF

      ! Set up cosine array for computing DFT
      DO L = 0, NDDH
        SCL = REAL( COS( PIT * L ) )
        SC(L) = SCL
        SC(NDD-L) = -SCL
        SC(NDD+L) = -SCL
        SC(NDD2-L) = SCL
      END DO

      ! Compute the residual V-DFT
      CALL DFTCAL( NDD2, TWON, NO, VO, NE, VE, SC, KS, BA, NLP_S )

      ! Scale V-DFT to get the A-DFT
      DO J = 1, KS
        BA(J) = REAL( DBLE( BA(J) ) * J / BAVFAC )
      END DO

      RETURN
      END



      SUBROUTINE DDFT( DTT, NDD, NDDH, NDDHM, NDD2, TWON, PIT, PIT2, &
       BADFAC, D, D0D, V0D, A0D, ASD, B1D, B2D, DOD, DE, SC, &
       KS, BA, NLP_S )

!***********************************************************************
!* Computes the A-DFT from the Residual Displacement Signal
!***********************************************************************

      INTEGER NDD, NDDH, NDDHM, NDD2, KS, NLP_S, NO, NE, NI, I, L, J

      REAL D(0:NLP_S), DOD(0:NLP_S/2), DE(0:NLP_S/2), &
       SC(0:2*NLP_S), BA(0:NLP_S), SCL

      DOUBLE PRECISION DTT, TWON, PIT, PIT2, BADFAC, &
       D0D, V0D, A0D, ASD, B1D, B2D, T, TN, DI, DNI


      ! Set up odd and even length indices
      NO = NDDH
      NE = NDDHM

      ! Compute the zeroed, folded displacement
      DOD(0) = 0.
      DE(0) = 0.
      DO I = 1, NDDHM
        T = I * DTT
        NI = NDD - I
        TN = NI * DTT
        DI = D(I) - D0D - V0D * T - A0D * T**2 - ASD * T**3 - B1D * DSIN( PIT * I ) - B2D * DSIN( PIT2 * I )
        DNI = D(NI) - D0D - V0D * TN - A0D * TN**2 - ASD * TN**3 - B1D * DSIN( PIT * NI ) - B2D * DSIN( PIT2 * NI )
        DOD(I) = REAL( DI + DNI )
        DE(I) = REAL( DI - DNI )
      END DO
      IF ( NO > NDDHM ) THEN
        DOD(NO) = REAL( D(NO) - D0D - V0D * ( NO * DTT ) - &
         A0D * ( NO * DTT )**2 - ASD * ( NO * DTT )**3 - &
         B1D * DSIN( PIT * NO ) - B2D * DSIN( PIT2 * NO ) )
      END IF

      ! Set up sine array for computing DFT
      DO L = 0, NDDH
        SCL = REAL( SIN( PIT * L ) )
        SC(L) = SCL
        SC(NDD-L) = SCL
        SC(NDD+L) = -SCL
        SC(NDD2-L) = -SCL
      END DO

      ! Compute the residual D-DFT
      CALL DFTCAL( NDD2, TWON, NO, DOD, NE, DE, SC, KS, BA, NLP_S )

      ! Scale D-DFT to get the A-DFT
      DO J = 1, KS
        BA(J) = REAL( DBLE( BA(J) ) * J**2 / BADFAC )
      END DO

      RETURN
      END



      SUBROUTINE DFTCAL( NDD2, TWON, NO, YO, NE, YE, SC, KS, BY, &
       NLP_S )

!***********************************************************************
!* Computes the DFT of the Folded Residual Input Signal
!***********************************************************************

      INTEGER NDD2, NO, NE, KS, NLP_S, I, J, L

      REAL YO(0:NLP_S/2), YE(0:NLP_S/2), SC(0:2*NLP_S), BY(0:NLP_S)

      DOUBLE PRECISION TWON, BJ


      ! Compute odd DFT coefficients
      DO J = 1, KS, 2
        L = J
        BJ = 0.D0
        DO I = 1, NO
          BJ = BJ + YO(I) * SC(L)
          L = MOD( L+J, NDD2 )
        END DO
        BY(J) = REAL( TWON * BJ )
      END DO

      ! Compute even DFT coefficients
      DO J = 2, KS, 2
        L = J
        BJ = 0.D0
        DO I = 1, NE
          BJ = BJ + YE(I) * SC(L)
          L = MOD( L+J, NDD2 )
        END DO
        BY(J) = REAL( TWON * BJ )
      END DO

      RETURN
      END



      SUBROUTINE FILTER( FILE_NAME, U, PI, ALPHA, ATYP, VTYP, DTYP, &
       AFL, VFL, DFL, AUN, VUN, DUN, FC, CF, DEL_I, DTT, FTD, &
       FTF, FTF_C, DTF, DTF_C, BAVFAC, BADFAC, W0, FMD, FMC, &
       A, A0, AN, V, V0, VN, D, D0, DN, AF, VF, DF, &
       A0A, ASA, B1A, B2A, V0V, A0V, ASV, B1V, B2V, &
       D0D, V0D, A0D, ASD, B1D, B2D, CUTF, CORF, STPF, &
       KC, KS, BA, BAW, BVW, BDW, H, SINF, COSF, NFP_S, NLP_S, &
       OF_CTRL, AFIL, VFIL, DFIL, DEF_FILE, OUT_FILE, FILMOD, &
       DIR_OUT, NO_INCR, REPORT, OUTPUT_LIST, BATCH, NFREQ )

!***********************************************************************
!* Filters Residual Signals Given A-DFT and Restores Baseline
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'
      INCLUDE 'file.fi'

      RECORD /UDS_Y/  U
      RECORD /UDS_CONTROL/  C
      RECORD /FILE/  REPORT, OUTPUT_LIST

      LOGICAL NO_INCR, BATCH

      INTEGER KC, KS, NFP_S, NLP_S, NFREQ, &
       IOS, IOS_W, I, IDT, ISUB, NSUB, NLP_F, KSF, NDF

      REAL DEL_I, A(NFP_S:NLP_S), V(NFP_S:NLP_S), D(NFP_S:NLP_S), &
       AF(0:NLP_S), VF(0:NLP_S), DF(0:NLP_S), &
       BA(0:NLP_S), BAW(0:NLP_S), BVW(0:NLP_S), BDW(0:NLP_S), &
       H(0:NLP_S), SINF(0:4*NLP_S), COSF(0:4*NLP_S)

      DOUBLE PRECISION PI, ALPHA, FC, CF, DTT, FTD, FTF, DTF, &
       BAVFAC, BADFAC, W0, FMD, FMC, A0, AN, V0, VN, D0, DN, &
       A0A, ASA, B1A, B2A, V0V, A0V, ASV, B1V, B2V, &
       D0D, V0D, A0D, ASD, B1D, B2D, &
       CUTF, CORF, STPF

      CHARACTER FILE_NAME*(*), ATYP*(*), VTYP*(*), DTYP*(*), &
       AFL, VFL, DFL, AUN*(*), VUN*(*), DUN*(*), FILMOD, &
       FTF_C*(*), DTF_C*(*), OF_CTRL, AFIL*(*), VFIL*(*), DFIL*(*), &
       DEF_FILE*(*), OUT_FILE*(*), DIR_OUT*(*), FILCOD


      ! Functions ______________________________________________________

      CHARACTER DNCASE, UPCASE*25, LJUST*25

      EXTERNAL DNCASE, UPCASE, LJUST


      ! Banner for multiple frequencies
      IF ( NFREQ > 1 ) WRITE( *, '(//1X,F12.3,A)', IOSTAT=IOS_W ) &
       CUTF, ' (Hz) output files:'

      ! Set final time value
      IF ( FTF_C == ' ' ) THEN
        FTF = FTD
      ELSE IF ( FTF > FTD ) THEN
        WRITE( *, '(/A,F15.6,A)', IOSTAT=IOS_W ) &
         ' *** WARNING - Final time set to signal final time of ', &
         FTD, ' (sec)'
        FTF = FTD
      END IF

      ! Set filtered signal time step
      ISUB = 0
      IF ( ( DTF_C == ' ' ) .OR. ( DTF_C == 'I' ) ) THEN
        ! Input time step
        ISUB = 1
        DTF = DTT
      ELSE IF ( DTF_C(1:1) == 'I' ) THEN ! Subsampling requested
        READ( DTF_C(2:), '(BN,I24)', IOSTAT=IOS ) ISUB
        DTF = DTT * ISUB
      ELSE IF ( CORF >= FMD ) THEN ! Check for subsampling
        NSUB = NINT( DTF / DTT )
        IF ( EQUAL_DP( DTF, NSUB*DTT, 1.D-15 ) ) ISUB = NSUB
      END IF

      ! Set end index for filtered signals
      NLP_F = NINT( FTF / DTF )
      DO WHILE ( NLP_F * DTF > FTD )
        NLP_F = NLP_F - 1
      END DO
      IF ( NLP_F > NLP_S ) THEN
        WRITE( *, '(/A,I6)', IOSTAT=IOS_W ) &
         ' *** WARNING - Final index set to max value of: ', &
         NLP_S
        NLP_F = NLP_S
      END IF
      FTF = DTF * NLP_F

      ! Generate default filtered signal file names
      DEF_FILE = FILE_NAME
      CALL FN_EXTRACT( DEF_FILE )
      CALL ADD_PATH( DIR_OUT, DEF_FILE )
      CALL FILTER_FLAG( 'TIME SERIES', U.FCUT, U.FSTP, FILCOD )
      CALL FN_FLAG( DEF_FILE, DNCASE(AFL)//'f'//FILCOD, 3, IDT )
      IF ( .NOT. NO_INCR ) CALL FN_INCR( DEF_FILE )


      ! Get filtered signal file names
      IF ( ( BATCH ) .OR. ( OF_CTRL == 'G' ) ) THEN ! Default names
        IF ( AFIL /= 'N' ) THEN
          AFIL = DEF_FILE
          IF ( .NOT. NO_INCR ) CALL FN_INCR( AFIL )
        END IF
        IF ( VFIL /= 'N' ) THEN
          VFIL = DEF_FILE
          VFIL(IDT:IDT) = DNCASE( VFL )
          IF ( .NOT. NO_INCR ) CALL FN_INCR( VFIL )
        END IF
        IF ( DFIL /= 'N' ) THEN
          DFIL = DEF_FILE
          DFIL(IDT:IDT) = DNCASE( DFL )
          IF ( .NOT. NO_INCR ) CALL FN_INCR( DFIL )
        END IF
      ELSE ! Prompt for file names
  101   OUT_FILE = DEF_FILE
        IF ( .NOT. NO_INCR ) CALL FN_INCR( OUT_FILE )
        CALL FN_NAME_EXTRACT( OUT_FILE )
        WRITE( *, 601 ) ATYP, OUT_FILE(:L_TRIM(OUT_FILE))
        READ( *, '(A)', END=199 ) AFIL
        IF ( AFIL == ' ' ) THEN
          AFIL = OUT_FILE
        ELSE IF ( UPCASE( LJUST( AFIL ) ) == 'N' ) THEN
          AFIL = 'N'
        ELSE
          CALL ADD_PATH( DIR_OUT, AFIL )
          IF ( .NOT. FN_VALID( AFIL ) ) THEN
            WRITE( *, * ) '*** Invalid file name'
            GO TO 101
          END IF
        END IF

  102   OUT_FILE = DEF_FILE
        OUT_FILE(IDT:IDT) = DNCASE( VFL )
        IF ( .NOT. NO_INCR ) CALL FN_INCR( OUT_FILE )
        CALL FN_NAME_EXTRACT( OUT_FILE )
        WRITE( *, 601 ) VTYP, OUT_FILE(:L_TRIM(OUT_FILE))
        READ( *, '(A)', END=101 ) VFIL
        IF ( VFIL == ' ' ) THEN
          VFIL = OUT_FILE
        ELSE IF ( UPCASE( LJUST( VFIL ) ) == 'N' ) THEN
          VFIL = 'N'
        ELSE
          CALL ADD_PATH( DIR_OUT, VFIL )
          IF ( .NOT. FN_VALID( VFIL ) ) THEN
            WRITE( *, * ) '*** Invalid file name'
            GO TO 102
          END IF
        END IF

  103   OUT_FILE = DEF_FILE
        OUT_FILE(IDT:IDT) = DNCASE( DFL )
        IF ( .NOT. NO_INCR ) CALL FN_INCR( OUT_FILE )
        CALL FN_NAME_EXTRACT( OUT_FILE )
        WRITE( *, 601 ) DTYP, OUT_FILE(:L_TRIM(OUT_FILE))
        READ( *, '(A)', END=101 ) DFIL
        IF ( DFIL == ' ' ) THEN
          DFIL = OUT_FILE
        ELSE IF ( UPCASE( LJUST( DFIL ) ) == 'N' ) THEN
          DFIL = 'N'
        ELSE
          CALL ADD_PATH( DIR_OUT, DFIL )
          IF ( .NOT. FN_VALID( DFIL ) ) THEN
            WRITE( *, * ) '*** Invalid file name'
            GO TO 103
          END IF
        END IF

        IF ( ( AFIL == 'N' ) .AND. &
         ( VFIL == 'N' ) .AND. &
         ( DFIL == 'N' ) ) THEN
          WRITE( *, '(/A)' ) ' *** No outputs specified'
          GO TO 101
        END IF
      END IF

      IF ( ( CORF >= FMD ) .AND. ( ISUB > 0 ) ) THEN ! Subsample

        WRITE( *, '(/A)' ) &
         ' *** Subsampling for fast max frequency output'
        DO I = 0, NLP_F
          AF(I) = A(I*ISUB)
          VF(I) = V(I*ISUB)
          DF(I) = D(I*ISUB)
        END DO

      ELSE ! Filter

        ! Compute windowed, constrained DFT's
        CALL WINCON( PI, ALPHA, BAVFAC, BADFAC, W0, &
         B1A, B2A, B1V, B2V, B1D, B2D, CORF, STPF, KC, KS, KSF, BA, &
         VFIL, DFIL, BAW, BVW, BDW, H, NLP_S )

        ! Perform inverse DFT for filtered signals
        CALL FILREC( PI, FTD, DTF, NLP_F, KSF, AFIL, VFIL, DFIL, &
         BAW, BVW, BDW, NDF, AF, VF, DF, SINF, COSF, NLP_S )

        ! Restore polynomial baseline function to filtered signals
        CALL BLINER( NLP_F, DTF, NDF, A0, AN, V0, VN, D0, DN, &
         A0A, ASA, V0V, A0V, ASV, D0D, V0D, A0D, ASD, AF, VF, DF, NLP_S )

      END IF

      ! Set output fields
      U.XTYP = 'TIME'
      U.XUNITS = 'SECONDS'
      U.CURTYP = 'TIME SERIES'
      U.NFP = 0
      U.NLP = NLP_F
      IF ( ISUB == 1 ) THEN ! Use input (single precision) DEL
        U.DEL = DEL_I
      ELSE
        U.DEL = REAL( DTF )
      END IF
      IF ( ( FILMOD == 'S' ) .OR. ( FILMOD == 'M' ) ) THEN
        U.STATUS = 'FILTERED'
      ELSE IF ( ( ( FC > 0.D0 ) .AND. &
       ( STPF > FC * (.5+1.D-15) ) ) .OR. &
       ( ( CF > 0.D0 ) .AND. ( STPF > CF * (1.+1.D-15) ) ) .OR. &
       ( ( U.PREF > 0. ) .AND. &
       ( STPF > U.PREF * (.5+1.D-15) ) ) ) &
       THEN
        WRITE( *, '(/A)' ) ' *** WARNING - Filter overlap distortion present'
        U.STATUS = 'DISTORTED'
      ELSE IF ( STPF > FMC * (1.+1.D-14 ) ) THEN
        WRITE( *, '(/A)' ) ' *** WARNING - Filter window truncated by Nyquist limit'
        U.STATUS = 'NYQUIST LIMITED'
      ELSE IF ( DTF * STPF >= .5D0 ) THEN
        WRITE( *, '(/A)' ) ' *** WARNING - Aliasing present - Time step too large'
        U.STATUS = 'ALIASED'
      ELSE
        U.STATUS = 'FILTERED'
      END IF
      U.CD1 = 'SimFil Filtered'

      ! Output filtered data files
      CALL UDS_CONTROL_INIT( C )
      IF ( AFIL /= 'N' ) THEN
        U.YTYP = ATYP
        U.YUNITS = AUN
        DO I = U.NFP, U.NLP
          U.Y(I) = AF(I)
        END DO
        CALL UDS_PUT( AFIL, U, C, REPORT, OUTPUT_LIST, DIR_OUT, .TRUE., IOS )
      END IF
      IF ( VFIL /= 'N' ) THEN
        U.YTYP = VTYP
        U.YUNITS = VUN
        DO I = U.NFP, U.NLP
          U.Y(I) = VF(I)
        END DO
        CALL UDS_PUT( VFIL, U, C, REPORT, OUTPUT_LIST, DIR_OUT, .TRUE., IOS )
      END IF
      IF ( DFIL /= 'N' ) THEN
        U.YTYP = DTYP
        U.YUNITS = DUN
        DO I = U.NFP, U.NLP
          U.Y(I) = DF(I)
        END DO
        CALL UDS_PUT( DFIL, U, C, REPORT, OUTPUT_LIST, DIR_OUT, .TRUE., IOS )
      END IF

  199 RETURN

  601 FORMAT(/1X,A20,' file name?   ( N - None )'/' [',A,']'/' >> ',$)

      END



      SUBROUTINE WINCON( PI, ALPHA, BAVFAC, BADFAC, W0, &
       B1A, B2A, B1V, B2V, B1D, B2D, CORF, STPF, KC, KS, KSF, BA, &
       VFIL, DFIL, BAW, BVW, BDW, H, NLP_S )

!***********************************************************************
!* Computes Windowed, Velocity Tail Constrained DFT's from A-DFT
!***********************************************************************

      INTEGER KC, KS, KSF, NLP_S, J

      REAL BA(0:NLP_S), BAW(0:NLP_S), BVW(0:NLP_S), BDW(0:NLP_S), &
       H(0:NLP_S)

      DOUBLE PRECISION PI, ALPHA, BAVFAC, BADFAC, W0, B1A, B2A, &
       B1V, B2V, B1D, B2D, CORF, STPF, FDIF, CFR, W0FR, &
       OLAMN, OLAMD, OLAM, ELAMN, ELAMD, ELAM, BAWJ

      CHARACTER VFIL*(*), DFIL*(*)


      ! Compute frequency window coefficients
      DO J = 1, KC
        H(J) = 1.
      END DO
      IF ( ( STPF > CORF ) .AND. ( KS > KC ) ) THEN
        FDIF = STPF - CORF
        CFR = CORF / FDIF
        W0FR = W0 / FDIF
        DO J = KC+1, KS
          H(J) = REAL( ( 1.D0 + DCOS( ( ( J * W0FR - CFR )**ALPHA ) * PI ) ) / 2 )
        END DO
      END IF

      ! Compute odd Lagrange multiplier for velocity tail constraints
      OLAMN = 0.D0
      OLAMD = 0.D0
      DO J = 1, KS, 2
        OLAMN = OLAMN + ( H(J) * BA(J) ) / J
        OLAMD = OLAMD + H(J)**2
      END DO

      ! Compute even Lagrange multiplier for velocity tail constraints
      ELAMN = 0.D0
      ELAMD = 0.D0
      DO J = 2, KS, 2
        ELAMN = ELAMN + ( H(J) * BA(J) ) / J
        ELAMD = ELAMD + H(J)**2
      END DO

      ! Compute odd windowed, constrained DFT's
      IF ( KS < 3 ) THEN
        BAW(1) = 0.
        BVW(1) = 0.
        BDW(1) = 0.
      ELSE
        OLAM = OLAMN / OLAMD
        DO J = 1, KS, 2
          BAWJ = H(J) * BA(J) - OLAM * ( H(J)**2 ) * J
          BAW(J) = REAL( BAWJ )
          IF ( VFIL /= 'N' ) BVW(J) = REAL( BAWJ * BAVFAC / J )
          IF ( DFIL /= 'N' ) BDW(J) = REAL( BAWJ * BADFAC / ( J**2 ) )
        END DO
      END IF

      ! Compute even windowed, constrained DFT's
      IF ( KS < 4 ) THEN
        BAW(2) = 0.
        BVW(2) = 0.
        BDW(2) = 0.
      ELSE
        ELAM = ELAMN / ELAMD
        DO J = 2, KS, 2
          BAWJ = H(J) * BA(J) - ELAM * ( H(J)**2 ) * J
          BAW(J) = REAL( BAWJ )
          IF ( VFIL /= 'N' ) BVW(J) = REAL( BAWJ * BAVFAC / J )
          IF ( DFIL /= 'N' ) BDW(J) = REAL( BAWJ * BADFAC / ( J**2 ) )
        END DO
      END IF

      ! Set one extra DFT coefficient to zero for reconstruction
      BAW(KS+1) = 0.
      BVW(KS+1) = 0.
      BDW(KS+1) = 0.

      ! Add trigonometric baseline contribution to DFT's
      KSF = MAX( KS, 2 )
      BAW(1) = REAL( BAW(1) + B1A )
      BAW(2) = REAL( BAW(2) + B2A )
      BVW(1) = REAL( BVW(1) + B1V )
      BVW(2) = REAL( BVW(2) + B2V )
      BDW(1) = REAL( BDW(1) + B1D )
      BDW(2) = REAL( BDW(2) + B2D )

      RETURN
      END



      SUBROUTINE FILREC( PI, FTD, DTF, NLP_F, KSF, AFIL, VFIL, DFIL, &
       BAW, BVW, BDW, NDF, AF, VF, DF, SINF, COSF, NLP_S )

!***********************************************************************
!* Performs Filtered Residual Signal Reconstruction from DFT's
!***********************************************************************

      INTEGER NLP_F, KSF, NDF, NLP_S, &
       NSPF, NDFMAX, NDFH, NDF2, NDFI, NDFSIN, NDFCOS, I, J, L, IL

      REAL BAW(0:NLP_S), BVW(0:NLP_S), BDW(0:NLP_S), &
       AF(0:NLP_S), VF(0:NLP_S), DF(0:NLP_S), &
       SINF(0:4*NLP_S), COSF(0:4*NLP_S)

      DOUBLE PRECISION PI, FTD, DTF, PIT, SL, CL, &
       AO, AE, VO, VE, DD, DE, AI, VI, DI, TFAC, SJ

      CHARACTER AFIL*(*), VFIL*(*), DFIL*(*), RMETH

      SAVE NDFSIN, NDFCOS

      DATA NDFSIN / 0 /, NDFCOS / 0 /


      ! Analyze timing to see if fast reconstruction can be used
      RMETH = ' '
      PIT = 0.0D0 ! Suppress may be used uninitialized warning
      NSPF = 1
      NDF = NINT( FTD / DTF )
      NDFMAX = 2 * NLP_S
      IF ( DTF > FTD ) RMETH = 'S'
      DO WHILE ( RMETH == ' ' )
        IF ( NDF > NDFMAX ) THEN ! Use slow method
          RMETH = 'S'
        ELSE IF ( DMOD( DTF * (1.+1.D-15), FTD/NDF ) < DTF * (1.D-12 ) ) THEN
          RMETH = 'F'
          DTF = NSPF * FTD / NDF
        ELSE ! Try larger number of steps per filtered output step
          NSPF = NSPF + 1
          NDF = NINT( NSPF * FTD / DTF )
        END IF
      END DO

      ! Set up trigonometric values for reconstruction
      IF ( RMETH == 'F' ) THEN ! Fast method values
        NDFH = NDF / 2
        NDF2 = NDF * 2
        PIT = PI / NDF
        IF ( ( ( AFIL /= 'N' ) .OR. ( DFIL /= 'N' ) ) .AND. ( NDF /= NDFSIN ) ) THEN
          ! Set up new sine array
          DO L = 0, NDFH
            SL = SIN( PIT * L )
            SINF(L) = REAL( SL )
            SINF(NDF-L) = REAL( SL )
            SINF(NDF+L) = REAL( -SL )
            SINF(NDF2-L) = REAL( -SL )
          END DO
          NDFSIN = NDF
        END IF
        IF ( ( VFIL /= 'N' ) .AND. ( NDF /= NDFCOS ) ) THEN
          ! Set up new cosine array
          DO L = 0, NDFH
            CL = COS( PIT * L )
            COSF(L) = REAL( CL )
            COSF(NDF-L) = REAL( -CL )
            COSF(NDF+L) = REAL( -CL )
            COSF(NDF2-L) = REAL( CL )
          END DO
          NDFCOS = NDF
        END IF
      ELSE IF ( RMETH == 'S' ) THEN ! Slow method values
        NDF = -1
        WRITE( *, '(/A/4X,A)' ) &
         ' *** Slow filtered signal reconstruction used', &
         ' DFT time span and filter time step lack large gcd'
        PIT = PI * DTF / FTD
      END IF

      ! Produce filtered data through inverse DFT transform
      IF ( ( RMETH == 'F' ) .AND. ( NSPF == 1 ) .AND. ( NLP_F > NDFH*1.2 ) ) THEN
        ! Use fast method with time folding
        IF ( ( AFIL /= 'N' ) .AND. ( VFIL /= 'N' ) .AND. ( DFIL /= 'N' ) ) THEN
          ! Reconstruct A, V, and D
          DO I = 1, NDFH
            AO = 0.D0
            AE = 0.D0
            VO = 0.D0
            VE = 0.D0
            DD = 0.D0
            DE = 0.D0
            L = I
            DO J = 1, KSF, 2
              SL = SINF(L)
              AO = AO + BAW(J) * SL
              DD = DD + BDW(J) * SL
              VO = VO + BVW(J) * DBLE( COSF(L) )
              L = MOD( L+I, NDF2 )
              SL = SINF(L)
              AE = AE + BAW(J+1) * SL
              DE = DE + BDW(J+1) * SL
              VE = VE + BVW(J+1) * DBLE( COSF(L) )
              L = MOD( L+I, NDF2 )
            END DO
            AF(I) = REAL( AO + AE )
            VF(I) = REAL( VO + VE )
            DF(I) = REAL( DD + DE )
            NDFI = NDF - I
            IF ( NDFI <= NLP_F ) THEN
              AF(NDFI) = REAL( AO - AE )
              VF(NDFI) = REAL( VE - VO )
              DF(NDFI) = REAL( DD - DE )
            END IF
          END DO
        ELSE IF ( ( AFIL /= 'N' ) .AND. ( VFIL /= 'N' ) ) THEN
          ! Reconstruct A and V
          DO I = 1, NDFH
            AO = 0.D0
            AE = 0.D0
            VO = 0.D0
            VE = 0.D0
            L = I
            DO J = 1, KSF, 2
              AO = AO + BAW(J) * DBLE( SINF(L) )
              VO = VO + BVW(J) * DBLE( COSF(L) )
              L = MOD( L+I, NDF2 )
              AE = AE + BAW(J+1) * DBLE( SINF(L) )
              VE = VE + BVW(J+1) * DBLE( COSF(L) )
              L = MOD( L+I, NDF2 )
            END DO
            AF(I) = REAL( AO + AE )
            VF(I) = REAL( VO + VE )
            NDFI = NDF-I
            IF ( NDFI <= NLP_F ) THEN
              AF(NDFI) = REAL( AO - AE )
              VF(NDFI) = REAL( VE - VO )
            END IF
          END DO
        ELSE IF ( ( AFIL /= 'N' ) .AND. ( DFIL /= 'N' ) ) THEN
          ! Reconstruct A and D
          DO I = 1, NDFH
            AO = 0.D0
            AE = 0.D0
            DD = 0.D0
            DE = 0.D0
            L = I
            DO J = 1, KSF, 2
              SL = SINF(L)
              AO = AO + BAW(J) * SL
              DD = DD + BDW(J) * SL
              L = MOD( L+I, NDF2 )
              SL = SINF(L)
              AE = AE + BAW(J+1) * SL
              DE = DE + BDW(J+1) * SL
              L = MOD( L+I, NDF2 )
            END DO
            AF(I) = REAL( AO + AE )
            DF(I) = REAL( DD + DE )
            NDFI = NDF-I
            IF ( NDFI <= NLP_F ) THEN
              AF(NDFI) = REAL( AO - AE )
              DF(NDFI) = REAL( DD - DE )
            END IF
          END DO
        ELSE IF ( ( VFIL /= 'N' ) .AND. ( DFIL /= 'N' ) ) THEN
          ! Reconstruct V and D
          DO I = 1, NDFH
            VO = 0.D0
            VE = 0.D0
            DD = 0.D0
            DE = 0.D0
            L = I
            DO J = 1, KSF, 2
              VO = VO + BVW(J) * DBLE( COSF(L) )
              DD = DD + BDW(J) * DBLE( SINF(L) )
              L = MOD( L+I, NDF2 )
              VE = VE + BVW(J+1) * DBLE( COSF(L) )
              DE = DE + BDW(J+1) * DBLE( SINF(L) )
              L = MOD( L+I, NDF2 )
            END DO
            VF(I) = REAL( VO + VE )
            DF(I) = REAL( DD + DE )
            NDFI = NDF-I
            IF ( NDFI <= NLP_F ) THEN
              VF(NDFI) = REAL( VE - VO )
              DF(NDFI) = REAL( DD - DE )
            END IF
          END DO
        ELSE IF ( AFIL /= 'N' ) THEN
          ! Reconstruct A
          DO I = 1, NDFH
            AO = 0.D0
            AE = 0.D0
            L = I
            DO J = 1, KSF, 2
              AO = AO + BAW(J) * DBLE( SINF(L) )
              L = MOD( L+I, NDF2 )
              AE = AE + BAW(J+1) * DBLE( SINF(L) )
              L = MOD( L+I, NDF2 )
            END DO
            AF(I) = REAL( AO + AE )
            IF ( NDF-I <= NLP_F ) AF(NDF-I) = REAL( AO - AE )
          END DO
        ELSE IF ( DFIL /= 'N' ) THEN
          ! Reconstruct D
          DO I = 1, NDFH
            DD = 0.D0
            DE = 0.D0
            L = I
            DO J = 1, KSF, 2
              DD = DD + BDW(J) * DBLE( SINF(L) )
              L = MOD( L+I, NDF2 )
              DE = DE + BDW(J+1) * DBLE( SINF(L) )
              L = MOD( L+I, NDF2 )
            END DO
            DF(I) = REAL( DD + DE )
            IF ( NDF-I <= NLP_F ) DF(NDF-I) = REAL( DD - DE )
          END DO
        ELSE IF ( VFIL /= 'N' ) THEN
          ! Reconstruct V
          DO I = 1, NDFH
            VO = 0.D0
            VE = 0.D0
            L = I
            DO J = 1, KSF, 2
              VO = VO + BVW(J) * DBLE( COSF(L) )
              L = MOD( L+I, NDF2 )
              VE = VE + BVW(J+1) * DBLE( COSF(L) )
              L = MOD( L+I, NDF2 )
            END DO
            VF(I) = REAL( VE + VO )
            IF ( NDF-I <= NLP_F ) VF(NDF-I) = REAL( VE - VO )
          END DO
        END IF
      ELSE IF ( RMETH == 'F' ) THEN ! Use fast method w/o time folding
        IF ( ( AFIL /= 'N' ) .AND. ( VFIL /= 'N' ) .AND. ( DFIL /= 'N' ) ) THEN
          ! Reconstruct A, V, and D
          IL = 0
          DO I = 1, NLP_F
            AI = 0.D0
            VI = 0.D0
            DI = 0.D0
            IL = IL + NSPF
            L = IL
            DO J = 1, KSF
              SL = SINF(L)
              AI = AI + BAW(J) * SL
              DI = DI + BDW(J) * SL
              VI = VI + BVW(J) * DBLE( COSF(L) )
              L = MOD( L+IL, NDF2 )
            END DO
            AF(I) = REAL( AI )
            VF(I) = REAL( VI )
            DF(I) = REAL( DI )
          END DO
        ELSE IF ( ( AFIL /= 'N' ) .AND. ( VFIL /= 'N' ) ) THEN
          ! Reconstruct A and V
          IL = 0
          DO I = 1, NLP_F
            AI = 0.D0
            VI = 0.D0
            IL = IL + NSPF
            L = IL
            DO J = 1, KSF
              AI = AI + BAW(J) * DBLE( SINF(L) )
              VI = VI + BVW(J) * DBLE( COSF(L) )
              L = MOD( L+IL, NDF2 )
            END DO
            AF(I) = REAL( AI )
            VF(I) = REAL( VI )
          END DO
        ELSE IF ( ( AFIL /= 'N' ) .AND. ( DFIL /= 'N' ) ) THEN
          ! Reconstruct A and D
          IL = 0
          DO I = 1, NLP_F
            AI = 0.D0
            DI = 0.D0
            IL = IL + NSPF
            L = IL
            DO J = 1, KSF
              SL = SINF(L)
              AI = AI + BAW(J) * SL
              DI = DI + BDW(J) * SL
              L = MOD( L+IL, NDF2 )
            END DO
            AF(I) = REAL( AI )
            DF(I) = REAL( DI )
          END DO
        ELSE IF ( ( VFIL /= 'N' ) .AND. ( DFIL /= 'N' ) ) THEN
          ! Reconstruct V and D
          IL = 0
          DO I = 1, NLP_F
            VI = 0.D0
            DI = 0.D0
            IL = IL + NSPF
            L = IL
            DO J = 1, KSF
              VI = VI + BVW(J) * DBLE( COSF(L) )
              DI = DI + BDW(J) * DBLE( SINF(L) )
              L = MOD( L+IL, NDF2 )
            END DO
            VF(I) = REAL( VI )
            DF(I) = REAL( DI )
          END DO
        ELSE IF ( AFIL /= 'N' ) THEN
          ! Reconstruct A
          IL = 0
          DO I = 1, NLP_F
            AI = 0.D0
            IL = IL + NSPF
            L = IL
            DO J = 1, KSF
              AI = AI + BAW(J) * DBLE( SINF(L) )
              L = MOD( L+IL, NDF2 )
            END DO
            AF(I) = REAL( AI )
          END DO
        ELSE IF ( VFIL /= 'N' ) THEN
          ! Reconstruct V
          IL = 0
          DO I = 1, NLP_F
            VI = 0.D0
            IL = IL + NSPF
            L = IL
            DO J = 1, KSF
              VI = VI + BVW(J) * DBLE( COSF(L) )
              L = MOD( L+IL, NDF2 )
            END DO
            VF(I) = REAL( VI )
          END DO
        ELSE IF ( DFIL /= 'N' ) THEN
          ! Reconstruct D
          IL = 0
          DO I = 1, NLP_F
            DI = 0.D0
            IL = IL + NSPF
            L = IL
            DO J = 1, KSF
              DI = DI + BDW(J) * DBLE( SINF(L) )
              L = MOD( L+IL, NDF2 )
            END DO
            DF(I) = REAL( DI )
          END DO
        END IF
      ELSE ! Use slow method
        IF ( ( AFIL /= 'N' ) .AND. ( VFIL /= 'N' ) .AND. ( DFIL /= 'N' ) ) THEN
          ! Reconstruct A, V, and D
          DO I = 1, NLP_F
            AI = 0.D0
            VI = 0.D0
            DI = 0.D0
            TFAC = PIT * I
            DO J = 1, KSF
              SJ = SIN( TFAC * J )
              AI = AI + BAW(J) * SJ
              DI = DI + BDW(J) * SJ
              VI = VI + BVW(J) * COS( TFAC * J )
            END DO
            AF(I) = REAL( AI )
            VF(I) = REAL( VI )
            DF(I) = REAL( DI )
          END DO
        ELSE IF ( ( AFIL /= 'N' ) .AND. ( VFIL /= 'N' ) ) THEN
          ! Reconstruct A and V
          DO I = 1, NLP_F
            AI = 0.D0
            VI = 0.D0
            TFAC = PIT * I
            DO J = 1, KSF
              AI = AI + BAW(J) * SIN( TFAC * J )
              VI = VI + BVW(J) * COS( TFAC * J )
            END DO
            AF(I) = REAL( AI )
            VF(I) = REAL( VI )
          END DO
        ELSE IF ( ( AFIL /= 'N' ) .AND. ( DFIL /= 'N' ) ) THEN
          ! Reconstruct A and D
          DO I = 1, NLP_F
            AI = 0.D0
            DI = 0.D0
            TFAC = PIT * I
            DO J = 1, KSF
              SJ = SIN( TFAC * J )
              AI = AI + BAW(J) * SJ
              DI = DI + BDW(J) * SJ
            END DO
            AF(I) = REAL( AI )
            DF(I) = REAL( DI )
          END DO
        ELSE IF ( ( VFIL /= 'N' ) .AND. ( DFIL /= 'N' ) ) THEN
          ! Reconstruct V and D
          DO I = 1, NLP_F
            VI = 0.D0
            DI = 0.D0
            TFAC = PIT * I
            DO J = 1, KSF
              VI = VI + BVW(J) * COS( TFAC * J )
              DI = DI + BDW(J) * SIN( TFAC * J )
            END DO
            VF(I) = REAL( VI )
            DF(I) = REAL( DI )
          END DO
        ELSE IF ( AFIL /= 'N' ) THEN
          ! Reconstruct A
          DO I = 1, NLP_F
            AI = 0.D0
            TFAC = PIT * I
            DO J = 1, KSF
              AI = AI + BAW(J) * SIN( TFAC * J )
            END DO
            AF(I) = REAL( AI )
          END DO
        ELSE IF ( VFIL /= 'N' ) THEN
          ! Reconstruct V
          DO I = 1, NLP_F
            VI = 0.D0
            TFAC = PIT * I
            DO J = 1, KSF
              VI = VI + BVW(J) * COS( TFAC * J )
            END DO
            VF(I) = REAL( VI )
          END DO
        ELSE IF ( DFIL /= 'N' ) THEN
          ! Reconstruct D
          DO I = 1, NLP_F
            DI = 0.D0
            TFAC = PIT * I
            DO J = 1, KSF
              DI = DI + BDW(J) * SIN( TFAC * J )
            END DO
            DF(I) = REAL( DI )
          END DO
        END IF
      END IF

      RETURN
      END



      SUBROUTINE BLINER( NLP_F, DTF, NDF, A0, AN, V0, VN, D0, DN, &
       A0A, ASA, V0V, A0V, ASV, D0D, V0D, A0D, ASD, AF, VF, DF, NLP_S )

!***********************************************************************
!* Restores Polynomial Baseline Function to Filtered Signals
!***********************************************************************

      INTEGER NLP_F, NDF, NLP_S, I

      REAL AF(0:NLP_S), VF(0:NLP_S), DF(0:NLP_S)

      DOUBLE PRECISION DTF, A0, AN, V0, VN, D0, DN, &
       A0A, ASA, V0V, A0V, ASV, D0D, V0D, A0D, ASD, T, TS, TC


      ! Set time zero tails
      AF(0) = REAL( A0 )
      VF(0) = REAL( V0 )
      DF(0) = REAL( D0 )

      ! Restore polynomial baseline function
      DO I = 1, NLP_F
        T = I * DTF
        TS = T**2
        TC = T**3
        AF(I) = REAL( AF(I) + A0A + ASA * T )
        VF(I) = REAL( VF(I) + V0V + A0V * T + ASV * TS )
        DF(I) = REAL( DF(I) + D0D + V0D * T + A0D * TS + ASD * TC )
      END DO

      ! Set DFT end tails
      IF ( NLP_F == NDF ) THEN ! Filter end tail = DFT end tail
        AF(NLP_F) = REAL( AN )
        VF(NLP_F) = REAL( VN )
        DF(NLP_F) = REAL( DN )
      END IF

      RETURN
      END



      SUBROUTINE DFT_OUT( FILE_NAME, U, ATYP, VTYP, DTYP, &
       AFL, VFL, DFL, AUN, VUN, DUN, FC, CF, &
       BAVFAC, BADFAC, W0, B1A, B2A, B1V, B2V, B1D, B2D, &
       DFTTYP, STPF, KS, BA, BAW, BVW, BDW, &
       NLP_S, OF_CTRL, AFIL, VFIL, DFIL, DEF_FILE, OUT_FILE, &
       DIR_OUT, NO_INCR, REPORT, OUTPUT_LIST, BATCH, NFREQ )

!***********************************************************************
!* Outputs A-DFT of Residual Input Signal
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'
      INCLUDE 'file.fi'

      RECORD /UDS_Y/  U
      RECORD /UDS_CONTROL/  C
      RECORD /FILE/  REPORT, OUTPUT_LIST

      LOGICAL NO_INCR, BATCH

      INTEGER KS, NLP_S, NFREQ, IOS, IOS_W, IDT, KSF, I

      REAL BA(0:NLP_S), BAW(0:NLP_S), BVW(0:NLP_S), BDW(0:NLP_S)

      DOUBLE PRECISION FC, CF, BAVFAC, BADFAC, W0, &
       B1A, B2A, B1V, B2V, B1D, B2D, STPF

      CHARACTER FILE_NAME*(*), ATYP*(*), VTYP*(*), DTYP*(*), &
       AFL, VFL, DFL, AUN*(*), VUN*(*), DUN*(*), &
       DFTTYP, OF_CTRL, AFIL*(*), VFIL*(*), DFIL*(*), &
       DEF_FILE*(*), OUT_FILE*(*), DIR_OUT*(*), &
       FILCOD


      ! Functions ______________________________________________________

      CHARACTER DNCASE, UPCASE*25, LJUST*25

      EXTERNAL DNCASE, UPCASE, LJUST


      ! Banner for multiple frequencies
      IF ( NFREQ > 1 ) WRITE( *, '(//1X,F12.3,A)', IOSTAT=IOS_W ) &
       STPF, ' (Hz) output files:'

      ! Generate default A-DFT file name
      DEF_FILE = FILE_NAME
      CALL FN_EXTRACT( DEF_FILE )
      CALL ADD_PATH( DIR_OUT, DEF_FILE )
      CALL FILTER_FLAG( 'DFT ', U.FCUT, REAL( STPF ), FILCOD )
      CALL FN_FLAG( DEF_FILE, DNCASE(AFL)//'d'//FILCOD, 3, IDT )
      IF ( .NOT. NO_INCR ) CALL FN_INCR( DEF_FILE )


      ! Get DFT file names
      IF ( ( BATCH ) .OR. ( OF_CTRL == 'G' ) ) THEN ! Default names
        IF ( AFIL /= 'N' ) THEN
          AFIL = DEF_FILE
          IF ( .NOT. NO_INCR ) CALL FN_INCR( AFIL )
        END IF
        IF ( VFIL /= 'N' ) THEN
          VFIL = DEF_FILE
          VFIL(IDT:IDT) = DNCASE( VFL )
          IF ( .NOT. NO_INCR ) CALL FN_INCR( VFIL )
        END IF
        IF ( DFIL /= 'N' ) THEN
          DFIL = DEF_FILE
          DFIL(IDT:IDT) = DNCASE( DFL )
          IF ( .NOT. NO_INCR ) CALL FN_INCR( DFIL )
        END IF
      ELSE ! Prompt for file names
  101   OUT_FILE = DEF_FILE
        OUT_FILE(IDT:IDT) = DNCASE( AFL )
        IF ( .NOT. NO_INCR ) CALL FN_INCR( OUT_FILE )
        CALL FN_NAME_EXTRACT( OUT_FILE )
        WRITE( *, 601 ) ATYP, OUT_FILE(:L_TRIM(OUT_FILE))
        READ( *, '(A)', END=199 ) AFIL
        IF ( AFIL == ' ' ) THEN
          AFIL = OUT_FILE
        ELSE IF ( UPCASE( LJUST( AFIL ) ) == 'N' ) THEN
          AFIL = 'N'
        ELSE
          CALL ADD_PATH( DIR_OUT, AFIL )
          IF ( .NOT. FN_VALID( AFIL ) ) THEN
            WRITE( *, * ) '*** Invalid file name'
            GO TO 101
          END IF
        END IF

  102   OUT_FILE = DEF_FILE
        OUT_FILE(IDT:IDT) = DNCASE( VFL )
        IF ( .NOT. NO_INCR ) CALL FN_INCR( OUT_FILE )
        CALL FN_NAME_EXTRACT( OUT_FILE )
        WRITE( *, 601 ) VTYP, OUT_FILE(:L_TRIM(OUT_FILE))
        READ( *, '(A)', END=101 ) VFIL
        IF ( VFIL == ' ' ) THEN
          VFIL = OUT_FILE
        ELSE IF ( UPCASE( LJUST( VFIL ) ) == 'N' ) THEN
          VFIL = 'N'
        ELSE
          CALL ADD_PATH( DIR_OUT, VFIL )
          IF ( .NOT. FN_VALID( VFIL ) ) THEN
            WRITE( *, * ) '*** Invalid file name'
            GO TO 102
          END IF
        END IF

  103   OUT_FILE = DEF_FILE
        OUT_FILE(IDT:IDT) = DNCASE( DFL )
        IF ( .NOT. NO_INCR ) CALL FN_INCR( OUT_FILE )
        CALL FN_NAME_EXTRACT( OUT_FILE )
        WRITE( *, 601 ) DTYP, OUT_FILE(:L_TRIM(OUT_FILE))
        READ( *, '(A)', END=101 ) DFIL
        IF ( DFIL == ' ' ) THEN
          DFIL = OUT_FILE
        ELSE IF ( UPCASE( LJUST( DFIL ) ) == 'N' ) THEN
          DFIL = 'N'
        ELSE
          CALL ADD_PATH( DIR_OUT, DFIL )
          IF ( .NOT. FN_VALID( DFIL ) ) THEN
            WRITE( *, * ) '*** Invalid file name'
            GO TO 103
          END IF
        END IF

        IF ( ( AFIL == 'N' ) .AND. &
         ( VFIL == 'N' ) .AND. &
         ( DFIL == 'N' ) ) THEN
          WRITE( *, '(/A)' ) ' *** No outputs specified'
          GO TO 101
        END IF
      END IF

      ! Compute needed A, V, and D DFT's
      CALL DFTSCL( BAVFAC, BADFAC, B1A, B2A, B1V, B2V, B1D, B2D, &
       KS, KSF, BA, VFIL, DFIL, DFTTYP, BAW, BVW, BDW, NLP_S )

      ! Set output fields
      U.XTYP = 'FREQUENCY'
      U.XUNITS = 'HZ'
      IF ( DFTTYP == 'M' ) THEN
        U.CURTYP = 'DFT MAG.'
      ELSE
        U.CURTYP = 'DFT'
      END IF
      U.NFP = 0
      U.NLP = KSF
      U.DEL = REAL( W0 )
      IF ( ( ( FC > 0.D0 ) .AND. ( STPF > FC*(.5+1.D-15) ) ) &
       .OR. ( ( CF > 0.D0 ) .AND. ( STPF > CF*(1.+1.-15) ) ) &
       .OR. ( ( U.PREF > 0. ) .AND. &
       ( STPF > U.PREF*(.5+1.D-15) ) ) ) THEN
        WRITE( *, '(/A)' ) ' *** WARNING - DFT overlap distortion present'
        U.STATUS = 'DISTORTED'
      ELSE
        U.STATUS = 'COMPACTED'
      END IF
      U.CD1 = 'SimFil DFT'

      ! Output DFT data files
      CALL UDS_CONTROL_INIT( C )
      IF ( AFIL /= 'N' ) THEN
        U.YTYP = ATYP
        U.YUNITS = AUN
        DO I = U.NFP, U.NLP
          U.Y(I) = BAW(I)
        END DO
        CALL UDS_PUT( AFIL, U, C, REPORT, OUTPUT_LIST, DIR_OUT, .TRUE., IOS )
      END IF
      IF ( VFIL /= 'N' ) THEN
        U.YTYP = VTYP
        U.YUNITS = VUN
        DO I = U.NFP, U.NLP
          U.Y(I) = BVW(I)
        END DO
        CALL UDS_PUT( VFIL, U, C, REPORT, OUTPUT_LIST, DIR_OUT, .TRUE., IOS )
      END IF
      IF ( DFIL /= 'N' ) THEN
        U.YTYP = DTYP
        U.YUNITS = DUN
        DO I = U.NFP, U.NLP
          U.Y(I) = BDW(I)
        END DO
        CALL UDS_PUT( DFIL, U, C, REPORT, OUTPUT_LIST, DIR_OUT, .TRUE., IOS )
      END IF

  199 RETURN

  601 FORMAT(/1X,A20,' DFT file name?   ( N - None )'/' [',A,']'/' >> ',$)

      END



      SUBROUTINE DFTSCL( BAVFAC, BADFAC, B1A, B2A, B1V, B2V, B1D, B2D, &
       KS, KSF, BA, VFIL, DFIL, DFTTYP, BAW, BVW, BDW, NLP_S )

!***********************************************************************
!* Computes (Unwindowed, Unconstrained) DFT's from A-DFT
!***********************************************************************

      INTEGER KS, KSF, NLP_S, J

      REAL BA(0:NLP_S), BAW(0:NLP_S), BVW(0:NLP_S), BDW(0:NLP_S)

      DOUBLE PRECISION BAVFAC, BADFAC, B1A, B2A, B1V, B2V, B1D, B2D, BAWJ

      CHARACTER VFIL*(*), DFIL*(*), DFTTYP


      ! Compute needed A, V, and D DFT's
      DO J = 1, KS
        BAWJ = BA(J)
        BAW(J) = REAL( BAWJ )
        IF ( VFIL /= 'N' ) BVW(J) = REAL( BAWJ * BAVFAC / J )
        IF ( DFIL /= 'N' ) BDW(J) = REAL( BAWJ * BADFAC / ( J**2 ) )
      END DO

      ! Add trigonometric baseline contribution to DFT's
      KSF = MAX( KS, 2 )
      BAW(1) = REAL( BAW(1) + B1A )
      BAW(2) = REAL( BAW(2) + B2A )
      BVW(1) = REAL( BVW(1) + B1V )
      BVW(2) = REAL( BVW(2) + B2V )
      BDW(1) = REAL( BDW(1) + B1D )
      BDW(2) = REAL( BDW(2) + B2D )

      ! Convert into magnitude DFT's if requested
      IF ( DFTTYP == 'M' ) THEN
        DO J = 1, KSF
          BAW(J) = ABS( BAW(J) )
          BVW(J) = ABS( BVW(J) )
          BDW(J) = ABS( BDW(J) )
        END DO
      END IF

      RETURN
      END



      SUBROUTINE RAW_OUT( FILE_NAME, U, ATYP, VTYP, DTYP, &
       AFL, VFL, DFL, AUN, VUN, DUN, &
       NLP_I, DEL_I, DTT, STT, FTT, &
       STF, STF_C, FTF, FTF_C, DTF, DTF_C, &
       A, V, D, AF, VF, DF, NFP_S, NLP_S, &
       AFIL, VFIL, DFIL, DEF_FILE, OUT_FILE, &
       DIR_OUT, NO_INCR, REPORT, OUTPUT_LIST, BATCH )

!***********************************************************************
!* Outputs Subsampled/Raw Signals
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'
      INCLUDE 'file.fi'

      RECORD /UDS_Y/  U
      RECORD /UDS_CONTROL/  C
      RECORD /FILE/  REPORT, OUTPUT_LIST

      LOGICAL NO_INCR, BATCH

      INTEGER NLP_I, NFP_S, NLP_S, &
       IOS, IOS_W, IDT, ISUB, NSUB, NFP_O, NLP_O, I, II, IP

      REAL DEL_I, A(NFP_S:NLP_S), V(NFP_S:NLP_S), D(NFP_S:NLP_S), &
       AF(NFP_S:NLP_S), VF(NFP_S:NLP_S), DF(NFP_S:NLP_S)

      DOUBLE PRECISION DTT, STT, FTT, STF, FTF, DTF, TI, TII, FRAC

      CHARACTER FILE_NAME*(*), ATYP*(*), VTYP*(*), DTYP*(*), &
       AFL, VFL, DFL, AUN*(*), VUN*(*), DUN*(*), &
       STF_C*(*), FTF_C*(*), DTF_C*(*), &
       AFIL*(*), VFIL*(*), DFIL*(*), DEF_FILE*(*), OUT_FILE*(*), &
       DIR_OUT*(*), FILCOD


      ! Functions ______________________________________________________

      CHARACTER DNCASE, UPCASE*25, LJUST*25

      EXTERNAL DNCASE, UPCASE, LJUST


      ! Get initial time value
      IF ( STF_C == ' ' ) THEN
        STF = STT
      ELSE
        IF ( STF < STT ) THEN
          WRITE( *, '(/2A,F15.6,A)', IOSTAT=IOS_W ) &
           ' *** WARNING - Initial time set to ', &
           'signal initial time of ', STT, ' (sec)'
          STF = STT
        END IF
      END IF

      ! Get final time value
      IF ( FTF_C == ' ' ) THEN
        FTF = FTT
      ELSE
        IF ( FTF > FTT ) THEN
          WRITE( *, '(/2A,F15.6,A)', IOSTAT=IOS_W ) &
           ' *** WARNING - Final time set to ', &
           'signal final time of ', FTT, ' (sec)'
          FTF = FTT
        END IF
      END IF

      ! Get subsampled signal time step
      ISUB = 0
      IF ( ( DTF_C == ' ' ) .OR. ( DTF_C == 'I' ) ) THEN
        ! Input time step
        ISUB = 1
        DTF = DTT
      ELSE IF ( DTF_C(1:1) == 'I' ) THEN ! Subsampling requested
        READ( DTF_C(2:), '(BN,I24)', IOSTAT=IOS ) ISUB
        DTF = DTT * ISUB
      ELSE ! Check for subsampling
        NSUB = NINT( DTF / DTT )
        IF ( EQUAL_DP( DTF, NSUB*DTT, 1.D-15 ) ) ISUB = NSUB
      END IF

      ! Set start index for output signals
      NFP_O = NINT( STF / DTF )
      DO WHILE ( NFP_O * DTF < STT )
        NFP_O = NFP_O + 1
      END DO
      IF ( NFP_O < NFP_S ) THEN
        WRITE( *, '(/A,I6)', IOSTAT=IOS_W ) &
         ' *** WARNING - Initial index set to min value of: ', &
         NFP_S
        NFP_O = NFP_S
      END IF
      STF = DTF * NFP_O

      ! Set end index for output signals
      NLP_O = NINT( FTF / DTF )
      DO WHILE ( NLP_O * DTF > FTT )
        NLP_O = NLP_O - 1
      END DO
      IF ( NLP_O > NLP_S ) THEN
        WRITE( *, '(/A,I6)', IOSTAT=IOS_W ) &
         ' *** WARNING - Final index set to max value of: ', &
         NLP_S
        NLP_O = NLP_S
      END IF
      FTF = DTF * NLP_O

      ! Generate default signal file names
      DEF_FILE = FILE_NAME
      CALL FN_EXTRACT( DEF_FILE )
      CALL ADD_PATH( DIR_OUT, DEF_FILE )
      CALL FILTER_FLAG( 'TIME SERIES', U.FCUT, U.FSTP, FILCOD )
      IF ( FILCOD /= '0' ) THEN
        CALL FN_FLAG( DEF_FILE, DNCASE(AFL)//'f'//FILCOD, 3, IDT )
      ELSE
        CALL FN_FLAG( DEF_FILE, DNCASE(AFL)//'cs', 3, IDT )
      END IF
      IF ( .NOT. NO_INCR ) CALL FN_INCR( DEF_FILE )


      ! Get output signal file names
      IF ( BATCH ) THEN ! Assign default file names
        IF ( AFIL /= 'N' ) THEN
          AFIL = DEF_FILE
          IF ( .NOT. NO_INCR ) CALL FN_INCR( AFIL )
        END IF
        IF ( VFIL /= 'N' ) THEN
          VFIL = DEF_FILE
          VFIL(IDT:IDT) = DNCASE( VFL )
          IF ( .NOT. NO_INCR ) CALL FN_INCR( VFIL )
        END IF
        IF ( DFIL /= 'N' ) THEN
          DFIL = DEF_FILE
          DFIL(IDT:IDT) = DNCASE( DFL )
          IF ( .NOT. NO_INCR ) CALL FN_INCR( DFIL )
        END IF
      ELSE ! Prompt for file names
  101   OUT_FILE = DEF_FILE
        OUT_FILE(IDT:IDT) = DNCASE( AFL )
        IF ( .NOT. NO_INCR ) CALL FN_INCR( OUT_FILE )
        CALL FN_NAME_EXTRACT( OUT_FILE )
        WRITE( *, 601 ) ATYP, OUT_FILE(:L_TRIM(OUT_FILE))
        READ( *, '(A)', END=199 ) AFIL
        IF ( AFIL == ' ' ) THEN
          AFIL = OUT_FILE
        ELSE IF ( UPCASE( LJUST( AFIL ) ) == 'N' ) THEN
          AFIL = 'N'
        ELSE
          CALL ADD_PATH( DIR_OUT, AFIL )
          IF ( .NOT. FN_VALID( AFIL ) ) THEN
            WRITE( *, * ) '*** Invalid file name'
            GO TO 101
          END IF
        END IF

  102   OUT_FILE = DEF_FILE
        OUT_FILE(IDT:IDT) = DNCASE( VFL )
        IF ( .NOT. NO_INCR ) CALL FN_INCR( OUT_FILE )
        CALL FN_NAME_EXTRACT( OUT_FILE )
        WRITE( *, 601 ) VTYP, OUT_FILE(:L_TRIM(OUT_FILE))
        READ( *, '(A)', END=101 ) VFIL
        IF ( VFIL == ' ' ) THEN
          VFIL = OUT_FILE
        ELSE IF ( UPCASE( LJUST( VFIL ) ) == 'N' ) THEN
          VFIL = 'N'
        ELSE
          CALL ADD_PATH( DIR_OUT, VFIL )
          IF ( .NOT. FN_VALID( VFIL ) ) THEN
            WRITE( *, * ) '*** Invalid file name'
            GO TO 102
          END IF
        END IF

  103   OUT_FILE = DEF_FILE
        OUT_FILE(IDT:IDT) = DNCASE( DFL )
        IF ( .NOT. NO_INCR ) CALL FN_INCR( OUT_FILE )
        CALL FN_NAME_EXTRACT( OUT_FILE )
        WRITE( *, 601 ) DTYP, OUT_FILE(:L_TRIM(OUT_FILE))
        READ( *, '(A)', END=101 ) DFIL
        IF ( DFIL == ' ' ) THEN
          DFIL = OUT_FILE
        ELSE IF ( UPCASE( LJUST( DFIL ) ) == 'N' ) THEN
          DFIL = 'N'
        ELSE
          CALL ADD_PATH( DIR_OUT, DFIL )
          IF ( .NOT. FN_VALID( DFIL ) ) THEN
            WRITE( *, * ) '*** Invalid file name'
            GO TO 103
          END IF
        END IF

        IF ( ( AFIL == 'N' ) .AND. &
         ( VFIL == 'N' ) .AND. &
         ( DFIL == 'N' ) ) THEN
          WRITE( *, '(/A)' ) ' *** No outputs specified'
          GO TO 101
        END IF
      END IF

      ! Subsample or interpolate to output time step
      IF ( ISUB > 0 ) THEN ! Subsample

        DO I = NFP_O, NLP_O
          AF(I) = A(I*ISUB)
          VF(I) = V(I*ISUB)
          DF(I) = D(I*ISUB)
        END DO

      ELSE ! Interpolate

        DO I = NFP_O, NLP_O
          TI = I * DTF
          II = NINT( TI / DTT )
          IF ( ( II * DTT > TI ) .OR. ( II >= NLP_I ) ) II = II - 1
          IP = II + 1
          TII = II * DTT
          FRAC = ( TI - TII ) / DTT
          AF(I) = REAL( A(II) + FRAC * ( A(IP) - A(II) ) )
          VF(I) = REAL( V(II) + FRAC * ( V(IP) - V(II) ) )
          DF(I) = REAL( D(II) + FRAC * ( D(IP) - D(II) ) )
        END DO

      END IF

      ! Set output fields
      U.XTYP = 'TIME'
      U.XUNITS = 'SECONDS'
      U.CURTYP = 'TIME SERIES'
      U.STATUS = 'COMPUTED'
      U.NFP = NFP_O
      U.NLP = NLP_O
      IF ( ISUB == 1 ) THEN ! Use input (single precision) DEL
        U.DEL = DEL_I
      ELSE
        U.DEL = REAL( DTF )
      END IF
      U.CD1 = 'SimFil Computed'

      ! Output data files
      CALL UDS_CONTROL_INIT( C )
      IF ( AFIL /= 'N' ) THEN
        U.YTYP = ATYP
        U.YUNITS = AUN
        DO I = U.NFP, U.NLP
          U.Y(I) = AF(I)
        END DO
        CALL UDS_PUT( AFIL, U, C, REPORT, OUTPUT_LIST, DIR_OUT, .TRUE., IOS )
      END IF
      IF ( VFIL /= 'N' ) THEN
        U.YTYP = VTYP
        U.YUNITS = VUN
        DO I = U.NFP, U.NLP
          U.Y(I) = VF(I)
        END DO
        CALL UDS_PUT( VFIL, U, C, REPORT, OUTPUT_LIST, DIR_OUT, .TRUE., IOS )
      END IF
      IF ( DFIL /= 'N' ) THEN
        U.YTYP = DTYP
        U.YUNITS = DUN
        DO I = U.NFP, U.NLP
          U.Y(I) = DF(I)
        END DO
        CALL UDS_PUT( DFIL, U, C, REPORT, OUTPUT_LIST, DIR_OUT, .TRUE., IOS )
      END IF

  199 RETURN

  601 FORMAT(/1X,A20,' file name?   ( N - None )'/' [',A,']'/' >> ',$)

      END
