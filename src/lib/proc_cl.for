      SUBROUTINE PROC_CL( CL_ARG, NUM_ARGS, PROG_NAME, TEMPLATE_STR,
     & ALL_BATCH, NO_BATCH, NO_INCR, REPORT, OUTPUT_LIST, DIR_OUT,
     & ONE_PASS, REP_OPEN, OUT_OPEN )

!***********************************************************************
!* Processes the Standard Command Line Arguments
!* . Call after stripping out program-specific arguments
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

      CHARACTER*(*) CL_ARG(0:*) ! Command line argument array

      INTEGER NUM_ARGS ! Number of command line arguments

      CHARACTER*(*) PROG_NAME ! Program name

      CHARACTER*(*) TEMPLATE_STR ! Template string on command line

      LOGICAL ALL_BATCH ! Indicates always use batch mode

      LOGICAL NO_BATCH ! Indicates never use batch mode

      LOGICAL NO_INCR ! Indicates not to increment output file names

      RECORD /FILE/  REPORT ! Report file object

      RECORD /FILE/  OUTPUT_LIST ! Output list file object

      CHARACTER*(*) DIR_OUT ! Output directory

      LOGICAL ONE_PASS ! Indicates one-pass mode

      LOGICAL REP_OPEN ! Specifies to open a requested report file

      LOGICAL OUT_OPEN ! Specifies to open a requested output list file


      ! Variables ______________________________________________________

      INTEGER I, NS, I_OTHER

      CHARACTER ARG*255, OTHER*255, C


      ! Process command line arguments
      TEMPLATE_STR = ' '
      DIR_OUT = ' '
      ALL_BATCH = .FALSE.
      NO_BATCH = .FALSE.
      NO_INCR = .FALSE.
      CALL FILE_INIT( REPORT )
      CALL FILE_INIT( OUTPUT_LIST )
      I_OTHER = 0 ! Suppress may be used uninitialized warning
      OTHER = ' '
      DO I = 1, NUM_ARGS
        CALL STR_UPCASE( ARG, CL_ARG(I) )
        C = ARG(1:1)
        IF ( ( C .EQ. '-' ) .OR. ( C .EQ. '/' ) ) THEN
          NS = 2
        ELSE
          NS = 1
        END IF
        IF ( ( ARG(NS:) .EQ. '?' ) .OR.
     &     ( ARG(NS:) .EQ. '??' ) .OR.
     &     ( ARG(NS:) .EQ. 'HELP' ) ) THEN ! Show command line syntax
          CALL SYNTAX_CL( PROG_NAME )
          IF ( ( ARG(NS:) .EQ. '??' ) .OR.
     &       ( ARG(NS:) .EQ. 'HELP' ) ) THEN ! Show command line help
            CALL HELP_CL( PROG_NAME )
          END IF
          STOP ' '
        ELSE IF ( ( ARG(NS:NS+1) .EQ. 'T=' ) .OR.
     &     ( ARG(NS:NS+1) .EQ. 'T#' ) ) THEN
          TEMPLATE_STR = CL_ARG(I)(NS+2:)
          CL_ARG(I) = ' '
        ELSE IF ( ARG(NS:) .EQ. 'BATCH' ) THEN
          ALL_BATCH = .TRUE.
          NO_BATCH = .FALSE.
          CL_ARG(I) = ' '
        ELSE IF ( ( ARG(NS:) .EQ. 'NO_BATCH' ) .OR.
     &     ( ARG(NS:) .EQ. 'NOBATCH' ) ) THEN
          ALL_BATCH = .FALSE.
          NO_BATCH = .TRUE.
          CL_ARG(I) = ' '
        ELSE IF ( ( ARG(NS:) .EQ. 'NO_INCR' ) .OR.
     &     ( ARG(NS:) .EQ. 'NOINCR' ) ) THEN
          NO_INCR = .TRUE.
          CL_ARG(I) = ' '
        ELSE IF ( ARG(NS:) .EQ. 'REP' ) THEN
          REPORT.EXISTS = .TRUE.
          CL_ARG(I) = ' '
        ELSE IF ( ARG(NS:) .EQ. 'OUT' ) THEN
          OUTPUT_LIST.EXISTS = .TRUE.
          OUTPUT_LIST.NAME = ' '
          CL_ARG(I) = ' '
        ELSE IF ( ( ARG(NS:NS+3) .EQ. 'OUT=' ) .OR.
     &     ( ARG(NS:NS+3) .EQ. 'OUT#' ) ) THEN
          OUTPUT_LIST.EXISTS = .TRUE.
          OUTPUT_LIST.NAME = CL_ARG(I)(NS+4:)
          CL_ARG(I) = ' '
        ELSE IF ( ( ARG(NS:NS+3) .EQ. 'DIR=' ) .OR.
     &     ( ARG(NS:NS+3) .EQ. 'DIR#' ) ) THEN
          DIR_OUT = CL_ARG(I)(NS+4:)
          CL_ARG(I) = ' '
        ELSE IF ( ( NS .EQ. 1 ) .AND. ( OTHER .EQ. ' ' ) ) THEN
          OTHER = CL_ARG(I)(NS:)
          I_OTHER = I
        END IF
      END DO

      ! Use unflagged argument for file name/template/hitlist
      IF ( ( TEMPLATE_STR .EQ. ' ' ) .AND. ( OTHER .NE. ' ' ) ) THEN
        TEMPLATE_STR = OTHER
        CL_ARG(I_OTHER) = ' '
      END IF

      ! Set up the output directory
      CALL SET_DIR( DIR_OUT )

      ! Set one-pass mode if template and BATCH entered
      ONE_PASS = ( ( ALL_BATCH ) .AND. ( TEMPLATE_STR .NE. ' ' ) )

      ! Open report and output list files
      IF ( ( REPORT.EXISTS ) .AND. ( REP_OPEN ) ) THEN
        CALL OPEN_REP( REPORT, PROG_NAME, DIR_OUT, NO_INCR )
      END IF
      IF ( ( OUTPUT_LIST.EXISTS ) .AND. ( OUT_OPEN ) ) THEN
        CALL OPEN_OUT( OUTPUT_LIST, PROG_NAME, DIR_OUT )
      END IF

      RETURN
      END



      SUBROUTINE SYNTAX_CL( PROG_NAME )

!***********************************************************************
!* Displays the Standard Program Command Line Syntax
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/10/18
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) PROG_NAME ! Program name


      WRITE( *, '(A//1X,A8,1X,A/10X,A)' ) ' Syntax:',
     & PROG_NAME(:L_TRIM(PROG_NAME)),
     & '[[T=]<template|hitlist>] [[NO_]BATCH] [NO_INCR] [REP]',
     & '[OUT[=<outlist>]] [DIR=<dir_out>] [?[?]]'

      RETURN
      END



      SUBROUTINE HELP_CL( PROG_NAME )

!***********************************************************************
!* Displays the Standard Program Command Line Help
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/10/18
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) PROG_NAME ! Program name


      ! Variables ______________________________________________________

      INTEGER IOS

      CHARACTER P_NAME*11


      P_NAME = PROG_NAME
      IF ( BLANK( P_NAME ) ) P_NAME = '<prog_name>'
      WRITE( *, '(/A,7(/10X,A))', IOSTAT=IOS ) ' Usage:',
     & '<template|hitlist> :  '//
     & 'Input file name template or hitlist file name',
     & 'BATCH/NO_BATCH :  Always/never apply batch processing',
     & 'NO_INCR :  Don''t increment names of existing output files',
     & 'REP :  Generate a report file in '//
     & P_NAME(:LEN_TRIM(P_NAME))//'.rep',
     & 'OUT :  Generate an output list file in <outlist> ['//
     & P_NAME(:LEN_TRIM(P_NAME))//'.out]',
     & 'DIR=<dir_out> :  Output to directory <dir_out>',
     & '?/?? :  Display command line syntax/help'

      RETURN
      END
