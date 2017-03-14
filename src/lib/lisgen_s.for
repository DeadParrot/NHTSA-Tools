      SUBROUTINE LISGEN_S( LIST, DIR_SEARCH, PATH_EXP, TEMP_STR )

!***********************************************************************
!* Adds to String or File Name List Based on a Given Template
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/10/18
!***********************************************************************

      ! Headers
      INCLUDE 'file.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      RECORD /FILE/  LIST ! List file object

      LOGICAL DIR_SEARCH ! Specifies whether to search for list as files

      LOGICAL PATH_EXP ! Specifies whether to return full-path file name

      CHARACTER*(*) TEMP_STR ! Template string


      ! Variables ______________________________________________________

      LOGICAL MATCHED, DONE, SET, FIRST_PASS

      INTEGER IGL(11), IGR(11), LG(11),
     & NIR(10), IIRA(10), IRL(10,100), IRR(10,100), LIR(10,100),
     & NSIR(10,100), ISIR(10,100), IRO(10),
     & LTEMP, NS, NG, NR, NIR_NR, IND, ILP, IFNB,
     & NLPAR, NRPAR, NPAR_MAX, NPAR(10),
     & IR, IROA, IROI, IIR, IA, IRA, IRAI, IRLS, IRRS, MAX_IRO, LIRA,
     & I, IG, IH, IO, IP, IQ, IS, ISV, ISL, ISU,
     & LLS, LUS, LSV, NLS, NTS, IHM, IOS, IOSL, IOSU

      CHARACTER FMTD*7, TEMP_O*255, STR_TYP(21), SLS*255, SUS*255,
     & SEQ_VAL(10)*255, NSTR*25, C, CL, CU, APOS

      PARAMETER ( APOS = '''' )


      ! Functions ______________________________________________________

      CHARACTER LJUST*255

      EXTERNAL LJUST


      ! Check for blank string
      IF ( TEMP_STR .EQ. ' ' ) RETURN

      ! Check that file is open and formatted
      INQUIRE( LIST.UNIT, FORMATTED=FMTD, OPENED=LIST.OPEN,
     & IOSTAT=IOS )
      IF ( ( .NOT. LIST.OPEN ) .OR. ( IOS .NE. 0 ) ) THEN
        WRITE( *, * ) '*** ERROR - List file is not open'
        RETURN
      ELSE IF ( FMTD .EQ. 'NO' ) THEN
        WRITE( *, * ) '*** ERROR - List file is not FORMATTED'
        RETURN
      END IF

      ! Initializations
      MATCHED = .FALSE.
      LTEMP = LEN_TRIM( TEMP_STR )
      NS = 0
      NG = 0
      NR = 0
      NPAR_MAX = 0
      IND = 1

      ! Partition string into global and repeating string elements
      DO WHILE ( IND .LE. LTEMP )

        ! Increment the number of strings counter
        NS = NS + 1

        ! Process this string
        IF ( TEMP_STR(IND:IND) .NE. '(' ) THEN ! Global string

          ! Assign global string values
          NG = NG + 1
          STR_TYP(NS) = 'G'
          ILP = INDEX( TEMP_STR(IND:), '(' )
          IF ( ILP .EQ. 0 ) THEN ! Last group
            IGL(NG) = IND
            IGR(NG) = LTEMP
            LG(NG) = LTEMP - IND + 1
            IND = LTEMP + 1
          ELSE ! Process next repeat group
            IGL(NG) = IND
            IGR(NG) = IND + ILP - 2
            LG(NG) = ILP - 1
            IND = IND + ILP - 1
          END IF

        ELSE ! Repeat string

          ! Check number of repeat strings
          IF ( NR .GE. 10 ) THEN
            WRITE( *, * )
     &         '*** ERROR - Limit of 10 repeat strings exceeded'
            RETURN
          END IF

          ! Assign repeat string index and type flag
          NR = NR + 1
          STR_TYP(NS) = 'R'

          ! Count left parens
          NLPAR = 1
          IND = IND + 1
          DO WHILE ( TEMP_STR(IND:IND) .EQ. '(' )
            NLPAR = NLPAR + 1
            IND = IND + 1
          END DO

          ! Get the range and length of each repeat element
          NIR_NR = 0
          DONE = .FALSE.
          DO WHILE ( .NOT. DONE )
            IF ( IND .GT. LTEMP ) THEN
              WRITE( *, * )
     &           '*** ERROR - Unclosed last repeat group'
              RETURN
            END IF
            NIR_NR = NIR_NR + 1
            IF ( NIR_NR .GT. 100 ) THEN
              WRITE( *, * ) '*** ERROR - More than 100 ',
     &           'elements in repeat group # ', NR
              RETURN
            END IF
            IFNB = IND
            DO WHILE ( ( TEMP_STR(IFNB:IFNB) .EQ. ' ' ) .AND.
     &         ( IFNB .LT. LTEMP ) )
              IFNB = IFNB + 1
            END DO
            IF ( TEMP_STR(IFNB:IFNB) .EQ. '"' ) THEN ! Quoted element
              IND = IFNB + 1
              IQ = INDEX( TEMP_STR(IND:), '"' )
              IF ( IQ .EQ. 0 ) THEN
                WRITE( *, * )
     &             '*** ERROR - Unmatched " in repeat group # ', NR
                RETURN
              END IF
              IRL(NR,NIR_NR) = IND
              IRR(NR,NIR_NR) = IND + IQ - 2
              IND = IND + IQ
              DO WHILE ( ( TEMP_STR(IND:IND) .EQ. ' ' ) .AND.
     &           ( IND .LT. LTEMP ) )
                IND = IND + 1
              END DO
              IF ( TEMP_STR(IND:IND) .EQ. ',' ) THEN
                IND = IND + 1
              ELSE IF ( TEMP_STR(IND:IND) .EQ. APOS ) THEN
                IND = IND + 1
              ELSE IF ( TEMP_STR(IND:IND) .EQ. ')' ) THEN
                IND = IND + 1
                DONE = .TRUE.
              ELSE
                WRITE( *, * )
     &             '*** ERROR - Invalid repeat group # ', NR
                RETURN
              END IF
            ELSE ! Non-quoted element
              IRL(NR,NIR_NR) = IND
              C = TEMP_STR(IND:IND)
              DO WHILE ( .NOT. ANY_CHARS( C, ',)'//APOS ) )
                IND = IND + 1
                IF ( IND .GT. LTEMP ) THEN
                  WRITE( *, * )
     &               '*** ERROR - Unclosed last repeat group'
                  RETURN
                END IF
                C = TEMP_STR(IND:IND)
              END DO
              IRR(NR,NIR_NR) = IND - 1
              IF ( C .EQ. ')' ) THEN ! Last element
                DONE = .TRUE.
              ELSE
                IND = IND + 1
              END IF
            END IF
            LIR(NR,NIR_NR) = IRR(NR,NIR_NR) - IRL(NR,NIR_NR) + 1
          END DO
          NIR(NR) = NIR_NR

          ! Count right parens
          NRPAR = 1
          IND = IND + 1
          DO WHILE ( TEMP_STR(IND:IND) .EQ. ')' )
            NRPAR = NRPAR + 1
            IND = IND + 1
          END DO

          ! Check matching number of parens / Assign number of parens
          IF ( NLPAR .NE. NRPAR ) THEN
            WRITE( *, * ) '*** ERROR - Mismatched number of ',
     &         'parens in repeat group # ', NR
            RETURN
          END IF
          NPAR(NR) = NLPAR
          NPAR_MAX = MAX( NPAR_MAX, NLPAR )

          ! Check for empty repeat string
          IF ( ( NIR_NR .EQ. 1 ) .AND. ( LIR(NR,1) .EQ. 0 ) ) THEN
            ! Empty repeat string - Eliminate it
            NR = NR - 1
            NIR_NR = 0 ! To skip sequence element check
          END IF

          ! Check sequence elements
          DO IIR = 1, NIR_NR
            IRLS = IRL(NR,IIR)
            IRRS = IRR(NR,IIR)
            NSIR(NR,IIR) = 0
            IH = INDEX( TEMP_STR(IRLS:IRRS), '-' )
            IF ( ( IRLS .LE. IRRS ) .AND. ( IH .NE. 0 ) .AND.
     &         ( TEMP_STR(IRLS-1:IRLS-1) .NE. '"' ) ) THEN
              ! Hyphen - Check for sequence
              SLS = LJUST( TEMP_STR(IRLS:IRLS+IH-2) )
              SUS = LJUST( TEMP_STR(IRRS-LIR(NR,IIR)+IH+1:IRRS) )
              LLS = LEN_TRIM( SLS )
              LUS = LEN_TRIM( SUS )
              IF ( MIN( LLS, LUS ) .LT. 1 ) THEN
                ! Non-sequence use of hyphen
                WRITE( *, * ) '*** WARNING - Non-sequence ',
     &             'use of hyphen in repeat group # ', NR
              ELSE IF ( ( LLS .EQ. 1 ) .AND. ( LUS .EQ. 1 ) ) THEN
                ! Single char sequence limits
                CL = SLS(1:1)
                CU = SUS(1:1)
                IF ( ( CL .GT. CU ) .OR.
     &             ( ( ( CL .LT. '0' ) .OR. ( CU .GT. '9' ) ) .AND.
     &             ( ( CL .LT. 'A' ) .OR. ( CU .GT. 'Z' ) ) .AND.
     &             ( ( CL .LT. 'a' ) .OR. ( CU .GT. 'z' ) ) ) )
     &             THEN ! Non-sequence use of hyphen
                  WRITE( *, * ) '*** WARNING - Non-sequence ',
     &               'use of hyphen in repeat group # ', NR
                ELSE ! Good sequence
                  LIR(NR,IIR) = -1 ! Flag as length=1 sequence
                  NSIR(NR,IIR) = ICHAR( CU ) - ICHAR( CL ) + 1
                END IF
              ELSE ! Multiple char numerical sequence
                NSTR = SLS(:LLS)
                READ( NSTR, '(BN,I25)', IOSTAT=IOSL ) ISL
                NSTR = SUS(:LUS)
                READ( NSTR, '(BN,I25)', IOSTAT=IOSU ) ISU
                IF ( ( IOSL .NE. 0 ) .OR. ( IOSU .NE. 0 ) ) THEN
                  ! Non-sequence use of hyphen
                  WRITE( *, * ) '*** WARNING - Non-sequence ',
     &               'use of hyphen in repeat group # ', NR
                ELSE IF ( ISL .GT. ISU ) THEN
                  WRITE( *, * ) '*** ERROR - Bad integer ',
     &               'sequence in repeat group # ', NR
                  RETURN
                ELSE ! Good sequence
                  LIR(NR,IIR) = -2 ! Flag as multiple char num sequence
                  NSIR(NR,IIR) = ISU - ISL + 1
                END IF
              END IF
            END IF
          END DO

        END IF

      END DO

      ! Set up repeat string expansion order array
      IROI = 0
      DO IP = NPAR_MAX, 1, -1
        DO IR = NR, 1, -1
          IF ( NPAR(IR) .EQ. IP ) THEN
            IROI = IROI + 1
            IRO(IROI) = IR
          END IF
        END DO
      END DO

      ! Repeat element selection arrays are initialized
      DO IR = 1, NR
        IIRA(IR) = 1
        DO IIR = 1, NIR(IR)
          ISIR(IR,IIR) = 0
        END DO
      END DO

      ! Generate strings
      DONE = .FALSE.
      FIRST_PASS = .TRUE.
      DO WHILE ( .NOT. DONE )

        ! Apply speedometer logic to set the active repeat string
        IF ( NR .GT. 0 ) THEN
          IROA = 1
          IRA = IRO(1)
          SET = .FALSE.
          DO WHILE ( .NOT. SET )
            IF ( FIRST_PASS ) THEN
              ISIR(IRA,IIRA(IRA)) = 0
              MAX_IRO = IROA
              SET = .TRUE.
            ELSE IF ( ISIR(IRA,IIRA(IRA)) .LT. NSIR(IRA,IIRA(IRA)) )
     &       THEN
              ! Active element is a non-maxed sequence - Increment
              ISIR(IRA,IIRA(IRA)) = ISIR(IRA,IIRA(IRA)) + 1
              MAX_IRO = IROA
              SET = .TRUE.
            ELSE IF ( IIRA(IRA) .EQ. NIR(IRA) ) THEN
              ! At last repeat element
              IIRA(IRA) = 1 ! Reset active string to first element
              ISIR(IRA,IIRA(IRA)) = 0 ! Reset sequence index
              IROA = IROA + 1 ! Increment active repeat string pointer
              IF ( IROA .GT. NR ) THEN ! All indices are maxed - Done
                DONE = .TRUE.
                SET = .TRUE.
              ELSE ! Set active repeat string
                IRA = IRO(IROA)
              END IF
            ELSE ! Use next element in current active repeat string
              IIRA(IRA) = IIRA(IRA) + 1
              ISIR(IRA,IIRA(IRA)) = 0
              MAX_IRO = IROA
              SET = .TRUE.
            END IF
          END DO
        END IF

        ! Initialize all sequence repeat strings on first pass
        IF ( FIRST_PASS ) THEN
          MAX_IRO = NR
          FIRST_PASS = .FALSE.
        END IF

        IF ( .NOT. DONE ) THEN ! Generate next string

          ! Update repeat strings that have changed
          DO IRAI = 1, MAX_IRO
            IR = IRO(IRAI)
            IA = IIRA(IR)
            LIRA = LIR(IR,IA)
            IF ( LIRA .LT. 0 ) THEN ! Sequence
              IRLS = IRL(IR,IA)
              IH = INDEX( TEMP_STR(IRLS:), '-' )
              SLS = LJUST( TEMP_STR(IRLS:IRLS+IH-2) )
              LLS = LEN_TRIM( SLS )
              IF ( LIRA .EQ. -1 ) THEN ! Single char sequence
                IF ( ISIR(IR,IA) .EQ. 0 ) THEN ! Start of sequence
                  ISIR(IR,IA) = 1
                  SEQ_VAL(IR) = SLS(1:1)
                ELSE
                  SEQ_VAL(IR) = CHAR(
     &               ICHAR( SLS(1:1) ) + ISIR(IR,IA) - 1 )
                END IF
              ELSE IF ( LIRA .LT. -1 ) THEN ! Multiple char seq
                IF ( ISIR(IR,IA) .EQ. 0 ) THEN ! Start of sequence
                  ISIR(IR,IA) = 1
                  SEQ_VAL(IR) = SLS(:LLS)
                ELSE
                  NSTR = SLS(:LLS)
                  READ( NSTR, '(BN,I25)' ) ISV
                  WRITE( SEQ_VAL(IR), '(BN,I25)', IOSTAT=IOS )
     &               ISV + ISIR(IR,IA) - 1
                  SEQ_VAL(IR) = LJUST( SEQ_VAL(IR) )
                  LSV = LEN_TRIM( SEQ_VAL(IR) )
                  SUS = ' '
                  SUS(LLS-LSV+1:) = SEQ_VAL(IR)
                  I = 1
                  DO WHILE ( ( I .LT. LEN(SUS) ) .AND.
     &               ( SUS(I:I) .EQ. ' ' ) )
                    SUS(I:I) = '0'
                    I = I + 1
                  END DO
                  SEQ_VAL(IR) = SUS
                END IF
              END IF
            END IF
          END DO

          ! Generate output string
          IG = 0
          IR = 0
          IO = 1
          IS = 1
          DO WHILE ( ( IS .LE. NS ) .AND. ( IO .LE. 255 ) )
            IF ( STR_TYP(IS) .EQ. 'G' ) THEN ! Global string
              IG = IG + 1
              TEMP_O(IO:255) = TEMP_STR(IGL(IG):IGR(IG))
              IO = IO + LG(IG)
            ELSE ! Repeat string
              IR = IR + 1
              IA = IIRA(IR)
              LIRA = LIR(IR,IA)
              IF ( LIRA .LT. 0 ) THEN ! Sequence value
                IRLS = IRL(IR,IA)
                IH = INDEX( TEMP_STR(IRLS:), '-' )
                ! Compute number of leading and trailing spaces
                NLS = 0
                DO WHILE ( TEMP_STR(IRLS+NLS:IRLS+NLS) .EQ. ' ' )
                  NLS = NLS + 1
                END DO
                IHM = IRLS + IH - 2
                NTS = 0
                DO WHILE ( TEMP_STR(IHM-NTS:IHM-NTS) .EQ. ' ' )
                  NTS = NTS + 1
                END DO
                TEMP_O(IO:) = ' '
                TEMP_O(IO+NLS:255) = SEQ_VAL(IR)
                IO = IO + LEN_TRIM( SEQ_VAL(IR) ) + NLS + NTS
              ELSE ! Non-sequence value
                TEMP_O(IO:255) = TEMP_STR(IRL(IR,IA):IRR(IR,IA))
                IO = IO + LIRA
              END IF
            END IF
            IS = IS + 1
          END DO
          TEMP_O(IO:) = ' '

          ! Perform directory search if requested - Output string
          IF ( DIR_SEARCH ) THEN ! Do directory search
            CALL FIND_FILES( LIST, PATH_EXP, TEMP_O, MATCHED )
          ELSE ! Output string to file
            WRITE( LIST.UNIT, '(A)', IOSTAT=IOS ) TEMP_O(:IO-1)
            IF ( IOS .NE. 0 ) THEN
              WRITE( *, * ) '*** ERROR - List file write failed'
              RETURN
            END IF
            LIST.N_REC = LIST.N_REC + 1
          END IF

          ! Flag done if no repeat strings
          IF ( NR .EQ. 0 ) DONE = .TRUE.

        END IF

      END DO

      ! Done
      IF ( ( DIR_SEARCH ) .AND. ( .NOT. MATCHED ) )
     & WRITE( *, * ) '*** No matching files found'

      RETURN
      END
