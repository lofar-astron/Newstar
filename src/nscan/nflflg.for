C+ NFLFLG.FOR
C  JEN 931116 (based on nflflg.for by WNB)
C
C  Revisions:
C
C	JEN 940216	Add DODRYRUN mode switch
C	CMV 940707	Add warning if HASCANS selected
C	JPH 940929	Add explanatory texts
C			Remove default option (belong in .psc file)
C       HjV 941031	Line too long
C
	SUBROUTINE NFLFLG
C
C  (Un)-flag scan data
C
C  Result:
C
C	CALL NFLFLG 	will flag or unflag scan data
C
C PIN references:
C
C	FLAG_OPTION
C	FLAG_MODE
C       OPS_COPY
C       OPS_MANUAL
C       OPS_CLIP
C       OPS_NOISE
C       OPS_DETERM
C	USER_FLAG
C
C  Include files:
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'NFL_DEF'
	INCLUDE 'CBITS_DEF'
	INCLUDE 'STH_O_DEF'		!SET HEADER
	INCLUDE 'SCH_O_DEF'		!SCAN HEADER
C
C  Parameters:
C
C  Arguments:
C
C  Function references:
C
	LOGICAL WNDPAR			!GET DWARF PARAMETER
	LOGICAL WNDNOD			!GET NODE
	LOGICAL WNFOP			!OPEN FILE
	LOGICAL WNFRD			!READ DATA
	LOGICAL WNFWR			!WRITE DATA
	CHARACTER*32 WNTTSG		!SET NAME
	LOGICAL WNDSTQ			!GET SETS
	INTEGER WNCAJ			!GET INTEGER FROM TEXT
        LOGICAL NFLFL9                  !DELETE INTERNAL FLAG LIST 
        REAL NFLST1                     !STATISTICS
        LOGICAL NFLCNT                  !FLAG COUNTING
        LOGICAL NFLCUB                  !UV-DATA CUBE
C
C  Data declarations:
C
	INTEGER		MASK		! flag mask
	CHARACTER*4     CFLAGS(8)	! flag names
	DATA            CFLAGS /'MAN','CLIP','NOIS','ADD','SHAD',
	1    			'U3','U2','U1'/
C
C NB: The variables CHARACTER OPTION*24 and OPT*3 are defined in a common.
C
	CHARACTER*8	FLOPT		!FLAG_MODE default 
	CHARACTER*8	UTILOPT		!Utility option (INSP,STAT,MODE) 
        CHARACTER*24    OPER            !CURRENT FLAGGING OPERATION
	INTEGER		NOPER		!CURRENT OPERATION NR
	INTEGER         DFAR		!FLAG FILE AREA (FOR FLIST OPS)
        CHARACTER*80    TXT80           !TEXT BUFFER
	LOGICAL         FLOPS           !CONTINUE TO NFLOPS
C
	LOGICAL		MODE_CORRDAT	!MODE: CORRECT DATA BEFORE USE
	LOGICAL		MODE_TRACE	!MODE: TRACE FLAGGING OPERATION
	LOGICAL		MODE_SHOW_CNT	!SHOW FLAG COUNTS AFTER OPERATION
	INTEGER		MODE_USERFLAG	!USER SPECIFIED FLAG (OVERRIDE)	
	LOGICAL		MODE_DODRYRUN	!DO DRY RUN FIRST, IF RELEVANT
C
	LOGICAL		CORRDAT		!Transient version of MODE_CORRDAT
	LOGICAL		TRACE		!Transient version of MODE_TRACE
	LOGICAL		SHOW_CNT	!Transient version of MODE_SHOW_CNT
	INTEGER		USERFLAG	!Transient version of MODE_CORRDAT
	LOGICAL		DODRYRUN	!Transient version of MODE_DODRYRUN
C-
C******************************************************************************
C******************************************************************************
C INIT
C
	MODE_CORRDAT=.FALSE.			!NO CORRECTION OF DATA
	MODE_TRACE = .FALSE.                    !NO TRACING OF OPS
	MODE_SHOW_CNT = .TRUE.			!SHOW FLAG COUNT AFTER OPS
	MODE_USERFLAG = 0			!NO SELFLAG OVERRIDE
	MODE_DODRYRUN = .TRUE.		        !DO DRY RUN FIRST, IF RELEVANT
C
	DFAR=0					!NO FLAG LIST AREA YET
C
        R0 = NFLST1 ('INIT',' ',' ',0,0.,0.)    !Initialise Statistics buffers
C
C*****************************************************************************
C GET HYPERCUBE TO WORK ON
C
 10	CONTINUE                                !FALL-BACK POINT 
C
	CALL WNCTXT(F_T,'!/
	1!4C\You have selected the flagging/data-statistics branch of NFLAG.!/
	1!4C\You must now define the "primary data cube" to work on.!/
	1!4C\(You may later define a sub-cube for specific operations or!/
	1!4C\ redefine the primary cube.)')
	IF (.NOT.NFLCUB ('SPECIFY','NODE',0,0,0,0)) GOTO 800
	IF (.NOT.NFLCUB ('SPECIFY','SETS',0,0,0,0)) GOTO 800
	IF (.NOT.NFLCUB ('SPECIFY','HYPERCUBE',0,0,0,0)) GOTO 800
	CALL WNCTXT(F_T,' ')
C
C*****************************************************************************
C GET FLAG_OPTION
C
        UTILOPT = ' '                           !Utility (INSP,MODE,STAT)
 100	CONTINUE
	IF (.NOT.WNDPAR('FLAG_OPTION',OPTION,LEN(OPTION),J0)) THEN
	  IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 800	!READY
          FLOPT = 'QUIT'
	  GOTO 100				!ERROR
	END IF
	IF (J0.EQ.0) GOTO 100			!EMPTY STRING, TRY AGAIN
	IF (J0.LT.0) GOTO 100			!WILDCARD (?)
C
 101    CONTINUE                                !RETURN-POINT FOR KNOWN OPTION
C
C  Reset switches for flagging operations to default (MODE) values:
C
	USERFLAG   = MODE_USERFLAG
	CORRDAT    = MODE_CORRDAT
	TRACE      = MODE_TRACE
	SHOW_CNT   = MODE_SHOW_CNT
	DODRYRUN   = MODE_DODRYRUN
C
C  If busy in `utility' group of operations, go there:
C
        IF (UTILOPT(:3).EQ.'INS') GOTO 190	!INSPECT (FLAGS)
        IF (UTILOPT(:3).EQ.'STA') GOTO 180	!STATISTICS (DATA)
        IF (UTILOPT(:3).EQ.'MOD') GOTO 120	!MODE 
        UTILOPT = ' '
C
C=========================================================================
C  OPTION QUIT:
C
	IF (OPTION(:3).EQ.'QUI') THEN
	  GOTO 800				!READY, EXIT
C
C=========================================================================
C  OPTION CLEAR (example of flagging operation called from this level):
C
	ELSE IF (OPTION(:3).EQ.'CLE') THEN
	  OPER = 'CLE'                          !FLAGGING OPERATION
          OPTION = ' '                          !ENSURE RETURN TO FLAG_OPTION
          FLOPT = '""'                          !DEFAULT FLAG_OPTION
	  CONTINUE                              !TO "CALL NFLOPS" BELOW
C
C=========================================================================
C OPTION FLIST (Interactions with internal flag list):
C
        ELSE IF (OPTION(:3).EQ.'FLI') THEN
C
 110      CONTINUE
	  IF (.NOT.WNDPAR('OPS_FLIST',OPER,LEN(OPER),J0,'QUIT')) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 100	!GO BACK TO FLAG_OPTION (?)
	    GOTO 110				!ERROR, TRY AGAIN
	  END IF
	  IF (J0.EQ.0) GOTO 110			!EMPTY STRING, TRY AGAIN
	  IF (J0.LT.0) GOTO 110			!WILDCARD, TRY AGAIN
C
	  IF (OPER(:3).EQ.'QUI') THEN
            FLOPT = OPTION                      !DEFAULT FLAG OPTION
	    GOTO 100				!BACK TO FLAG_OPTION
C
          ELSE IF (OPER(:3).EQ.'CLE') THEN      !CLEAR FLAGS      
            CONTINUE
          ELSE IF (OPER(:3).EQ.'STA') THEN      !DATA STATISTICS      
            GOTO 180                      
          ELSE IF (OPER(:3).EQ.'INS') THEN      !INSPECT FLAGS      
            GOTO 190                      
C
          ELSE                                  !FLAG-LIST OPERATIONS
            CALL NFLIST (OPER,USERFLAG,DFAR,
     1                   SHOW_CNT,TRACE)
            GOTO 110                            !BACK TO OPS_FLIST
          END IF
C
C=============================================================================
C OPTION FCOPY:
C
        ELSE IF (OPTION(:3).EQ.'FCO') THEN
 130	  CONTINUE
	  IF (.NOT.WNDPAR('OPS_FCOPY',OPER,LEN(OPER),J0,'QUIT')) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 100	!GO BACK TO FLAG_OPTION (?)
	    GOTO 130				!ERROR, TRY AGAIN
	  END IF
	  IF (J0.EQ.0) GOTO 130			!EMPTY STRING, TRY AGAIN
	  IF (J0.LT.0) GOTO 130			!WILDCARD, TRY AGAIN
	  IF (OPER(:3).EQ.'QUI') THEN
            FLOPT = OPTION                      !DEFAULT FLAG OPTION
	    GOTO 100				!BACK TO FLAG_OPTION
C
          ELSE IF (OPER(:3).EQ.'CLE') THEN      !CLEAR FLAGS      
            CONTINUE
          ELSE IF (OPER(:3).EQ.'STA') THEN      !STATISTICS      
            GOTO 180                      
          ELSE IF (OPER(:3).EQ.'INS') THEN      !INSPECT      
            GOTO 190                      
C
          END IF
C
C=============================================================================
C OPTION MANUAL:
C
        ELSE IF (OPTION(:3).EQ.'MAN') THEN
 140	  CONTINUE
	  IF (.NOT.WNDPAR('OPS_MANUAL',OPER,LEN(OPER),J0,'QUIT')) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 100	!GO BACK TO FLAG_OPTION (?)
	    GOTO 140				!ERROR, TRY AGAIN
	  END IF
	  IF (J0.EQ.0) GOTO 140			!EMPTY STRING, TRY AGAIN
	  IF (J0.LT.0) GOTO 140			!WILDCARD, TRY AGAIN
	  IF (OPER(:3).EQ.'QUI') THEN
            FLOPT = OPTION                      !DEFAULT FLAG OPTION
	    GOTO 100				!BACK TO FLAG_OPTION
C
          ELSE IF (OPER(:3).EQ.'CLE') THEN      !CLEAR FLAGS      
            CONTINUE
          ELSE IF (OPER(:3).EQ.'STA') THEN      !DATA STATISTICS      
            GOTO 180                      
          ELSE IF (OPER(:3).EQ.'INS') THEN      !INSPECT FLAGS      
            GOTO 190                      
C
          END IF
C
C=============================================================================
C OPTION HASCANS:
C
        ELSE IF (OPTION(:3).EQ.'HAS') THEN
 	  CALL WNCTXT(F_T,
 	1	  'BEWARE: Flagging on entire HA-Scans will override'//
 	1	'!/        any hypercube settings for  IFRS and POLS')
 150	  CONTINUE
	  IF (.NOT.WNDPAR('OPS_SCANS',OPER,LEN(OPER),J0,'QUIT')) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 100	!GO BACK TO FLAG_OPTION (?)
	    GOTO 150				!ERROR, TRY AGAIN
	  END IF
	  IF (J0.EQ.0) GOTO 150			!EMPTY STRING, TRY AGAIN
	  IF (J0.LT.0) GOTO 150			!WILDCARD, TRY AGAIN
	  IF (OPER(:3).EQ.'QUI') THEN
            FLOPT = OPTION                      !DEFAULT FLAG OPTION
	    GOTO 100				!BACK TO FLAG_OPTION
C
          ELSE IF (OPER(:3).EQ.'CLE') THEN      !CLEAR FLAGS      
            OPER = 'CLH'                        !HEADERS ONLY
          ELSE IF (OPER(:3).EQ.'STA') THEN      !DATA STATISTICS      
            GOTO 180                      
          ELSE IF (OPER(:3).EQ.'INS') THEN      !INSPECT FLAGS      
            GOTO 190                      
C
          END IF
C
C=============================================================================
C OPTION CLIPDATA:
C
        ELSE IF (OPTION(:3).EQ.'CLI') THEN
  160     CONTINUE
	  IF (.NOT.WNDPAR('OPS_CLIPDATA',OPER,LEN(OPER),J0,'QUIT')) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 100	!GO BACK TO FLAG_OPTION (?)
	    GOTO 160				!ERROR, TRY AGAIN
	  END IF
	  IF (J0.EQ.0) GOTO 160			!EMPTY STRING, TRY AGAIN
	  IF (J0.LT.0) GOTO 160			!WILDCARD, TRY AGAIN
	  write(*,*)oper
	  IF (OPER(:3).EQ.'QUI') THEN
            FLOPT = OPTION                      !DEFAULT FLAG OPTION
	    GOTO 100				!BACK TO FLAG_OPTION
C
          ELSE IF (OPER(:3).EQ.'CLE') THEN      !CLEAR FLAGS      
            OPER = 'CLD'                        !UV-DATA ONLY
          ELSE IF (OPER(:3).EQ.'STA') THEN      !DATA STATISTICS      
            GOTO 180                      
          ELSE IF (OPER(:3).EQ.'INS') THEN      !INSPECT FLAGS      
            GOTO 190                      
C
          END IF
	  write(*,*)oper
C
C=============================================================================
C OPTION DETERM:
C
        ELSE IF (OPTION(:3).EQ.'DET') THEN
  170     CONTINUE
	  IF (.NOT.WNDPAR('OPS_DETERM',OPER,LEN(OPER),J0,'QUIT')) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 100	!GO BACK TO FLAG_OPTION (?)
	    GOTO 170				!ERROR, TRY AGAIN
	  END IF
	  IF (J0.EQ.0) GOTO 170			!EMPTY STRING, TRY AGAIN
	  IF (J0.LT.0) GOTO 170			!WILDCARD, TRY AGAIN
	  IF (OPER(:3).EQ.'QUI') THEN
            FLOPT = OPTION                      !DEFAULT FLAG OPTION
	    GOTO 100				!BACK TO FLAG_OPTION
C
          ELSE IF (OPER(:3).EQ.'CLE') THEN      !CLEAR FLAGS      
            OPER = 'CLD'                        !UV-DATA ONLY
          ELSE IF (OPER(:3).EQ.'STA') THEN      !DATA STATISTICS      
            GOTO 180                      
          ELSE IF (OPER(:3).EQ.'INS') THEN      !INSPECT FLAGS     
            GOTO 190                      
C
          END IF
C
C=============================================================================
C OPTION MODE: Can be accessed as FLAG_OPTION (main group),
C              but also from the sub-groups of operations.
C
        ELSE IF (OPTION(:3).EQ.'MOD') THEN
C
  120     CONTINUE
          UTILOPT = 'MODE'
C
  121     CONTINUE
C
C  Display the current status:
C
	  TXT80 = '!4C\Current environment:'
	  TXT80(30:) = 'NOCORR'
	  IF (MODE_CORRDAT) TXT80(30:) = 'CORR'
	  TXT80(38:) = 'NOTRACE'
	  IF (MODE_TRACE) TXT80(38:) = 'TRACE'
	  TXT80(46:) = 'NOSHOW'
	  IF (MODE_SHOW_CNT) TXT80(46:) = 'SHOW'
	  TXT80(54:) = 'NODRYRUN'
	  IF (MODE_DODRYRUN) TXT80(54:) = 'DRYRUN'
	  CALL WNCTXT (F_T,TXT80)
C
	  IF (MODE_USERFLAG.NE.0) THEN
	    I1=1
	    IF (MODE_USERFLAG.EQ.FL_ALL) THEN
	      TXT80(1:6)='ALL='
	      I1=7
	    ENDIF
	    MASK=FL_MAN
	    DO I=1,8
	      IF (IAND(MASK,MODE_USERFLAG) .NE.0) THEN
		TXT80(I1:I1+5)=CFLAGS(I)
		I1=I1+5
		MASK=MASK/2
	      ENDIF
	    ENDDO	    
	    CALL WNCTXT (F_T,'!4C\Current USER_FLAGS: !30C!AS',TXT80(1:I1))
	  END IF
C
C  Prompt the user:
C
 122	  CONTINUE
	  IF (.NOT.WNDPAR('FLAG_MODE',OPER,LEN(OPER),
	1	J0)) THEN			! get one value at a time
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 122	! list of values exhausted
	    GOTO 120				! ERROR, TRY AGAIN
	  END IF
	  IF (J0.EQ.0) OPER='QUIT'			!
C
	  IF (OPER(:3).EQ.'QUI') THEN
            UTILOPT = ' '
	    IF (OPTION(:3).EQ.'MOD') THEN       !CALLED AS FLAG_OPTION
              FLOPT = '""'                      !DEFAULT FLAG OPTION
              GOTO 100	                        !BACK TO FLAG_OPTION
            ELSE                                !CALLED BY OPS-SUBGROUP
              GOTO 101                          !BACK TO OPS-SUBGROUP
            END IF
C
C Hypercube:
C
	  ELSE IF (OPER(:3).EQ.'HYP') THEN      !HYPERCUBE (POL,IFR,HA)
	    IF (.NOT.NFLCUB ('SPECIFY','HYPERCUBE',0,0,0,0)) GOTO 120
C
	  ELSE IF (OPER(:3).EQ.'SEC') THEN      !SETS OF SECTORS
	    IF (.NOT.NFLCUB ('SPECIFY','SETS',0,0,0,0)) GOTO 120
	    IF (.NOT.NFLCUB ('SPECIFY','HYPERCUBE',0,0,0,0)) GOTO 120
C
	  ELSE IF (OPER(:4).EQ.'NODE') THEN      !SCN NODE
	    CALL WNFCL(FCAIN)                           !?????
	    IF (.NOT.NFLCUB ('SPECIFY','NODE',0,0,0,0)) GOTO 120
	    IF (.NOT.NFLCUB ('SPECIFY','SETS',0,0,0,0)) GOTO 120
	    IF (.NOT.NFLCUB ('SPECIFY','HYPERCUBE',0,0,0,0)) GOTO 120
C
!*** USERFLAG (Specify user-flag type(s) to override default type(s))
C
	  ELSE IF (OPER(:3).EQ.'UFL') THEN	!USER FLAG
	    CALL WNDDA3('USER_FLAGS',MODE_USERFLAG)        !GET USER FLAGS
C
!*** CORRECT/NOCORRECT:
C
	  ELSE IF (OPER(:4).EQ.'CORR') THEN
	    MODE_CORRDAT=.TRUE.
	  ELSE IF (OPER(:4).EQ.'NOCO') THEN
	    MODE_CORRDAT=.FALSE.
C
!*** SHOW/NOSHOW:
C
	  ELSE IF (OPER(:4).EQ.'SHOW') THEN
	    MODE_SHOW_CNT=.TRUE.
	  ELSE IF (OPER(:4).EQ.'NOSH') THEN
	    MODE_SHOW_CNT=.FALSE.
C
!*** TRACE/NOTRACE:
C
	  ELSE IF (OPER(:4).EQ.'TRAC') THEN
	    MODE_TRACE=.TRUE.
	  ELSE IF (OPER(:4).EQ.'NOTR') THEN
	    MODE_TRACE=.FALSE.
C
!*** ESTIMATE DEFAULT CLIP VALUES:
C
	  ELSE IF (OPER(:3).EQ.'DRY') THEN
	    MODE_DODRYRUN=.TRUE.
	  ELSE IF (OPER(:4).EQ.'NODR') THEN
	    MODE_DODRYRUN=.FALSE.
C
          ELSE                                 
	    CALL WNCTXT(F_TP,'Unknown MODE operation, try again')
          END IF
C
C  Adapt the transient switches too:
C
	  USERFLAG = MODE_USERFLAG
	  CORRDAT  = MODE_CORRDAT
	  TRACE    = MODE_TRACE
	  SHOW_CNT = MODE_SHOW_CNT
	  DODRYRUN = MODE_DODRYRUN
C
          GOTO 121                  !BACK TO FLAG_MODE (ALWAYS)
C
C=============================================================================
C OPTION STATIST (Statistics): Can be accessed as FLAG_OPTION (main group),
C           but also from the sub-groups of operations.
C
        ELSE IF (OPTION(:3).EQ.'STA') THEN
 180      CONTINUE
C
 181      CONTINUE
          UTILOPT = 'STAT'
C
	  IF (.NOT.WNDPAR('OPS_STATIST',OPER,LEN(OPER),J0,'QUIT')) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 183	!GO BACK TO FLAG_OPTION (?)
	    GOTO 181				!ERROR, TRY AGAIN
	  END IF
	  IF (J0.EQ.0) GOTO 181			!EMPTY STRING, TRY AGAIN
	  IF (J0.LT.0) GOTO 181			!WILDCARD, TRY AGAIN
C
 182	  CONTINUE
	  FLOPS = .FALSE.                       !Default switch
	  IF (OPER(:3).EQ.'QUI') THEN
 183	    CONTINUE
            UTILOPT = ' '
	    IF (OPTION(:3).EQ.'STA') THEN       !CALLED AS FLAG_OPTION
              FLOPT = '""'                      !DEFAULT FLAG OPTION
              GOTO 100	                        !BACK TO FLAG_OPTION
            ELSE                                !CALLED BY OPS-SUBGROUP
              GOTO 101                          !BACK TO OPS-SUBGROUP
            END IF
C
	  ELSE IF (OPER(:3).EQ.'INS') THEN
	    GOTO 190				!INSPECT FLAGS
	  ELSE IF (OPER(:3).EQ.'MOD') THEN
	    GOTO 120				!CHANGE FLAG_MODES
C
	  ELSE IF (OPER(:3).EQ.'ACC') THEN      !ACCUMULATE STATISTICS
	    SHOW_CNT = .FALSE.                  !DO NOT SHOW FLAG-COUNT
	    FLOPS = .TRUE.                      
	  ELSE IF (OPER(:3).EQ.'ACD') THEN      !ACCUM. FOR DATA ONLY (?)
	    SHOW_CNT = .FALSE.                  !DO NOT SHOW FLAG-COUNT
	    FLOPS = .TRUE.                      
	  ELSE IF (OPER(:3).EQ.'ACH') THEN      !ACCUM. FOR HEADERS ONLY
	    SHOW_CNT = .FALSE.                  !DO NOT SHOW FLAG-COUNT
	    FLOPS = .TRUE.                      
C
          ELSE IF (OPER(:6).EQ.'GROUPS') THEN   !SHOW DEFINED GROUPS
            R0 = NFLST1 ('SHOW','#GROUPS',' ',0,0.,0.)
C
          ELSE IF (OPER(:7).EQ.'EXPLAIN') THEN  !EXPLAIN STATISTICAL QTS
            R0 = NFLST1 ('EXPLAIN','MEAN',TXT80,0,0.,0.)
            R0 = NFLST1 ('EXPLAIN','RMS',TXT80,0,0.,0.)
            R0 = NFLST1 ('EXPLAIN','RMSMS',TXT80,0,0.,0.)
            R0 = NFLST1 ('EXPLAIN','RMSVAR',TXT80,0,0.,0.)
            R0 = NFLST1 ('EXPLAIN','DCOFF',TXT80,0,0.,0.)
            R0 = NFLST1 ('EXPLAIN','MIN',TXT80,0,0.,0.)
            R0 = NFLST1 ('EXPLAIN','MAX',TXT80,0,0.,0.)
            R0 = NFLST1 ('EXPLAIN','WTOT',TXT80,0,0.,0.)
C
          ELSE IF (OPER(:3).EQ.'SCA') THEN      !SHOW SCAN HEADER STATISTICS
            R0 = NFLST1 ('SHOW','#SEPAR',' ',0,0.,0.)
            TXT80 = 'Some statistics of UNFLAGGED Scan header info.'
            R0 = NFLST1 ('SHOW','#TEXT',TXT80,0,0.,0.)
            R0 = NFLST1 ('SHOW','#SEPAR',' ',0,0.,0.)
            R0 = NFLST1 ('SHOW','#HEADER',' ',0,0.,0.) 
            R0 = NFLST1 ('SHOW','MAXABCS',' ',1,0.,0.) ! Max(abs(cos),abs(sin))
            R0 = NFLST1 ('SHOW','##REDNS',' ',1,0.,0.) ! Redundancy noise
            R0 = NFLST1 ('SHOW','##ALGNS',' ',1,0.,0.) ! Align/Selfcal noise
            R0 = NFLST1 ('SHOW','#SEPAR',' ',0,0.,0.)
C
          ELSE IF (OPER(:6).EQ.'SINGLE') THEN      !SHOW SINGLE-SLOT GROUPS
            R0 = NFLST1 ('SHOW','#SEPAR',' ',0,0.,0.)
            TXT80 = 'Statistics of all single-slot groups:'
            R0 = NFLST1 ('SHOW','#TEXT',TXT80,0,0.,0.)
            R0 = NFLST1 ('SHOW','#SEPAR',' ',0,0.,0.)
            R0 = NFLST1 ('SHOW','#HEADER',' ',0,0.,0.) 
            R0 = NFLST1 ('SHOW','#SINGLES',' ',1,0.,0.)  
            R0 = NFLST1 ('SHOW','#SEPAR',' ',0,0.,0.)
C
          ELSE IF (OPER(:5).EQ.'LISTS') THEN
	    CALL NFLST3 (OPER,' ',' ')            !
          ELSE IF (OPER(:5).EQ.'DCOFF') THEN
	    CALL NFLST3 (OPER,' ',' ')            !
C
          ELSE IF (OPER(:5).EQ.'UVDAT') THEN      !SHOW DATA STATISTICS
            R0 = NFLST1 ('SHOW','#SEPAR',' ',0,0.,0.)
            TXT80 = 'Some statistics of UNFLAGGED data.'
            R0 = NFLST1 ('SHOW','#TEXT',TXT80,0,0.,0.)
            R0 = NFLST1 ('SHOW','#SEPAR',' ',0,0.,0.)
            R0 = NFLST1 ('SHOW','#HEADER',' ',0,0.,0.) 
            R0 = NFLST1 ('SHOW','##DAT_A_',' ',1,0.,0.)     ! Amplitudes
            R0 = NFLST1 ('SHOW','##DAT_P_',' ',1,0.,0.)     ! Phases
            R0 = NFLST1 ('SHOW','##DAT_C_',' ',1,0.,0.)     ! Cosines
            R0 = NFLST1 ('SHOW','##DAT_S_',' ',1,0.,0.)     ! Sines
            R0 = NFLST1 ('SHOW','#SEPAR',' ',1,0.,0.)
C
          ELSE IF (OPER(:2).EQ.'XX' .OR.
     1             OPER(:2).EQ.'XY' .OR.
     1             OPER(:2).EQ.'YX' .OR.
     1             OPER(:2).EQ.'YY') THEN
            R0 = NFLST1 ('SHOW','#SEPAR',' ',0,0.,0.)
            TXT80 = 'Some statistics of UNFLAGGED data.'
            R0 = NFLST1 ('SHOW','#TEXT',TXT80,0,0.,0.)
            R0 = NFLST1 ('SHOW','#SEPAR',' ',0,0.,0.)
            R0 = NFLST1 ('SHOW','#HEADER',' ',0,0.,0.)
            R0 = NFLST1 ('SHOW','##_'//OPER(:2),' ',1,0.,0.)  !overall
            R0 = NFLST1 ('SHOW','#SEPAR',' ',0,0.,0.)
C
          ELSE
	    CALL NFLST3 ('GROUP',OPER,' ')       !SHOW accumulation group
          END IF
	  IF (.NOT.FLOPS) GOTO 181               !Back to OPS_STAT
C
C=============================================================================
C OPTION INSPECT: Can be accessed as FLAG_OPTION (main group),
C                     but als from the sub-groups of operations.
C
        ELSE IF (OPTION(:3).EQ.'INS') THEN
C
  190     CONTINUE
C
	  CALL WNCTXT (F_T,' ')
	  CALL WNCTXT (F_T,'You may now inspect a summary of the'
     1         //' flags that were counted during ')
          CALL WNCTXT (F_T,'the last flagging operation.')
	  CALL WNCTXT (F_T,'Use COUNT to count ALL flags in'
     1         //' (a sub-cube of) the specified data hyper-cube.')
	  CALL WNCTXT (F_T,' ')
C
  191     CONTINUE
          UTILOPT = 'INSPECT'
C
	  IF (.NOT.WNDPAR('OPS_INSPECT',OPER,LEN(OPER),J0,'QUIT')) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 193	!GO BACK TO FLAG_OPTION (?)
	    GOTO 191				!ERROR, TRY AGAIN
	  END IF
	  IF (J0.EQ.0) GOTO 191			!EMPTY STRING, TRY AGAIN
	  IF (J0.LT.0) GOTO 191			!WILDCARD, TRY AGAIN
C
  192	  CONTINUE
	  IF (OPER(:3).EQ.'QUI') THEN
  193	    CONTINUE
            UTILOPT = ' '
	    IF (OPTION(:3).EQ.'INS') THEN       !CALLED AS FLAG_OPTION
              FLOPT = '""'                      !DEFAULT FLAG OPTION
              GOTO 100	                        !BACK TO FLAG_OPTION
            ELSE                                !CALLED BY OPS-SUBGROUP
              GOTO 101                          !BACK TO OPS-SUBGROUP
            END IF
C
          ELSE IF (OPER(:3).EQ.'STA') THEN      !STATISTICS      
            GOTO 180                      
C
	  ELSE IF (OPER(:3).EQ.'CLE') THEN      !CLEAR FLAGS
	    CONTINUE
	  ELSE IF (OPER(:3).EQ.'CLD') THEN      !CLEAR DATA FLAGS
	    CONTINUE
	  ELSE IF (OPER(:3).EQ.'CLH') THEN      !CLEAR HEADER FLAGS
	    CONTINUE
C
	  ELSE IF (OPER(:3).EQ.'COU') THEN      !COUNT FLAGS
            CONTINUE                            !TO FLAGGING OPERATIONS
C
          ELSE IF ((OPER(:3).EQ.'FTY') .OR.       
     1             (OPER(:2).EQ.'HA') .OR. 
     1             (OPER(:3).EQ.'CHA') .OR. 
     1             (OPER(:3).EQ.'TEL') .OR. 
     1             (OPER(:3).EQ.'IFR')) THEN    
	    TXT80 = OPER(:3)                    !TRANSFER INPUT STRING
	    TXT80 = OPER                        !Temporary
	    IF (INDEX(OPER,'_').GT.0) THEN
	      CALL WNCTXT (F_T,'Switch detected: '//OPER(3:))
	    END IF
            JS = NFLCNT('SHOW',TXT80,0,0,0,0,0)  !SHOW FLAGS IN VARIOUS PROJ.
            GOTO 191
C
          ELSE
	    CALL WNCTXT(F_TP,'Unknown INSPECT operation, try again')
            GOTO 191                             !BACK TO OPS_INSPECT
          END IF
C
C=============================================================================
C UNKNOWN OPTION:
C
        ELSE
	  CALL WNCTXT(F_TP,'Unknown FLAG_OPTION, try again')
          GOTO 100                              !BACK TO FLAG_OPTION
	END IF
C 
C*****************************************************************************
C*****************************************************************************
C*****************************************************************************
C FLAGGING OPERATIONS:
C
C Execute the specified flagging operation:
C
	CALL NFLOPS (OPER,USERFLAG,CORRDAT,
     1               SHOW_CNT,TRACE,DODRYRUN)
C
C When the operation is finished, return to the relevant operations-group:
C
	IF (OPTION.EQ.' ') GOTO 100             !BACK TO FLAG_OPTION 
        GOTO 101                                !BACK TO GROUP OF OPERATIONS
C
C**************************************************************************
C Finished all flagging operations:
C
 800	CONTINUE
C
C Make sure that the flag-list area has gone properly:
C
	JS=NFLFL9(DFAR)
C
	RETURN
	END
