C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	SYS_VIEW
C.Keywords:	Program Parameters, Defaults, Show
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	900421 FMO - recreation
C.Version:	910718 FMO - allow for longer-than-132 output lines (patch)
C.Version:	910817 FMO - downgraded: no /INFO, /PRINT, /LEVEL, /ALL
C.Version:	920206 GvD - add former optional arguments to CLI_GET
C.Version:	930728 CMV - add /GENERAL switch
C.Version:	930923 CMV - logical names for new maintenance system
C.Version:	931108 CMV - new option /SHORT 
C.Version:	940119 CMV - use WNGLUN i.s.o GEN_LUN
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	SUBROUTINE VIEW
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Show the current defaults for program parameters
C.Returns:	Not applicable
C.Notes:
C	- Parameter:
C		symbol_list		default: *$*_* (i.e. all)
C	- Qualifiers (the names can be abbreviated to a single letter):
C		/INPUT=save_file	default: /NOINPUT
C		/SUBSTITUTE		default: /NOSUBSTITUTE
C		/EXTERNAL		default: /NOEXTERNAL
C		/SHORT			default: /NOSHORT
C		/TEST or /NOTEST	default: DWARF control parameter
C		/GENERAL		default: /NOGENERAL
C
C	- The symbol lists (parameter and /EXCLUDE value) are comma-separated
C	  lists of DWARF symbol names:
C		<program_name>$<stream_name>_<parameter_name>
C	  where each name can be absent or wildcarded (*). The dollar and
C	  underscore prefixes are part of the stream and parameter name
C	  components.
C	- The lists will be expanded as follows: each absent component will be
C	  replaced by the component from the previous symbol name, except that
C	  the stream for global programs will be set to $0. The default for the
C	  first name is
C		*$<current_stream>_*.
C
C	- If /GENERAL is given, the symbol list can contain general symbols
C	  as well, and only the value for each symbol is given.
C	- Normally, output is <keyword> = <value>; if /GENERAL is given with
C	  a single keyword, only value is output.
C
C	- If /SHORT is given, no program prefixes etc will be given, only
C	  external keys will be shown without the "(user)" remark.
C
C	- If /INPUT is given, all the definitions in the save file (defaults:
C	  current directory and type .SAV) that match the symbol list will be
C	  displayed.
C	- If the symbol list contains a wildcard program or stream name, all
C	  the current external defaults with names matching the symbol list
C	  will be displayed.
C	- Otherwise all the program parameters matching the symbol list will be
C	  displayed together with their external or PPD defaults (if present).
C	  But if you give /EXTERNAL only those parameters will be displayed
C	  which have an external default.
C-------------------------------------------------------------------------
C
	CHARACTER*(*)	PROGRAM, BLANK, ASTER, COMMA
	CHARACTER*(*)	OPAR, CPAR, EQUALS
		PARAMETER (PROGRAM = 'VIEW')
		PARAMETER (BLANK   = ' '   )
		PARAMETER (ASTER   = '*'   )
		PARAMETER (COMMA   = ','   )
		PARAMETER (OPAR    = ' ('  )
		PARAMETER (CPAR    = ')'   )
		PARAMETER (EQUALS  = ' = ' )
	CHARACTER*(*)	ANUMX
		PARAMETER (ANUMX = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_$')
C
	INTEGER		NRARG, PAR, Q, QVAL
		PARAMETER (NRARG = 7)
		PARAMETER (PAR   = CLI__PARAMETER)
		PARAMETER (Q     = CLI__QUALIFIER)
		PARAMETER (QVAL  = CLI__QUALIFIER+CLI__VALUE)
	CHARACTER*11	NAME(NRARG)
	INTEGER		ATTR(NRARG)
	CHARACTER*12	PROMPT(NRARG)
	CHARACTER*5	DEFVAL(NRARG)
		DATA NAME   /'SYMLIST','INPUT','SUBSTITUTE',
     &	                     'TEST','EXTERNAL','GENERAL','SHORT'/
		DATA ATTR   / PAR     , QVAL  ,Q           ,
     &	                      Q    , Q        ,Q,Q /
		DATA PROMPT /' '      ,' '    ,' '         ,
     &	                     ' '   ,' '       ,' ',' '/
		DATA DEFVAL /'*$*_*'  ,' '    ,' '         ,
     &	                     ' '   ,' '       ,' ',' '/
C
	INTEGER		CLI_INIT, CLI_GET
	INTEGER		MSG_INIT, MSG_SET
	INTEGER		DWC_CTL_OPEN, DWC_TEST_PUT, DWC_TEST_INQ
	INTEGER		VP_DEF_CHECK, DWC_LEVEL_GET, DWC_SYM_BUILD
	INTEGER		DWC_SYMLIST_EXPAND, DWC_SYM_SPLIT
	INTEGER		PV_DEF_GET
	INTEGER		PPD_INIT, PPD_EXIT
	INTEGER		PPD_READ_U, PPD_READ_UNXT, PPD_UNAM_GET, PPD_AMAS_GET
	INTEGER		STR_SIGLEN, STR_SKIP_W, STR_COPY_W, STR_COPY_U
	INTEGER		STR_MATCH_L
	INTEGER		FILNAM_FULL, GEN_FORIOS
	INTEGER		SYMBOL_SEARCH, SYMBOL_GET
C
	CHARACTER	SYMLIST*512, VALUE*255, WORK*512, SAVFILE*80, TYPE*20
	CHARACTER	NAM*64, PROG*9, STREAM*12, KEY*16, DUM*1
	CHARACTER	LASTPROG*9, KEYWORD*16
	INTEGER		LL, LV, LW, LF, LT, LDUM
	INTEGER		LN, LP, LS, LK
	INTEGER		IS, IOS, TMP, LUN, MINL, PTR
	INTEGER		USERLEVEL, MAXLEVEL, NRSYM, NR
	LOGICAL		DO_SUBST, ALL_DEFAULTS, FIRST_KEY
	LOGICAL		DO_GENERAL, DO_SHORT
	LOGICAL		TEST_MODE, PROG_DEF, PROTO, FOUND, SINGLE
		DATA LASTPROG /BLANK/
		DATA NRSYM    /0/
C
C
C					Initialize
C					- get DWARF control variables
C					- start messenger
C					- initialize command-line interpreter
C
	IS = DWC_CTL_OPEN ()
	IF (IAND(IS,1).NE.0) IS = MSG_INIT (PROGRAM,F_T)
	IF (IAND(IS,1).NE.0) IS = CLI_INIT (NRARG,NAME,ATTR,PROMPT,DEFVAL)
	IF (IAND(IS,1).EQ.0) GOTO 999

C
C					Check /GENERAL, if given do not
C					expand symbol list
C
	IS = CLI_GET ('GENERAL',DUM,LDUM)
	IF (IAND(IS,1).EQ.0) GOTO 999
	DO_GENERAL=(IS.EQ.DWC_PRESENT)
C
C					Get and expand the symbol list
C
	IS = CLI_GET ('SYMLIST',VALUE,LV)
	IF (IAND(IS,1).NE.0) THEN
	  IF (DO_GENERAL) THEN
	     LL=LV
	     SYMLIST(:LL)=VALUE(:LV)
	  ELSE
	     IS = DWC_SYMLIST_EXPAND (VALUE(:LV),SYMLIST,LL)
	  END IF
	END IF
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					+----------------------+
C					| List general symbols |
C					+----------------------+
C					If /GENERAL is given:
C					- just display all matching symbols
C
	IF (DO_GENERAL) THEN
	    NR = 0
	    SINGLE=(INDEX(SYMLIST(:LL),ASTER).EQ.0 .AND.
     &	            INDEX(SYMLIST(:LL),COMMA).EQ.0 )
	    TMP = SYMBOL_SEARCH (SYMLIST(:LL),BLANK,NR,NAM,LN)
	    IF (IAND(TMP,1).EQ.0) IS = TMP
	    DO WHILE (LN.GT.0)
		TMP = SYMBOL_GET (NAM(:LN),VALUE,LV)
		IF (IAND(TMP,1).EQ.0) IS = TMP
		IF (SINGLE) THEN
		  CALL WNCTXT(DWLOG,'!AS',VALUE(:LV))
	        ELSE
		  CALL WNCTXT(DWLOG,'  !AS = !AS',NAM(:LN),VALUE(:LV))
	        END IF
C		NRSYM = NRSYM+1
		TMP = SYMBOL_SEARCH (SYMLIST(:LL),BLANK,NR,NAM,LN)
		IF (IAND(TMP,1).EQ.0) IS = TMP
	    END DO
	    IF (IAND(IS,1).EQ.0) THEN				!some symbol error:
	    ELSE IF (NRSYM.EQ.0) THEN			!no matching defaults:
C		CALL WNCTXT(DWLOG,'  No symbols found')	! tell
	    END IF
	    GOTO 999					!anyway: exit
	END IF
C
C					+---------------------+
C					| List from save file |
C					+---------------------+
C					If /INPUT=file is given:
C					- display all the lines in which the
C					  symbol name matches the symbol list
C					- exit
C
	IS = CLI_GET ('INPUT',VALUE,LV)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (LV.GT.0) THEN
	    IS = FILNAM_FULL (VALUE(:LV),SAVFILE,LF,'.SAV')
	    IF (IAND(IS,1).EQ.0) GOTO 999
	    CALL WNGLUN(LUN)
	    IF (LUN.EQ.0) THEN
		IS=GEN_LUNNOFREE
		GOTO 999
	    END IF
	    CALL WNCTXT(DWLOG,'!/List from save file !AS',SAVFILE(:LF))
	    OPEN (UNIT=LUN,FILE=SAVFILE(:LF),TYPE='OLD',IOSTAT=IOS)
	    IF (IOS.EQ.0) READ (LUN,'(Q,A)',IOSTAT=IOS) LW,WORK
	    DO WHILE (IOS.EQ.0)
		PTR = 1
		LN = 0					!extract name
		TMP = STR_SKIP_W (BLANK,WORK(:LW),PTR)
		TMP = STR_COPY_W (ANUMX,WORK(:LW),PTR,NAM,LN)
		IF (PTR.LT.LW .AND. WORK(PTR:PTR).EQ.ASTER) THEN
		    PTR = PTR+1				!skip asterisk
		    TMP = STR_COPY_W (ANUMX,WORK(:LW),PTR,NAM,LN)
		END IF
		IF (LN.GT.0) THEN			!matching name ?
		    TMP = STR_MATCH_L (NAM(:LN),SYMLIST(:LL),NR)
		    IF (TMP.EQ.1) THEN			!yes:
			CALL WNCTXT(DWLOG,'!AS',WORK(:LW))
			NRSYM = NRSYM+1
		    END IF
		END IF
		READ (LUN,'(Q,A)',IOSTAT=IOS) LW,WORK
	    END DO
	    IF (IOS.GT.0) THEN				!I/O error:
		IS = GEN_FORIOS (SAVFILE(:LF))		! set error status
	    ELSE IF (NRSYM.EQ.0) THEN			!no matching symbols:
		CALL WNCTXT(DWLOG,'  No symbols found')	! tell
	    END IF
	    GOTO 999					!anyway: exit
	END IF
C
C					+-----------------------------+
C					| List only external defaults |
C					+-----------------------------+
C					If the symbol list contains a wild
C					program and/or stream name:
C					- display all the external defaults
C					  with names matching the symbol list
C					- exit
C
	IF (INDEX(SYMLIST(:LL),'*$').NE.0 .OR.
	1   INDEX(SYMLIST(:LL),'$*').NE.0) THEN
	    NR = 0
	    TMP = SYMBOL_SEARCH (SYMLIST(:LL),BLANK,NR,NAM,LN)
	    IF (IAND(TMP,1).EQ.0) IS = TMP
	    DO WHILE (LN.GT.0)
		TMP = SYMBOL_GET (NAM(:LN),VALUE,LV)
		IF (IAND(TMP,1).EQ.0) IS = TMP
		CALL WNCTXT(DWLOG,'  !AS = !AS',NAM(:LN),VALUE(:LV))
		NRSYM = NRSYM+1
		TMP = SYMBOL_SEARCH (SYMLIST(:LL),BLANK,NR,NAM,LN)
		IF (IAND(TMP,1).EQ.0) IS = TMP
	    END DO
	    IF (IAND(IS,1).EQ.0) THEN				!some symbol error:
	    ELSE IF (NRSYM.EQ.0) THEN			!no matching defaults:
		CALL WNCTXT(DWLOG,'  No symbols found')	! tell
	    END IF
	    GOTO 999					!anyway: exit
	END IF
C
C					+-------------------------+
C					| List program parameters |
C					+-------------------------+
C					Inspect the qualifiers
C
	IS = CLI_GET ('EXTERNAL',DUM,LDUM)
	IF (IAND(IS,1).EQ.0) GOTO 999
	ALL_DEFAULTS = IS.NE.DWC_PRESENT		!list all defaults ?
C
	IS = CLI_GET ('SHORT',DUM,LDUM)
	IF (IAND(IS,1).EQ.0) GOTO 999
	DO_SHORT=(IS.EQ.DWC_PRESENT)
	IF (DO_SHORT) ALL_DEFAULTS=.FALSE.
C
	IS = CLI_GET ('TEST',DUM,LDUM)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (IS.NE.DWC_ABSENT) THEN			!/[NO]TEST given:
		TEST_MODE = IS.EQ.DWC_PRESENT		! en/disable test mode
		IS = DWC_TEST_PUT (TEST_MODE)
	ELSE						!no test qualifier:
		IS=DWC_SUCCESS
		TEST_MODE = DWC_TEST_INQ ()		! get default mode
	END IF
C
	IS = CLI_GET ('SUBSTITUTE',DUM,LDUM)
	IF (IAND(IS,1).EQ.0) GOTO 999
	DO_SUBST = IS.EQ.DWC_PRESENT			!substitute symbols ?
	IS = DWC_LEVEL_GET (USERLEVEL,MAXLEVEL)		!get userlevel
C
C					Loop through the symbol list
C
	PTR = 0
 100	PTR = PTR+1
	IF (PTR.GT.LL) GOTO 900				!end of list
	LN = 0
	TMP = STR_COPY_U (COMMA,SYMLIST(:LL),PTR,NAM,LN)
	TMP = DWC_SYM_SPLIT (NAM(:LN),PROG,LP,STREAM,LS,KEY,LK)
C
C					If new program name:
C					- close PPD file and open new one
C					- no error if PPD file is not found
C
	IF (PROG.NE.LASTPROG) THEN
	    IF (LASTPROG.NE.BLANK) TMP = PPD_EXIT ()
	    TMP = PPD_INIT (PROG)
	    IF (IAND(TMP,1).EQ.0) THEN
		IF (TMP.EQ.PPD_PPDNOTFND) THEN
		ELSE
		    IS = MSG_SET (TMP,0)
		END IF
		LASTPROG = BLANK
		GOTO 100				!next list element
	    END IF
	    LASTPROG = PROG
	END IF
C
C					Get the full parameter name
C					- if explicit keyword: just that one
C
	FIRST_KEY = .TRUE.
 200	IF (KEY(:LK).NE.'*') THEN
		KEYWORD = KEY
		TMP = PPD_READ_U (KEYWORD)
		LK = STR_SIGLEN (KEYWORD)
		IF (IAND(TMP,1).EQ.0) THEN
		    IF (TMP.EQ.PPD_KEYAMBIG) THEN
			IS = MSG_SET (TMP,1)
			CALL WNCTXT(DWLOG,DWMSG,KEY)
		    ELSE
			IS = MSG_SET (DWC_UNKKEYW,1)
			CALL WNCTXT(DWLOG,DWMSG,KEY,' ',PROG(:LP))
		    ENDIF
		    GOTO 100				!next list element
		END IF
C
C					- if wildcard keyword, get the first or
C					  next one from the PPD file, accepting
C					  test parameters only if in test mode
C
	ELSE
		FOUND = .FALSE.
		DO WHILE (.NOT.FOUND)
		    IF (FIRST_KEY) THEN
			FIRST_KEY = .FALSE.
			TMP = PPD_READ_U (' ')
		    ELSE
			TMP = PPD_READ_UNXT ()
		    ENDIF
		    IF (TMP.EQ.PPD_ENDOFFILE) GOTO 100	!PPD end: next list elm
		    IF (IAND(TMP,1).NE.0)
	1			TMP = PPD_UNAM_GET (KEYWORD,LK,MINL,PROTO)
		    IF (IAND(TMP,1).EQ.0) THEN			!PPD error:
			GOTO 100			! next list element
		    END IF
		    FOUND = TEST_MODE .OR. IAND(PPD_AMAS_GET('TEST'),1).EQ.0
		END DO
	END IF
C
C					Process the parameter default:
C					- build full name
C					- get the value
C					- if /EXTERNAL: skip unless external
C					- if /SUBSTITUTE: expand the value
C					- display the name and value
C					- go for the next parameter
C
	TMP = DWC_SYM_BUILD (PROG(:LP),STREAM(:LS),KEYWORD(:LK),NAM,LN)
	TMP = PV_DEF_GET (NAM(:LN),WORK,LW,TYPE,LT)
	PROG_DEF = LT.NE.0 .AND. TYPE(1:1).EQ.'p'
	IF (ALL_DEFAULTS .OR. (LT.NE.0.AND.TYPE(1:1).NE.'p')) THEN
		IF (DO_SUBST) THEN
		    TMP = VP_DEF_CHECK (NAM(:LN),WORK(:LW),PROG_DEF,
	1					USERLEVEL,VALUE,LV)
		    IF (IAND(TMP,1).EQ.0) IS = TMP
		ELSE
		    VALUE = WORK(:LW)
		    LV = LW
		END IF
		IF (DO_SHORT.AND.LV.GT.0) THEN
		    CALL WNCTXT(DWLOG,'  !AS = !AS',KEYWORD(:LK),
	1	                                     VALUE(:LV))
	        ELSE IF (DO_SHORT) THEN
		    CALL WNCTXT(DWLOG,'  !AS = /NOASK',KEYWORD(:LK))
		ELSE IF (LV.GT.0) THEN
		    CALL WNCTXT(DWLOG,'  !AS (!AS) = !AS',
	1				NAM(:LN),TYPE(:LT),VALUE(:LV))
		ELSE
		    CALL WNCTXT(DWLOG,'  !AS (!AS)',NAM(:LN),TYPE(:LT))
		END IF
		NRSYM = NRSYM+1
	END IF
	IF (KEY(:LK).EQ.'*') GOTO 200			!go for next parameter
	GOTO 100					!or next list element
C
 900	CONTINUE					!end of symbol list
	IF (LASTPROG.NE.BLANK) TMP = PPD_EXIT ()	!close PPD file
	IF (IAND(IS,1).EQ.0) THEN				!some error:
	ELSE IF (NRSYM.EQ.0) THEN			!no matching defaults:
	    CALL WNCTXT(DWLOG,'  No symbols found')	! tell
	END IF
	GOTO 999					!anyway: exit
C
C					Terminate the program
C
 999	E_C = MSG_SET(IS,0) 		!WNGEX exit code
	END
