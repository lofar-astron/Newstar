C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	ABP_RUN
C.Keywords:	Automatic Batch Programming
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	FX-Fortran
C.Environment:	Alliant
C.Comments:
C.Version:	910626 FMO - creation
C.Version:	910704 FMO - search .EXE in EXEUSER after EXEDWARF (<-Foley)
C.Version:	910708 FMO - allow different ABP and DWARF program names,
C			- act upon RUNMODE=DEFINE
C.Version:	910815 FMO - renew ABP_CLEAR (PPD file no longer needed)
C			- renew ABP_RESTORE
C.Version:	910909 FMO - adapted for Alliant
C.Version:	911206 GvD - use EXECL iso. SYSTEM to start program
C			PPD_EXIT is called in ABP_RUN_DO
C			Added routine ABP_RUN_SPEC
C			ABP_SPECIFY is removed (SP_LIST can be used)
C			Up to 20 arguments are allowed in the CLI (was 10)
C.Version:	920228 GvD - search .sav-file on SYS$LOGIN, then on ABPDIR
C.Version:	930923 CMV - logical names for new maintenance system
C.Version:	940119 CMV - use WNGLUN i.s.o. GEN_GET_LUN
C.Version:	940121 CMV - changed messenger
C.Version:	940211 WNB - change file inquire
C.Version:	940628 CMV - built in some safety checks
C.Version:      010709 AXC - Linux port:tmpchar in calls
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	LOGICAL FUNCTION ABP_RUN_INIT (PROGNAM,DWFNAM,NRARG,NAME,DEFVAL,
	1					EXEFIL,STREAM,RUNMODE)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	PROGNAM		!(i) ABP program name
	CHARACTER*(*)	DWFNAM		!(i) DWARF program name
	INTEGER		NRARG		!(i) nr of possible arguments
	CHARACTER*(*)	NAME(*)		!(i) argument names
	CHARACTER*(*)	DEFVAL(*)	!(i) default argument values
	CHARACTER*(*)	STREAM		!(o) stream name (prefixed with $)
	CHARACTER*(*)	EXEFIL		!(o) expanded spec of executable
	CHARACTER*(*)	RUNMODE		!(o) run mode
C
C.Purpose:	Initialise ABP program
C.Returns:	.TRUE. for success, .FALSE. otherwise
C.Notes:
C	- Initialise DWARF control, start messenger and start command-line
C	  interpreter.
C	- Check presence of program executable in EXEDWARF (or in EXEUSER).
C	- Get stream name and run mode from command line.
C	- Open PPD file.
C	- Refresh appropriate external defaults.
C-------------------------------------------------------------------------
C
	INTEGER		PARM, QUAL		!argument-attribute codes
	PARAMETER (PARM = CLI__PARAMETER)
	PARAMETER (QUAL = CLI__QUALIFIER+CLI__DEFAULT+CLI__VALUE)
C
	INTEGER		ATTR(20)		!argument attributes
	CHARACTER*1	PROMPT(20)		!prompt strings
	DATA ATTR /PARM,19*QUAL/		!1 parameter, up to 19 qual's
	DATA PROMPT /20*' '/			!dummy prompt strings
C
	LOGICAL		ABP_CLEAR, ABP_RESTORE
	INTEGER		DWC_CTL_OPEN, DWC_STREAM_CHECK
	INTEGER		MSG_INIT, MSG_SET
	INTEGER		CLI_INIT, CLI_GET
	INTEGER		PPD_INIT
	INTEGER		STR_SIGLEN, FILNAM_FULL
	LOGICAL		WNFOP
C
	CHARACTER	SAVFIL*80, XSTREAM*16, TMP*80
	INTEGER		IS, LP, LD, LE, LR, LS, LSV, LX
	LOGICAL		OK, EXIST
	INTEGER		FCAT
C
C
C				Initialize
C				- get DWARF control variables
C				- start messenger
C				- start command-line interpreter
C
	LP = STR_SIGLEN (PROGNAM)
	LD = STR_SIGLEN (DWFNAM)
	IS = DWC_CTL_OPEN ()
	TMP='ABPX_'//PROGNAM(:LP)
	IF (IAND(IS,1).NE.0) IS = MSG_INIT(TMP,F_T)
	IF (IAND(IS,1).NE.0) IS = CLI_INIT (NRARG,NAME,ATTR,PROMPT,DEFVAL)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C				Build the full executable spec
C				and check its existence
C
	TMP=DWFNAM(:LD)//'.EXE'
	IS = FILNAM_FULL (TMP,EXEFIL,LE,'n_exe')
	IF (IAND(IS,1).EQ.0) GOTO 999
	EXIST=WNFOP(FCAT,EXEFIL(:LE),'R')
	IF (.NOT.EXIST) THEN
	   TMP=DWFNAM(:LD)//'.EXE'
	   IS = FILNAM_FULL (TMP,EXEFIL,LE,'n_uexe')
	   IF (IAND(IS,1).EQ.0) GOTO 999
	   EXIST=WNFOP(FCAT,EXEFIL(:LE),'R')
	END IF
	IF (.NOT.EXIST) GOTO 991
	CALL WNFCL(FCAT)
C
C				Get standard command-line arguments
C
	IS = CLI_GET ('STREAM',XSTREAM,LX)
	IF (IAND(IS,1).NE.0) IS = CLI_GET ('RUNMODE',RUNMODE,LR)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C				Stream name must be present
C
	IF (XSTREAM(:LX).EQ.'""') GOTO 992
C
C				Check stream name
C				- must be valid stream name, not $0
C				- <prognam><stream>.sav must exist
C				  in ABPDIR or user's login directory
C
	IS = DWC_STREAM_CHECK (XSTREAM(:LX),STREAM,LS,.FALSE.)
	IF (IAND(IS,1).EQ.0) GOTO 993
	IF (STREAM(:LS).EQ.'$' .OR. STREAM(:LS).EQ.'$0') GOTO 993
	TMP=PROGNAM(:LP)//STREAM(:LS)//'.SAV'
	IS = FILNAM_FULL (TMP,
	1			SAVFIL,LSV,'HOME')
	IF (IAND(IS,1).EQ.0) GOTO 993
	EXIST=WNFOP(FCAT,SAVFIL(:LSV),'R')
	IF (.NOT.EXIST) THEN
		TMP=PROGNAM(:LP)//STREAM(:LS)//'.SAV'
		IS = FILNAM_FULL (TMP
	1		,SAVFIL,LSV,'ABPDIR')
		IF (IAND(IS,1).EQ.0) GOTO 993
		EXIST=WNFOP(FCAT,SAVFIL(:LSV),'R')
		IF (.NOT.EXIST) THEN
		  CALL WNCTXT(DWLOG,'File !AS not found',SAVFIL(:LSV))
		  GOTO 993
		END IF
	ELSE
	   CALL WNCTXT(DWLOG,'Using !AS from HOME directory',SAVFIL)
	END IF
	CALL WNFCL(FCAT)
C
C				Refresh external defaults
C
	TMP=DWFNAM(:LD)//'$0'
	OK = ABP_CLEAR (TMP)
	TMP=DWFNAM(:LD)//STREAM(:LS)
	IF (OK) OK = ABP_CLEAR (TMP)
	IF (OK) OK = ABP_RESTORE (TMP,SAVFIL(:LSV))
	IF (.NOT.OK) GOTO 990
C
C				Open PPD file
C
	IS = PPD_INIT (DWFNAM(:LD))
	IF (IAND(IS,1).EQ.0) THEN
		CALL WNCTXT(DWLOG,'!_!AS.PPD',DWFNAM(:LD))
		GOTO 999
	END IF
C
C
	ABP_RUN_INIT = .TRUE.
	RETURN
C
 990	CALL WNCTXT(DWLOG,'No symbols read from stream...')
	GOTO 999
 991	CALL WNCTXT(DWLOG,'File !AS not found',EXEFIL(:LE))
	GOTO 999
 992	CALL WNCTXT(DWLOG,'Stream name is missing')
	GOTO 999
 993	CALL WNCTXT(DWLOG,'Invalid stream: !AS',XSTREAM(:LX))
	GOTO 999
 999	IS = MSG_SET(IS,0)
	ABP_RUN_INIT = .FALSE.
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	LOGICAL FUNCTION ABP_RUN_DO (DWFNAM,STREAM,EXEFIL,RUNMODE)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	DWFNAM		!(i) DWARF program name
	CHARACTER*(*)	STREAM		!(i) stream name (with $ prefix)
	CHARACTER*(*)	EXEFIL		!(i) expanded spec of executable
	CHARACTER*(*)	RUNMODE		!(i) run mode
C
C.Purpose:	Run the program in the given stream
C.Returns:	.TRUE. for success, .FALSE. otherwise
C.Notes:
C	- If RUNMODE is DRYRUN: only close the symbol facility.
C-------------------------------------------------------------------------
C
	INTEGER		STR_SIGLEN
	INTEGER		PPD_EXIT
	INTEGER		SYMBOL_DEFINE, SYMBOL_EXIT
C
	CHARACTER*255	SYMVAL,TMP*80,TMP2*80
	INTEGER		IS, LD, LS, LV, LE
C
C
	IS = PPD_EXIT ()
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (RUNMODE.EQ.'DRYRUN') THEN
		IS = SYMBOL_EXIT ()			!keep symbol def
		IF (IAND(IS,1).EQ.0) GOTO 999
		ABP_RUN_DO = .TRUE.
		RETURN
	END IF
C					Create symbol DWARF_<dwfnam>_CONTROL
C					- containing ASK, SAVE, TEST switches
C					- stream length and name
C					- zero length (no input file)
C
	LD = STR_SIGLEN (DWFNAM)
	LS = STR_SIGLEN (STREAM)
	LE = STR_SIGLEN (EXEFIL)
	IF (RUNMODE.EQ.'DEFINE') THEN
		SYMVAL = '221  '//STREAM(:LS)//'00'	!do ASK and SAVE
	ELSE
		SYMVAL = '111  '//STREAM(:LS)//'00'	!don't
	END IF
	WRITE (SYMVAL(4:5),'(I2.2)') LS
	LV = 3+2+LS+2
	TMP='DWARF_'//DWFNAM(:LD)//'_CONTROL'
	IS = SYMBOL_DEFINE (TMP,SYMVAL(:LV),
	1						DWC__LOCALSYM)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Run the DWARF program
C					Append a zero for the C-routine EXECL
C
	E_C = IS					!Set exit status
	IS = SYMBOL_EXIT ()				!keep symbol def
	IF (IAND(IS,1).EQ.0) GOTO 999
	TMP=EXEFIL(:LE)//CHAR(0)
	TMP2=DWFNAM(:LD)//CHAR(0)
	CALL GEN_EXECL (TMP, TMP2)	!run
	ABP_RUN_DO = .TRUE.
	RETURN
C
C					Error section
C
 999	ABP_RUN_DO = .FALSE.
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	LOGICAL FUNCTION ABP_CLEAR (PROGSTRM)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	PROGSTRM	!(i) DWARF program name and stream
C
C.Purpose:	Clear the external defaults
C.Returns:	.TRUE. for success, .FALSE. otherwise
C.Notes:
C-------------------------------------------------------------------------
C
	CHARACTER*(*)	BLANK
		PARAMETER (BLANK = ' ')
C
	INTEGER		SYMBOL_SEARCH, SYMBOL_DELETE
C
	CHARACTER	NAM*64,TMP*80
	INTEGER		IS, LN, NR
C
C
	NR = 0
	LN = 1
	TMP= PROGSTRM//'_*'
	DO WHILE (LN.GT.0)
		IS = SYMBOL_SEARCH (TMP,BLANK,NR,NAM,LN)
		IF (IAND(IS,1).EQ.0) GOTO 999
		IF (LN.GT.0) IS = SYMBOL_DELETE (NAM(:LN),DWC__GLOBALSYM)
		IF (IAND(IS,1).EQ.0) GOTO 999
	END DO
C
	ABP_CLEAR = .TRUE.
	RETURN
C
 999	ABP_CLEAR = .FALSE.
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	LOGICAL FUNCTION ABP_RESTORE (PROGSTRM,SAVFILE)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	PROGSTRM	!(i) DWARF program name and stream
	CHARACTER*(*)	SAVFILE		!(i) save file
C
C.Purpose:	Restore parameter defaults from file
C.Returns:	.TRUE. for success, .FALSE. otherwise
C.Notes:
C	- The external defaults for the program in the given stream are
C	  restored from the save file.
C	- The save file must have been created via DWARF's SAVE command. It
C	  then contains definition lines of the form
C		#<progstrm>_<keyword>="<value>"
C	  where # indicates a blank space,
C	- Only defaults with the proper <progstrm> are restored.
C	- All other lines are ignored.
C	- Temporarily the old-style save files are still readable.
C-------------------------------------------------------------------------
C
	CHARACTER*(*)	BLANK, UNDERSC, ASTER, EQUAL, QUOTE
		PARAMETER (BLANK   = ' ')
		PARAMETER (UNDERSC = '_')
		PARAMETER (ASTER   = '*')
		PARAMETER (EQUAL   = '=')
		PARAMETER (QUOTE   = '"')
	CHARACTER*(*)	ANUMX
		PARAMETER (ANUMX = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_')
C
	INTEGER		GEN_FORIOS
	INTEGER		SYMBOL_DEFINE
	INTEGER		STR_COPY, STR_COPY_W
	INTEGER		WNCALN
C
	CHARACTER	LINE*512, DEFSTART*25, NAM*64, VALUE*255
	INTEGER		LP, LL, LD, LN, LV
	INTEGER		IS, LUN, PTR
	LOGICAL		NEW_STYLE
	INTEGER		NDEF
C
C
	LP = WNCALN(PROGSTRM)
	NDEF=0
C
C					Open the save file and check the first
C					line to see whether it is an old- or
C					new-style save file
C					- old files start with a .GLOBAL line
C
	CALL WNGLUN(LUN)
	IF (LUN.EQ.0) GOTO 999
	OPEN (LUN,NAME=SAVFILE,STATUS='OLD',ERR=998)
	READ (LUN,'(Q,A)',END=900,ERR=997) LL, LINE
	NEW_STYLE = LINE.NE.'.GLOBAL'
	IF (NEW_STYLE) THEN
		DEFSTART = BLANK//PROGSTRM(:LP)//UNDERSC
		LD = LP+2
		GOTO 101
	ELSE
	        CALL WNCTXT(DWLOG,
	1	  'OLD-STYLE save file, how did you ever get it?')
		DEFSTART = BLANK//BLANK//PROGSTRM(:LP)//UNDERSC
		LD = LP+3
	ENDIF
C
C					Read and check the next line
C					- skip non-definition lines
C
 100	READ (LUN,'(Q,A)',END=900,ERR=997) LL, LINE
 101	IF (LL.GT.0) LL = WNCALN(LINE(:LL))
	IF (NEW_STYLE) THEN
		IF (LL.LT.LD+5) GOTO 100		!too short a line
		IF (LINE(:LD).NE.DEFSTART(:LD)) GOTO 100!wrong start
		PTR = 2					!skip to start name
	ELSE
		IF (LL.LT.LD+8) GOTO 100
		IF (LINE(:LD).NE.DEFSTART(:LD)) GOTO 100
		PTR = 3
	END IF
	IF (LINE(LL:LL).NE.QUOTE) GOTO 100		!wrong end
C
C					- extract the name
C					- remove a possible abbreviation
C					  character (asterisk)
C
	NAM = DEFSTART(PTR:LD)				!fill first part name
	LN = LD-PTR+1
	PTR = LD+1					!point to start of key
	IS = STR_COPY_W (ANUMX,LINE(:LL),PTR,NAM,LN)	!extract rest
	IF (PTR.LT.LL .AND. LINE(PTR:PTR).EQ.ASTER) THEN!asterisk encountered:
		PTR = PTR+1				! skip it
		IS = STR_COPY_W (ANUMX,LINE(:LL),PTR,NAM,LN)!extract rest name
	END IF
C
C					- extract the value (must be quoted)
C
	IF (NEW_STYLE) THEN
		IF (PTR+2.GE.LL) GOTO 100		!too short aline
		IF (LINE(PTR:PTR+1).NE.EQUAL//QUOTE)
	1			GOTO 100		!value not quoted
		PTR = PTR+2
	ELSE
		IF (PTR+5.GE.LL) GOTO 100
		IF (LINE(PTR:PTR+4).NE.BLANK//EQUAL//EQUAL//BLANK//QUOTE)
	1			GOTO 100
		PTR = PTR+5
	END IF
	LV = 0						!clear value
	IS = STR_COPY (LINE(PTR:LL-1),VALUE,LV)		!extract value
	IF (IS.LT.0) GOTO 100				!too long: skip
C
C					Define the symbol
C
	IS = SYMBOL_DEFINE (NAM(:LN),VALUE(:LV),DWC__GLOBALSYM)
	IF (IAND(IS,1).EQ.0) GOTO 996
	NDEF=NDEF+1
	GOTO 100
C
C					Wrap up
C
 900	CLOSE (UNIT=LUN,ERR=998)
	CALL WNGLUF(LUN)
	IF (NDEF.EQ.0) THEN
	   CALL WNCTXT(DWLOG,'No symbols defined at all....')
	   ABP_RESTORE = .FALSE.
	ELSE
	   CALL WNCTXT(DWLOG,'!UJ symbols defined....',NDEF)
	   ABP_RESTORE = .TRUE.
	END IF
	RETURN
C
 997	IS = GEN_FORIOS (SAVFILE)			!read error
 996	CLOSE (UNIT=LUN,ERR=998)			!processing error
	CALL WNGLUF(LUN)
	GOTO 999
 998	IS = GEN_FORIOS (SAVFILE)			!open or close error
	CALL WNGLUF(LUN)
 999	ABP_RESTORE = .FALSE.				!setup error
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	LOGICAL FUNCTION ABP_RUN_SPEC (DWFNAM,STREAM,SW,ANAME,PNAME,NRARG)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	DWFNAM		!(i) DWARF program name
	CHARACTER*(*)	STREAM		!(i) stream name (with $ prefix)
	INTEGER		SW(*)		!(i) 0=ignore, 1=required, 2=optional
	CHARACTER*(*)	ANAME(*)	!(i) names as known to ABP
	CHARACTER*(*)	PNAME(*)	!(i) names as known to PPD
	INTEGER		NRARG		!(i) #arguments in above arrays
C					     The first 2 can be ignored
C					     (they are STREAM and RUNMODE)
C
C
C.Purpose:	Get and test qualifier values and 'specify' them
C.Returns:	.TRUE. for success, otherwise .FALSE.
C.Notes:
C	- The qualifier values are read from CLI and it is tested if they
C	  should have a value.
C	- Each qualifier is handled by SP_LIST_KEY to define its symbol
C	- SP_LIST_KEY cannot be called before all values are read from the CLI.
C	  This is because SP_DEF_CHECK in it resets the CLI.
C-------------------------------------------------------------------------
C
	INTEGER		CLI_GET
	INTEGER		SP_LIST_KEY
C
	CHARACTER	WORK*512
	INTEGER		IS, LW, LWT
	INTEGER		STW(20), SEW(20)
C
C
C				Loop through all qualifiers.
C				Ignore if the switch is 0.
C				First assemble all values in a string.
C				This is necessary because SP_DEF_CHECK resets
C				the CLI, so CLI_GET would lost its context.
C
	LWT = 1
	DO I = 3,NRARG
	    STW(I) = LWT			!start of value in string
	    SEW(I) = 0				!end of value
	    IF (SW(I) .NE. 0) THEN
		IS = CLI_GET (ANAME(I), WORK(LWT:), LW)		!no ignore
		IF (IAND(IS,1).EQ.0) GOTO 999
		IF (LW .EQ. 0  .OR.  WORK(LWT:LWT+LW-1) .EQ. '""') THEN
		    IF (SW(I) .EQ. 1) THEN
			CALL WNCTXT(F_TP,
	1		   'Qualifier !AS missing',ANAME(I))
			GOTO 999
		    ENDIF
		ELSE
		    LWT    = LWT + LW		!update string length
		    SEW(I) = LWT - 1		!save value end
		ENDIF
	    ENDIF
	ENDDO
C
C					Now define all the values.
C
	DO I = 3,NRARG
	    IF (SEW(I) .NE. 0) THEN
		IS = SP_LIST_KEY (DWFNAM,STREAM,PNAME(I),WORK(STW(I):SEW(I)))
		IF (IAND(IS,1).EQ.0) GOTO 999
	    ENDIF
	ENDDO
C
C
 900	ABP_RUN_SPEC = .TRUE.
	RETURN
C
 999	ABP_RUN_SPEC = .FALSE.
	RETURN
	END
