C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	SYS_SAVE
C.Keywords:	Program Parameters, External Defaults, Save
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	850315 JPH - creation SYSDWARF:SAVE.COM
C.Version:	850522 JPH - suppress control symbols; confirm execution via
C			editor (writing directly wouldn't work)
C.Version:	850531 JPH - assume dummy P1 parameter in SAVE symbol
C			(This makes it possible to specify SAVE /<qualifiers>
C			without any parameter.)
C.Version:	850617 JPH - insert .GLOBAL in DWARF defaults save file
C.Version:	851015 JPH - deassign DWF_SLATE which for unknown reasons
C			sometimes remains after a spawn
C.Version:	900108 FMO - new code; use wild cards in SHOW SYMBOL command;
C			don't remove DWARF$0_IBMODE and DWARF$0_IDENT
C			(RESTORE will suppress their restoration)
C.Version:	910813 FMO - recreation as a fortran program
C			- only save DWARF symbols (default list *$*_*)
C			- added qualifiers /EXCLUDE and /LOG
C.Version:	920206 GvD - add former optional arguments to CLI_GET/DWC_INPUT
C.Version:	920318 GvD - open save file as UNKNOWN, because NEW fails
C			on DECstation if already existing
C.Version:	930923 CMV - logical names for new maintenance system
C.Version:	940119 CMV - use WNGLUN i.s.o GEN_LUN
C.Version:	940315 CMV - default output name now: dwarf.par
C.Version:	940628 CMV - add CLOSE for HP
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	SUBROUTINE SAVE
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C.Purpose:	Save the specified external defaults in a file
C.Returns:	Not applicable
C.Notes:
C	- Parameter:
C		symbol_list		default *$*_*
C	- Qualifiers (the names can be abbreviated to a single letter):
C		/OUTPUT=file		default: /OUTPUT=[]DWARFSAVE.PAR
C		/CONFIRM		default: /NOCONFIRM
C		/EXCLUDE=list		default: /NOEXCLUDE
C		/LOG=LONG or /NOLOG	default: /LOG=SHORT
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
C	- If an incomplete output file specification is given, the missing
C	  components will be taken from the default file specification.
C	- /CONFIRM will ask for you to confirm each individual save action;
C	  the qualifier will be ignored in batch mode.
C	- /LOG=L reports each individual save action and the total nr of
C	  symbols saved, /LOG=S only reports the total number, and /NOLOG
C	  reports nothing.
C-------------------------------------------------------------------------
C
	CHARACTER*(*)	PROGNAME, BLANK
		PARAMETER (PROGNAME = 'SAVE')
		PARAMETER (BLANK    = ' '   )
C
	INTEGER		NRARG, P, Q, QV, QVD
		PARAMETER (NRARG = 5)
		PARAMETER (P     = CLI__PARAMETER)
		PARAMETER (Q     = CLI__QUALIFIER)
		PARAMETER (QV    = CLI__QUALIFIER+CLI__VALUE)
		PARAMETER (QVD   = CLI__QUALIFIER+CLI__VALUE+CLI__DEFAULT)
	CHARACTER*7	NAME(NRARG)
	INTEGER		ATTR(NRARG)
	CHARACTER*12	PROMPT(NRARG)
	CHARACTER*13	DEFVAL(NRARG)
		DATA NAME   /'INCLIST','EXCLUDE','CONFIRM','LOG'  ,'OUTPUT'/
		DATA ATTR   / P       , QV      , Q       , QVD   , QVD    /
		DATA PROMPT /' '      ,' '      ,' '      ,' '    ,' '     /
		DATA DEFVAL /'*$*_*'  ,' '      ,' ','SHORT',   'dwarf.par'/
C
	INTEGER		CLI_INIT, CLI_GET, FILNAM_FULL
	INTEGER		MSG_INIT, MSG_SET
	INTEGER		DWC_CTL_OPEN, DWC_IBMODE_INQ, DWC_INPUT
	INTEGER		DWC_SYMLIST_EXPAND, SYMBOL_SEARCH, SYMBOL_GET
	INTEGER		GEN_FORIOS
C
	CHARACTER*255	VALUE, INCLIST, EXCLIST
	CHARACTER	NAM*64, FILE*80, YN*1, DUM*1
	INTEGER		LV, LI, LE, LN, LF, LDUM
	INTEGER		IS, TMP, NRSAVE, NR, LUN
	LOGICAL		DO_SAVE, DO_CONFIRM, LONG_LOG, SHORT_LOG
		DATA NRSAVE     /0/
		DATA DO_SAVE    /.TRUE./
		DATA DO_CONFIRM /.FALSE./
		DATA SHORT_LOG  /.TRUE./
		DATA LONG_LOG   /.FALSE./
C
C
C					Initialize
C					- get DWARF control variables
C					- start messenger
C					- initialize command-line interpreter
C
	LUN=0
	IS = DWC_CTL_OPEN ()
	IF (IAND(IS,1).NE.0) IS = MSG_INIT (PROGNAME,F_T)
	IF (IAND(IS,1).NE.0) IS = CLI_INIT (NRARG,NAME,ATTR,PROMPT,DEFVAL)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Interpret the command line
C					- get and expand symbol list
C					- get and expand exclude list
C					- get confirm qualifier
C					- get log qualifier
C					- get output file
C
	IS = CLI_GET ('INCLIST',VALUE,LV)
	IF (IAND(IS,1).NE.0)
	1		IS = DWC_SYMLIST_EXPAND (VALUE(:LV),INCLIST,LI)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	IS = CLI_GET ('EXCLUDE',VALUE,LV)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (LV.EQ.0) THEN
		EXCLIST = BLANK
		LE = 1
	ELSE
		IS = DWC_SYMLIST_EXPAND (VALUE(:LV),EXCLIST,LE)
		IF (IAND(IS,1).EQ.0) GOTO 999
		IF (LE.EQ.0) LE = 1
	END IF
C
	IF (IAND(DWC_IBMODE_INQ('BATCH'),1).EQ.0) THEN
		IS = CLI_GET ('CONFIRM',DUM,LDUM)
		IF (IAND(IS,1).EQ.0) GOTO 999
		DO_CONFIRM = IS.EQ.DWC_PRESENT
	ENDIF
C
	IS = CLI_GET ('LOG',VALUE,LV)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (LV.EQ.0) THEN
		SHORT_LOG = .FALSE.
	ELSE IF (VALUE(1:1).EQ.'L') THEN
		LONG_LOG = .TRUE.
	END IF
C
	IS = CLI_GET ('OUTPUT',VALUE,LV)
	IF (VALUE.EQ.' ') THEN
	   VALUE='dwarfsave'
	   LV=9
	ENDIF
	IF (IAND(IS,1).NE.0)
	1		IS = FILNAM_FULL (VALUE(:LV),FILE,LF,'.par')
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Open the save file
C
	CALL WNGLUN(LUN)
	IF (LUN.EQ.0) GOTO 999
	OPEN (UNIT=LUN,FILE=FILE(:LF),STATUS='UNKNOWN',ERR=998)
	WRITE(LUN,'(A)') '! Written by save.exe'
C
C					Find the next symbol name
C					matching INCLIST but not EXCLIST
C
	NR = 0
	IS = SYMBOL_SEARCH (INCLIST(:LI),EXCLIST(:LE),NR,NAM,LN)
	IF (IAND(IS,1).EQ.0) GOTO 999
	DO WHILE (LN.GT.0)
C
C					- get the symbol value
C					- ask confirmation (if active)
C					- delete the symbol
C					- increment counter and log (if active)
C
		IS = SYMBOL_GET (NAM(:LN),VALUE,LV)
		IF (DO_CONFIRM) THEN
		    IS = DWC_INPUT (YN,NAM(:LN)//' = '//VALUE(:LV)//
	1		', save this symbol? (Y,[N])',LDUM,1,0)
		    IF (IAND(IS,1).EQ.0) YN = 'N'
		    DO_SAVE = YN.EQ.'Y'
		END IF
		IF (DO_SAVE) THEN
		    WRITE (LUN,'(1X,A)',ERR=998)
	1		NAM(:LN)//'="'//VALUE(:LV)//'"'
		    NRSAVE = NRSAVE+1
		    IF (LONG_LOG) CALL WNCTXT(DWLOG,
	1		'Symbol !AS = !AS is saved',NAM(:LN),VALUE(:LV))
		ENDIF
		IS = SYMBOL_SEARCH (INCLIST(:LI),EXCLIST(:LE),NR,NAM,LN)
		IF (IAND(IS,1).EQ.0) GOTO 999
	ENDDO
	GOTO 999
C
C					Wrap up
C					- write any remaining messages
C					- write the nr of symbols saved
C
 998	IS = GEN_FORIOS (FILE(:LF))
 999	IF (LUN.NE.0) CLOSE(LUN)
	IF (SHORT_LOG) CALL WNCTXT(DWLOG,
	1	'!SJ symbols saved in !AS',NRSAVE,FILE)
	E_C = MSG_SET(IS,0) 		!WNGEX exit code
	END
