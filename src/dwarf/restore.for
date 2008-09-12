C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	SYS_RESTORE
C.Keywords:	DWARF Environment, Restore
C.Author:	Johan Hamaker (NFRA, Dwingeloo)
C.Language:	VAX-Fortran
C.Environment:	VAX
C.Comments:
C.Version:	870407 JPH - creation RSTENVRMT.FOR
C.Version:	900108 FMO - new name, new code; do not overwrite existing
C			definitions
C.Version:	900227 FMO - new LNM routine used
C.Version:	900502 FMO - new GEN_LUN module
C.Version:	910117 FMO - add /NOLOG qualifier
C.Version:	910124 FMO - correct small error
C.Version:	910508 FMO - use CLI and set proper exit status, suppress
C			logging (start/end-message problem)
C.Version:	910814 FMO - Separate program for full-environment restore
C			(called ENVRESTORE.FOR). This program only restores
C			proper DWARF symbols.
C			- Simplify parsing (assume standard format as produced
C			by old or new SAVE commands; old ones start with
C			a .GLOBAL line)
C			- Added various qualifiers
C.Version:	920206 GvD - add former optional arguments to CLI_GET/DWC_INPUT
C.Version:	940119 CMV - use WNGLUN i.s.o GEN_LUN
C.Version:	940315 CMV - default extension now .par
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	SUBROUTINE RESTORE
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C.Purpose:	Implement DWARF's RESTORE command
C.Returns:	Not applicable
C.Notes:
C	- Parameter:
C		Save_file		required
C	- Qualifiers (the names can be abbreviated to a single letter):
C		/INCLUDE=list		default: /INCLUDE=*$*_*
C		/EXCLUDE=list		default: /NOEXCLUDE
C		/LOG=LONG or /NOLOG	default: /LOG=SHORT
C		/CONFIRM		default: /NOCONFIRM
C		/OVERWRITE		default: /NOOVERWRITE
C	- All new symbols defined in the save file with names matching the
C	  include list but not the exclude list will be defined. Existing
C	  symbols may be redefined if the /OVERRIDE qualifiers is given.
C	- All lines in the file that do not have the proper format for a symbol
C	  definition, are just ignored. This opens many ways to add comments.
C
C	- The symbol lists (/INCLUDE and /EXCLUDE values) are comma-separated
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
C	- /CONFIRM will ask for you to confirm each individual restore action;
C	  the qualifier will be ignored in batch mode.
C	- /OVERWRITE will also redefine already existing symbols.
C	- /LOG=L reports each individual restore action and the total nr of
C	  symbols restored, /LOG=S only reports the total number, and /NOLOG
C	  reports nothing.
C-------------------------------------------------------------------------
C
	CHARACTER*(*)	PROGNAME, BLANK, ASTER, EQUAL, QUOTE
		PARAMETER (PROGNAME= 'RESTORE')
		PARAMETER (BLANK   = ' ')
		PARAMETER (ASTER   = '*')
		PARAMETER (EQUAL   = '=')
		PARAMETER (QUOTE   = '"')
	CHARACTER*(*)	ANUMX
		PARAMETER (ANUMX = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789$_')
	INTEGER		NRARG, PR, Q, QV, QVD
		PARAMETER (NRARG = 6)
		PARAMETER (PR    = CLI__EXPRESSION+CLI__REQUIRED)
		PARAMETER (Q     = CLI__QUALIFIER)
		PARAMETER (QV    = CLI__QUALIFIER+CLI__VALUE)
		PARAMETER (QVD   = CLI__QUALIFIER+CLI__VALUE+CLI__DEFAULT)
	CHARACTER*9	NAME(NRARG)
	INTEGER		ATTR(NRARG)
	CHARACTER*26	PROMPT(NRARG)
	CHARACTER*5	DEFVAL(NRARG)
		DATA NAME   /'SAVFILE'
	1		,'INCLUDE','EXCLUDE','LOG'  ,'CONFIRM','OVERWRITE'/
		DATA ATTR   / PR
	1		, QVD     , QV      , QVD   , Q       , Q         /
		DATA PROMPT /'Save file (default ext .par)'   ,5*BLANK    /
		DATA DEFVAL /BLANK
	1		,'*$*_*'  ,BLANK    ,'SHORT',BLANK    ,BLANK      /
C
	INTEGER		DWC_CTL_OPEN, DWC_CTL_FILL
	INTEGER		DWC_IBMODE_INQ, DWC_INPUT, DWC_SYMLIST_EXPAND
	INTEGER		CLI_INIT, CLI_GET, FILNAM_FULL
	INTEGER		MSG_INIT, MSG_SET
	INTEGER		GEN_FORIOS
	INTEGER		SYMBOL_GET, SYMBOL_DEFINE, SYMBOL_EXIT
	INTEGER		STR_SIGLEN, STR_COPY, STR_COPY_W, STR_MATCH_L
C
	CHARACTER*255	VALUE, INCLIST, EXCLIST, WORK, LINE*512
	CHARACTER	NAM*64, SAVFILE*80, YN*1, DUM*1
	INTEGER		LV, LI, LE, LW, LL, LN, LF, LDUM
	INTEGER		IS, TMP, LUN, PTR, NRDEF, MATCHNR
	LOGICAL		NEW_STYLE, NEW_DWARF, DO_CONFIRM, DO_OVERWRITE
	LOGICAL		DO_RESTORE, LONG_LOG, SHORT_LOG
		DATA NRDEF        /0/
		DATA NEW_DWARF    /.FALSE./
		DATA DO_CONFIRM   /.FALSE./
		DATA DO_OVERWRITE /.FALSE./
		DATA LONG_LOG     /.FALSE./
		DATA SHORT_LOG    /.TRUE./
C
C
C					Initialize
C					- get DWARF control variables
C					- start messenger
C					- initialize command-line interpreter
C
	IS = DWC_CTL_OPEN ()				! ignore false return
	IS = MSG_INIT (PROGNAME,F_T)
	IF (IAND(IS,1).NE.0) IS = CLI_INIT (NRARG,NAME,ATTR,PROMPT,DEFVAL)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Interpret the command line
C					- get full save-file specification
C					- get and expand symbol list
C					- get and expand exclude list
C					- get confirm qualifier
C					- get overwrite qualifier
C					- get log qualifier
C
	IS = CLI_GET ('SAVFILE',VALUE,LV)
	IF (IAND(IS,1).NE.0)
	1		IS = FILNAM_FULL (VALUE(:LV),SAVFILE,LF,'.PAR')
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	IS = CLI_GET ('INCLUDE',VALUE,LV)
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
	END IF
C
	IS = CLI_GET ('OVERWRITE',DUM,LDUM)
	IF (IAND(IS,1).EQ.0) GOTO 999
	DO_OVERWRITE = IS.EQ.DWC_PRESENT
C
	IS = CLI_GET ('LOG',VALUE,LV)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (LV.EQ.0) THEN
		SHORT_LOG = .FALSE.
	ELSE IF (VALUE(1:1).EQ.'L') THEN
		LONG_LOG = .TRUE.
	END IF
C
C					Open the save file and check the first
C					line to see whether it is an old- or
C					new-style save file
C					- old files start with a .GLOBAL line
C
	CALL WNGLUN(LUN)
	IF (LUN.EQ.0) THEN
	   IS = GEN_LUNNOFREE
	   GOTO 999
	END IF
	OPEN (LUN,NAME=SAVFILE(:LF),STATUS='OLD',ERR=998)
	READ (LUN,'(Q,A)',END=900,ERR=998) LL, LINE
	NEW_STYLE = LINE.NE.'.GLOBAL'
	IF (NEW_STYLE) GOTO 101
C
C					Read and check the next line
C					- skip non-definition lines
C
 100	READ (LUN,'(Q,A)',END=900,ERR=998) LL, LINE
 101	IF (LL.GT.0) LL = STR_SIGLEN (LINE(:LL))
	IF (NEW_STYLE) THEN
		IF (LL.EQ.10) GOTO 100			!too short a line
		IF (LINE(1:1).NE.BLANK) GOTO 100	!wrong start
		PTR = 2					!skip to start name
	ELSE
		IF (LL.EQ.14) GOTO 100
		IF (LINE(1:2).NE.BLANK) GOTO 100
		PTR = 3
	END IF
	IF (LINE(LL:LL).NE.QUOTE) GOTO 100		!wrong end
C
C					- extract the name and check against
C					  the include and exclude lists
C					- remove a possible abbreviation
C					  character (asterisk)
C
	LN = 0						!clear name
	IS = STR_COPY_W (ANUMX,LINE(:LL),PTR,NAM,LN)	!extract name
	IF (PTR.LT.LL .AND. LINE(PTR:PTR).EQ.ASTER) THEN!asterisk encountered:
		PTR = PTR+1				! skip it
		IS = STR_COPY_W (ANUMX,LINE(:LL),PTR,NAM,LN)!extract rest name
	END IF
	IF (LN.EQ.0) GOTO 100				!no name: skip
	IS = STR_MATCH_L (NAM(:LN),INCLIST(:LI),MATCHNR)!included ?
	IF (IS.NE.1) GOTO 100				!no: skip
	IF (EXCLIST(:LE).NE.BLANK) THEN			!excluded ?
		IS = STR_MATCH_L (NAM(:LN),EXCLIST(:LE),MATCHNR)
		IF (IS.EQ.1) GOTO 100			!yes: skip
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
C					Possibly restore the symbol:
C					- get current symbol value (if any)
C					- check overwrite if necessary
C					- ask confirmation (if active)
C					- define the symbol
C					- increment counter and log (if active)
C
	IS = SYMBOL_GET (NAM(:LN),WORK,LW)		!get current value
	IF (IAND(IS,1).EQ.0) THEN
	   IS = MSG_SET(IS,0)
	   GOTO 999
	END IF
	IF (LW.EQ.0) THEN				!no current symbol:
		DO_RESTORE = .TRUE.			! restore
	ELSE IF (WORK(:LW).EQ.VALUE(:LV)) THEN		!no new definition:
		DO_RESTORE = .FALSE.			! do not restore
	ELSE IF (DO_OVERWRITE) THEN			!overwrite allowed:
		DO_RESTORE = .TRUE.			! restore
	ELSE						!otherwise:
		DO_RESTORE = .FALSE.			! do not restore
	END IF
	IF (DO_RESTORE .AND. DO_CONFIRM) THEN
		IF (LW.EQ.0) THEN
			IS = DWC_INPUT (YN,NAM(:LN)//' = '//VALUE(:LV)//
	1		', restore this symbol? (Y,[N])',LDUM,1,0)
		ELSE
			CALL WNCTXT(DWLOG,'Currently: !AS = !AS',
	1				NAM(:LN),WORK(:LW))
			IS = DWC_INPUT (YN,'	overwrite with value '//
	1				VALUE(:LV)//'? (Y,[N])',LDUM,1,0)
		END IF
		IF (IAND(IS,1).EQ.0) YN = 'N'
		DO_RESTORE = YN.EQ.'Y'
	END IF
	IF (DO_RESTORE) THEN
		IS = SYMBOL_DEFINE (NAM(:LN),VALUE(:LV),DWC__GLOBALSYM)
		IF (IAND(IS,1).EQ.0) GOTO 999
		NRDEF = NRDEF+1
		IF (LONG_LOG) CALL WNCTXT(DWLOG,
	1		'Symbol !AS = !AS is restored',NAM(:LN),VALUE(:LV))
		IF (NAM(:8).EQ.'DWARF$0_') NEW_DWARF = .TRUE.
	END IF
	GOTO 100
C
C					Wrap up
C					- define DWARF control symbol
C					- report
C					- close symbol facility
C
 998	IS = GEN_FORIOS (SAVFILE(:LF))			!open or read error
 900	IF (NEW_DWARF) TMP = DWC_CTL_FILL ()
 999	CONTINUE
	IF (SHORT_LOG) CALL WNCTXT(DWLOG,
	1	'!SJ symbols restored from file !AS',NRDEF,SAVFILE(:LF))
	IF (NRDEF.GT.0) TMP = SYMBOL_EXIT ()
	E_C=MSG_SET(IS,0)		!WNGEX exit code
	END
