C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	SYS_CLEAR
C.Keywords:	Program Parameters, External Defaults, Clear
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	900416 FMO - recreation
C.Version:	910813 FMO - recreation, work from list of existing symbols
C.Version:	920206 GvD - add former optional arguments to CLI_GET/DWC_INPUT
C.Version:	940121 CMV - changed messenger
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	SUBROUTINE CLEAR
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Clear the specified external defaults
C.Returns:	Not applicable
C.Notes:
C	- Parameter:
C		Symbol_list		required
C	- Qualifiers (the names can be abbreviated to a single letter):
C		/EXCLUDE=list		default: /NOEXCLUDE
C		/CONFIRM		default: /NOCONFIRM
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
C	- /CONFIRM will ask for you to confirm each individual clear action;
C	  the qualifier will be ignored in batch mode.
C	- /LOG=L reports each individual clear action and the total nr of
C	  symbols cleared, /LOG=S only reports the total number, and /NOLOG
C	  reports nothing.
C-------------------------------------------------------------------------
C
	CHARACTER*(*)	PROGNAME, BLANK
		PARAMETER (PROGNAME = 'CLEAR')
		PARAMETER (BLANK    = ' '    )
C
	INTEGER		NRARG, PR, Q, QV, QVD
		PARAMETER (NRARG = 4)
		PARAMETER (PR    = CLI__PARAMETER+CLI__REQUIRED)
		PARAMETER (Q     = CLI__QUALIFIER)
		PARAMETER (QV    = CLI__QUALIFIER+CLI__VALUE)
		PARAMETER (QVD   = CLI__QUALIFIER+CLI__VALUE+CLI__DEFAULT)
	CHARACTER*7	NAME(NRARG)
	INTEGER		ATTR(NRARG)
	CHARACTER*12	PROMPT(NRARG)
	CHARACTER*5	DEFVAL(NRARG)
		DATA NAME   /'INCLIST'    ,'EXCLUDE' ,'CONFIRM' ,'LOG'  /
		DATA ATTR   / PR          , QV       , Q        , QVD   /
		DATA PROMPT /'Symbol_list',' '       ,' '       ,' '    /
		DATA DEFVAL /' '          ,' '       ,' '       ,'SHORT'/
C
	INTEGER		CLI_INIT, CLI_GET
	INTEGER		MSG_INIT, MSG_SET
	INTEGER		DWC_CTL_OPEN, DWC_IBMODE_INQ, DWC_INPUT
	INTEGER		DWC_SYM_SPLIT, DWC_SYMLIST_EXPAND
	INTEGER		SYMBOL_SEARCH, SYMBOL_GET, SYMBOL_DELETE, SYMBOL_EXIT
C
	CHARACTER*255	VALUE, INCLIST, EXCLIST
	CHARACTER	NAM*64, PROGNAM*9, STREAM*12, KEY*16, YN*1, DUM*1
	INTEGER		LV, LI, LE, LN, LP, LS, LK, LDUM
	INTEGER		IS, TMP, NRCLEAR, NR
	LOGICAL		DO_DELETE, DO_CONFIRM, LONG_LOG, SHORT_LOG, DWARFMSG
		DATA NRCLEAR     /0/
		DATA DO_DELETE   /.TRUE./
		DATA DO_CONFIRM  /.FALSE./
		DATA LONG_LOG    /.FALSE./
		DATA SHORT_LOG   /.TRUE./
		DATA DWARFMSG    /.FALSE./
C
C
C					Initialize
C					- get DWARF control variables
C					- start messenger
C					- initialize command-line interpreter
C
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
	END IF
C
	IS = CLI_GET ('LOG',VALUE,LV)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (LV.EQ.0) THEN
		SHORT_LOG = .FALSE.
	ELSE IF (VALUE(1:1).EQ.'L') THEN
		LONG_LOG = .TRUE.
	END IF
C
C					Find the next symbol name
C					matching INCLIST but not EXCLIST
C
	NR = 0
	IS = SYMBOL_SEARCH (INCLIST(:LI),EXCLIST(:LE),NR,NAM,LN)
	IF (IAND(IS,1).EQ.0) GOTO 999
	DO WHILE (LN.GT.0)
C
C					- DWARF symbols cannot be cleared
C
		IS = DWC_SYM_SPLIT (NAM(:LN),PROGNAM,LP,STREAM,LS,KEY,LK)
		IF (PROGNAM(:LP).EQ.'DWARF') THEN
			IF (.NOT.DWARFMSG) CALL WNCTXT(DWLOG,
	1		'DWARF$0_* symbols cannot be cleared; use SPECIFY')
			DWARFMSG = .TRUE.
C
C					- ask confirmation (if active)
C					- delete the symbol
C					- increment counter and log (if active)
C
		ELSE
			IF (DO_CONFIRM) THEN
			    IS = SYMBOL_GET (NAM(:LN),VALUE,LV)
			    IS = DWC_INPUT (YN,NAM(:LN)//' = '//VALUE(:LV)//
	1			', clear this symbol? (Y,[N])',LDUM,1,0)
			    IF (IAND(IS,1).EQ.0) YN = 'N'
			    DO_DELETE = YN.EQ.'Y'
			END IF
			IF (DO_DELETE) THEN
			    IS = SYMBOL_DELETE (NAM(:LN),DWC__GLOBALSYM)
			    IF (IAND(IS,1).NE.0) THEN
				NRCLEAR = NRCLEAR+1
				IF (LONG_LOG) CALL WNCTXT(DWLOG,
	1			    'Symbol !AS is cleared',NAM(:LN))
			    END IF
			END IF
		END IF
		IS = SYMBOL_SEARCH (INCLIST(:LI),EXCLIST(:LE),NR,NAM,LN)
		IF (IAND(IS,1).EQ.0) GOTO 999
	END DO
C
C					Wrap up
C					- write any remaining messages
C					- write the nr of symbols deleted
C					- close the symbol facility
C 
 999	IF (SHORT_LOG) CALL WNCTXT(DWLOG,'!SJ symbols cleared',NRCLEAR)
	IF (NRCLEAR.GT.0) TMP = SYMBOL_EXIT ()
	E_C = MSG_SET(IS,0)		! Exit code for WNGEX
	END
