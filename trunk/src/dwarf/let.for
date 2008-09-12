C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	SYS_LET
C.Keywords:	Utility Program, Define DCL Symbols
C.Author:	Ger van Diepen (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	820620 GVD - creation
C.Version:	881204 FMO - improved layout, use new MSG and CLI routines
C.Version:	890423 FMO - use SYMBOL-DEFINE i.s.o. DWC_SETSYM
C.Version:	900228 FMO - use DWC_CTL_OPEN to get DWARF control
C.Version:	900416 FMO - use logical*4 only
C.Version:	910813 FMO - remove LIB$DO_COMMAND, add SYMBOL_EXIT call,
C			add /LOG qualifier
C.Version:	920206 GvD - add former optional arguments to DWC_INPUT
C.Version:	940121 CMV - changed messenger
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	SUBROUTINE LET
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C.Purpose:	Define arbitrary global DCL symbols
C.Returns:	Not applicable
C.Notes:
C	- Parameter:
C		name=value		optional
C	- Qualifier (the names can be abbreviated to a single letter):
C		/LOG=LONG or /NOLOG	default: /LOG=SHORT
C
C	- If you do not give a parameter, you will be prompted for definitions
C	  until you answer with #.
C	- The parameter (or input line) will be compressed as follows:
C	  blanks and tabs that are not part of a quoted substring, are removed.
C	- The symbol names shouldn't contain special characters and they
C	  shouldn't be reserved names.
C	- LET SYMBOL=VALUE is equivalent to DCL's SYMBOL:==VALUE.
C	- /LOG=L reports each individual symbol definition and the total nr of
C	  symbols defined, /LOG=S only reports the total number, and /NOLOG
C	  reports nothing.
C-------------------------------------------------------------------------
C
	CHARACTER*(*)	PROGNAME
		PARAMETER (PROGNAME = 'LET')
C
	INTEGER		NRARG, X, QVD
		PARAMETER (NRARG = 2)
		PARAMETER (X   = CLI__EXPRESSION)
		PARAMETER (QVD = CLI__QUALIFIER+CLI__VALUE+CLI__DEFAULT)
	CHARACTER*7	NAME(NRARG)
	INTEGER		ATTR(NRARG)
	CHARACTER*1	PROMPT(NRARG)
	CHARACTER*5	DEFVAL(NRARG)
		DATA NAME   /'SYMDEF','LOG'  /
		DATA ATTR   / X      , QVD   /
		DATA PROMPT /' '     ,' '    /
		DATA DEFVAL /' '     ,'SHORT'/
C
	INTEGER		CLI_INIT, CLI_GET
	INTEGER		MSG_INIT, MSG_SET
	INTEGER		DWC_CTL_OPEN, DWC_INPUT, DWC_TSTSYM
	INTEGER		STR_COLLAPS
	INTEGER		SYMBOL_DEFINE, SYMBOL_EXIT
C
	CHARACTER*255	LINE, VALUE
	INTEGER		IS, LL, LV, TMP, NRDEF
	LOGICAL		DO_ASK, LONG_LOG, SHORT_LOG
		DATA NRDEF     /0/
		DATA LONG_LOG  /.FALSE./
		DATA SHORT_LOG /.TRUE./
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
	IF (IAND(IS,1).EQ.0) GOTO 900
C
C					Interpret the command line
C
	IS = CLI_GET ('SYMDEF',LINE,LL)
	IF (IAND(IS,1).EQ.0) GOTO 900
	DO_ASK = LL.EQ.0
C
	IS = CLI_GET ('LOG',VALUE,LV)
	IF (IAND(IS,1).EQ.0) GOTO 900
	IF (LV.EQ.0) THEN
		SHORT_LOG = .FALSE.
	ELSE IF (VALUE(1:1).EQ.'L') THEN
		LONG_LOG = .TRUE.
	END IF
C
C					If no definition is given, the user
C					will be asked for it (repeatedly);
C					input is taken from SYS$INPUT
C
 100	IF (DO_ASK) THEN
		IS = DWC_INPUT (LINE,'Symbol Definition',LL,0,0)
		IF (IAND(IS,1).EQ.0) GOTO 900			!CTRL/Z, '#' or error
		IF (LL.EQ.0) GOTO 900			!blank answer
		IF (LINE(:LL).EQ.'EOD') GOTO 900	!'EOD' answer
		IF (LINE(:1).EQ.'$') GOTO 900		!DCL line
	END IF
C
C					Check and act upon the definition
C					- remove blanks and tabs that are
C					  not part of a quoted substring
C					- check the syntax
C					- define the symbol
C
	LL = STR_COLLAPS (LINE)
	I = INDEX(LINE,'=')
	IF (I.GT.1 .AND. I.LT.LL) THEN
		IS = DWC_TSTSYM (LINE(:I-1))
		IF (IAND(IS,1).EQ.0) THEN
		ELSE
		    IS = SYMBOL_DEFINE (LINE(:I-1),LINE(I+1:LL),DWC__GLOBALSYM)
		    IF (IAND(IS,1).NE.0) THEN
			NRDEF = NRDEF+1
			IF (LONG_LOG) CALL WNCTXT(DWLOG,
	1		    'Global symbol !AS is defined with value !AS',
	2		     LINE(:I-1),LINE(I+1:LL))
		    END IF
		END IF
	ELSE
		CALL WNCTXT(DWLOG,'No symbol name, = or value given')
	END IF
	IF (DO_ASK) GOTO 100
C
 900	IF (SHORT_LOG) CALL WNCTXT(DWLOG,'!SJ symbols defined',NRDEF)
	IF (NRDEF.GT.0) TMP = SYMBOL_EXIT ()
	E_C = MSG_SET(IS,0)		! Exit code for WNGEX
	END
