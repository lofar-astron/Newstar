C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	SP_LIST
C.Keywords:	Program Parameters, External Defaults, Specify
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	910911 FMO - created
C.Version:	911206 GvD - function was LOGICAL iso. INTEGER
C			split into SP_LIST and SP_LIST_KEY
C.Version:	920214 GvD - no optional arguments in MSG anymore
C.Version:	940223 CMV - Handle quotes properly
C.Version:	010709 AXC - Linux port - Quals(QPTR:QPTR) changed
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER FUNCTION SP_LIST (PROGNAM,STREAM,QUALS)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	PROGNAM		!(i) DWARF program name
	CHARACTER*(*)	STREAM		!(i) stream name (with $ prefix)
	CHARACTER*(*)	QUALS		!(i) line with qualifiers
C
C
C.Purpose:	Define external defaults for program parameters
C.Returns:	Status code (.TRUE. for success, otherwise .FALSE.)
C	success	DWC_SUCCESS
C	error		2
C.Notes:
C	- The qualifier line consists of concatenated elements of the format:
C		/<keyword>=<value_string>
C	  where <keyword> is the user's name for a parameter in the current
C	  program, and where <value_string> must obey the same rules as the
C	  answers to DWARF prompts, except that question marks and value
C	  qualifiers are not allowed.
C	- If QUALS is blank or does not start with a slash, the routine
C	  immediately returns with a success status.
C	- The keywords and values will be checked and if they are correct, the
C	  corresponding DWARF symbols will be defined.
C-------------------------------------------------------------------------
C
C
	CHARACTER*(*)	BLANK, TAB, WHITE, QUOTE
		PARAMETER (BLANK  = ' ')
		PARAMETER (TAB    = '	')
		PARAMETER (WHITE  = BLANK//TAB)
		PARAMETER (QUOTE  = '"')
C
	INTEGER		SP_LIST_KEY
	INTEGER		DWC_STR_STANDARD
	INTEGER		STR_COPY_U, STR_SKIP_W, WNCAL0, WNCALN
C
	CHARACTER	LINE*255, WORK*255, KEY*16
	INTEGER		LW, LL, LK, LQ
	INTEGER		IS, TMP, PTR, QPTR, QPTR2
	LOGICAL		QUOTED
C
C
C				QUALS string must start with /
C
	LQ = WNCAL0(QUALS)
	IF (LQ.EQ.0 .OR. QUALS(1:1).NE.'/') GOTO 900
C
C				Extract next qualifier
C
	QPTR = 1
 100	IF (QPTR.GE.LQ) GOTO 900			!end of QUALS: ready
	QPTR = QPTR+1					!skip slash
	TMP= STR_SKIP_W (WHITE,QUALS(:LQ),QPTR)		!skip leading whites
	LW = 0						!clear work string
C
C	Skip up to the next slash that appears outside quotes
C
	QUOTED=.FALSE.	
	QPTR2=QPTR
	DO WHILE (QPTR.LT.LQ.AND.
	1	(QUALS(QPTR:QPTR).NE.'/'.OR.QUOTED))
	   IF (QUALS(QPTR:QPTR).EQ.QUOTE) THEN
	      QUOTED=.NOT.QUOTED
	   END IF
	   QPTR=QPTR+1
	END DO
	IF (QUALS(QPTR:QPTR).EQ.'/') QPTR=QPTR-1
	IF (QPTR.EQ.QPTR2) GOTO 100			!empty: go for next
	WORK=QUALS(QPTR2:QPTR)
	LW=WNCALN(WORK)
	TMP = DWC_STR_STANDARD (WORK(:LW),LINE,LL)	!standardise string
	IF (LL.EQ.0) GOTO 100				!empty: go for next
C
C				Extract keyword
C
	PTR = 1
	KEY = BLANK
	LK = 0						!clear keyword
	IF (LINE(1:1).EQ.'?') GOTO 991			!no ? allowed
	IS = STR_COPY_U ('=',LINE(:LL),PTR,KEY,LK)	!read keyword
	IF (LK.EQ.0 .OR. PTR.GE.LL) GOTO 991		!key or value missing
	PTR = PTR+1					!skip equal sign
	IF (LINE(PTR:PTR).EQ.BLANK) PTR = PTR+1		!skip blank
C
C				Check and define the value
C				Go for next qualifier
C
	IS = SP_LIST_KEY (PROGNAM,STREAM,KEY,LINE(PTR:LL))
	IF (IAND(IS,1).EQ.0) GOTO 999
	GOTO 100
C
 900	SP_LIST = DWC_SUCCESS
	RETURN
C
 991	IS=2
	CALL WNCTXT(DWLOG,'Incorrect definition syntax: !AS',LINE(:LL))
	GOTO 999
C
 999	SP_LIST = IS
	RETURN
	END
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER FUNCTION SP_LIST_KEY (PROGNAM,STREAM,PKEY,VAL)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	PROGNAM		!(i) DWARF program name
	CHARACTER*(*)	STREAM		!(i) stream name (with $ prefix)
	CHARACTER*(*)	PKEY		!(i) PPD keyword name
	CHARACTER*(*)	VAL		!(i) keyword value
C
C
C.Purpose:	Define external default for program parameter
C.Returns:	Status code (.TRUE. for success, otherwise .FALSE.)
C	success	DWC_SUCCESS
C	error		2
C.Notes:
C	- The keyword and value will be checked and if they are correct, the
C	  corresponding DWARF symbol will be defined.
C-------------------------------------------------------------------------
C
C
	INTEGER		SP_DEF_CHECK
	INTEGER		DWC_SYM_BUILD
	INTEGER		PPD_READ_U, PPD_SSTR_GET, PPD_IOCD_GET
	INTEGER		SYMBOL_DEFINE, WNCALN
C
	CHARACTER	WORK*255, SYMBOL*40, KEY*16
	CHARACTER	SSTR*5, GROUP*9, IOCD*8
	INTEGER		LK, LW, LS, LG, LI, LSYM
	INTEGER		IS, LEVEL
C
C				Read parameter description from PPD
C				- read by user's name
C				- only accept input-type parms
C				  which can have a local default
C
	KEY = PKEY
	LK = WNCALN (PKEY)
	IS = PPD_READ_U (KEY)				!key maybe expanded !!
	IF (IS.EQ.PPD_KEYAMBIG) GOTO 992		!ambiguous abbrev
	IF (IAND(IS,1).EQ.0) GOTO 993				!unknown key
	LK = WNCALN (KEY)
	IS = PPD_IOCD_GET (IOCD,LI)			!get I/O code
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (INDEX('MI',IOCD(:1)).EQ.0) GOTO 994		!no input parameter
	IS = PPD_SSTR_GET (SSTR,LS,GROUP,LG)		!get default types
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (INDEX(SSTR(:LS),'L').EQ.0) GOTO 995		!no local default
C
C				Check the value
C
	LEVEL = 0					!dummy help level
	IS = DWC_SYM_BUILD (PROGNAM,STREAM,KEY(:LK),SYMBOL,LSYM)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IS = SP_DEF_CHECK (SYMBOL(:LSYM),LEVEL,-1,VAL,WORK,LW,.TRUE.)
	IF (IS.EQ.DWC_KEYVAHELP) GOTO 996		!no ? allowed
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C				Define the symbol
C
	IS = SYMBOL_DEFINE (SYMBOL(:LSYM),WORK(:LW),DWC__GLOBALSYM)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C
 900	SP_LIST_KEY = DWC_SUCCESS
	RETURN
C
 992	IS = 2
	CALL WNCTXT(DWLOG,'Ambiguous abbreviation, !AS, matches:',
	1						KEY(:LK))
	GOTO 999
 993	IS = 2
	CALL WNCTXT(DWLOG,'Unknown parameter: !AS',KEY(:LK))
	GOTO 999
 994	IS = 2
	CALL WNCTXT(DWLOG,'Unknown input paramater: !AS',KEY(:LK))
	GOTO 999
 995	IS = 2
	CALL WNCTXT(DWLOG,'No local default allowed for !AS',KEY(:LK))
	GOTO 999
 996	IS = 2
	CALL WNCTXT(DWLOG,'Incorrect value for !AS: !AS',KEY(:LK),
	1						WORK(:LW))
	GOTO 999
C
 999	SP_LIST_KEY = IS
	RETURN
	END
