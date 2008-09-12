C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	GP_INI
C.Keywords:	Program Parameters, Initial Defaults
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		Common fields used:
C	INTEGER*4	PARM$EXELDYN	! (m) length of VM block
C	INTEGER*4	PARM$EXEADYN	! (m) address of VM block
C	INTEGER*4	PARM$EXEAVAL	! (m) address of value part
C	INTEGER*4	PARM$EXEASW	! (m) address of switches part
C	INTEGER*4	PARM$EXENRS	! (m) nr of sets
C	INTEGER*4	PARM$EXEVPS	! (m) reserved nr of values per set
C	INTEGER*4	PARM$EXETYPE	! (m) type of initial default
C
C.Version:	900416 FMO - recreation
C.Version:	920206 GvD - add former optional arguments to CLI_GET
C.Version:	920513 GvD - TOBY not allowed for logicals
C.Version:	940329 CMV - Ignore errors in defaults
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GP_INI ()
C	          ENTRY    GP_INI_CLEAR ()
C	          ENTRY    GP_INI_PUT (VALBLK,TYPE)
C	          ENTRY    GP_INI_GET (VALBLK,TYPE)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INTEGER*4	GP_INI_CLEAR, GP_INI_PUT, GP_INI_GET
C
	INTEGER*4	VALBLK(8)	! (i/o) value block descriptor
	INTEGER*4	TYPE		! (i/o) type of initial default
C
C.Purpose:	Manipulate the description of the initial-defaults value block
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	false status code returned by referenced routines
C.Notes:
C	- PUT copies a value-block descriptor into the initial-defaults block
C	  descriptor and sets the default type code.
C	- GET returns the block descriptor and the default type code:
C	  TYPE = 0 (none), 1 (SPECIFY), or 2 (PPD or caller default possible.
C-------------------------------------------------------------------------
C
	INCLUDE 'PARM_6_DEF'
C
	INTEGER*4	SCALAR_BIT, TOBY_BIT
		PARAMETER (SCALAR_BIT = 0)
		PARAMETER (TOBY_BIT   = 1)
C
	INTEGER*4	PPD_DTYPE_GET, PPD_NVAL_GET, PPD_AMAS_GET
	INTEGER*4	CLEAR_BLJ, MOVE_BLJ
C
	CHARACTER*1	DTYPE
	INTEGER*4	IS, NVAL, MNVAL, MXVAL
C
C
	GP_INI = DWC_SUCCESS
	RETURN
C
C	==================
	ENTRY GP_INI_CLEAR ()
C	==================
C
	IS = CLEAR_BLJ (PARM$EXELDYN,7)
C
	GP_INI_CLEAR = DWC_SUCCESS
	RETURN
C
C	================
	ENTRY GP_INI_PUT (VALBLK,TYPE)
C	================
C
	IS = MOVE_BLJ (VALBLK,PARM$EXELDYN,6)
	PARM$EXETYPE = TYPE
C
	GP_INI_PUT = DWC_SUCCESS
	RETURN
C
C	================
	ENTRY GP_INI_GET (VALBLK,TYPE)
C	================
C
	IS = MOVE_BLJ (PARM$EXELDYN,VALBLK,6)
	IF (IAND(IS,1).NE.0) TYPE = PARM$EXETYPE
	IF (IAND(IS,1).NE.0) IS = PPD_DTYPE_GET (DTYPE,VALBLK(7))
	IF (IAND(IS,1).NE.0) IS = PPD_NVAL_GET (NVAL,MNVAL,MXVAL)
	IF (IAND(IS,1).EQ.0) GOTO 999
	VALBLK(8) = 0
	IF (NVAL.EQ.1) VALBLK(8) = IBSET (VALBLK(8),SCALAR_BIT)
	IF (DTYPE.NE.'C' .AND. DTYPE.NE.'L'
	1			.AND. IAND(PPD_AMAS_GET('VECTOR'),1) .EQ. 0)
	2		VALBLK(8) = IBSET (VALBLK(8),TOBY_BIT)
C
	GP_INI_GET = DWC_SUCCESS
	RETURN
C
 999	GP_INI_GET = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GP_INI_FILL (SYMBOL,ASK)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	SYMBOL		! (i) full symbol name
	INTEGER*4	ASK(2)		! (o) ask switches
C
C.Purpose:	Fill the initial-defaults block for a program parameter
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	fatal	DWC_PARWRDEF	error in default value
C.Notes:
C	- ASK(1) gives the ASK qualifier on the SPECIFY default.
C	  The coding is: -1 for /NOASK, 0 if not given, +1 for /ASK.
C	- ASK(2) does the same for the PPD default.
C-------------------------------------------------------------------------
C
C
	INTEGER*4	GP_INI_CLEAR, GP_INI_PUT, GP_INI_DECODE
	INTEGER*4	PV_DEF_GET, DWC_SYM_SPLIT, PPD_DVSTR_GET
	INTEGER*4	MSG_SET  
C
	CHARACTER	VALUE*255, TYPE*16, PROG*16, STREAM*16, KEY*16
	INTEGER*4	IS, LV, LT, TYPCOD, LP, LS, LK, VALBLK(8)
C
C
	ASK(1) = 0
	ASK(2) = 0
	IS = DWC_SYM_SPLIT (SYMBOL,PROG,LP,STREAM,LS,KEY,LK)
	IF (IAND(IS,1).NE.0) IS = GP_INI_CLEAR ()
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Get the initial default (if any)
C					- its type (1: SPECIFY, 2: program)
C					- the ASK switch (from qualifier)
C
	IS = PV_DEF_GET (SYMBOL,VALUE,LV,TYPE,LT)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (LT.GT.0) THEN
C
C
C					SPECIFY default present (local/group):
C					- if values, convert to value block
C					- if qualifier only, use PPD default
C
	    IF (TYPE(1:1).NE.'p') THEN
		TYPCOD = 1
		IS = GP_INI_DECODE (VALUE,STREAM(:LS),.FALSE.,VALBLK,ASK(1))
C
C	If an error occured, just ignore
C
C		IF (IAND(IS,1).EQ.0) GOTO 999
	        IF (IAND(IS,1).EQ.0) THEN
	           GP_INI_FILL = MSG_SET (DWC_PARWRDEF,1)
	           CALL WNCTXT(DWLOG,DWMSG,SYMBOL)
		   ASK(1)=0
		   ASK(2)=0
	           VALBLK(1)=0
		ELSE IF (VALBLK(1).EQ.0) THEN
			IS = PPD_DVSTR_GET (VALUE,LV)
			IF (IAND(IS,1).EQ.0) GOTO 999
			IF (LV.GT.0) THEN
				TYPCOD = 2
				IS = GP_INI_DECODE (VALUE,STREAM(:LS),
	1						.TRUE.,VALBLK,ASK(2))
				IF (IAND(IS,1).EQ.0) GOTO 999
			ENDIF
		ENDIF
C
C					Program default allowed:
C					- if there is no PPD default value
C					  (no default or qualifier only),
C					  VALBLK will be zero
C
	    ELSE
		TYPCOD = 2
		IS = GP_INI_DECODE (VALUE,STREAM(:LS),.TRUE.,VALBLK,ASK(2))
		IF (IAND(IS,1).EQ.0) GOTO 999
	    ENDIF
C
C					If a value has been found: store it
C
	    IF (VALBLK(1).NE.0) IS = GP_INI_PUT (VALBLK,TYPCOD)
	ENDIF
C
C
	GP_INI_FILL = DWC_SUCCESS
	RETURN
C
 999	GP_INI_FILL = MSG_SET (DWC_PARWRDEF,1)
	CALL WNCTXT(DWLOG,DWMSG,SYMBOL)
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GP_INI_DECODE (VALUE,STREAM,SWDV,VALBLK,ASK)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	VALUE		! (i) input value string
	CHARACTER*(*)	STREAM		! (i) stream name (for substitution)
	LOGICAL*4	SWDV		! (i) .TRUE. (PPD) or .FALSE. (SPECIFY)
	INTEGER*4	VALBLK(8)	! (o) value-block descriptor
	INTEGER*4	ASK		! (o) ASK qualifier switch
C					      -1: /NOASK, 0: not given, 1: /ASK
C
C.Purpose:	Process the initial default for a program parameter
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS	also if no values
C	false status codes returned by referenced modules
C.Notes:
C	- Evaluate the input string and convert it to a value block.
C	- The allowed qualifiers are /(NO)ASK and /(NO)SUBSTITUTE (although
C	  the latter one will be ignored: substitution is always done).
C	- All symbols must be known now.
C	- If VALUE does not contain a value, VALBLK will be cleared.
C-------------------------------------------------------------------------
C
C
	INTEGER*4	CLI_RESET, CLI_PARSE, CLI_GET
	INTEGER*4	DWC_STR_SUBST, DWC_HELP
	INTEGER*4	PV_BLK_ALLOC, PV_BLK_DECODE, PV_BLK_RELEASE
	INTEGER*4	MSG_SET  , CLEAR_BLJ
C
	INTEGER*4	NRARG
		PARAMETER (NRARG = 3)
	CHARACTER*10	NAME(NRARG)
	INTEGER*4	ATTR(NRARG)
	CHARACTER*1	PROMPT(NRARG)
	CHARACTER*1	DEFVAL(NRARG)
		DATA NAME   /'VALSTR'       ,'ASK'          ,'SUBSTITUTE' /
		DATA ATTR   /CLI__EXPRESSION,CLI__QUALIFIER,CLI__QUALIFIER/
		DATA PROMPT /' '            ,' '            ,' '          /
		DATA DEFVAL /' '            ,' '            ,' '          /
C
	CHARACTER	WORK*255, VALSTR*255, DUM*1
	INTEGER*4	IS, LW, LVAL, LD, ERRPTR, DLEVEL
	LOGICAL*4	SWSYM
	BYTE		DEFARR(1)				! dummy
C
C
C					Substitute symbols
C
	SWSYM = .FALSE.
	IS = DWC_STR_SUBST (VALUE,WORK,LW,STREAM,ERRPTR,.FALSE.,SWSYM)
	IF (IAND(IS,1).EQ.0) GOTO 991
C
C					If help request (not allowed): return
C
	IS = DWC_HELP (WORK(:LW),-1,DLEVEL)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Analyse the string
C					- reset the Command-Line Interpreter
C					- parse the string
C					- extract the pure value string
C
	IS = CLI_RESET (NRARG,NAME,ATTR,PROMPT,DEFVAL)
	IF (IAND(IS,1).NE.0) IS = CLI_PARSE (WORK(:LW))
	IF (IAND(IS,1).NE.0) IS = CLI_GET ('VALSTR',VALSTR,LVAL)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Analyse the qualifiers
C					- return the ASK qualifier switch
C					- ignore any SUBSTITUTE qualifier
C
	IS = CLI_GET ('ASK',DUM,LD)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (IS.EQ.DWC_PRESENT) THEN
		ASK = 1
	ELSE IF (IS.EQ.DWC_NEGATED) THEN
		ASK = -1
	ELSE
		ASK = 0
	ENDIF
C
C					Convert value string to value block
C					- first allocate memory for the block
C					- in case of error: release memory
C
	IF (LVAL.GT.0) THEN
		IS = PV_BLK_ALLOC (VALSTR(:LVAL),VALBLK)
		IF (IAND(IS,1).EQ.0) GOTO 999
C
		IS = PV_BLK_DECODE (VALSTR(:LVAL),VALBLK,STREAM,
	1					.FALSE.,SWSYM,SWDV,DEFARR,0)
		IF (IAND(IS,1).EQ.0) THEN
			IS = PV_BLK_RELEASE (VALBLK)
			GOTO 999
		ENDIF
	ELSE
		IS = CLEAR_BLJ (VALBLK,8)
	ENDIF
C
C
	GP_INI_DECODE = DWC_SUCCESS
	RETURN
C
 991	GP_INI_DECODE = MSG_SET (DWC_EXPERRMSG,1)
	CALL WNCTXT(DWLOG,DWMSG,' ',ERRPTR,WORK(:LW))
	RETURN
 999	GP_INI_DECODE = IS
	RETURN
	END
