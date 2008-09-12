C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	BP_DEF_CHECK
C.Keywords:	Program Parameters, PPD Default, Check
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	900416 FMO - recreation
C.Version:	940120 CMV - changed messenger
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION BP_DEF_CHECK (VALUE)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	VALUE		! (i) input value string
C
C.Purpose:	Check the syntax of a PPD default
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	false status codes returned by referenced modules
C.Notes:
C	- The string may contain unknown symbols; they will temporarily be
C	  replaced by '1' to allow the syntax checking.
C	- The only allowed qualifier is /(NO)ASK.
C	- The string cannot be a help request (question mark).
C	- The message buffer is cleared (BLDPPD reports itself).
C-------------------------------------------------------------------------
C
C
	CHARACTER*(*)	STREAM
		PARAMETER (STREAM = '$1')
C
	INTEGER*4	CLI_RESET, CLI_PARSE, CLI_GET
	INTEGER*4	DWC_STR_SUBST, DWC_HELP
	INTEGER*4	PV_BLK_ALLOC, PV_BLK_DECODE, PV_BLK_RELEASE
C
	INTEGER*4	NRARG
		PARAMETER (NRARG = 2)
	CHARACTER*6	NAME(NRARG)
	INTEGER*4	ATTR(NRARG)
	CHARACTER*1	PROMPT(NRARG)
	CHARACTER*1	DEFVAL(NRARG)
		DATA NAME   /'VALSTR'        ,'ASK'         /
		DATA ATTR   /CLI__EXPRESSION ,CLI__QUALIFIER/
		DATA PROMPT /' '             ,' '           /
		DATA DEFVAL /' '             ,' '           /
C
	CHARACTER*255	WORK, VALSTR
	INTEGER*4	IS, LW, LVAL, TMP, ERRPTR, DLEVEL, VALBLK(8)
	LOGICAL*4	SWSYM
	BYTE		DEFARR(1)			! dummy
C
C
C					Substitute symbols
C
	SWSYM = .FALSE.
	IS = DWC_STR_SUBST (VALUE,WORK,LW,STREAM,ERRPTR,.TRUE.,SWSYM)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					If help request: return warning
C
	IS = DWC_HELP (WORK(:LW),-1,DLEVEL)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Analyse the default string
C					- reset the Command-Line Interpreter
C					- parse the string
C					- extract the pure value string
C
	IS = CLI_RESET (NRARG,NAME,ATTR,PROMPT,DEFVAL)
	IF (IAND(IS,1).NE.0) IS = CLI_PARSE (WORK(:LW))
	IF (IAND(IS,1).NE.0) IS = CLI_GET ('VALSTR',VALSTR,LVAL)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Check the values
C					- allocate memory for the value block
C					- convert the string to a block
C					- release memory
C
	IF (LVAL.GT.0) THEN
		IS = PV_BLK_ALLOC (VALSTR(:LVAL),VALBLK)
		IF (IAND(IS,1).EQ.0) GOTO 999
		IS = PV_BLK_DECODE (VALSTR(:LVAL),VALBLK,STREAM,
	1					.TRUE.,SWSYM,.TRUE.,DEFARR,0)
		TMP = PV_BLK_RELEASE (VALBLK)
		IF (IAND(IS,1).EQ.0) GOTO 999
	ENDIF
C
C
	BP_DEF_CHECK = DWC_SUCCESS
	RETURN
C
 999	BP_DEF_CHECK = IS
	RETURN
	END
