C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	VP_DEF_CHECK
C.Keywords:	Program Parameters, View, Check Values
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	900416 FMO - recreation
C.Version:	920206 GvD - add former optional arguments to CLI_GET
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION VP_DEF_CHECK (SYMBOL,VALUE,PROG_DEF,DLEVEL,
	1						VALOUT,LOUT)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	SYMBOL		! (i) full parameter name
	CHARACTER*(*)	VALUE		! (i) input value string
	LOGICAL*4	PROG_DEF	! (i) is value a program default ?
	INTEGER*4	DLEVEL		! (m) helplevel minus userlevel
	CHARACTER*(*)	VALOUT		! (o) output value string
	INTEGER*4	LOUT		! (o) significant length of VALOUT
C
C.Purpose:	Process the value of a program parameter
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	false status codes returned by referenced modules
C.Notes:
C	- Check and evaluate the input string.
C	- There may be no unknown symbols.
C	- The allowed qualifier is /(NO)ASK.
C	- An output string will be generated which is the fully evaluated and
C	  expanded input string (including possible qualifiers).
C-------------------------------------------------------------------------
C
	INTEGER*4	CLI_RESET, CLI_PARSE, CLI_GET
	INTEGER*4	DWC_STR_SUBST, DWC_SYM_SPLIT, DWC_HELP
	INTEGER*4	PV_BLK_DECODE, PV_BLK_ENCODE
	INTEGER*4	PV_BLK_ALLOC, PV_BLK_RELEASE
	INTEGER		STR_COPY, MSG_SET
C
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
	CHARACTER	WORK*255, VALSTR*255, PROG*16, STREAM*16, KEY*16
	CHARACTER	DUM*1
	INTEGER*4	IS, LW, LVAL, LD, LP, LS, LK
	INTEGER*4	ERRPTR, VALBLK(8)
	LOGICAL*4	SWSYM
	BYTE		DEFARR(1)				! dummy
C
C
C					Substitute symbols
C
	SWSYM = .FALSE.
	IS = DWC_SYM_SPLIT (SYMBOL,PROG,LP,STREAM,LS,KEY,LK)
	IF (IAND(IS,1).NE.0) IS = DWC_STR_SUBST
	1		(VALUE,WORK,LW,STREAM(:LS),ERRPTR,.FALSE.,SWSYM)
	IF (IAND(IS,1).EQ.0) GOTO 991
C
C					If help request:
C					- give help and return
C
	IS = DWC_HELP (WORK(:LW),1,DLEVEL)
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
C					Check the value string
C					- allocate memory for the value block
C
	IS = PV_BLK_ALLOC (VALSTR(:LVAL),VALBLK)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					- convert value string to value block
C
	IS = PV_BLK_DECODE (VALSTR(:LVAL),VALBLK,STREAM(:LS),
	1					.FALSE.,SWSYM,PROG_DEF,DEFARR,0)
	IF (IAND(IS,1).EQ.0) GOTO 992
C
C					- convert back to string and
C					  append ASK qualifier to string
C					  (substitute qual no longer important)
C
	IS = PV_BLK_ENCODE (VALBLK,VALOUT,LOUT)
	IF (IAND(IS,1).NE.0) IS = CLI_GET ('ASK',DUM,LD)
	IF (IAND(IS,1).EQ.0) GOTO 992
	IF (IS.EQ.DWC_PRESENT) THEN
		IS = STR_COPY (' /ASK',VALOUT,LOUT)
	ELSE IF (IS.EQ.DWC_NEGATED) THEN
		IS = STR_COPY (' /NOASK',VALOUT,LOUT)
	ENDIF
C
C					- release memory and return
C
	IS = PV_BLK_RELEASE (VALBLK)
	VP_DEF_CHECK = DWC_SUCCESS
	RETURN
C
 991	IS = MSG_SET (DWC_EXPERRMSG,1)
	CALL WNCTXT(DWLOG,DWMSG,' ',ERRPTR,WORK(:LW))
	GOTO 999
 992	IS = PV_BLK_RELEASE (VALBLK)
	GOTO 999
 999	VP_DEF_CHECK = MSG_SET (DWC_PARWRDEF,1)
	CALL WNCTXT(DWLOG,DWMSG,KEY(:LK))
	RETURN
	END
