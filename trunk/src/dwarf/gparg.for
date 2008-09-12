C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	GP_ARG
C.Keywords:	Program Parameters, Get Value, Arguments
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	900416 FMO - recreation
C.Version:	920214 GvD - no optional arguments in MSG anymore
C.Version:	920429 GvD - put argument DEFAULT in front of DEFARR, because
C			if DEFARR is passed with %REF on the SUN, it is still
C			treated as a string, so its length is also passed and
C			used as length of DEFAULT.
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GP_ARG ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Make source module name known
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C.Notes:	Dummy function
C-------------------------------------------------------------------------
C
C
	GP_ARG = DWC_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GP_ARG_CHECK (LARR,NBYT,NR,DEFAULT,LDEF,
	1				DEFSTR,DEFARR,NRDEF,LDARR,FLAGS)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	LARR		! (i) length of array elements in bytes
	INTEGER*4	NBYT		! (i) total length of array in bytes
	INTEGER*4	NR		! (i) nr of filled elements in array
	CHARACTER*(*)	DEFAULT		! (o) default as standard value string
	INTEGER*4	LDEF		! (o) significant length of DEFAULT
	CHARACTER*(*)	DEFSTR		! (i) default value (given as a string)
	BYTE		DEFARR(*)	! (i) default value (given as an array)
	INTEGER*4	NRDEF		! (i) nr of elements in DEFARR
	INTEGER*4	LDARR		! (i) length of DEFARR elements
	INTEGER*4	FLAGS		! (i) flags to control GET_PARM
C
C.Purpose:	Check the GET_PARM arguments
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	fatal	DWC_PARTOOSML	ARRAY doen't contain enough elements
C	fatal	DWC_PARELTSML	elements in character-type ARRAY are too short
C	fatal	DWC_PARNONR	NR argument is required but not present
C	fatal	DWC_TWICEVAL	default value is given as string and as array
C	fatal	DWC_TBNOTALL	TOBY format is not allowed
C	fatal	DWC_TBNOMULT	TOBY format, but NRDEF is not a multiple of 3
C	fatal	DWC_TOOMANSET	DEFARR contains too many sets
C	false status codes returned by referenced modules
C.Notes:
C	- The default value can be given either as a string (non-blank DEFSTR)
C	  or as an array (NRDEF defined). In both cases it will be converted to
C	  a standard DWARF value string, in which all symbols have been
C	  substituted (they must be known now).
C	- The data type of the array is determined by the PPD file.
C	- If the flag PARM__TOBY is given, the array is assumed to be in TOBY
C	  format (triplets: start, end, increment) in which case NRDEF must
C	  be a multiple of 3.
C-------------------------------------------------------------------------
C
C
	CHARACTER*(*)	BLANK
	INTEGER*4	NRARG
		PARAMETER (BLANK  = ' ')
		PARAMETER (NRARG = 1)
C
	CHARACTER*10	NAME(NRARG)
	INTEGER*4	ATTR(NRARG)
	CHARACTER*1	PROMPT(NRARG)
	CHARACTER*1	DEFVAL(NRARG)
		DATA NAME   /'VALSTR'       /
		DATA ATTR   /CLI__EXPRESSION/
		DATA PROMPT /BLANK          /
		DATA DEFVAL /BLANK          /
C
	INTEGER*4	PV_DEF_ENCODE
	INTEGER*4	CLI_RESET, CLI_PARSE, CLI_GET
	INTEGER*4	DWC_STR_STANDARD, DWC_STR_SUBST
	INTEGER*4	DWC_STREAM_GET, DWC_HELP
	INTEGER*4	PPD_DTYPE_GET, PPD_NVAL_GET, PPD_AMAS_GET
	INTEGER*4	MSG_SET  
C
	CHARACTER	VALSTR*255, WORK*255, STREAM*16, DTYPE*1
	INTEGER*4	LVAL, LW, LS
	INTEGER*4	IS, PLEN, NRVPS, MNVPS, MXVPS
	INTEGER*4	DLEVEL, ERRPTR
	LOGICAL*4	SWSYM
C
C
	DEFAULT = BLANK
	LDEF = 0
C
C					Get the size of the value and the
C					required number of values per set
C
	IS = PPD_DTYPE_GET (DTYPE,PLEN)
	IF (IAND(IS,1).NE.0) IS = PPD_NVAL_GET (NRVPS,MNVPS,MXVPS)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Check the array size
C					- required: LARR >= PLEN
C					  and  NBYT/LARR >= NRVPS
C
	IF (LARR.LT.PLEN) GOTO 991
	I = NBYT/LARR
	IF (I.LT.NRVPS) GOTO 992
C
C					The NR argument must be present:
C					- if the nr of values can be > 1
C					- or if wildcards or null values
C					  are possible
C
	IF (NR.EQ.UNDEF_J) THEN
		IF (NRVPS.GT.1
	1	.OR. IAND(PPD_AMAS_GET('WILD_CARDS'),1) .NE. 0
	2	.OR. IAND(PPD_AMAS_GET('NULL_VALUES'),1) .NE. 0) GOTO 993
	ENDIF
C
C					If the caller provided a default:
C					- convert to standard value string
 
	LVAL = 0					! assume: no default
	IF (DEFSTR.NE.BLANK) THEN
		IF (NRDEF.NE.UNDEF_J .OR. DEFARR(1).NE.UNDEF_B) GOTO 994
		IS = DWC_STR_STANDARD (DEFSTR,VALSTR,LVAL)
		IF (IAND(IS,1).EQ.0) GOTO 999
	ELSE IF (NRDEF.NE.UNDEF_J .OR. DEFARR(1).NE.UNDEF_B) THEN
		IS = PV_DEF_ENCODE (DEFARR,NRDEF,LDARR,FLAGS,VALSTR,LVAL)
		IF (IAND(IS,1).EQ.0) GOTO 999
	ENDIF
C
C					- substitute symbols (all known now)
C
	IF (LVAL.GT.0) THEN
		SWSYM = .FALSE.
		IS = DWC_STREAM_GET (STREAM,LS,.FALSE.)
		IF (IAND(IS,1).EQ.0) GOTO 999
		IS = DWC_STR_SUBST (VALSTR(:LVAL),WORK,LW,STREAM(:LS),
	1						ERRPTR,.FALSE.,SWSYM)
		IF (IAND(IS,1).EQ.0) GOTO 995
C
C					- help request not allowed
C
		IS = DWC_HELP (WORK(:LW),-1,DLEVEL)
		IF (IAND(IS,1).EQ.0) GOTO 999
C
C					- check the final string
C
		IS = CLI_RESET (NRARG,NAME,ATTR,PROMPT,DEFVAL)
		IF (IAND(IS,1).NE.0) IS = CLI_PARSE (WORK(:LW))
		IF (IAND(IS,1).NE.0) IS = CLI_GET ('VALSTR',DEFAULT,LDEF)
		IF (IAND(IS,1).EQ.0) GOTO 999
	ENDIF
C
C
	GP_ARG_CHECK = DWC_SUCCESS
	RETURN
C
 991	GP_ARG_CHECK = MSG_SET (DWC_PARELTSML,1)
	CALL WNCTXT(DWLOG,DWMSG,LARR,PLEN)
	RETURN
 992	GP_ARG_CHECK = MSG_SET (DWC_PARTOOSML,1)
	CALL WNCTXT(DWLOG,DWMSG,I,NRVPS)
	RETURN
 993	GP_ARG_CHECK = MSG_SET (DWC_PARNONR,0)
	RETURN
 994	GP_ARG_CHECK = MSG_SET (DWC_TWICEVAL,0)
	RETURN
 995	GP_ARG_CHECK = MSG_SET (DWC_EXPERRMSG,0)
	CALL WNCTXT(DWLOG,DWMSG,BLANK,ERRPTR,WORK(:LW))
	RETURN
 999	GP_ARG_CHECK = IS
	RETURN
	END
