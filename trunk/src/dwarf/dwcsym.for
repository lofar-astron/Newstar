C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	DWC_SYM
C.Keywords:	DWARF, Symbols
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	900416 FMO - recreation
C.Version:	910807 FMO - recognize abbreviations of symbol names YES and NO
C			in _TRANSL (for Alliant)
C.Version:	920214 GvD - no optional arguments in MSG anymore
C.Version:	931116 CMV - Allow . in stead of $ for stream at input
C.Version:	010709 AXC - linux port - tmpchar in calls
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_SYM_SPLIT (SYMBOL,PROGNAM,LP,STREAM,
	1							LS,KEY,LK)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	SYMBOL		! (i) symbol name
	CHARACTER*(*)	PROGNAM		! (o) program name
	INTEGER*4	LP		! (o) significant length of PROGNAM
	CHARACTER*(*)	STREAM		! (o) stream name (with '$' prefix)
	INTEGER*4	LS		! (o) significant length of STREAM
	CHARACTER*(*)	KEY		! (o) keyword (without '_' prefix)
	INTEGER*4	LK		! (o) significant length of KEY
C
C.Purpose:	Split a DWARF symbol name in its components
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	warning	DWC_STRTOOSHO	one of the output strings is too short
C.Notes:
C	- No syntax checks are done.
C	- Trailing blanks in components are removed.
C	- All components will be extracted, even if one of them has to be
C	  truncated.
C-------------------------------------------------------------------------
C
C
	CHARACTER*(*)	BLANK, DOLLAR, DOT, UNDERSC
		PARAMETER (BLANK = ' ')
		PARAMETER (DOLLAR = '$')
		PARAMETER (DOT = '.')
		PARAMETER (UNDERSC = '_')
C
	INTEGER*4	STR_SIGLEN, STR_COPY_U, STR_COPY
	INTEGER*4	MSG_SET  
C
	INTEGER*4	IS, LSYM, PTR, TMP
C
C
	PROGNAM = BLANK
	STREAM = BLANK
	KEY = BLANK
	LP = 0
	LS = 0
	LK = 0
	LSYM = STR_SIGLEN (SYMBOL)
	PTR = 1
	IS = DWC_SUCCESS
C
C					Extract program name
C
	TMP = STR_COPY_U (DOLLAR//DOT//UNDERSC,
     &	                  SYMBOL(:LSYM),PTR,PROGNAM,LP)
	IF (TMP.LT.0) IS = MSG_SET (DWC_STRTOOSHO,1)
	IF (TMP.LT.0) CALL WNCTXT(DWLOG,DWMSG,LEN(PROGNAM))
	LP = STR_SIGLEN (PROGNAM(:LP))
C
C					Extract stream name (with '$' prefix)
C
	IF (PTR.LE.LSYM .AND. (SYMBOL(PTR:PTR).EQ.DOLLAR.OR.
     &	                       SYMBOL(PTR:PTR).EQ.DOT) ) THEN
		TMP = STR_COPY_U (UNDERSC,SYMBOL(:LSYM),PTR,STREAM,LS)
		IF (TMP.LT.0) IS = MSG_SET (DWC_STRTOOSHO,1)
		IF (TMP.LT.0) CALL WNCTXT(DWLOG,DWMSG,LEN(STREAM))
	        STREAM(1:1)=DOLLAR	!In case it happened to be DOT
		LS = STR_SIGLEN (STREAM(:LS))
	ENDIF
C
C					Extract keyword (without '_' prefix)
C
	IF (PTR.LT.LSYM) THEN
		TMP = STR_COPY (SYMBOL(PTR+1:LSYM),KEY,LK)
		IF (TMP.LT.0) IS = MSG_SET (DWC_STRTOOSHO,1)
		IF (TMP.LT.0) CALL WNCTXT(DWLOG,DWMSG,LEN(KEY))
		LK = STR_SIGLEN (KEY(:LK))
	ENDIF
C
	DWC_SYM_SPLIT = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_SYM_BUILD (PROGNAM,STREAM,KEY,SYMBOL,LSYM)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	PROGNAM		! (i) program name
	CHARACTER*(*)	STREAM		! (i) stream name (with '$' prefix)
	CHARACTER*(*)	KEY		! (i) keyword (without '_' prefix)
	CHARACTER*(*)	SYMBOL		! (o) symbol name
	INTEGER*4	LSYM		! (o) significant length of SYMBOL
C
C.Purpose:	Build a DWARF symbol name from its components
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	warning	DWC_STRTOOSHO	the output string is too short
C.Notes:
C	- Only non-blank components will be used (ignore trailing blanks).
C	- No syntax checks are done.
C-------------------------------------------------------------------------
C
C
	CHARACTER*(*)	BLANK, UNDERSC
		PARAMETER (BLANK = ' ')
		PARAMETER (UNDERSC = '_')
C
	INTEGER*4	STR_SIGLEN, STR_COPY
	INTEGER*4	MSG_SET  
C
	INTEGER*4	IS, LP, LS, LK
	CHARACTER	TMP*64
C
C
	SYMBOL = BLANK
	LSYM = 0
	LP = STR_SIGLEN (PROGNAM)
	LS = STR_SIGLEN (STREAM)
	LK = STR_SIGLEN (KEY)
C
	IF (LP.GT.0) IS = STR_COPY (PROGNAM(:LP),SYMBOL,LSYM)
	IF (LS.GT.0) IS = STR_COPY (STREAM(:LS),SYMBOL,LSYM)
	TMP=UNDERSC//KEY(:LK)
	IF (LK.GT.0) IS = STR_COPY (TMP(:LK+1),SYMBOL,LSYM)
	LSYM = STR_SIGLEN (SYMBOL(:LSYM))
C		
	IF (IS.GT.0) THEN
		DWC_SYM_BUILD = DWC_SUCCESS
	ELSE
		DWC_SYM_BUILD = MSG_SET (DWC_STRTOOSHO,1)
		CALL WNCTXT(DWLOG,DWMSG,LEN(SYMBOL))
	ENDIF
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_SYM_EXPAND (SYMBOL,STREAM,XSYMBOL,LX)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	SYMBOL		! (i) symbol name
	CHARACTER*(*)	STREAM		! (i) fill-up stream name
	CHARACTER*(*)	XSYMBOL		! (o) expanded symbol name
	INTEGER*4	LX		! (o) significant length of XSYMBOL
C
C.Purpose:	Expand a DWARF symbol name by inserting a stream name
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C.Notes:
C	- If STREAM is a valid stream name (with or without '$' prefix) and
C	  if SYMBOL is a valid name for a DWARF symbol without a stream
C	  component (e.g., <prognam>_<keyword>), the stream component will be
C	  inserted.
C	- If the program name is a global one ('DWARF' or 'GLOBAL'), the global
C	  stream name ('$0') will be used, otherwise the given stream name.
C	- If SYMBOL or STREAM violate the DWARF syntax in any way, the
C	  original name is copied to XSYMBOL. All messages will be suppressed.
C-------------------------------------------------------------------------
C
C
	INTEGER*4	DWC_SYM_SPLIT, DWC_SYM_BUILD
	INTEGER*4	DWC_PROG_CHECK, DWC_STREAM_CHECK, DWC_STREAM_GET
	INTEGER*4	STR_SIGLEN
C
	CHARACTER*16	PROG, XSTREAM, TMP, KEY
	INTEGER*4	IS, LP, LS, LT, LK
	LOGICAL*4	IS_GLOBAL
C
C
C					Check whether STREAM is a valid
C					stream name (add prefix if necessary)
C
	IS = DWC_STREAM_CHECK (STREAM,XSTREAM,LS,.FALSE.)
	IF (IAND(IS,1).EQ.0 .OR. LS.EQ.0) GOTO 999
C
C					Split the symbol name in its components
C					and check their presence/absence
C
	IS = DWC_SYM_SPLIT (SYMBOL,PROG,LP,TMP,LT,KEY,LK)
	IF (IAND(IS,1).EQ.0 .OR.
	1   LP.EQ.0 .OR. LT.GT.0 .OR. LK.EQ.0) GOTO 999
C
C					Check the syntax of PROG and get
C					the global stream if appropriate
C
	IS = DWC_PROG_CHECK (PROG(:LP),LP,IS_GLOBAL)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (IS_GLOBAL) THEN
		IS = DWC_STREAM_GET (XSTREAM,LS,IS_GLOBAL)
		IF (IAND(IS,1).EQ.0) GOTO 999
	ENDIF
C
C					Build the expanded symbol name
C
	IS = DWC_SYM_BUILD (PROG(:LP),XSTREAM(:LS),KEY(:LK),XSYMBOL,LX)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	DWC_SYM_EXPAND = DWC_SUCCESS
	RETURN
C
 999	XSYMBOL = SYMBOL
	LX = STR_SIGLEN (XSYMBOL)
	DWC_SYM_EXPAND = DWC_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_SYM_TRANSL (SYMBOL,VALUE,LVAL,CHKSW,SWSYM)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	SYMBOL		! (i) symbol name
	CHARACTER*(*)	VALUE		! (o) symbol value
	INTEGER*4	LVAL		! (o) significant length of VALUE
	LOGICAL*4	CHKSW		! (i) unknown symbols allowed ?
	LOGICAL*4	SWSYM		! (m) were there unknown symbols ?
C
C.Purpose:	Translate a DWARF symbol
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	warning	DWC_STRTOOSHO	value string has been truncated
C	error	DWC_SYMNOTDEF	unknown symbol (only for CHKSW = .TRUE.)
C.Notes:
C	- The value string will be converted to a standard DWARF string.
C	- If the symbol is unknown, the dummy value '1' will be returned.
C-------------------------------------------------------------------------
C
C
	CHARACTER*(*)	DUMMY
	INTEGER*4	LDUM
		PARAMETER (DUMMY = '1')		! dummy symbol value
		PARAMETER (LDUM  =  1 )
C
	INTEGER*4	DWC_STR_STANDARD
	INTEGER*4	MSG_SET  , SYMBOL_GET
C
	CHARACTER*255	WORK
	INTEGER*4	IS, LW
C
C
C					Translate the symbol
C					- explicitly translate YES and NO
C					- use dummy value for unknown symbol
C
	IF (SYMBOL.EQ.'YES' .OR. SYMBOL.EQ.'YE' .OR. SYMBOL.EQ.'Y') THEN
		WORK = '.TRUE.'
		LW = 6
	ELSE IF (SYMBOL.EQ.'NO' .OR. SYMBOL.EQ.'N') THEN
		WORK = '.FALSE.'
		LW = 7
	ELSE
		IS = SYMBOL_GET (SYMBOL,WORK,LW)
		IF (LW.EQ.0) THEN
			SWSYM = .TRUE.
			IF (.NOT.CHKSW) GOTO 999	! unknown not allowed
			WORK = DUMMY
			LW = LDUM
		ENDIF
	ENDIF
C
C					Convert to standard DWARF string
C
	DWC_SYM_TRANSL = DWC_STR_STANDARD (WORK(:LW),VALUE,LVAL)
	RETURN
C
 999	DWC_SYM_TRANSL = MSG_SET (DWC_SYMNOTDEF,1)
	CALL WNCTXT(DWLOG,DWMSG,SYMBOL)
	RETURN
	END
