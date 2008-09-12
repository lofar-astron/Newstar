C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	DWC_STREAM
C.Keywords:	DWARF, Stream Name
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		Common variables used:
C	CHARACTER*12	DWARF$STREAM_C	! (m) current stream name
C	INTEGER*4	DWARF$LENSTR	! (m) significant length
C
C.Version:	900228 FMO - creation
C.Version:	910923 FMO - XYZ are alphabetics too !! (CHECK)
C.Version:	920214 GvD - no optional arguments in MSG anymore
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_STREAM_PUT (STREAM)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STREAM		! (i) stream name
C
C.Purpose:	Check and store the stream name
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	error	DWC_STRINVNR	illegal stream name
C.Notes:
C	- First, STREAM will be converted to uppercase.
C	- A valid stream name consists of at most 11 alpha-numeric characters
C	  possibly prefixed with a '$'.
C	- If the prefix is absent, it will be inserted.
C	- If STREAM is blank or empty, the current name is kept.
C-------------------------------------------------------------------------
C
	INCLUDE 'DWARF_4_DEF'
C
	INTEGER*4	DWC_STREAM_CHECK, STR_UPCASE
C
	CHARACTER*16	UPCSTREAM, XSTREAM
	INTEGER*4	IS, LS
C
C
	UPCSTREAM = STREAM
	IS = STR_UPCASE (UPCSTREAM)
	IS = DWC_STREAM_CHECK (UPCSTREAM,XSTREAM,LS,.FALSE.)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	IF (LS.GT.0) THEN
		DWARF$STREAM_C = XSTREAM(:LS)
		DWARF$LENSTR = LS
	ENDIF
C
	DWC_STREAM_PUT = DWC_SUCCESS
	RETURN
C
 999	DWC_STREAM_PUT = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_STREAM_GET (STREAM,LS,IS_GLOBAL)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STREAM		! (o) stream name (with $ prefix)
	INTEGER*4	LS		! (o) significant length of STREAM
	LOGICAL*4	IS_GLOBAL	! (i) for global DWARF program name ?
C
C.Purpose:	Get stream name
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	warning	DWC_STRTOOSHO	STREAM too short
C.Notes:
C	- For global program names STREAM = '$0' will be returned.
C	- Otherwise, the current stream name will be copied from the DWARF
C	  control common, including the prefix).
C	- In case of string overflow the truncated name (and its length)
C	  will be returned.
C-------------------------------------------------------------------------
C
	INCLUDE 'DWARF_4_DEF'
C
	CHARACTER*(*)	BLANK, GLOBAL
		PARAMETER (BLANK  = ' ')
		PARAMETER (GLOBAL = '$0')
C
	INTEGER*4	STR_COPY, MSG_SET  
C
	INTEGER*4	IS
C
C
	STREAM = BLANK
	LS = 0
	IF (IS_GLOBAL) THEN
		IS = STR_COPY (GLOBAL,STREAM,LS)
	ELSE
		IS = STR_COPY (DWARF$STREAM_C(:DWARF$LENSTR),STREAM,LS)
	ENDIF
	IF (IS.LT.0) GOTO 999
C
	DWC_STREAM_GET = DWC_SUCCESS
	RETURN
C
 999	DWC_STREAM_GET = MSG_SET (DWC_STRTOOSHO,1)
	CALL WNCTXT(DWLOG,DWMSG,LEN(STREAM))
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_STREAM_CHECK (STREAM,XSTREAM,LX,IS_GLOBAL)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STREAM		! (i) stream name
	CHARACTER*(*)	XSTREAM		! (o) stream name (with $ prefix)
	INTEGER*4	LX		! (o) significant length of XSTREAM
	LOGICAL*4	IS_GLOBAL	! (i) for global DWARF program name ?
C
C.Purpose:	Check stream name
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS	also for blank STREAM (then: LX = 0)
C	warning	DWC_STRTOOSHO	XSTREAM too short
C	error	DWC_STRINVNR	illegal stream name
C.Notes:
C	- A valid stream name consists of at most 11 alpha-numeric characters
C	  possibly prefixed with a '$'.
C	- If the prefix is absent, it will be inserted.
C	- For global program names XSTREAM must be '$0'.
C	- If STREAM is blank or empty, no checks will be done.
C-------------------------------------------------------------------------
C
C
	CHARACTER*(*)	BLANK, ANUM, PREFIX, GLOBAL
	INTEGER*4	MAXLX
		PARAMETER (BLANK  = ' ')
		PARAMETER (ANUM   = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789')
		PARAMETER (PREFIX = '$')
		PARAMETER (GLOBAL = '$0')
		PARAMETER (MAXLX  = 12)
C
	INTEGER*4	STR_SIGLEN, STR_COPY_W
	INTEGER*4	MSG_SET  
C
	INTEGER*4	IS, LS, PTR
C
C
	LS = STR_SIGLEN (STREAM)
	IF (LS.EQ.0) THEN
		XSTREAM = BLANK
		LX = 0
	ELSE
		XSTREAM = PREFIX
		LX = 1
		PTR = 1
		IF (STREAM(1:1).EQ.PREFIX) PTR = 2
		IF (PTR.GT.LS) GOTO 999			! just a prefix
		IS = STR_COPY_W (ANUM,STREAM(:LS),PTR,XSTREAM,LX)
		IF (LX.GT.MAXLX) GOTO 999		! too long
		IF (IS.LT.0) GOTO 998			! string overflow
		IF (PTR.LE.LS) GOTO 999			! not alpha-numeric
		IF (IS_GLOBAL .AND. XSTREAM(:LX).NE.GLOBAL) GOTO 999
	ENDIF
C
	DWC_STREAM_CHECK = DWC_SUCCESS
	RETURN
C
 998	DWC_STREAM_CHECK = MSG_SET (DWC_STRTOOSHO,1)
	CALL WNCTXT(DWLOG,DWMSG,LEN(XSTREAM))
	RETURN
C
 999	DWC_STREAM_CHECK = MSG_SET (DWC_STRINVNR,1)
	CALL WNCTXT(DWLOG,DWMSG,STREAM)
	RETURN
	END
