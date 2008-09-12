C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	DWC_IDENT
C.Keywords:	DWARF, Login Identification
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		Common variables used:
C	INTEGER*4	LENID		! (m) significant length of IDENT
C	CHARACTER*4	IDENT_C		! (m) login identification
C
C.Version:	900319 FMO - creation
C.Version:	911127 GvD - fixed little bug in DWC_IDENT_PUT
C			LI was tested on .EQ.0 iso. .NE.0
C.Version:	920214 GvD - no optional arguments in MSG anymore
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_IDENT ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Make source module name known
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C.Notes:	Dummy routine
C-------------------------------------------------------------------------
C
C
	DWC_IDENT = DWC_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_IDENT_GET (IDENT,LI)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INCLUDE 'DWARF_4_DEF'
C
	CHARACTER*(*)	IDENT		! (o) login identification
	INTEGER*4	LI		! (o) significant length of IDENT
C
C.Purpose:	Get the login identification
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	warning	DWC_STRTOOSHO	identification string is truncated
C.Notes:
C-------------------------------------------------------------------------
C
C
	INTEGER		MSG_SET
C
C
	IDENT = DWARF$IDENT_C
	IF (DWARF$LENID.GT.LEN(IDENT)) GOTO 999
	LI = DWARF$LENID
C
	DWC_IDENT_GET = DWC_SUCCESS
	RETURN
C
 999	LI = LEN (IDENT)
	DWC_IDENT_GET = MSG_SET (DWC_STRTOOSHO,1)
	CALL WNCTXT(DWLOG,DWMSG,LEN(IDENT))
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_IDENT_PUT (IDENT)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INCLUDE 'DWARF_4_DEF'
C
	CHARACTER*(*)	IDENT		! (i) login identification
C
C.Purpose:	Store the login identification
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	warning	DWC_STRTOOSHO	identification string is truncated
C.Notes:
C-------------------------------------------------------------------------
C
	INTEGER		WNCALN,MSG_SET
C
	INTEGER*4	LI
C
C
	LI = WNCALN (IDENT)
	IF (LI.GT.LEN(DWARF$IDENT_C)) GOTO 999
	IF (LI.NE.0) THEN
		DWARF$IDENT_C = IDENT(:LI)
		DWARF$LENID = LI
	ENDIF
C
	DWC_IDENT_PUT = DWC_SUCCESS
	RETURN
C
 999	DWC_IDENT_PUT = MSG_SET (DWC_STRTOOSHO,1)
	CALL WNCTXT(DWLOG,DWMSG,LEN(DWARF$IDENT_C))
	RETURN
	END
