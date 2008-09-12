C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	DWC_IBMODE
C.Keywords:	DWARF, Execution Mode
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		Common variables used:
C	INTEGER*4	DWARF$IBMODE	! (m) execution mode
C						(0 = interactive, 1 =batch)
C
C.Version:	900302 FMO - creation
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_IBMODE ()
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
	DWC_IBMODE = DWC_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_IBMODE_PUT (IBMODE)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	IBMODE		! (i) execution mode
C
C.Purpose:	Store execution mode
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C.Notes:
C	- IBMODE = 0 (interactive) or = 1 (batch).
C-------------------------------------------------------------------------
C
	INCLUDE 'DWARF_4_DEF'
C
	DWARF$IBMODE = IBMODE
C
	DWC_IBMODE_PUT = DWC_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_IBMODE_INQ (IBMODE)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	IBMODE		! (i) execution mode
C
C.Purpose:	Is IBMODE the current execution mode ?
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success		1	yes
C	warning		0	no
C	error		2	invalid mode (no message stored)
C.Notes:
C	- IBMODE can be 'INTERACTIVE', 'BATCH' or abbreviations.
C-------------------------------------------------------------------------
C
	INCLUDE 'DWARF_4_DEF'
C
	CHARACTER*(*)	MODE_LIST
		PARAMETER (MODE_LIST = 'INTERACTIVE,BATCH')
C
	INTEGER*4	STR_MATCH_L
C
	INTEGER*4	IS, NR
C
C
	IS = STR_MATCH_L (IBMODE,MODE_LIST,NR)
	IF (NR.EQ.0) THEN
		DWC_IBMODE_INQ = 2
	ELSE IF ((NR.EQ.1 .AND. DWARF$IBMODE.EQ.0) .OR.
	1	 (NR.EQ.2 .AND. DWARF$IBMODE.EQ.1)) THEN
		DWC_IBMODE_INQ = 1
	ELSE
		DWC_IBMODE_INQ = 0
	ENDIF
C
	RETURN
	END
