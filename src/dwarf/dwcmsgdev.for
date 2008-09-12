C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	DWC_MSGDEV
C.Keywords:	DWARF, Message Device
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		Common variables used:
C	INTEGER*4	DWARF$MSGDEV	! (m) message device
C			= 0 (terminal), = 1 (printer) or = 2 (both).
C
C.Version:	900319 FMO - creation
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_MSGDEV ()
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
	DWC_MSGDEV = DWC_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_MSGDEV_PUT (MSGDEV)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	MSGDEV		! (i) message device
C
C.Purpose:	Store message device
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'DWARF_4_DEF'
C
	DWARF$MSGDEV = MSGDEV
C
	DWC_MSGDEV_PUT = DWC_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_MSGDEV_INQ (MSGDEV)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	MSGDEV		! (i) message device
C
C.Purpose:	Is MSGDEV the current message device ?
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success		1	yes
C	warning		0	no
C	error		2	invalid mode (no message stored)
C.Notes:
C	- MSGDEV can be 'TERMINAL', 'PRINTER' or abbreviations.
C-------------------------------------------------------------------------
C
	INCLUDE 'DWARF_4_DEF'
C
	CHARACTER*(*)	DEV_LIST
		PARAMETER (DEV_LIST = 'TERMINAL,PRINTER')
C
	INTEGER*4	STR_MATCH_L
C
	INTEGER*4	IS, NR
C
C
	IS = STR_MATCH_L (MSGDEV,DEV_LIST,NR)
	IF (NR.EQ.0) THEN
		DWC_MSGDEV_INQ = 2
	ELSE IF ((NR.EQ.1 .AND. DWARF$MSGDEV.NE.1) .OR.
	1	 (NR.EQ.2 .AND. DWARF$MSGDEV.NE.0)) THEN
		DWC_MSGDEV_INQ = 1
	ELSE
		DWC_MSGDEV_INQ = 0
	ENDIF
C
	RETURN
	END
