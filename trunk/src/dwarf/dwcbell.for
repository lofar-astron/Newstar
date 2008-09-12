C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	DWC_BELL
C.Keywords:	DWARF, Bell Switch
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		Common fields used:
C	INTEGER*4	DWARF$BELL	! (m) bell enabled ?
C
C.Version:	900318 FMO - creation
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_BELL ()
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
	DWC_BELL = DWC_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_BELL_PUT (SWITCH)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	LOGICAL*4	SWITCH		! (i) switch ON ?
C
C.Purpose:	Enable or disable DWARF's bell signal
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'DWARF_4_DEF'
C
C
	IF (SWITCH) THEN
		DWARF$BELL = 1
	ELSE
		DWARF$BELL = 0
	ENDIF
C
	DWC_BELL_PUT = DWC_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_BELL_INQ ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Is DWARF's bell enabled ?
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success		1	yes
C	warning		0	no
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'DWARF_4_DEF'
C
	DWC_BELL_INQ = DWARF$BELL
	RETURN
	END
