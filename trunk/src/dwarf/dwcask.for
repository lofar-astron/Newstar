C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	DWC_ASK
C.Keywords:	DWARF, Ask Switch
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		Common fields used:
C	INTEGER*4	DWARF$ASK	! (m) ask enabled ?
C
C.Version:	900318 FMO - creation
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_ASK ()
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
	DWC_ASK = DWC_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_ASK_PUT (SWITCH)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	LOGICAL*4	SWITCH		! (i) switch ON ?
C
C.Purpose:	Enable or disable DWARF's ask
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'DWARF_4_DEF'
C
C
	IF (SWITCH) THEN
		DWARF$ASK = 1
	ELSE
		DWARF$ASK = 0
	ENDIF
C
	DWC_ASK_PUT = DWC_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_ASK_INQ ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Is DWARF's ask enabled ?
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success		1	yes
C	warning		0	no
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'DWARF_4_DEF'
C
	DWC_ASK_INQ = DWARF$ASK
	RETURN
	END
