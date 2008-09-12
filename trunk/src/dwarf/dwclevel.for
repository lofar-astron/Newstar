C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	DWC_LEVEL
C.Keywords:	DWARF, User Level
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		Common variables used:
C	INTEGER*4	DWARF$LEVEL	! (m) user level
C					2 (beginner), 1 (average), 0 (expert)
C
C.Version:	900309 FMO - creation
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_LEVEL ()
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
	DWC_LEVEL = DWC_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_LEVEL_PUT (LEVEL)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	LEVEL		! (i) user level
C
C.Purpose:	Set new user level
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'DWARF_4_DEF'
C
	DWARF$LEVEL = MIN (LEVEL,2)
C
	DWC_LEVEL_PUT = DWC_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_LEVEL_GET (LEVEL,MAXLEVEL)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	LEVEL		! (o) user level
	INTEGER*4	MAXLEVEL	! (o) maximum user level
C
C.Purpose:	Get current user level
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'DWARF_4_DEF'
C
	LEVEL = DWARF$LEVEL
	MAXLEVEL = 2
C
	DWC_LEVEL_GET = DWC_SUCCESS
	RETURN
	END
