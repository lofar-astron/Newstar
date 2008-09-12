C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	DWC_SAVE
C.Keywords:	DWARF, Save Switch
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		Common fields used:
C	INTEGER*4	DWARF$SAVELAST	! (m) in save mode ?
C
C.Version:	900319 FMO - creation
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_SAVE ()
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
	DWC_SAVE = DWC_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_SAVE_PUT (SWITCH)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	LOGICAL*4	SWITCH		! (i) switch save mode on ?
C
C.Purpose:	Enable or disable save mode
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'DWARF_4_DEF'
C
C
	IF (SWITCH) THEN
		DWARF$SAVELAST = 1
	ELSE
		DWARF$SAVELAST = 0
	ENDIF
C
	DWC_SAVE_PUT = DWC_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_SAVE_INQ ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Inquire whether DWARF is in save mode
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success		1	yes
C	warning		0	no
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'DWARF_4_DEF'
C
	DWC_SAVE_INQ = DWARF$SAVELAST
	RETURN
	END
