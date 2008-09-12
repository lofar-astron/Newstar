C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	DWC_LOGLEVEL
C.Keywords:	DWARF, Log Level
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		Common variables used:
C	INTEGER*4	DWARF$LOGLEVEL	! (m) log level (between 0 and 8)
C
C.Version:	900319 FMO - creation
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_LOGLEVEL ()
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
	DWC_LOGLEVEL = DWC_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_LOGLEVEL_PUT (LEVEL)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	LEVEL		! (i) log level
C
C.Purpose:	Set new log level
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'DWARF_4_DEF'
C
	DWARF$LOGLEVEL = MAX (0,MIN(LEVEL,8))
C
	DWC_LOGLEVEL_PUT = DWC_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_LOGLEVEL_GET (LEVEL,MAXLEVEL)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	LEVEL		! (o) log level
	INTEGER*4	MAXLEVEL	! (o) maximum log level
C
C.Purpose:	Get current log level
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'DWARF_4_DEF'
C
	LEVEL = DWARF$LOGLEVEL
	MAXLEVEL = 8
C
	DWC_LOGLEVEL_GET = DWC_SUCCESS
	RETURN
	END
