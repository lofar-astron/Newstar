C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	GENSW_FORIOS
C.Keywords:	Fortran I/O errors
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	SUN
C.Comments:	Calls library routines IERRNO and GERROR
C.Version:	880421 FMO - created
C.Version:	920214 GvD - no optional arguments in MSG anymore
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GEN_FORIOS (FILE)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	FILE		! (i) file name
C
C.Purpose:	Get Fortran I/O status and store message in case of error
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	GEN_SUCCESS
C	error	GEN_FORIOERR
C.Notes:	An appropriate error message is left in the message buffer
C-------------------------------------------------------------------------
C
C
	INTEGER*4	STR_SIGLEN
	INTEGER*4	IERRNO		! FX/Fortran, return error number
	EXTERNAL	GERROR		! FX/Fortran, return error message
 
	CHARACTER*80	STR
C
C
	IF (IERRNO().GT.0) GOTO 999
C
	GEN_FORIOS = GEN_SUCCESS
	RETURN
C
 999	CALL GERROR(STR)
	GEN_FORIOS = GEN_FORIOERR
	CALL WNCTXT(DWLOG,STR(:STR_SIGLEN(STR))//'!/on file !AS',FILE)
	RETURN
	END
