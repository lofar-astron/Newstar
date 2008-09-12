C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	GENUN_BRDCAST
C.Keywords:	Terminal output
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	UNIX
C.Comments:
C.Version:	880508 FMO - Created
C.Version:	910825 FMO - use PRINT i.s.o. TYPE
C.Version:	920316 GvD - use GEN_OUTPUT iso. PRINT
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GEN_BRDCAST (TEXT)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	TEXT		! (i) text to be written
C
C.Purpose:	Force immediate write of a text to the terminal
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success		1	always
C.Notes:	For the moment just a Fortran PRINT
C-------------------------------------------------------------------------
C
	CALL GEN_OUTPUT (TEXT)
C
	GEN_BRDCAST = 1
	RETURN
	END
