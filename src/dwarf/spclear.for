C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	SP_CLEAR
C.Keywords:	Program Parameters, External Defaults, Clear
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	900416 FMO - recreation
C.Version:	920214 GvD - no optional arguments in MSG anymore
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION SP_CLEAR (PROGNAM,STREAM)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	PROGNAM		! (i) program name
	CHARACTER*(*)	STREAM		! (i) stream name
C
C.Purpose:	Clear all external defaults for a given program and stream
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	false status code returned by referenced modules
C.Notes:
C	- All symbols containing the default values for input-type program
C	  parameters (listed in the PPD file) are deleted.
C-------------------------------------------------------------------------
C
C
	CHARACTER*(*)	BLANK
		PARAMETER (BLANK = ' ')
C
	INTEGER*4	DWC_TEST_INQ, DWC_SYM_BUILD
	INTEGER*4	PPD_READ_U, PPD_READ_UNXT
	INTEGER*4	PPD_AMAS_GET, PPD_IOCD_GET, PPD_UNAM_GET
	INTEGER*4	SYMBOL_DELETE
C
	CHARACTER	SYMBOL*50, KEY*16, IOCD*8
	INTEGER*4	IS, LS, LK, LMIN, LI
	LOGICAL*4	TEST_MODE, PROTO
C
C
C					Loop through the PPD file
C					- accept only input parameters
C					  and test parameters if in test mode
C
	TEST_MODE = DWC_TEST_INQ ()
	IS = PPD_READ_U (BLANK)
	DO WHILE (IAND(IS,1).NE.0 .AND. IS.NE.PPD_ENDOFFILE)
		IF (TEST_MODE .OR. IAND(PPD_AMAS_GET('TEST'),1).EQ.0) THEN
			IS = PPD_IOCD_GET (IOCD,LI)
			IF (IAND(IS,1).EQ.0) GOTO 999
			IF (INDEX('MI',IOCD(:1)).NE.0) THEN
				IS = PPD_UNAM_GET (KEY,LK,LMIN,PROTO)
				IF (IAND(IS,1).NE.0) IS = DWC_SYM_BUILD (PROGNAM,STREAM,
	1						KEY(:LK),SYMBOL,LS)
				IF (IAND(IS,1).NE.0) IS = SYMBOL_DELETE (SYMBOL(:LS),
	1						DWC__GLOBALSYM)
				IF (IAND(IS,1).EQ.0) GOTO 999
			ENDIF
		ENDIF
		IS = PPD_READ_UNXT ()
	ENDDO
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	SP_CLEAR = DWC_SUCCESS
	RETURN
C
 999	SP_CLEAR = IS
	RETURN
	END
