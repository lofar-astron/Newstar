C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	SP_COPY
C.Keywords:	Program Parameters, External Defaults, Copy
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	900416 FMO - recreation
C.Version:	920214 GvD - no optional arguments in MSG anymore
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION SP_COPY (OUTPROG,OUTSTREAM,INPROG,INSTREAM)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	OUTPROG		! (i) name of output program
	CHARACTER*(*)	OUTSTREAM	! (i) name of output stream
	CHARACTER*(*)	INPROG		! (i) name of input program
	CHARACTER*(*)	INSTREAM	! (i) name of input stream
C
C.Purpose:	Copy the external defaults from one prognam$stream to another
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	false status codes returned by referenced routines
C.Notes:
C-------------------------------------------------------------------------
C
C
	CHARACTER*(*)	BLANK
		PARAMETER (BLANK = ' ')
C
	INTEGER*4	DWC_TEST_INQ, DWC_SYM_BUILD
	INTEGER*4	PPD_READ_U, PPD_READ_UNXT
	INTEGER*4	PPD_AMAS_GET, PPD_IOCD_GET, PPD_UNAM_GET
	INTEGER*4	SYMBOL_GET, SYMBOL_DEFINE
C
	CHARACTER	WORK*255, SYMBOL*50, KEY*16, IOCD*8
	INTEGER*4	IS, LW, LS, LK, LMIN, LI
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
		    IF (IAND(IS,1).NE.0) IS = DWC_SYM_BUILD (INPROG,INSTREAM,
	1						KEY(:LK),SYMBOL,LS)
		    IF (IAND(IS,1).NE.0) IS = SYMBOL_GET (SYMBOL(:LS),WORK,LW)
		    IF (IAND(IS,1).NE.0 .AND. LW.GT.0) THEN
			IS = DWC_SYM_BUILD (OUTPROG,OUTSTREAM,
	1						KEY(:LK),SYMBOL,LS)
			IF (IAND(IS,1).NE.0) IS = SYMBOL_DEFINE (SYMBOL(:LS),
	1					WORK(:LW),DWC__GLOBALSYM)
		    ENDIF
		    IF (IAND(IS,1).EQ.0) GOTO 999
		ENDIF
	    ENDIF
	    IS = PPD_READ_UNXT ()
	ENDDO
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	SP_COPY = DWC_SUCCESS
	RETURN
C
 999	SP_COPY = IS
	RETURN
	END
