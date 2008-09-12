C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	DWC_PRCMODE
C.Keywords:	DWARF, Process Mode
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C	The process mode is kept in a local saved variable with value
C	PARM__SUBPROC (=1) for a subprocess, or
C	PARM__MAINPROC (=0) for a main process
C.Version:	900302 FMO - creation
C.Version:	910826 FMO - PARM_n no longer needed
C.Version:	940119 CMV - Remove check on owner and PARM__
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER FUNCTION DWC_PRCMODE_SET ()
C	        ENTRY    DWC_PRCMODE_INQ (PRCMODE)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INTEGER		DWC_PRCMODE_INQ
C
	CHARACTER*(*)	PRCMODE		! (i) process mode
C
C.Purpose:	Set process mode or inquire after the current mode
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS	for SET
C	success		1	for INQ if PRCMODE is the current mode
C	warning		0	for INQ if PRCMODE is not the current mode
C	error		2	for INQ if PRCMODE is an invalid mode (no msg)
C	false status returned by referenced routine (for SET)
C.Notes:
C	- PRCMODE can be 'SUBPROCESS', 'MAINPROCESS' or abbreviations.
C-------------------------------------------------------------------------
C
C
	CHARACTER*(*)	MODE_LIST
		PARAMETER (MODE_LIST = 'SUBPROCESS,MAINPROCESS')
C
	INTEGER		DWC_IBMODE_INQ, STR_MATCH_L
C
	INTEGER		IS, NR
	INTEGER		PROCESS_MODE
		DATA PROCESS_MODE /0/
		SAVE PROCESS_MODE
C
C
	PROCESS_MODE = 0
	DWC_PRCMODE_SET = DWC_SUCCESS
	RETURN
C
C	=====================
	ENTRY DWC_PRCMODE_INQ (PRCMODE)
C	=====================
C
	IS = STR_MATCH_L (PRCMODE,MODE_LIST,NR)
	IF (NR.EQ.0) THEN
		DWC_PRCMODE_INQ = 2
	ELSE IF ((NR.EQ.1 .AND. PROCESS_MODE.EQ.1) .OR.
	1	 (NR.EQ.2 .AND. PROCESS_MODE.EQ.0)) THEN
		DWC_PRCMODE_INQ = 1
	ELSE
		DWC_PRCMODE_INQ = 0
	ENDIF
C
	RETURN
	END
