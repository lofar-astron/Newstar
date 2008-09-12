C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	PPD_INIT
C.Keywords:	PPD File, Initialize and Exit PPD Operations
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	900415 FMO - recreation
C.Version:	920224 GvD - no optional arguments in MSG anymore
C.Version:	930923 CMV - logical names for new maintenance system
C.Version:	010709 AXC - Linux port - tmpchar in calls
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_INIT (PROGNAM)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	PROGNAM		! (i) program name
C
C.Purpose:	Open and map the PPD file
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success		1	PPD file found in EXEDWARF
C	info	DWC_EXEUSER	PPD file found in EXEUSER
C	false status codes returned by referenced modules
C.Notes:
C	- The file <prognam>.PPD is taken from EXEUSER: (a message will be
C	  written) or from EXEDWARF:.
C	- The status array of the mapped PPF file will be filled.
C-------------------------------------------------------------------------
C
C
	INTEGER*4	PPD_STAT_FILL, PPD_FILE_FIND, PPD_FILE_OPEN
	INTEGER*4	STR_SIGLEN, MSG_SET
C
	CHARACTER*64	FULLSPEC,TMP
	INTEGER*4	IS, ISFOUND, LP, LF
C
C
C					Search for the PPD file
C
	LP = STR_SIGLEN (PROGNAM)
	IS = PPD_FILE_FIND (PROGNAM(:LP),FULLSPEC,LF)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (IS.EQ.DWC_EXEUSER) THEN
		ISFOUND = MSG_SET(DWC_EXEUSER,1)
		TMP=PROGNAM(:LP)//'.PPD'
		CALL WNCTXT(DWLOG,DWMSG,TMP)
	ELSE
		ISFOUND = 1
	ENDIF
C
C					Open and map the file
C
	IS = PPD_FILE_OPEN (FULLSPEC(:LF))
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Fill the status array
C
	IS = PPD_STAT_FILL ()
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C
	PPD_INIT = ISFOUND
	RETURN
C
 999	PPD_INIT = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_EXIT ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Unmap and close the currently open PPD file
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success		1
C	false status codes returned by referenced modules
C.Notes:
C	- The status array of the mapped PPD file will be cleared.
C-------------------------------------------------------------------------
C
	INTEGER*4	PPD_STAT_CLEAR, PPD_FILE_CLOSE
C
	INTEGER*4	IS
C
C
C					Unmap and close the file
C
	IS = PPD_FILE_CLOSE ()
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Clear the status array
C
	IS = PPD_STAT_CLEAR ()
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	PPD_EXIT = 1
	RETURN
C
 999	PPD_EXIT = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_SAVE ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	De-activate the currently open PPD file
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	false status codes returned by referenced modules
C.Notes:
C	This routine de-activates the current PPD file, so that another one
C	can be activated temporarily by a call to PPD_INIT. The original PPD
C	file is re-activated by calling PPD_EXIT and then PPD_RESTORE.	
C-------------------------------------------------------------------------
C
C
	INTEGER*4	PPD_STAT_SAVE
C
	INTEGER*4	IS
C
C
	IS = PPD_STAT_SAVE ()
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	PPD_SAVE = PPD_SUCCESS
	RETURN
C
 999	PPD_SAVE = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_RESTORE ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Re-activate the original PPD file
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	false status codes returned by referenced modules
C.Notes:
C	This routine re-activates the PPD file that has been de-activated by
C	a call to PPD_SAVE.
C-------------------------------------------------------------------------
C
C
	INTEGER*4	PPD_STAT_RESTORE
C
	INTEGER*4	IS
C
C
	IS = PPD_STAT_RESTORE ()
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	PPD_RESTORE = PPD_SUCCESS
	RETURN
C
 999	PPD_RESTORE = IS
	RETURN
	END
