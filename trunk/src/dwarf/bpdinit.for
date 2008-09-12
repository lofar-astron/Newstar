C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	BPD_INIT
C.Keywords:	PPD File, Build, Initialize
C.Author:	Friso Olnon Kombrink (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	900415 FMO - recreation
C.Version:	930923 CMV - logical names for new maintenance system
C.Version:      010907 AXC - Linux port - tmpcahr in calls
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION BPD_INIT (PROGNAM,PASSNR)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	PROGNAM		! (i) program name
	INTEGER*4	PASSNR		! (i) compilation pass number
C
C.Purpose:	Open the source file and initialize the compilation buffers
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	false status codes returned by referenced modules
C.Notes:
C	- The source file PROGNAM//'.PIN' will be looked for in the current
C	  directory.
C-------------------------------------------------------------------------
C
C
	CHARACTER*(*)	SRCTYP
		PARAMETER (SRCTYP = '.PIN')
C
	INTEGER*4	CPL_SRC_OPEN, CPL_SRC_REWIND
	INTEGER*4	CPL_ERR_INIT
	INTEGER*4	BPD_INDEX_INIT, BPD_PARM_INIT
	INTEGER*4	BPD_PROTO_INIT, BPD_HELP_INIT
	INTEGER*4	STR_SIGLEN, FILNAM_FULL
C
	CHARACTER*80	FILESPEC, TMP
	INTEGER*4	IS, LP, LF
C
C
	IF (PASSNR.EQ.1) THEN
		LP = STR_SIGLEN (PROGNAM)
		TMP=PROGNAM(:LP)//SRCTYP
		IS = FILNAM_FULL (TMP,FILESPEC,LF,' ')
		IF (IAND(IS,1).NE.0) IS = CPL_SRC_OPEN (FILESPEC(:LF))
	ELSE
		IS = CPL_SRC_REWIND ()
	ENDIF
	IF (IAND(IS,1).NE.0) IS = CPL_ERR_INIT ()
	IF (IAND(IS,1).NE.0) IS = BPD_INDEX_INIT ()
	IF (IAND(IS,1).NE.0) IS = BPD_PARM_INIT ()
	IF (IAND(IS,1).NE.0) IS = BPD_PROTO_INIT ()
	IF (IAND(IS,1).NE.0) IS = BPD_HELP_INIT ()
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	BPD_INIT = PPD_SUCCESS
	RETURN
C
 999	BPD_INIT = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION BPD_EXIT ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Close the source file
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	false status codes returned by referenced modules
C.Notes:
C-------------------------------------------------------------------------
C
C
	INTEGER*4	CPL_SRC_CLOSE
C
	INTEGER*4	IS
C
C
	IS = CPL_SRC_CLOSE ()
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	BPD_EXIT = PPD_SUCCESS
	RETURN
C
 999	BPD_EXIT = IS
	RETURN
	END


















