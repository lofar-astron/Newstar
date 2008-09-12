C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	BPD_WRITE
C.Keywords:	PPD File, Build, Write the PPD File
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	900415 FMO - recreation
C.Version:	920224 GvD - no optional arguments in MSG anymore
C.Version:	930923 CMV - logical names for new maintenance system
C.Version:      010907 AXC - linuxport - tmpchar in calls
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION BPD_WRITE (PROGNAM,PASSNR)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	PROGNAM		! (i) program name
	INTEGER*4	PASSNR		! (i) compilation pass nr
C
C.Purpose:	Write the PPD file to disk
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_NOSUCCESS	compilation errors found
C	false status codes returned by referenced modules
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDREC_4_DEF'
C
	CHARACTER*(*)	OBJTYPE
	INTEGER*4	RECLEN
		PARAMETER (OBJTYPE = '.PPD')
		PARAMETER (RECLEN = 512)
C
	INTEGER*4	BPD_INDEX_SORT	,BPD_INDEX_UNIQ
	INTEGER*4	BPD_INDEX_INQ	,BPD_INDEX_WRITE
	INTEGER*4	BPD_PARM_INQ	,BPD_PARM_WRITE
	INTEGER*4	BPD_HELP_INQ	,BPD_HELP_WRITE
	INTEGER*4	BPD_PROTO_INQ	,BPD_PROTO_WRITE
	INTEGER*4	CPL_OBJ_OPEN	,CPL_OBJ_WRITE	,CPL_OBJ_CLOSE
	INTEGER*4	CPL_OBJ_DELETE	,CPL_ERR_GETSUM
	INTEGER		STR_SIGLEN, MSG_SET, FILNAM_FULL
C
	CHARACTER*80	FILESPEC,TMP
	INTEGER*4	IS, NENTRIES, NBYTES, NERR, NWARN, LP, LF
C
C
C					Delete the 1-st pass PPD file
C
	IF (PASSNR.GT.1) THEN
		IS = CPL_OBJ_DELETE ()					
		IF (IAND(IS,1).EQ.0) GOTO 999
	ENDIF
C
C					If compilation errors: return
C
	IS = CPL_ERR_GETSUM (NERR,NWARN)
	IF (IAND(IS,1).NE.0 .AND. NERR.GT.0)
	1				IS = MSG_SET (PPD_NOSUCCESS,0)
	IF (IAND(IS,1).EQ.0) GOTO 999
C		
C					Finalize the index buffer
C					- sort on ascending program's name
C					- determine and store for each parameter
C					  the minimum nr of characters that
C					  uniquely identify the user's name
C
	IS = BPD_INDEX_SORT ()
	IF (IAND(IS,1).NE.0) IS = BPD_INDEX_UNIQ ()
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Build the file description block
C
	NBYTES = PPDFD__LENGTH*4
	PPDFD$STOT = ((NBYTES-1)/RECLEN+1)*RECLEN
C
	IS = BPD_INDEX_INQ (NENTRIES,NBYTES)
	IF (IAND(IS,1).EQ.0) GOTO 999
	PPDFD$NINDEX = NENTRIES
	IF (NENTRIES.GT.0) THEN
		PPDFD$INDEX = PPDFD$STOT
		PPDFD$SINDEX = ((NBYTES-1)/RECLEN+1)*RECLEN
		PPDFD$STOT = PPDFD$STOT+PPDFD$SINDEX
	ELSE
		PPDFD$INDEX = UNDEF_J
		PPDFD$SINDEX = 0
	ENDIF
C
	IS = BPD_PARM_INQ (NENTRIES,NBYTES)
	IF (IAND(IS,1).EQ.0) GOTO 999
	PPDFD$NPARM = NENTRIES
	IF (NENTRIES.GT.0) THEN
		PPDFD$PARM = PPDFD$STOT
		PPDFD$SPARM = ((NBYTES-1)/RECLEN+1)*RECLEN
		PPDFD$STOT = PPDFD$STOT+PPDFD$SPARM
	ELSE
		PPDFD$PARM = UNDEF_J
		PPDFD$SPARM = 0
	ENDIF
C
	IS = BPD_HELP_INQ (NENTRIES,NBYTES)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (NENTRIES.GT.0) THEN
		PPDFD$HELP = PPDFD$STOT
		PPDFD$SHELP = ((NBYTES-1)/RECLEN+1)*RECLEN
		PPDFD$STOT = PPDFD$STOT+PPDFD$SHELP
	ELSE
		PPDFD$HELP = UNDEF_J
		PPDFD$SHELP = 0
	ENDIF
C
	IS = BPD_PROTO_INQ (NENTRIES,NBYTES)
	IF (IAND(IS,1).EQ.0) GOTO 999
	PPDFD$NPARMPT = NENTRIES
	IF (NENTRIES.GT.0) THEN
		PPDFD$PARMPT = PPDFD$STOT
		PPDFD$SPARMPT = ((NBYTES-1)/RECLEN+1)*RECLEN
		PPDFD$STOT = PPDFD$STOT+PPDFD$SPARMPT
	ELSE
		PPDFD$PARMPT = UNDEF_J
		PPDFD$SPARMPT = 0
	ENDIF
C
	LP = STR_SIGLEN (PROGNAM)
	PPDFD$IMAGE = PROGNAM(:LP)
C
C					Open the object file in 
C					  the current directory
C
	TMP=PROGNAM(:LP)//OBJTYPE
	IS = FILNAM_FULL (TMP,FILESPEC,LF,' ')
	IF (IAND(IS,1).NE.0) IS = CPL_OBJ_OPEN (FILESPEC(:LF),RECLEN)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Write the file description block
C					and the buffers to the file
C
	IS = CPL_OBJ_WRITE (PPDFD_,PPDFD__LENGTH*4)
	IF (IAND(IS,1).NE.0) IS = BPD_INDEX_WRITE ()
	IF (IAND(IS,1).NE.0) IS = BPD_PARM_WRITE ()
	IF (IAND(IS,1).NE.0) IS = BPD_HELP_WRITE ()
	IF (IAND(IS,1).NE.0) IS = BPD_PROTO_WRITE ()
	IF (IAND(IS,1).EQ.0) GOTO 998
C
C
	BPD_WRITE = PPD_SUCCESS
	IS = CPL_OBJ_CLOSE ()
	RETURN
C
 998	BPD_WRITE = IS
	IS = CPL_OBJ_DELETE ()
	RETURN
C
 999	BPD_WRITE = IS
	RETURN
	END
