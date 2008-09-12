C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	CLI_STR
C.Keywords:	Command Line Interpreter, String Buffer
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C	All argument names, values and prompt strings are kept in a local
C	buffer in this module. They can only be accessed via the entry points
C	provided: INIT, PUT, GET
C.Version:	881205 FMO - creation
C.Version:	900420 FMO - expand comments
C.Version:	910913 FMO - double the buffer size
C.Version:	920214 GvD - no optional arguments in MSG anymore
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION CLI_STR ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	CLI_STR_INIT	! ()
	INTEGER*4	CLI_STR_PUT	! (ID,STRING,LSTR)
	INTEGER*4	CLI_STR_GET	! (ID,STRING,LSTR)
C
	INTEGER*4	ID		! (m) string identifier
	CHARACTER*(*)	STRING		! (i/o) string
	INTEGER*4	LSTR		! (i/o) significant length of STRING
C
C.Purpose:	Manipulate the CLI-string buffer
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	warning	DWC_STRTOOSHO	output string truncated (GET)
C	error	DWC_CLISTRMAX	too many CLI strings (PUT)
C	error	DWC_CLISTROVR	CLI-string buffer overflow (PUT)
C	fatal	DWC_CLISTRINV	invalid ID (PUT and GET) -> ABORT
C.Notes:
C	- Each string in the buffer has an identifier. The caller must declare
C	  these identifiers as INTEGER*4 variables with initial value 0, and
C	  carry them around (never re-assign).
C	- The module can manage up to 40 CLI strings with a total length of up
C	  to 1024 characters.
C	- If the module gets an invalid ID, the program will be aborted.
C	- CLI strings are stored/retrieved literally, e.g. trailing blanks and
C	  tabs are considered to be significant.
C
C	INIT
C	- Clears the buffer and returns with DWC_SUCCESS.
C
C	PUT
C	- If ID = 0, STRING(:LSTR) and LSTR will be stored in the buffer and
C	  ID will receive a value, provided that LSTR > 0.
C	- If ID > 0, the associated string will be removed from the buffer.
C	  If LSTR > 0, STRING(:LSTR) and LSTR will be stored in its place.
C	  Otherwise, ID will be reset to 0.
C
C	GET
C	- If ID > 0, the associated string and its length will be copied from
C	  the buffer into STRING and LSTR. If STRING is too short, the string
C	  will be truncated.
C	- If ID = 0, STRING will be made blank and LSTR will be set to 0.
C-------------------------------------------------------------------------
C
C
C					Definition of the string buffer
C
	INTEGER*4	MAXID, BUFSIZ
	CHARACTER*(*)	BLANK
		PARAMETER (MAXID  = 40 )		! max nr of ID's
		PARAMETER (BUFSIZ = 1024)		! buffer size (bytes)
		PARAMETER (BLANK  = ' ')
	CHARACTER	BUF*(BUFSIZ)
	INTEGER*4	LBUF, OFFS(MAXID), LENG(MAXID)
		SAVE BUF, LBUF, OFFS, LENG
		DATA BUF  /BLANK  /			! string buffer
		DATA LBUF /0      /			! used length of buffer
		DATA OFFS /MAXID*0/			! offsets of strings
		DATA LENG /MAXID*0/			! lengths of strings
C
	INTEGER*4	CLEAR_BLJ, STR_COPY
	INTEGER		MSG_SET
C
	CHARACTER	WORK*(BUFSIZ)
	INTEGER*4	IS, TMP
C
C
C	ENTRY CLI_STR ()				! Dummy entry point
C	=============
C
	CLI_STR = DWC_SUCCESS
	RETURN
C
C
	ENTRY CLI_STR_INIT ()				! Initialize buffer
C	==================
C
	IS = CLEAR_BLJ (OFFS,MAXID)
	IS = CLEAR_BLJ (LENG,MAXID)
	BUF = BLANK
	LBUF = 0
C
	CLI_STR_INIT = DWC_SUCCESS
	RETURN
C
C
	ENTRY CLI_STR_PUT (ID,STRING,LSTR)		! Put string in buffer
C	=================
C
C
	IF (ID.LT.0 .OR. ID.GT.MAXID) GOTO 9999
C
C					If no old string present:
C					- if new string given:
C					  put it in first free slot
C					  and return the slot nr in ID	
C					- otherwise:
C					  just return with success status
C
	IF (ID.EQ.0) THEN
		IF (LSTR.GT.0) THEN
			ID = 1
			DO WHILE (ID.LE.MAXID .AND. LENG(ID).GT.0)
				ID = ID+1
			ENDDO
			IF (ID.GT.MAXID) GOTO 9991
			OFFS(ID) = LBUF
			LENG(ID) = LSTR
			IS = STR_COPY (STRING(:LSTR),BUF,LBUF)
			IF (IS.LT.0) GOTO 9992
		ENDIF
C
C					Otherwise:
C					- remove the old string
C					  and update the offset array
C					- put the new string in the old slot
C					  or clear the slot and return ID=0
C
	ELSE
		IF (OFFS(ID)+LENG(ID).EQ.LBUF) THEN
			LBUF = OFFS(ID)
		ELSE
			WORK = BUF(OFFS(ID)+LENG(ID)+1:LBUF)
			BUF(OFFS(ID)+1:) = WORK
			LBUF = LBUF-LENG(ID)
			DO TMP = 1,MAXID
				IF (OFFS(TMP).GT.OFFS(ID))
	1				OFFS(TMP) = OFFS(TMP)-LENG(ID)
			ENDDO
		ENDIF
C
		IF (LSTR.GT.0) THEN
			OFFS(ID) = LBUF
			LENG(ID) = LSTR
			IS = STR_COPY (STRING(:LSTR),BUF,LBUF)
			IF (IS.LT.0) GOTO 9992
		ELSE
			OFFS(ID) = 0
			LENG(ID) = 0
			ID = 0
		ENDIF
	ENDIF
C
	CLI_STR_PUT = DWC_SUCCESS
	RETURN
C
 9991	CLI_STR_PUT = MSG_SET (DWC_CLISTRMAX,1)
	CALL WNCTXT(DWLOG,DWMSG,MAXID)
	ID = 0
	RETURN
C
 9992	CLI_STR_PUT = MSG_SET (DWC_CLISTROVR,1)
	CALL WNCTXT(DWLOG,DWMSG,BUFSIZ)
	RETURN
C
C
	ENTRY CLI_STR_GET (ID,STRING,LSTR)		! Get string from buffer
C	==================
C
	IF (ID.LT.0 .OR. ID.GT.MAXID) GOTO 9999
C
	STRING = BLANK
	LSTR = 0
	IF (ID.GT.0 .AND. LENG(ID).GT.0) THEN
		IS = STR_COPY (BUF(OFFS(ID)+1:OFFS(ID)+LENG(ID)),STRING,LSTR)
		IF (IS.LT.0) GOTO 9993
	ENDIF
	CLI_STR_GET = DWC_SUCCESS
	RETURN
C
 9993	CLI_STR_GET = MSG_SET (DWC_STRTOOSHO,1)
	CALL WNCTXT(DWLOG,DWMSG,LEN(STRING))
	RETURN
C
 9999	CLI_STR = MSG_SET (DWC_CLISTRINV,1)
	CALL WNCTXT(DWLOG,DWMSG,ID)
	CALL WNGEX
	RETURN
	END
