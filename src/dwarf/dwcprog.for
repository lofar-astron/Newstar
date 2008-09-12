C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	DWC_PROG
C.Keywords:	DWARF, Program Name
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C	The program name and its significant length are kept in local saved
C	variables. Their initial values are 'NN' and 2, respectively.
C.Version:	900227 FMO - creation
C.Version:	910826 FMO - PARM_n no longer needed
C.Version:	920214 GvD - no optional arguments in MSG anymore
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER FUNCTION DWC_PROG_PUT (PROG)
C	        ENTRY    DWC_PROG_GET (PROG,LP)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INTEGER		DWC_PROG_GET
C
	CHARACTER*(*)	PROG		!(i/o) program name
	INTEGER		LP		!(o) significant length of PROG
C
C.Purpose:	Check and store, or get the program name
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS	also for blank program name (PUT)
C	error	DWC_LOKILLIMG	illegal program name (PUT)
C	warning	DWC_STRTOOSHO	PROG is too short, message stored (GET)
C.Notes:
C	PUT:
C	- First, PROG is converted to uppercase.
C	- A valid program name consists of an alphabetic character followed by
C	  at most 8 alpha-numeric characters.
C	- If the program name is blank, the current name is kept.
C	GET:
C	- In case of string overflow the truncated name (and its length)
C	  will be returned.
C-------------------------------------------------------------------------
C
C
	INTEGER		DWC_PROG_CHECK, STR_UPCASE, MSG_SET  
C
	CHARACTER*16	UPCPROG
	INTEGER		IS, LUP
	LOGICAL		IS_GLOBAL
C
	CHARACTER*12	PROGNAM
	INTEGER		LPROGNAM
		DATA PROGNAM  /'NN'/
		DATA LPROGNAM /2/
		SAVE PROGNAM, LPROGNAM
C
C
	UPCPROG = PROG
	IS = STR_UPCASE (UPCPROG)
	IS = DWC_PROG_CHECK (UPCPROG,LUP,IS_GLOBAL)
	IF (IAND(IS,1).EQ.0) GOTO 991
C
	IF (LUP.GT.0) THEN
		PROGNAM = UPCPROG(:LUP)
		LPROGNAM = LUP
	ENDIF
C
	DWC_PROG_PUT = DWC_SUCCESS
	RETURN
C
 991	DWC_PROG_PUT = IS
	RETURN
C
C	==================
	ENTRY DWC_PROG_GET (PROG,LP)
C	==================
C
	PROG = PROGNAM(:LPROGNAM)
	IF (LEN(PROG).LT.LPROGNAM) GOTO 992
	LP = LPROGNAM
C
	DWC_PROG_GET = DWC_SUCCESS
	RETURN
C
 992	LP = LEN (PROG)
	DWC_PROG_GET = MSG_SET (DWC_STRTOOSHO,1)
	CALL WNCTXT(DWLOG,DWMSG,LP)
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER FUNCTION DWC_PROG_CHECK (PROG,LP,IS_GLOBAL)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	PROG		!(i) program name
	INTEGER		LP		!(o) significant length of PROG
	LOGICAL		IS_GLOBAL	!(o) global DWARF program name ?
C
C.Purpose:	Check program name
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS	also for blank program name
C	error	DWC_LOKILLIMG	illegal program name (messages stored)
C.Notes:
C	- A valid program name consists of an alphabetic character followed by
C	  at most 8 alpha-numeric characters.
C	- GLOBAL and DWARF are global program names.
C-------------------------------------------------------------------------
C
C
	CHARACTER*(*)	GLOBAL_LIST
	INTEGER		MAXLP
		PARAMETER (GLOBAL_LIST = 'GLOBAL,DWARF')
		PARAMETER (MAXLP = 9)
C
	INTEGER		STR_SIGLEN, STR_CHECK_ANUMA, STR_MATCH_L
	INTEGER		MSG_SET  
C
	INTEGER		IS, NR
C
C
	IS_GLOBAL = .FALSE.
	LP = STR_SIGLEN (PROG)
	IF (LP.GT.0) THEN
		IF (LP.GT.MAXLP) GOTO 999
		IS = STR_CHECK_ANUMA (PROG(:LP))
		IF (IAND(IS,1).EQ.0) GOTO 999
		IS = STR_MATCH_L (PROG(:LP),GLOBAL_LIST,NR)
		IS_GLOBAL = IS.EQ.1
	ENDIF
C
	DWC_PROG_CHECK = DWC_SUCCESS
	RETURN
C
 999	DWC_PROG_CHECK = MSG_SET (DWC_LOKILLIMG,1)
	CALL WNCTXT(DWLOG,DWMSG,PROG(:LP))
	RETURN
	END
