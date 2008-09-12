C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	PPD_LIST
C.Keywords:	PPD File, List
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	900415 FMO - recreation
C.Version:	900502 FMO - new GEN_LUN module
C.Version:	920213 GvD - no optional arguments anymore
C.Version:	930923 CMV - logical names for new maintenance system
C.Version:	940118 CMV - used WNCFOP, WNCALN i.s.o. DWARF stuff
C.Version:	010709 AXC - linux port - tmpchar in calls
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_LIST (PROGNAM,PRTFLAGS)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	PROGNAM		! (i) program name
	INTEGER*4	PRTFLAGS	! (i) disposition flags
C
C.Purpose:	Create a listing of the PPD file
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	false status codes returned by referenced modules
C.Notes:
C-------------------------------------------------------------------------
C
C
	LOGICAL    L__FALSE
	PARAMETER (L__FALSE = .FALSE.)
C
	CHARACTER*(*)	HELPFIL, WILDKEY, LISTYP, PPDTYP
		PARAMETER (HELPFIL = 'HELP.DAT')
		PARAMETER (WILDKEY = '*')
		PARAMETER (LISTYP = '.MLIS')
		PARAMETER (PPDTYP = '.PPD')
C
	INTEGER*4	PPD_INIT, PPD_EXIT, PPD_HELP
	INTEGER*4	FILNAM_FULL, GEN_FORIOS
	INTEGER		WNCALN
C
	CHARACTER	LINE*132, HELPSPEC*80, PPDSPEC*80, TMP*80
	INTEGER*4	IS, ISTMP, HELPLUN, LP, LEVEL, LHS, LPS
	LOGICAL*4	SWP, SWINP, SWPROT
	INTEGER*4	LISTID
		DATA LISTID /-1/
C
C
C					Open and map the PPD file
C
	LP = WNCALN(PROGNAM)
	IS = PPD_INIT (PROGNAM(:LP))
	IF (IAND(IS,1).EQ.0) GOTO 999
	TMP=PROGNAM(:LP)//PPDTYP
	IF (IAND(IS,7).EQ.3) THEN
		IS = FILNAM_FULL (TMP,PPDSPEC,LPS,'n_uexe')
	ELSE
		IS = FILNAM_FULL (TMP,PPDSPEC,LPS,'n_exe')
	ENDIF
	IF (IAND(IS,1).EQ.0) GOTO 998
C
C					Create, fill and rewind the help file
C
	IS = FILNAM_FULL (HELPFIL,HELPSPEC,LHS,' ')
	IF (IAND(IS,1).NE.0) CALL WNGLUN(HELPLUN)
	IF (HELPLUN.EQ.0) GOTO 998
	OPEN (HELPLUN,FILE=HELPSPEC(:LHS),TYPE='NEW',ACCESS='SEQUENTIAL',
	1				FORM='FORMATTED',IOSTAT=ISTMP)
	IF (ISTMP.NE.0.AND.ISTMP.NE.128) THEN
		IS = GEN_FORIOS (HELPSPEC(:LHS))
		GOTO 997
	ENDIF
C
	SWP = .FALSE.			! in PIN order
	SWINP = .FALSE.			! all IO codes allowed
	SWPROT = .TRUE.			! prototypes too
	LEVEL = 3			! maximum info level
	IS = PPD_HELP (WILDKEY,SWP,SWINP,SWPROT,LEVEL,HELPLUN)
	IF (IAND(IS,1).EQ.0) GOTO 996
C
	REWIND HELPLUN
 
C
C					Open the print file
C					and fill the header lines
C
	TMP=PROGNAM(:LP)//LISTYP
	CALL WNCFOP(LISTID,TMP)
	CALL WNCFHD(LISTID,1,'!50CListing of !AS',PPDSPEC(:LPS))
	CALL WNCFSV(LISTID,F_DIS,PRTFLAGS)
C
C					Copy help file to print file
C
	DO WHILE (IAND(IS,1).NE.0)
		READ (HELPLUN,'(A)',IOSTAT=ISTMP,END=200) LINE
		IF (ISTMP.EQ.0) THEN
			CALL WNCTXT(LISTID,'!AS',LINE)
		ELSE
			IS = GEN_FORIOS (HELPSPEC(:LHS))
		ENDIF
	ENDDO
	GOTO 995
C
C					Close the files
C
 200	CALL WNCFCL(LISTID)
	IF (IAND(IS,1).EQ.0) GOTO 995
C
	CLOSE (HELPLUN,DISPOSE='DELETE')
	CALL WNGLUF(HELPLUN)
C
	IS = PPD_EXIT ()
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Return
C
	PPD_LIST = PPD_SUCCESS
	RETURN
C
 995	CALL WNCFCL(LISTID)
 996	CLOSE (HELPLUN,DISPOSE='DELETE')
 997	CALL WNGLUF(HELPLUN)
 998	ISTMP = PPD_EXIT ()
 999	PPD_LIST = IS
	RETURN
	END
