C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	BPD_COMPILE
C.Keywords:	PPD File, Build, Compilation
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	900415 FMO - recreation
C.Version:	930923 CMV - logical names for new maintenance system
C.Version:      010709 AXC - Linux port - tmpchar in calls
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION BPD_COMPILE (PROGNAM,DO_LIST,PRTFLAGS,
	1							DO_UPDATE)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	PROGNAM		! (i) program name
	LOGICAL*4	DO_LIST		! (i) full compilation listing ?
	INTEGER*4	PRTFLAGS	! (i) disposition of listing
	LOGICAL*4	DO_UPDATE	! (i) update mode ?
C
C.Purpose:	Compile the PIN file into a PPD file
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	false status codes returned by referenced modules
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'BLDPPD_2_DEF'
C
	CHARACTER*(*)	LISTYPE
		PARAMETER (LISTYPE = '.LIS')
C
C					Parameter-definition keywords
C					and corresponding work indices
C
	INTEGER*4	NPAR
		PARAMETER (NPAR = 22)
	CHARACTER*16	PAR(NPAR)
	INTEGER*4	WPAR(NPAR)
		DATA PAR /
	1		'USER_PARAMETER','KEYWORD'	,'COPY'		,
	2		'PROG_PARAMETER','DATA_TYPE'	,'IO'		,
	3		'LENGTH'	,'NVALUES'	,'MAX_NSETS'	,
	4		'MIN_NVALUES'	,'MAX_NVALUES'	,'CHECKS'	,
	5		'ATTRIBUTES'	,'SWITCHES'	,'MINIMUM'	,
	6		'MAXIMUM'	,'UNITS'	,'SEARCH'	,
	7		'DEFAULTS'	,'OPTIONS'	,'PROMPT'	,
	8		'HELP' /
		DATA WPAR /
	1		W_UNAM		,W_UNAM		,W_COPY		,
	2		W_PNAM		,W_DTYPE	,W_IOCD		,
	3		W_PLEN		,W_NVAL		,W_NSETS	,
	4		W_MNVAL		,W_MXVAL	,W_CMAS		,
	5		W_AMAS		,W_AMAS		,W_MIN		,
	6		W_MAX		,W_USTR		,W_SSTR		,
	7		W_DVSTR		,W_OPSTR	,W_PRSTR	,
	8		W_HSTR /
C
	INTEGER*4	BPD_INIT, CPL_READ, BPD_BUILD, BPD_STORE
	INTEGER*4	BPD_WRITE, CPL_LIST, BPD_EXIT
	INTEGER*4	FILNAM_FULL, STR_SIGLEN
C
	CHARACTER*16	STARTKEY(2), NEXTKEY, FILESPEC*80, TMPC*80
	INTEGER*4	IS, TMP, PASSNR, NSTART, LP, LL1, LF
	LOGICAL*4	EOF, INTREF, FILL_WORK
C
C
C					Initialize
C					- set compilation pass nr
C					- set change-of-group keywords
C
	PASSNR = 1
	NSTART = 2
	STARTKEY(1) = PAR(1)
	STARTKEY(2) = PAR(2)
	LP = STR_SIGLEN (PROGNAM)
C
C					- open the source file
C					- initialize the compilation buffers
C
 100	IS = BPD_INIT (PROGNAM(:LP),PASSNR)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Process the parameter definitions
C					one by one
C					- read the group of source lines
C					- build the parameter description
C					- store the description
C
	EOF = .FALSE.
	INTREF = .FALSE.
	FILL_WORK = .TRUE.
	DO WHILE (.NOT.EOF)
		IS = CPL_READ (NPAR,PAR,WPAR,FILL_WORK,NSTART,STARTKEY,NEXTKEY)
		EOF = IS.EQ.CPL_SRCEOF
		IF (IAND(IS,1).NE.0)
	1		IS = BPD_BUILD (PROGNAM(:LP),PASSNR,DO_UPDATE)
		IF (IAND(IS,1).NE.0) THEN
			IF (IS.EQ.PPD_INTREF) THEN
				INTREF = .TRUE.
			ELSE
				IS = BPD_STORE ()
			ENDIF
		ENDIF
		IF (IAND(IS,1).EQ.0) GOTO 999
	ENDDO
C
C					Write the PPD file
C
	IS = BPD_WRITE (PROGNAM(:LP),PASSNR)

C
C					If the compilation was successful
C					and internal references were found:
C					- do a second compilation pass
C
	IF (IAND(IS,1).NE.0 .AND. INTREF) THEN
		PASSNR = 2
		GOTO 100
	ENDIF
C
C					Make the compilation listing
C					- put in current directory
C					- also if compilation errors were found
C
	IF (IAND(IS,1).NE.0 .OR. IS.EQ.PPD_NOSUCCESS) THEN
	        LL1=LP
	        DO WHILE (LL1.GT.1.AND.PROGNAM(LL1:LL1).NE.'/'
	1	                  .AND.PROGNAM(LL1:LL1).NE.']'
	1	                  .AND.PROGNAM(LL1:LL1).NE.':')
		   LL1=LL1-1
		END DO
		TMPC = PROGNAM(LL1:LP)//LISTYPE
		TMP = FILNAM_FULL (TMPC,FILESPEC,LF,' ')
		TMP = CPL_LIST (FILESPEC(:LF),DO_LIST,PRTFLAGS)
	ENDIF
C
C					Close the source file
C
 999	BPD_COMPILE = IS
	IS = BPD_EXIT ()
	RETURN
	END
