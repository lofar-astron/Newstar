C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	DWC_STR
C.Keywords:	DWARF, String
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	900416 FMO - recreation
C.Version:	920214 GvD - no optional arguments in MSG anymore
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_STR ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Make module name known
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C.Notes:
C-------------------------------------------------------------------------
C
C
	DWC_STR = DWC_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_STR_STANDARD (STRIN,STROUT,LOUT)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRIN		! (i) input string
	CHARACTER*(*)	STROUT		! (o) output string
	INTEGER*4	LOUT		! (o) significant length of STROUT
C
C.Purpose:	Translate a string to a standard DWARF string
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	warning	DWC_STRTOOSHO	output string too short
C.Notes:
C	The translation steps are:
C	- leave quoted substrings unchanged, but append a closing quote if that
C	  is not present;
C	- convert lowercase to uppercase;
C	- convert tabs to blanks;
C	- compress multiple blanks to a single blank;
C	- remove comment (starting with exclamation mark);
C
C	If the output string is too short, a message will be stored in the
C	message buffer and the truncated string will be returned.
C-------------------------------------------------------------------------
C
C
	CHARACTER*(*)	BLANK, TAB, WHITE, QUOTE, EXCLA, SPECIAL
		PARAMETER (BLANK   = ' ')
		PARAMETER (TAB     = '	')
		PARAMETER (WHITE   = BLANK//TAB)
		PARAMETER (QUOTE   = '"')
		PARAMETER (EXCLA   = '!')
		PARAMETER (SPECIAL = WHITE//QUOTE//EXCLA)
C
	INTEGER*4	STR_SIGLEN, STR_UPCASE
	INTEGER*4	STR_SKIP_W, STR_COPY, STR_COPY_U
	INTEGER*4	MSG_SET  
C
	INTEGER*4	IS, LIN, PTR, NCOPY, SAVLOUT, MAXOUT
C
C
C					Copy until the next special
C					character and make uppercase
C
	STROUT = BLANK
	LOUT = 0
	NCOPY=0
	LIN = STR_SIGLEN (STRIN)
	PTR = 1
	DO WHILE (PTR.LE.LIN)
		SAVLOUT = LOUT
		NCOPY = STR_COPY_U (SPECIAL,STRIN(:LIN),PTR,STROUT,LOUT)
		IF (LOUT.GT.SAVLOUT) IS = STR_UPCASE (STROUT(SAVLOUT+1:LOUT))
C
C					End of string or start of comment: stop
C
		IF (PTR.GT.LIN) THEN
		ELSE IF (STRIN(PTR:PTR).EQ.EXCLA) THEN
			PTR = LIN+1
C
C					Quoted substring:
C					- copy it unchanged
C					- add closing quote if necessary
C
		ELSE IF (STRIN(PTR:PTR).EQ.QUOTE) THEN
			NCOPY = STR_COPY (QUOTE,STROUT,LOUT)
			PTR = PTR+1
			NCOPY = STR_COPY_U (QUOTE,STRIN(:LIN),PTR,STROUT,LOUT)
			NCOPY = STR_COPY (QUOTE,STROUT,LOUT)
			PTR = PTR+1
C
C					White substring:
C					- replace by single blank
C					
		ELSE
			NCOPY = STR_COPY (BLANK,STROUT,LOUT)
			PTR = PTR+1
			IS = STR_SKIP_W (WHITE,STRIN(:LIN),PTR)
		ENDIF
	ENDDO
C
C
	IF (NCOPY.LT.0) THEN
		MAXOUT = LEN (STROUT)
		DWC_STR_STANDARD = MSG_SET (DWC_STRTOOSHO,1)
		CALL WNCTXT(DWLOG,DWMSG,MAXOUT)
	ELSE
		DWC_STR_STANDARD = DWC_SUCCESS
	ENDIF
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_STR_SUBST (STRING,STROUT,LOUT,STREAM,
	1						ERRPTR,CHKSW,SWSYM)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRING		! (i) input string
	CHARACTER*(*)	STROUT		! (o) output string
	INTEGER*4	LOUT		! (o) significant length of STROUT
	CHARACTER*(*)	STREAM		! (i) stream name
	INTEGER*4	ERRPTR		! (o) position of error in STROUT
	LOGICAL*4	CHKSW		! (i) unknown symbols allowed
	LOGICAL*4	SWSYM		! (m) unknown symbols found ?
C
C.Purpose:	Substitute all symbols between apostrophes
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	warning	DWC_STRTOOSHO	output string too short
C	error	DWC_NOENDQUO	missing end quote
C	error	DWC_NOENDAPOS	missing end apostroph
C	error	DWC_MUTUALSUB	too many substitutions
C	error	DWC_SYMNOTDEF	symbol not found
C.Notes:
C	- Apostrophes in literals (quoted substrings) are not treated as
C	  as substitution-symbol indicators.
C	- If necessary, the streamnr will be added to a symbol name.
C	- If the value of the symbol itself contains substitution symbols,
C	  they will be substituted, etc.
C	- The max. nr of substitutions is 25 to avoid substitution looping.
C-------------------------------------------------------------------------
C
C
	INTEGER*4	MAXSUB
	CHARACTER*(*)	QUOTE, APOSTR
		PARAMETER (MAXSUB = 25)
		PARAMETER (QUOTE  = '"')
		PARAMETER (APOSTR = '''')
C
	INTEGER*4	DWC_SYM_EXPAND, DWC_SYM_TRANSL
	INTEGER*4	STR_SIGLEN, STR_SKIP_U, STR_COPY, MSG_SET  
C
	CHARACTER*255	VALUE, WORK
	INTEGER*4	LMAX, LVAL, LW
	INTEGER*4	IS, PTR, START, NRSUBS
C
C
	ERRPTR = 0
	NRSUBS = 0
	STROUT = STRING
	LOUT = STR_SIGLEN (STRING)
	LMAX = LEN(STROUT)
	IF (LOUT.GT.LMAX) GOTO 991			! STROUT too short
C
C					Skip until the start of a literal
C					or substitution substring
C
	PTR = 1
	IS = STR_SKIP_U (QUOTE//APOSTR,STROUT(:LOUT),PTR)
	DO WHILE (PTR.LE.LOUT)
		START = PTR
C
C					If literal substring:
C					- skip through it
C
		IF (STROUT(PTR:PTR).EQ.QUOTE) THEN
			PTR = PTR+1
			IS = STR_SKIP_U (QUOTE,STROUT(:LOUT),PTR)
			IF (PTR.GT.LOUT) GOTO 992	! missing end quote
			PTR = PTR+1
C
C					If substitution substring:
C					- extract the symbol name
C					  (error if no end apostroph present)
C					- check the nr of substitutions
C
		ELSE
			PTR = PTR+1
			IS = STR_SKIP_U (APOSTR//QUOTE,STROUT(:LOUT),PTR)
			IF (PTR.GT.LOUT .OR. STROUT(PTR:PTR).EQ.QUOTE) GOTO 993
C
			IF (NRSUBS.GE.MAXSUB) GOTO 994	! too many substitutions
			NRSUBS = NRSUBS+1
C
C					Translate the symbol
C					- expand symbol name if needed
C
			IS = DWC_SYM_EXPAND (STROUT(START+1:PTR-1),STREAM,
	1							WORK,LW)
			IF (IAND(IS,1).NE.0) IS = DWC_SYM_TRANSL (WORK(:LW),VALUE,LVAL,
	1							CHKSW,SWSYM)
			IF (IAND(IS,1).EQ.0) GOTO 995
C
C					Replace the symbol name by the value
C
			LW = 0
			IS = STR_COPY (STROUT(PTR+1:LOUT),WORK,LW) ! save rest
			LOUT = START-1			! keep first part
			IS = STR_COPY (VALUE(:LVAL)//	! append value
	1			WORK(:LW),STROUT,LOUT)	! append rest
			IF (IS.LT.0) GOTO 996		! STROUT too short
			PTR = START
		ENDIF
C
C					Look for the next special substring
C					starting after the literal or at the
C					start of the substituted substring
C
		IS = STR_SKIP_U (QUOTE//APOSTR,STROUT(:LOUT),PTR)
	ENDDO
C
C
	LOUT = STR_SIGLEN (STROUT(:LOUT))
	DWC_STR_SUBST = DWC_SUCCESS
	RETURN
C
 991	DWC_STR_SUBST =  MSG_SET (DWC_STRTOOSHO,1)
	CALL WNCTXT(DWLOG,DWMSG,LMAX)
	RETURN
C
 992	DWC_STR_SUBST = MSG_SET (DWC_NOENDQUO,0)
	ERRPTR = START
	RETURN
C
 993	DWC_STR_SUBST = MSG_SET (DWC_NOENDAPOS,0)
	ERRPTR = START
	RETURN
C
 994	DWC_STR_SUBST = MSG_SET (DWC_MUTUALSUB,0)
	ERRPTR = START+1
	RETURN
C
 995	DWC_STR_SUBST = IS
	ERRPTR = START+1
	RETURN
C
 996	DWC_STR_SUBST = MSG_SET (DWC_STRTOOSHO,1)
	CALL WNCTXT(DWLOG,DWMSG,LMAX)
	ERRPTR = START+1
	RETURN
	END
