C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	PPD_PROMPT
C.Keywords:	PPD File, Prompt Info
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	900415 FMO - recreation
C.Version:	920224 GvD - no optional arguments in MSG anymore
C.Version:	930513 HjV - Change size WORK to 2500 i.s.o. 1600
C.Version:	930613 HjV - Change size WORK to 5000 i.s.o. 2500
C.Version:	940829 HjV - Change size WORK from 5000 to 10000
C		941019 JPH - Change order of prompt components; '|' line control
C			      Remove useless code
C		950124 JPH - Correct keyword/prompt/options formatting
C
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_PROMPT (PARMSPEC,LEVEL,SWPNAM,PROMPT,LP)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	PARMSPEC	! (i) parameter specification
	INTEGER*4	LEVEL		! (i) level of prompt info (0, 1 or 2)
	LOGICAL*4	SWPNAM		! (i) program's parameter name ?
	CHARACTER*(*)	PROMPT		! (o) prompt string
	INTEGER*4	LP		! (o) its significant length
C
C.Purpose:	Compose the prompt string for a program parameter
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_NOCURENTR	no current parameter selected
C	error	PPD_STRTOOSML	output string too short
C	false status returned by referenced routines
C.Notes:
C	The parameter spec has the format: [program[$stream]_][keyword].
C	If the keyword is given, the prompt string for the that parameter
C	will be composed. Otherwise, the current parameter will be used.
C-------------------------------------------------------------------------
C
C
	INTEGER*4	PPD_STAT_INQ	,PPD_READ_P	,PPD_READ_U
	INTEGER*4	PPD_UNAM_GET	,PPD_USTR_GET	,PPD_OPSTR_GET
	INTEGER*4	PPD_PRSTR_GET
	INTEGER*4	STR_COPY	,STR_SIGLEN	,MSG_SET  
C
	CHARACTER	WORK*256, PROGSTRM*80, KEYWORD*16
	INTEGER*4	LW, LMIN, LSPEC, LPS, LK
	INTEGER*4	IS, MAPB, ADDR, HLPB
	LOGICAL*4	PROTOTYPE, LF
C
C
C					Set up
C					- split the parameter spec into
C					  program$stream and keyword
C
	PROMPT = ' '
	LP = 0
	LSPEC = STR_SIGLEN (PARMSPEC)
	LPS = INDEX (PARMSPEC(:LSPEC),'_')
	IF (LPS.GT.0) PROGSTRM = PARMSPEC(:LPS)
	LK = LSPEC-LPS
	IF (LK.GT.0) KEYWORD = PARMSPEC(LPS+1:LSPEC)
C
C					Get the wanted parameter description
C
	IF (LK.GT.0) THEN
		IF (SWPNAM) THEN
			IS = PPD_READ_P (KEYWORD)
		ELSE
			IS = PPD_READ_U (KEYWORD)
		ENDIF
	ELSE
		IS = PPD_STAT_INQ (MAPB,ADDR,HLPB)
		IF (ADDR.EQ.0) IS = MSG_SET (PPD_NOCURENTR,0)
	ENDIF
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Get the prompt string
C					- level 0: user's parameter name
C					- level 1: add units and options
C					- level 2: add prompt string
C
	IS = PPD_UNAM_GET (WORK,LW,LMIN,PROTOTYPE)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (LPS.GT.0) IS = STR_COPY (PROGSTRM(:LPS),PROMPT,LP)
	IF (LW.GT.0)  IS = STR_COPY (WORK(:LW),PROMPT,LP)
	IF (IS.LT.0) GOTO 998
C
!	IF (LEVEL.GT.0) THEN
!		IS = PPD_USTR_GET (WORK,LW)
!		IF (IAND(IS,1).EQ.0) GOTO 999
!		IF (LW.GT.0) IS = STR_COPY (' ('//WORK(:LW)// ')',PROMPT,LP)
!		IF (IS.LT.0) GOTO 998
C
!		IF (LEVEL.GT.1) THEN
			IS = PPD_PRSTR_GET (WORK,LW)
			IF (IAND(IS,1).EQ.0) GOTO 999
			LF = (WORK(LW:LW) .EQ.'|')
			IF (.NOT.LF) THEN
				LW=LW+1
				WORK(LW:LW)=' '
			ENDIF
			IF (LW.GT.0) IS = STR_COPY 
	1			(' '//WORK(:LW),PROMPT,LP)
			IF (IS.LT.0) GOTO 998
!		ENDIF
		IS = PPD_OPSTR_GET (WORK,LW)
		LF = (WORK(LW:LW) .EQ.'|')
		IF (LF) LW= LW-1
		IF (IAND(IS,1).EQ.0) GOTO 999
		IF (LW.GT.0) IS = STR_COPY ('('//WORK(:LW)//')',PROMPT,LP)
		IF (LF) THEN
			LP = LP +1
			PROMPT(LP:LP) = '|'
		ENDIF
		IF (IS.LT.0) GOTO 998
!	ENDIF
C
C
 900	PPD_PROMPT = PPD_SUCCESS
	RETURN
C
 998	PPD_PROMPT = MSG_SET (PPD_STRTOOSML,0)
	RETURN
C
 999	PPD_PROMPT = IS
	RETURN
	END
