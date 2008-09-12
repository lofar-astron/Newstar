C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	PPD_PRSTR
C.Keywords:	PPD File, Parameter Prompt String
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		Common variables used:
C	CHARACTER*(*)	PPDPD$DESCR	! (m) description in string format
C	INTEGER*4	PPDPD$LENG	! (m) current sign length of descr
C	INTEGER*4	PPDPD$PROFF	! (m) offset of default vals in descr
C	INTEGER*4	PPDPD$PRLEN	! (m) length of default vals
C
C.Version:	900415 FMO - recreation
C.Version:	920224 GvD - no optional arguments in MSG anymore
C.Version:	930510 HjV - Change some INTEGER*2 into INTEGER*4
C.Version:	940120 CMV - Use indirect addressing (A_B)
C		940909 JPH - PPD_PRSTR_LSET: dynamic help texts
C			     Insert hyphen in front of prompt text
C		940923 JPH - Correct prompt length\
C		940930 JPH - Correct test for clear (.GT.' ' --> .NE.)
C		941020 JPH - Suppress '- ' if no prompt string 
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_PRSTR_PUT (STRING,DO_CHECK)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRING		! (i) proposed prompt string
	LOGICAL*4	DO_CHECK	! (i) check the string ?
C
C.Purpose:	Check and store the prompt string for a program parameter
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_PSEARCH	no PPD default allowed
C.Notes:
C	- The prompt string is stored in the variable-length part of the
C	  current parameter description. Its offset w.r.t. the start of the
C	  description and its significant length are stored in the fixed part
C	  (fields PPDPD$PROFF and PPDPD$PRLEN).
C	- If no prompt string is given, the offset is set to UNDEF_J.
C	- In case of errors, no messages are stored in the regular message
C	  buffer. The calling routine (BPD_BUILD) takes care of that.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDREC_4_DEF'
C
	INTEGER*4	STR_SIGLEN
C
	INTEGER*4	LSTR
C
C
	PPDPD$PROFF = UNDEF_J
	PPDPD$PRLEN = 0
C
	LSTR = STR_SIGLEN (STRING)
	IF (LSTR.GT.0) THEN
		PPDPD$PROFF = PPDPD$LENG
		PPDPD$PRLEN = LSTR
		PPDPD$LENG = PPDPD$LENG+LSTR
		IF (PPDPD$LENG.GT.PPDPD__LENGTH*4) GOTO 9999
		PPDPD$DESCR(PPDPD$PROFF+1:PPDPD$LENG) = STRING(:LSTR)
	ENDIF
C
	PPD_PRSTR_PUT = PPD_SUCCESS
	RETURN
C
 9999	PPD_PRSTR_PUT = 4
	CALL WNCTXT(DWLOG,'PPDPD_ overflow: tell DWARF manager')
	CALL WNGEX
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_PRSTR_GET (STRING,LS)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRING		! (o) prompt string
	INTEGER*4	LS		! (o) its significant length
C
C.Purpose:	Get the prompt string from the current parameter description
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_STRTOOSML	output string too short
C.Notes:
C	- If a dynamic prompt string has been set, it is used
CC	- If no prompt string is given, a blank string will be returned.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDREC_4_DEF'
C
	INTEGER*4	STR_COPY, MSG_SET  
C
	INTEGER*4	IS
	CHARACTER*128	LPR
	INTEGER		LPR_L
	INTEGER		PPD_PRSTR_LSET, STR_SIGLEN
	DATA		LPR_L/0/
C
C
C					Get the string
C
	LS = 2
	STRING(1:2) = '- '
	IF (LPR_L.NE.0) THEN
	    LS=MIN(LPR_L+2,LEN(STRING)-2)
	    STRING(3:)=LPR
	ELSEIF (PPDPD$PROFF.NE.UNDEF_J) THEN
	    IS = STR_COPY
	1	(PPDPD$DESCR(PPDPD$PROFF+1:PPDPD$PROFF+PPDPD$PRLEN),
	2						STRING(1:),LS)
	    IF (IS.LT.0) GOTO 999
	ENDIF
	IF (LS .EQ.2) LS= 0
C
	PPD_PRSTR_GET = PPD_SUCCESS
	RETURN
C
 999	PPD_PRSTR_GET = MSG_SET (PPD_STRTOOSML,0)
	RETURN
C
C
C This entry point is used to set dynamic prompt text that will be shown in 
C  subsequent prompts in lieu of the .PPD prompt text. The text is stored in the
C  local buffer LPR
C
	ENTRY PPD_PRSTR_LSET (STRING)
C
	IF (STRING.NE.' ') THEN
		LPR=STRING
		LPR_L=STR_SIGLEN(STRING)
	ELSE
		LPR_L=0
	ENDIF	
C
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_PRSTR_XGET (STRING,LS)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRING		! (o) prompt string
	INTEGER*4	LS		! (o) its significant length
C
C.Purpose:	Get the prompt string for the current parameter
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_SEQERROR	no PPD file mapped
C	error	PPD_NOCURENTR	no current parameter selected
C	error	PPD_STRTOOSML	output string too short
C.Notes:
C	- The string is fetched directly from the mapped PPD file using
C	  the offset and length given in the current parameter description.
C	- Use XGET i.s.o. GET when the variable-length part of the current
C	  description contains data for another parameter (e.g. the COPY
C	  parameter in BLDPPD).
C	- If no prompt string is given, a blank string will be returned.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDREC_4_DEF'
C
	INTEGER*4	PPD_STAT_INQ, MOVE_BLB, MSG_SET  
C
	INTEGER*4	IS, MAPB, ADDR, HLPB
C
C
	STRING = ' '
	LS = 0
C
C					Make sure that the PPD file is mapped
C					and that a parameter has been selected
C
	IS = PPD_STAT_INQ (MAPB,ADDR,HLPB)
	IF (MAPB.EQ.0) THEN
		IS = PPD_SEQERROR
		GOTO 999
	ELSE IF (ADDR.EQ.0) THEN
		IS = PPD_NOCURENTR
		GOTO 999
	ENDIF
C
C					Get the string
C
	IF (PPDPD$PROFF.NE.UNDEF_J) THEN
		LS = PPDPD$PRLEN
		ADDR = ADDR+PPDPD$PROFF+1
		IF (LS.LE.LEN(STRING)) THEN
			IS = MOVE_BLB (A_B(ADDR-A_OB),%REF(STRING),LS)
		ELSE
			LS = LEN(STRING)
			IS = MOVE_BLB (A_B(ADDR-A_OB),%REF(STRING),LS)
			IS = PPD_STRTOOSML
			GOTO 999
		ENDIF
	ENDIF
C
	PPD_PRSTR_XGET = PPD_SUCCESS
	RETURN
C
 999	PPD_PRSTR_XGET = MSG_SET (IS,0)
	RETURN
C
	END
