C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	PPD_HSTR
C.Keywords:	PPD File, Parameter Help String
C.Author:	Kasper Kombrink (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		Common variables used:
C	INTEGER*4	PPDPD$HOFF	! (m) offset of help string in buffer
C	INTEGER*4	PPDPD$HLEN	! (m) length of help string
C
C.Version:	900415 FMO - recreation
C.Version:	920224 GvD - no optional arguments in MSG anymore
C.Version:	930510 HjV - Change some INTEGER*2 into INTEGER*4
C.Version:	940120 CMV - use indirect addressing (A_B)
C		940909 JPH - PPD_HSTR_LSET: dynamic help texts
C.              941031 HjV - Typo
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_HSTR_PUT (STRING,DO_CHECK)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRING		! (i) proposed help string
	LOGICAL*4	DO_CHECK	! (i) check the string ?
C
C.Purpose:	Check and store the help string for a program parameter
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	false status codes returned by referenced routines
C.Notes:
C	- The help string is stored in the dynamic help buffer described by the
C	  status array BPD$HELP. Its offset w.r.t. the start of the buffer and
C	  its significant length are stored in the fixed part of the current
C	  parameter description (fields PPDPD$HOFF and PPDPD$HLEN).
C	- If no help string is given, the offset is set to UNDEF_J.
C	- In case of errors, no messages are stored in the regular message
C	  buffer. The calling routine (BPD_BUILD) takes care of that.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDREC_4_DEF'
C
	INTEGER*4	BPD_HELP_PUT, STR_SIGLEN
C
	INTEGER*4	IS, LSTR, HOFF
C
C
	PPDPD$HOFF = UNDEF_J
	PPDPD$HLEN = 0
C
	LSTR = STR_SIGLEN (STRING)
	IF (LSTR.GT.0) THEN
		IS = BPD_HELP_PUT (STRING(:LSTR),HOFF)
		IF (IAND(IS,1).EQ.0) GOTO 999
		PPDPD$HOFF = HOFF
		PPDPD$HLEN = LSTR
	ENDIF
C
	PPD_HSTR_PUT = PPD_SUCCESS
	RETURN
C
 999	PPD_HSTR_PUT = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_HSTR_XGET (STRING,LS)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRING		! (o) help string
	INTEGER*4	LS		! (o) its significant length
C
C.Purpose:	Get the help string for the current parameter
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_SEQERROR	no PPD file mapped
C	error	PPD_NOCURENTR	no current parameter selected
C	error	PPD_STRTOOSML	output string too short
C.Notes:
C	- If a dynamic help string has been set, it is copied first
C	- The string is fetched from the mapped PPD file (help area) using
C	  the offset and length given in the current parameter description,
C	  and appended.
C	- If no help string is given, a blank string will be returned.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDREC_4_DEF'
C
	INTEGER*4	PPD_STAT_INQ, MOVE_BLB, MSG_SET  
C
	INTEGER*4	IS, MAPB, ADDR, HLPB
	INTEGER		LHELP_MX
	PARAMETER	(LHELP_MX=512)
	CHARACTER*(LHELP_MX)	LHELP
	INTEGER		LHELP_L, LL
	LOGICAL		DYNONLY
	INTEGER		PPD_HSTR_LSET, STR_SIGLEN
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
C					Get the dynamic help text. If it ends in
C					a line '#-' this signals that the static
C					help text must be ignored.
C
	LL=MIN(LHELP_L,LEN(STRING))
	IF (LL.GT.0) STRING(1:LL)=LHELP
	IF (PPDPD$HOFF.NE.UNDEF_J) THEN
		LS = PPDPD$HLEN
		ADDR = HLPB+PPDPD$HOFF+1
		IF (LS+LL .LE.LEN(STRING)) THEN
			IS = MOVE_BLB (A_B(ADDR-A_OB),%REF(STRING(LL+1:)),LS)
			LS = LS +LL
		ELSE
			LS = LEN(STRING) -LL
			IS = MOVE_BLB (A_B(ADDR-A_OB),%REF(STRING(LL+1:)),LS)
			LS = LS +LL
			IS = PPD_STRTOOSML
			GOTO 999
		ENDIF
	ENDIF
C
	PPD_HSTR_XGET = PPD_SUCCESS
	RETURN
C
 999	PPD_HSTR_XGET = MSG_SET (IS,0)
	RETURN
C
C
C This entry point is used to set dynamic help text that will be shown in 
C  subsequent prompts BEFORE the .PPD help text. The text is stored in the local
C  buffer LHELP with a blank line appended to separate it from the .ppd text.
C
	ENTRY PPD_HSTR_LSET (STRING)
C
	IF (STRING.GT.' ') THEN
		LHELP_L=MIN(STR_SIGLEN(STRING),LHELP_MX)
		DYNONLY=LHELP(LHELP_L-1:LHELP_L).EQ.'#-'
		IF (.NOT.DYNONLY) LHELP_L=MIN(STR_SIGLEN(STRING),LHELP_MX-4)
							! reserve for CRLF*-
		LHELP(1:LHELP_L)=STRING
		DO I=1,LHELP_L-1
			IF (LHELP(I:I+1).EQ.'!/')	! replace !/ with CRLF
	1		  LHELP(I:I+1)=CHAR(13)//CHAR(10)
 		ENDDO
		IF (.NOT.DYNONLY) THEN
			LHELP(LHELP_L+1:LHELP_L+4)=CHAR(13)//CHAR(10)//'*-'
			LHELP_L=LHELP_L+4
		ENDIF
	ELSE
		LHELP_L=4
		LHELP(1:4)=CHAR(13)//CHAR(10)//'*-'
	ENDIF
C			
	RETURN

	END
