C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	PPD_DVSTR
C.Keywords:	PPD File, Parameter Default Values
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		Common variables used:
C	CHARACTER*(*)	PPDPD$DESCR	! (m) description in string format
C	INTEGER*4	PPDPD$LENG	! (m) current sign length of descr
C	INTEGER*4	PPDPD$DVOFF	! (m) offset of default vals in descr
C	INTEGER*4	PPDPD$DVLEN	! (m) length of default vals
C
C.Version:	900415 FMO - recreation
C.Version:	920224 GvD - no optional arguments in MSG anymore
C.Version:	930510 HjV - Change some INTEGER*2 into INTEGER*4
C.Version:	940120 CMV - Use indirect addressing (A_B)
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_DVSTR_PUT (STRING,DO_CHECK)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRING		! (i) proposed default values
	LOGICAL*4	DO_CHECK	! (i) check the string ?
C
C.Purpose:	Check and store the default values for a program parameter
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_PSEARCH	no PPD default allowed
C.Notes:
C	- The default values are stored in the variable-length part of the
C	  current parameter description. Its offset w.r.t. the start of the
C	  description and its significant length are stored in the fixed part
C	  (fields PPDPD$DVOFF and PPDPD$DVLEN).
C	- If no default values are given, the offset is set to UNDEF_J.
C	- In case of errors, no messages are stored in the regular message
C	  buffer. The calling routine (BPD_BUILD) takes care of that.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDREC_4_DEF'
C
	INTEGER*4	PPD_SSTR_GET, STR_SIGLEN
C
	CHARACTER	SSTR*5, GROUP*32
	INTEGER*4	IS, LSTR, LS, LG
C
C
	PPDPD$DVOFF = UNDEF_J
	PPDPD$DVLEN = 0
C
	LSTR = STR_SIGLEN (STRING)
	IF (LSTR.GT.0) THEN
		PPDPD$DVOFF = PPDPD$LENG
		PPDPD$DVLEN = LSTR
		PPDPD$LENG = PPDPD$LENG+LSTR
		IF (PPDPD$LENG.GT.PPDPD__LENGTH*4) GOTO 9999
		PPDPD$DESCR(PPDPD$DVOFF+1:PPDPD$LENG) = STRING(:LSTR)
C
C					Is a PPD default allowed ?
C
		IS = PPD_SSTR_GET (SSTR,LS,GROUP,LG)
		IF (INDEX(SSTR(:LS),'P').EQ.0) THEN
			IS = PPD_PSEARCH
			GOTO 999
		ENDIF
	ENDIF
C
	PPD_DVSTR_PUT = PPD_SUCCESS
	RETURN
C
 999	PPD_DVSTR_PUT = IS
	RETURN
C
 9999	PPD_DVSTR_PUT = 4
	CALL WNCTXT(DWLOG,'PPDPD_ overflow: tell DWARF manager')
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_DVSTR_GET (STRING,LS)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRING		! (o) default values
	INTEGER*4	LS		! (o) its significant length
C
C.Purpose:	Get the default values from the current parameter description
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_STRTOOSML	output string too short
C.Notes:
C	- If no default values are given, a blank string will be returned.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDREC_4_DEF'
C
	INTEGER*4	STR_COPY, MSG_SET  
C
	INTEGER*4	IS
C
C
C					Get the string
C
	STRING = ' '
	LS = 0
	IF (PPDPD$DVOFF.NE.UNDEF_J) THEN
	    IS = STR_COPY
	1		(PPDPD$DESCR(PPDPD$DVOFF+1:PPDPD$DVOFF+PPDPD$DVLEN),
	2							STRING,LS)
	    IF (IS.LT.0) GOTO 999
	ENDIF
C
	PPD_DVSTR_GET = PPD_SUCCESS
	RETURN
C
 999	PPD_DVSTR_GET = MSG_SET (PPD_STRTOOSML,0)
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_DVSTR_XGET (STRING,LS)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRING		! (o) default values
	INTEGER*4	LS		! (o) its significant length
C
C.Purpose:	Get the default values for the current parameter
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
C	- If no default values are given, a blank string will be returned.
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
	IF (PPDPD$DVOFF.NE.UNDEF_J) THEN
		LS = PPDPD$DVLEN
		ADDR = ADDR+PPDPD$DVOFF+1
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
	PPD_DVSTR_XGET = PPD_SUCCESS
	RETURN
C
 999	PPD_DVSTR_XGET = MSG_SET (IS,0)
	RETURN
	END
