C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	PPD_USTR
C.Keywords:	PPD File, Parameter Units
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		Common variables used:
C	CHARACTER*(*)	PPDPD$DESCR	! (m) description in string format
C	INTEGER*4	PPDPD$LENG	! (m) current sign length of descr
C	INTEGER*4	PPDPD$UOFF	! (m) offset of units string in descr
C	INTEGER*4	PPDPD$ULEN	! (m) length of units string
C
C.Version:	900415 FMO - recreation
C.Version:	920224 GvD - no optional arguments in MSG anymore
C.Version:	930510 HjV - Change some INTEGER*2 into INTEGER*4
C.Version:	940120 CMV - Use indirect addressing (A_B)
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_USTR_PUT (STRING,DO_CHECK)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRING		! (i) proposed units string
	LOGICAL*4	DO_CHECK	! (i) check the string ?
C
C.Purpose:	Check and store the units string for a program parameter
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_UNITINV	invalid unit
C.Notes:
C	- The units string is stored in the variable-length part of the current
C	  parameter description. Its offset w.r.t. the start of the description
C	  and its significant length are stored in the fixed part (fields
C	  PPDPD$UOFF and PPDPD$ULEN).
C	- If a unit list is given, only the first list element will be kept.
C	- If no unit is given, the offset is set to UNDEF_J
C	- In case of errors, no messages are stored in the regular message
C	  buffer. The calling routine (BPD_BUILD) takes care of that.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDREC_4_DEF'
C
	CHARACTER*(*)	COMMA
		PARAMETER (COMMA = ',')
C
	INTEGER*4	READ_UNIT, STR_SIGLEN
C
	CHARACTER*16	UGRP
	REAL*8		UFACT
	INTEGER*4	IS, LS
C
C
	PPDPD$UOFF = UNDEF_J
	PPDPD$ULEN = 0
C
	LS = STR_SIGLEN (STRING)
	IF (LS.GT.0) THEN
		I = INDEX (STRING(:LS),COMMA)
		IF (I.GT.0) LS = STR_SIGLEN (STRING(:I-1))
	ENDIF
	IF (LS.GT.0) THEN
		IF (DO_CHECK) THEN
			IS = READ_UNIT (STRING(:LS),UGRP,UFACT)
			IF (IAND(IS,1).EQ.0) GOTO 999
		ENDIF
		PPDPD$UOFF = PPDPD$LENG
		PPDPD$ULEN = LS
		PPDPD$LENG = PPDPD$LENG+LS
		IF (PPDPD$LENG.GT.PPDPD__LENGTH*4) GOTO 9999
		PPDPD$DESCR(PPDPD$UOFF+1:PPDPD$LENG) = STRING(:LS)
	ENDIF
C
	PPD_USTR_PUT = PPD_SUCCESS
	RETURN
C
 999	PPD_USTR_PUT = PPD_UNITINV
	RETURN
C
 9999	PPD_USTR_PUT = 4
	CALL WNCTXT(DWLOG,'PPDPD_ overflow: tell DWARF manager')
	CALL WNGEX
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_USTR_GET (STRING,LS)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRING		! (o) units string
	INTEGER*4	LS		! (o) its significant length
C
C.Purpose:	Get the units string from the current parameter description
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_STRTOOSML	output string too short
C.Notes:
C	- If no units string is given, a blank string will be returned.
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
	IF (PPDPD$UOFF.NE.UNDEF_J) THEN
	    IS = STR_COPY
	1	(PPDPD$DESCR(PPDPD$UOFF+1:PPDPD$UOFF+PPDPD$ULEN),
	2							STRING,LS)
	    IF (IS.LT.0) GOTO 999
	ENDIF
C
	PPD_USTR_GET = PPD_SUCCESS
	RETURN
C
 999	PPD_USTR_GET = MSG_SET (PPD_STRTOOSML,0)
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_USTR_XGET (STRING,LS)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRING		! (o) units string
	INTEGER*4	LS		! (o) its significant length
C
C.Purpose:	Get the units string for the current parameter
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
C	- If no units string is given, a blank string will be returned.
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
	IF (PPDPD$UOFF.NE.UNDEF_J) THEN
		LS = PPDPD$ULEN
		ADDR = ADDR+PPDPD$UOFF+1
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
	PPD_USTR_XGET = PPD_SUCCESS
	RETURN
C
 999	PPD_USTR_XGET = MSG_SET (IS,0)
	RETURN
	END
