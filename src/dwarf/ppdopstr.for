C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	PPD_OPSTR
C.Keywords:	PPD File, Parameter Options
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		Common variables used:
C	CHARACTER*(*)	PPDPD$DESCR	! (m) description in string format
C	INTEGER*4	PPDPD$LENG	! (m) current sign length of descr
C	INTEGER*4	PPDPD$OPOFF	! (m) offset of options string in descr
C	INTEGER*4	PPDPD$OPLEN	! (m) length of options string
C
C.Version:	900415 FMO - recreation
C.Version:	920224 GvD - no optional arguments in MSG anymore
C.Version:	930510 HjV - Change some INTEGER*2 into INTEGER*4
C.Version:	940120 CMV - Use indirect addressing (A_B)
C		940909 JPH - PPD_OPSTR_LSET: dynamic help texts
C		941221 JPH - formatting chatracters ';|[]/' in options string
C		941213 JPH - More formatting. COMMA --> DELIM
C		010709 AXC - Linux port - Parameter
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_OPSTR_PUT (STRING,DO_CHECK)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRING		! (i) proposed options string
	LOGICAL*4	DO_CHECK	! (i) check the string ?
C
C.Purpose:	Check and store the options string for a program parameter
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_OPTNOVAL	option check requested, but no options given
C	error	PPD_OPTNOCHK	options given, but no option check requested
C	error	PPD_OPTINVAL	invalid option syntax
C.Notes:
C	- The options string is stored in the variable-length part of the
C	  current parameter description. Its offset w.r.t. the start of the
C	  description and its significant length are stored in the fixed part
C	  (fields PPDPD$OPOFF and PPDPD$OPLEN).
C	- If no options are given, the offset is set to UNDEF_J.
C	- In case of errors, no messages are stored in the regular message
C	  buffer. The calling routine (BPD_BUILD) takes care of that.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDREC_4_DEF'
C
	INTEGER*4	LBUF
	CHARACTER*(*)	EMPTVAL
		PARAMETER (LBUF    = 512 )
		PARAMETER (EMPTVAL = '[]')
	INTEGER*4	PPD_CMAS_GET, PPD_DTYPE_GET
	INTEGER*4	CPL_VALLIST
	INTEGER*4	STR_SIGLEN
C
	CHARACTER*1	DTYPE
	BYTE		BUF(LBUF)
	INTEGER*4	IS, LSTR, PLEN, COUNT
C
C
	PPDPD$OPOFF = UNDEF_J
	PPDPD$OPLEN = 0
C
	LSTR = STR_SIGLEN (STRING)
	IF (LSTR.GT.0 .AND. STRING(:LSTR).NE.EMPTVAL) THEN
		PPDPD$OPOFF = PPDPD$LENG
		PPDPD$OPLEN = LSTR
		PPDPD$LENG = PPDPD$LENG+LSTR
		IF (PPDPD$LENG.GT.PPDPD__LENGTH*4) GOTO 9999
		PPDPD$DESCR(PPDPD$OPOFF+1:PPDPD$LENG) = STRING(:LSTR)
C
C					Is options string allowed and valid ?
C
		IF (IAND(PPD_CMAS_GET('OPTIONS'),1).EQ.0 .AND.
	1	    IAND(PPD_CMAS_GET('ABBREV_OPTIONS'),1).EQ.0) THEN
			IS = PPD_OPTNOCHK
			GOTO 999
		ENDIF
		IS = PPD_DTYPE_GET (DTYPE,PLEN)
		IS = CPL_VALLIST (STRING(:LSTR),DTYPE,PLEN,BUF,LBUF,COUNT)
		IF (IAND(IS,1).EQ.0) THEN
			IS = PPD_OPTINVAL
			GOTO 999
		ENDIF
C
C					Is options string required ?
C
	ELSE
		IF (IAND(PPD_CMAS_GET('OPTIONS'),1).NE.0 .OR.
	1	    IAND(PPD_CMAS_GET('ABBREV_OPTIONS'),1).NE.0) THEN
			IS = PPD_OPTNOVAL
			GOTO 999
		ENDIF
	ENDIF
C
	PPD_OPSTR_PUT = PPD_SUCCESS
	RETURN
C
 999	PPD_OPSTR_PUT = IS
	RETURN
C
 9999	PPD_OPSTR_PUT = 4
	CALL WNCTXT(DWLOG,'PPDPD_ overflow: tell DWARF manager')
	CALL WNGEX
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_OPSTR_GET (STRING,LS)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRING		! (o) options string
	INTEGER*4	LS		! (o) its significant length
C
C.Purpose:	Get the options string from the current parameter description
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_STRTOOSML	output string too short
C.Notes:
C	- If a dynamic options string has been set, it is used
C	- If no options are given, a blank string will be returned.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDREC_4_DEF'
C
	INTEGER*4	STR_COPY, MSG_SET  
C
	INTEGER*4	IS

	CHARACTER*128	LOPT
	INTEGER		LOPT_L
	COMMON /LOPT/	LOPT_L,LOPT

	INTEGER		PPD_OPSTR_LSET, STR_SIGLEN
C
C
C					Get the string
C
	STRING = ' '
	LS = 0
	IF (LOPT_L.NE.0) THEN
		LS=MIN(LOPT_L,LEN(STRING))
		STRING=LOPT
	ELSEIF (PPDPD$OPOFF.NE.UNDEF_J) THEN
	    IS = STR_COPY
	1	(PPDPD$DESCR(PPDPD$OPOFF+1:PPDPD$OPOFF+PPDPD$OPLEN),
	2							STRING,LS)
	    IF (IS.LT.0) GOTO 999
	ENDIF
C
	PPD_OPSTR_GET = PPD_SUCCESS
	RETURN
C
 999	PPD_OPSTR_GET = MSG_SET (PPD_STRTOOSML,0)
	RETURN
C
C
C This entry point is used to set dynamic prompt text that will be shown in 
C  subsequent prompts in lieu of the .PPD prompt text. The text is stored in the
C  local buffer LOPT
C
	ENTRY PPD_OPSTR_LSET (STRING)
C
	IF (STRING.GT.' ') THEN
		LOPT=STRING
		LOPT_L=STR_SIGLEN(STRING)
	ELSE
		LOPT_L=0
	ENDIF
C		
	RETURN
C
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_OPSTR_XGET (STRING,LS)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRING		! (o) options string
	INTEGER*4	LS		! (o) its significant length
C
C.Purpose:	Get the options string for the current parameter
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
C	- If no options are given, a blank string will be returned.
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
	IF (PPDPD$OPOFF.NE.UNDEF_J) THEN
		LS = PPDPD$OPLEN
		ADDR = ADDR+PPDPD$OPOFF+1
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
	PPD_OPSTR_XGET = PPD_SUCCESS
	RETURN
C
 999	PPD_OPSTR_XGET = MSG_SET (IS,0)
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_OPSTR_MATCH (BSTR,LENG,SHORT)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	BYTE		BSTR(*)		! (m) byte string with option
	INTEGER*4	LENG		! (i) its length
	LOGICAL*4	SHORT		! (i) unique abbreviation allowed

	CHARACTER*512	LOPT
	INTEGER		LOPT_L
	COMMON /LOPT/	LOPT_L,LOPT
C
C.Purpose:	Check whether the option is a valid one
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_OPTINVAL	invalid option
C	error	PPD_OPTNOTUNI	ambiguous abbreviation
C.Notes:
C	- If a dynamic options string has been set, it is used
C	- The complete option will be returned in BSTR.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDREC_4_DEF'
C
	CHARACTER*(*)	BLANK, COMMA, DELIM
	CHARACTER	TAB*1, LF*1, CR*1, WHITE*4
		PARAMETER (BLANK = ' ')
		PARAMETER (COMMA = ',')
		PARAMETER (DELIM = ',;|[]:' )
C
	INTEGER*4	STR_COPY, STR_MATCH_L
	INTEGER*4	STR_SKIP_U, STR_SKIP_W, STR_COPY_U
	INTEGER*4	MOVE_BLB, MSG_SET  
C
	CHARACTER	LIST*256, OPTION*80
	INTEGER*4	IS, LL, LO, MATCHNR, PTR
C
	TAB   = CHAR(9)
	LF    = CHAR(10)
	CR    = CHAR(13)
	WHITE = BLANK//TAB//LF//CR
C
C					Get the options list and
C					the option in string format
C
	LL = 0
	IF (LOPT_L.NE.0) THEN
		LL=LOPT_L
		LIST=LOPT
	ELSEIF (PPDPD$OPOFF.NE.UNDEF_J) THEN
		IS = STR_COPY
	1	(PPDPD$DESCR(PPDPD$OPOFF+1:PPDPD$OPOFF+PPDPD$OPLEN),LIST,LL)
	ENDIF
	IF (LL.EQ.0) GOTO 999
	IS = MOVE_BLB (BSTR,%REF(OPTION),LENG)
C
C					Match option with list
C
	IS = STR_MATCH_L (OPTION(:LENG),LIST(:LL),MATCHNR)
	IF (MATCHNR.EQ.0) GOTO 999
C
C					Abbreviated match
C					- only allowed for SHORT=.TRUE.
C					- must be unique
C					- return the complete option
C
	IF (IS.NE.1) THEN
		IF (.NOT.SHORT) GOTO 999
		IF (IS.EQ.0) GOTO 998
		OPTION(:LENG) = BLANK
		LO = 0
		PTR = 1
		DO I = 1,MATCHNR-1
			IS = STR_SKIP_U (DELIM,LIST(:LL),PTR)
!!			IS = STR_SKIP_U (COMMA//';|/[]',LIST(:LL),PTR)
			PTR = PTR+1
		ENDDO
		IS = STR_SKIP_W (WHITE,LIST(:LL),PTR)
		IS = STR_COPY_U (DELIM//WHITE,LIST(:LL),PTR,OPTION,LO)
!!		IS = STR_COPY_U (COMMA//WHITE//';|/[]',LIST(:LL),PTR,OPTION,LO)
		IS = MOVE_BLB (%REF(OPTION),BSTR,LENG)
	ENDIF
C
C
	PPD_OPSTR_MATCH = PPD_SUCCESS
	RETURN
C
 998	PPD_OPSTR_MATCH = MSG_SET (PPD_OPTNOTUNI,0)
	RETURN
C
 999	PPD_OPSTR_MATCH = MSG_SET (PPD_OPTINVAL,0)
	RETURN
C
	END
