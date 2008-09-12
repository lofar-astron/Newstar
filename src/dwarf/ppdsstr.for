C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	PPD_SSTR
C.Keywords:	PPD File, Parameter Search Strategy
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		Common variables used:
C	CHARACTER*(*)	PPDPD$DESCR	! (m) description in string format
C	INTEGER*4	PPDPD$LENG	! (m) current sign length of descr
C	INTEGER*4	PPDPD$SOFF	! (m) offset of search-strategy string
C	INTEGER*4	PPDPD$SLEN	! (m) its signif length
C	INTEGER*4	PPDPD$GOFF	! (m) offset of group name
C	INTEGER*4	PPDPD$GLEN	! (m) its signif length
C
C.Version:	900415 FMO - recreation
C.Version:	920224 GvD - no optional arguments in MSG anymore
C.Version:	930510 HjV - Change some INTEGER*2 into INTEGER*4
C.Version:	940120 CMV - Use indirect addressing (A_B)
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_SSTR_PUT (STRING,GROUP,DO_CHECK)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRING		! (i) proposed search strategy
	CHARACTER*(*)	GROUP		! (i) group name
	LOGICAL*4	DO_CHECK	! (i) check the string ?
C
C.Purpose:	Check and store the search strategy for a program parameter
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_SEARCHINV	invalid search list
C	error	PPD_GLOFILNF	group PPD file not found
C.Notes:
C	- The search strategy and group name are stored in the variable-length
C	  part of the current parameter description. The offsets of the strings
C	  w.r.t. the start of the description and their significant lengths are
C	  stored in the fixed part (fields PPDPD$UOFF, PPDPD$ULEN, PPDPD$GOFF
C	  and PPDPD$GLEN).
C	- If a string is not given, the offset is set to UNDEF_J.
C	- In case of errors, no messages are stored in the regular message
C	  buffer. The calling routine (BPD_BUILD) takes care of that.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDREC_4_DEF'
C
	CHARACTER*(*)	EMPTVAL, DEFSSTR, DEFGSTR
		PARAMETER (EMPTVAL  = '[]'       )
		PARAMETER (DEFSSTR  = 'L,P'      )
		PARAMETER (DEFGSTR  = ' '        )
C
	INTEGER*4	PPD_SSTR_SPLIT, PPD_FILE_FIND
	INTEGER*4	STR_SIGLEN
C
	CHARACTER	SSTR*5, GSTR*32, FULLSPEC*64
	INTEGER*4	IS, LS, LG, LF
C
C
	PPDPD$SOFF = UNDEF_J
	PPDPD$SLEN = 0
	PPDPD$GOFF = UNDEF_J
	PPDPD$GLEN = 0
C
C					Split and check input string
C
	IF (DO_CHECK) THEN
		LS = STR_SIGLEN (STRING)
		IF (LS.EQ.0 .OR. STRING(:LS).EQ.EMPTVAL) THEN
			SSTR = DEFSSTR
			GSTR = DEFGSTR
			LS = STR_SIGLEN (SSTR)
			LG = STR_SIGLEN (GSTR)
		ELSE
			IS = PPD_SSTR_SPLIT (STRING(:LS),SSTR,LS,GSTR,LG)
			IF (IAND(IS,1).EQ.0) GOTO 999
		ENDIF
	ELSE
		SSTR = STRING
		GSTR = GROUP
		LS = STR_SIGLEN (SSTR)
		LG = STR_SIGLEN (GSTR)
	ENDIF
C
C					Check and store group name
C					- file <group>.PPD must exist
C
	IF (LG.GT.0) THEN
		IS = PPD_FILE_FIND (GSTR(:LG),FULLSPEC,LF)
		IF (IAND(IS,1).EQ.0) THEN
			IS = PPD_GLOFILNF
			GOTO 999
		ENDIF
		PPDPD$GOFF = PPDPD$LENG
		PPDPD$GLEN = LG
		PPDPD$LENG = PPDPD$LENG+LG
		IF (PPDPD$LENG.GT.PPDPD__LENGTH*4) GOTO 9999
		PPDPD$DESCR(PPDPD$GOFF+1:PPDPD$LENG) = GSTR(:LG)
	ENDIF
C
C					Store the search strategy
C
	IF (LS.GT.0) THEN
		PPDPD$SOFF = PPDPD$LENG
		PPDPD$SLEN = LS
		PPDPD$LENG = PPDPD$LENG+LS
		IF (PPDPD$LENG.GT.PPDPD__LENGTH*4) GOTO 9999
		PPDPD$DESCR(PPDPD$SOFF+1:PPDPD$LENG) = SSTR(:LS)
	ENDIF
C
	PPD_SSTR_PUT = PPD_SUCCESS
	RETURN
C
 999	PPD_SSTR_PUT = IS
	RETURN
C
 9999	PPD_SSTR_PUT = 4
	CALL WNCTXT(DWLOG,'PPDPD_ overflow: tell DWARF manager')
	CALL WNGEX
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_SSTR_GET (SSTR,LS,GROUP,LG)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	SSTR		! (o) search strategy
	INTEGER*4	LS		! (o) its significant length
	CHARACTER*(*)	GROUP		! (o) group name for global search
	INTEGER*4	LG		! (o) its significant length
C
C.Purpose:	Get the search strategy from the current parameter description
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_STRTOOSML	output string too short
C.Notes:
C	- If no search strategy is given, a blank string will be returned.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDREC_4_DEF'
C
	INTEGER*4	STR_COPY, MSG_SET  
C
	INTEGER*4	IS
C
C
	SSTR = ' '
	LS = 0
	GROUP = ' '
	LG = 0
C
C					Get the search strategy
C
	IF (PPDPD$SOFF.NE.UNDEF_J) THEN
	    IS = STR_COPY
	1	(PPDPD$DESCR(PPDPD$SOFF+1:PPDPD$SOFF+PPDPD$SLEN),
	2							SSTR,LS)
	    IF (IS.LT.0) GOTO 999
	ENDIF
C
C					Get the group name
C
	IF (PPDPD$GOFF.NE.UNDEF_J) THEN
	    IS = STR_COPY
	1	(PPDPD$DESCR(PPDPD$GOFF+1:PPDPD$GOFF+PPDPD$GLEN),
	2							GROUP,LG)
	    IF (IS.LT.0) GOTO 999
	ENDIF
C
	PPD_SSTR_GET = PPD_SUCCESS
	RETURN
C
 999	PPD_SSTR_GET = MSG_SET (PPD_STRTOOSML,0)
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_SSTR_XGET (SSTR,LS,GROUP,LG)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	SSTR		! (o) search strategy
	INTEGER*4	LS		! (o) its significant length
	CHARACTER*(*)	GROUP		! (o) group name for global search
	INTEGER*4	LG		! (o) its significant length
C
C.Purpose:	Get the search strategy for the current parameter
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_SEQERROR	no PPD file mapped
C	error	PPD_NOCURENTR	no current parameter selected
C	error	PPD_STRTOOSML	output string too short
C.Notes:
C	- The strings are fetched directly from the mapped PPD file using
C	  the offsets and lengths given in the current parameter description.
C	- Use XGET i.s.o. GET when the variable-length part of the current
C	  description contains data for another parameter (e.g. the COPY
C	  parameter in BLDPPD).
C	- If no search strategy is given, a blank string will be returned.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDREC_4_DEF'
C
	INTEGER*4	PPD_STAT_INQ, MOVE_BLB, MSG_SET  
C
	INTEGER*4	IS, MAPB, ADDR, HLPB
C
C
	SSTR = ' '
	LS = 0
	GROUP = ' '
	LG = 0
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
C					Get the search strategy
C
	IF (PPDPD$SOFF.NE.UNDEF_J) THEN
		LS = PPDPD$SLEN
		IF (LS.LE.LEN(SSTR)) THEN
			IS = MOVE_BLB (A_B(ADDR+PPDPD$SOFF+1-A_OB),
	1			%REF(SSTR),LS)
		ELSE
			LS = LEN(SSTR)
			IS = MOVE_BLB (A_B(ADDR+PPDPD$SOFF+1-A_OB),
	1			%REF(SSTR),LS)
			IS = PPD_STRTOOSML
			GOTO 999
		ENDIF
	ENDIF
C
C					Get the group name
C
	IF (PPDPD$GOFF.NE.UNDEF_J) THEN
		LG = PPDPD$GLEN
		IF (LG.LE.LEN(GROUP)) THEN
			IS = MOVE_BLB (A_B(ADDR+PPDPD$GOFF+1-A_OB),
	1			%REF(GROUP),LG)
		ELSE
			LG = LEN(GROUP)
			IS = MOVE_BLB (A_B(ADDR+PPDPD$GOFF+1-A_OB),
	1			%REF(GROUP),LG)
			IS = PPD_STRTOOSML
			GOTO 999
		ENDIF
	ENDIF
C
	PPD_SSTR_XGET = PPD_SUCCESS
	RETURN
C
 999	PPD_SSTR_XGET = MSG_SET (IS,0)
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_SSTR_SPLIT (LIST,SSTR,LS,GSTR,LG)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	LIST		! (i) proposed search list
	CHARACTER*(*)	SSTR		! (o) search strategy
	INTEGER*4	LS		! (o) signif length of search strategy
	CHARACTER*(*)	GSTR		! (o) group name
	INTEGER*4	LG		! (o) signif length of group name
C
C.Purpose:	Split the search list
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_SEARCHINV	invalid search list
C.Notes:
C	- In case of errors, no messages are stored in the regular message
C	  buffer. The calling routine (PPD_SSTR_PUT) takes care of that.
C-------------------------------------------------------------------------
C
C
	CHARACTER*(*)	COLON, COMMA
	INTEGER*4	NCODE
		PARAMETER (COLON = ':')
		PARAMETER (COMMA = ',')
		PARAMETER (NCODE = 3  )
	CHARACTER*7	CODE(NCODE)
		DATA CODE /'LOCAL','GLOBAL','PROGRAM' /
C
	INTEGER*4	STR_SIGLEN, STR_MATCH_A, STR_COPY_U, STR_COPY
C
	CHARACTER*16	FIELD
	INTEGER*4	LL, LF
	INTEGER*4	IS, PTR, NR, LASTNR
	LOGICAL*4	GROUP_FLAG
C
C
	GSTR = ' '
	SSTR = ' '
	LG = 0
	LS = 0
	LL = STR_SIGLEN (LIST)
	PTR = 1
	LASTNR = 0
C
C					Extract the next search-list field
C
	DO WHILE (PTR.LE.LL)
		LF = 0
		IS = STR_COPY_U (COMMA//COLON,LIST(:LL),PTR,FIELD,LF)
		GROUP_FLAG = PTR.LE.LL .AND. LIST(PTR:PTR).EQ.COLON
C
C					Check and abbreviate
C
		IS = STR_MATCH_A (FIELD(:LF),NCODE,CODE,NR)
		IF (NR.LE.LASTNR) GOTO 999
		LASTNR = NR
		FIELD = CODE(NR)(1:1)
C
C					If GLOBAL search:
C					- extract the group name if given
C					  or use default name 'GLOBAL'
C
		IF (FIELD(1:1).EQ.'G') THEN
			IF (GROUP_FLAG) THEN
				LG = 0
				PTR = PTR+1
				IS = STR_COPY_U (COMMA,LIST(:LL),PTR,GSTR,LG)
			ELSE
				GSTR = 'GLOBAL'
				LG = 6
			ENDIF
C
C					Otherwise: no group name allowed
C
		ELSE
			IF (GROUP_FLAG) GOTO 999
		ENDIF
C
C					Add search code (first character)
C					to the search strategy
C
		IF (LS.GT.0) THEN
			IS = STR_COPY (COMMA//FIELD(1:1),SSTR,LS)
		ELSE
			IS = STR_COPY (FIELD(1:1),SSTR,LS)
		ENDIF
		PTR = PTR+1
	ENDDO
C
C
	PPD_SSTR_SPLIT = PPD_SUCCESS
	RETURN
C
 999	PPD_SSTR_SPLIT = PPD_SEARCHINV
	RETURN
	END
