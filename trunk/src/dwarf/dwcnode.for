C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	DWC_NODE
C.Keywords:	DWARF, Node Name
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		Common variables used:
C	CHARACTER*80	DWARF$CURNODE_C	! (m) current node name
C	INTEGER*4	DWARF$LENNODE	! (m) significant length of node name
C
C.Version:	900219 FMO - creation
C.Version:	920214 GvD - no optional arguments in MSG anymore
C.Version:	010709 AXC - linux port - tmpchar in calls
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_NODE ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Make source-module name known
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C.Notes:	Dummy function
C-------------------------------------------------------------------------
C
C
	DWC_NODE = DWC_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_NODE_EXPAND_A (NRSET,NRVAL,LARR,ARRAY,NR)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	NRSET		! (i) nr of sets
	INTEGER*4	NRVAL		! (i) nr of reserved values per set
	INTEGER*4	LARR		! (i) length of single value
	BYTE		ARRAY(LARR,NRVAL,*) ! (m) node name array
	INTEGER*4	NR(*)		! (i) nr of used elements per set
C
C.Purpose:	Expand a block of node names
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	error	DWC_NODCOMERR	error in node name expansion
C.Notes:
C-------------------------------------------------------------------------
C
C
	CHARACTER*(*)	BLANK
		PARAMETER (BLANK  = ' ')
C
	INTEGER*4	DWC_NODE_GET, DWC_NODE_EXPAND
	INTEGER*4	MOVE_BLB, STR_SIGLEN, MSG_SET  
C
	CHARACTER	XNODE*80, NODE*80, CURNODE*80
	INTEGER*4	IS, LX, LN, LC
C
C
C					Start with DWARF's current node
C
	CURNODE = BLANK
	LC = 1
	IS = DWC_NODE_GET (CURNODE,LC)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Replace all relative node names in
C					the block with expanded names
C					- skip undefined block elements
C
	DO I = 1,NRSET
	 DO J = 1,NR(I)
	  IF (ARRAY(1,J,I).NE.UNDEF_B) THEN
		NODE = BLANK
		IS = MOVE_BLB (ARRAY(1,J,I),%REF(NODE),LARR)
		LN = STR_SIGLEN (NODE(:LARR))
		IS = DWC_NODE_EXPAND (NODE(:LN),CURNODE(:LC),XNODE(:LARR),LX)
		IF (IS.EQ.DWC_SETCURNOD) THEN		! set new current node
			CURNODE = XNODE
			LC = LX-1
		ELSE
			IF (IAND(IS,1).EQ.0) GOTO 999
		ENDIF
		IS = MOVE_BLB (%REF(XNODE),ARRAY(1,J,I),LARR)
	  ENDIF
	 ENDDO
	ENDDO
C
	DWC_NODE_EXPAND_A = DWC_SUCCESS
	RETURN
C
 999	DWC_NODE_EXPAND_A = MSG_SET (DWC_NODCOMERR,1)
	CALL WNCTXT(DWLOG,DWMSG,NODE(:LN),CURNODE(:LC))
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_NODE_EXPAND (NODE,CURNODE,XNODE,LX)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	NODE		! (i) node name
	CHARACTER*(*)	CURNODE		! (i) current node name
	CHARACTER*(*)	XNODE		! (o) expanded node name
	INTEGER*4	LX		! (o) significant length of XNODE
C
C.Purpose:	Expand a node name
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	.TRUE.
C	warning	DWC_SETCURNOD	set new current node is requested
C	error	DWC_APPMINUS	minus sign found in NODE, but no more fields
C				left in intermediate result
C	error	DWC_APPTOOLON	result is too long for XNODE
C.Notes:
C	- The node name can consist of one or more fields separated by dots
C	  (e.g. ABC.DEF.GHIJ)
C	- If the name starts with an empty field (empty name or name starting
C	  with dot or minus sign), the current node name will be prefixed.
C	  If the name ends with an empty field (trailing dot), that field will
C	  be ignored.
C	  Other empty fields (2 subsequent dots) are not allowed.
C	- If a field consists of one or more minus signs, each minus sign
C	  causes the deletion of the last field in the intermediate result.
C	- Examples:
C		CURNODE		NODE		XNODE
C		-------		-------		-------
C		A.B		C		C
C		A.B		.C		A.B.C
C		A.B		-		A
C		A.B.C.D		.--.E.F		A.B.E.F
C		A.B.C.D		--.E.-.F	A.B.F
C-------------------------------------------------------------------------
C
C
	CHARACTER*(*)	MINUS, DELIM, SETCUR
		PARAMETER (DELIM = '.')
		PARAMETER (MINUS = '-')
		PARAMETER (SETCUR = ':')
C
	INTEGER*4	DWC_NODE_CHECK
	INTEGER*4	STR_SIGLEN, STR_COPY_U, STR_COPY
	INTEGER*4	MSG_SET  
C
	INTEGER*4	IS, LN, PTR
	LOGICAL*4	DO_SETCUR
C
C
	XNODE = CURNODE
	LX = STR_SIGLEN (XNODE)
	LN = STR_SIGLEN (NODE)
	IF (LN.EQ.0) GOTO 900				! empty node name
C
C					If trailing colon:
C					- remove it
C					- set flag for restoration at exit
C					If trailing delimiter:
C					- remove it
C
	IF (NODE(LN:LN).EQ.SETCUR) THEN
		DO_SETCUR = .TRUE.
		LN = LN-1
		IF (LN.EQ.0) GOTO 900
	ENDIF
	IF (NODE(LN:LN).EQ.DELIM) THEN
		LN = LN-1
		IF (LN.EQ.0) GOTO 900
	ENDIF
C
C					Check the syntax of the node name
C
	IS = DWC_NODE_CHECK (NODE(:LN))
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Expand
C
	IF (NODE(1:1).NE.DELIM .AND. NODE(1:1).NE.MINUS) LX = 0
	PTR = 1
	DO WHILE (PTR.LE.LN)
		IF (NODE(PTR:PTR).EQ.DELIM) PTR = PTR+1	! skip delimiter
C
C					Remove last field of XNODE
C					- including its starting delimiter
C
		IF (NODE(PTR:PTR).EQ.MINUS) THEN
			IF (LX.EQ.0) GOTO 993		! not possible
			DO WHILE (LX.GT.1 .AND. XNODE(LX:LX).NE.DELIM)
				LX = LX-1
			ENDDO
			LX = LX-1			! new length of XNODE
			PTR = PTR+1			! next position in NODE
C
C					Append NODE field to XNODE
C					- if not first field: with delimiter
C
		ELSE
			IF (LX.GT.0) IS = STR_COPY (DELIM,XNODE,LX)
			IS = STR_COPY_U (DELIM,NODE(:LN),PTR,XNODE,LX)
			IF (IS.LT.0) GOTO 994		! not enough room
		ENDIF
	ENDDO
C
C					Restore "set current node" indicator
C
	IF (DO_SETCUR) THEN
		IS = STR_COPY (SETCUR,XNODE,LX)
		IF (IS.LT.0) GOTO 994			! not enough room
		DWC_NODE_EXPAND = DWC_SETCURNOD
		RETURN
	ENDIF
C
 900	DWC_NODE_EXPAND = DWC_SUCCESS
	RETURN
C
 993	DWC_NODE_EXPAND = MSG_SET (DWC_APPMINUS,0)
	RETURN
 994	DWC_NODE_EXPAND = MSG_SET (DWC_APPTOOLON,1)
	CALL WNCTXT(DWLOG,DWMSG,LEN(XNODE))
	RETURN
 999	DWC_NODE_EXPAND = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_NODE_PUT (NODE)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	NODE		! (i) node name
C
C.Purpose:	Set new current node
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	warning	DWC_STRTOOSHO	node name has been truncated
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'DWARF_4_DEF'
C
	INTEGER*4	STR_SIGLEN, MSG_SET  
C
	INTEGER*4	LN
C
C
	LN = STR_SIGLEN (NODE)
	IF (LN.GT.LEN(DWARF$CURNODE_C)) GOTO 999
	IF (LN.GT.0) THEN
		DWARF$CURNODE_C = NODE(:LN)
		DWARF$LENNODE = LN
	ENDIF
C
	DWC_NODE_PUT = DWC_SUCCESS
	RETURN
C
 999	DWC_NODE_PUT = MSG_SET (DWC_STRTOOSHO,1)
	CALL WNCTXT(DWLOG,DWMSG,LEN(DWARF$CURNODE_C))
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_NODE_GET (NODE,LN)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	NODE		! (o) node name
	INTEGER*4	LN		! (o) significant length of NODE
C
C.Purpose:	Get the current node
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	warning	DWC_STRTOOSHO	node name has been truncated
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'DWARF_4_DEF'
C
	CHARACTER*(*)	BLANK
		PARAMETER (BLANK  = ' ')
C
	INTEGER*4	STR_COPY, MSG_SET  
C
	INTEGER*4	IS
C
C
	LN = 0
	NODE = BLANK
	IS = STR_COPY (DWARF$CURNODE_C(:DWARF$LENNODE),NODE,LN)
	IF (IS.LT.0) GOTO 999
C
	DWC_NODE_GET = DWC_SUCCESS
	RETURN
C
 999	DWC_NODE_GET = MSG_SET (DWC_STRTOOSHO,1)
	CALL WNCTXT(DWLOG,DWMSG,LEN(NODE))
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_NODE_CHECK (NODE)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	NODE		! (i) node name
C
C.Purpose:	Check the syntax of the node name
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	.TRUE.
C	error	DWC_APPTWODOT	2 subsequent dots in NODE
C	error	GEN_ISNOTANM	subname is not alphanumeric or all minus signs
C.Notes:
C	- The node name can consist of one or more fields separated by dots
C	  (e.g. ABC.DEF.GHIJ)
C	- The name can start with an empty field (empty name or name starting
C	  with dot or minus sign) and end with an empty field (trailing dot).
C	  Other empty fields (2 subsequent dots) are not allowed.
C	- All other fields must be either alphanumeric or all minus signs.
C-------------------------------------------------------------------------
C
C
	CHARACTER*(*)	ANUM, MINUS, DELIM
		PARAMETER (ANUM = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789')
		PARAMETER (DELIM = '.')
		PARAMETER (MINUS = '-')
C
	INTEGER*4	STR_SIGLEN, STR_SKIP_W
	INTEGER*4	MSG_SET  
C
	INTEGER*4	IS, LN, PTR, SAVPTR
C
C
	LN = STR_SIGLEN (NODE)
	PTR = 1						! start of first field
	IF (NODE(1:1).EQ.DELIM) THEN			! field is empty
		IF (LN.EQ.1) GOTO 991			! next field is empty
		PTR = PTR+1				! start of next field
	ENDIF
	DO WHILE (PTR.LE.LN)
		SAVPTR = PTR
		IS = STR_SKIP_W (ANUM,NODE(:LN),PTR)
		IF (IS.EQ.0) IS = STR_SKIP_W (MINUS,NODE(:LN),PTR)
		IF (PTR.LE.LN) THEN
			IF (NODE(PTR:PTR).NE.DELIM) GOTO 992 ! invalid field
			IF (IS.EQ.0 .OR. PTR.EQ.LN) GOTO 991 ! empty field
			PTR = PTR+1			! start of next field
		ENDIF
	ENDDO
C
	DWC_NODE_CHECK = DWC_SUCCESS
	RETURN
C
 991	DWC_NODE_CHECK = MSG_SET (DWC_APPTWODOT,0)
	RETURN
 992	DWC_NODE_CHECK = MSG_SET (GEN_ISNOTANM,1)
	CALL WNCTXT(DWLOG,DWMSG,NODE(SAVPTR:PTR))
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_NODE_SET (NODE)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	NODE		! (i) node name
C
C.Purpose:	Set new current node if requested
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS	no "set current node" request
C	warning	DWC_SETCURNOD	new current node is set
C.Notes:
C	- If NODE ends with a colon, the current node will be set to NODE
C	  without the colon, and a message will be written.
C-------------------------------------------------------------------------
C
C
	CHARACTER*(*)	SETCUR, QUOTE
		PARAMETER (SETCUR = ':')
		PARAMETER (QUOTE  = '"')
C
	INTEGER*4	DWC_NODE_PUT
	INTEGER*4	STR_SIGLEN, MSG_SET
C
	INTEGER*4	IS, LN
	CHARACTER	TMP*80
C
C
C					Remember that the colon caused
C					GET_PARM to return the node name
C					as a quoted string
C
	LN = STR_SIGLEN (NODE)
	IF (LN.GE.3 .AND. NODE(LN-1:LN-1).EQ.SETCUR) GOTO 900
C
	DWC_NODE_SET = DWC_SUCCESS
	RETURN
C
 900	IS = DWC_NODE_PUT (NODE(2:LN-2))	! ignore false return
	DWC_NODE_SET = MSG_SET(DWC_SETCURNOD,1)
	TMP=QUOTE//NODE(2:LN-2)//QUOTE
	CALL WNCTXT(DWLOG,DWMSG,TMP)
	RETURN
	END
