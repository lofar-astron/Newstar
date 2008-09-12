C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	GEN_CVT
C.Keywords:	Numbers, Convert
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	900305 FMO - creation
C.Version:	920214 GvD - no optional arguments in MSG anymore
C.Version:	920508 GvD - added entry GEN_CVT_NR_L to convert logicals
C.Version:		- use FLOAT(LARGEST_B) for SUN
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GEN_CVT ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Make source-module name known
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	GEN_SUCCESS
C.Notes:	Dummy routine
C-------------------------------------------------------------------------
C
	GEN_CVT = GEN_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GEN_CVT_NR_D (DATYP,NR,DVAL)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*1	DATYP		! (i) data type (B,I,J,R,D)
	BYTE		NR(*)		! (i) input nr of type DATYP
	REAL*8		DVAL		! (o) output nr		
C
C.Purpose:	Convert a number of given type to REAL*8
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	GEN_SUCCESS
C	error	GEN_INVDATTYP	invalid data type
C.Notes:
C-------------------------------------------------------------------------
C
C
	INTEGER*4	MOVE_BLB, MSG_SET  
C
	INTEGER*2	II
	INTEGER*4	IS
	REAL*4		R
C
	IF (DATYP.EQ.'B') THEN
		DVAL = NR(1)
	ELSE IF (DATYP.EQ.'I') THEN
		IS = MOVE_BLB (NR,II,2)
		DVAL = II
	ELSE IF (DATYP.EQ.'J') THEN
		IS = MOVE_BLB (NR,J,4)
		DVAL = J
	ELSE IF (DATYP.EQ.'R') THEN
		IS = MOVE_BLB (NR,R,4)
		DVAL = R
	ELSE IF (DATYP.EQ.'D') THEN
		IS = MOVE_BLB (NR,DVAL,8)
	ELSE
		GOTO 999
	ENDIF
C
	GEN_CVT_NR_D = GEN_SUCCESS
	RETURN
C
 999	GEN_CVT_NR_D = MSG_SET (GEN_INVDATTYP,1)
	CALL WNCTXT(DWLOG,DWMSG,DATYP)
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GEN_CVT_D_NR (DATYP,DVAL,NR)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*1	DATYP		! (i) data type (B,I,J,R,D)
	REAL*8		DVAL		! (i) input nr		
	BYTE		NR(*)		! (o) output nr of type DATYP
C
C.Purpose:	Convert a REAL*8 number to given type
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	GEN_SUCCESS
C	info		3	truncated nr
C	error	GEN_INVDATTYP	invalid data type
C.Notes:
C-----------------------------------------------------------------------
C
C
	INTEGER*4	MOVE_BLB, MSG_SET  
C
	INTEGER*2	II
	INTEGER*4	IS, TMP
	REAL*4		R
C
C
	IS = GEN_SUCCESS
	TMP=LARGEST_B
	IF (DATYP.EQ.'B') THEN
		IF (DVAL.EQ.UNDEF_D) THEN
			NR(1) = UNDEF_B
		ELSE IF (DVAL.GE.FLOAT(TMP)+0.5) THEN
			NR(1) = LARGEST_B
			IS = 3
		ELSE IF (DVAL.LE.-FLOAT(TMP)-0.5) THEN
			I     = LARGEST_B
			NR(1) = -I
			IS = 3
		ELSE
			NR(1) = NINT(DVAL)
		ENDIF
C
	ELSE IF (DATYP.EQ.'I') THEN
		IF (DVAL.EQ.UNDEF_D) THEN
			II = UNDEF_I
		ELSE IF (DVAL.GE.FLOAT(LARGEST_I)+0.5) THEN
			II = LARGEST_I
			IS = 3
		ELSE IF (DVAL.LE.-FLOAT(LARGEST_I)-0.5) THEN
			II = -LARGEST_I
			IS = 3
		ELSE
			II = NINT(DVAL)
		ENDIF
		TMP = MOVE_BLB (II,NR,2)
C
	ELSE IF (DATYP.EQ.'J') THEN
		IF (DVAL.EQ.UNDEF_D) THEN
			J = UNDEF_J
		ELSE IF (DVAL.GE.FLOAT(LARGEST_J)+0.5) THEN
			J = LARGEST_J
			IS = 3
		ELSE IF (DVAL.LE.-FLOAT(LARGEST_J)-0.5) THEN
			J = -LARGEST_J
			IS = 3
		ELSE
			J = NINT(DVAL)
		ENDIF
		TMP = MOVE_BLB (J,NR,4)
C
	ELSE IF (DATYP.EQ.'R') THEN
		IF (DVAL.EQ.UNDEF_D) THEN
			R = UNDEF_R
		ELSE
			R = DVAL
		ENDIF
		TMP = MOVE_BLB (R,NR,4)
C
	ELSE IF (DATYP.EQ.'D') THEN
		TMP = MOVE_BLB (DVAL,NR,8)
	ELSE
		GOTO 999
	ENDIF
C
C
	GEN_CVT_D_NR = IS
	RETURN
C
 999	GEN_CVT_D_NR = MSG_SET (GEN_INVDATTYP,1)
	CALL WNCTXT(DWLOG,DWMSG,DATYP)
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GEN_CVT_NR_L (DATYP,VAL)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*1	DATYP		! (i) data type (B,I,J)
	BYTE		VAL(*)		! (m) value
C
C.Purpose:	Convert a number of given type to LOGICAL*1, *2 or *4
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	GEN_SUCCESS
C	error	GEN_INVDATTYP	invalid data type
C.Notes:
C-------------------------------------------------------------------------
C
C
	INTEGER*4	MOVE_BLB, MSG_SET  
C
	INTEGER*2	II2
	INTEGER*4	II4, IS
	LOGICAL*1	LL1
	LOGICAL*2	L2
	LOGICAL*4	L4
C
	IF (DATYP.EQ.'B') THEN
		II2 = VAL(1)
		LL1 = IAND(II2,1).EQ.1
		IS = MOVE_BLB (LL1,VAL,1)
	ELSE IF (DATYP.EQ.'I') THEN
		IS = MOVE_BLB (VAL,II2,2)
		L2 = IAND(II2,1).EQ.1
		IS = MOVE_BLB (L2,VAL,2)
	ELSE IF (DATYP.EQ.'J') THEN
		IS = MOVE_BLB (VAL,II4,4)
		L4 = IAND(II4,1).EQ.1
		IS = MOVE_BLB (L4,VAL,4)
	ELSE
		GOTO 999
	ENDIF
C
	GEN_CVT_NR_L = GEN_SUCCESS
	RETURN
C
 999	GEN_CVT_NR_L = MSG_SET (GEN_INVDATTYP,1)
	CALL WNCTXT(DWLOG,DWMSG,DATYP)
	RETURN
	END
