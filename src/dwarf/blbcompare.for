C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	BLB_COMPARE
C.Keywords:	Block of Bytes, Compare
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	890111 FMO - creation
C.Version:	980112 FMO - added BLB_COMPAR1
C.Version:	920214 GvD - no optional arguments in MSG anymore
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION BLB_COMPARE (VAL1,VAL2,DTYPE,LENG)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	BYTE		VAL1(*)		! (i) first value
	BYTE		VAL2(*)		! (i) second value
	CHARACTER*1	DTYPE		! (i) datatype code (B,I,J,R,D,C,L)
	INTEGER*4	LENG		! (i) length of C_type value
C
C.Purpose:	Compare two values of the given type
C.Returns:	1 (VAL1=VAL2), 0 (VAL1<VAL2) , or 2 (VAL1>VAL2)
C.Notes:
C	Logical values (type 'L') are assumed to be 1 byte long; only the least
C	significant bit is tested. If these are not equal, 0 will be returned.
C-------------------------------------------------------------------------
C
	INTEGER*4	MOVE_BLB, MOVE_BLI, MOVE_BLJ, MOVE_BLR, MOVE_BLD
C
	INTEGER*4	IS
	INTEGER*2	II1, II2
	REAL*4		R2
	DOUBLE PRECISION	D2
	CHARACTER*256	C1, C2
C
C
	IF (DTYPE.EQ.'B') THEN
		IF (VAL1(1).LT.VAL2(1)) GOTO 900
		IF (VAL1(1).GT.VAL2(1)) GOTO 902
	ELSE IF (DTYPE.EQ.'I') THEN
		IS = MOVE_BLI (VAL1,II1,1)
		IS = MOVE_BLI (VAL2,II2,1)
		IF (II1.LT.II2) GOTO 900
		IF (II1.GT.II2) GOTO 902
	ELSE IF (DTYPE.EQ.'J') THEN
		IS = MOVE_BLJ (VAL1,J1,1)
		IS = MOVE_BLJ (VAL2,J2,1)
		IF (J1.LT.J2) GOTO 900
		IF (J1.GT.J2) GOTO 902
	ELSE IF (DTYPE.EQ.'R') THEN
		IS = MOVE_BLR (VAL1,R1,1)
		IS = MOVE_BLR (VAL2,R2,1)
		IF (R1.LT.R2) GOTO 900
		IF (R1.GT.R2) GOTO 902
	ELSE IF (DTYPE.EQ.'D') THEN
		IS = MOVE_BLD (VAL1,D1,1)
		IS = MOVE_BLD (VAL2,D2,1)
		IF (D1.LT.D2) GOTO 900
		IF (D1.GT.D2) GOTO 902
	ELSE IF (DTYPE.EQ.'C') THEN
		IS = MOVE_BLB (VAL1,%REF(C1),LENG)
		IS = MOVE_BLB (VAL2,%REF(C2),LENG)
		IF (C1.LT.C2) GOTO 900
		IF (C1.GT.C2) GOTO 902
	ELSE IF (DTYPE.EQ.'L') THEN
		IF (VAL1(1)-VAL2(1)) GOTO 900
	ELSE
		CALL WNCTXT(DWLOG,'Invalid datatype code in BLB_COMPARE')
		CALL WNGEX
	ENDIF
C
	BLB_COMPARE = 1
	RETURN
C
 900	BLB_COMPARE = 0
	RETURN
C
 902	BLB_COMPARE = 2
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION BLB_COMPAR1 (VAL,DTYPE)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	BYTE		VAL(*)		! (i) value
	CHARACTER*1	DTYPE		! (i) datatype code (B,I,J,R,D)
C
C.Purpose:	Compare numerical value with 1
C.Returns:	1 (VAL = 1), 0 (VAL < 1) , or 2 (VAL > 1)
C.Notes:
C-------------------------------------------------------------------------
C
	INTEGER*4	MOVE_BLI, MOVE_BLJ, MOVE_BLR, MOVE_BLD
C
	INTEGER*4	IS
	INTEGER*2	II
	REAL*4		R
	REAL*8		D
C
C
	IF (DTYPE.EQ.'B') THEN
		IF (VAL(1).LT.1) GOTO 900
		IF (VAL(1).GT.1) GOTO 902
	ELSE IF (DTYPE.EQ.'I') THEN
		IS = MOVE_BLI (VAL,II,1)
		IF (II.LT.1) GOTO 900
		IF (II.GT.1) GOTO 902
	ELSE IF (DTYPE.EQ.'J') THEN
		IS = MOVE_BLJ (VAL,J,1)
		IF (J.LT.1) GOTO 900
		IF (J.GT.1) GOTO 902
	ELSE IF (DTYPE.EQ.'R') THEN
		IS = MOVE_BLR (VAL,R,1)
		IF (R.LT.1.0) GOTO 900
		IF (R.GT.1.0) GOTO 902
	ELSE IF (DTYPE.EQ.'D') THEN
		IS = MOVE_BLD (VAL,D,1)
		IF (D.LT.1.D0) GOTO 900
		IF (D.GT.1.D0) GOTO 902
	ELSE
		CALL WNCTXT(DWLOG,'Invalid datatype code in BLB_COMPAR1')
		CALL WNGEX
	ENDIF
C
	BLB_COMPAR1 = 1
	RETURN
C
 900	BLB_COMPAR1 = 0
	RETURN
C
 902	BLB_COMPAR1 = 2
	RETURN
	END
