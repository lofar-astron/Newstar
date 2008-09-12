C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	CPL_DYN
C.Keywords:	Compiler Utility, Dynamic Buffer
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C	The buffer is described in the status array (INTEGER*4 elements):
C	(1) = extend size (bytes)
C	(2) = size of dynamic buffer (0 = not yet allocated)
C	(3) = virtual address of dynamic buffer
C	(4) = nr of bytes written
C	(5) = nr of entries written
C	(6) = offset of last-written entry
C
C.Version:	900415 FMO - recreation
C.Version:	920214 GvD - no optional arguments in MSG anymore
C.Version:	940120 CMV - use WNGGVM i.s.o. GEN_GET_VM, use A_B etc.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION CPL_DYN_PUT (NBARR,BARR,DYNSTAT)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	NBARR		! (i) size of BARR in bytes
	BYTE		BARR(*)		! (i) array to be moved into the buffer
	INTEGER*4	DYNSTAT(6)	! (m) status array dynamic buffer
C
C.Purpose:	Write a byte array into the dynamic buffer
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	CPL_SUCCESS
C	fatal	CPL_DYNFILERR	error report left in message buffer
C.Notes:
C-------------------------------------------------------------------------
C
C
	INTEGER		MOVE_BLB, MSG_SET
	LOGICAL		WNGGVM, WNGFVM
C
	INTEGER*4	IS, OLDSIZ, OLDADR
C
C
C					If no buffer is allocated yet
C					or the current buffer is too small:
C					allocate/extend the buffer
C
	IF (NBARR.GT.0) THEN
	   DYNSTAT(6) = DYNSTAT(4)
	   DYNSTAT(4) = DYNSTAT(4) + NBARR
	   IF (DYNSTAT(4).GT.DYNSTAT(2)) THEN
	      OLDSIZ = DYNSTAT(2)
	      OLDADR = DYNSTAT(3)
	      DYNSTAT(2) = ((DYNSTAT(4)-1)/DYNSTAT(1)+1)*DYNSTAT(1)
	      IF (.NOT.WNGGVM(DYNSTAT(2),DYNSTAT(3))) GOTO 999
C
	      IF (OLDSIZ.NE.0) THEN
	         IS = MOVE_BLB (A_B(OLDADR-A_OB),
	1			A_B(DYNSTAT(3)-A_OB),
	1			DYNSTAT(6))
	         IF (.NOT.WNGFVM(OLDSIZ,OLDADR)) GOTO 999
	      ENDIF
	   ENDIF
C
C					Put BARR into the buffer
C
	   IS = MOVE_BLB (BARR,A_B(DYNSTAT(3)+DYNSTAT(6)-A_OB),NBARR)
	   IF (IAND(IS,1).EQ.0) GOTO 999
	   DYNSTAT(5) = DYNSTAT(5)+1
	ENDIF
C
C					Return
C
	CPL_DYN_PUT = CPL_SUCCESS
	RETURN
C
 999	CPL_DYN_PUT = MSG_SET (CPL_DYNFILERR,0)
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION CPL_DYN_WRITE (DYNSTAT,SWFREE)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	DYNSTAT(6)	! (i) status array dynamic buffer
	LOGICAL*4	SWFREE		! (i) release virtual memory ?
C
C.Purpose:	Write the dynamic buffer into the object file
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	CPL_SUCCESS
C	fatal	CPL_DYNWRTERR	error report left in message buffer
C.Notes:
C-------------------------------------------------------------------------
C
	LOGICAL		WNGFVM
	INTEGER*4	CPL_OBJ_WRITE
	INTEGER*4	CLEAR_BLJ
	INTEGER		MSG_SET
C
	INTEGER*4	IS
C
C
	IF (DYNSTAT(2).GT.0) THEN
	   IS = CPL_OBJ_WRITE (A_B(DYNSTAT(3)-A_OB),DYNSTAT(4))
	   IF (IAND(IS,1).EQ.0) GOTO 999
	   IF (SWFREE) THEN
	      IF (.NOT.WNGFVM(DYNSTAT(2),DYNSTAT(3))) GOTO 999
	      IS = CLEAR_BLJ (DYNSTAT(2),3)
	      IF (IAND(IS,1).EQ.0) GOTO 999
	   ENDIF
	ENDIF
C
	CPL_DYN_WRITE = CPL_SUCCESS
	RETURN
C
 999	CPL_DYN_WRITE = MSG_SET (CPL_DYNWRTERR,0)
	RETURN
	END
