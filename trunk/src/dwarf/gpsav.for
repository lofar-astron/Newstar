C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	GP_SAV
C.Keywords:	Program Parameters, Get Value, Save
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		Common fields used:
C	INTEGER*4	PARM$SAVADR	! (m) address of save buffer in VM
C	INTEGER*4	PARM$SAVLEN	! (m) significant length of save buffer
C
C.Version:	900412 FMO - combined parts of old routines
C.Version:	920214 GvD - no optional arguments in MSG anymore
C.Version:	940120 CMV - use WNGGVM i.s.o. GEN_GET_VM, indirect addressing
C.Version:	010709 AXC - linux port - parameter changes
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GP_SAV_INIT ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INCLUDE 'PARM_6_DEF'
C
C
C.Purpose:	Initialize the SAVE control fields
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C.Notes:
C-------------------------------------------------------------------------
C
C
	PARM$SAVLEN = 0
	PARM$SAVADR = 0
C
	GP_SAV_INIT = DWC_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GP_SAV_SWITCH (SWITCH)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INCLUDE 'PARM_6_DEF'
C
	LOGICAL*4	SWITCH		! (i) switch ON or OFF
C
C.Purpose:	Switch SAVE actions ON or OFF
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	false status from GEN_GET_VM
C.Notes:
C	- If necessary, virtual memory will be allocated.
C-------------------------------------------------------------------------
C
	INTEGER		MSG_SET
	LOGICAL		WNGGVM
C
	INTEGER*4	IS
C
C
C					PARM$SAVADR = 0: no SAVE buffer yet
C					PARM$SAVLEN < 0: SAVE currently OFF
C					PARM$SAVLEN > 0: SAVE currently ON
C
	PARM$SAVLEN = ABS (PARM$SAVLEN)
	IF (SWITCH) THEN
	   IF (PARM$SAVADR.EQ.0) THEN
		IF (.NOT.WNGGVM(PARM__LENVS,PARM$SAVADR)) GOTO 999
		PARM$SAVLEN = 0
	   ENDIF
	ELSE
	   PARM$SAVLEN = -PARM$SAVLEN
	ENDIF
C
	GP_SAV_SWITCH = DWC_SUCCESS
	RETURN
C
 999	GP_SAV_SWITCH = MSG_SET (DWC_PPDNOVIRT,1)
	CALL WNCTXT(DWLOG,DWMSG,PARM__LENVS)
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GP_SAV_WRITE (VALUE,LV)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INCLUDE 'PARM_6_DEF'
C
	CHARACTER*(*)	VALUE		! (i) value string
	INTEGER*4	LV		! (i) significant length of string
C
C.Purpose:	Save value string if SAVE is active
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	fatal	DWC_SAVEOVFLO	save-string overflow
C.Notes:
C-------------------------------------------------------------------------
C
C
	BYTE		SEPAR
C
	INTEGER		MSG_SET, MOVE_BLB
C
	INTEGER*4	IS
C
	SEPAR = ICHAR(';')
C
C					If SAVELAST active: save string value
C
	IF (PARM$SAVADR.NE.0 .AND. PARM$SAVLEN.GE.0) THEN
		IF (PARM$SAVLEN+LV+1.GT.PARM__LENVS) GOTO 999
		IF (PARM$SAVLEN.NE.0) THEN
			IS = MOVE_BLB (SEPAR,
	1			A_B(PARM$SAVADR+PARM$SAVLEN-A_OB),1)
			PARM$SAVLEN = PARM$SAVLEN+1
		ENDIF
		IS = MOVE_BLB (%REF(VALUE),
	1		A_B(PARM$SAVADR+PARM$SAVLEN-A_OB),LV)
		PARM$SAVLEN = PARM$SAVLEN+LV
	ENDIF
C
C
	GP_SAV_WRITE = DWC_SUCCESS
	RETURN
C
 999	GP_SAV_WRITE = MSG_SET (DWC_SAVEOVFLO,0)
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GP_SAV_DEFINE (SYMBOL,PKEY)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INCLUDE 'PARM_6_DEF'
C
	CHARACTER*(*)	SYMBOL		! (i) name of the symbol
	CHARACTER*(*)	PKEY		! (i) program's parameter name
C
C.Purpose:	Define the SAVE symbol for a program parameter
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C.Notes:
C-------------------------------------------------------------------------
C
C
	INTEGER		SYMBOL_DEFINE, MOVE_BLB, MSG_SET
C
	CHARACTER*255	VALUE
	INTEGER*4	IS, LV
C
C
	IF (PARM$SAVLEN.NE.0) THEN
		LV = ABS (PARM$SAVLEN)
		IS = MOVE_BLB (A_B(PARM$SAVADR-A_OB),%REF(VALUE),LV)
		IS = SYMBOL_DEFINE (SYMBOL,VALUE(:LV),DWC__GLOBALSYM)
		IF (IAND(IS,1).EQ.0) THEN
		   IS = MSG_SET (DWC_PARAMERR,1)
		   CALL WNCTXT(DWLOG,PKEY)
		END IF
	ENDIF
C
	GP_SAV_DEFINE = DWC_SUCCESS
	RETURN
	END
