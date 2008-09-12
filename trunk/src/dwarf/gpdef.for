C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	GP_DEF
C.Keywords:	Program Parameters, Get Value, Current Defaults
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.		WNB 920918 Changed test on DEFADYN to NE iso GT
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		Common fields used:
C	INTEGER*4	PARM$DEFLDYN	! (m) length of VM block (all sets)
C	INTEGER*4	PARM$DEFADYN	! (m) address of VM block
C	INTEGER*4	PARM$DEFAVAL	! (m) address of value part
C	INTEGER*4	PARM$DEFASW	! (m) address of switches part
C	INTEGER*4	PARM$DEFNRS	! (m) nr of sets
C	INTEGER*4	PARM$DEFVPS	! (m) reserved nr of values per set
C	INTEGER*4	PARM$DEFSNR	! (m) current set nr
C	INTEGER*4	PARM$DEFPTR	! (m) pointer to current value
C	INTEGER*4	PARM$DEFCNT	! (m) counter for TOBY format
C	INTEGER*4	PARM$DEFLDEF	! (m) length of VM buffer (single set)
C	INTEGER*4	PARM$DEFADEF	! (m) address of VM buffer
C
C.Version:	900325 FMO - creation
C.Version:	920214 GvD - no optional arguments in MSG anymore
C.Version:	920513 GvD - TOBY not allowed for logicals
C.Version:	940120 CMV - use WNGGVM i.s.o. GEN_GET_VM, indirect addressing
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GP_DEF ()
C	          ENTRY    GP_DEF_CLEAR ()
C	          ENTRY    GP_DEF_RELEASE ()
C	          ENTRY    GP_DEF_PUT (VALBLK,IS_OWNER)
C	          ENTRY    GP_DEF_GET (VALBLK,IS_OWNER,IS_FILLED)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INTEGER*4	GP_DEF_CLEAR, GP_DEF_RELEASE, GP_DEF_PUT, GP_DEF_GET
C
	INTEGER*4	VALBLK(8)	! (i/o) value block descriptor
	LOGICAL*4	IS_OWNER	! (i/o) block owned by this module ?
	LOGICAL*4	IS_FILLED	! (o) any value in block ?
C
C.Purpose:	Manipulate the description of current-defaults value block
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	false status codes returned by referenced routines
C.Notes:
C	- CLEAR clears the value block descriptor.
C	- RELEASE releases the memory occupied by the value block (provided
C	  that this module "owns" the block) and clears the descriptor.
C	- PUT copies a value-block descriptor into the current-defaults block
C	  descriptor, does or doesn't make this modules the block "owner" and
C	  resets the pointers to zero.
C	- GET returns the block descriptor and tells whether the block is owned
C	  by this module and whether it contains any value.
C-------------------------------------------------------------------------
C
	INCLUDE 'PARM_6_DEF'
C
	INTEGER*4	SCALAR_BIT, TOBY_BIT
		PARAMETER (SCALAR_BIT = 0)
		PARAMETER (TOBY_BIT   = 1)
C
	INTEGER*4	PPD_DTYPE_GET, PPD_NVAL_GET, PPD_AMAS_GET
	INTEGER*4	CLEAR_BLJ, MOVE_BLJ
	LOGICAL		WNGFVM
C
	CHARACTER*1	DTYPE
	INTEGER*4	IS, NVAL, MNVAL, MXVAL
	LOGICAL		TMP
C
C
	GP_DEF = DWC_SUCCESS			! dummy main entry point
	RETURN
C
C	==================
	ENTRY GP_DEF_CLEAR ()
C	==================
C
	IS = CLEAR_BLJ (PARM$DEFLDYN,11)
C
	GP_DEF_CLEAR = DWC_SUCCESS
	RETURN
C
C	====================
	ENTRY GP_DEF_RELEASE ()
C	====================
C
	IF (PARM$DEFLDYN.NE.0)
	1	TMP = WNGFVM(PARM$DEFLDYN,PARM$DEFADYN)
	IF (PARM$DEFADEF.NE.0)
	1	TMP = WNGFVM(PARM$DEFLDEF,PARM$DEFADEF)
	IS = CLEAR_BLJ (PARM$DEFLDYN,11)
C
	GP_DEF_RELEASE = DWC_SUCCESS
	RETURN
C
C	================
	ENTRY GP_DEF_PUT (VALBLK,IS_OWNER)
C	================
C
	IS = MOVE_BLJ (VALBLK,PARM$DEFLDYN,6)
	IF (.NOT.IS_OWNER) PARM$DEFLDYN = 0
	PARM$DEFSNR = 0					! current set nr
	PARM$DEFPTR = 0					! current value nr
	PARM$DEFCNT = 0					! TOBY counter
C
	GP_DEF_PUT = DWC_SUCCESS
	RETURN
C
C	================
	ENTRY GP_DEF_GET (VALBLK,IS_OWNER,IS_FILLED)
C	================
C
	IS = MOVE_BLJ (PARM$DEFLDYN,VALBLK,6)
	IF (IAND(IS,1).NE.0) IS = PPD_DTYPE_GET (DTYPE,VALBLK(7))
	IF (IAND(IS,1).NE.0) IS = PPD_NVAL_GET (NVAL,MNVAL,MXVAL)
	IF (IAND(IS,1).EQ.0) GOTO 999
	VALBLK(8) = 0					! flags
	IF (NVAL.EQ.1) VALBLK(8) = IBSET (VALBLK(8),SCALAR_BIT)
	IF (DTYPE.NE.'C' .AND. DTYPE.NE.'L'
	1			.AND. IAND(PPD_AMAS_GET('VECTOR'),1) .EQ. 0)
	2		VALBLK(8) = IBSET (VALBLK(8),TOBY_BIT)
	IS_OWNER = PARM$DEFLDYN.NE.0
	IS_FILLED = PARM$DEFADYN.NE.0			!!920918
C
	GP_DEF_GET = DWC_SUCCESS
	RETURN
C
 999	GP_DEF_GET = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GP_DEF_READ (ARRAY,NR,VALUE,LV)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	BYTE		ARRAY(*)	! (o) value array
	INTEGER*4	NR		! (o) nr of filled elements in array
	CHARACTER*(*)	VALUE		! (o) value string
	INTEGER*4	LV		! (o) significant length of string
C
C.Purpose:	Read the next value set from the defaults block
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	warning		0	end of value block reached
C	false status codes returned by referenced routines
C.Notes:
C	- The pointers will be updated.
C-------------------------------------------------------------------------
C
	INCLUDE 'PARM_6_DEF'
C
	INTEGER*4	GP_DEF_GET, PV_BLK_READ
C
	INTEGER*4	IS, VALBLK(8)
	LOGICAL*4	IS_OWNER, IS_FILLED
C
C
	IS = GP_DEF_GET (VALBLK,IS_OWNER,IS_FILLED)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IS = 0
	IF (IS_FILLED) IS = PV_BLK_READ (VALBLK,PARM$DEFSNR,PARM$DEFPTR,
	1				PARM$DEFCNT,ARRAY,NR,.TRUE.,VALUE,LV)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	GP_DEF_READ = DWC_SUCCESS
	RETURN
C
 999	GP_DEF_READ = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GP_DEF1 ()
C	          ENTRY    GP_DEF1_ALLOC (VALBLK)
C	          ENTRY    GP_DEF1_GET (ADDRESS)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INTEGER*4	GP_DEF1_ALLOC, GP_DEF1_GET
C
	INTEGER*4	VALBLK(8)	! (i) value block descriptor
	INTEGER*4	ADDRESS		! (o) buffer address
C
C.Purpose:	Manipulate the description of the single default set buffer
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	false status code returned by reference routines
C.Notes:
C	- ALLOC allocates memory for a single value set from the value block.
C	- GET returns the buffer address.
C-------------------------------------------------------------------------
C
	INCLUDE 'PARM_6_DEF'
C
	INTEGER*4	MSG_SET  
	LOGICAL		WNGGVM
C
	INTEGER*4	IS, NBYTES, ADDR
C
C
	GP_DEF1 = DWC_SUCCESS			! dummy main entry point
	RETURN
C
C	===================
	ENTRY GP_DEF1_ALLOC (VALBLK)
C	===================
C
	NBYTES = VALBLK(6)*VALBLK(7)
	IF (.NOT.WNGGVM(NBYTES,ADDR)) GOTO 999
	PARM$DEFLDEF = NBYTES
	PARM$DEFADEF = ADDR
C
	GP_DEF1_ALLOC = DWC_SUCCESS
	RETURN
C
 999	GP_DEF1_ALLOC = MSG_SET (DWC_PPDNOVIRT,1)
	CALL WNCTXT(DWLOG,DWMSG,NBYTES)
	RETURN
C
C	=================
	ENTRY GP_DEF1_GET (ADDRESS)
C	=================
C
	ADDRESS = PARM$DEFADEF
C
	GP_DEF1_GET = DWC_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GP_DEF_FILL (DEFSTR,LDEF,FLAGS)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	DEFSTR		! (i) default value (standard string)
	INTEGER*4	LDEF		! (i) significant length of DEFSTR
	INTEGER*4	FLAGS		! (i) flags
C
C.Purpose:	Fill the parameter's DEF value block
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	false status codes returned by referenced modules
C.Notes:
C	- Possible flags:
C	  PARM__OVERRIDE	program default overrides the SPECIFY value
C
C	The default value can be obtained in 2 ways:
C	- from the known value in the parameter area (this can be the
C	  SPECIFY value or the PPD default). This way will be taken if the
C	  value comes from SPECIFY and the user did not give PARM__OVERRIDE,
C	  or if the user did not provide a program default.
C	- from the program default the caller gave in the GET_PARM call.
C
C	The function will allocate virtual memory to store a single set from the
C	default value (this will be used by GP_VAL_FILL). It will free virtual
C	memory that is no longer needed.
C-------------------------------------------------------------------------
C
C
	INTEGER*4	GP_ASK_SET
	INTEGER*4	PV_DEF_DECODE, GP_DEF_GET, GP_DEF_PUT, GP_DEF_RELEASE
	INTEGER*4	GP_DEF1_ALLOC
	INTEGER*4	GP_INI_GET
C
	INTEGER*4	IS, INITYPE, INIBLK(8)
	INTEGER*4	SWTYPE, SWABSENT, VALBLK(8)
	LOGICAL*4	IS_OWNER, IS_FILLED
C
C
C					Free the current default dynamic storage
C
	IS = GP_DEF_RELEASE ()
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					If there is there a caller default
C					and there is no SPECIFY value
C					(or it must be overruled):
C					- convert it to a value block and
C					  store its description in DEF fields
C
	SWTYPE = 1					! assume no SPEC val
	IS = GP_INI_GET (INIBLK,INITYPE)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (LDEF.NE.0 .AND.
	1   (INITYPE.NE.1 .OR. IAND(FLAGS,PARM__OVERRIDE).NE.0)) THEN
		IS = PV_DEF_DECODE (DEFSTR,LDEF,VALBLK)
		IF (IAND(IS,1).EQ.0) GOTO 999
		IS_OWNER = .TRUE.
		IS = GP_DEF_PUT (VALBLK,IS_OWNER)
		IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Otherwise:
C					- use the SPECIFY value
C
	ELSE
		IS_OWNER = .FALSE.			! INI is owner
		IS = GP_DEF_PUT (INIBLK,IS_OWNER)
		IF (IAND(IS,1).EQ.0) GOTO 999
		IF (INITYPE.EQ.1) SWTYPE = -1		! SPEC val used
	ENDIF
C
C					If there is a default:
C					- allocate VM for a single set
C
	SWABSENT = 1					! assume no default
	IS = GP_DEF_GET (VALBLK,IS_OWNER,IS_FILLED)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (IS_FILLED) THEN
		IS = GP_DEF1_ALLOC (VALBLK)
		IF (IAND(IS,1).EQ.0) GOTO 999
		SWABSENT = 0
	ENDIF
C
C					Tell the ASK manager the type of
C					default (or its absence), and
C					let him set the final ask switch
C
	IS = GP_ASK_SET (SWABSENT,4)
	IF (IAND(IS,1).NE.0) IS = GP_ASK_SET (SWTYPE,5)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	GP_DEF_FILL = DWC_SUCCESS
	RETURN
C
 999	GP_DEF_FILL = IS
	RETURN
	END
