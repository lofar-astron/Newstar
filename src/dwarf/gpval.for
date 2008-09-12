C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	GP_VAL
C.Keywords:	Program Parameters, Get Value, VAL Block
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.		WNB 920918	Changed test on ADYN to NE iso GT
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		Common fields used:
C	INTEGER*4	PARM$VALLDYN	! (m) length of VM block (all sets)
C	INTEGER*4	PARM$VALADYN	! (m) address of VM block
C	INTEGER*4	PARM$VALAVAL	! (m) address of value part
C	INTEGER*4	PARM$VALASW	! (m) address of switches part
C	INTEGER*4	PARM$VALNRS	! (m) nr of sets
C	INTEGER*4	PARM$VALVPS	! (m) reserved nr of values per set
C	INTEGER*4	PARM$VALSNR	! (m) current set nr
C	INTEGER*4	PARM$VALPTR	! (m) pointer to current value
C	INTEGER*4	PARM$VALCNT	! (m) counter for TOBY format
C
C.Version:	900416 FMO - recreation
C.Version:	911213 GvD - clear VALBLK(1) at beginning of GP_VAL_FILL
C			was wrong when multiple defaults and /ASK
C.Version:	920122 GvD - also clear VALBLK(1) before asking; otherwise
C			same block is owned by VAL and DEF
C			   - pass .FALSE. to GP_VAL_PUT if default is used
C.Version:	920214 GvD - no optional arguments in MSG anymore
C.Version:	920513 GvD - TOBY not allowed for logicals
C.Version:	940120 CMV - use WNGGVM i.s.o. GEN_GET_VM, indirect addressing
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GP_VAL ()
C	          ENTRY    GP_VAL_CLEAR ()
C	          ENTRY    GP_VAL_RELEASE ()
C	          ENTRY    GP_VAL_PUT (VALBLK,IS_OWNER)
C	          ENTRY    GP_VAL_GET (VALBLK,IS_OWNER,IS_FILLED)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INCLUDE 'PARM_6_DEF'
C
	INTEGER*4	GP_VAL_CLEAR, GP_VAL_RELEASE, GP_VAL_PUT, GP_VAL_GET
C
	INTEGER*4	VALBLK(8)	! (i/o) description of value block
	LOGICAL*4	IS_OWNER	! (i/o) block owned by this module ?
	LOGICAL*4	IS_FILLED	! (o) any value in block ?
C
C.Purpose:	Manipulate the description of the value block
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	false status codes returned by referenced routines
C.Notes:
C	- CLEAR clears the value block descriptor.
C	- RELEASE releases the memoryoccupied by the value block (provided
C	  that this module "owns" the block) and clears the descriptor.
C	- PUT copies a value-block descriptor into the value block descriptor,
C	  does or doesn't make this module the block "owner", and resets the
C	  pointers to zero.
C	- GET returns the block descriptor and tells whether the block is owned
C	  by this module and whether it contains any value.
C-------------------------------------------------------------------------
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
	GP_VAL = DWC_SUCCESS
	RETURN
C
C	==================
	ENTRY GP_VAL_CLEAR ()
C	==================
C
	IS = CLEAR_BLJ (PARM$VALLDYN,9)
C
	GP_VAL_CLEAR = DWC_SUCCESS
	RETURN
C
C	====================
	ENTRY GP_VAL_RELEASE ()
C	====================
C
	IF (PARM$VALLDYN.NE.0)
	1		TMP = WNGFVM(PARM$VALLDYN,PARM$VALADYN)
	IS = CLEAR_BLJ (PARM$VALLDYN,9)
C
	GP_VAL_RELEASE = DWC_SUCCESS
	RETURN
C
C	================
	ENTRY GP_VAL_PUT (VALBLK,IS_OWNER)
C	================
C
	IS = MOVE_BLJ (VALBLK,PARM$VALLDYN,6)
	IF (.NOT.IS_OWNER) PARM$VALLDYN = 0
	PARM$VALSNR = 0				! current set nr
	PARM$VALPTR = 0				! current value nr
	PARM$VALCNT = 0				! TOBY counter
C
	GP_VAL_PUT = DWC_SUCCESS
	RETURN
C
C	================
	ENTRY GP_VAL_GET (VALBLK,IS_OWNER,IS_FILLED)
C	================
C
	IS = MOVE_BLJ (PARM$VALLDYN,VALBLK,6)
	IF (IAND(IS,1).NE.0) IS = PPD_DTYPE_GET (DTYPE,VALBLK(7))
	IF (IAND(IS,1).NE.0) IS = PPD_NVAL_GET (NVAL,MNVAL,MXVAL)
	IF (IAND(IS,1).EQ.0) GOTO 999
	VALBLK(8) = 0
	IF (NVAL.EQ.1)
	1	VALBLK(8) = IBSET (VALBLK(8),SCALAR_BIT)
	IF (DTYPE.NE.'C' .AND. DTYPE.NE.'L'
	1			.AND. IAND(PPD_AMAS_GET('VECTOR'),1) .EQ. 0)
	2	VALBLK(8) = IBSET (VALBLK(8),TOBY_BIT)
	IS_OWNER = PARM$VALLDYN.NE.0
	IS_FILLED = PARM$VALADYN.NE.0			!!920918
C
	GP_VAL_GET = DWC_SUCCESS
	RETURN
C
 999	GP_VAL_GET = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GP_VAL_READ_N (ARRAY,NR,VALUE,LV)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INCLUDE 'PARM_6_DEF'
C
	BYTE		ARRAY(*)	! (o) numerical value array
	INTEGER*4	NR		! (o) nr of filled elements in array
	CHARACTER*(*)	VALUE		! (o) value string
	INTEGER*4	LV		! (o) significant length of string
C
C.Purpose:	Read the next value set from the numerical value block
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	success	DWC_WILDCARD	wildcard value (also NR = -1 on return)
C	success	DWC_NULLVALUE	null value (also NR = 0 on return)
C	warning		0	end of value block reached
C	warning	DWC_ENDOFLOOP	value is end-of-loop signal
C	false status codes returned by referenced routines
C.Notes:
C	- The pointers will be updated.
C-------------------------------------------------------------------------
C
	INTEGER*4	GP_VAL_GET, PV_BLK_READ
C
	INTEGER*4	IS, VALBLK(8)
	LOGICAL*4	IS_OWNER, IS_FILLED
C
C
	IS = GP_VAL_GET (VALBLK,IS_OWNER,IS_FILLED)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IS = 0
	IF (IS_FILLED) IS = PV_BLK_READ (VALBLK,PARM$VALSNR,PARM$VALPTR,
	1				PARM$VALCNT,ARRAY,NR,.FALSE.,VALUE,LV)
C
C					Value set found:
C					- set status codes for special sets
C
	IF (IAND(IS,1).NE.0) THEN
		IF (NR.EQ.PARM__NULL) THEN			!0
			IS = DWC_NULLVALUE
		ELSE IF (NR.EQ.PARM__WILD) THEN			!-1
			IS = DWC_WILDCARD
		ELSE IF (NR.EQ.PARM__EOF) THEN			!-2
			IS = DWC_ENDOFLOOP
		ELSE
			IS = DWC_SUCCESS
		ENDIF
	ENDIF
C
 999	GP_VAL_READ_N = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GP_VAL_READ_C (ARRAY,NR,VALUE,LV)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INCLUDE 'PARM_6_DEF'
C
	CHARACTER*(*)	ARRAY(*)	! (o) character-type value array
	INTEGER*4	NR		! (o) nr of filled elements in array
	CHARACTER*(*)	VALUE		! (o) value string
	INTEGER*4	LV		! (o) significant length of string
C
C.Purpose:	Read the next value set from the character-type value block
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	success	DWC_WILDCARD	wildcard value (also NR = -1 on return)
C	success	DWC_NULLVALUE	null value (also NR = 0 on return)
C	warning		0	end of value block reached
C	warning	DWC_ENDOFLOOP	value is end-of-loop signal
C	false status codes returned by referenced routines
C.Notes:
C	- The pointers will be updated.
C-------------------------------------------------------------------------
C
	CHARACTER*(*)	BLANK
		PARAMETER (BLANK = ' ')
C
	INTEGER*4	GP_VAL_GET, PV_BLK_READ
	INTEGER		MOVE_BLB, MSG_SET
	LOGICAL		WNGGVM, WNGFVM
C
	INTEGER*4	IS, OFF, VALBLK(8), ADDRESS
	LOGICAL*4	IS_OWNER, IS_FILLED
	LOGICAL		TMP
C
C
	ADDRESS = 0
	IS = GP_VAL_GET (VALBLK,IS_OWNER,IS_FILLED)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IS = 0
	IF (IS_FILLED) THEN
		IF (.NOT.WNGGVM(VALBLK(6)*VALBLK(7),ADDRESS))
	1		GOTO 991		! set buffer
		IS = PV_BLK_READ (VALBLK,PARM$VALSNR,PARM$VALPTR,PARM$VALCNT,
	1			A_B(ADDRESS-A_OB),NR,.FALSE.,VALUE,LV)
	ENDIF
C
C					Value set found:
C					- set status codes for special sets
C					- copy blank-filled to output array
C
	IF (IAND(IS,1).NE.0) THEN
		IF (NR.EQ.PARM__NULL) THEN			!0
			IS = DWC_NULLVALUE
		ELSE IF (NR.EQ.PARM__WILD) THEN			!-1
			IS = DWC_WILDCARD
		ELSE IF (NR.EQ.PARM__EOF) THEN			!-2
			IS = DWC_ENDOFLOOP
		ELSE
		  OFF = 0
		  DO I = 1,NR
		     ARRAY(I) = BLANK
		     IS = MOVE_BLB (A_B(ADDRESS+OFF-A_OB),
	1				%REF(ARRAY(I)),VALBLK(7))
		     IF (IAND(IS,1).EQ.0) GOTO 999
		     OFF = OFF+VALBLK(7)
		  ENDDO
		  IS = DWC_SUCCESS
		ENDIF
	ENDIF
C
 999	GP_VAL_READ_C = IS
	IF (ADDRESS.NE.0) TMP=WNGFVM(VALBLK(6)*VALBLK(7),ADDRESS)
	RETURN
 991	GP_VAL_READ_C = MSG_SET (DWC_PPDNOVIRT,1)
	CALL WNCTXT(DWLOG,DWMSG,VALBLK(6)*VALBLK(7))
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GP_VAL_FILL (DEFSTR,LDEF,FLAGS)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	DEFSTR		! (i) default value (standard string)
	INTEGER*4	LDEF		! (i) significant length of DEFSTR
	INTEGER*4	FLAGS		! (i) flags to control GET_PARM
C
C.Purpose:	Fill the parameter's value block
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	warning	DWC_ENDOFLOOP	end-of-loop
C	false status codes returned by referenced routines
C.Notes:
C	Possible flags:
C	  PARM__OVERRIDE	program default overrides the SPECIFY value
C
C	The new value sets can come from the initial-default sets (EXE block),
C	the terminal or the program default. They will be stored in the
C	VAL block.
C
C	If the value sets must come from the terminal, GP_VAL_FILL will prompt
C	the user with the current default set (if available). Per prompt the
C	next default set will be used.
C
C	The user can give several answers:
C	- CTRL/Z or # means end-of-loop
C	- a value string, which will be checked and converted
C	- qualifiers /NOASK or /(NO)SAVELAST
C	- /ASK=keyword to reset the ASK switch for another keyword
C	- ? to ask for help information
C
C	If GP_VAL_FILL does not prompt, it will return the status end-of-loop
C	for parameters with the LOOP attribute in the PPD file when the value
C	sets are exhausted. The next time the value sets will be returned.
C
C	GP_VAL_FILL also (de-)activates SAVE according to the user's SAVE
C	qualifier (or DWARF's control parameter SAVE).
C-------------------------------------------------------------------------
C
	CHARACTER*(*)	BLANK
	INTEGER*4	OFF, USER
		PARAMETER (OFF = -1)
		PARAMETER (USER = 2)
		PARAMETER (BLANK = ' ')
C
	INTEGER*4	GP_ASK_SET, GP_ASK_INQ
	INTEGER*4	GP_DEF_FILL, GP_DEF_GET, GP_DEF1_GET, GP_DEF_READ
	INTEGER*4	GP_DEF_CLEAR, GP_DEF_RELEASE
	INTEGER*4	GP_INI_PUT
	INTEGER*4	GP_INP
	INTEGER*4	GP_LOOP_SWITCH, GP_LOOP_SET
	INTEGER*4	GP_SAV_SWITCH
	INTEGER*4	GP_VAL_PUT, GP_VAL_RELEASE
	INTEGER*4	PV_DEF_DECODE
	INTEGER*4	PPD_UNAM_GET
	INTEGER		MSG_SET
C
	CHARACTER	DEFAULT*255, KEY*16
	INTEGER*4	IS, LDEF1, LK, LKMIN, DLEVEL, DEFADR, NRDEF, VALBLK(8)
	LOGICAL*4	DO_ASK, DO_NOASK, DO_SAVE
	LOGICAL*4	IS_OWNER, IS_FILLED, PROTO
C
C
C					Set up
C					- free VAL block if still in use
C					- must the user be asked ?
C					- get the single-default-set buffer
C
	VALBLK(1) = 0
	IS = GP_VAL_RELEASE ()
	IF (IAND(IS,1).NE.0) IS = GP_ASK_INQ (DO_ASK)
	IF (IAND(IS,1).NE.0) IS = GP_DEF1_GET (DEFADR)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Get next value set from the default
C					(both as an array and as a string)
C					- DEFARR may still be in TOBY format,
C					  so it cannot be used directly when
C					  the default should be returned !
C
 100	IS = GP_DEF_READ (A_B(DEFADR-A_OB),NRDEF,DEFAULT,LDEF1)
	IF (IAND(IS,1).EQ.0) THEN
		IF (IS.NE.0) GOTO 999
C
C					No more defaults:
C					- try get new ones (if none, it is OK)
C					- refresh ask switch and buffer address
C
		IS = GP_DEF_FILL (DEFSTR,LDEF,FLAGS)
		IF (IAND(IS,1).NE.0) IS = GP_ASK_INQ (DO_ASK)
		IF (IAND(IS,1).NE.0) IS = GP_DEF1_GET (DEFADR)
		IF (IAND(IS,1).EQ.0) GOTO 999
C
C					If we have to ask the user:
C					- if there is a default: get a set
C					- otherwise: set blank default string
C
		IF (DO_ASK) THEN
			IS = GP_DEF_GET (VALBLK,IS_OWNER,IS_FILLED)
			IF (IAND(IS,1).EQ.0) GOTO 999
			IF (IS_FILLED) GOTO 100
			DEFAULT = BLANK
			LDEF1 = 0
C
C					If we don't have to ask:
C					- if end-of-loop condition:
C					  release default block and set status
C					- otherwise:
C					  move default to value block
C					- anyway: return
C
		ELSE
			IS = GP_LOOP_SWITCH ()
			IF (IS.EQ.DWC_EOFCTRLZ) THEN
				IS = GP_DEF_RELEASE ()
				IF (IAND(IS,1).NE.0) GOTO 990
			ELSE IF (IAND(IS,1).NE.0) THEN
				IS = GP_DEF_GET (VALBLK,IS_OWNER,IS_FILLED)
				IF (IAND(IS,1).NE.0)
	1				IS = GP_VAL_PUT (VALBLK,.FALSE.)
				IF (IAND(IS,1).NE.0) IS = GP_DEF_CLEAR ()
				IF (IAND(IS,1).NE.0) GOTO 900
			ENDIF
			GOTO 999
		ENDIF
	ENDIF
C
C					Get input from the user
C					- check and convert the input string
C					- store the data in a value block
C					- possibly return end-of-loop status
C
	DO_NOASK = .FALSE.
	DO_SAVE = .FALSE.
	DLEVEL = 0
	VALBLK(1) = 0
 200	IF (DO_ASK) THEN
 		IS = GP_INP (DEFAULT,LDEF1,DLEVEL,
	1			A_B(DEFADR-A_OB),NRDEF,
	1			VALBLK,DO_NOASK,DO_SAVE)
		IF (IS.EQ.DWC_EOFCTRLZ) GOTO 990
		IF (IAND(IS,1).EQ.0) GOTO 999
	ENDIF
C
C					If no value given: take default
C					- if there is none, repeat ask
C					- otherwise, decode the default
C
	IF (VALBLK(1).EQ.0) THEN
		IF (LDEF1.EQ.0) THEN
			IS = PPD_UNAM_GET (KEY,LK,LKMIN,PROTO)
			IS = MSG_SET (DWC_PARGIVVAL,1)
			CALL WNCTXT(DWLOG,DWMSG,KEY(:LK))
			DO_ASK = .TRUE.
			GOTO 200
		ENDIF
		IS = PV_DEF_DECODE (DEFAULT,LDEF1,VALBLK)
		IF (IAND(IS,1).EQ.0) GOTO 999
	ENDIF
C
C					If /NOASK given:
C					- store data in EXE and VAL blocks
C					- suppress further asking
C					- set loop switch to "value given"
C					Otherwise:
C					- store data in the VAL block
C
	IF (DO_NOASK) THEN
		IS = GP_INI_PUT (VALBLK,1)		! mimic SPECIFY type
		IF (IAND(IS,1).NE.0) IS = GP_VAL_PUT (VALBLK,.FALSE.)! EXE is owner
		IF (IAND(IS,1).NE.0) IS = GP_ASK_SET (OFF,USER)	! user said NOASK
		IF (IAND(IS,1).NE.0) IS = GP_LOOP_SET ()
	ELSE
		IS = GP_VAL_PUT (VALBLK,.TRUE.)		! VAL is owner
	ENDIF
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Activate or de-activate SAVE actions
C
	IS = GP_SAV_SWITCH (DO_SAVE)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C
 900	GP_VAL_FILL = DWC_SUCCESS
	RETURN
 990	GP_VAL_FILL = DWC_ENDOFLOOP
	RETURN
 999	GP_VAL_FILL = IS
	RETURN
	END
