C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	GP_ASK
C.Keywords:	Program Parameters, Get Value, Ask Control
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		Common fields used:
C	INTEGER*4	PARM$ASK	! (m) parameter's ask switch
C	INTEGER*4	PARM$ASKSW	! (m) prompt user ?
C
C.Version:	900302 FMO - creation
C.Version:	920214 GvD - no optional arguments in MSG anymore
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GP_ASK ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Make source module name known
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C.Notes:	Dummy routine
C-------------------------------------------------------------------------
C
C
	GP_ASK = DWC_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GP_ASK_INIT (SWITCH)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	SWITCH(2)	! (i) ask switches
C
C.Purpose:	Initialize the ASK control
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C.Notes:
C	- SWITCH are the ask switches derived from the ASK qualifiers on the
C	  initial parameter defaults (1 for SPECIFY, 2 for PPD).
C	  = -1 (/NOASK), = 0 (not given) or = 1 (/ASK).
C-------------------------------------------------------------------------
C
	INCLUDE 'PARM_6_DEF'
C
	INTEGER*4	DWC_ASK_INQ
C
C
	PARM$ASKSW = 0
	IF (SWITCH(1).NE.0) THEN		! SPECIFY default with /[NO]ASK
		PARM$ASK = SWITCH(1)
	ELSE IF (IAND(DWC_ASK_INQ(),1) .NE. 0) THEN	! user gave EXECUTE/ASK
		PARM$ASK = 1			!  or set DWARF ASK control on
	ELSE
		PARM$ASK = SWITCH(2)
	ENDIF
C
	GP_ASK_INIT = DWC_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GP_ASK_SET (SWITCH,TYPE)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	SWITCH		! (i) ask switch
	INTEGER*4	TYPE		! (i) type of switch
C
C.Purpose:	Switch ASK action ON or OFF
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	error	DWC_PARNOVAL	no value available in batch mode
C.Notes:
C	- SWITCH = 1 (ON), -1 (OFF).
C	- TYPE = 1 (from default), 2 (from user input), 3 (from RETRY_PARM),
C		4 (from absence of def value), 5 (from type of default used).
C	- Any other SWITCH or TYPE will leave things unchanged.
C-------------------------------------------------------------------------
C
	INCLUDE 'PARM_6_DEF'
C
	INTEGER*4	DWC_IBMODE_INQ, PPD_AMAS_GET, MSG_SET  
	LOGICAL*4	NO_DEFAULT
C
C
C					ASK qualifier from user
C
	IF (TYPE.EQ.1 .OR. TYPE.EQ.2) THEN
		IF (SWITCH.EQ.1) THEN
			PARM$ASK = 1
		ELSE IF (SWITCH.EQ.-1) THEN
			PARM$ASK = -1
		ENDIF
C
C					RETRY_PARM from program
C
	ELSE IF (TYPE.EQ.3) THEN
		IF (SWITCH.EQ.1) THEN
			PARM$ASK = PARM$ASK+100
		ENDIF
C
C					Absence/presence of default value
C
	ELSE IF (TYPE.EQ.4) THEN
		NO_DEFAULT = SWITCH.EQ.1
C
C					Do we prompt or don't we ?
C
	ELSE IF (TYPE.EQ.5) THEN
		IF (PARM$ASK.GT.50) THEN		! RETRY_PARM active:
			PARM$ASK = PARM$ASK-100		!	de-activate
			PARM$ASKSW = .TRUE.		!	yes
		ELSE IF (IAND(DWC_IBMODE_INQ('BATCH'),1) .NE. 0) THEN	! batch
			IF (NO_DEFAULT) GOTO 999	!	value required
			PARM$ASKSW = .FALSE.		!	no
		ELSE IF (NO_DEFAULT) THEN		! value absent:
			PARM$ASKSW = .TRUE.		!	yes
		ELSE IF (IAND(PPD_AMAS_GET('ASK'),1) .NE. 0) THEN ! PPD ASK attr
			PARM$ASKSW = .TRUE.		!	yes
		ELSE IF (PARM$ASK.NE.0) THEN		! user gave /(NO)ASK:
			PARM$ASKSW = PARM$ASK.EQ.1	!	follow it
		ELSE					! default action:
			PARM$ASKSW = SWITCH.EQ.1	!	yes, unless the
		ENDIF					!	SPEC def is used
	ENDIF
C
	GP_ASK_SET = DWC_SUCCESS
	RETURN
C
 999	GP_ASK_SET = MSG_SET (DWC_PARNOVAL,0)
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GP_ASK_INQ (SWITCH)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	LOGICAL*4	SWITCH		! (o) prompt for input ?
C
C.Purpose:	Inquire whether the program must prompt for values
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'PARM_6_DEF'
C
	SWITCH = PARM$ASKSW
	GP_ASK_INQ = DWC_SUCCESS
	RETURN
	END
