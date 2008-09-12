C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	GP_LOOP
C.Keywords:	Program Parameters, Get Value, Loop Control
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C	Common fields used:
C	INTEGER*4	PARM$LOOP	! (m) loop switch
C
C	- PARM$LOOP = 0 for parameters without the LOOP attribute (always).
C	- PARM$LOOP = 2 initially for a LOOP parameter. The next GET_PARM
C		will return the first of the available value sets.
C	- PARM$LOOP = 1	once the program has started using the values sets.
C		When the sets are exhausted, the switch is reset to 2 and
C		GET_PARM will return with the warning status DWC_ENDOFLOOP.
C
C.Version:	900322 FMO - combined pieces of old routines
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GP_LOOP_INIT ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Initialize the LOOP switch
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'PARM_6_DEF'
C
	INTEGER*4	PPD_AMAS_GET
C
	PARM$LOOP = 0
	IF (IAND(PPD_AMAS_GET('LOOP'),1) .NE. 0) PARM$LOOP = 2
C
	GP_LOOP_INIT = DWC_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GP_LOOP_SWITCH ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Flip the LOOP switch for a parameter with the LOOP attribute
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS	also if no LOOP attribute
C	warning	DWC_EOFCTRLZ	end-of-loop signal
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'PARM_6_DEF'
C
C
	IF (PARM$LOOP.EQ.0) THEN
		GP_LOOP_SWITCH = DWC_SUCCESS
	ELSE IF (PARM$LOOP.EQ.1) THEN
		GP_LOOP_SWITCH = DWC_EOFCTRLZ
		PARM$LOOP = 2
	ELSE
		GP_LOOP_SWITCH = DWC_SUCCESS
		PARM$LOOP = 1
	ENDIF
C
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GP_LOOP_SET ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Set the LOOP switch for parameter with LOOP attribute
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'PARM_6_DEF'
C
	IF (PARM$LOOP.GT.0) PARM$LOOP = 1
	GP_LOOP_SET = DWC_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GP_LOOP_RESET ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Reset the LOOP switch to its initial condition
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'PARM_6_DEF'
C
	IF (PARM$LOOP.GT.0) PARM$LOOP = 2
	GP_LOOP_RESET = DWC_SUCCESS
	RETURN
	END
