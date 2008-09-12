C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	DWC_SYSOUT
C.Keywords:	DWARF, Standard Output Device
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C	The output device type is kept in a local saved variable with value
C	0 (terminal) or 1 (otherwise)
C.Version:	900301 FMO - creation
C.Version:	910826 FMO - PARM_n no longer needed
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER FUNCTION DWC_SYSOUT_SET ()
C	        ENTRY    DWC_SYSOUT_INQ (SYSOUT)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INTEGER		DWC_SYSOUT_INQ
C
	CHARACTER*(*)	SYSOUT		!(i) device type
C
C.Purpose:	Set or inquire after the standard output device
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS	for SET
C	success		1	for INQ if SYSOUT is the correct type
C	warning		0	for INQ if SYSOUT is not the correct type
C	error		2	for INQ if SYSOUT is invalid type (no msg)
C.Notes:
C	SET:
C	- For a terminal store 0, otherwise store 1.
C	INQ:
C	- SYSOUT can be 'TERMINAL' or an abbreviation.
C-------------------------------------------------------------------------
C
C
	CHARACTER*(*)	TYPE_LIST
		PARAMETER (TYPE_LIST = 'TERMINAL')
C
	INTEGER		GEN_TERMSW, STR_MATCH_L
C
	INTEGER		IS, NR
	INTEGER		OUTPUT_DEVICE
		DATA OUTPUT_DEVICE /0/
		SAVE OUTPUT_DEVICE
C
C
	IF (IAND(GEN_TERMSW('SYS$OUTPUT'),1) .NE. 0) THEN
		OUTPUT_DEVICE = 0
	ELSE
		OUTPUT_DEVICE = 1
	ENDIF
C
	DWC_SYSOUT_SET = DWC_SUCCESS
	RETURN
C
C	====================
	ENTRY DWC_SYSOUT_INQ (SYSOUT)
C	====================
C
	IS = STR_MATCH_L (SYSOUT,TYPE_LIST,NR)
	IF (NR.EQ.0) THEN
		DWC_SYSOUT_INQ = 2
	ELSE IF (OUTPUT_DEVICE.EQ.0) THEN
		DWC_SYSOUT_INQ = 1
	ELSE
		DWC_SYSOUT_INQ = 0
	ENDIF
C
	RETURN
	END
