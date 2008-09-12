C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	DWC_TSTSYM
C.Keywords:
C.Author:	Ger van Diepen (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	830110 GVD - creation
C.Version:	901219 FMO - new code
C.Version:	910910 FMO - allow DWARF_<prognam>_CONTROL symbol names
C.Version:	920214 GvD - no optional arguments in MSG anymore
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_TSTSYM (SYMBOL)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*) SYMBOL		! (i) symbol name
C
C.Purpose:	Test whether a user's symbol name is legal to DWCL
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	error	DWC_INVSYMNAM	invalid name (too long or not alphanumeric)
C	error	DWC_RESERVSYM	reserved name
C.Notes:
C	A legal name is:
C	- not longer than 16 characters,
C	- alphanumeric with the first character alphabetic, or of the form
C	  DWARF_<prognam>_CONTROL, or equal to DWARF$X,
C	- not a reserved name (e.g. SHOW, LET, KEEP, etc...).
C-------------------------------------------------------------------------
C
C
	INTEGER*4	STR_SIGLEN, STR_CHECK_ANUMA, STR_MATCH_L, MSG_SET  
C
	INTEGER*4	LS, MATCHNR, IS
	CHARACTER*(*)	NAMELIST		! reserved names
		PARAMETER (NAMELIST =
	1		'LET,SPECIFY,VIEW,CLEAR,SAVE,RESTORE,'//
	2		'EXECUTE,ACTIVE,WAIT,CALCULATE,COMPILE,'//
	3		'TO,BY,WHILE,DO,IF,ENDDO,ENDIF')
C
C
	LS = STR_SIGLEN (SYMBOL)
	IF (LS.GT.32) GOTO 991
	IF (SYMBOL(:LS).EQ.'DWARF$X') GOTO 900
	IF (LS.GT.14 .AND. SYMBOL(:6).EQ.'DWARF_' .AND.
	1		 SYMBOL(LS-7:LS).EQ.'_CONTROL') GOTO 900
	IF (IAND(STR_CHECK_ANUMA(SYMBOL(:LS)),1) .EQ. 0) GOTO 991
	IS = STR_MATCH_L (SYMBOL,NAMELIST,MATCHNR)
	IF (IS.EQ.1) GOTO 992
C
 900	DWC_TSTSYM = DWC_SUCCESS
	RETURN
C
 991	DWC_TSTSYM = MSG_SET (DWC_INVSYMNAM,0)
	RETURN
 992	DWC_TSTSYM = MSG_SET (DWC_RESERVSYM,1)
	CALL WNCTXT(DWLOG,DWMSG,SYMBOL)
	RETURN
	END
