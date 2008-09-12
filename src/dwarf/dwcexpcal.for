C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	DWC_EXPCAL
C.Keywords:	DWARF Expression, Evaluate
C.Author:	Ger van Diepen (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	820715 GVD - creation DWCEXPCAL
C.Version:	840622 GVD - replaced UDF_RDUNI by READ_UNIT
C.Version:	900110 FMO - Remove obsolete arg's UNIADR and UNISIZE
C.Version:	900111 FMO - Catch unit '1' (i.e. no unit)
C.Version:	900321 FMO - use DWC_EXPR_SOLVE
C.Version:	900416 FMO - logical*4 arguments
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_EXPCAL (STRING,UNITSTR,STREAM,CHKSW,
	1						SWSYM,RESULT,ERRPTR)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRING		! (m) expression
	CHARACTER*(*)	UNITSTR		! (i) possible unit codes
	CHARACTER*(*)	STREAM		! (i) stream name for DWARF symbol names
	LOGICAL*4	CHKSW		! (i) unknown symbols allowed ?
	LOGICAL*4	SWSYM		! (m) unknown symbols found ?
	REAL*8		RESULT		! (o) value
	INTEGER*4	ERRPTR		! (o) position of error in STRING
C
C.Purpose:	Evaluate an expression to a REAL*8 value
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	false status code from READ_UNIT or DWC_EXPR_SOLVE
C.Notes:
C	- The first code in the unit string is the default unit code. The
C	  result of the calculation will be expressed in that unit. The default
C	  unit for trigonometric functions will be degrees.
C-------------------------------------------------------------------------
C
C
	CHARACTER*(*)	BLANK, COMMA, DEFUNIT
		PARAMETER (BLANK   = ' ')
		PARAMETER (COMMA   = ',')
		PARAMETER (DEFUNIT = 'DEG')
C
	INTEGER*4	DWC_EXPR_SOLVE, STR_COPY_U, READ_UNIT
C
	CHARACTER*10	UNIT, GROUP
	INTEGER*4	IS, LU, PTR
	REAL*8		FACTOR
C
C
C
C					Get the default unit
C
	IF (UNITSTR.EQ.BLANK) THEN
		UNIT = DEFUNIT
	ELSE
		UNIT = ' '
		LU = 0
		PTR = 1
		IS =  STR_COPY_U (COMMA,UNITSTR,PTR,UNIT,LU)
		IF (UNIT(:LU).EQ.'1') UNIT = DEFUNIT
	ENDIF
C
C					Get the conversion factor
C					and evaluate the expression
C
	ERRPTR = 0
	IS = READ_UNIT (UNIT,GROUP,FACTOR)
	IF (IAND(IS,1).NE.0) IS = DWC_EXPR_SOLVE
	1	(STRING,STREAM,FACTOR,UNITSTR,RESULT,ERRPTR,CHKSW,SWSYM)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C
	DWC_EXPCAL = DWC_SUCCESS
	RETURN
C
 999	DWC_EXPCAL = IS
	RETURN
	END
