C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	PPD_UNAM
C.Keywords:	PPD File, Parameter Name
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		Common variables used:
C	CHARACTER*16	PPDPD$UNAM	! (m) user's parameter name
C	INTEGER*4	PPDPD$LUNAM	! (m) length of unique abbreviation
C	CHARACTER*16	PPDPD$PNAM	! (m) program's parameter name
C
C.Version:	900415 FMO - recreation
C.Version:	920224 GvD - no optional arguments in MSG anymore
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_UNAM_PUT (NAME,DO_CHECK)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	NAME		! (i) proposed name
	LOGICAL*4	DO_CHECK	! (i) check the name ?
C
C.Purpose:	Check and store the user's name for a program parameter
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_UNAMNOT	no name given
C	error	PPD_MAX16	name too long
C	error	PPD_PARINV	invalid name
C	error	PPD_PARNOTUNI	name not unique
C.Notes:
C	The name and its significant length are stored in the current
C	parameter description (fields PPDPD$UNAM and PPDPD$LUNAM). Later on,
C	the length of the unique abbreviation will be determined and stored
C	in PPDPD$LUNAM.
C
C	A valid name:
C	- is not blank and not longer than 16 characters
C	- starts with an alphabetic, possibly prefixed with a '$' (prototype)
C	- contains only alphanumeric or underscore characters
C	- must be unique
C
C	In case of errors, no messages are stored in the regular message
C	buffer. The calling routine (BPD_BUILD) takes care of that.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDREC_4_DEF'
C
	CHARACTER*(*)	PROTO
		PARAMETER (PROTO = '$')
C
	INTEGER*4	BPD_INDEX_GETU
	INTEGER*4	STR_SIGLEN, STR_CHECK_ANUM_
 
C
	INTEGER*4	IS, LN, PTR
C
C
C					Store the name
C
	LN = STR_SIGLEN (NAME)
	PPDPD$UNAM = NAME(:LN)
	PPDPD$LUNAM = STR_SIGLEN (PPDPD$UNAM)
C
C					Check the syntax
C
	IF (DO_CHECK) THEN
		IF (LN.EQ.0) THEN
			IS = PPD_UNAMNOT
			GOTO 999
		ELSE IF (LN.GT.LEN(PPDPD$UNAM)) THEN
			IS = PPD_MAX16
			GOTO 999
		ENDIF
		PTR = 1
		IF (NAME(1:1).EQ.PROTO) PTR = 2
		IF (IAND(STR_CHECK_ANUM_(NAME(PTR:LN)),1) .EQ. 0) THEN
			IS = PPD_PARINV
			GOTO 999
		ENDIF
C
C					Check whether the name is unique
C					- loop through the index entries
C
		IS = BPD_INDEX_GETU (NAME(:LN))
		IF (IAND(IS,1).NE.0) THEN
			IS = PPD_PARNOTUNI
			GOTO 999
		ENDIF
	ENDIF
C
C
	PPD_UNAM_PUT = PPD_SUCCESS
	RETURN
C
 999	PPD_UNAM_PUT = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_PNAM_PUT (NAME,DO_CHECK)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	NAME		! (i) proposed name
	LOGICAL*4	DO_CHECK	! (i) check the name ?
C
C.Purpose:	Check and store the program's name for a program parameter
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_MAX16	name too long
C	error	PPD_PARINV	invalid name
C	error	PPD_PARNOTUNI	name not unique
C.Notes:
C	The name is stored in the current parameter description (field
C	PPDPD$PNAM). If no name is given, the user's name will be used
C	(without prototype prefix).
C
C	A valid name:
C	- is not longer than 16 characters
C	- starts with an alphabetic character
C	- contains only alphanumeric or underscore characters
C	- must be unique
C
C	In case of errors, no messages are stored in the regular message
C	buffer. The calling routine (BPD_BUILD) takes care of that.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDREC_4_DEF'
C
	CHARACTER*(*)	PROTO, EMPTVAL
		PARAMETER (PROTO   = '$' )
		PARAMETER (EMPTVAL = '[]')
C
	INTEGER*4	BPD_INDEX_GETP
	INTEGER*4	STR_SIGLEN, STR_CHECK_ANUM_
C
	INTEGER*4	IS, LN
C
C
C					Store the name
C
	LN = STR_SIGLEN (NAME)
	IF (LN.EQ.0 .OR. NAME(:LN).EQ.EMPTVAL) THEN
		IF (PPDPD$UNAM(1:1).NE.PROTO) THEN
			PPDPD$PNAM = PPDPD$UNAM
		ELSE
			PPDPD$PNAM = PPDPD$UNAM(2:)
		ENDIF
	ELSE
		PPDPD$PNAM = NAME(:LN)
	ENDIF
C
C					Check the syntax
C
	IF (LN.GT.0) THEN
		IF (LN.GT.LEN(PPDPD$PNAM)) THEN
			IS = PPD_MAX16
			GOTO 999
		ELSE IF (IAND(STR_CHECK_ANUM_(NAME(:LN)),1) .EQ. 0) THEN
			IS = PPD_PARINV
			GOTO 999
		ENDIF
	ENDIF
C
C					Check whether the name is unique
C					- loop through the index entries
C
	IS = BPD_INDEX_GETP (NAME(:LN))
	IF (IAND(IS,1).NE.0) THEN
		IS = PPD_PARNOTUNI
		GOTO 999
	ENDIF
C
	PPD_PNAM_PUT = PPD_SUCCESS
	RETURN
C
 999	PPD_PNAM_PUT = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_UNAM_GET (NAME,LN,LMIN,PROTOTYPE)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	NAME		! (o) user's name
	INTEGER*4	LN		! (o) its significant length
	INTEGER*4	LMIN		! (o) length of unique abbreviation
	LOGICAL*4	PROTOTYPE	! (o) prototype name ?
C
C.Purpose:	Get the user's parameter name from the current description
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_STRTOOSML	output string too short
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDREC_4_DEF'
C
	CHARACTER*(*)	PROTO
		PARAMETER (PROTO = '$')
C
	INTEGER*4	STR_SIGLEN, MSG_SET  
C
C
	NAME = PPDPD$UNAM
	LN = STR_SIGLEN (NAME)
	LMIN = PPDPD$LUNAM
	PROTOTYPE = PPDPD$UNAM(1:1).EQ.PROTO
C
	IF (LN.EQ.STR_SIGLEN(PPDPD$UNAM)) THEN
		PPD_UNAM_GET = PPD_SUCCESS
	ELSE
		PPD_UNAM_GET = MSG_SET (PPD_STRTOOSML,0)
	ENDIF
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_PNAM_GET (NAME,LN)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	NAME		! (o) program's name
	INTEGER*4	LN		! (o) its significant length
C
C.Purpose:	Get the program's parameter name from the current description
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_STRTOOSML	output string too short
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDREC_4_DEF'
C
	INTEGER*4	STR_SIGLEN, MSG_SET  
C
C
	NAME = PPDPD$PNAM
	LN = STR_SIGLEN (NAME)
C
	IF (LN.EQ.STR_SIGLEN(PPDPD$PNAM)) THEN
		PPD_PNAM_GET = PPD_SUCCESS
	ELSE
		PPD_PNAM_GET = MSG_SET (PPD_STRTOOSML,0)
	ENDIF
	RETURN
	END
