C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	PPD_IOCD
C.Keywords:	PPD File, Parameter I/O Code
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		Common variables used:
C	CHARACTER*1	PPDPD$IOCD	! (m) I/O code
C
C.Version:	900415 FMO - recreation
C.Version:	920224 GvD - no optional arguments in MSG anymore
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_IOCD_PUT (STRING,DO_CHECK)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRING		! (i) proposed code
	LOGICAL*4	DO_CHECK	! (i) check validity ?
C
C.Purpose:	Check and store the I/O code for a program parameter
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_IOINV	invalid code
C.Notes:
C	- The code will be abbreviated to 1 character and stored in the
C	  current parameter description (field PPDPD$IOCD).
C	- If an invalid code is given, the default ('INPUT') will be used.
C	- If DO_CHECK is off, the function just returns with PPD_SUCCESS.
C	- In case of errors, no messages are stored in the regular message
C	  buffer. The calling routine (BPD_BUILD) takes care of that.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDREC_4_DEF'
C
	INTEGER*4	MAXNR
	CHARACTER*(*)	EMPTVAL, DEFIOCD
		PARAMETER (MAXNR   =  4  )
		PARAMETER (EMPTVAL = '[]')
		PARAMETER (DEFIOCD = 'I' )
	CHARACTER*6	NAMES(MAXNR)
		DATA NAMES /'INPUT','OUTPUT','MODIFY','NONE'/
C
	INTEGER*4	STR_SIGLEN, STR_MATCH_A
C
	INTEGER*4	IS, LS, NR
C
C
C					Check and abbreviate the code
C
	IF (DO_CHECK) THEN
		PPDPD$IOCD = DEFIOCD
		LS = STR_SIGLEN (STRING)
		IF (LS.GT.0 .AND. STRING(:LS).NE.EMPTVAL) THEN
			IS = STR_MATCH_A (STRING(:LS),MAXNR,NAMES,NR)
			IF (IAND(IS,1).EQ.0) GOTO 999
			PPDPD$IOCD = STRING(:1)
		ENDIF
	ENDIF
C
	PPD_IOCD_PUT = PPD_SUCCESS
	RETURN
C
 999	PPD_IOCD_PUT = PPD_IOINV
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_IOCD_GET (CODE,LC)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	CODE		! (o) full I/O code
	INTEGER*4	LC		! (o) its significant length
C
C.Purpose:	Get the I/O code from the current parameter description
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_IOINV	invalid I/O code
C	error	PPD_STRTOOSML	output string too short
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDREC_4_DEF'
C
	INTEGER*4	MAXNR
		PARAMETER (MAXNR = 4)
	CHARACTER*6	NAMES(MAXNR)
		DATA NAMES /'INPUT','OUTPUT','MODIFY','NONE'/
C
	INTEGER*4	STR_SIGLEN, STR_MATCH_A, MSG_SET  
C
	INTEGER*4	IS, NR
C
C
	CODE = ' '
	LC = 0
C
	IS = STR_MATCH_A (PPDPD$IOCD,MAXNR,NAMES,NR)
	IF (IAND(IS,1).EQ.0) THEN
		IS = PPD_IOINV
		GOTO 999
	ENDIF
C
	CODE = NAMES(NR)
	LC = STR_SIGLEN (CODE)
	IF (LC.LT.STR_SIGLEN(NAMES(NR))) THEN
		IS = PPD_STRTOOSML
		GOTO 999
	ENDIF
C
	PPD_IOCD_GET = PPD_SUCCESS
	RETURN
C
 999	PPD_IOCD_GET = MSG_SET (IS,0)
	RETURN
	END
