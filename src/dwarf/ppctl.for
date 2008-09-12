C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	PP_CTL
C.Keywords:	Program Parameters, Store Value, Control
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	900416 FMO - recreation
C.Version:	910910 FMO - add missing argument in STR_COPY_U call
C.Version:	920214 GvD - no optional arguments in MSG anymore
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION  PP_CTL_OPEN (PKEY,PROGSTRM,SYMBOL,
	1						LSYM,FOREIGN)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	PKEY		! (i) program's parameter name
	CHARACTER*(*)	PROGSTRM	! (i) target program and stream names
	CHARACTER*(*)	SYMBOL		! (o) name of symbol to be defined
	INTEGER*4	LSYM		! (o) its significant length
	LOGICAL*4	FOREIGN		! (o) put for foreign program ?
C
C.Purpose:	Set up for PUT_PARM
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCES
C	fatal	DWC_UNKPRKEYW	unknown output-type program parameter
C	false status codes returned by referenced routines
C.Notes:
C-------------------------------------------------------------------------
C
C
	INTEGER*4	DWC_SYM_SPLIT, DWC_SYM_BUILD
	INTEGER*4	DWC_PROG_GET, DWC_PROG_CHECK
	INTEGER*4	DWC_STREAM_GET, DWC_STREAM_CHECK
	INTEGER*4	PPD_SAVE, PPD_INIT, PPD_READ_P
	INTEGER*4	PPD_IOCD_GET, PPD_UNAM_GET
	INTEGER*4	MSG_SET  , STR_SIGLEN
C
	CHARACTER	CURPROG*16, PROGNAM*16, STREAM*16, XSTREAM*16
	CHARACTER	KEY*16, IOCD*6
	INTEGER*4	IS, LCP, LP, LS, LK, LKMIN, LIOCD
	LOGICAL*4	PROTO, IS_GLOBAL
C
C
	FOREIGN = .FALSE.
	IS = DWC_PROG_GET (CURPROG,LCP)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Isolate and check the names of
C					the target program and stream
C					- use the current names as defaults
C
	IS = DWC_SYM_SPLIT (PROGSTRM,PROGNAM,LP,STREAM,LS,KEY,LK)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (LP.EQ.0) PROGNAM = CURPROG
	IS = DWC_PROG_CHECK (PROGNAM,LP,IS_GLOBAL)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (LS.EQ.0) THEN
		IS = DWC_STREAM_GET (STREAM,LS,IS_GLOBAL)
	ELSE
		IS = DWC_STREAM_CHECK (STREAM,XSTREAM,LS,IS_GLOBAL)
	ENDIF
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					If target is not the current program:
C					- save the current PPD-status block
C					- set the flag for later PPD restore
C					- open the PPD file of the target
C			
	IF (PROGNAM(:LP).NE.CURPROG(:LCP)) THEN
		IS = PPD_SAVE ()
		IF (IAND(IS,1).EQ.0) GOTO 999
		FOREIGN = .TRUE.
		IS = PPD_INIT (PROGNAM(:LP))
		IF (IAND(IS,1).EQ.0) GOTO 999
	ENDIF
C
C					Read the parameter definition (PPD)
C					- check the IO code (output or modify)
C					- get the user's parameter name
C					- build the full symbol name
C
	LK = STR_SIGLEN (PKEY)
	IS = PPD_READ_P (PKEY(:LK))
	IF (IAND(IS,1).NE.0) IS = PPD_IOCD_GET (IOCD,LIOCD)
	IF (INDEX('OM',IOCD(1:1)).EQ.0) GOTO 992
C
	IS = PPD_UNAM_GET (KEY,LK,LKMIN,PROTO)
	IF (IAND(IS,1).NE.0) IS = DWC_SYM_BUILD
	1		(PROGNAM(:LP),STREAM(:LS),KEY(:LK),SYMBOL,LSYM)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C
	PP_CTL_OPEN = DWC_SUCCESS
	RETURN
C
 992	PP_CTL_OPEN = MSG_SET (DWC_UNKPRKEYW,1)
	CALL WNCTXT(DWLOG,DWMSG,PKEY(:LK),
	1					' output-',PROGNAM)
	RETURN
 999	PP_CTL_OPEN = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION  PP_CTL_CLOSE (PROGSTRM,FOREIGN)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	PROGSTRM	! (i) target program and stream names
	LOGICAL*4	FOREIGN		! (i) put for foreign program ?
C
C.Purpose:	Close the PUT_PARM operations
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCES
C	false status codes returned by referenced routines
C.Notes:
C-------------------------------------------------------------------------
C
C
	INTEGER*4	DWC_CTL_FILL
	INTEGER*4	PPD_EXIT, PPD_RESTORE
	INTEGER*4	STR_COPY_U
C
	CHARACTER	PROGNAM*9
	INTEGER*4	LP, IS, PTR
C
C
C					If a "foreign" program was used:
C					- close its PPD file
C					- if the program is DWARF: redefine
C					  the symbol DWARF_CONTROL_COMMON
C					- restore the PPD status block of the
C					  current program
C
	IF (FOREIGN) THEN
		IS = PPD_EXIT ()
		IF (IAND(IS,1).EQ.0) GOTO 991
		LP = 0
		PTR = 1
		IS = STR_COPY_U ('$',PROGSTRM,PTR,PROGNAM,LP)
		IF (PROGNAM(:LP).EQ.'DWARF') THEN
			IS = DWC_CTL_FILL ()
			IF (IAND(IS,1).EQ.0) GOTO 991
		ENDIF
		IS = PPD_RESTORE ()
		IF (IAND(IS,1).EQ.0) GOTO 992
	ENDIF
C
C
	PP_CTL_CLOSE = DWC_SUCCESS
	RETURN
C
 991	PP_CTL_CLOSE = IS
	IS = PPD_RESTORE ()
 992	IF (IAND(IS,1).EQ.0) PP_CTL_CLOSE = IS
	RETURN
	END
