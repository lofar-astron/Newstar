C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	DWC_CTL
C.Keywords:	DWARF, Program, Control
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		Common fields used:
C	INTEGER*4	DWARF_(*)		! (m) all fields
C
C.Version:	900416 FMO - recreation
C.Version:	920214 GvD - no optional arguments in MSG anymore
C.Version:	940120 CMV - use indirect addressing (A_B)
C		940209 WNB - compiler problems
C.Version:	010709 AXC - ichar
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_CTL ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Make source module name known
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS	always
C.Notes:	Dummy routine
C-------------------------------------------------------------------------
C
	DWC_CTL = DWC_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_CTL_OPEN ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INCLUDE 'DWARF_4_DEF'
C
C
C.Purpose:	Get DWARF control parameters in common
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	error	DWC_UNKDWCOM	symbol not defined (no message stored)
C	false status code returned by referenced routines
C.Notes:
C	- The parameters are kept in the symbol DWARF_CONTROL_COMMON. The value
C	  of that symbol will overlay DWARF's control common.
C	- If the symbol doesn't exist, the common contains its initial values.
C	- Only SPECIFY can create the control symbol.
C-------------------------------------------------------------------------
C
	INTEGER*4	SYMBOL_GET, MOVE_BLB
C
	CHARACTER	WORK*255,DCOM*40
	INTEGER*4	IS, LW
C
C
	DCOM='DWARF_CONTROL_COMMON'
	IS = SYMBOL_GET (DCOM,WORK,LW)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (LW.EQ.0) GOTO 998
C
	IS = MOVE_BLB (%REF(WORK),DWARF_,LW)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	DWC_CTL_OPEN = DWC_SUCCESS
	RETURN
 998	DWC_CTL_OPEN = DWC_UNKDWCOM
	RETURN
 999	DWC_CTL_OPEN = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_CTL_SAVE ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INCLUDE 'DWARF_4_DEF'
C
C
C.Purpose:	Save DWARF control parameters
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	false status code returned by referenced routines
C.Notes:
C	- The parameters are saved in the symbol DWARF_CONTROL_COMMON.
C	  The value of that symbol will overlay DWARF's control common.
C	- Only SPECIFY can create the control symbol.
C-------------------------------------------------------------------------
C
	INTEGER*4	SYMBOL_DEFINE, MOVE_BLB
C
	CHARACTER	WORK*255
	INTEGER*4	IS
C
C
	IS = MOVE_BLB (DWARF_,%REF(WORK),DWARF__LENGTH*4)
	IF (IAND(IS,1).NE.0) IS = SYMBOL_DEFINE ('DWARF_CONTROL_COMMON',
	1				WORK(:DWARF__LENGTH*4),DWC__GLOBALSYM)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	DWC_CTL_SAVE = DWC_SUCCESS
	RETURN
 999	DWC_CTL_SAVE = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_CTL_UPDATE (PROG)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	PROG		! (i) program name
C
C.Purpose:	Update the program control parameters
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	false status code returned by referenced routines
C.Notes:
C	- The updates (if any) are kept in the symbol DWARF_<prog>_CONTROL
C	  that was defined by EXECUTE.EXE.
C	- Most program control parameters are in DWARF's control common,
C	  but some are still in PARM's control common. The relevant fields will
C	  be changed via the relevant routines.
C-------------------------------------------------------------------------
C
	INTEGER*4	DWC_PROG_PUT, DWC_PROG_GET, DWC_STREAM_PUT
	INTEGER*4	DWC_ASK_PUT, DWC_SAVE_PUT, DWC_TEST_PUT
	INTEGER*4	DWC_PRCMODE_SET, DWC_SYSIN_SET, DWC_SYSOUT_SET
	INTEGER*4	SYMBOL_GET, GEN_FORIOS
C
	CHARACTER	PROGNAM*16, WORK*255, INFILE*80, DCOM*40
	INTEGER*4	IS, LP, LW, LI, LS, PTR
	INTEGER*4	ASKSW, SAVESW, TESTSW
C
C
C					Check and store the program name
C
	IS = DWC_PROG_PUT (PROG)
	IF (IAND(IS,1).NE.0) IS = DWC_PROG_GET (PROGNAM,LP)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Get and store the updates
C
	DCOM='DWARF_'//PROGNAM(:LP)//'_CONTROL'
	IS = SYMBOL_GET (DCOM,WORK,LW)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (LW.EQ.0) THEN
		LI = 0
	ELSE
		READ (WORK(:5),'(3I1,I2)',ERR=998) ASKSW,SAVESW,TESTSW,LS
		PTR = 6
		IF (ASKSW.NE.0) IS = DWC_ASK_PUT (ASKSW.EQ.2)
		IF (IAND(IS,1).NE.0 .AND. SAVESW.NE.0)
	1			IS = DWC_SAVE_PUT (SAVESW.EQ.2)
		IF (IAND(IS,1).NE.0 .AND. TESTSW.NE.0)
	1			IS = DWC_TEST_PUT (TESTSW.EQ.2)
		IF (IAND(IS,1).NE.0 .AND. LS.GT.0)
	1			IS = DWC_STREAM_PUT (WORK(6:PTR+LS-1))
		IF (IAND(IS,1).EQ.0) GOTO 999
		PTR = PTR+LS
		READ (WORK(PTR:PTR+1),'(I2)',ERR=998) LI
		PTR = PTR+2
		IF (LI.GT.0) INFILE = WORK(PTR:PTR+LI-1)
	ENDIF
C
C					Set process mode and standard
C					input and output devices
C					(in this order !!!)
C
	IF (IAND(IS,1).NE.0) IS = DWC_PRCMODE_SET ()			! process mode
	IF (IAND(IS,1).NE.0) IS = DWC_SYSIN_SET (INFILE,LI)		! standard input
	IF (IAND(IS,1).NE.0) IS = DWC_SYSOUT_SET ()			! standard output
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C
	DWC_CTL_UPDATE = DWC_SUCCESS
	RETURN
 998	DWC_CTL_UPDATE = GEN_FORIOS ('work_string')
	RETURN
 999	DWC_CTL_UPDATE = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_CTL_FILL ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Fill the DWARF common and define the symbol DWARF_CONTROL_COMMON
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	fatal	DWC_DWSERROR	error while interpreting DWARF symbols
C.Notes:
C	The value of the symbol will be the string equivalent of the common
C	array DWARF_.
C-------------------------------------------------------------------------
C
	CHARACTER*(*)	PROGNAM, STREAM, BLANK
		PARAMETER (PROGNAM  = 'DWARF')
		PARAMETER (STREAM   = '$0'   )
		PARAMETER (BLANK    = ' '    )
C
	INTEGER*4	PPD_INIT, PPD_EXIT, PPD_READ_U, PPD_READ_UNXT
	INTEGER*4	PPD_PNAM_GET
	INTEGER*4	CLI_RESET, CLI_PARSE, CLI_GET
	INTEGER*4	DWC_CTL_FILL_S, DWC_CTL_SAVE
	INTEGER*4	DWC_SYM_BUILD, DWC_STR_SUBST, DWC_HELP
	INTEGER*4	PV_DEF_GET
	INTEGER*4	PV_BLK_ALLOC, PV_BLK_DECODE, PV_BLK_RELEASE
	INTEGER		MSG_SET
C
	INTEGER*4	NRARG
		PARAMETER (NRARG = 1)
	CHARACTER*10	NAME(NRARG)
	INTEGER*4	ATTR(NRARG)
	CHARACTER*1	PROMPT(NRARG)
	CHARACTER*1	DEFVAL(NRARG)
		DATA NAME   /'VALSTR'       /
		DATA ATTR   /CLI__EXPRESSION/
		DATA PROMPT /' '            /
		DATA DEFVAL /' '            /
C
	CHARACTER	VALSTR*255, WORK*255, KEY*16, SYMBOL*38, TYPE*1
	INTEGER*4	LVAL, LW, LK, LS, LT
	INTEGER*4	IS, ERRPTR, DLEVEL, TMP, VALBLK(8)
	LOGICAL*4	SWSYM
	BYTE		DEFARR(1)				! dummy
C
C
C					Open the PPD file and loop
C					through all DWARF parameters
C
	IS = PPD_INIT (PROGNAM)
	IF (IAND(IS,1).NE.0) IS = PPD_READ_U (BLANK)
	IF (IAND(IS,1).EQ.0) GOTO 999
	DO WHILE (IAND(IS,7).EQ.1)
C
C					Get program's parameter name
C					and its default value
C
		IS = PPD_PNAM_GET (KEY,LK)
		IF (IAND(IS,1).NE.0)
	1	    IS = DWC_SYM_BUILD (PROGNAM,STREAM,KEY(:LK),SYMBOL,LS)
		IF (IAND(IS,1).NE.0)
	1	    IS = PV_DEF_GET (SYMBOL(:LS),VALSTR,LVAL,TYPE,LT)
		IF (IAND(IS,1).EQ.0) GOTO 999
		IF (LVAL.EQ.0) GOTO 991
C
C					Substitute symbols
C
		SWSYM = .FALSE.
		IS = DWC_STR_SUBST (VALSTR(:LVAL),WORK,LW,STREAM,ERRPTR,
	1							.FALSE.,SWSYM)
		IF (IAND(IS,1).EQ.0) GOTO 992
C
C					If help request (not allowed): return
C
		IS = DWC_HELP (WORK(:LW),-1,DLEVEL)
		IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Analyse the string
C					- reset the Command-Line Interpreter
C					- parse the string
C					- extract the pure value string
C
		IS = CLI_RESET (NRARG,NAME,ATTR,PROMPT,DEFVAL)
		IF (IAND(IS,1).NE.0) IS = CLI_PARSE (WORK(:LW))
		IF (IAND(IS,1).NE.0) IS = CLI_GET ('VALSTR',VALSTR,LVAL)
		IF (IAND(IS,1).EQ.0) GOTO 999
		IF (LVAL.EQ.0) GOTO 991
C
C					Check the value string
C					- allocate memory for the value block
C					- convert value string to value block
C
		IS = PV_BLK_ALLOC (VALSTR(:LVAL),VALBLK)
		IF (IAND(IS,1).EQ.0) GOTO 999
C
		IS = PV_BLK_DECODE (VALSTR(:LVAL),VALBLK,STREAM,
	1					.FALSE.,SWSYM,.TRUE.,DEFARR,0)
C
C					Put the values in the DWARF common
C					and release virtual memory
C
		IF (IAND(IS,1).NE.0) IS = DWC_CTL_FILL_S (KEY(:LK),VALBLK(7),
	1					A_B(VALBLK(2)-A_OB),
	1					A_B(VALBLK(3)-A_OB))
		TMP = PV_BLK_RELEASE (VALBLK)
		IF (IAND(IS,1).NE.0) IS = PPD_READ_UNXT ()
		IF (IAND(IS,1).EQ.0) GOTO 999
	ENDDO
C
C					Define the control symbol
C
	IS = DWC_CTL_SAVE ()
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C
	IS = PPD_EXIT ()
	DWC_CTL_FILL = DWC_SUCCESS
	RETURN
 991	IS = MSG_SET (DWC_NODWVALUE,1)
	CALL WNCTXT(DWLOG,DWMSG,SYMBOL(:LS))
	GOTO 999
 992	IS = MSG_SET (DWC_EXPERRMSG,1)
	CALL WNCTXT(DWLOG,DWMSG,'keyword '//KEY(:LK),ERRPTR,WORK(:LW))
 999	DWC_CTL_FILL = MSG_SET(DWC_DWSERROR,0)
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_CTL_FILL_S (KEYWORD,LVAL,NRVAL,ARR)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	KEYWORD		! (i) program keyword
	INTEGER*4	LVAL		! (i) length of value (in bytes)
	INTEGER*4	NRVAL		! (i) number of values in value array
	BYTE		ARR(*)		! (i) value array
C
C.Purpose:	Put the value of a DWARF parameter into the DWARF common
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	fatal	DWC_UNKDWKEY	unknown DWARF keyword
C.Notes:
C-------------------------------------------------------------------------
C
	INTEGER*4	DWC_ASK_PUT, DWC_BELL_PUT, DWC_EXTENDSZ_PUT
	INTEGER*4	DWC_IBMODE_PUT, DWC_IDENT_PUT, DWC_IOBFSZ_PUT
	INTEGER*4	DWC_LEVEL_PUT, DWC_LOGFATAL_PUT, DWC_LOGLEVEL_PUT
	INTEGER*4	DWC_MSGDEV_PUT, DWC_NODE_PUT, DWC_SAVE_PUT
	INTEGER*4	DWC_STREAM_PUT, DWC_TEST_PUT
	INTEGER		MSG_SET, MOVE_BLB
C
	CHARACTER*255	WORK
	INTEGER*4	IS
C
C
C					Stream name
C
	IF (KEYWORD.EQ.'STREAM') THEN
		IS = MOVE_BLB (ARR,%REF(WORK),LVAL)
		IF (IAND(IS,1).NE.0) IS = DWC_STREAM_PUT (WORK(:LVAL))
C
C					Userlevel
C
	ELSE IF (KEYWORD.EQ.'USERLEVEL') THEN
		IF (ARR(1).EQ.ICHAR('E')) THEN			! expert
			IS = DWC_LEVEL_PUT (0)
		ELSE IF (ARR(1).EQ.ICHAR('A')) THEN		! average
			IS = DWC_LEVEL_PUT (1)
		ELSE
			IS = DWC_LEVEL_PUT (2)		! beginner (default)
		ENDIF
C
C					Message device(s)
C
	ELSE IF (KEYWORD.EQ.'MESSAGEDEVICE') THEN
		IF (ARR(1).EQ.ICHAR('P')) THEN			! printer
			IS = DWC_MSGDEV_PUT (1)
		ELSE IF (NRVAL.LE.1) THEN		! terminal (default)
			IS = DWC_MSGDEV_PUT (0)
		ELSE
			IS = DWC_MSGDEV_PUT (2)		! both
		ENDIF
C
C					Terminal bell
C
	ELSE IF (KEYWORD.EQ.'BELL') THEN
		IS = DWC_BELL_PUT (ARR(2).EQ.ICHAR('N'))	! on or off (default)
 
C
C					Buffer size for bulk-IO
C
	ELSE IF (KEYWORD.EQ.'IOBUFSIZE') THEN
		IS = DWC_IOBFSZ_PUT (ARR)
C
C					Minimum extendsize for disk files
C
	ELSE IF (KEYWORD.EQ.'EXTENDSIZE') THEN
		IS = DWC_EXTENDSZ_PUT (ARR)
C
C					Current node name
C
	ELSE IF (KEYWORD.EQ.'CURNODE') THEN
		IS = MOVE_BLB (ARR,%REF(WORK),LVAL)
		IF (IAND(IS,1).NE.0) IS = DWC_NODE_PUT (WORK(:LVAL))
C
C					Ask switch
C
	ELSE IF (KEYWORD.EQ.'ASK') THEN
		IS = DWC_ASK_PUT (ARR(1).EQ.ICHAR('Y'))	! on or off (default)
C
C					Savelast switch
C
	ELSE IF (KEYWORD.EQ.'SAVELAST') THEN
		IS = DWC_SAVE_PUT (ARR(1).EQ.ICHAR('Y'))	! on or off (default)
C
C					Test switch
C
	ELSE IF (KEYWORD.EQ.'TEST') THEN
		IS = DWC_TEST_PUT (ARR(1).EQ.ICHAR('Y'))	! on or off (default)
C
C					Loglevel
C
	ELSE IF (KEYWORD.EQ.'LOGLEVEL') THEN
		IS = DWC_LOGLEVEL_PUT (ARR)
C
C					Log switch for fatal runs
C
	ELSE IF (KEYWORD.EQ.'LOGFATAL') THEN
		IS = DWC_LOGFATAL_PUT (ARR(1).EQ.ICHAR('Y'))	! on or off (default)
C
C					Login identification string
C
	ELSE IF (KEYWORD.EQ.'IDENT') THEN
		IS = MOVE_BLB (ARR,%REF(WORK),LVAL)
		IF (IAND(IS,1).NE.0) IS = DWC_IDENT_PUT (WORK(:LVAL))
C
C					Mode
C
	ELSE IF (KEYWORD.EQ.'IBMODE') THEN
		IF (ARR(1).EQ.ICHAR('B')) THEN
			IS = DWC_IBMODE_PUT (1)		! batch
		ELSE
			IS = DWC_IBMODE_PUT (0)		! interactive (default)
		ENDIF
C
C					Unknown DWARF parameter
C
	ELSE
		IS = MSG_SET (DWC_UNKDWKEY,1)
		CALL WNCTXT(DWLOG,DWMSG,KEYWORD)
	ENDIF
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C
	DWC_CTL_FILL_S = DWC_SUCCESS
	RETURN
 999	DWC_CTL_FILL_S = IS
	RETURN
	END
