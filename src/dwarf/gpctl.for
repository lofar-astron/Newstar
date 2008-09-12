C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	GP_CTL
C.Keywords:	Program Parameters, Get Value, Control
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		Common fields used:
C	INTEGER*4	PARM$KEYPART(*)	! (m) parameter-specific fields
C	CHARACTER*16	PARM$KEY	! (m) user's parameter name
C	INTEGER*4	PARM$LK		! (m) significant length of the keyword
C
C.Version:	900416 FMO - recreation
C.Version:	920214 GvD - no optional arguments in MSG anymore
C.Version:	940120 CMV - use WNGGVM i.s.o. GEN_GET_VM, indirect addressing
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GP_CTL ()
C	          ENTRY    GP_CTL_OPEN (KEYWORD)
C	          ENTRY    GP_CTL_CLOSE ()
C	          ENTRY    GP_CTL_END ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INCLUDE 'PARM_6_DEF'
C
	INTEGER*4	GP_CTL_OPEN, GP_CTL_CLOSE, GP_CTL_END
C
	CHARACTER*(*)	KEYWORD		! (i) program's name of the parameter
C
C.Purpose:	Open, close or end the GET_PARM control
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	error	2		too many active program parameters (OPEN)
C	fatal	DWC_UNKPRKEYW	invalid keyword (OPEN)
C	false status codes returned by references routines
C.Notes:
C	- OPEN loads the parameter descriptions in common blocks
C	- CLOSE saves the parameter-value administration in virtual memory
C	- END defines external defaults for all parameters for which values
C	  were saved via /SAVE requests.
C-------------------------------------------------------------------------
C
	CHARACTER*(*)	BLANK
	INTEGER*4	MAXNR			! max nr of used parameters
		PARAMETER (BLANK = ' ')
		PARAMETER (MAXNR = 100)
	CHARACTER*16	PNAM_LIST(MAXNR)	! list of used param names
	INTEGER*4	PADR_LIST(MAXNR)	! list of VM-block addresses
	INTEGER*4	PCOUNT			! nr of used parameters
	INTEGER*4	ACTIVE_NR		! nr of currently active param
		DATA PNAM_LIST /MAXNR*' '/
		DATA PADR_LIST /MAXNR*0/
		DATA PCOUNT /0/
		DATA ACTIVE_NR /0/
		SAVE PNAM_LIST, PADR_LIST, PCOUNT, ACTIVE_NR
C
	INTEGER*4	GP_INI_FILL, GP_DEF_CLEAR, GP_VAL_CLEAR, GP_SAV_INIT
	INTEGER*4	GP_LOOP_INIT, GP_ASK_INIT, GP_SAV_DEFINE
	INTEGER*4	DWC_PROG_GET, DWC_STREAM_GET, DWC_SYM_BUILD
	INTEGER*4	PPD_READ_P, PPD_IOCD_GET, PPD_UNAM_GET
	INTEGER*4	STR_UPCASE, STR_MATCH_A
	INTEGER*4	MOVE_BLB
	LOGICAL		WNGGVM
	INTEGER		MSG_SET
C
	CHARACTER	SYMBOL*40, PROGNAM*16, STREAM*16, UPCKEY*16, IOCD*6
	INTEGER*4	IS, LSYM, LP, LS, LIO, NR, LKMIN, ADDRESS, ASKSW(2)
	LOGICAL*4	PROTO
C
	GP_CTL = DWC_SUCCESS
	RETURN
C
C	=================
	ENTRY GP_CTL_OPEN (KEYWORD)
C	=================
C
C					Read the parameter description
C					from the PPD file into common
C					- convert the name to uppercase
C					- accept input-type parameters only
C
	UPCKEY = KEYWORD
	IS = STR_UPCASE (UPCKEY)
	IF (IAND(IS,1).NE.0) IS = PPD_READ_P (UPCKEY)
	IF (IAND(IS,1).NE.0) IS = PPD_IOCD_GET (IOCD,LIO)
	IF (IAND(IS,1).EQ.0) GOTO 999				! PPD access error
	IF (INDEX('IM',IOCD(1:1)).EQ.0) GOTO 991	! no input-type
C
C					If the parameter has already been used:
C					- copy its control block into common
C					- set the new active-number
C
	IS = STR_MATCH_A (UPCKEY,PCOUNT,PNAM_LIST,NR)
	IF (IS.EQ.1) THEN				! full match
		IS = MOVE_BLB (A_B(PADR_LIST(NR)-A_OB),
	1		PARM$KEYPART,PARM__LENKP)
		IF (IAND(IS,1).EQ.0) GOTO 999
		ACTIVE_NR = NR
C
C					Otherwise:
C					- initialize its control common
C					- save it in a virtual memory block
C					- add the parameter to the used-list
C					- set the new active-number
C
	ELSE
		IF (PCOUNT.GE.MAXNR) GOTO 992
		IS = DWC_PROG_GET (PROGNAM,LP)
		IF (IAND(IS,1).NE.0) IS = DWC_STREAM_GET (STREAM,LS,.FALSE.)
		IF (IAND(IS,1).NE.0)
	1		IS = PPD_UNAM_GET (PARM$KEY,PARM$LK,LKMIN,PROTO)
		IF (IAND(IS,1).NE.0) IS = DWC_SYM_BUILD
	1		(PROGNAM(:LP),STREAM(:LS),PARM$KEY(:PARM$LK),SYMBOL,LSYM)
		IF (IAND(IS,1).NE.0) IS = GP_LOOP_INIT ()
		IF (IAND(IS,1).NE.0) IS = GP_INI_FILL (SYMBOL(:LSYM),ASKSW)
		IF (IAND(IS,1).NE.0) IS = GP_ASK_INIT (ASKSW)
		IF (IAND(IS,1).NE.0) IS = GP_DEF_CLEAR ()
		IF (IAND(IS,1).NE.0) IS = GP_VAL_CLEAR ()
		IF (IAND(IS,1).NE.0) IS = GP_SAV_INIT ()
		IF (IAND(IS,1).EQ.0) GOTO 999
C
		IF (.NOT.WNGGVM(PARM__LENKP,ADDRESS)) THEN
			IS = MSG_SET (DWC_PPDNOVIRT,1)
			CALL WNCTXT(DWLOG,DWMSG,PARM__LENKP)
			GOTO 999
		ENDIF
		IS = MOVE_BLB (PARM$KEYPART,A_B(ADDRESS-A_OB),PARM__LENKP)
		IF (IAND(IS,1).EQ.0) GOTO 999
C
		PCOUNT = PCOUNT+1
		PNAM_LIST(PCOUNT) = UPCKEY
		PADR_LIST(PCOUNT) = ADDRESS
		ACTIVE_NR = PCOUNT
	ENDIF
C
	GP_CTL_OPEN = DWC_SUCCESS
	RETURN
C
 991	GP_CTL_OPEN = MSG_SET (DWC_UNKPRKEYW,1)
	CALL WNCTXT(DWLOG,DWMSG,UPCKEY,' input-',BLANK)
	RETURN
 992	GP_CTL_OPEN = 2
	CALL WNCTXT(DWLOG,'Too many active program parameters')
	RETURN
 999	GP_CTL_OPEN = IS
	RETURN
C
C	==================
	ENTRY GP_CTL_CLOSE ()
C	==================
C
C					Write the common back to VM block
C
	IF (ACTIVE_NR.GT.0) 
	1	IS = MOVE_BLB (PARM$KEYPART,
	1		A_B(PADR_LIST(ACTIVE_NR)-A_OB),PARM__LENKP)
C
	ACTIVE_NR = 0
	GP_CTL_CLOSE = DWC_SUCCESS
	RETURN
C
C	================
	ENTRY GP_CTL_END ()
C	================
C
C					Define all SAVE symbols
C
	IS = DWC_PROG_GET (PROGNAM,LP)			! ignore false returns
	IS = DWC_STREAM_GET (STREAM,LS,.FALSE.)
C
	DO NR = 1,PCOUNT
		IS = MOVE_BLB (A_B(PADR_LIST(NR)-A_OB),
	1		PARM$KEYPART,PARM__LENKP)
		IF (IAND(IS,1).NE.0) IS = DWC_SYM_BUILD
	1		(PROGNAM(:LP),STREAM(:LS),PARM$KEY(:PARM$LK),SYMBOL,LSYM)
		IF (IAND(IS,1).NE.0)
	1		IS = GP_SAV_DEFINE (SYMBOL(:LSYM),PNAM_LIST(NR))
	ENDDO
C
	GP_CTL_END = DWC_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GP_CTL_RESET (KEYWORD)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	KEYWORD		! (i) parameter name
C
C.Purpose:	Reset the GET_PARM control for a program parameter
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	false status codes returned by referenced routines
C.Notes:
C	- The control will be reset when the user answered /ASK=<keyword>
C	  on a DWARF prompt.
C	- The next call of GET_PARM for that parameter will then result in
C	  a question on the terminal.
C-------------------------------------------------------------------------
C
	INTEGER*4	GP_CTL_OPEN, GP_CTL_CLOSE
	INTEGER*4	GP_ASK_SET, GP_VAL_RELEASE, GP_LOOP_RESET
	INTEGER*4	PPD_PNAM_GET, PPD_READ_U
C
	CHARACTER*16	SAVPKEY, PKEY
	INTEGER*4	IS, LSAV, LPK
C
C
C					Close the current parameter control
C
	IS = PPD_PNAM_GET (SAVPKEY,LSAV)
	IF (IAND(IS,1).NE.0) IS = GP_CTL_CLOSE ()
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Find the wanted parameter and
C					open its control
C
	IS = PPD_READ_U (KEYWORD)
	IF (IAND(IS,1).NE.0) IS = PPD_PNAM_GET (PKEY,LPK)
	IF (IAND(IS,1).NE.0) IS = GP_CTL_OPEN (PKEY)
	IF (IAND(IS,1).EQ.0) GOTO 998	
C
C					Reset the relevant control fields
C	
	IS = GP_VAL_RELEASE ()				! clear value array
	IF (IAND(IS,1).NE.0) IS = GP_ASK_SET (1,2)			! user said ASK
	IF (IAND(IS,1).NE.0) IS = GP_LOOP_RESET ()			! reset loop control
	IF (IAND(IS,1).EQ.0) GOTO 998
C
C					Close the parameter control
C					and restore the original one
C
	IS = GP_CTL_CLOSE ()
 998	IS = GP_CTL_OPEN (SAVPKEY)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	GP_CTL_RESET = DWC_SUCCESS
	RETURN
C
 999	GP_CTL_RESET = IS
	RETURN
	END
