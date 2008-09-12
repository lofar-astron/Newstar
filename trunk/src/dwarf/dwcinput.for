C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	DWC_INPUT
C.Keywords:	Dwarf Control, Get Input
C.Author:	Ger van Diepen (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	900407 FMO - recreation
C.Version:	920206 GvD - no optional arguments anymore
C.Version:	930716 CMV - bell in advance for script mode
C.Version:	010709 AXC -`nchar*1 and tmpchar in calls
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_INPUT (LINE,PROMPT,LL,DEVCOD,BELLSW)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	LINE		! (o) answer
	CHARACTER*(*)	PROMPT		! (i) prompt
	INTEGER*4	LL		! (o) length of answer (optional)
	INTEGER*4	DEVCOD		! (i) input-device code
C				0 (default) SYS$INPUT
C				1	SYS$COMMAND
C				2	SYS$COMMAND (from subprocess)
	INTEGER*4	BELLSW		! (i) bell switch
C				0 (default) no bell signal
C				1	bell signal if DWARF's bell is enabled
C				2	always bell
C
C.Purpose:	Ask the user for input
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	info	DWC_GETINPTR	answer truncated
C	warning	DWC_EOFCTRLZ	end of file (CTRL/Z or '#' given)
C	fatal	DWC_GETINPERR	I/O error
C.Notes:
C	- The prompt string will be completed with ': '. In batch mode no
C	  prompts will be printed.
C	- The answer may be composed of several lines: if an input line ends
C	  with a hyphen, the user will be prompted for a continuation line, and
C	  that line will be appended to the answer (without hyphen).
C	- The answer will be translated as follows: lowercase characters are
C	  converted into uppercase, tabs into blanks, multiple blanks into a
C	  single blank, a leading blank and trailing comments (starting
C	  with '!') are removed.
C	- If the answer is too long, it will be truncated and an informational
C	  message will be left in the message buffer.
C	- Usually, SYS$INPUT and SYS$COMMAND will be the same device (both
C	  terminal or batch-job). Only in the case of indirect command files,
C	  SYS$INPUT will be the command file and SYS$COMMAND the terminal.
C-------------------------------------------------------------------------
C
C
	INTEGER*4	MAXDEV
	CHARACTER*1	BLANK, COLON, PRCONT, BELL
		PARAMETER (MAXDEV = 2)
		PARAMETER (BLANK  = ' ')
		PARAMETER (COLON  = ':')
		PARAMETER (PRCONT = '_')
C
	INTEGER*4	DWC_BELL_INQ, DWC_STR_STANDARD
	INTEGER*4	GEN_INPUT,STR_SIGLEN
	INTEGER*4	MSG_SET
C
	CHARACTER	PRADD*2, WORK*255, TMP*256
	INTEGER*4	LP, LLSAV, LLADD, XDEVCOD
	INTEGER*4	IS
C
	BELL   = CHAR(7)
C
C					Check device code and bell switch
C
	XDEVCOD = DEVCOD
	IF (DEVCOD.LT.0 .OR. DEVCOD.GT.MAXDEV) XDEVCOD = 0
	PRADD = COLON//BLANK
C
C					Get first input line
C
	I=STR_SIGLEN(PROMPT)
	IF (BELLSW.EQ.2 .OR.
	1   (BELLSW.EQ.1 .AND. IAND(DWC_BELL_INQ(),1).NE.0)) THEN
	    IF (PROMPT.NE.BLANK) THEN
	       TMP=BELL//PROMPT(:I)//PRADD
	       I=STR_SIGLEN(TMP)+1
	       IS = GEN_INPUT (LINE,TMP(:I),XDEVCOD)
	    ELSE
	       TMP=BELL//PRADD
	       I=STR_SIGLEN(TMP)+1
	       IS = GEN_INPUT (LINE,TMP(:I),XDEVCOD)
	    ENDIF
	ELSE
	    IF (PROMPT.NE.BLANK) THEN
	        TMP=PROMPT(:I)//PRADD
	        I=STR_SIGLEN(TMP)+1
		IS = GEN_INPUT (LINE,TMP(:I),XDEVCOD)
	    ELSE
		IS = GEN_INPUT (LINE,PRADD,XDEVCOD)
	    ENDIF
	ENDIF
C
	IF (IS.NE.DWC_SUCCESS) GOTO 999
	IS = DWC_STR_STANDARD (LINE,WORK,LL)
	IF (IAND(IS,1).EQ.0) THEN
		CALL WNCTXT(DWLOG,
	1	'WORK string in DWC_INPUT too short; tell DWARF manager')
		CALL WNGEX
	END IF
	LINE = WORK(:LL)
C
C					Get possible continuation lines
C					- LLSAV is used to make sure that
C					  only 1 continuation line will be
C					  asked when an answer ends with ------
C
	LLSAV = LL
	DO WHILE (LINE(LL:LL).EQ.'-' .AND. LL.GE.LLSAV)
		LLSAV = LL
		LINE(LL:LL) = BLANK
		IS = GEN_INPUT (LINE(LL:),PRCONT//PRADD(:LP),XDEVCOD)
		IF (IS.NE.DWC_SUCCESS) GOTO 999
		IS = DWC_STR_STANDARD (LINE(LL:),WORK,LLADD)
		IF (IAND(IS,1).EQ.0) THEN
			CALL WNCTXT(DWLOG,
	1	     'WORK string in DWC_INPUT too short; tell DWARF manager')
			CALL WNGEX
		END IF
		LINE(LL:) = WORK(:LLADD)
		LL = LLSAV-1+LLADD
	ENDDO
C
C					Remove possible leading blank
C
	IF (LL.GT.0 .AND. LINE(1:1).EQ.BLANK) THEN
		LINE = LINE(2:)
		LL = LL-1
	ENDIF
C
C					Return
C
 999	IF (IS.EQ.DWC_GETINPTR) THEN
		LL = LEN(LINE)
		DWC_INPUT = MSG_SET (DWC_GETINPTR,1)
		CALL WNCTXT(DWLOG,DWMSG,LL)
	ELSE		
		DWC_INPUT = IS
	ENDIF
	RETURN
	END
