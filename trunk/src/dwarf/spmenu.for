C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	SP_MENU
C.Keywords:	Program Parameters, External Defaults, Specify
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	900416 FMO - recreation
C.Version:	901217 FMO - write messages on retry (label 300)
C.Version:	920206 GvD - add former optional arguments to DWC_INPUT
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION SP_MENU (PROGNAM,STREAM,DO_LOCAL,DO_SUBST,
	1							DO_EXTERN)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	PROGNAM		! (i) program name
	CHARACTER*(*)	STREAM		! (i) stream name
	LOGICAL*4	DO_LOCAL	! (i) only prompt for local defaults ?
	LOGICAL*4	DO_SUBST	! (i) symbol substitution requested ?
	LOGICAL*4	DO_EXTERN	! (i) only change external defaults ?
C
C.Purpose:	Let the user define external defaults	
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C.Notes:
C	- The user will be prompted to specify default values for all
C	  parameters defined in the PPD file of the current program.
C	- The given values will be checked and if they are correct, the
C	  corresponding DWARF symbols will be defined.
C	- The user can get help information for each parameter by answering
C	  with a question mark. The value prompt will be repeated.
C	- The user can give a CTRL/Z or # answer to indicate that he doesn't
C	  wish to set any further defaults.
C
C	Special features (see PPD definitions):
C	- OUTPUT type parameters are never shown.
C	- For parameters that cannot have an external default the current
C	  default will be shown, but the user cannot specify an other default.
C	- TEST parameters will only be shown if DWARF is in test mode (set
C	  via SPECIFY DWARF or by giving the command SPECIFY/TEST program).
C	- In DO_EXTERN mode (SPECIFY/EXTERNAL was given), the routine will
C	  only show (and prompt for) those parameters for which an external
C	  default already exists.
C	  This mode can be used to edit a set of defaults created through
C	  EXECUTE/SAVE without interference from parameters irrelevant to the
C	  application.
C-------------------------------------------------------------------------
C
C
	CHARACTER*(*)	BLANK, OPNPAR, CLOPAR, EQUALS
		PARAMETER (BLANK  = ' '  )
		PARAMETER (OPNPAR = '  (')
		PARAMETER (CLOPAR = ')'  )
		PARAMETER (EQUALS = ' = ')
C
	INTEGER*4	DWC_TEST_INQ, DWC_LEVEL_GET, DWC_SYM_BUILD
	INTEGER*4	DWC_INPUT, SP_DEF_CHECK, PV_DEF_GET
	INTEGER*4	PPD_READ_U, PPD_READ_UNXT, PPD_UNAM_GET, PPD_IOCD_GET
	INTEGER*4	PPD_AMAS_GET, PPD_SSTR_GET, PPD_PROMPT
	INTEGER*4	STR_COPY, SYMBOL_DEFINE
	INTEGER*4	MSG_SET
C
	CHARACTER*255	VALUE, WORK, PROMP1, PROMP2
	INTEGER*4	LVAL, LW, LP1, LP2
	CHARACTER	SYMBOL*50, UKEY*16, IOCD*8, TYPE*16, SSTR*5, GROUP*9
	INTEGER*4	LSYM, LU, LMIN, LI, LT, LS, LG
	INTEGER*4	IS, CURLEVEL, MAXLEVEL, DLEVEL
	LOGICAL*4	SWP, PROTO, TEST_MODE
C
C
	SP_MENU = DWC_SUCCESS
C
C					Go through the PPD file
C
	TEST_MODE = DWC_TEST_INQ ()
	IS = PPD_READ_U (BLANK)
	IF (IAND(IS,1).EQ.0) GOTO 999
	DO WHILE (IAND(IS,7).EQ.1)
		IS = PPD_UNAM_GET (UKEY,LU,LMIN,PROTO)
		IF (IAND(IS,1).NE.0) IS = PPD_IOCD_GET (IOCD,LI)
		IF (IAND(IS,1).EQ.0) GOTO 999
C
C					- skip TEST params unless in test mode
C					- skip non-input params
C
		IF (.NOT.TEST_MODE
	1		.AND. IAND(PPD_AMAS_GET('TEST'),1).NE.0) GOTO 500
		IF (IOCD(1:1).NE.'I' .AND. IOCD(1:1).NE.'M') GOTO 500
C
C					Get the current value for this parameter
C					- in DO_EXTERN mode, prompt only if
C					  an external default already exists.
C
		IS = DWC_SYM_BUILD (PROGNAM,STREAM,UKEY,SYMBOL,LSYM)
		DLEVEL = 0
 300		IS = PV_DEF_GET (SYMBOL(:LSYM),VALUE,LVAL,TYPE,LT)
		IF (DO_EXTERN .AND. (LT.EQ.0 .OR. TYPE(1:1).EQ.'p')) GOTO 500
C
C					Put the type of default and its value
C					in the prompt string
C
		LP2 = 0
		IF (LT.NE.0) THEN
			IS = STR_COPY (OPNPAR//TYPE(:LT)//CLOPAR,PROMP2,LP2)
			IF (LVAL.GT.0) IS = STR_COPY (EQUALS//VALUE(:LVAL),
	1							PROMP2,LP2)
		ENDIF
C
C					If the user cannot specify a local
C					value, print the value and a message
C
		IF (DO_LOCAL) THEN
		    IS = PPD_SSTR_GET (SSTR,LS,GROUP,LG)
		    IF (INDEX(SSTR(:LS),'L').EQ.0) THEN
			IS = MSG_SET (DWC_NOLOCVAL,1)
			CALL WNCTXT(DWLOG,DWMSG,UKEY)
			IF (LVAL.GT.0) THEN
			    CALL WNCTXT (DWLOG,'	!AS!AS',
	1					UKEY,PROMP2(:LP2))
			ENDIF
			GOTO 500
		    ENDIF
		ENDIF
C
C					Prompt on SYS$COMMAND (= terminal)
C
		SWP = .FALSE.
		IS = DWC_LEVEL_GET (CURLEVEL,MAXLEVEL)
		IS = PPD_PROMPT (BLANK,CURLEVEL+DLEVEL,SWP,PROMP1,LP1)
		IS = DWC_INPUT (WORK,PROMP1(:LP1)//PROMP2(:LP2),LW,1,0)
		IF (IAND(IS,1).EQ.0) GOTO 999				!CTRL/Z
C
C					Check the value
C					- if OK: define the symbol
C					- otherwise: repeat the question
C
		IF (LW.GT.0) THEN
			IS = SP_DEF_CHECK (SYMBOL(:LSYM),DLEVEL,0,WORK(:LW),
	1						VALUE,LVAL,DO_SUBST)
			IF (IAND(IS,1).EQ.0) THEN
				GOTO 300
			ENDIF
			IS = SYMBOL_DEFINE (SYMBOL(:LSYM),VALUE(:LVAL),
	1						DWC__GLOBALSYM)
		ENDIF
 500		IS = PPD_READ_UNXT ()
		IF (IAND(IS,1).EQ.0) GOTO 999
	ENDDO
C
C
	SP_MENU = DWC_SUCCESS
	RETURN
C
 999	SP_MENU = DWC_SUCCESS
	RETURN
	END
