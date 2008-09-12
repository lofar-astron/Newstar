C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	GEN_INPUT
C.Keywords:	Get Input Line
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	UNIX
C.Comments:
C	Only input from standard input is supported now.
C.Version:	900412 FMO - creation
C.Version:	900901 FMO - reformat long prompts, remove CR
C.Version:	910825 FMO - don't prompt if the implied input unit (5) is not
C			a terminal
C.Version:	920214 GvD - no optional arguments in MSG anymore
C.Version:	920316 GvD - use GEN_OUTPUT to write to standard output
C.Version:	920513 GvD - report IO-error more exactly (via GEN_FORIOS)
C.Version:      930615 CMV - Use wncaln to get length of input
C.Version:	930802 CMV - Correct carriage return handling
C		941021 JPH - Prompt formatting
C		941206 JPH - Always output prompt. Repeat reply if input not
C			      from terminal
C		950110 JPH - Make prompt output and reply echo conditional on  
C			      environment variable N_PSCTEST
C			     Remove DEV(2) ('FERRY', totally obsolete)
C		950117 JPH - Make '\' an alternative for "" (null input)
C		950208 JPH - Allow for comments following '\' null reply
C		951026 JPH - For batch applications; Output prompt terminator 
C				'~' if N_PSCTEST set  
C
C			   - use GEN_ISATERM iso. ISATTY to isolate system-dep.
C		961025 HjV - Add test for wn_da__ because
C				first time GEN_ISATERM(5) returns false
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GEN_INPUT (LINE,PROMPT,DEVCOD)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	LINE		! (o) answer
	CHARACTER*(*)	PROMPT		! (i) prompt
	INTEGER*4	DEVCOD		! (i) input-device code
C
C.Purpose:	Ask a single line from the user
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS	normal completion
C	warning	DWC_EOFCTRLZ	end of file (CTRL/Z or '#' given)
C	info	DWC_GETINPTR	answer truncated
C	fatal	DWC_GETINPERR	I/O error
C.Notes:
C	- The input is read from the implied unit, which is assumed to have
C	  logical unit nr 5. If the implied unit is not a terminal, no prompt
C	  will be issued.
C	- JPH 941021: The new prompt formatting is presently parallelled with
C	  the old formatting code, in order to enable a smooth transition. Once
C	  all the .psc/pef files have been reformatted, the old branch with the
C	  selection code (variable BAR) can de taken out.
C-------------------------------------------------------------------------
C
C
	CHARACTER*(*)	BLANK, DELIM
		PARAMETER (BLANK = ' ')
		PARAMETER (DELIM = ',=:'//BLANK)
C
	INTEGER*4	GEN_FORIOS, MSG_SET  , WNCALN
	INTEGER*4	STR_COPY, STR_COPY_U
	LOGICAL		GEN_ISATERM
C
	CHARACTER	ANSWER*255, STR*80
	LOGICAL		FMTD
	INTEGER*4	IS, LP, LA, LS, LI, PTR, SAVPTR, SAVLS, PMX 
	CHARACTER*11	DEV(0:1)
	 DATA DEV /'SYS$INPUT','SYS$COMMAND'/
	CHARACTER	N_PSCTEST
	 DATA		N_PSCTEST/'#'/
C
C
	IF (N_PSCTEST.EQ.'#') CALL WNGSEG('N_PSCTEST',N_PSCTEST)
	IF (GEN_ISATERM(5) .OR. N_PSCTEST.NE.' ') THEN ! terminal input or
						!  test mode
	   LP = LEN (PROMPT)			! set length of prompt
	ELSE					! ordinary batch mode
	   LP = 0				! ignore the prompt
	   IF (.NOT.GEN_ISATERM(5)) THEN	! Not terminal input on Alpha
#ifdef wn_da__
	      LP = LEN (PROMPT)			! set length of prompt
#endif
	   END IF       
	END IF	
	PTR = 1					!next prompt char to be copied
	LI = 0					!indentation length
	FMTD = .FALSE.				!option format unknown
C
C					Loop until prompt is complete
C
	DO WHILE (PTR.LE.LP)
	  STR = BLANK				!clear output string
	  LS = LI
	  PMX = MIN (LP, PTR+75)
!!	  IS = STR_COPY_U ('|',PROMPT(:LP),	!copy up to line delimiter
	  IS = STR_COPY_U ('|',PROMPT(:PMX),	!copy up to line delimiter
	1	PTR,STR,LS)
	  DO WHILE (PROMPT(PTR:PTR).EQ.'|')
	    	PTR = PTR+1			!skip line delimiters
	  ENDDO

	  IF (PTR.GT.LP) THEN  			! prompt completely copied:
	    IF (N_PSCTEST.NE.' ') THEN		! batch mode
	      LS=LS+1
	      STR(LS:LS)='~'			! append ~ and output with CR
	      CALL GEN_OUTPUT (STR(:LS))	!  (batch_sync.exe will strip)
	    ELSE   
	      CALL GEN_OUTPUT_NOCR (STR(:LS))	!write string (no CR)
	    ENDIF
 	  ELSE					!copy incomplete:
	    CALL GEN_OUTPUT (STR(:LS))		!write output string
	    LI = 4				!indent 4 chars
	  ENDIF
 	ENDDO
C
C	Read input (this used to be through '(Q,A)', but this caused
C	crashes on the SUN when reading from redirected stdin.
C
	READ (*,'(A)',END=900,ERR=999) ANSWER
	IF (ANSWER(1:1).EQ.'\') ANSWER='""'
	LA=WNCALN(ANSWER)
	LINE = ANSWER(:LA)
C
	IF (N_PSCTEST.NE.' ') CALL GEN_OUTPUT (ANSWER(:LA))
C
C					Check status and return
C
	IF (LA.GT.LEN(LINE)) THEN
		GEN_INPUT = DWC_GETINPTR		!answer truncated
	ELSE IF (LINE.EQ.'#') THEN
		GEN_INPUT = DWC_EOFCTRLZ		!end of input
	ELSE
		GEN_INPUT = DWC_SUCCESS			!normal completion
	ENDIF
	RETURN
C
 900	WRITE(*,*) '#     '				!Supply new line
	GEN_INPUT = DWC_EOFCTRLZ			!end of input
	RETURN
C
 999	GEN_INPUT = GEN_FORIOS ('Lunit 5')
	GEN_INPUT = MSG_SET (DWC_GETINPERR,1)
	CALL WNCTXT(DWLOG,DWMSG,DEV(DEVCOD))
	RETURN
	END
