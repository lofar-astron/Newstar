C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	DWC_HELP
C.Keywords:	Program Parameters, Help
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	900309 FMO - new code
C.Version:	930902 CMV - option for hypertext browser, remove /HOLD etc
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_HELP (STRING,PRTSW,DLEVEL)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	STRING		! (i) value string
	INTEGER*4	PRTSW		! (i) print control for help info
	INTEGER*4	DLEVEL		! (m) helplevel minus userlevel
C
C.Purpose:	Check for and process a help request
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS	no help request
C	warning	DWC_KEYVAHELP	help request (no message stored)
C.Notes:
C	- The string is a help request when it consists of a series of question
C	  marks and blanks.
C	- If two question marks are typed, or if the current helplevel
C	  is larger than one, the hypertext browser is used.
C	- If a hypertext request fails, we return to terminal help
C	- If a hypertext request is succesfull, we stick to it
C	- The helplevel is determined by the number of question marks typed
C	  (plus the current userlevel). It will be incremented at every call,
C	  so the user must initialize it when necessary.
C The following arguments are obsolete now:
C	- DLEVEL is the difference between the helplevel and the userlevel. Its 
C	  output value depends on PRTSW.
C	- PRTSW < 0	set DLEVEL = number of question marks in the string
C	        > 0	print the help information and set DLEVEL = 0
C	        = 0	print the help information if the new helplevel exceeds
C			the maximum user level (now 2), and add the number of
C			question marks in the string to DLEVEL
C-------------------------------------------------------------------------
C
C
	CHARACTER*(*)	BLANK, REQUEST
	INTEGER*4	LUN
		PARAMETER (BLANK   = ' ')
		PARAMETER (REQUEST = '?')
		PARAMETER (LUN = 6)			! LUN for help output
C
	INTEGER*4	DWC_LEVEL_GET, DWC_LEVEL_PUT
	INTEGER*4	PPD_HELP
	INTEGER*4	STR_SIGLEN, STR_SKIP_W, STR_MATCH_L
C
	INTEGER*4	IS, LSTR, PTR, NR, NRQ, LEVEL, MAXLEVEL
	LOGICAL*4	DO_HOLD
C
C
C					The string must start with a
C					question mark. Otherwise: return
C
	LSTR = STR_SIGLEN (STRING)
	IF (LSTR.EQ.0 .OR. STRING(1:1).NE.REQUEST) GOTO 999
	NRQ = 1
	PTR = 2
	DO_HOLD = .FALSE.
C
C					Count the nr of question marks
C					and check for the /HOLD qualifier
C					- the string is no help request if any
C					  other non-blank character occurs
C
	IS = STR_SKIP_W (BLANK,STRING(:LSTR),PTR)
	DO WHILE (PTR.LE.LSTR)
		IF (STRING(PTR:PTR).EQ.REQUEST) THEN	! question mark
			NRQ = NRQ+1
			PTR = PTR+1
			IS = STR_SKIP_W (BLANK,STRING(:LSTR),PTR)
		ELSE					! other char
			GOTO 999
		ENDIF
	ENDDO
C
C					Determine the help level
C
C	DLEVEL = DLEVEL+NRQ				! level difference
C	IS = DWC_LEVEL_GET (LEVEL,MAXLEVEL)		! user and max level
C	LEVEL = LEVEL+DLEVEL				! help level
C	IF (DO_HOLD) THEN
C		IS = DWC_LEVEL_PUT (LEVEL)		! set user = help level
C		DLEVEL = 0				! clear difference
C	ENDIF
C
C					Write help info if requested
C					- in user's parameter name order
C					- only non-prototype input parameters
C
C	IF (PRTSW.LT.0) THEN
C		DLEVEL = NRQ
C	ELSE IF (PRTSW.GT.0 .OR. LEVEL.GT.MAXLEVEL) THEN
		IS = PPD_HELP (BLANK,.FALSE.,.TRUE.,.FALSE.,NRQ,LUN)
C		IF (PRTSW.GT.0) DLEVEL = 0
C	ENDIF
C
	DWC_HELP = DWC_KEYVAHELP
	RETURN
C
 999	DWC_HELP = DWC_SUCCESS
	RETURN
	END
