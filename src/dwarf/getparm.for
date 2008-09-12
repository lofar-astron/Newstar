C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	GET_PARM
C.Keywords:	Program Parameters, Get Value
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	900322 FMO - creation
C.Version:	911213 GvD - do not return UNDEF_J in NR, otherwise
C			GP_ARG_CHECK treats NELEM next time as not given.
C.Version:	920305 GvD - split into GETPARM.FOR and GENGETPAR.FOR
C			(system dependencies in GENGETPAR.FOR)
C.Version:	920429 GvD - changed order of GP_ARG_CHECK arguments because
C			%REF is still passed as a string on the SUN.
C.Version:	920513 GvD - test NR=UNDEF_J at the end
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C	GET_PARM (KEYWORD,ARRAY,NBYT,NR,DEFSTR,DEFARR,NRDEF,FLAGS,STROUT)
C
C					Required arguments:
C
C	CHARACTER*(*)	KEYWORD		! (i) program's parameter name
C	<datatype>	ARRAY(*)	! (o) data array
C				It must be passed in the standard Fortran way.
C				The array must be declared according to the
C				data type defined in the PIN file. The elements
C				in a character-type array must at least have
C				the length given in the PIN file.
C	INTEGER*4	NBYT		! (i) total length of array in bytes
C				The function will calculate the nr of elements
C				in the array by dividing the element length
C				(determined by the data type or the length of
C				character-type elements) into this total
C				array length. If the array is too small to
C				receive all data, an error condition will be
C				returned.
C
C					Optional arguments:
C
C	INTEGER*4	NR		! (o) nr of filled elements in array
C				The argument must be present if there can be
C				more than one element (NVAL>1 in PPD file) or
C				if wildcards or null values are possible
C				(SWITCHES=WILD or SWITCHES=NULL).
C				The following special values can be returned:
C				-2 if end-of-loop or CNTRL/Z given
C				-1 if a wildcard value was given (value=*),
C				 0 if a null value was given (value="").
C				Undefined values between defined values are
C				also counted. E.g. (for TYPE=I) 1,,2
C				results in ARRAY=1,UNDEF_I,2 and NR=3.
C	CHARACTER*(*)	DEFSTR		! (i) default value (given as a string)
C				The string will be evaluated as if it was
C				entered in answer to a DWARF prompt.
C	<datatype>	DEFARR(*)	! (i) default value (given as an array)
C				Must be declared in the same way as ARRAY.
C				The array will be converted to a prompt string.
C	INTEGER*4	NRDEF		! (i) nr of elements in DEFARR
C				Ignored when no DEFARR argument is present.
C				 1 (default)
C				 0: default value is "" (null value)
C				-1: default value is * (wildcard)
C	INTEGER*4	FLAGS		! (i) control flags
C				The flags are defined in the module FLAGS_1
C				(use statement INCLUDE '(FLAGS_1)' in FORTRAN).
C				PARM__OVERRIDE = the default you give via
C					DEFSTR or DEFARR must override the
C					current value of the keyword.
C					GET_PARM will prompt the user to show
C					that the default is taken.
C				PARM__TOBY = the default in DEFARR is given
C					in TO/BY-format. That is: the data is
C					ordered in the way FROM-TO-STEPSIZE
C					(NRDEF then must be a multiple of 3)
C	CHARACTER*(*)	STROUT		! (o) last value set
C				(as interpreted by GET_PARM).
C
C.Purpose:	Get a value set for a program parameter
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	success	DWC_WILDCARD	wildcard value (NR = -1 on return)
C	success	DWC_NULLVALUE	null value (NR = 0 on return)
C	warning	DWC_ENDOFLOOP	end of loop or CNTRL/Z
C	warning	DWC_STRTOOSHO	string overflow during value conversion
C	error	DWC_PARWRANS	wrong answer (in batch mode)
C	fatal	DWC_PARNOTFND	invalid keyword
C	fatal	DWC_PARTOOSML	ARRAY doen't contain enough elements
C	fatal	DWC_PARELTSML	string elements in ARRAY are too short
C	fatal	DWC_PARNONR	NR argument is required but not present
C	fatal	DWC_PARNOVAL	no value found
C	fatal	DWC_PARWRDEF	wrong default value given
C	fatal	DWC_GETINPERR	error getting input
C	fatal	DWC_SAVEOVFLO	save-string overflow
C.Notes:
C	- This function passes a complete argument list to the action routines
C	  GET_PARM_N (numerical data) or GET_PARM_C (character data).
C
C	If no more parameter values are available, 3 things can happen:
C	- If GET_PARM must ask on the terminal, it will do so and it will take
C	  the first value set from the new data. If the user answers with
C	  CTRL/Z, the status DWC_ENDOFLOOP will be returned.
C	- If GET_PARM must not ask and the parameter is of the LOOP-type
C	  (ATTRIBUTE=LOOP in the PIN file), the routine will return the status
C	  DWC_ENDOFLOOP. At the next call GET_PARM will start re-using the
C	  current value sets.
C	- If GET_PARM must not ask and the parameter is of the non-LOOP type,
C	  the routine will start re-using the current value sets without
C	  warning.
C
C	GET_PARM will ask if:
C	- the ASK switch is on (either set by the PIN file or by the user,
C	  where the PIN file overrides the user)
C	- a PIN default or a caller default is taken as the value, unless
C	  /NOASK is given for this keyword. A PIN or caller default will be
C	  taken if no value has been given via SPECIFY. A caller default will
C	  always be taken if the caller gives the flag PARM__OVERRIDE.
C
C	- The format of the prompt strings issued by GET_PARM will depend on
C	  the current userlevel.
C	- If there is a default (given via SPECIFY, in the PIN file or in the
C	  argument list of GET_PARM), it will always be part of the prompt
C	  string. GET_PARM will not show the complete default, but only the
C	  next value set from the default.
C	- When the value sets in the default are exhausted, GET_PARM will
C	  either look for a new default from the caller or re-use the same
C	  default.
C
C	The user can give several types of answers:
C	- A value string, which then will be the new value.
C	- A wildcard (*) provided that the WILD option is set in the PIN file.
C	  GET_PARM then will return with the success status DWC_WILDCARD and
C	  with NR = -1.
C	- A "null" value ("") provided that the NULL option is set in the PIN
C	  file. GET_PARM then will return with the success status DWC_NULLVALUE
C	  and with NR = 0.
C	- An empty answer (just a <RETURN>), in which case the default will be
C	  the new value.
C	- One of the previous answers appended with qualifiers:
C	  /NOASK tells GET_PARM to stop asking values for this keyword.
C		GET_PARM will start asking again once the user has answered
C		/ASK=keyword to a prompt for any other program parameter.
C	  /(NO)SAVELAST will override the SAVE switch set via SPECIFY DWARF
C		or EXECUTE/SAVE. When SAVE is set, the values (typed-in or
C		default) will be saved in a DWARF symbol (as in SPECIFY).
C	- A question mark (?) tells GET_PARM to show help information and to
C	  repeat the prompt.
C	- /ASK=keyword tells GET_PARM to prompt when a value is needed for
C	  the specified program parameter. This qualifier cannot be appended
C	  to a any other type of answer.
C	- CTRL/Z or # will return the status DWC_ENDOFLOOP to the caller.
C	  Note that this status can also be returned for LOOP-type parameters.
C
C	- If the value for a node name parameter ends with a colon, the current
C	  node will be set to the value (without colon). This setting only
C	  holds for the current program run; the current node set via SPECIFY
C	  DWARF is not changed.
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GET_PARM_N (KEYWORD,ARRAY,NBYT,
	1				NR,DEFSTR,DEFARR,NRDEF,FLAGS,STROUT)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	KEYWORD		! (i) program's parameter name
	BYTE		ARRAY(*)	! (o) data array
	INTEGER*4	NBYT		! (i) total length of array in bytes
	INTEGER*4	NR		! (o) nr of filled elements in array
	CHARACTER*(*)	DEFSTR		! (i) default value (given as a string)
	BYTE		DEFARR(*)	! (i) default value (given as an array)
	INTEGER*4	NRDEF		! (i) nr of elements in DEFARR
	INTEGER*4	FLAGS		! (i) flags to control GET_PARM
	CHARACTER*(*)	STROUT		! (o) last value set
C
C.Purpose:	Get the next value set for a numerical program parameter
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	see GET_PARM, but extra:
C	warning		0	data type is CHARACTER
C.Notes:
C	- This function is called by GET_PARM for any program parameter.
C	- It sets up the GET_PARM environment for the parameter.
C	- For numerical-type parameters, it does all the necessary things.
C	- For character-type parameters, the routine returns to GET_PARM
C	  with status 0. GET_PARM then calls GET_PARM_C to do the work.
C-------------------------------------------------------------------------
C
C
	CHARACTER*(*)	BLANK, EOLVAL, EOLMSG
		PARAMETER (BLANK  = ' ')
		PARAMETER (EOLVAL = '#')
		PARAMETER (EOLMSG = '#         ! = end-of-loop')
C
	INTEGER*4	GP_CTL_OPEN, GP_CTL_CLOSE
	INTEGER*4	GP_ARG_CHECK
	INTEGER*4	GP_VAL_READ_N, GP_VAL_FILL
	INTEGER*4	GP_SAV_WRITE
	INTEGER*4	PPD_DTYPE_GET
	INTEGER*4	MSG_SET
C
	CHARACTER	VALUE*255, DEFAULT*255, LOGMSG*255, DTYPE*1
	INTEGER*4	IS, LV, LDEF, PLEN, FOUND
C
C
C					Start GET_PARM operations
C					- check the parameter name
C					- load its PPD description in common
C					- load value administration in common
C
	STROUT = BLANK
	IS = GP_CTL_OPEN (KEYWORD)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Get data type and length
C					- get out for character type
C
	IS = PPD_DTYPE_GET (DTYPE,PLEN)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (DTYPE.EQ.'C') THEN
		GET_PARM_N = 0
		RETURN
	ENDIF
C
C					Analyse the arguments
C
	IS = GP_ARG_CHECK (PLEN,NBYT,NR,DEFAULT,LDEF,
	1				DEFSTR,DEFARR,NRDEF,PLEN,FLAGS)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Get the next value set or end-of-loop
C					- if the value block is exhausted,
C					  get new value sets (or end-of-loop)
C					  and loop back
C
	FOUND = GP_VAL_READ_N (ARRAY,NR,VALUE,LV)
	DO WHILE (IAND(FOUND,1).EQ.0 .AND. FOUND.NE.DWC_ENDOFLOOP)
		IS = GP_VAL_FILL (DEFAULT,LDEF,FLAGS)
		IF (IAND(IS,1).NE.0) THEN
			FOUND = GP_VAL_READ_N (ARRAY,NR,VALUE,LV)
		ELSE
			IF (IS.NE.DWC_ENDOFLOOP) GOTO 999
			FOUND = DWC_ENDOFLOOP
		ENDIF
	ENDDO
	GET_PARM_N = FOUND
C
C					Build save string and log message
C
	IF (IAND(FOUND,1).NE.0) THEN
		LOGMSG = VALUE(:LV)
	ELSE
		VALUE = EOLVAL
		LV = 1
		LOGMSG = EOLMSG
	ENDIF
	STROUT = VALUE(:LV)
C
C					Close GET_PARM operations
C					- write log message at level 3,
C					- possibly save the value string
C					- save the value administration
C					- return
C					Do not return UNDEF_J in NR, otherwise
C					GP_ARG_CHECK treats NELEM next time
C					as not given.
C
C	CALL WNCTXT (DWLOG,DWMSG,'!AS = !AS',KEYWORD,LOGMSG)
	IS = GP_SAV_WRITE (VALUE,LV)
	IF (IAND(IS,1).EQ.0) GET_PARM_N = IS
C
C
	IS = GP_CTL_CLOSE ()
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (NR.EQ.UNDEF_J) NR = UNDEF_J+1
	RETURN
C
 999	GET_PARM_N = IS
	IS = MSG_SET (DWC_PARAMERR,1)
	CALL WNCTXT(DWLOG,DWMSG,KEYWORD)
	IF (NR.EQ.UNDEF_J) NR = UNDEF_J+1
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION GET_PARM_C (KEYWORD,ARRAY,NBYT,
	1				NR,DEFSTR,DEFARR,NRDEF,FLAGS,STROUT)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	KEYWORD		! (i) program's parameter name
	CHARACTER*(*)	ARRAY(*)	! (o) data array
	INTEGER*4	NBYT		! (i) total length of array in bytes
	INTEGER*4	NR		! (o) nr of filled elements in array
	CHARACTER*(*)	DEFSTR		! (i) default value (given as a string)
	CHARACTER*(*)	DEFARR(1)	! (i) default value (given as an array)
	INTEGER*4	NRDEF		! (i) nr of elements in DEFARR
	INTEGER*4	FLAGS		! (i) flags to control GET_PARM
	CHARACTER*(*)	STROUT		! (o) last value set
C
C.Purpose:	Get the next value set for a character-type program parameter
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	see GET_PARM
C.Notes:
C-------------------------------------------------------------------------
C
C
	CHARACTER*(*)	BLANK, EOLVAL, EOLMSG
		PARAMETER (BLANK  = ' ')
		PARAMETER (EOLVAL = '#')
		PARAMETER (EOLMSG = '#         ! = end-of-loop')
C
	INTEGER*4	GP_CTL_CLOSE
	INTEGER*4	GP_ARG_CHECK
	INTEGER*4	GP_VAL_READ_C, GP_VAL_FILL
	INTEGER*4	GP_SAV_WRITE
	INTEGER*4	PPD_CMAS_GET, DWC_NODE_SET
	INTEGER*4	MSG_SET
C
	CHARACTER*255	VALUE, DEFAULT, LOGMSG
	INTEGER*4	IS, LV, LDEF, FOUND
C
C
	STROUT = BLANK
C
C					Analyse the arguments
C
	IS = GP_ARG_CHECK (LEN(ARRAY(1)),NBYT,NR,DEFAULT,LDEF,
	1		DEFSTR,%REF(DEFARR(1)),NRDEF,LEN(DEFARR(1)),FLAGS)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Get the next value set or end-of-loop
C					- if the value block is exhausted,
C					  get new value sets (or end-of-loop)
C					  and loop back
C
 100	FOUND = GP_VAL_READ_C (ARRAY,NR,VALUE,LV)
	DO WHILE (IAND(FOUND,1).EQ.0 .AND. FOUND.NE.DWC_ENDOFLOOP)
		IS = GP_VAL_FILL (DEFAULT,LDEF,FLAGS)
		IF (IAND(IS,1).NE.0) THEN
			FOUND = GP_VAL_READ_C (ARRAY,NR,VALUE,LV)
		ELSE
			IF (IS.NE.DWC_ENDOFLOOP) GOTO 999
			FOUND = DWC_ENDOFLOOP
		ENDIF
	ENDDO
	GET_PARM_C = FOUND
C
C					Build save string and log message
C
	IF (IAND(FOUND,1).NE.0) THEN
		LOGMSG = VALUE(:LV)
	ELSE
		VALUE = EOLVAL
		LV = 1
		LOGMSG = EOLMSG
	ENDIF
	STROUT = VALUE(:LV)
C
C					Close GET_PARM operations
C					- write log message at level 3,
C					- possibly save the value string
C					- act on "set current node" flag
C					  (loop back to get the next node name)
C					- save the value administration
C					- return
C					Do not return UNDEF_J in NR, otherwise
C					GP_ARG_CHECK treats NELEM next time
C					as not given.
C
C	CALL WNCTXT (DWLOG,'!AS = !AS',KEYWORD,LOGMSG)
	IS = GP_SAV_WRITE (VALUE,LV)
	IF (IAND(IS,1).EQ.0) GET_PARM_C = IS
C
	IF (IAND(PPD_CMAS_GET('NODE'),1) .NE. 0) THEN
		IS = DWC_NODE_SET (VALUE(:LV))
		IF (IS.EQ.DWC_SETCURNOD) THEN
			STROUT = BLANK
			GOTO 100
		ENDIF
	ENDIF
C
	IS = GP_CTL_CLOSE ()
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (NR.EQ.UNDEF_J) NR = UNDEF_J+1
	RETURN
C
 999	GET_PARM_C = IS
	IS = MSG_SET (DWC_PARAMERR,1)
	CALL WNCTXT(DWLOG,DWMSG,KEYWORD)
	IF (NR.EQ.UNDEF_J) NR = UNDEF_J+1
	RETURN
	END
