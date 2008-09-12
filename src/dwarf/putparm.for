C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	PUT_PARM
C.Keywords:	Program Parameters, Store Value
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	900222 FMO - creation
C.Version:	920305 GvD - split into PUTPARM.FOR and GENPUTPAR.FOR
C			(system dependencies in GENPUTPAR.FOR)
C.Version:	920429 GvD - changed order of PP_ARG_CHECK arguments to overcome
C			SUN problems because length is still passed with %REF
C.Version:	940117 CMV - removed put_parm, now only put_parm_n and
C			put_parm_c calls available.
C.Version:	940308 WNB - no define message for NGEN X_ parameters
C.Version:	940427 CMV - add PUT_PARM_G to define global symbols
C.Version:	940823 CMV - no define messages anymore (confuses users)
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C The Put_parameter interface consists of three routines:
C
C
C	PUT_PARM_G(KEYWORD,VALSTR)
C
C		Put a character value from VALSTR in the global KEYWORD
C
C	PUT_PARM_C(KEYWORD,VALSTR,PROGSTRM)
C
C		Put a character value from VALSTR in the KEYWORD
C
C	PUT_PARM_N(KEYWORD,VALARR,NRVAL,FLAGS,PROGSTRM)
C
C		Put numeric values of the proper type (derived from 
C		the ppd-file) in the KEYWORD. The flags are defined in 
C		the module FLAGS_1 (use statement INCLUDE '(FLAGS_1)' in 
C		FORTRAN). The only value that is possible so far is:
C
C		   PARM__TOBY = the array is given in TO/BY-format.
C			That is: the data is ordered in the way FROM, TO,
C			STEPSIZE (NRVAL then must be a multiple of 3).
C
C	CHARACTER*(*)	KEYWORD		! (i) program's parameter name
C
C	CHARACTER*(*)	VALSTR		! (i) value (given as a string)
C				The string will not be converted in any way.
C				You must give characters in uppercase and
C				should not use TAB-characters.
C
C	<datatype>	VALARR(*)	! (i) value (given as an array)
C				It must be passed in the standard Fortran way.
C				The array must be declared according to the
C				data type defined in the PIN file. It will be
C				converted to a string.
C	INTEGER*4	NRVAL		! (i) nr of elements in VALARR
C				Ignored when no VALARR argument is present.
C				= 1 (default)
C	INTEGER*4	FLAGS		! (i) control flags
C
C	CHARACTER*(*)	PROGSTRM	! (i) target program and stream names
C				format: [<program>][$<stream>]
C				defaults: current names
C
C.Purpose:	Define an external default for a program parameter
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	error	DWC_PARAMERR	messages written
C.Notes:
C	This function gives an application program the possibility to define
C	an external default in the same way as SPECIFY does.
C	- The DWARF symbol <program>$<stream>_<keyword> will be defined.
C	  The program and stream names are taken from the argument PROGSTRM or
C	  default to the current names. <program> can be a group name, and
C	  <stream> can be 0.
C	- The value can be given either as a string or as an array. If it's
C	  given as an array, it will be converted to a string.
C
C --- Obsolete, please do not use this feature because it is phased out.
C	A program parameter is known under two, in principle different, names:
C	- The program-keyword (PROG_PARAMETER in the PPD file) is used in the
C	  program code, e.g. in PUT_PARM's argument list.
C	- The user-keyword (KEYWORD in the PPD file) is used outside the
C	  program, e.g. in DWARF's prompts and in DWARF symbol names.
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PUT_PARM_N (PKEY,VALARR,NRVAL,
	1				FLAGS,PROGSTRM)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	PKEY		! (i) program's parameter name
	BYTE		VALARR(*)	! (i) value given as an array
	INTEGER*4	NRVAL		! (i) number of values in the array
	INTEGER*4	FLAGS		! (i) additional flags
	CHARACTER*(*)	PROGSTRM	! (i) target program and stream names
C
C.Purpose:	Define an external default for a program parameter
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCES
C	error	DWC_PARAMERR	messages written
C.Notes:
C-------------------------------------------------------------------------
C
C
	CHARACTER*(*)	BLANK
		PARAMETER (BLANK = ' ')
C
	INTEGER*4	PP_ARG_CHECK
	INTEGER*4	PP_CTL_OPEN, PP_CTL_CLOSE
	INTEGER*4	PPD_DTYPE_GET
	INTEGER*4	PV_DEF_ENCODE
	INTEGER*4	SYMBOL_DEFINE
	INTEGER*4	MSG_SET
C
	CHARACTER	VALUE*255, SYMBOL*38, DTYPE*1
	INTEGER*4	IS, PLEN, LVAL, LSYM
	LOGICAL*4	FOREIGN
C
C
C					Set up for PUT_PARM
C
	IS = PP_CTL_OPEN (PKEY,PROGSTRM,SYMBOL,LSYM,FOREIGN)
	IF (IAND(IS,1).EQ.0) GOTO 998
C
C					Get the parameter's data type and
C					get out if it is character type
C
	IS = PPD_DTYPE_GET (DTYPE,PLEN)
	IF (IAND(IS,1).EQ.0) GOTO 998
	IF (DTYPE.EQ.'C') THEN
		IS = PP_CTL_CLOSE (BLANK,FOREIGN)
		IF (IAND(IS,1).EQ.0) GOTO 999
		PUT_PARM_N = 0
		RETURN
	ENDIF
C
C					Convert the value to a proper string
C
	IS = PV_DEF_ENCODE (VALARR,NRVAL,PLEN,FLAGS,VALUE,LVAL)
	IF (LVAL.EQ.0) THEN
		IS = MSG_SET (DWC_PARNOVAL,1)
		CALL WNCTXT(DWLOG,DWMSG,PKEY)
	END IF
	IF (IAND(IS,1).EQ.0) GOTO 998
C
C					Define the DWARF symbol
C
	IS = SYMBOL_DEFINE (SYMBOL(:LSYM),VALUE(:LVAL),DWC__GLOBALSYM)
	IF (IAND(IS,1).EQ.0) GOTO 998
C
C					Write a log message at level 3
C
C	IF (INDEX(SYMBOL(:LSYM),'_X_').EQ.0)	!MESSAGE IF NOT X_ VALUE
C	1		CALL WNCTXT(DWLOG,'DWARF symbol !AS = !AS defined',
C	1		SYMBOL(:LSYM),VALUE(:LVAL))
C
C					Close PUT_PARM operations
C
	IS = PP_CTL_CLOSE (PROGSTRM,FOREIGN)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	PUT_PARM_N = DWC_SUCCESS
	RETURN
C
 998	IS = PP_CTL_CLOSE (BLANK,FOREIGN)
 999	PUT_PARM_N = MSG_SET (DWC_PARAMERR,1)
	CALL WNCTXT(DWLOG,DWMSG,PKEY)
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PUT_PARM_C (PKEY,VALSTR,PROGSTRM)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	PKEY		! (i) program's parameter name
	CHARACTER*(*)	VALSTR		! (i) value given as a string
	CHARACTER*(*)	PROGSTRM	! (i) target program and stream names
C
C.Purpose:	Define an external default for a program parameter
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCES
C	error	DWC_PARAMERR	messages written
C.Notes:
C-------------------------------------------------------------------------
C
C
	CHARACTER*(*)	BLANK
		PARAMETER (BLANK = ' ')
C
	INTEGER*4	PP_ARG_CHECK
	INTEGER*4	PP_CTL_OPEN, PP_CTL_CLOSE
	INTEGER*4	DWC_STR_STANDARD
	INTEGER*4	SYMBOL_DEFINE
	INTEGER*4	MSG_SET
C
	CHARACTER	VALUE*255, SYMBOL*38
	INTEGER*4	IS, LVAL, LSYM
	LOGICAL*4	FOREIGN
C
C
	FOREIGN = .FALSE.
C
C					Set up for PUT_PARM
C
	IS = PP_CTL_OPEN (PKEY,PROGSTRM,SYMBOL,LSYM,FOREIGN)
	IF (IAND(IS,1).EQ.0) GOTO 998
C
C					Convert the value to a proper string
C
C	IS = DWC_STR_STANDARD (VALUE,LVAL,VALSTR)
	IS = DWC_STR_STANDARD (VALSTR,VALUE,LVAL)
	IF (LVAL.EQ.0) THEN
		IS = MSG_SET (DWC_PARNOVAL,1)
		CALL WNCTXT(DWLOG,DWMSG,PKEY)
	END IF
	IF (IAND(IS,1).EQ.0) GOTO 998
C
C					Define the DWARF symbol
C
	IS = SYMBOL_DEFINE (SYMBOL(:LSYM),VALUE(:LVAL),DWC__GLOBALSYM)
	IF (IAND(IS,1).EQ.0) GOTO 998
C
C					Write a log message at level 3
C
C	IF (INDEX(SYMBOL(:LSYM),'_X_').EQ.0)	!MESSAGE IF NOT X_ VALUE
C	1		CALL WNCTXT(DWLOG,'DWARF symbol !AS = !AS defined',
C	1		SYMBOL(:LSYM),VALUE(:LVAL))
C
C					Close PUT_PARM operations
C
C
	IS = PP_CTL_CLOSE (PROGSTRM,FOREIGN)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
	PUT_PARM_C = DWC_SUCCESS
	RETURN
C
 998	IS = PP_CTL_CLOSE (BLANK,FOREIGN)
 999	PUT_PARM_C = MSG_SET (DWC_PARAMERR,1)
	CALL WNCTXT(DWLOG,DWMSG,PKEY)
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PUT_PARM_G (SYMBOL,VALSTR)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	SYMBOL		! (i) name of global symbol
	CHARACTER*(*)	VALSTR		! (i) value given as a string
C
C.Purpose:	Define an value for a global symbol
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCES
C	error	DWC_PARAMERR	messages written
C.Notes:
C-------------------------------------------------------------------------
C
C
	CHARACTER*(*)	BLANK
		PARAMETER (BLANK = ' ')
C
	INTEGER*4	DWC_STR_STANDARD
	INTEGER*4	SYMBOL_DEFINE
	INTEGER*4	MSG_SET
	INTEGER*4	WNCAL0
C
	CHARACTER	VALUE*255
	INTEGER*4	IS, LVAL, LSYM
C
C					Convert the value to a proper string
C
	IS = DWC_STR_STANDARD (VALSTR,VALUE,LVAL)
	IF (LVAL.EQ.0) THEN
		IS = MSG_SET (DWC_PARNOVAL,1)
		CALL WNCTXT(DWLOG,DWMSG,SYMBOL)
	END IF
	IF (IAND(IS,1).EQ.0) GOTO 998
C
C					Define the DWARF symbol
C
	LSYM=WNCAL0(SYMBOL)
	IF (LSYM.EQ.0) GOTO 998
	IS = SYMBOL_DEFINE (SYMBOL(:LSYM),VALUE(:LVAL),DWC__GLOBALSYM)
	IF (IAND(IS,1).EQ.0) GOTO 998
C
C					Write a log message at level 3
C
C	CALL WNCTXT(DWLOG,'DWARF symbol !AS = !AS defined',
C	1		SYMBOL(:LSYM),VALUE(:LVAL))
C
	PUT_PARM_G = DWC_SUCCESS
	RETURN
C
 998	PUT_PARM_G = MSG_SET (DWC_PARAMERR,1)
	CALL WNCTXT(DWLOG,DWMSG,SYMBOL)
	RETURN
	END

