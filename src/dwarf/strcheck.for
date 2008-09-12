C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	STR_CHECK
C.Keywords:	String, Check Type
C.Author:	Ger van Diepen (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	821203 GVD - creation of GENISNUM
C.Version:	831110 GVD
C.Version:	890112 FMO - removed optional arguments: length check must be
C			done by caller, no messages are stored.
C			- no GEN status codes are returned
C			- new code using STR_SKIP
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION STR_CHECK_ALPH (STRING)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INTEGER*4	STR_CHECK_NUM !(STRING)
	INTEGER*4	STR_CHECK_ANUM !(STRING)
	INTEGER*4	STR_CHECK_ANUM_ !(STRING)
	INTEGER*4	STR_CHECK_ANUMX !(STRING)
	INTEGER*4	STR_CHECK_ANUMA !(STRING)
C
	CHARACTER*(*)	STRING		! (i) string to be checked
C
C.Purpose:	Check the type of the string
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C.Notes:
C	ALPH	alphabetic (uppercase only)
C	NUM	numeric (digits only)
C	ANUM	alphanumeric
C	ANUM_	extended alphanumeric (_ also allowed)
C	ANUMX	extended alphanumeric (_ and $ also allowed)
C	ANUMA	alphanumeric but first character must be alphabetic
C
C	Null-strings are acceptable for all types except ANUMA.
C-------------------------------------------------------------------------
C
	CHARACTER*(*)	ALPHABET, DIGITS, USCORE, DOLLAR
		PARAMETER (ALPHABET = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ')
		PARAMETER (DIGITS   = '0123456789')
		PARAMETER (USCORE   = '_')
		PARAMETER (DOLLAR   = '$')
C
	INTEGER*4	STR_SKIP_W
C
	INTEGER*4	PTR, NSKIP
C
C
C	ENTRY STR_CHECK_ALPH (STRING)
C
	PTR = 1
	NSKIP = STR_SKIP_W (ALPHABET,STRING,PTR)
	STR_CHECK_ALPH = NSKIP.EQ.LEN(STRING)
	RETURN
C
C
	ENTRY STR_CHECK_NUM (STRING)
C
	PTR = 1
	NSKIP = STR_SKIP_W (DIGITS,STRING,PTR)
	STR_CHECK_NUM = NSKIP.EQ.LEN(STRING)
	RETURN
C
C
	ENTRY STR_CHECK_ANUM (STRING)
C
	PTR = 1
	NSKIP = STR_SKIP_W (ALPHABET//DIGITS,STRING,PTR)
	STR_CHECK_ANUM = NSKIP.EQ.LEN(STRING)
	RETURN
C
C
	ENTRY STR_CHECK_ANUM_ (STRING)
C
	PTR = 1
	NSKIP = STR_SKIP_W (ALPHABET//DIGITS//USCORE,STRING,PTR)
	STR_CHECK_ANUM_ = NSKIP.EQ.LEN(STRING)
	RETURN
C
C
	ENTRY STR_CHECK_ANUMX (STRING)
C
	PTR = 1
	NSKIP = STR_SKIP_W (ALPHABET//DIGITS//USCORE//DOLLAR,STRING,PTR)
	STR_CHECK_ANUMX = NSKIP.EQ.LEN(STRING)
	RETURN
C
C
	ENTRY STR_CHECK_ANUMA (STRING)
C
	PTR = 1
	NSKIP = STR_SKIP_W (ALPHABET,STRING,PTR)
	IF (NSKIP.EQ.0) THEN
		STR_CHECK_ANUMA = 0
	ELSE IF (PTR.GT.LEN(STRING)) THEN
		STR_CHECK_ANUMA = 1
	ELSE
		NSKIP = STR_SKIP_W (ALPHABET//DIGITS,STRING,PTR)
		STR_CHECK_ANUMA = PTR.GT.LEN(STRING)
	ENDIF
	RETURN
C
C
	END
