C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	DWC_SYMLIST
C.Keywords:	DWARF Symbol List
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C	- A DWARF symbol list consists of one or more comma-separated fields.
C	- Each field in principle has 3 components:
C		<program_name>$<stream_name>_<parameter_name>
C	  where each name can be absent or wildcarded (*). The dollar and
C	  underscore prefixes are part of the stream and parameter name
C	  components.
C.Version:	910805 FMO - creation
C.Version:	920214 GvD - no optional arguments in MSG anymore
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION DWC_SYMLIST_EXPAND (INLIST,OUTLIST,LOUT)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	INLIST		!(i) input list
	CHARACTER*(*)	OUTLIST		!(o) output list
	INTEGER*4	LOUT		!(o) significant length of outlist
C
C.Purpose:	Expand the symbol list
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success		1
C	warning	DWC_STRTOOSHO	outlist buffer to short
C	error	DWC_SYNERRSYM	syntax error in symbol name
C.Notes:
C	- If a component is absent in an input list field, the component from
C	  the previous field will be inserted, except that the default stream
C	  for global programs (DWARF and GLOBAL) is $0. The first defaults are:
C	  *$<current_stream>_*.
C	- The syntax of each output field will be checked.
C-------------------------------------------------------------------------
C
C
	CHARACTER*(*)	BLANK, COMMA, WILD, DOLLAR
		PARAMETER (BLANK  = ' ')
		PARAMETER (COMMA  = ',')
		PARAMETER (WILD   = '*')
		PARAMETER (DOLLAR = '$')
C
	INTEGER*4	DWC_SYM_SPLIT, DWC_SYM_BUILD
	INTEGER*4	DWC_PROG_CHECK, DWC_STREAM_GET, DWC_STREAM_CHECK
	INTEGER*4	STR_SIGLEN, STR_COPY, STR_COPY_U, STR_CHECK_ANUMX
	INTEGER*4	MSG_SET  
C
	CHARACTER	DEFPROG*9, DEFSTRM*12, DEFKEY*16
	CHARACTER	PROGNAM*9, STREAM*12, KEY*16
	CHARACTER	INFIELD*64, OUTFIELD*64, STRM*16
	INTEGER*4	LIF, LOF, LDP, LDS, LDK, LP, LS, LK
	INTEGER*4	IS, LIN, PTR
	LOGICAL*4	IS_GLOBAL
C
C
	OUTLIST = BLANK
	LOUT = 0
C
C					Set initial defaults
C
	DEFPROG = WILD
	LDP = 1
	IS = DWC_STREAM_GET (DEFSTRM,LDS,.FALSE.)
	DEFKEY = WILD
	LDK = 1
C
C					Extract next list field
	PTR = 1
	LIN = STR_SIGLEN (INLIST)
	DO WHILE (PTR.LE.LIN)
		LIF = 0
		IS = STR_COPY_U (COMMA,INLIST(:LIN),PTR,INFIELD,LIF)
C
C					Split the symbol name
C
		IS = DWC_SYM_SPLIT (INFIELD(:LIF),PROGNAM,LP,STRM,LS,KEY,LK)
		IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Check the program name
C					and refresh the default
C
		IF (LP.EQ.0) THEN
			PROGNAM = DEFPROG
			LP = LDP
			IF (PROGNAM(:LP).EQ.WILD) THEN
				IS_GLOBAL = .FALSE.
			ELSE
				IS = DWC_PROG_CHECK (PROGNAM,LP,IS_GLOBAL)
				IF (IAND(IS,1).EQ.0) GOTO 999
			ENDIF
		ELSE IF (PROGNAM(:LP).EQ.WILD) THEN
			IS_GLOBAL = .FALSE.
			DEFPROG = WILD
			LDP = 1
		ELSE
			IS = DWC_PROG_CHECK (PROGNAM,LP,IS_GLOBAL)
			IF (IAND(IS,1).EQ.0) GOTO 999
			DEFPROG = PROGNAM
			LDP = LP
		ENDIF
C
C					Check the stream name
C					and refresh the default
C
		IF (LS.EQ.0) THEN
			IF (IS_GLOBAL) THEN
				IS = DWC_STREAM_GET (STRM,LS,.TRUE.)
			ELSE
				STRM = DEFSTRM
				LS = LDS
			ENDIF
		ENDIF
		IF (STRM(:LS).NE.DOLLAR//WILD) THEN
			IS = DWC_STREAM_CHECK (STRM(:LS),STREAM,LS,IS_GLOBAL)
			IF (IAND(IS,1).EQ.0) GOTO 999
		ELSE
			STREAM = STRM(:LS)
		ENDIF
		IF (.NOT.IS_GLOBAL) THEN
			DEFSTRM = STREAM
			LDS = LS
		ENDIF
C
C					Check the parameter name
C					and refresh the default
C
		IF (LK.EQ.0) THEN
			KEY = DEFKEY
			LK = LDK
		ELSE IF (KEY(:LK).NE.WILD) THEN
			IF (LK.GT.16) GOTO 999
			IS = STR_CHECK_ANUMX (KEY(:LK))
			IF (IAND(IS,1).EQ.0) GOTO 999
			DEFKEY = KEY(:LK)
			LDK = LK
		ELSE
			DEFKEY = WILD
			LDK = 1
		ENDIF
C
C					Append field to output list
C
		IF (LOUT.GT.0) IS = STR_COPY (COMMA,OUTLIST,LOUT)
		IS = DWC_SYM_BUILD (PROGNAM(:LP),STREAM(:LS),KEY(:LK),
	1						OUTFIELD,LOF)
		IS = STR_COPY (OUTFIELD(:LOF),OUTLIST,LOUT)
		IF (IS.LT.0) GOTO 998
		PTR = PTR+1				!skip separator
	ENDDO
C
C
	DWC_SYMLIST_EXPAND = 1
	RETURN
C
 998	DWC_SYMLIST_EXPAND = MSG_SET (DWC_STRTOOSHO,1)
	CALL WNCTXT(DWLOG,DWMSG,LEN(OUTLIST))
	RETURN
 999	DWC_SYMLIST_EXPAND = MSG_SET (DWC_SYNERRSYM,1)
	CALL WNCTXT(DWLOG,DWMSG,INFIELD(:LIF))
	RETURN
	END
