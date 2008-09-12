C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	GENUN_SYMBOL
C.Keywords:	Symbols
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	UNIX
C.Comments:
C	- Symbols tie a value to a name.
C	- Symbol names are extended-alphanumeric strings (uppercase letters,
C	  digits, dollar sign and underscore) of up to 64 characters. The first
C	  character cannot be a digit. When a symbol name is entered with
C	  lowercase letters, these will be converted to uppercase; trailing
C	  blanks will be ignored.
C	- Symbol values may contain up to 255 ASCII characters. All blanks are
C	  significant including leading and trailing ones. Symbols can have
C	  null values, which are given as '""'.
C	- On the Alliant, all symbols are global and their definitions are kept
C	  in the file defined under the environment variable DWARF_SYMBOLS
C	  The file has to exist.
C	- Each program works with its own copy of the symbol file. Only at
C	  program termination, the master file will be updated.
C
C.Version:	900418 FMO - creation
C.Version:	900502 FMO - new GEN_LUN module
C.Version:	910808 FMO - rewritten, added SYMBOL_SEARCH and SYMBOL_EXIT
C.Version:	910830 FMO - allow for symbol file defined under environment
C			variable DWARF_SYMBOLS
C.Version:	911121 GvD - main part is written in C; this Fortran file
C			is only an interface layer
C.Version:	920214 GvD - no optional arguments in MSG anymore
C		940209 WNB - cleanup entry argument names; split off
C	        940216 CMV - pass back NAME to L2NAM for search
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER FUNCTION SYMBOL_DEFINE (L1NAME,VALUE,L1TYPE)
C		ENTRY    SYMBOL_DELETE (L1NAME,L2TYPE)
C		ENTRY    SYMBOL_GET    (L1NAME,VALUE,LVAL)
C		ENTRY    SYMBOL_SEARCH (INCLIST,EXCLIST,NR,L2NAME,LNAM)
C		ENTRY    SYMBOL_EXIT   ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	L1NAME,L2NAME	!(i,o) name
	CHARACTER*(*)	VALUE		!(i,o) value
	INTEGER		L1TYPE,L2TYPE	!(i) type (not used)
	INTEGER		LVAL		!(o) length of the value
	CHARACTER*(*)	INCLIST		!(i) list with symbol names to include
	CHARACTER*(*)	EXCLIST		!(i) list with symbol names to exclude
	INTEGER		NR		!(m) symbol nr
	INTEGER		LNAM		!(o) length of the name
C
C.Purpose:	Maintain and access symbol table
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	GEN_SUCCESS	also if blank name (no definition)
C	error	GEN_SYMDEFERR	any define/exit error, message left in buffer
C	error	GEN_SYMDELERR	any delete error, message left in buffer
C	error	GEN_SYMGETERR	any get/search error, message left in buffer
C.Notes:
C	- The symbol table is implemented in a C subroutine, which uses a file
C	  defined by the environment variable DWARF_SYMBOLS.
C	  This file is opened, read and closed if a symbol routine is used
C	  for the first time. It is updated (if needed) by SYMBOL_EXIT.
C	  These Fortran routines are merely an interface to the C routines.
C	- Symbol names are converted to uppercase and trailing blanks are
C	  ignored.
C	- Symbol values are stored as given.
C	  Values with zero length will be converted to null values ('""').
C
C	DEFINE
C	- Define a new or redefine an old symbol.
C
C	DELETE
C	- Delete a symbol.
C
C	GET
C	- Get the value of the given symbol.
C	- If the symbol does not exist or an error occurred, a blank value
C	  with length = 0 will be returned.
C	- Otherwise, the length will be positive; null values are returned
C	  as '""' with length = 2.
C	- If the value buffer is too short, the truncated value and an error
C	  code will be returned.
C
C	SEARCH
C	- Find the first or next symbol with a matching name.
C	- INCLIST and EXCLIST are comma-separated lists of symbol names; the
C	  names may contain wildcards (*), each matching a substring of zero or
C	  more characters.
C	- A matching symbol name is a name that matches at least one INCLIST
C	  element and does not match any EXCLIST element. Abbreviated matches
C	  are not recognised.
C	- If NR.le.0 on input, the first matching name in the list of defined
C	  symbols and its number (output NR) will be returned.
C	- If NR.gt.0 on input, the next matching name and its number will be
C	  returned; the search starts at symbol number NR+1.
C	- If there is no matching symbol, NR=0, NAME=blank and LNAM=0 will be
C	  returned.
C	- If the NAME buffer is too short, the truncated name, its length and
C	  an error code will be returned.
C
C	EXIT
C	- Close the symbol facility.
C	- The symbol file is updated if any symbol has been defined or deleted.
C-------------------------------------------------------------------------
C
C
	CHARACTER*(*)	WILD
		PARAMETER (WILD   = '*')
	INTEGER		NAMSIZ, VALSIZ
		PARAMETER (NAMSIZ = 64 )	!max length of names
		PARAMETER (VALSIZ = 255)	!max length of values
C
C					C-functions
C
	INTEGER		SYMBOL_INIT_C, SYMBOL_GET_C, SYMBOL_ADD_C
	INTEGER		SYMBOL_EXIT_C
C
C					Other functions
	INTEGER		SYMBOL_SEAR
	INTEGER		STR_SIGLEN, STR_UPCASE
	INTEGER		MSG_SET  
C
C					Entry-points
	INTEGER		SYMBOL_DELETE, SYMBOL_GET, SYMBOL_SEARCH, SYMBOL_EXIT
C
	INTEGER		TYPE
	CHARACTER	NAM*(NAMSIZ+1), VAL*(VALSIZ)
	CHARACTER	NAME*(NAMSIZ+1)
	INTEGER		LN, LV, OK, IS
	INTEGER		TP	!0=GET   1=DEFINE   2=DELETE   -1=SEARCH
C
C					Define error codes for various entries
	INTEGER ERRNR(-1:2)
	DATA ERRNR /GEN_SYMGETERR, GEN_SYMGETERR, GEN_SYMDEFERR,
	1						GEN_SYMDELERR/
C
C					Define permanent "first time" switch
	LOGICAL FIRST
	DATA    FIRST /.TRUE./
	SAVE    FIRST
C
C
C
C			This is the SYMBOL_DEFINE entry
	NAME=L1NAME
	TP = 1
	TYPE=L1TYPE
	GOTO 100
C
C			This is the SYMBOL_DELETE entry
	ENTRY SYMBOL_DELETE (L1NAME,L2TYPE)
	NAME=L1NAME
	TYPE=L2TYPE
	TP = 2
	GOTO 100
C
C			This is the SYMBOL_GET entry
	ENTRY SYMBOL_GET (L1NAME,VALUE,LVAL)
	NAME=L1NAME//' '
	TP = 0
	GOTO 100
C
C			This is the SYMBOL_SEARCH entry
	ENTRY SYMBOL_SEARCH (INCLIST,EXCLIST,NR,L2NAME,LNAM)
	NAME=L2NAME
	TP = -1
	GOTO 100
C
C
C					Initialize to success
C					On first invocation:
C					- initialize the symbol facility
C
  100	OK = GEN_SUCCESS
	IF (FIRST) THEN
	  OK = SYMBOL_INIT_C ()
	  IF (IAND(OK,1) .NE. 1) THEN
	    CALL WNCTXT(DWLOG,
	1	'Error during initialization of symbol facility')
	    CALL WNCTXT(DWLOG,
	1	'Probably file DWARF_SYMBOLS could not be opened')
	    GOTO 929
	  ENDIF
	  FIRST = .FALSE.
	ENDIF
C
C					Handle the symbol name
C					Test its length and convert to uppercase
C					Add a 0 to the name for the C subroutine
C
	IF (TP.GE.0) THEN
	    LN = STR_SIGLEN (NAME)
	    IF (LN.EQ.0) THEN
		CALL WNCTXT (DWLOG,'Blank name not allowed')
		GOTO 929
	    ENDIF
	    IF (LN.GT.NAMSIZ) THEN
		CALL WNCTXT(DWLOG,'Name too long (!SJ, max length !SJ)',
	1							LN,NAMSIZ)
		GOTO 929
	    ENDIF
	    IF (INDEX(NAME(:LN),WILD).NE.0) THEN
		CALL WNCTXT (DWLOG,'No wildcards allowed in name')
		GOTO 929
	    ENDIF
	    NAM = NAME
	    IS  = STR_UPCASE (NAM)
	    NAM(LN+1:LN+1) = CHAR(0)
C
C
C					Get the symbol value
C					(ignore return status)
C					Generate message if buffer too short
C
	    IF (TP.EQ.0) THEN
		IS    = SYMBOL_GET_C (NAM,LVAL,VAL)
		VALUE = VAL(:LVAL)
		IF (LEN(VALUE).LT.LVAL) THEN
		    LVAL = LEN(VALUE)
		    CALL WNCTXT(DWLOG,
	1		'Value truncated to !SJ characters',LVAL)
		    GOTO 929
		ENDIF
C
C					Define the symbol
C					Error if value too long
C					Define as "" if null value
C
	    ELSEIF (TP.EQ.1) THEN
		LV = LEN(VALUE)
		IF (LV.GT.VALSIZ) THEN
		    CALL WNCTXT(DWLOG,
	1		'Value too long (!SJ, max length !SJ)',
	1						LV,VALSIZ)
		    GOTO 929
		ENDIF
		IF (LV.EQ.0) THEN
		    VAL = '""'
		    LV  = 2
		ELSE
		    VAL = VALUE
		ENDIF
		OK = SYMBOL_ADD_C (LN,NAM,LV,VAL,1)
	    ELSE
C
C					Delete the symbol
C
		OK = SYMBOL_ADD_C (LN,NAM,0,' ',2)
	    ENDIF
C
C					Get the next symbol
C
	ELSE
	    OK = SYMBOL_SEAR (INCLIST,EXCLIST,NR,L2NAME,LNAM)
	ENDIF
	GOTO 990
C
C
C					Generate error message
  929	OK = MSG_SET (ERRNR(TP),1)
	CALL WNCTXT(DWLOG,DWMSG,NAME)
C
  990	SYMBOL_DEFINE = OK
	RETURN
C
C			This is the SYMBOL_EXIT entry-point.
C			Exit if nothing done.
C			Otherwise EXIT the symbol facility properly.
C			Generate a message in case of errors.
	ENTRY SYMBOL_EXIT ()
	OK = GEN_SUCCESS			!Initialize to success
	IF (.NOT.FIRST) THEN
	    OK = SYMBOL_EXIT_C ()
	ENDIF
	IF (IAND(OK,1) .NE. 1) THEN
		OK = MSG_SET (GEN_SYMDEFERR,1)
		CALL WNCTXT(DWLOG,DWMSG,'update')
	END IF
	SYMBOL_DEFINE = OK
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER FUNCTION SYMBOL_SEAR (INCLIST,EXCLIST,NR,NAME,LNAM)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	INCLIST		!(i) list with symbol names to include
	CHARACTER*(*)	EXCLIST		!(i) list with symbol names to exclude
	INTEGER		NR		!(m) symbol nr
	CHARACTER*(*)	NAME		!(o) symbol name
	INTEGER		LNAM		!(o) length of the name
C
C.Purpose:	Find the first or next symbol with a matching name
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	GEN_SUCCESS	also if no matching symbol name does exist
C	error	GEN_SYMGETERR	any error, message left in buffer
C.Notes:
C	- INCLIST and EXCLIST are comma-separated lists of symbol names; the
C	  names may contain wildcards (*), each matching a substring of zero or
C	  more characters.
C	- A matching symbol name is a name that matches at least one INCLIST
C	  element and does not match any EXCLIST element. Abbreviated matches
C	  are not recognised.
C	- If NR.le.0 on input, the first matching name in the list of defined
C	  symbols and its number (output NR) will be returned.
C	- If NR.gt.0 on input, the next matching name and its number will be
C	  returned; the search starts at symbol number NR+1.
C	- If there is no matching symbol, NR=0, NAME=blank and LNAM=0 will be
C	  returned.
C	- If the NAME buffer is too short, the truncated name, its length and
C	  an error code will be returned.
C-------------------------------------------------------------------------
C
C
	CHARACTER*(*)	BLANK, COMMA, WILD
		PARAMETER (BLANK  = ' ')
		PARAMETER (COMMA  = ',')
		PARAMETER (WILD   = '*')
	INTEGER		NAMSIZ
		PARAMETER (NAMSIZ = 64)			!max length of names
C
	INTEGER		SYMBOL_NEXT_C
	INTEGER		STR_SIGLEN, STR_MATCH_L
	INTEGER		MSG_SET
C
	CHARACTER	NAM*(NAMSIZ)
	INTEGER		IS, LN, LINC, LEXC, MATCHNR
C
C
C					Set up
C
	NAME = BLANK					!set blank name
	LNAM = 0					!set zero length
	NAM  = BLANK
	LN   = 0
	LINC = STR_SIGLEN (INCLIST)			!length of INCLIST
	IF (LINC.EQ.0) GOTO 930				!no INCLIST: ready
	LEXC = STR_SIGLEN (EXCLIST)			!length of EXCLIST
C
C					Get the name of the next defined symbol
C					that matches INCLIST but not EXCLIST
C
	DO WHILE (SYMBOL_NEXT_C (NR,NAM,LN) .GT. 0)	!get next symbol name
	    IS = STR_MATCH_L (NAM(:LN),INCLIST(:LINC),MATCHNR)
	    IF (IS.EQ.1) THEN				! matches INCLIST
		IF (LEXC.EQ.0) GOTO 930			! no EXCLIST: break
		IS = STR_MATCH_L (NAM(:LN),EXCLIST(:LEXC),MATCHNR)
		IF (IS.NE.1) GOTO 930			! no match: break
	    ENDIF
	ENDDO
C
C					Wrap up
C
 930	IF (LN.GT.0) THEN				!proper name found:
		NAME = NAM(:LN)				! copy it to NAME
		IF (LEN(NAME).LT.LN) GOTO 938		! buffer too short
		LNAM = LN				! copy name length
	ELSE						!otherwise:
		NAME = BLANK				! return blank NAME
		LNAM = 0				! return zero length
		NR   = 0				! return zero NR
	ENDIF
	SYMBOL_SEAR = GEN_SUCCESS
	RETURN
C
 938	LNAM = LEN(NAME)
	CALL WNCTXT(DWLOG,'Name truncated to !SJ characters',LNAM)
 	SYMBOL_SEAR = MSG_SET (GEN_SYMGETERR,1)
	CALL WNCTXT(DWLOG,DWMSG,NAM(:LN))
	RETURN
	END
