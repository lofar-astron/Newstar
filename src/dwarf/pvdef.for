C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	PV_DEF
C.Keywords:	Program Parameters, Default Values
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	900416 FMO - recreation
C.Version:	920214 GvD - no optional arguments in MSG anymore
C.Version:	920508 GvD - do not allow TOBY for logical values
C.Version:	940117 CMV - use WNGARA i.s.o. GEN_ADDRESS
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PV_DEF ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Make source module name known
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C.Notes:
C-------------------------------------------------------------------------
C
C
	PV_DEF = DWC_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PV_DEF_GET (SYMBOL,DEFSTR,LDEF,DEFTYP,LTYP)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	SYMBOL		! (i) symbol name
	CHARACTER*(*)	DEFSTR		! (o) default value
	INTEGER*4	LDEF		! (o) significant length of DEFSTR
	CHARACTER*(*)	DEFTYP		! (o) type of default value
	INTEGER*4	LTYP		! (o) significant length of DEFTYP
C
C.Purpose:	Get the default value sets for a program parameter
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	false status codes returned by referenced routines
C.Notes:
C	- SYMBOL is the full parameter name (prognam$stream_keyword).
C	- The PPD search-sequence string determines what kind of defaults are
C	  allowed (local, group, program) and these are looked for in the order:
C	  stream-specific local	in DWARF symbol	<prog>$<stream>_<key>
C	  all-stream local		"	<prog>$0_<key>
C	  stream-specific group		"	<group>$<stream>_<key>
C	  all-stream group		"	<group>$0_<key>
C	  program			in PPD file
C	- DEFTYP tells where the default came from:
C	  'local', 'local$0', <group>, <group>//'$0', 'program' or <blank>,
C	  where <group> denotes the group name (uppercase).
C	- LTYP gives the corresponding length (=0 for <blank>). If DEFTYP is
C	  too short the type string will be truncated (considered successfull).
C	- If a program default is allowed, type 'program' will be returned
C	  even if there is no PPD default, because the caller can provide a
C	  default in the argument list of GET_PARM.
C	- If no default is found, LDEF = 0 will be returned.
C-------------------------------------------------------------------------
C
C
	CHARACTER*(*)	BLANK
		PARAMETER (BLANK = ' ')
C
	INTEGER*4	DWC_SYM_SPLIT, DWC_SYM_BUILD, DWC_STREAM_GET
	INTEGER*4	PPD_SSTR_GET, PPD_DVSTR_GET
	INTEGER*4	STR_SIGLEN, SYMBOL_GET
C
	CHARACTER*16	PROG, STREAM, KEY, ALLSTREAM, SSTR, GROUP, WORK*50
	INTEGER*4	IS, LP, LS, LK, LA, LSSTR, LG, LW
C
C
	DEFSTR = BLANK
	LDEF = 0
C
C					Set up:
C					- split symbol name in its components
C					- get the all-stream name
C					- get the default-search procedure
C					  and the possible group name
C
	IS = DWC_SYM_SPLIT (SYMBOL,PROG,LP,STREAM,LS,KEY,LK)
	IF (IAND(IS,1).NE.0) IS = DWC_STREAM_GET (ALLSTREAM,LA,.TRUE.)
	IF (IAND(IS,1).NE.0) IS = PPD_SSTR_GET (SSTR,LSSTR,GROUP,LG)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Stream-specific local default ?
C
	IF (INDEX(SSTR(:LSSTR),'L').NE.0) THEN
	    IS = SYMBOL_GET (SYMBOL,DEFSTR,LDEF)
	    IF (IAND(IS,1).EQ.0) GOTO 999
	    IF (LDEF.GT.0) THEN
		DEFTYP = 'local'
		GOTO 900					! ready
C
C					All-stream local default ?
C
	    ELSE IF (STREAM(:LS).NE.ALLSTREAM(:LA)) THEN
		IS = DWC_SYM_BUILD (PROG(:LP),ALLSTREAM(:LA),KEY(:LK),WORK,LW)
		IF (IAND(IS,1).NE.0) IS = SYMBOL_GET (WORK(:LW),DEFSTR,LDEF)
		IF (IAND(IS,1).EQ.0) GOTO 999
		IF (LDEF.GT.0) THEN
		    DEFTYP = 'local'//ALLSTREAM(:LA)
		    GOTO 900					! ready
		ENDIF
	    ENDIF
	ENDIF
C
C					Stream-specific group default ?
C
	IF (INDEX(SSTR(:LSSTR),'G').NE.0) THEN
	    IS = DWC_SYM_BUILD (GROUP(:LG),STREAM(:LS),KEY(:LK),WORK,LW)
	    IF (IAND(IS,1).NE.0) IS = SYMBOL_GET (WORK(:LW),DEFSTR,LDEF)
	    IF (IAND(IS,1).EQ.0) GOTO 999
	    IF (LDEF.GT.0) THEN
		DEFTYP = GROUP(:LG)
		GOTO 900					! ready
C
C					All-stream local default ?
C
	    ELSE IF (STREAM(:LS).NE.ALLSTREAM(:LA)) THEN
		IS = DWC_SYM_BUILD (GROUP(:LG),ALLSTREAM(:LA),KEY(:LK),WORK,LW)
		IF (IAND(IS,1).NE.0) IS = SYMBOL_GET (WORK(:LW),DEFSTR,LDEF)
		IF (IAND(IS,1).EQ.0) GOTO 999
		IF (LDEF.GT.0) THEN
		    DEFTYP = GROUP(:LG)//ALLSTREAM(:LA)
		    GOTO 900					! ready
		ENDIF
	    ENDIF
	ENDIF
C
C					Program default allowed ?
C					- look for a PPD default
C
	IF (INDEX(SSTR(:LSSTR),'P').NE.0) THEN
		DEFTYP = 'program'
		IS = PPD_DVSTR_GET (DEFSTR,LDEF)
		IF (IAND(IS,1).EQ.0) GOTO 999
		GOTO 900					! ready
	ENDIF
C
C					So, no local or group default found
C					and no program default allowed
C
	DEFTYP = BLANK
	LTYP = 0
	PV_DEF_GET = DWC_SUCCESS
	RETURN
C
 900	LTYP = STR_SIGLEN (DEFTYP)
	PV_DEF_GET = DWC_SUCCESS
	RETURN
C
 999	PV_DEF_GET = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PV_DEF_DECODE (DEFAULT,LDEF,VALBLK)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	DEFAULT		! (i) value string
	INTEGER*4	LDEF		! (i) significant length of DEFAULT
	INTEGER*4	VALBLK(8)	! (o) value block descriptor
C
C.Purpose:	Convert the default string to a value block
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	error	DWC_PARWRDEF	error in default
C.Notes:
C	- There can be no unknown symbols, help request or qualifiers.
C	- Allocate virtual memory for the value block (filled by PV_BLK_DECODE).
C-------------------------------------------------------------------------
C
C
	INTEGER*4	DWC_STREAM_GET
	INTEGER*4	PV_BLK_ALLOC, PV_BLK_DECODE, PV_BLK_RELEASE
	INTEGER*4	MSG_SET  
C
	CHARACTER	STREAM*16
	INTEGER*4	IS, LS
	LOGICAL*4	SWSYM
	BYTE		DEFARR(1)			! dummy
C
C
C					Allocate memory for the value block
C
	IS = PV_BLK_ALLOC (DEFAULT(:LDEF),VALBLK)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (VALBLK(2).EQ.0) GOTO 999
C
C					Convert value string to value block
C					- no unknown symbols allowed
C					- release value block in case of error
C
	SWSYM = .FALSE.
	IS = DWC_STREAM_GET (STREAM,LS,.FALSE.)
	IF (IAND(IS,1).EQ.0) GOTO 999
	IS = PV_BLK_DECODE (DEFAULT(:LDEF),VALBLK,STREAM(:LS),
	1					.FALSE.,SWSYM,.TRUE.,DEFARR,0)
	IF (IAND(IS,1).EQ.0) THEN
		IS = PV_BLK_RELEASE (VALBLK)
		GOTO 999
	ENDIF
C
C
	PV_DEF_DECODE = DWC_SUCCESS
	RETURN
C
 999	PV_DEF_DECODE = MSG_SET (DWC_PARWRDEF,1)
	CALL WNCTXT(DWLOG,DWMSG,' ')
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PV_DEF_ENCODE (DEFARR,NRDEF,LDARR,FLAGS,
	1						VALSTR,LVAL)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	BYTE		DEFARR(*)	! (i) default value (given as an array)
	INTEGER*4	NRDEF		! (i) nr of elements in DEFARR
	INTEGER*4	LDARR		! (i) length of DEFARR elements
	INTEGER*4	FLAGS		! (i) flags to control GET_PARM
	CHARACTER*(*)	VALSTR		! (o) default as standard value string
	INTEGER*4	LVAL		! (o) significant length of VALSTR
C
C.Purpose:	Convert a default array to a standard default string
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	DWC_SUCCESS
C	fatal	DWC_TBNOTALL	TOBY format is not allowed
C	fatal	DWC_TBNOMULT	TOBY format, but NRDEF is not a multiple of 3
C	fatal	DWC_TOOMANSET	DEFARR contains too many sets
C	false status codes returned by referenced modules
C.Notes:
C	- The data type of the array is determined by the PPD file.
C	- If the flag PARM__TOBY is given, the array is assumed to be in TOBY
C	  format (triplets: start, end, increment) in which case NRDEF must
C	  be a multiple of 3.
C-------------------------------------------------------------------------
C
C
	INTEGER*4	MAXSET
		PARAMETER (MAXSET = 25)
C
	INTEGER*4	PV_BLK_ENCODE
	INTEGER*4	PPD_DTYPE_GET, PPD_NVAL_GET, PPD_AMAS_GET
	INTEGER*4	MSG_SET  , WNGARA
C
	CHARACTER	DTYPE*1
	INTEGER*4	IS, PLEN, NRVPS, MNVPS, MXVPS
	INTEGER*4	NRSETS, NRVAL(MAXSET), VALBLK(8)
	LOGICAL*4	SWTOBY
C
C
C					Get the size of the value and the
C					required number of values per set
C
	IS = PPD_DTYPE_GET (DTYPE,PLEN)
	IF (IAND(IS,1).NE.0) IS = PPD_NVAL_GET (NRVPS,MNVPS,MXVPS)
	IF (IAND(IS,1).EQ.0) GOTO 999				! shouldn't occur
C
C					TOBY format given and allowed ?
C					if so: is NRDEF a multiple of 3 ?
C
	SWTOBY = IAND(FLAGS,PARM__TOBY).NE.0
	IF (SWTOBY) THEN
		IF (DTYPE.EQ.'C' .OR. DTYPE.EQ.'L'
	1		.OR. IAND(PPD_AMAS_GET('VECTOR'),1).NE.0) GOTO 991
		IF (MOD(NRDEF,3).NE.0) GOTO 992
	ENDIF
C
C					Determine the nr of sets
C					and the nr of values per set
C
	IF (NRDEF.EQ.UNDEF_J) THEN		! 1-element array
		NRVPS = 1
		NRSETS = 1
		NRVAL(1) = 1
	ELSE IF (NRDEF.LE.0) THEN		! "null" or "wild":
		NRSETS = 1			! can only be 1 set
		NRVAL(1) = NRDEF
	ELSE IF (NRVPS.EQ.1) THEN		! normal scalar:
		NRVPS = NRDEF			! put all values
		NRSETS = 1			!  in one set
		NRVAL(1) = NRDEF
	ELSE					! normal vector
		IF (SWTOBY) NRVPS = NRVPS*3	!  or array
		NRSETS = 1+(NRDEF-1)/NRVPS
		IF (NRSETS.GT.MAXSET) GOTO 993
		DO I = 1,NRSETS-1		! assume full sets
			NRVAL(I) = NRVPS	!  except for last one
		ENDDO				!   (rest of values)
		NRVAL(NRSETS) = NRDEF-(NRSETS-1)*NRVPS
	ENDIF
C
C					Convert the array to a string
C
	VALBLK(1) = 0				! dummy length
	VALBLK(2) = WNGARA(NRVAL)		! addr nrval/set array
	VALBLK(3) = WNGARA(DEFARR)		! addr value array
	VALBLK(4) = 0				! dummy addr
	VALBLK(5) = NRSETS			! nr sets
	VALBLK(6) = NRVPS			! nr reserved vals/set
	VALBLK(7) = LDARR			! nr bytes/value
	VALBLK(8) = 0				! flags (TOBY form ?)
	IF (SWTOBY) VALBLK(8) = 2
	IS = PV_BLK_ENCODE (VALBLK,VALSTR,LVAL)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C
	PV_DEF_ENCODE = DWC_SUCCESS
	RETURN
C
 991	PV_DEF_ENCODE = MSG_SET (DWC_TBNOTALL,0)
	RETURN
 992	PV_DEF_ENCODE = MSG_SET (DWC_TBNOMULT,0)
	RETURN
 993	PV_DEF_ENCODE = MSG_SET (DWC_TOOMANSET,1)
	CALL WNCTXT(DWLOG,DWMSG,NRSETS,MAXSET)
	RETURN
C
 999	PV_DEF_ENCODE = IS
	RETURN
	END
