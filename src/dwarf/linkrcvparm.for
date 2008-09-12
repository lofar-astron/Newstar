C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	GEN_LINK_RCVPARM
C.Keywords:	Program Parameters, Get Value, Link
C.Author:	Ger van Diepen (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	Any
C.Comments:	This is a special version of GET_PARM, which can be used
C		to ask parameters on another system using the link.
C.Version:	920101 GvD - creation
C.Version:	920507 GvD - put data on 8-byte boundary
C.Version:	921207 HjV - Some lines to long for HP
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION LINK_RCV_PARM (SD,BUF,BUFC,MAXL)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
	INTEGER*4	SD		! (i) socket descriptor
	BYTE		BUF(*)		! (m) buffer with PARM name and values
	CHARACTER*(*)	BUFC		! (m) idem as string
	INTEGER*4	MAXL		! (i) max. length of BUF
C
C.Purpose:	Get a value set for a program parameter and send it via the link
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
C-------------------------------------------------------------------------
C
C
	INTEGER*4	IS, ST, STD, NRD
	INTEGER*4	NR, NBYT, NRDEF, LEND, LENS, LENDEF, PLEN, FLAGS
	CHARACTER*16	KEYWORD
	CHARACTER*1	DTYPE
	CHARACTER*1023	STROUT
	BYTE		VAL(4096)
	CHARACTER*4096	VALC
	EQUIVALENCE (VAL,VALC)
C
	INTEGER*4	GET_PARM
	INTEGER*4	MSG_SET  , STR_SIGLEN
	INTEGER*4	LINK_WRITE
C
C
C					Get the various values from the buffer.
C					(convert to host byte order)
C
	CALL LINK_NTOHJ (BUF(5)  ,NBYT   ,1)		! size of value area
	CALL LINK_NTOHJ (BUF(9)  ,PLEN   ,1)		! length of data value
	CALL LINK_NTOHJ (BUF(13) ,LEND   ,1)		! length of def. string
	CALL LINK_NTOHJ (BUF(17) ,NRDEF  ,1)		! #defaults
	CALL LINK_NTOHJ (BUF(21) ,FLAGS  ,1)		! flags
	CALL LINK_NTOHJ (BUF(25) ,LENS   ,1)		! length of STROUT
	ST      = 29
	DTYPE   = BUFC(ST:ST)
	KEYWORD = BUFC(ST+LEND+1:ST+LEND+16)
C
C				Get the defaults if they are there.
C				We leave a string in the buffer.
C				Numeric data is converted in the buffer itself,
C				while shifting it 16 bytes to the left because
C				in place conversion is not possible.
C				Note that if NRDEF is undefined, the first
C				byte of the data indicates if a default exists.
C
	STD = ST + LEND + 16			! skip dtype,defstr,keyword
	STD = (1 + (STD-1)/8) * 8		! 8-byte boundary
	STD = STD + 1
	IF (DTYPE.EQ.'C') THEN
	    CALL LINK_NTOHJ (BUF(STD), LENDEF, 1)	! length def. char.value
	    STD = STD + 4
	ELSE
	    NRD = NRDEF
	    IF (NRDEF.EQ.UNDEF_J) THEN
		IF (BUF(STD) .NE. UNDEF_B) THEN
		    NRD = 1				! there is a default
		    STD = STD + 1
		ENDIF
	    ENDIF
	    IF (NRD.GT.0) THEN
		CALL LINK_NTOH (DTYPE, BUF(STD), BUF(STD-16), NRD)
		STD = STD - 16
	    ENDIF
	ENDIF
C
C				Get the parameter value(s).
C
	IF (DTYPE.EQ.'C') THEN
	    IS = GET_PARM (KEYWORD, VALC(:PLEN), NBYT, NR,
     *			BUFC(ST+1:ST+LEND), BUFC(STD:STD+LENDEF-1),
     *			NRDEF, FLAGS, STROUT)
	ELSE
	    IS = GET_PARM (KEYWORD, VAL, NBYT, NR, BUFC(ST+1:ST+LEND),
     *				BUF(STD), NRDEF, FLAGS, STROUT(:LENS))
	ENDIF
C
C				Return them to the user.
C				Test if the buffer is big enough.
C
	CALL LINK_HTONJ (IS  , BUF(5) , 1)
	CALL LINK_HTONJ (NR  , BUF(9) , 1)
	CALL LINK_HTONJ (LENS, BUF(13), 1)
	ST = 16
	IF (NR.LT.0) NR = 0
	LENS = STR_SIGLEN (STROUT(:LENS))
	IF (DTYPE.EQ.'R') PLEN = 2*PLEN
	IF (NR*PLEN + LENS .GT. MAXL) THEN
	    CALL WNCTXT(DWLOG,'Buffer in LINK_RCV_PARM too small')
	    IS = MSG_SET (DWC_GETINPERR,0)
	    NR = 0
	    LENS = 0
	ENDIF
	IF (DTYPE.EQ.'C') THEN
	    BUFC(ST+1:ST+NR*PLEN) = VALC(:NR*PLEN)
	ELSE
	    CALL LINK_HTON (DTYPE, VAL, BUF(ST+1), NR)
	ENDIF
	ST = ST + NR*PLEN
	IF (LENS.GT.0) THEN
	    BUFC(ST+1:ST+LENS) = STROUT(:LENS)
	    ST = ST + LENS
	ENDIF
	IS = LINK_WRITE (SD, BUF, ST)
C
C
 999	LINK_RCV_PARM = IS
	RETURN
	END
