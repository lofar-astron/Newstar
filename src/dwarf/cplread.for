C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	CPL_READ
C.Keywords:	Compiler Utility, Read Source
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	900415 FMO - recreation
C.Version:	920214 GvD - no optional arguments in MSG anymore
C.Version:	930613 HjV - Change size of work-buffer from 2000 to 5000
C.Version:	940829 HjV - Change size of work-buffer from 5000 to 10000
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION CPL_READ (NKEY,KEY,WKEY,SWWORK,NCOGKEY,COGKEY,
	1							NXTKEY)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	NKEY		! (i) nr of valid source keywords
	CHARACTER*(*)	KEY(NKEY)	! (i) valid source keywords
	INTEGER*4	WKEY(NKEY)	! (i) corresponding work indices
	LOGICAL*4	SWWORK		! (i) write into work buffer ?
	INTEGER*4	NCOGKEY		! (i) nr of change-of-group keywords
	CHARACTER*(*)	COGKEY(NCOGKEY)	! (i) change-of-group keywords
	CHARACTER*(*)	NXTKEY		! (o) change-of-group keyword found
C
C.Purpose:	Read a group of source lines into the work buffer
C.Returns:	Status code
C	success	CPL_SUCCESS	regular end of group
C	info	CPL_SRCEOF	regular end of file
C	error	CPL_FLDUNEXP	unexpected field
C	error	CPL_EOFUNEXP	unexpected end of file
C	false	status codes returned by referenced routines
C.Notes:
C-------------------------------------------------------------------------
C
	INTEGER*4	CPL_WRK_INIT, CPL_WRK_PUTLNR, CPL_WRK_PUTVAL
	INTEGER*4	CPL_SRC_GETKEY, CPL_SRC_GETVAL, CPL_SRC_BACKSP
	INTEGER*4	CPL_ERR_PUT
	INTEGER		STR_MATCH_A, MSG_SET
C
	CHARACTER	KEYWORD*80, VALUE*10000
	INTEGER*4	LENKEY, LENVAL
	INTEGER*4	IS, LINENR, LASTNR, MATCH, FIELDNR
	LOGICAL*4	IN_GROUP
		SAVE IN_GROUP
C
C
C					Initialize the work buffers
C
	IN_GROUP = .FALSE.
	IF (SWWORK) THEN
		IS = CPL_WRK_INIT ()
		IF (IAND(IS,1).EQ.0) GOTO 999
	ENDIF
C
C					Extract the next keyword
C
 100	IS = CPL_SRC_GETKEY (KEYWORD,LENKEY,LINENR)
	IF (IS.EQ.CPL_SRCEOF) GOTO 999				! end file
	IF (IAND(IS,1).EQ.0) GOTO 999					! read error
C
C					Check the keyword
C
	IF (IN_GROUP) THEN
		IS = STR_MATCH_A (KEYWORD(:LENKEY),NCOGKEY,COGKEY,MATCH)
		IF (MATCH.GT.0) THEN				! end of group
			IS = CPL_SRC_BACKSP ()
			NXTKEY = COGKEY(MATCH)
			GOTO 999
		ENDIF
		IS = STR_MATCH_A (KEYWORD(:LENKEY),NKEY,KEY,MATCH)
	ELSE
		IS = STR_MATCH_A (KEYWORD(:LENKEY),NKEY,KEY,MATCH)
		IN_GROUP = MATCH.GT.0 .AND. WKEY(MATCH).EQ.1
		IF (.NOT.IN_GROUP) THEN				! wrong start
			IS = MSG_SET (CPL_FLDUNEXP,1)
			CALL WNCTXT(DWLOG,DWMSG,LINENR)
			GOTO 999
		ENDIF
	ENDIF
C
	IF (SWWORK) THEN
		IF (MATCH.GT.0) THEN
			IF (IS.NE.0) THEN
				FIELDNR = WKEY(MATCH)
			ELSE
				IS = CPL_ERR_PUT (CPL_FLDNOTUNI,LINENR)
				IF (IAND(IS,1).EQ.0) GOTO 999
			ENDIF
		ELSE
			IS = CPL_ERR_PUT (CPL_FLDINVAL,LINENR)
			IF (IAND(IS,1).EQ.0) GOTO 999
			FIELDNR = 0
		ENDIF
	ENDIF
C
C					Extract the value
C
	IS = CPL_SRC_GETVAL (VALUE,LENVAL,LASTNR)
	IF (IS.EQ.CPL_SRCEOF) IS = MSG_SET (CPL_EOFUNEXP,0)	! wrong EOF
	IF (IAND(IS,1).EQ.0) GOTO 999					! or read error
C
C					Store the nr of last source line read
C					and the linenr and value of the field
C
	IF (SWWORK) THEN
		IS = CPL_WRK_PUTLNR (0,LASTNR)
		IF (IAND(IS,1).NE.0 .AND. FIELDNR.GT.0) THEN
			IS = CPL_WRK_PUTLNR (FIELDNR,LINENR)
			IF (IAND(IS,1).NE.0 .AND. LENVAL.GT.0) THEN
				IS = CPL_WRK_PUTVAL (FIELDNR,VALUE(:LENVAL))
			ENDIF
		ENDIF
		IF (IAND(IS,1).EQ.0) GOTO 999
	ENDIF
C
C				End of entry: goto next entry
C
	GOTO 100
C
C				Return
C
 999	CPL_READ = IS
	RETURN
	END
