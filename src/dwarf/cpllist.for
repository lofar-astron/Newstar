C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	CPL_LIST
C.Keywords:	Compiler Utility, List
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C.Version:	900415 FMO - recreation
C.Version:	900920 FMO - only open print file for DO_LIST=.TRUE.
C.Version:	920213 GvD - no optional arguments anymore
C.Version:	940118 CMV - used WNCFOP, WNCALN i.s.o. DWARF stuff
C.Version:      940217 HjV - Change WNCFGC in WNCFGV
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION CPL_LIST (LISTNAME,DO_LIST,PRTFLAGS)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	LISTNAME	! (i) list file specification
	LOGICAL*4	DO_LIST		! (i) create compilation listing ?
	INTEGER*4	PRTFLAGS	! (i) disposition flags
C
C.Purpose:	Create output listing for the various compilers
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	CPL_SUCCESS
C	error	CPL_CLISTERR	error report left in message buffer
C.Notes:
C	The source lines are merged with any error messages generated.
C	The error messages and the compilation summary will also be displayed
C	on the user terminal.						
C-------------------------------------------------------------------------
C
	LOGICAL    L__FALSE
	PARAMETER (L__FALSE = .FALSE.)
C
	CHARACTER*(*)	SUMTXT, SRCTXT, WARTXT, ERRTXT, CRETXT, DELTXT
		PARAMETER (SUMTXT = '!/compilation summary :')
		PARAMETER (SRCTXT = '!/  source file                   : !AS')
		PARAMETER (WARTXT =   '  total number of warnings      : !SL')
		PARAMETER (ERRTXT =   '  total number of severe errors : !SL')
		PARAMETER (CRETXT = '!/!AS created')
		PARAMETER (DELTXT = '!/no object file created')
C
	INTEGER*4	CPL_SRC_REWIND, CPL_SRC_GETLINE, CPL_SRC_INQUIRE
	INTEGER*4	CPL_ERR_SORT, CPL_ERR_GETMSG, CPL_ERR_GETSUM
	INTEGER*4	CPL_OBJ_INQUIRE
	INTEGER		MSG_SET
C
	CHARACTER	SRCNAME*64, OBJNAME*64, SRCLINE*255, PRTLINE*132
	INTEGER*4	FLAGS, LSN, LON, LSL, LPL, NLL, NPL, NHC
	INTEGER*4	IS, LINENR, NERR, NWARN
	INTEGER*4	LISTID /0/, LISTLCNT, LISTPLEN
C
C
C					Set up
C
	IS = CPL_ERR_SORT ()
	IF (IAND(IS,1).NE.0) IS = CPL_SRC_REWIND ()
	IF (IAND(IS,1).EQ.0) GOTO 999
	IF (DO_LIST) THEN
		LISTID=-1
		CALL WNCFOP(LISTID,LISTNAME)
		IF (LISTID.EQ.-1) GOTO 999
		CALL WNCFHD(LISTID,1,'!50C Compilation listing')
		CALL WNCFSV(LISTID,F_DIS,PRTFLAGS)
	ENDIF
C
C					Read next source line
C
 100	IS = CPL_SRC_GETLINE (SRCLINE,LSL,LINENR)
	IF (IS.EQ.CPL_SRCEOF) GOTO 900
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Write to print file
C
	IF (DO_LIST) CALL WNCTXT(LISTID,'!4$ZJ    !AS',
	1	LINENR,SRCLINE(:LSL))
C
C					If error(s) detected in source line:
C					- write source line to terminal
C					- write messages to file and terminal
C
	IS = CPL_ERR_GETMSG (LINENR,PRTLINE,LPL)
	IF (IAND(IS,1).NE.0 .AND. LPL.GT.0) THEN
	   CALL WNCTXT(F_T,'!/!4$ZJ    !AS',LINENR,SRCLINE(:LSL))
	   DO WHILE (IAND(IS,1).NE.0 .AND. LPL.GT.0)
		IF (DO_LIST) THEN
		   CALL WNCTXT(F_T+LISTID,'!AS',PRTLINE(:LPL))
		ELSE
		   CALL WNCTXT(F_T,'!AS',PRTLINE(:LPL))
		END IF
		IF (IAND(IS,1).NE.0) 
	1		IS = CPL_ERR_GETMSG (LINENR,PRTLINE,LPL)
	   ENDDO
	ENDIF
	IF (IAND(IS,1).EQ.0) GOTO 999
	GOTO 100
C
C					Get summary information
C
 900	IS = CPL_ERR_GETSUM (NERR,NWARN)
	IF (IAND(IS,1).NE.0) IS = CPL_SRC_INQUIRE (SRCNAME,LSN)
	IF (IAND(IS,1).NE.0) IS = CPL_OBJ_INQUIRE (OBJNAME,LON)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C					Write summary to file and terminal
C
	IF (DO_LIST) THEN
		CALL WNCFGV(LISTID,F_LC,LISTLCNT)
		CALL WNCFGV(LISTID,F_PL,LISTPLEN)
		IF (LISTLCNT+8.GT.LISTPLEN) CALL WNCTXT(LISTID,'!^')
		CALL WNCTXT(F_T+LISTID,SUMTXT)
		CALL WNCTXT(F_T+LISTID,SRCTXT,SRCNAME(:LSN))
		CALL WNCTXT(F_T+LISTID,WARTXT,NWARN)
		CALL WNCTXT(F_T+LISTID,ERRTXT,NERR)
		IF (NERR.EQ.0) THEN
		   CALL WNCTXT(F_T+LISTID,CRETXT,OBJNAME(:LON))
		ELSE
		   CALL WNCTXT(F_T+LISTID,DELTXT)
		ENDIF
		CALL WNCFCL(LISTID)
C
C					Write summary to terminal
C
	ELSE
		CALL WNCTXT(F_T,SUMTXT)
		CALL WNCTXT(F_T,SRCTXT,SRCNAME(:LSN))
		CALL WNCTXT(F_T,WARTXT,NWARN)
		CALL WNCTXT(F_T,ERRTXT,NERR)
		IF (NERR.EQ.0) THEN
		   CALL WNCTXT(F_T,CRETXT,OBJNAME(:LON))
		ELSE
		   CALL WNCTXT(F_T,DELTXT)
		ENDIF
	ENDIF
C
C					Return
C
	CPL_LIST = CPL_SUCCESS
	RETURN
C
 999	CPL_LIST = MSG_SET (CPL_CLISTERR,0)
	RETURN
	END
