C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	PPD_INDEX
C.Keywords:	PPD File, Index
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		Common variables used:
C	INTEGER*4	PPS$INXB	! (r) address of index in PPD file
C	INTEGER*4	PPS$NRINX	! (r) nr of index entries
C	INTEGER*4	PPS$NRINXPR	! (m) nr of last selected index entry
C
C.Version:	900415 FMO - recreation
C.Version:	920224 GvD - no optional arguments in MSG anymore
C.Version:	940120 CMV - use indirect addressing (A_B)
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_INDEX_GETP (PNAM,PDOFF)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	PNAM		! (i) program's parameter name
	INTEGER*4	PDOFF		! (o) offset of parameter description
C
C.Purpose:	Get the location of the parameter description
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	info	PPD_ENDOFFILE	no parameters (only for blank PNAM)
C	error	PPD_SEQERROR	no PPD file open
C	error	PPD_KEYNOTFND	unknown parameter name
C.Notes:
C	- A blank PNAM indicates the first normal parameter in the index.
C	- The index number of the parameter will be saved in PPS$NRINXPR.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDSTAT_2_DEF'
	INCLUDE 'PPDREC_4_DEF'
C
	INTEGER*4	MOVE_BLJ, STR_SIGLEN, MSG_SET  
C
	INTEGER*4	IS, LNAM, ADDR, NR
	LOGICAL*4	FOUND
C
C
C					Make sure that a PPD file is open
C
	IF (PPS$INXB.EQ.0) THEN
		IS = PPD_SEQERROR
		GOTO 999
	ENDIF
C
C					Loop through the index
C					- skip prototype entries
C					- stop when the proper entry is found
C
	LNAM = STR_SIGLEN (PNAM)
	ADDR = PPS$INXB+1
	NR = 0
	FOUND = .FALSE.
	DO WHILE (.NOT.FOUND .AND. NR.LT.PPS$NRINX)
		IS = MOVE_BLJ (A_B(ADDR-A_OB),PPDID_,PPDID__LENGTH)
		NR = NR+1
		ADDR = ADDR + PPDID__LENGTH*4
		IF (PPDID$UNAM(1:1).NE.'$') THEN
			FOUND = LNAM.EQ.0 .OR. PNAM.EQ.PPDID$PNAM
		ENDIF
	ENDDO
C
	IF (.NOT.FOUND) THEN
		IF (LNAM.GT.0) THEN
			IS = PPD_KEYNOTFND
		ELSE
			IS = PPD_ENDOFFILE
		ENDIF
		GOTO 999
	ENDIF
C
C					Fill the output argument
C					and save the index pointer
C
	PDOFF = PPDID$PARMOFF
	PPS$NRINXPR = NR
	PPD_INDEX_GETP = PPD_SUCCESS
	RETURN
C
 999	PPD_INDEX_GETP = MSG_SET (IS,0)
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_INDEX_GETU (UNAM,PDOFF)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	UNAM		! (m) user's parameter name
	INTEGER*4	PDOFF		! (o) offset of parameter description
C
C.Purpose:	Get the location of the prototype/parameter description
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_SEQERROR	no PPD file open
C	error	PPD_KEYNOTFND	unknown parameter name
C	error	PPD_KEYAMBIG	ambiguously abbreviated name
C	error	PPD_STRTOOSML	UNAM not long enough for the full name
C.Notes:
C	- The complete user's name will be returned in UNAM.
C	- The index pointer PPS$NRINXPR will be set to 0.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDSTAT_2_DEF'
	INCLUDE 'PPDREC_4_DEF'
C
	INTEGER*4	MAXNSYN
		PARAMETER (MAXNSYN = 16)
C
	INTEGER*4	MOVE_BLJ, STR_SIGLEN, MSG_SET  
C
	CHARACTER*16	UNAMSYN(MAXNSYN)
	INTEGER*4	IS, ADDR, LU, NSYN, NR
	LOGICAL*4	FOUND
C
C
C					Make sure that a PPD file is open
C
	IF (PPS$INXB.EQ.0) THEN
		IS = PPD_SEQERROR
		GOTO 999
	ENDIF
C
C					Loop through the index
C					- stop when a unique match is found
C
	NR = 0
	ADDR = PPS$INXB+1
	LU = STR_SIGLEN (UNAM)
	NSYN = 0
	FOUND = .FALSE.
	DO WHILE (.NOT.FOUND .AND. NR.LT.PPS$NRINX)
		IS = MOVE_BLJ (A_B(ADDR-A_OB),PPDID_,PPDID__LENGTH)
		NR = NR+1
		ADDR = ADDR + PPDID__LENGTH*4
		IF (UNAM(:LU).EQ.PPDID$UNAM(:LU)) THEN
			IF (LU.GE.PPDID$LUNAM) THEN
				FOUND = .TRUE.
			ELSE IF (NSYN.LT.MAXNSYN) THEN
				NSYN = NSYN+1
				UNAMSYN(NSYN) = PPDID$UNAM
			ENDIF
		ENDIF
	ENDDO
C
	IF (.NOT.FOUND) THEN
		IF (NSYN.EQ.0) THEN
			IS = PPD_KEYNOTFND
		ELSE
			DO I = 1,NSYN
				CALL WNCTXT(DWLOG,'!AS',UNAMSYN(I))
			ENDDO
			IS = PPD_KEYAMBIG
		ENDIF
		GOTO 999
	ENDIF
C
	LU = STR_SIGLEN (PPDID$UNAM)
	IF (LU.GT.LEN(UNAM)) THEN
		IS = PPD_STRTOOSML
		GOTO 999
	ENDIF
C
C					Fill the output arguments
C					and clear the index pointer
C
	UNAM = PPDID$UNAM(:LU)
	PDOFF = PPDID$PARMOFF
	PPS$NRINXPR = 0
	PPD_INDEX_GETU = PPD_SUCCESS
	RETURN
C
 999	PPD_INDEX_GETU = MSG_SET (IS,1)
	CALL WNCTXT(DWLOG,DWMSG,UNAM)
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION PPD_INDEX_GETNXT (PDOFF)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	PDOFF		! (o) offset of parameter description
C
C.Purpose:	Get the location of the description of the next parameter
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	info	PPD_ENDOFFILE	no more normal parameters
C	error	PPD_SEQERROR	no PPD file open
C.Notes:
C	- If called after GETP, the next normal parameter in the index will be
C	  taken. Otherwise, the first one will be taken.
C	- The index number of the parameter will be saved in PPS$NRINXPR.
C-------------------------------------------------------------------------
C
	INCLUDE 'PPDSTAT_2_DEF'
	INCLUDE 'PPDREC_4_DEF'
C
	INTEGER*4	MOVE_BLJ, MSG_SET  
C
	INTEGER*4	IS, ADDR, NR
	LOGICAL*4	FOUND
C
C
C					Make sure that a PPD file is open
C
	IF (PPS$INXB.EQ.0) THEN
		IS = PPD_SEQERROR
		GOTO 999
	ENDIF
C
C					Get the next normal index entry
C					- skip prototype entries
C
	NR = PPS$NRINXPR
	ADDR = PPS$INXB + NR*PPDID__LENGTH*4 + 1
	FOUND = .FALSE.
	DO WHILE (.NOT.FOUND .AND. NR.LT.PPS$NRINX)
		IS = MOVE_BLJ (A_B(ADDR-A_OB),PPDID_,PPDID__LENGTH)
		NR = NR+1
		ADDR = ADDR + PPDID__LENGTH*4
		FOUND = PPDID$UNAM(1:1).NE.'$'
	ENDDO
C
	IF (.NOT.FOUND) THEN
		IS = PPD_ENDOFFILE
		GOTO 999
	ENDIF
C
C					Fill the output argument
C					and update the index pointer
C
	PDOFF = PPDID$PARMOFF
	PPS$NRINXPR = NR
	PPD_INDEX_GETNXT = PPD_SUCCESS
	RETURN
C
 999	PPD_INDEX_GETNXT = MSG_SET (IS,0)
	RETURN
	END
