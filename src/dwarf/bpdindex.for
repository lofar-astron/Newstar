C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	BPD_INDEX
C.Keywords:	PPD File, Build, Index Buffer
C.Author:	Friso Olnon (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	VAX or Alliant
C.Comments:
C		Common variables used:
C	INTEGER*4	BPD$INDEX(1)	! (r) extend size in bytes
C	INTEGER*4	BPD$INDEX(2)	! (m) current size in bytes
C	INTEGER*4	BPD$INDEX(3)	! (m) current address
C	INTEGER*4	BPD$INDEX(4)	! (m) nr of bytes written
C	INTEGER*4	BPD$INDEX(5)	! (m) nr of index entries written
C	INTEGER*4	BPD$INDEX(6)	! (-) offset of last-written entry
C
C.Version:	900415 FMO - recreation
C.Version:	920224 GvD - no optional arguments in MSG anymore
C.Version:	940120 CMV - use indirect addressing (A_B)
C-------------------------------------------------------------------------
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION BPD_INDEX_INIT ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Initialize the index buffer
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'BLDPPD_2_DEF'
C
	INTEGER*4	CLEAR_BLJ
C
	INTEGER*4	IS
C
C
	IS = CLEAR_BLJ (BPD$INDEX(2),5)
C
	BPD_INDEX_INIT = PPD_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION BPD_INDEX_PUT (PDOFF)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	PDOFF		! (i) offset of proto/parm description
C
C.Purpose:	Add an entry to the index buffer
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	false status returned by referenced routines
C.Notes:
C	PDOFF is the offset of the corresponding prototype or parameter in
C	the description buffer.
C-------------------------------------------------------------------------
C
	INCLUDE 'BLDPPD_2_DEF'
	INCLUDE 'PPDREC_4_DEF'
C
	INTEGER*4	PPD_UNAM_GET, PPD_PNAM_GET
	INTEGER*4	CPL_DYN_PUT
C
	INTEGER*4	IS, LN, LMIN
	LOGICAL*4	PROTOTYPE
C
C
C					Build the index entry
C
	IS = PPD_PNAM_GET (PPDID$PNAM,LN)
	IF (IAND(IS,1).NE.0)
	1		IS = PPD_UNAM_GET (PPDID$UNAM,LN,LMIN,PROTOTYPE)
	IF (IAND(IS,1).EQ.0) GOTO 999
	PPDID$LUNAM = 0
	PPDID$PARMOFF = PDOFF
C
C					Add the entry to the index buffer
C
	IS = CPL_DYN_PUT (PPDID__LENGTH*4,PPDID_,BPD$INDEX)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C
	BPD_INDEX_PUT = PPD_SUCCESS
	RETURN
C
 999	BPD_INDEX_PUT = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION BPD_INDEX_SORT ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Sort the index on ascending program's parameter name
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCESS
C.Notes:
C	The sort algorithm is "quickersort" as described in
C	"Communications of the ACM" nr 271.
C-------------------------------------------------------------------------
C
	INCLUDE 'BLDPPD_2_DEF'
	INCLUDE 'PPDREC_4_DEF'
C
	INTEGER*4	MOVE_BLJ
C
	CHARACTER*16	XPNAM, YPNAM
	INTEGER*4	XENT(PPDID__LENGTH), YENT(PPDID__LENGTH)
		EQUIVALENCE (XPNAM,XENT(PPDID_PNAM))
		EQUIVALENCE (YPNAM,YENT(PPDID_PNAM))
	INTEGER*4	IS, SS, SE, SM, X, Y, YH
	INTEGER*4	SNR, SAVSS(32), SAVSE(32)
C
	INTEGER*4	ADDR, N
		ADDR(N) = BPD$INDEX(3)+(N-1)*PPDID__LENGTH*4
C
C
	SS = 1						! segment start
	SE = BPD$INDEX(5)				! segment end
	SNR = 1						! segment number
C
	DO WHILE (SNR.GT.0)
C
C					Segment with more than 2 entries:
C					- split into smaller segments
C
	    IF (SE-SS.GT.1) THEN
C
C					Choose the middle entry as reference
C					- move it into the local array PPDID_
C					- put the first entry in its place
C
		SM = (SS+SE)/2
		IS = MOVE_BLJ (A_B(ADDR(SM)-A_OB),
	1		PPDID_,PPDID__LENGTH)
		IS = MOVE_BLJ (A_B(ADDR(SS)-A_OB),
	1		A_B(ADDR(SM)-A_OB),PPDID__LENGTH)
		Y = SE
C
C					Shuffle, so that all entries preceding
C					the reference <= the reference
C					and all others >= the reference
C					- find the first entry > PPDID_
C					- if found: find the last entry < PPDID_
C					- if found: swap the entries
C					  and look for the next "swap" pair
C
		DO X = SS+1,Y
		 IF (SS+1.LE.Y) THEN
		  IS = MOVE_BLJ (A_B(ADDR(X)-A_OB),XENT,PPDID__LENGTH)
		  IF (XPNAM.GT.PPDID$PNAM) THEN
			DO Y = Y,X,-1
			 IF (Y.GE.X) THEN
			  IS = MOVE_BLJ (A_B(ADDR(Y)-A_OB),
	1				YENT,PPDID__LENGTH)
			  IF (YPNAM.LT.PPDID$PNAM) THEN
				IS = MOVE_BLJ (YENT,
	1				A_B(ADDR(X)-A_OB),
	1				PPDID__LENGTH)
				IS = MOVE_BLJ (XENT,
	1				A_B(ADDR(Y)-A_OB),
	1				PPDID__LENGTH)
				YH = Y-1
				GOTO 45
			  ENDIF
			 ENDIF
			ENDDO
			Y = X-1
			GOTO 60
 45			Y = YH
		  ENDIF
		 ENDIF
		ENDDO
C
C					- move the last "lower" (Y) entry
C					  into the first position
C					- put the reference entry in its place
C
 60		IS = MOVE_BLJ (A_B(ADDR(Y)-A_OB),
	1			A_B(ADDR(SS)-A_OB),PPDID__LENGTH)
		IS = MOVE_BLJ (PPDID_,
	1			A_B(ADDR(Y)-A_OB),PPDID__LENGTH)
C
C					The segment now consists of 3 sections
C					(the middle one just is entry Y):
C					- the first Y-SS entries <= entry Y
C					- the last SE-Y entries >= entry Y
C					- save the start- and end-positions of
C					  the larger section in SAVSS and SAVSE
C					- make the smaller section the next
C					  segment to be sorted
C
		IF (Y-SS.GT.SE-Y) THEN
			SAVSS(SNR) = SS
			SAVSE(SNR) = Y-1
			SS = Y+1
		ELSE
			SAVSS(SNR) = Y+1
			SAVSE(SNR) = SE
			SE = Y-1
		ENDIF
		SNR = SNR+1
C
C					Segment with 1 or 2 entries:
C
	    ELSE
C
C					Swap if 2 entries in the wrong order
C
 
		IF (SE.GT.SS) THEN
		    IS = MOVE_BLJ (A_B(ADDR(SS)-A_OB),
	1				XENT,PPDID__LENGTH)
		    IS = MOVE_BLJ (A_B(ADDR(SE)-A_OB),
	1				YENT,PPDID__LENGTH)
		    IF (XPNAM.GT.YPNAM) THEN
			IS = MOVE_BLJ (YENT,
	1			A_B(ADDR(SS)-A_OB),PPDID__LENGTH)
			IS = MOVE_BLJ (XENT,
	1			A_B(ADDR(SE)-A_OB),PPDID__LENGTH)
		    ENDIF
		ENDIF
C
C					Take the next segment to be sorted
C
		SNR = SNR-1
		IF (SNR.GT.0) THEN
			SS = SAVSS(SNR)
			SE = SAVSE(SNR)
		ENDIF
	   ENDIF
	ENDDO
C
C
	BPD_INDEX_SORT = PPD_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION BPD_INDEX_UNIQ ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Determine the unique-abbreviation lengths of the user's names
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCESS
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'BLDPPD_2_DEF'
	INCLUDE 'PPDREC_4_DEF'
C
	INTEGER*4	BPD_PARM_PUTL, BPD_PROTO_PUTL
	INTEGER*4	STR_SIGLEN, MOVE_BLJ
C
	CHARACTER*16	UNAM, XUNAM
	INTEGER*4	LUNAM, XLUNAM
	INTEGER*4	XENT(PPDID__LENGTH)
		EQUIVALENCE (XUNAM,XENT(PPDID_UNAM))
		EQUIVALENCE (XLUNAM,XENT(PPDID_LUNAM))
	INTEGER*4	IS, X
	LOGICAL*4	READY
C
	INTEGER*4	ADDR, N
		ADDR(N) = BPD$INDEX(3)+(N-1)*PPDID__LENGTH*4
C
C
C					Do for all index entries
C					(NOTE: initially, all LUNAM's are zero)
C
	DO I = 1,BPD$INDEX(5)
		IS = MOVE_BLJ (A_B(ADDR(I)-A_OB),
	1			PPDID_,PPDID__LENGTH)
		UNAM = PPDID$UNAM
		LUNAM = PPDID$LUNAM
C
C					Check the user's name in the current
C					entry against that in all other entries
C					- start with current LUNAM + 1
C					- if a name is equal to the current
C					  name: update LUNAM in the comparison
C					  entry
C					- if at least one equality was found:
C					  increment the current LUNAM, and
C					  compare again
C					- otherwise: ready with current entry
C
		READY = .FALSE.
		DO WHILE (.NOT.READY .AND. LUNAM.LT.16)
		    LUNAM = LUNAM+1
		    READY = .TRUE.
		    DO X = 1,BPD$INDEX(5)
		     IF (X.NE.I) THEN
			IS = MOVE_BLJ (A_B(ADDR(X)-A_OB),
	1			XENT,PPDID__LENGTH)
			IF (XUNAM(:LUNAM).EQ.UNAM(:LUNAM)) THEN
			    XLUNAM = MIN (LUNAM,STR_SIGLEN(XUNAM))
			    IS = MOVE_BLJ (XENT,
	1				A_B(ADDR(X)-A_OB),
	1				PPDID__LENGTH)
			    READY = .FALSE.
			ENDIF
		     ENDIF
		    ENDDO
		ENDDO
C
C					Update LUNAM in the current index entry
C					and in the corresponding param descr
C
		PPDID$LUNAM = MIN (LUNAM,STR_SIGLEN(UNAM))
		IS = MOVE_BLJ (PPDID_,
	1		A_B(ADDR(I)-A_OB),PPDID__LENGTH)
		IF (UNAM(1:1).NE.'$') THEN
			IS = BPD_PARM_PUTL (PPDID$PARMOFF,PPDID$LUNAM)
		ELSE
			IS = BPD_PROTO_PUTL (PPDID$PARMOFF,PPDID$LUNAM)
		ENDIF
	ENDDO
C
C
	BPD_INDEX_UNIQ = PPD_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION BPD_INDEX_WRITE ()
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
C
C.Purpose:	Write the index buffer to the PPD file
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	false status codes returned by referenced modules
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'BLDPPD_2_DEF'
C
	INTEGER*4	CPL_DYN_WRITE
C
	INTEGER*4	IS
	LOGICAL*4	DO_RELEASE
C
C
	DO_RELEASE = .TRUE.
	IS = CPL_DYN_WRITE (BPD$INDEX,DO_RELEASE)
	IF (IAND(IS,1).EQ.0) GOTO 999
C
C
	BPD_INDEX_WRITE = PPD_SUCCESS
	RETURN
C
 999	BPD_INDEX_WRITE = IS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION BPD_INDEX_INQ (NINDEX,LINDEX)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	NINDEX		! (o) nr of index entries
	INTEGER*4	LINDEX		! (o) significant length in bytes
C
C.Purpose:	Get information about the index buffer
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'BLDPPD_2_DEF'
C
	NINDEX = BPD$INDEX(5)
	LINDEX = BPD$INDEX(4)
C
	BPD_INDEX_INQ = PPD_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION BPD_INDEX_GETU (UNAM)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	UNAM		! (i) user's parameter name
C
C.Purpose:	Check whether the name occurs in the index
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_KEYNOTFND	name not found
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'BLDPPD_2_DEF'
	INCLUDE 'PPDREC_4_DEF'
C
	INTEGER		MOVE_BLJ, MSG_SET
C
	INTEGER*4	IS, NR, ADDR
C
C
	ADDR = BPD$INDEX(3)
	DO NR = 1,BPD$INDEX(5)
		IS = MOVE_BLJ (A_B(ADDR-A_OB),PPDID_,PPDID__LENGTH)
		IF (UNAM.EQ.PPDID$UNAM) GOTO 999
		ADDR = ADDR+PPDID__LENGTH*4
	ENDDO
C
	BPD_INDEX_GETU = MSG_SET (PPD_KEYNOTFND,1)
	RETURN
C
 999	BPD_INDEX_GETU = PPD_SUCCESS
	RETURN
	END
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	INTEGER*4 FUNCTION BPD_INDEX_GETP (PNAM)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*(*)	PNAM		! (i) program's parameter name
C
C.Purpose:	Check whether the name occurs in the index
C.Returns:	Status code (.TRUE. for success, .FALSE. otherwise)
C	success	PPD_SUCCESS
C	error	PPD_PKYNOTFND	name not found
C.Notes:
C-------------------------------------------------------------------------
C
	INCLUDE 'BLDPPD_2_DEF'
	INCLUDE 'PPDREC_4_DEF'
C
	INTEGER		MOVE_BLJ, MSG_SET
C
	INTEGER*4	IS, NR, ADDR
C
C
	ADDR = BPD$INDEX(3)
	DO NR = 1,BPD$INDEX(5)
		IS = MOVE_BLJ (A_B(ADDR-A_OB),PPDID_,PPDID__LENGTH)
		IF (PNAM.EQ.PPDID$PNAM) GOTO 999
		ADDR = ADDR+PPDID__LENGTH*4
	ENDDO
C
	BPD_INDEX_GETP = MSG_SET(PPD_PKYNOTFND,1)
	RETURN
C
 999	BPD_INDEX_GETP = PPD_SUCCESS
	RETURN
	END
