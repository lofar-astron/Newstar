C+ NGCPRT.FOR
C  WNB 920820
C
C  Revisions:
C	HjV 930423	Change name of some keywords
C	WNB 930628	Change 	PLOT_ACTION into SET_ACTION
C				SCN_SETS into NGF_SETS
C	WNB 931214	No auto SHOW if EDIT
C	CMV 931220	Separate LAYOUT and OVERVIEW options
C	CMV 931220	Pass FCA of input file to WNDXLP and WNDSTA/Q
C	CMV 940811      Print only defined baselines if TRTYP=2
C       HjV 950530	Change SET_ACTION in SECTOR_ACTION
C
	SUBROUTINE NGCPRT
C
C  Show/edit data in NGF file
C
C  Result:
C
C	CALL NGCPRT	will show and/or edit data in NGF file
C
C  PIN references:
C
C	FILE_ACTION
C	SECTOR_ACTION
C	DATA_ACTION
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'NGC_DEF'
	INCLUDE 'NGF_O_DEF'		!PLOT HEADER
C
C  Parameters:
C
C
C  Arguments:
C
C
C  Function references:
C
	LOGICAL WNDPAR			!GET DWARF PARAMETER
	LOGICAL WNFRD			!READ FILE
	LOGICAL WNDSTA			!GET PLOTS TO DO
	LOGICAL NGCSTG			!GET A PLOT
C
C  Data declarations:
C
	CHARACTER*24 ACT		!ACTION ASKED
	INTEGER NGFP			!SUB-GROUP POINTER
	INTEGER SNAM(0:7)		!SET NAME
	COMPLEX LBUF(0:8191)		!DATA BUF
	  REAL EBUF(0:8191)
	  EQUIVALENCE (LBUF,EBUF)
	BYTE NGF(0:NGFHDL-1)		!PLOT HEADER
	  INTEGER NGFJ(0:NGFHDL/4-1)
	  INTEGER*2 NGFI(0:NGFHDL/2-1)
	  REAL NGFE(0:NGFHDL/4-1)
	  EQUIVALENCE (NGF,NGFJ,NGFI,NGFE)
C-
C
C SHOW FILE HEADER
C
	CALL NSCPFH(F_TP,FCAOUT)		!PRINT FILE HEADER
C
C FILE ACTION
C
 101	CONTINUE
	IF (.NOT.WNDPAR('FILE_ACTION',ACT,LEN(ACT),J,'CONT')) THEN !FILE ACTION
	  IF (E_C.EQ.DWC_ENDOFLOOP) THEN	!^Z
 102	    CONTINUE
	    RETURN				!READY
	  END IF
	  GOTO 101				!RETRY
	ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN
	  GOTO 102				!READY
	ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
	  ACT='CONT'				!ASSUME CONT
	END IF
	IF (ACT.EQ.'LAYOUT') THEN		  !SHOW LAYOUT
	  CALL NGCPFL(F_TP,FCAOUT,NODOUT,.FALSE.) !SHOW LAYOUT
	ELSE IF (ACT.EQ.'OVERVIEW') THEN	  !SHOW OVERVIEW
	  CALL NGCPFL(F_TP,FCAOUT,NODOUT,.TRUE.)  !SHOW OVERVIEW
	ELSE IF (ACT.EQ.'SHOW') THEN		  !SHOW DETAILS
	  CALL NSCXFH(F_TP,FCAOUT)
	ELSE IF (ACT.EQ.'EDIT') THEN		!EDIT
	  CALL NSCEFH(F_TP,FCAOUT)		!EDIT HEADER
	ELSE IF (ACT.EQ.'CONT') THEN		!CONT
	  GOTO 200				!DO PLOT
	ELSE
	  GOTO 102				!QUIT
	END IF
	GOTO 101				!UNKNOWN
C
C DO PLOT
C
 200	CONTINUE
	IF (.NOT.WNDSTA('NGF_SETS',MXNSET,SETS,FCAOUT)) GOTO 102 !GET SETS TO DO
	IF (SETS(0,0).EQ.0) GOTO 102		!NONE
 201	CONTINUE				!DO NEXT SET
	IF (.NOT.NGCSTG(FCAOUT,SETS,NGF,NGFP,SNAM)) GOTO 102 !GET PLOT
	CALL NGCPMH(F_TP,NGF,SNAM,NODOUT)	!SHOW PLOT HEADER
C
C PLOT ACTION
C
 301	CONTINUE
	IF (.NOT.WNDPAR('SECTOR_ACTION',ACT,LEN(ACT),J,'CONT')) THEN !PLOT ACTION
	  IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 101	!^Z, RETRY FILE ACTION
	  GOTO 301				!RETRY
	ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN
	  GOTO 101				!RETRY FILE ACTION
	ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
	  ACT='CONT'				!ASSUME CONT
	END IF
	IF (ACT.EQ.'NEXT') THEN			!NEXT PLOT
	  GOTO 201				!NEXT PLOT
	ELSE IF (ACT.EQ.'SHOW') THEN		!SHOW DETAILS
	  CALL NGCXMH(F_TP,FCAOUT,NGFP,SNAM)
	ELSE IF (ACT.EQ.'EDIT') THEN		!EDIT
	  CALL NGCEMH(F_TP,FCAOUT,NGFP,SNAM)	!EDIT
	ELSE IF (ACT.EQ.'CONT') THEN		!CONT
	  GOTO 400				!DO DATA
	ELSE
	  GOTO 101				!QUIT
	END IF
	GOTO 301				!UNKNOWN
C
C DO DATA
C
 400	CONTINUE
 401	CONTINUE
	IF (.NOT.WNDPAR('DATA_ACTION',ACT,LEN(ACT),J,'Q')) THEN !ACTION
	  IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 301	!^Z, RETRY SET ACTION
	  GOTO 401				!RETRY
	ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN
	  GOTO 301				!RETRY SET ACTION
	ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
	  ACT='Q'				!ASSUME >
	END IF
	CALL WNCAUC(ACT)			!MAKE UC
	IF (ACT(1:1).EQ.'Q') THEN		!QUIT
	  GOTO 301
	ELSE IF (ACT(1:1).EQ.'S' .OR. ACT(1:1).EQ.'A' .OR.
	1		ACT(1:1).EQ.'P') THEN	!SHOW DATA
C
C DATA DISPLAY
C
	  IF (.NOT.WNFRD(FCAOUT,LB_X*NGFJ(NGF_SCN_J),
	1		LBUF,NGFJ(NGF_DPT_J))) THEN !READ
		CALL WNCTXT(F_TP,'Error reading plot')
	        GOTO 401
	  END IF
	  IF (NGFJ(NGF_TRTYP_J).EQ.2) THEN
	     DO I=0,NGFJ(NGF_SCN_J)-1
	        IF (REAL(LBUF(I)).NE.NGCDLC) THEN
	           R1=(NGFE(NGF_HAB_E)+I*NGFE(NGF_HAI_E))*360.*10.
	           IF (ACT(1:1).EQ.'S') THEN
	              CALL WNCTXT(F_TP,'!5$UJ\. !6$E6.0 m !20$EC9.2',
	1		          I,R1,LBUF(I))
	           ELSE IF (ACT(1:1).EQ.'A') THEN
	              CALL WNCTXT(F_TP,'!5$UJ\. !6$E6.0 m !10$E9.2',
	1		          I,R1,ABS(LBUF(I)))
	           ELSE IF (ACT(1:1).EQ.'P') THEN
	              IF (REAL(LBUF(I)).EQ.0) THEN
	 	        R0=SIGN(PI/2.,AIMAG(LBUF(I)))
	              ELSE
	                R0=ATAN2(AIMAG(LBUF(I)),REAL(LBUF(I)))
	              END IF
	              CALL WNCTXT(F_TP,
	1		   '!5$UJ\. !6$E6.0 m !20$EAR9.2',I,R1,R0)
	           END IF
	        END IF
	     END DO
	  ELSE
	    IF (ACT(1:1).EQ.'S') THEN
	      CALL WNCTXT(F_TP,'!80$1Q1!20$#EC9.2',
	1		NGFJ(NGF_SCN_J),LBUF)	!SHOW DATA
	    ELSE IF (ACT(1:1).EQ.'A') THEN	!SHOW AMPL. DATA
	      DO I=0,NGFJ(NGF_SCN_J)-1
	        EBUF(I)=ABS(LBUF(I))
	      END DO
	      CALL WNCTXT(F_TP,'!80$1Q1!10$#E9.2',
	1		NGFJ(NGF_SCN_J),LBUF)
	    ELSE IF (ACT(1:1).EQ.'P') THEN	!SHOW PHASE DATA
	      DO I=0,NGFJ(NGF_SCN_J)-1
	        IF (REAL(LBUF(I)).EQ.0) THEN
		  EBUF(I)=SIGN(PI/2.,AIMAG(LBUF(I)))
	        ELSE
	          EBUF(I)=ATAN2(AIMAG(LBUF(I)),REAL(LBUF(I)))
	        END IF
	      END DO
	      CALL WNCTXT(F_TP,'!80$1Q1!10$#EAR9.2',
	1		NGFJ(NGF_SCN_J),LBUF)
	    END IF
	  END IF
	END IF
C
C NEXT ACTION
C
	GOTO 401				!NEXT ACTION
C
C
	END