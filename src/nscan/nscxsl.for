C+ NSCXSL.FOR
C  WNB 910211
C
C  Revisions:
C	WNB 931216	New EDIT format
C	JPH 961112	Correct comment (set --> scan)
C
	SUBROUTINE NSCXSL(PTYPE,INFCA,SCHP)
C
C  Show scan header
C
C  Result:
C
C	CALL NSCXSL ( PTYPE_J:I, INFCA_J:I, SCHP_J:I)
C					Show on output PTYPE the scan at SCHP
C					of file INFCA.
C	CALL NSCESL ( PTYPE_J:I, INFCA_J:I, SCHP_J:I)
C					Edit data
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'SCH_O_DEF'		!SCAN HEADER
	INCLUDE 'GFH_O_DEF'
	INCLUDE 'SCH_E_DEF'		!EDIT INFORMATION
C
C  Parameters:
C
	INTEGER MXDEP			!MAX. NESTING DEPTH
	  PARAMETER (MXDEP=8)
	INTEGER D_GEDL			!GENERAL DATA
	  PARAMETER (D_GEDL=1)
	INTEGER D_GMAX			!MAX. # OF DATA POINTS
	  PARAMETER (D_GMAX=100)
C
C  Arguments:
C
	INTEGER PTYPE			!PRINT TYPE (f_p, f_t ETC)
	INTEGER INFCA			!FILE DESCRIPTOR
	INTEGER SCHP			!SET HEADER POINTER
C
C  Function references:
C
	LOGICAL WNFRD			!READ DATA
	LOGICAL WNFWR			!WRITE DATA
	INTEGER WNGGJ			!GET J
C
C  Data declarations:
C
	CHARACTER*8 PLIST(10)		!KNOWN P: AREAS
	  DATA PLIST/	'SCH','STH',
	1		'B','I','J','E','D','X','Y',
	1		' '/
	INTEGER PLEN(0:1,10)		!P: LENGTH
	  DATA PLEN/	-1,SCHHDL,
	1		-1,4,
	1		-1,LB_B,-1,LB_I,-1,LB_J,-1,LB_E,
	1		-1,LB_D,-1,LB_X,-1,LB_Y,
	1		0,0/
	INTEGER DEP			!CURRENT DEPTH
	INTEGER DEPAR(4,MXDEP)		!SAVE DEPTH
	INTEGER CHP,CHDL		!CURRENT HEADER LENGTH, PTR
	INTEGER CTYP,CEDP		!CURRENT HEADER TYPE #, PTR INTO EDIT
	INTEGER CHPT			!NEXT HEADER POINTER
	INTEGER PSZ(0:1)		!P: OFFSET AND SIZE
	BYTE STH(0:4-1)			!DUMMY STH HEADER
	  BYTE SCH(0:SCHHDL-1)		!SCAN HEADER
	  BYTE D_G(0:D_GMAX*LB_Y-1)
	  EQUIVALENCE (STH,SCH,D_G)
	CHARACTER*8 D_G_EC(4,7)		!DATA TABLES
	  DATA D_G_EC/	'B','SB',' ',' ',
	1		'I','SI',' ',' ',
	1		'J','SJ',' ',' ',
	1		'E','E12.6',' ',' ',
	1		'D','D12.8',' ',' ',
	1		'X','26$EC12.6',' ',' ',
	1		'Y','26$DC12.8',' ',' '/
	INTEGER D_G_EJ(4,7)
	  DATA D_G_EJ/	0,1,0,LB_B,
	1		0,1,0,LB_I,
	1		0,1,0,LB_J,
	1		0,1,0,LB_E,
	1		0,1,0,LB_D,
	1		0,1,0,LB_X,
	1		0,1,0,LB_Y/
C-
C
C GET HEADER
C
	IF (.NOT.WNFRD(INFCA,SCHHDL,SCH,SCHP)) THEN
	  CALL WNCTXT(PTYPE,'Read error on input node')
	  RETURN
	END IF
C
C SHOW HEADER
C
	CALL WNCTXT(PTYPE,'!/Scan header description!/')
	CALL NSCXXS(PTYPE,SCH,SCHEDL,SCH_EC,SCH_EJ)	!ACTUAL SHOW
C
	RETURN
C
C NSCESL
C
	ENTRY NSCESL(PTYPE,INFCA,SCHP)
C
C INIT
C
	DEP=0					!CURRENT DEPTH
	CHP=SCHP				!HEADER POINTER
	CTYP=1					!CURRENT TYPE (GFH)
	CEDP=-1					!CURRENT POINTER IN EDIT LIST
	CHDL=SCHHDL				!CURRENT LENGTH
C
C ACTION
C
 10	CONTINUE
	DO WHILE (CTYP.GT.0)			!SOMETHING TO DO
	  IF (CHDL.LE.0) THEN			!GET NEW HEADER
	    IF (PLEN(0,CTYP).GE.0 .AND. CEDP.GT.0) THEN
	      CHDL=WNGGJ(STH(PLEN(0,CTYP)))	!LENGTH FROM FILE
	    ELSE
	      CHDL=PLEN(1,CTYP)			!DEFAULT LENGTH
	    END IF
	    CHDL=MIN(CHDL,PLEN(1,CTYP))		!MAKE SURE NO PROBLEMS
	    IF (CHDL.LE.0) GOTO 20		!NOT PRESENT; RESTART CURRENT
	  END IF
C
C GET HEADER
C
	  IF (CHP.EQ.0 .AND.
	1		(CTYP.LT.1 .OR.
	1		(CTYP.GT.1 .AND. CTYP.LT.3) .OR.
	1		(CTYP.GT.9))) GOTO 20	!NOT PRESENT
	  IF (CHP.GT.0 .AND. CHP.LT.GFHHDL .AND.
	1		(CTYP.LT.3 .OR. CTYP.GT.9)) THEN !MUST BE GFH
	    CTYP=1
	    CHDL=PLEN(1,CTYP)
	    CHP=0
	    CEDP=-1
	  END IF
	  CALL WNGMVZ(PLEN(1,CTYP),STH)		!CLEAR BEFORE READ
	  IF (.NOT.WNFRD(INFCA,CHDL,STH,CHP)) THEN
	    CALL WNCTXT(PTYPE,'Read error on input node')
	    RETURN
	  END IF
C
C EDIT HEADER
C
	  IF (CTYP.NE.2)
	1		CALL WNCTXT(PTYPE,'*** Editing !AS ***',PLIST(CTYP))
	  IF (DEP.GE.MXDEP) THEN		!SHIFT ONE
	    DO I=1,MXDEP-1
	      DO I1=1,4
	        DEPAR(I1,I)=DEPAR(I1,I+1)
	      END DO
	    END DO
	    DEP=MXDEP-1
	  END IF
	  DEP=DEP+1				!SAVE PREVIOUS
	  DEPAR(1,DEP)=CHP
	  DEPAR(2,DEP)=CTYP
	  DEPAR(3,DEP)=CEDP
	  DEPAR(4,DEP)=CHDL
	  IF (CTYP.EQ.1) THEN
	    CALL NSCXES(PTYPE,STH,SCHEDL,SCH_EC,SCH_EJ,PLIST,
	1		CTYP,CEDP,CHPT,PSZ)
	  ELSE IF (CTYP.EQ.2) THEN
	    CALL NSCESH(PTYPE,INFCA,CHP,0)	!DO STH
	    CTYP=0				!END CONTINUE
	  ELSE IF (CTYP.GE.3 .AND. CTYP.LE.9) THEN
	    CALL NSCXES(PTYPE,STH,D_GEDL,
	1		D_G_EC(1,CTYP-2),D_G_EJ(1,CTYP-2),PLIST,
	1		CTYP,CEDP,CHPT,PSZ)
	  END IF
	  IF (CTYP.GE.1000) THEN		!RELATIVE ADDRESS
	    CTYP=MOD(CTYP,1000)			!GET CORRECT TYPE
	    CHPT=CHP+CHPT			!CATER FOR OFFSET GIVEN
	  END IF
	  IF (CTYP.GE.3 .AND. CTYP.LE.9) THEN
	    CHPT=CHPT+PSZ(0)*D_G_EJ(4,CTYP-2)	!CATER FOR GIVEN OFFSET
	    D_G_EJ(2,CTYP-2)=MAX(1,MIN(PSZ(1),D_GMAX)) !MAX. NUMBER TO DO
	  END IF
C
C REWRITE HEADER
C
	  IF (.NOT.WNFWR(INFCA,CHDL,STH,CHP)) THEN
 30	    CONTINUE
	    CALL WNCTXT(PTYPE,'Write error on input node')
	    RETURN
	  END IF
	  CHP=CHPT				!NEXT HEADER POINTER
	  IF (CTYP.GE.3 .AND. CTYP.LE.9) THEN
	    CHDL=D_G_EJ(2,CTYP-2)*D_G_EJ(4,CTYP-2) !NEW LENGTH
	  ELSE
	   CHDL=0				!NEXT HEADER LENGTH
	  END IF
	END DO
C
C RETURN PREVIOUS LEVEL
C
	DEP=DEP-1
 20	CONTINUE
	IF (DEP.GT.0) THEN			!CAN DO MORE
	  CHP=DEPAR(1,DEP)
	  CTYP=DEPAR(2,DEP)
	  CEDP=DEPAR(3,DEP)
	  CHDL=DEPAR(4,DEP)
	  DEP=DEP-1
	  GOTO 10
	END IF
C
	RETURN
C
C
	END
