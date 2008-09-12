C+ NSCCOP.FOR
C  WNB 910911
C
C  Revisions:
C	HjV 920520	HP does not allow extended source lines
C	HjV 930311	Change some text
C	WNB 930607	New weights
C	WNB 930707	Get inputs; make simpler
C	WNB 930826	New HA range
C	CMV 931220	Pass FCA of input file to WNDXLP and WNDSTA/Q
C	CMV 940926	Close old file before asking new one
C
	SUBROUTINE NSCCOP
C
C  Copy SCN sets
C
C  Result:
C
C	CALL NSCCOP		Copies selected sets
C
C  Pin references:
C
C	INPUT_SCN_NODE
C	OUTPUT_SCN_NODE
C	SCN_SETS
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'GFH_O_DEF'		!FILE HEADER
	INCLUDE 'SGH_O_DEF'		!SUB-GROUP HEADER
	INCLUDE 'STH_O_DEF'		!SET HEADER
	INCLUDE 'SCH_O_DEF'		!SCAN HEADER
	INCLUDE 'NSC_DEF'
	INCLUDE 'OHW_O_DEF'		!WSRT BLOCKS
	INCLUDE 'FDW_O_DEF'
	INCLUDE 'FDX_O_DEF'
	INCLUDE 'SCW_O_DEF'
	INCLUDE 'SHW_O_DEF'
C
C  Parameters:
C
	INTEGER MXNCHK			!CHECK TABLE LENGTH
	  PARAMETER (MXNCHK=512)
C
C  Arguments:
C
C
C  Function references:
C
	INTEGER WNFEOF			!FILE LENGTH
	LOGICAL WNFOP			!OPEN FILE
	LOGICAL WNFWR			!WRITE DISK
	LOGICAL WNFRD			!READ DISK
	LOGICAL WNDLNF,WNDLNG,WNDLNK	!LINK MAPS
	LOGICAL WNDNOD			!GET NODE
	LOGICAL WNDPAR			!GET USER DATA
	LOGICAL WNDSTA			!GET USER SETS
	CHARACTER*32 WNTTSG		!MAP NAME
	LOGICAL NSCSTG			!GET SET
	LOGICAL NSCHAS			!GET HA RANGE
C
C  Data declarations:
C
	REAL HARAN(0:1)
	LOGICAL LFIRST			!FIRST LINK?
	INTEGER IPOL			!# OF POL.
	INTEGER OUTP			!DATA OUTPUT POINTER
	INTEGER SNAM(0:7)		!SET NAME
	INTEGER STHP(0:1)		!SET HEADER POINTER
	INTEGER CHKT(0:1,0:MXNCHK-1)	!CHECK PRESENCE OH ETC
	INTEGER NCHK
	INTEGER*2 IFR(0:STHIFR-1)	!IFR TABLE
	  BYTE OHW(0:OHWHDL-1)		!OH
	  BYTE FDW(0:FDWHDL+FDXHDL-1)	!FD+FDX
	  BYTE SCW(0:SCWHDL-1)		!SC
	  BYTE SHW(0:SHWHDL-1)		!SH
	  INTEGER*2 ODAT(0:2,0:4*STHIFR-1) !OUTPUT DATA
	  EQUIVALENCE (IFR,OHW,FDW,SCW,SHW,ODAT)
	BYTE STH(0:STHHDL-1,0:1)	!SET HEADER
	  INTEGER*2 STHI(0:STHHDL/2-1,0:1)
	  INTEGER STHJ(0:STHHDL/4-1,0:1)
	  REAL STHE(0:STHHDL/4-1,0:1)
	  DOUBLE PRECISION STHD(0:STHHDL/8-1,0:1)
	  EQUIVALENCE (STH,STHI,STHJ,STHE,STHD)
	BYTE SCH(0:SCHHDL-1,0:1)	!SCAN HEADER
	  INTEGER*2 SCHI(0:SCHHDL/2-1,0:1)
	  INTEGER SCHJ(0:SCHHDL/4-1,0:1)
	  REAL SCHE(0:SCHHDL/4-1,0:1)
	  DOUBLE PRECISION SCHD(0:SCHHDL/8-1,0:1)
	  EQUIVALENCE (SCH,SCHI,SCHJ,SCHE,SCHD)
C-
C
C INIT
C
	LFIRST=.TRUE.				!FIRST LINK
	NCHK=0
	SETS(0,0)=0				!NO SETS
	HARAN(0)=-179.99/360.			!HA RANGE
	HARAN(1)=+179.99/360.
 10	CONTINUE
	IF (.NOT.WNDNOD('INPUT_SCN_NODE','""','SCN',
	1		'R',NODIN,IFILE)) THEN
	  IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 900	!READY
	    GOTO 10				!REPEAT
	  ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN
	    GOTO 900				!READY
	  ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
	    GOTO 10				!MUST SPECIFY
	  END IF
	IF (.NOT.WNFOP(FCAIN,IFILE,'R')) GOTO 10 !OPEN INPUT FILE
 12	CONTINUE
	IF (.NOT.WNDSTA('SCN_SETS',MXNSET,SETS,FCAIN)) THEN !SETS TO USE
 11	  CONTINUE
	  CALL WNFCL(FCAIN)
	  GOTO 10				!RETRY FILE
	END IF
	IF (SETS(0,0).LE.0) GOTO 11		!NONE SPECIFIED
	IF (.NOT.NSCHAS(0,HARAN)) GOTO 12	!GET HA RANGE
 14	CONTINUE
	CALL WNFCL(FCAOUT)
	IF (.NOT.WNDNOD('OUTPUT_SCN_NODE','""','SCN','U',
	1		NODOUT,OFILE)) THEN
	  IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 11	!RETRY INPUT
	  GOTO 14				!REPEAT
	END IF
	IF (E_C.EQ.DWC_NULLVALUE) GOTO 11
	IF (E_C.EQ.DWC_WILDCARD) GOTO 14	!MUST SPECIFY
	IF (.NOT.WNFOP(FCAOUT,OFILE,'U')) GOTO 14 !CANNOT OPEN
C
C GET SETS
C
	DO WHILE(NSCSTG(FCAIN,SETS(0,0),STH(0,0),
	1		STHP(0),SNAM))		!GET A SET
	  IPOL=STHI(STH_PLN_I,0)		!# OF POL.
C
C CHECK IF ANYTHING WANTED
C
	  IF (HARAN(0).GT.STHE(STH_HAB_E,0)+(STHJ(STH_SCN_J,0)-1)*
	1		STHE(STH_HAI_E,0) .OR.
	1		HARAN(1).LT.STHE(STH_HAB_E,0)) GOTO 100 !FORGET SET
C
C PREPARE SET HEADER
C
	  CALL WNGMV(STHHDL,STH(0,0),STH(0,1))	!COPY SET HEADER
	  STHP(1)=WNFEOF(FCAOUT)		!WHERE TO WRITE
	  IF (.NOT.WNFWR(FCAOUT,STHHDL,STH(0,1),STHP(1))) THEN
 40	    CONTINUE
	    CALL WNCTXT(F_TP,'Error copying Sector !AS',
	1		WNTTSG(SNAM,0))
	    CALL WNGEX				!STOP
	  END IF
	  OUTP=WNFEOF(FCAOUT)			!POINTER TO DATA
	  STHJ(STH_SCNP_J,1)=OUTP
C
C ALL SCANS
C
	  J0=0						!COUNT SCANS
	  DO I=0,STHJ(STH_SCN_J,0)-1
	    IF (.NOT.WNFRD(FCAIN,SCHHDL,SCH(0,0),
	1		STHJ(STH_SCNP_J,0)+
	1		I*STHJ(STH_SCNL_J,0))) THEN	!READ SCAN HEADER
 41	      CONTINUE
	      CALL WNCTXT(F_TP,'Error reading data Sector !AS',
	1		WNTTSG(SNAM,0))
	      GOTO 100					!FORGET SET
	    END IF
	    IF (SCHE(SCH_HA_E,0).LT.HARAN(0) .OR.
	1		SCHE(SCH_HA_E,0).GT.HARAN(1)) GOTO 30 !FORGET SCAN
	    IF (.NOT.WNFRD(FCAIN,STHJ(STH_SCNL_J,0)-SCHHDL,ODAT,
	1		STHJ(STH_SCNP_J,0)+SCHHDL+
	1		I*STHJ(STH_SCNL_J,0))) GOTO 41	!READ DATA
	    J0=J0+1					!COUNT SCANS
	    SCHJ(SCH_IFRAC_J,0)=0			!NO IFR CORRECTIONS
	    SCHJ(SCH_IFRMC_J,0)=0
	    SCHJ(SCH_AIFRAC_J,0)=0
	    SCHJ(SCH_AIFRMC_J,0)=0
	    IF (.NOT.WNFWR(FCAOUT,SCHHDL,SCH(0,0),OUTP)) GOTO 40 !HEADER
	    OUTP=OUTP+SCHHDL
	    IF (.NOT.WNFWR(FCAOUT,STHJ(STH_SCNL_J,0)-SCHHDL,
	1		ODAT,OUTP)) GOTO 40		!DATA
	    OUTP=OUTP+STHJ(STH_SCNL_J,0)-SCHHDL
C
C NEXT SCAN
C
 30	    CONTINUE
	  END DO
C
C MAKE SET HEADER
C
	  IF (J0.LE.0) GOTO 100			!NO DATA, SKIP SET
	  STHE(STH_HAB_E,1)=MAX(STHE(STH_HAB_E,0),HARAN(0))
	  STHJ(STH_SCN_J,1)=J0
	  DO I=0,1				!NO MODEL COPIED
	    STHJ(STH_MDL_J+I,1)=0
	    STHJ(STH_MDD_J+I,1)=0
	  END DO
	  IF (STHJ(STH_IFRP_J,0).NE.0) THEN	!COPY IFR TABLE
	    DO I=0,MIN(MXNCHK,NCHK)-1
	      IF (STHJ(STH_IFRP_J,0).EQ.CHKT(0,I)) THEN
		STHJ(STH_IFRP_J,1)=CHKT(1,I)
		GOTO 60
	      END IF
	    END DO
	    IF (.NOT.WNFRD(FCAIN,LB_I*STHJ(STH_NIFR_J,0),IFR,
	1		STHJ(STH_IFRP_J,0))) GOTO 40
	    STHJ(STH_IFRP_J,1)=WNFEOF(FCAOUT)
	    IF (.NOT.WNFWR(FCAOUT,LB_I*STHJ(STH_NIFR_J,0),IFR,
	1		STHJ(STH_IFRP_J,1))) GOTO 40
	    DO I=0,1
	      CHKT(I,MOD(NCHK,MXNCHK))=STHJ(STH_IFRP_J,I)
	    END DO
	    NCHK=NCHK+1
	  END IF
 60	  CONTINUE
	  IF (STHJ(STH_FDP_J,0).NE.0) THEN	!COPY FD
	    DO I=0,MIN(MXNCHK,NCHK)-1
	      IF (STHJ(STH_FDP_J,0).EQ.CHKT(0,I)) THEN
		STHJ(STH_FDP_J,1)=CHKT(1,I)
		GOTO 61
	      END IF
	    END DO
	    IF (.NOT.WNFRD(FCAIN,STHJ(STH_NFD_J,0),FDW,
	1		STHJ(STH_FDP_J,0))) GOTO 40
	    STHJ(STH_FDP_J,1)=WNFEOF(FCAOUT)
	    IF (.NOT.WNFWR(FCAOUT,STHJ(STH_NFD_J,1),FDW,
	1		STHJ(STH_FDP_J,1))) GOTO 40
	    DO I=0,1
	      CHKT(I,MOD(NCHK,MXNCHK))=STHJ(STH_FDP_J,I)
	    END DO
	    NCHK=NCHK+1
	  END IF
 61	  CONTINUE
	  IF (STHJ(STH_OHP_J,0).NE.0) THEN	!COPY OH
	    DO I=0,MIN(MXNCHK,NCHK)-1
	      IF (STHJ(STH_OHP_J,0).EQ.CHKT(0,I)) THEN
		STHJ(STH_OHP_J,1)=CHKT(1,I)
		GOTO 62
	      END IF
	    END DO
	    IF (.NOT.WNFRD(FCAIN,STHJ(STH_NOH_J,0),OHW,
	1		STHJ(STH_OHP_J,0))) GOTO 40
	    STHJ(STH_OHP_J,1)=WNFEOF(FCAOUT)
	    IF (.NOT.WNFWR(FCAOUT,STHJ(STH_NOH_J,1),OHW,
	1		STHJ(STH_OHP_J,1))) GOTO 40
	    DO I=0,1
	      CHKT(I,MOD(NCHK,MXNCHK))=STHJ(STH_OHP_J,I)
	    END DO
	    NCHK=NCHK+1
	  END IF
 62	  CONTINUE
	  IF (STHJ(STH_SCP_J,0).NE.0) THEN	!COPY SC
	    DO I=0,MIN(MXNCHK,NCHK)-1
	      IF (STHJ(STH_SCP_J,0).EQ.CHKT(0,I)) THEN
		STHJ(STH_SCP_J,1)=CHKT(1,I)
		GOTO 63
	      END IF
	    END DO
	    IF (.NOT.WNFRD(FCAIN,STHJ(STH_NSC_J,0),SCW,
	1		STHJ(STH_SCP_J,0))) GOTO 40
	    STHJ(STH_SCP_J,1)=WNFEOF(FCAOUT)
	    IF (.NOT.WNFWR(FCAOUT,STHJ(STH_NSC_J,1),SCW,
	1		STHJ(STH_SCP_J,1))) GOTO 40
	    DO I=0,1
	      CHKT(I,MOD(NCHK,MXNCHK))=STHJ(STH_SCP_J,I)
	    END DO
	    NCHK=NCHK+1
	  END IF
 63	  CONTINUE
	  IF (STHJ(STH_SHP_J,0).NE.0) THEN	!COPY SH
	    DO I=0,MIN(MXNCHK,NCHK)-1
	      IF (STHJ(STH_SHP_J,0).EQ.CHKT(0,I)) THEN
		STHJ(STH_SHP_J,1)=CHKT(1,I)
		GOTO 64
	      END IF
	    END DO
	    IF (.NOT.WNFRD(FCAIN,STHJ(STH_NSH_J,0),SHW,
	1		STHJ(STH_SHP_J,0))) GOTO 40
	    STHJ(STH_SHP_J,1)=WNFEOF(FCAOUT)
	    IF (.NOT.WNFWR(FCAOUT,STHJ(STH_NSH_J,1),SHW,
	1		STHJ(STH_SHP_J,1))) GOTO 40
	    DO I=0,1
	      CHKT(I,MOD(NCHK,MXNCHK))=STHJ(STH_SHP_J,I)
	    END DO
	    NCHK=NCHK+1
	  END IF
 64	  CONTINUE
	  IF (.NOT.WNFWR(FCAOUT,STHHDL,STH(0,1),STHP(1))) GOTO 40 !NEW HEADER
C
C LINK SET
C
	  IF (LFIRST) THEN			!MAKE NEW JOB
	    IF (.NOT.WNDLNG(GFH_LINKG_1,0,SGH_GROUPN_1,
	1		FCAOUT,SGPH(0),SGNR(0))) THEN
 50	      CONTINUE
	      CALL WNCTXT(F_TP,'Error creating sub-group')
	      CALL WNGEX			!STOP
	    END IF
	    LFIRST=.FALSE.
	  END IF
	  DO I1=1,3
	    IF (.NOT.WNDLNF(SGPH(I1-1)+SGH_LINKG_1,SNAM(I1),SGH_GROUPN_1,
	1		FCAOUT,SGPH(I1),SGNR(I1))) GOTO 50
	  END DO
	  I1=4
	  IF (.NOT.WNDLNK(GFH_LINK_1,STHP(1),STH_SETN_1,
	1		FCAOUT)) GOTO 50
	  IF (.NOT.WNDLNG(SGPH(I1-1)+SGH_LINKG_1,STHP(1),SGH_GROUPN_1,
	1		FCAOUT,SGPH(I1),SGNR(I1))) GOTO 50
	  IF (.NOT.WNFRD(FCAOUT,STHHDL,STH(0,1),STHP(1))) GOTO 50 !HEADER
	  SGNR(5)=-1				!END NAME
	  CALL WNCTXT(F_T,'Scan !AS copied',WNTTSG(SGNR,0))
C
C NEXT SET
C
 100	  CONTINUE
	END DO
C
C READY
C
 900	CONTINUE
	CALL WNFCL(FCAOUT)			!CLOSE FILES
	CALL WNFCL(FCAIN)
C
	RETURN
C
C
	END
