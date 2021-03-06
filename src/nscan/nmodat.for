C+ NMODAT.FOR
C  WNB 900327
C
C  Revisions:
C	WNB 910814	Add (de-)beam
C	WNB 910909	Add CAL option
C	WNB 910909	Add NMODAY
C	WNB 911230	ADD NMOANC
C	WNB 920626	Add NMOACD
C	WNB 920818	Add NMOAAD
C	WNB 921104	Full HA range
C	WNB 921217	Add AFIND
C	HjV 930423	Change name of some keywords
C	WNB 930514	Add manual find
C	WNB 930615	Make correct MDL_NODE keywords
C	WNB 930825	New pol. selection
C	WNB 930826	New HA selection
C	HjV 930914	NSCIFS is now a function iso. a subroutine
C	WNB 931008	Add EDIT ACTION; chnage CC to CCBM
C	WNB 931011	Add SUPDATE
C	WNB 931119	Add REDIT, FEDIT action
C	CMV 931220	Pass FCA of input file to WNDXLP and WNDSTA/Q
C	CMV 940224	Add option INTERN, new entry NMODAW
C	CMV 940225	Split off some options from MODEL_OPTION
C	CMV 940228	Add option to edit and test the "unknown-flux" bit
C       HjV 940303      Tekst typo's
C	CMV 940328	Changed WNGGD call to WNGMV (did not work on HP)
C	CMV 940415	Add SHOW option to MODEL_MODIFY
C	CMV 940817	Prohibit INTERNAL for NMODAV (used for REDUN)
C	WNB 940821	Add Polarisation updates
C       WNB 950629	Add UPDATE_MODE,_CLUSTER
C       WNB 950630	Add more update options
C       WNB 950706	Add looped updates
C       HjV 951002	Change test in update-part (Change LLOPT3 in LLOPT4)
C       CMV 960122	Print reminder if INTERNAL is on
C	JPH 960130	Truncate .<number> in default MDL name
C	JEN 960403	Add option RMERGE, to merge nearby sources
C	JPH 960614	Zero DLDM
C	WNB 990729	Add X00-X03 as update modes
C	CMV 000928	Remove prefixed * in name of model file
C	CMV 031231	FLUX_KNOWN from byte to logical
C
	SUBROUTINE NMODAT
C
C  Get NMODEL program parameters
C
C  Result:
C
C	CALL NMODAT		
C		Will ask and set all program parameters
C	CALL NMODAW( NGSRC_J:O, STH_B(0:*):I)
C		Will ask and set all program parameters
C		for handle, and return # of sources in general list
C		The default model-file for the read option is
C		derived the sector header STH
C	CALL NMODAV( NGSRC_J:O, STH_B(0:*):I, FLUX_KNOWN_L:I)
C		Idem, and test if the 'unknown-flux' bit is cleared
C		Do not allow the use of INTERNAL
C	CALL NMODAX( NGSRC_J:O)	
C		Idem, but without a default model file
C	CALL NMODAY
C		Will ask for a (possible) output node
C
C  PIN references:
C
C	ACTION
C	FIND_TYPE
C	UPDATE_TYPE
C       UPDATE_MODE
C       UPDATE_CLUSTER
C	MODEL_OPTION
C	OUTPUT_MDL_NODE
C	INPUT_MDL_NODE
C	MDL_NODE
C	WMP_NODE
C	WMP_SETS
C	AREA
C	ID_START
C	MAP_LIMIT
C	MAX_NUMBER
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'NMO_DEF'
	INCLUDE 'CBITS_DEF'
	INCLUDE 'MPH_O_DEF'		!MAP HEADER
	INCLUDE 'MDH_O_DEF'		!MODEL HEADER
	INCLUDE 'MDL_O_DEF'		!MODEL LINE
	INCLUDE 'STH_O_DEF'		!SECTOR HEADER
	INCLUDE 'DLDM_DEF'		! beam offset for beam measurement
C		NOTE: DLDM is cleared for all entry points just to make sure
C		 that a lingering offset will not confuse NMOBMV. This is
C		 probably not necessary for the secondary entry points because
C		 these are called in a context in which a .SCN file is read and
C		 DLDM set by NSCSTG
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER NGSRC			!# OF SOURCES IN GENERAL LIST
	BYTE STH(0:STH__L-1)		!Sector header
	LOGICAL FN			!Return .TRUE. if flux known
C
C  Entry points:
C
C
C  Function references:
C
	INTEGER WNCALN			!SIGNIFICANT LENGTH OF STRING
	LOGICAL WNDPAR			!GET DWARF PARAMETER
	LOGICAL WNDNOD			!GET NODE NAME
	LOGICAL WNFOP			!OPEN FILE
	LOGICAL WNDSTA			!GET WMP_SETS/SCN_SETS TO DO
	LOGICAL NMASTG			!GET A MAP
	LOGICAL NMOSLI			!GET GENERAL SOURCE LIST
	LOGICAL NSCPLS			!GET POL. SELECTION
	LOGICAL NSCHAS			!SELECT HA
	LOGICAL NSCIFS			!Select/deselect interferometers
	LOGICAL NSCSTG			!Get a set
	DOUBLE PRECISION WNGGD		!Get double precision from byte array
C
C  Data declarations:
C
	LOGICAL	DO_UNF			!Test unknown flux bit at exit
	LOGICAL BACK_MOD		!Return-flag for show menu
	LOGICAL INTERNAL_OK		!Allow use of INTERNAL option
	CHARACTER FIELDMDL*128		!Name of modelfile for FIELD option
	CHARACTER LLOPT*24		!Local option
	  CHARACTER*4 LLOPT4
	  EQUIVALENCE (LLOPT,LLOPT4)
	INTEGER TMPCAP,TMPCDAP		!TEMP CORRECTIONS FOR INTERN OPTION
	INTEGER RANGE(0:1)		!SOURCE RANGE
	INTEGER SNAM(0:7)		!SET NAME
	INTEGER MXARE(0:3)		!MAX. SEARCH AREA
	  DATA MXARE/0,0,0,0/
	INTEGER MPHP			!MAP POINTER
	INTEGER STHP			!SET POINTER
	BYTE LBT
	BYTE LSTH(0:STH__L-1)		!SET HEADER
	  DOUBLE PRECISION LSTHD(0:STH__L/LB_D-1)
	  EQUIVALENCE (LSTH,LSTHD)
	BYTE MPH(0:MPHHDL-1)		!MAP HEADER
	  INTEGER MPHJ(0:MPHHDL/4-1)
	  EQUIVALENCE (MPH,MPHJ)
C-
C
C GET ACTION
C
 100	CONTINUE
	DLDM(0)=0				! reset beam
	DLDM(1)=0				!  offset
	IF (.NOT.WNDPAR('ACTION',ACTION,LEN(ACTION),J0,ACTION)) THEN
	  ACTION='QUIT'				!ASSUME END
	ELSE IF (J0.LE.0) THEN
	  ACTION='QUIT'				!ASSUME END
	END IF
C
C FROM/TO OLD
C
	IF (ACT.EQ.'FRO') THEN
 110	  CONTINUE
	  IF (.NOT.WNDPAR('OLD_FILE',FILIN,LEN(FILIN),J0,'""')) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 100	!RETRY ACTION
	    GOTO 110				!REASK
	  ELSE IF (J0.EQ.0) THEN		!RETRY ACTION
	    GOTO 100
	  ELSE IF (J0.LT.0) THEN		!MUST SPECIFY
	    GOTO 110
	  END IF
 111	  CONTINUE
	  IF (.NOT.WNDNOD('OUTPUT_MDL_NODE',' ','MDL',
	1			'W',NODOUT,FILOUT)) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 100	!RETRY ACTION
	    GOTO 111				!REPEAT
	  ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN
	    GOTO 100				!RETRY ACTION
	  ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
	    GOTO 111				!MUST SPECIFY
	  END IF
	  IF (.NOT.WNFOP(FCAOUT,FILOUT,'U')) THEN
	    CALL WNCTXT(F_TP,'!/Cannot open !AS (!XJ)',NODOUT,E_C)
	    GOTO 111				!RETRY
	  END IF
	ELSE IF (ACT.EQ.'TO_') THEN		!TO OLD
 112	  CONTINUE
	  IF (.NOT.WNDNOD('INPUT_MDL_NODE',' ','MDL',
	1			'R',NODOUT,FILOUT)) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 100	!RETRY ACTION
	    GOTO 112				!REPEAT
	  ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN
	    GOTO 100				!RETRY ACTION
	  ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
	    GOTO 112				!MUST SPECIFY
	  END IF
	  IF (.NOT.WNFOP(FCAOUT,FILOUT,'R')) THEN
	    CALL WNCTXT(F_TP,'!/Cannot open !AS (!XJ)',NODOUT,E_C)
	    GOTO 112				!RETRY
	  END IF
 113	  CONTINUE
	  IF (.NOT.WNDPAR('OLD_FILE',FILIN,LEN(FILIN),J0,'""')) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 100	!RETRY ACTION
	    GOTO 113				!REASK
	  ELSE IF (J0.EQ.0) THEN		!RETRY ACTION
	    GOTO 100
	  ELSE IF (J0.LT.0) THEN		!MUST SPECIFY
	    GOTO 113
	  END IF
C
C CONVERT/BEAM/DE-BEAM
C
	ELSE IF (ACT.EQ.'CON' .OR. ACT.EQ.'BEA' .OR. ACT.EQ.'DEB' .OR.
	1		ACT.EQ.'EDI' .OR. ACT.EQ.'FED' .OR.
	1		ACT.EQ.'RED') THEN
 120	  CONTINUE
	  IF (.NOT.WNDNOD('MDL_NODE',' ','MDL','R',NODOUT,FILOUT)) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 100	!RETRY ACTION
	    GOTO 120				!REPEAT
	  ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN
	    GOTO 100				!RETRY ACTION
	  ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
	    GOTO 120				!MUST SPECIFY
	  END IF
	  IF (.NOT.WNFOP(FCAOUT,FILOUT,'R')) THEN
	    CALL WNCTXT(F_TP,'!/Cannot open !AS (!XJ)',NODOUT,E_C)
	    GOTO 120				!RETRY
	  END IF
C
C NEW VERSION
C
	ELSE IF (ACT.EQ.'NVS') THEN
 130	  CONTINUE
	  IF (.NOT.WNDNOD('MDL_NODE',' ','MDL','R',NODOUT,FILOUT)) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 100	!RETRY ACTION
	    GOTO 130				!REPEAT
	  ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN
	    GOTO 100				!RETRY ACTION
	  ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
	    GOTO 130				!MUST SPECIFY
	  END IF
	  IF (.NOT.WNFOP(FCAOUT,FILOUT,'U')) THEN
	    CALL WNCTXT(F_TP,'!/Cannot open !AS (!XJ)',NODOUT,E_C)
	    GOTO 130				!RETRY
	  END IF
C
C CONVERT VAX FORMAT
C
	ELSE IF (ACT.EQ.'CVX') THEN
 131	  CONTINUE
	  IF (.NOT.WNDNOD('MDL_NODE',' ','MDL','R',NODOUT,FILOUT)) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 100	!RETRY ACTION
	    GOTO 131				!REPEAT
	  ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN
	    GOTO 100				!RETRY ACTION
	  ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
	    GOTO 131				!MUST SPECIFY
	  END IF
	  IF (.NOT.WNFOP(FCAOUT,FILOUT,'U')) THEN
	    CALL WNCTXT(F_TP,'!/Cannot open !AS (!XJ)',NODOUT,E_C)
	    GOTO 131				!RETRY
	  END IF
C
C SCAN FILE
C
C GET
C
	ELSE IF (ACT.EQ.'GET') THEN		!GET FROM SCAN FILE
 132	  CONTINUE
	  IF (.NOT.WNDNOD('SCN_NODE',' ','SCN','R',NODIN,FILIN)) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 100	!RETRY ACTION
	    GOTO 132				!REPEAT
	  ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN
	    GOTO 100				!RETRY ACTION
	  ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
	    GOTO 132				!MUST SPECIFY
	  END IF
	  IF (.NOT.WNFOP(FCAIN,FILIN,'R')) THEN
	    CALL WNCTXT(F_TP,'!/Cannot open !AS (!XJ)',NODIN,E_C)
	    GOTO 132				!RETRY
	  END IF
	  IF (.NOT.WNDSTA('SCN_SETS',MXNSET,SETS,FCAIN)) THEN !GET SETS
	    CALL WNFCL(FCAIN)
	    GOTO 132
	  END IF
C
C SAVE
C
	ELSE IF (ACT.EQ.'SAV') THEN		!SAVE MODEL IN SCAN FILE
 133	  CONTINUE
	  IF (.NOT.WNDNOD('SCN_NODE',' ','SCN','R',NODIN,FILIN)) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 100	!RETRY ACTION
	    GOTO 133				!REPEAT
	  ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN
	    GOTO 100				!RETRY ACTION
	  ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
	    GOTO 133				!MUST SPECIFY
	  END IF
	  IF (.NOT.WNFOP(FCAIN,FILIN,'U')) THEN
	    CALL WNCTXT(F_TP,'!/Cannot open !AS (!XJ)',NODIN,E_C)
	    GOTO 133				!RETRY
	  END IF
	  IF (.NOT.WNDSTA('SCN_SETS',MXNSET,SETS,FCAIN)) THEN !GET SETS
	    CALL WNFCL(FCAIN)
	    GOTO 133
	  END IF
C
C UPDATE
C
	ELSE IF (ACT.EQ.'UPD') THEN		!UPDATE MODEL WITH SCAN FILE
	  IF (.NOT.WNDPAR('UPDATE_TYPE',OPTION,LEN(OPTION),J0,'ILM')) THEN
	    OPTION='QUIT'
	  ELSE IF (J0.LE.0) THEN
	    OPTION='QUIT'
	  END IF
	  IF (OPT.EQ.'QUI') GOTO 100		!RETRY ACTION
	  IF (.NOT.WNDPAR('UPDATE_MODE',LLOPT,LEN(LLOPT),J0,'SEP')) THEN
	    OPTION='QUIT'
	  ELSE IF (J0.LE.0) THEN
	    OPTION='QUIT'
	  END IF
	  IF (LLOPT4.EQ.'QUIT') OPTION='QUIT'
	  IF (OPT.EQ.'QUI') GOTO 100		!RETRY ACTION
	  IF (LLOPT4.EQ.'SEPA') THEN
	     OPTION(4:5)='SS'
	  ELSE IF (LLOPT4.EQ.'CLUS') THEN
	     OPTION(4:5)='XS'
	  ELSE IF (LLOPT4.EQ.'COMB') THEN
	     OPTION(4:5)='YS'
	  ELSE IF (LLOPT4.EQ.'CONS') THEN
	     OPTION(4:5)='ZS'
	  ELSE IF (LLOPT4.EQ.'LCLU') THEN
	     OPTION(4:5)='XL'
	  ELSE IF (LLOPT4.EQ.'LCOM') THEN
	     OPTION(4:5)='YL'
	  ELSE IF (LLOPT4.EQ.'LCON') THEN
	     OPTION(4:5)='ZL'
	  ELSE
	     OPTION='QUIT'
	     GOTO 100
	  END IF
 141	  CONTINUE
	  IF (.NOT.WNDNOD('SCN_NODE',' ','SCN','U',NODIN,FILIN)) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 100	!RETRY UNIT
	    GOTO 141				!REPEAT
	  ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN
	    GOTO 100				!RETRY OPTION
	  ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
	    GOTO 141				!MUST SPECIFY
	  END IF
	  IF (.NOT.WNFOP(FCAIN,FILIN,'U')) THEN	!OPEN OUTPUT SCAN FILE
	    GOTO 141				!RETRY
	  END IF
C
	  IF (.NOT.WNDSTA('SCN_SETS',MXNSET,SETS,FCAIN)) THEN !NO SETS
 142	    CONTINUE
	    CALL WNFCL(FCAIN)
	    GOTO 141
	  END IF
	  IF (SETS(0,0).EQ.0) GOTO 142		!NONE SPECIFIED
C
	  IF (.NOT.NSCPLS(2,SPOL)) GOTO 141	!SELECT POL.
C
 143	  CONTINUE
	  IF (.NOT.NSCHAS(1,HARAN)) GOTO 142	!GET HA RANGE
C
	  IF (.NOT.NSCIFS(2,SIFRS)) GOTO 142	!SELECT INTERFEROMETERS
C
	  CALL NSCSAD(CORAP,CORDAP)		!GET CORRECTIONS TO (DE-)APPLY
	  IF (OPTION(4:4).NE.'S') THEN
	     IF (.NOT.NSCSTG(FCAIN,SETS,LSTH,STHP,SNAM)) THEN !FIND A SET
		GOTO 142
	     END IF
	     CALL WNDSTR(FCAIN,SETS) 		!RESET SET SEARCH
	     SORRAN(0)=2*1.5/(3000.*PI2*LSTHD(STH_FRQ_D)/CL/(1.E-6))
	1	  *DEG*3600.
	     SORRAN(1)=SORRAN(0)/ABS(SIN(LSTHD(STH_DEC_D)*DPI2)) !GUESS
	     IF (.NOT.WNDPAR('UPDATE_CLUSTER',SORRAN,2*LB_E,J0,
	1	  A_B(-A_OB),SORRAN,2)) THEN
		GOTO 143			!READY
	     ELSE IF (J0.EQ.0) THEN
		GOTO 143			!READY
	     ELSE IF (J0.LT.0) THEN		!ASSUME DEFAULT
		SORRAN(0)=2*1.5/(3000.*PI2*LSTHD(STH_FRQ_D)/CL/(1.E-6))
	1	     *DEG*3600.
		SORRAN(1)=SORRAN(0)/ABS(SIN(LSTHD(STH_DEC_D)*DPI2)) !GUESS
	     END IF
	     IF (SORRAN(0).LE.0.1) THEN
		SORRAN(0)=2*1.5/(3000.*PI2*LSTHD(STH_FRQ_D)/CL/(1.E-6))
	1	     *DEG*3600.
	     END IF
	     IF (SORRAN(1).LE.0.1) THEN
		SORRAN(1)=SORRAN(0)/ABS(SIN(LSTHD(STH_DEC_D)*DPI2)) !GUESS
	     END IF
	     DO I=0,1
		SORRAN(I)=SORRAN(I)/3600./DEG/2	!MAKE RADIANS HALF AXES
	     END DO
	  END IF
C
C MAP FIND
C
	ELSE IF (ACT.EQ.'FIN') THEN		!FIND SOURCES IN MAP
	  IF (.NOT.WNDPAR('FIND_TYPE',OPTION,LEN(OPTION),J0,'POS')) THEN
	    OPTION='QUIT'
	  ELSE IF (J0.LE.0) THEN
	    OPTION='QUIT'
	  END IF
	  IF (OPT.EQ.'QUI') GOTO 100		!RETRY ACTION
 134	  CONTINUE
	  IF (.NOT.WNDNOD('WMP_NODE',' ','WMP','R',NODIN,FILIN)) THEN !NODE
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 100	!RETRY ACTION
	    GOTO 134				!REPEAT
	  ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN
	    GOTO 100				!RETRY ACTION
	  ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
	    GOTO 134				!MUST SPECIFY
	  END IF
	  IF (.NOT.WNFOP(FCAIN,FILIN,'R')) THEN
	    CALL WNCTXT(F_TP,'!/Cannot open !AS (!XJ)',NODIN,E_C)
	    GOTO 134				!RETRY
	  END IF
	  IF (.NOT.WNDSTA('WMP_SETS',MXNSET,SETS,FCAIN)) GOTO 134 !GET MAPS TO SEARCH
	  IF (.NOT.NMASTG(FCAIN,SETS,MPH,MPHP,SNAM)) GOTO 134 !GET A MAP
	  CALL WNDSTR(FCAIN,SETS)		!RESET MAP SEARCH
	  DO I=0,3				!DEFAULT AREA
	    TAREA(I,0)=0
	    FAREA(I)=0
	  END DO
	  FAREA(2)=MPHJ(MPH_NRA_J)		!LENGTH LINE
	  FAREA(3)=MPHJ(MPH_NDEC_J)
	  TAREA(2,0)=FAREA(2)			!DEFAULT=FULL
	  TAREA(3,0)=FAREA(3)
	  MXARE(2)=FAREA(2)			!MAX=FULL
	  MXARE(3)=FAREA(3)
	  IF (OPT.NE.'MAN') THEN		!NOT MANUAL
	    CALL NMADAR(MXNAR,NAREA,FAREA,4,MXARE,TAREA(0,0),PAREA(0,1,0),
	1		TAREA(0,1),PAREA(0,1,1)) !GET AREAS
	  ELSE
	    CALL NMADAR(1,NAREA,FAREA,4,MXARE,TAREA(0,0),PAREA(0,1,0),
	1		TAREA(0,1),PAREA(0,1,1)) !GET AREAS
	  END IF
	  IF (NAREA.LE.0) GOTO 134		!NO AREA GIVEN
 135	  CONTINUE
	  IF (OPT.NE.'MAN') THEN		!NOT MANUAL
	    IF (.NOT.WNDPAR('MAP_LIMIT',MAPLIM,LB_E,J0,'0.1')) THEN !FLUX LIMIT
	      IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 134 !RETRY ACTION
	      GOTO 135				!REPEAT
	    END IF
	    IF (J0.EQ.0) GOTO 134
	    IF (J0.LT.0) MAPLIM=0.1
	    IF (.NOT.WNDPAR('MAX_NUMBER',MAXSRN,LB_J,J0,'20')) THEN !MAX.# SOURCES
	      IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 134 !RETRY ACTION
	      GOTO 135				!REPEAT
	    END IF
	  ELSE
	    MAXSRN=100				!TO START WITH
	  END IF
	  IF (J0.EQ.0) GOTO 134
	  IF (J0.LT.0) MAXSRN=20
	  IF (.NOT.WNDPAR('ID_START',IDEN,LB_J,J0,'1000')) THEN !ID START
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 134	!RETRY ACTION
	    GOTO 135				!REPEAT
	  END IF
	  IF (J0.EQ.0) GOTO 134
	  IF (J0.LT.0) IDEN=1000
	END IF
C
	RETURN					!RETURN
C
C GET ALL HANDLE OPTIONS
C
	ENTRY NMODAV(NGSRC,STH,FN)
C
	DO_UNF=.TRUE.					!Do test
	INTERNAL_OK=.FALSE.				!Do not allow
	GOTO 17
C
	ENTRY NMODAW(NGSRC,STH)
C	
	DO_UNF=.FALSE.					!Do not test
	INTERNAL_OK=.TRUE.				!Allow
	GOTO 17
C
C Construct default name
C
  17	CONTINUE
	CALL WNGMTS(STH_FIELD_N,STH(STH_FIELD_1),FIELDMDL) !GET FIELD NAME
	IF (FIELDMDL(1:1).EQ.'*') FIELDMDL=FIELDMDL(2:)    !STRIP LEADING *
  	I1=INDEX(FIELDMDL,'.')-1
  	IF (I1.LT.0) I1=WNCALN(FIELDMDL)
C	D0=WNGGD(STH(STH_FRQ_1))
	CALL WNGMV(LB_D,STH(STH_FRQ_1),D0)
	IF (D0.GT.290.AND.D0.LT.385) THEN
	   FIELDMDL=FIELDMDL(1:I1)//'_92cm'
	ELSE IF (D0.GT.600.AND.D0.LT.620) THEN
	   FIELDMDL=FIELDMDL(1:I1)//'_49cm'
	ELSE IF (D0.GT.1250.AND.D0.LT.1500) THEN
	   FIELDMDL=FIELDMDL(1:I1)//'_21cm'
	ELSE IF (D0.GT.4000.AND.D0.LT.6000) THEN
	   FIELDMDL=FIELDMDL(1:I1)//'_6cm'
	ELSE
	   FIELDMDL=' '					!Unknown
	END IF
C	
	GOTO 12
C
	ENTRY NMODAX(NGSRC)
C
	FIELDMDL=' '					!No default
	DO_UNF=.FALSE.					!Do not test
	INTERNAL_OK=.TRUE.				!Allow
C
 12	CONTINUE
	DLDM(0)=0				! (probably unnecessary)
	DLDM(1)=0
	IF (.NOT.NMOSLI(1024)) GOTO 800		!MAKE SURE THERE IS A LIST
	ACTION='HANDLE'				!MAKE SURE THERE IS ACTION
	CALL WNCTXT(F_TP,'There are !UJ sources in the list',
	1		GDESJ(MDH_NSRC_J))
 10	CONTINUE
	IF (.NOT.WNDPAR('MODEL_OPTION',OPTION,LEN(OPTION),J0,'QUIT')) THEN
	  OPTION='QUIT'
	ELSE IF (J0.LE.0) THEN
	  OPTION='QUIT'
	END IF
	IF (OPT.EQ.'QUI') GOTO 800		!READY
C
C WRITE
C
 11	CONTINUE
	IF (OPT.EQ.'WRI') THEN
	  IF (.NOT.WNDNOD('OUTPUT_MDL_NODE',' ','MDL',
	1			'W',NODOUT,FILOUT)) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 10	!RETRY OPTION
	    GOTO 11				!REPEAT
	  ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN
	    GOTO 10				!RETRY OPTION
	  ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
	    GOTO 11				!MUST SPECIFY
	  END IF
	  IF (.NOT.WNFOP(FCAOUT,FILOUT,'U')) THEN
	    CALL WNCTXT(F_TP,'!/Cannot open !AS (!XJ)',NODOUT,E_C)
	    GOTO 11				!RETRY
	  END IF
	  CALL NMOWRS(FCAOUT,GDES)		!WRITE
C
C READ
C
	ELSE IF (OPT.EQ.'REA') THEN
	  IF (.NOT.WNDNOD('INPUT_MDL_NODE',FIELDMDL,'MDL',
	1			  'R',NODOUT,FILOUT)) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 10 !RETRY OPTION
	    GOTO 11				 !REPEAT
	  ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN
	    GOTO 10				 !RETRY OPTION
	  ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
	    GOTO 11				 !MUST SPECIFY
	  END IF
	  IF (.NOT.WNFOP(FCAOUT,FILOUT,'R')) THEN
	    CALL WNCTXT(F_TP,'!/Cannot open !AS (!XJ)',NODOUT,E_C)
	    FIELDMDL=' '			!Forget if not found
	    GOTO 11				!RETRY
	  END IF
	  CALL NMORDS(FCAOUT)			!READ
	  CALL NMORDM(7,-1)			!AND ADD TO GENERAL AREA
	  CALL WNCTXT(F_TP,'There are !UJ sources in the list now',
	1		GDESJ(MDH_NSRC_J))
C
C CLEAR
C
	ELSE IF (OPT.EQ.'CLE') THEN
  19	  CONTINUE
	  LBT=.FALSE.
	  IF (.NOT.WNDPAR('CLEAR_REF',LBT,LB_B,J0,'NO')) THEN
	    GOTO 19				!RETRY
	  END IF
	  IF (LBT) THEN
	     CALL NMOHZD(GDES)
	  ELSE
	     GDESJ(MDH_NSRC_J)=0
	  END IF
C
C ADD
C
	ELSE IF (OPT.EQ.'ADD') THEN
	  CALL NMOADD
C
C INTERN
C
	ELSE IF (OPT.EQ.'INT') THEN
	  IF (INTERNAL_OK) THEN
	    CALL WNDDAP(TMPCAP,TMPCDAP) 		!Get current settings
	    IF (IAND(TMPCDAP,CAP_MOD).NE.0) THEN	!Make test if model is on
	       CALL WNCTXT(F_TP,'REMINDER: Internal model is selected')
	    END IF
	    TMPCDAP=IOR(TMPCDAP,CAP_MOD)		!Make sure model is on
	    CALL WNDDAP_SET(TMPCAP,TMPCDAP)	!Store new settings
	    CALL WNCTXT(F_TP+F_P1,'DE_APPLY += MODEL')
	  ELSE
	    CALL WNCTXT(F_TP,'The use of the INTERNAL option is '//
	1		     'NOT allowed in this context.')
	  END IF
C
C SHOW
C
	ELSE IF (OPT.EQ.'SHO') THEN
	  OPTION='SHOW'
	  BACK_MOD=.FALSE.
  188	  CONTINUE
	  IF (.NOT.WNDPAR('MODEL_SHOW',OPTION,
	1	           LEN(OPTION),J0,OPTION)) THEN
	    IF (BACK_MOD) THEN
	       GOTO 18				!BACK TO MODEL_MODIFY
	    ELSE
	       GOTO 10				!BACK TO MODEL_OPTION
	    END IF
	  ELSE IF (J0.LE.0.OR.OPT.EQ.'QUI') THEN
	    IF (BACK_MOD) THEN
	       GOTO 18				!BACK TO MODEL_MODIFY
	    ELSE
	       GOTO 10				!BACK TO MODEL_OPTION
	    END IF
	  END IF
C
	  IF (.NOT.WNDPAR('SOURCE_RANGE',RANGE,2*L_J/L_B,J0,'*')) THEN
	    GOTO 188				!RETRY
	  END IF
	  IF (J0.EQ.0) THEN
	    GOTO 188				!""
	  ELSE IF (J0.EQ.-1) THEN
	    RANGE(0)=1				!DEFAULT RANGE
	    RANGE(1)=1000000
	  ELSE IF (J0.EQ.1) THEN
	    RANGE(1)=1000000
	  END IF
	  J=F_TP				!ASSUME BOTH
	  IF (OPT.EQ.'LIS' .OR. OPT.EQ.'RLI') J=F_P
	  IF (OPT.EQ.'TOT') THEN		!TOTAL
	    CALL NMOPTT(J,RANGE)
	  ELSE IF (OPT(1:1).EQ.'R') THEN	!RA/DEC
	    CALL NMOPRR(J,RANGE)
	  ELSE
	    CALL NMOPRT(J,RANGE)
	  END IF
	  GOTO 188				!Back to MODEL_SHOW
C
C MODIFY
C
	ELSE IF (OPT.EQ.'MOD') THEN
  18	  CONTINUE
	  IF (.NOT.WNDPAR('MODEL_MODIFY',OPTION,
	1	           LEN(OPTION),J0,'QUIT')) THEN
	    GOTO 10				!RETRY
	  ELSE IF (J0.LE.0) THEN
	    GOTO 18				!RETRY
	  END IF
	  IF (OPT.EQ.'QUI') GOTO 10		!RETURN TO MODEL_OPTION
C
C SHOW
C
	  IF (OPT.EQ.'SHO') THEN
	     OPTION='SHOW'
	     BACK_MOD=.TRUE.
	     GOTO 188
C
C FLUX_KNOWN
C
	  ELSE IF (OPT.EQ.'FLU') THEN
	     IF (IAND(GDESJ(MDH_BITS_J),MDHUNF_M).EQ.0) THEN	!BIT OFF
		GDESJ(MDH_BITS_J)=
	1	     IOR(GDESJ(MDH_BITS_J),MDHUNF_M)		!SET BIT
	        CALL WNCTXT(F_TP,'The Unknown-Flux bit is now on')
	     ELSE						!BIT ON
		GDESJ(MDH_BITS_J)=
	1	     IAND(GDESJ(MDH_BITS_J),.NOT.MDHUNF_M)	!RESET BIT
	        CALL WNCTXT(F_TP,'The Unknown-Flux bit is now off')
	     END IF
C
C EDIT
C
	  ELSE IF (OPT.EQ.'EDI') THEN
	    CALL NMOAED
	  ELSE IF (OPT.EQ.'FED') THEN		!EDIT FIELD
	    IF (.NOT.WNDPAR('EDIT_FIELD',OPTION,LEN(OPTION),J0,
	1		'""')) THEN		!EDIT FIELD
	      GOTO 18				!RETRY OPTION
	    ELSE IF (J0.LE.0) THEN
	      GOTO 18
	    END IF
	    IF (OPT.EQ.'I') THEN
	      CALL NMOAFE(MDL_I_E)
	    ELSE IF (OPT.EQ.'L') THEN
	      CALL NMOAFE(MDL_L_E)
	    ELSE IF (OPT.EQ.'M') THEN
	      CALL NMOAFE(MDL_M_E)
	    ELSE IF (OPT.EQ.'Q') THEN
	      CALL NMOAFE(MDL_Q_E)
	    ELSE IF (OPT.EQ.'U') THEN
	      CALL NMOAFE(MDL_U_E)
	    ELSE IF (OPT.EQ.'V') THEN
	      CALL NMOAFE(MDL_V_E)
	    ELSE IF (OPT.EQ.'ID') THEN
	      CALL NMOAFJ(MDL_ID_J)
	    ELSE IF (OPT.EQ.'SI') THEN
	      CALL NMOAFE(MDL_SI_E)
	    ELSE IF (OPT.EQ.'RM') THEN
	      CALL NMOAFE(MDL_RM_E)
	    ELSE IF (OPT.EQ.'LA') THEN
	      CALL NMOAFE(MDL_EXT_E)
	    ELSE IF (OPT.EQ.'SA') THEN
	      CALL NMOAFE(MDL_EXT_E+1)
	    ELSE IF (OPT.EQ.'PA') THEN
	      CALL NMOAFE(MDL_EXT_E+2)
	    ELSE IF (OPT.EQ.'BIT') THEN
	      CALL NMOAFB(MDL_BITS_B)
	    ELSE IF (OPT.EQ.'TYP') THEN
	      CALL NMOAFB(MDL_TP1_B)
	    ELSE IF (OPT.EQ.'CCB') THEN
	      CALL NMOAFB(MDL_TP_B)
	    ELSE IF (OPT.EQ.'TP2') THEN
	      CALL NMOAFB(MDL_TP2_B)
	    END IF
C
C SORT
C
	  ELSE IF (OPT.EQ.'SOR') THEN		!SORT I
	    CALL NMOSRT(0,GDESJ)
	  ELSE IF (OPT.EQ.'FSO') THEN		!SORT FIELD
	    IF (.NOT.WNDPAR('SORT_TYPE',OPTION,LEN(OPTION),J0,
	1		'DECREASING')) THEN	!SORT TYPE
	      GOTO 18				!RETRY OPTION
	    ELSE IF (J0.LE.0) THEN
	      GOTO 18
	    END IF
	    IF (OPT.EQ.'DEC') THEN		!DECREASING
	      SORTYP=-1
	    ELSE
	      SORTYP=+1
	    END IF
	    IF (.NOT.WNDPAR('SORT_FIELD',OPTION,LEN(OPTION),J0,
	1		'""')) THEN		!SORT FIELD
	      GOTO 18				!RETRY OPTION
	    ELSE IF (J0.LE.0) THEN
	      GOTO 18
	    END IF
	    IF (OPT.EQ.'I') THEN
	      SOROFF=MDL_I_E
	      CALL NMOSRT(3,GDESJ)
	    ELSE IF (OPT.EQ.'L') THEN
	      SOROFF=MDL_L_E
	      CALL NMOSRT(3,GDESJ)
	    ELSE IF (OPT.EQ.'M') THEN
	      SOROFF=MDL_M_E
	      CALL NMOSRT(3,GDESJ)
	    ELSE IF (OPT.EQ.'Q') THEN
	      SOROFF=MDL_Q_E
	      CALL NMOSRT(3,GDESJ)
	    ELSE IF (OPT.EQ.'U') THEN
	      SOROFF=MDL_U_E
	      CALL NMOSRT(3,GDESJ)
	    ELSE IF (OPT.EQ.'V') THEN
	      SOROFF=MDL_V_E
	      CALL NMOSRT(3,GDESJ)
	    ELSE IF (OPT.EQ.'SI') THEN
	      SOROFF=MDL_SI_E
	      CALL NMOSRT(3,GDESJ)
	    ELSE IF (OPT.EQ.'RM') THEN
	      SOROFF=MDL_RM_E
	      CALL NMOSRT(3,GDESJ)
	    ELSE IF (OPT.EQ.'LA') THEN
	      SOROFF=MDL_EXT_E
	      CALL NMOSRT(3,GDESJ)
	    ELSE IF (OPT.EQ.'SA') THEN
	      SOROFF=MDL_EXT_E+1
	      CALL NMOSRT(3,GDESJ)
	    ELSE IF (OPT.EQ.'PA') THEN
	      SOROFF=MDL_EXT_E+2
	      CALL NMOSRT(3,GDESJ)
	    ELSE IF (OPT.EQ.'BIT') THEN
	      SOROFF=MDL_BITS_B
	      CALL NMOSRT(4,GDESJ)
	    ELSE IF (OPT.EQ.'TYP') THEN
	      SOROFF=MDL_TP1_B
	      CALL NMOSRT(4,GDESJ)
	    ELSE IF (OPT.EQ.'CCB') THEN
	      SOROFF=MDL_TP_B
	      CALL NMOSRT(4,GDESJ)
	    ELSE IF (OPT.EQ.'TP2') THEN
	      SOROFF=MDL_TP2_B
	      CALL NMOSRT(4,GDESJ)
	    ELSE IF (OPT.EQ.'ID') THEN
	      SOROFF=MDL_ID_J
	      CALL NMOSRT(2,GDESJ)
	    ELSE IF (OPT.EQ.'LM') THEN
	      CALL NMOSRT(6,GDESJ)
	    ELSE IF (OPT.EQ.'ML') THEN
	      CALL NMOSRT(7,GDESJ)
	    ELSE IF (OPT.EQ.'POL') THEN
	      CALL NMOSRT(8,GDESJ)
	    ELSE IF (OPT.EQ.'DIS') THEN
	      SORRAN(0)=0				!START
	      SORRAN(1)=0
	      IF (.NOT.WNDPAR('SORT_CENTRE',SORRAN,2*L_E/L_B,J0,
	1		A_B(-A_OB),SORRAN,2)) THEN !GET CENTRE
	        GOTO 18				!RETRY OPTION
	      END IF
	      IF (J0.LE.0) GOTO 18		!RETRY OPTION
	      SORRAN(0)=SORRAN(0)/3600./DEG	!MAKE RADIANS
	      SORRAN(1)=SORRAN(1)/3600./DEG
	      CALL NMOSRT(5,GDESJ)
	    END IF				!UNKNOWN
C
C MERGE
C
	  ELSE IF (OPT.EQ.'MER') THEN
	    CALL NMOAMG                         ! exact coincidence          
	  ELSE IF (OPT.EQ.'RME') THEN
	    CALL NMOAMR                         ! within given radius
C
C DELETE
C
	  ELSE IF (OPT.EQ.'DEL') THEN
	    CALL NMOADL
	  ELSE IF (OPT.EQ.'DNC') THEN
	    CALL NMOANC
	  ELSE IF (OPT.EQ.'DCL') THEN
	    CALL NMOACD
	  ELSE IF (OPT.EQ.'DAR') THEN
	    CALL NMOAAD
C
C CALIBRATE
C
	  ELSE IF (OPT.EQ.'CAL') THEN
	    CALL NMOADC
	  END IF
C
	  GOTO 18
C
	END IF
C
C MORE
C
	GOTO 10					!NEXT OPTION
C
C READY WITH DAX
C
 800	CONTINUE
	CALL WNCTXT(F_TP,'!/!UJ sources in list!/',
	1		GDESJ(MDH_NSRC_J))
	NGSRC=GDESJ(MDH_NSRC_J)			!# OF SOURCES IN LIST
C
	IF (DO_UNF) THEN
	   IF (IAND(GDESJ(MDH_BITS_J),MDHUNF_M).NE.0) THEN	!BIT OFF
	      FN=.FALSE.
	      CALL WNCTXT(F_TP,'Fluxes are considered unknown')
	   ELSE
	      FN=.TRUE.
	   ENDIF
	END IF
C
	RETURN					!READY
C
C NMODAY
C
	ENTRY NMODAY
C
	DLDM(0)=0				! (probably unecessary)
	DLDM(1)=0
 20	CONTINUE
	CALL WNFCL(FCAOUT)			!MAKE SURE NONE OPEN
	IF (.NOT.WNDNOD('OUTPUT_MDL_NODE',' ','MDL','W',NODOUT,FILOUT)) THEN
	  IF (E_C.EQ.DWC_ENDOFLOOP) RETURN	!NOT WANTED
	  GOTO 20				!REPEAT
	ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN
	  RETURN				!NOT WANTED
	ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
	  RETURN				!NOT WANTED
	END IF
	IF (.NOT.WNFOP(FCAOUT,FILOUT,'U')) THEN
	  CALL WNCTXT(F_TP,'!/Cannot open !AS (!XJ)',NODOUT,E_C)
	  GOTO 20				!RETRY
	END IF
C
	RETURN
C
C
	END
