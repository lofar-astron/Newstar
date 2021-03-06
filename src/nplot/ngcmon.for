C+ NGCMON.FOR
C  WNB 920821
C
C  Revisions:
C	HjV 930423	Change name of some keywords
C	CMV 931210	Add 'NGF_LOOPS' argument to WNDXLP
C	CMV 931220	Pass FCA of input file to WNDXLP and WNDSTA/Q
C	JPH 940818	NGF_LOOPS before NGF_SETS as in other Newstar programs
C
C
	SUBROUTINE NGCMON
C
C  Make MONGO file
C
C  Result:
C
C	CALL NGCMON			Make MONGO output file
C
C
C  Pin references:
C
C	NGF_SETS	Plots to show
C	PLOT_TYPE	Output type
C	MONGO_FILE	MONGO file name
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'NGF_O_DEF'		!PLOT HEADER
	INCLUDE 'NGC_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
C
C  Function references:
C
	LOGICAL WNDPAR			!GET USER DATA
	LOGICAL WNFRD			!READ DISK
	LOGICAL WNGGVA			!GET MEMORY
	LOGICAL WNDSTA			!ASK PLOTS
	LOGICAL WNDXLP			!ASK LOOPS
	LOGICAL WNDXLN			!LOOP
	INTEGER WNCALN			!STRING LENGTH
	CHARACTER*20 WNFFNM		!GET A FILE NAME
	LOGICAL NGCSTL			!GET PLOT
C
C  Data declarations:
C
	INTEGER NPLOT			!# OF PLOTS TO SHOW
	INTEGER NMON			!# OF COLUMNS
	INTEGER MONPT(MXNMON)		!PLOT HEADER POINTERS
	INTEGER NGFP			!PLOT HEADER POINTER
	INTEGER SNAM(0:7)		!PLOT NAME
	LOGICAL LFIRST			!FIRST LOOP
	INTEGER BUFAD			!DATA BUFFER
	INTEGER BUFL			!DATA BUFFER LENGTH
	REAL MONDAT(0:MXNMON)		!DATA
	CHARACTER*80 MONFIL		!MONGO FILE NAME
	REAL HA				!START HA
	INTEGER NPTS			!NUMBER OF MONGO LINES
	REAL HAINC			!HA INCREMENT
	BYTE NGF(0:NGFHDL-1,MXNMON)	!PLOT HEADER
	  INTEGER*2 NGFI(0:NGFHDL/2-1,MXNMON)
	  INTEGER NGFJ(0:NGFHDL/4-1,MXNMON)
	  REAL NGFE(0:NGFHDL/4-1,MXNMON)
	  CHARACTER*(NGFHDL) NGFC(MXNMON)
	  EQUIVALENCE (NGF,NGFC,NGFI,NGFJ,NGFE)
C-
C
C INIT
C
	OPTION='COS'				!ASSUME DEFAULT PLOT
C
C SELECT PLOTS
C
 10	CONTINUE
	IF (.NOT.WNDXLP('NGF_LOOPS',FCAOUT)) GOTO 10 !GET LOOPS
	LFIRST=.TRUE.				!FIRST LOOP
 20	CONTINUE
	IF (.NOT.WNDSTA('NGF_SETS',MXNSET,SETS,FCAOUT)) GOTO 10 
						!GET PLOTS TO USE
	IF (SETS(0,0).EQ.0) GOTO 900		!READY
	CALL WNDXLI(LPOFF)			!INIT. LOOPS
	IF (.NOT.WNDXLN(LPOFF)) GOTO 10		!NO MORE IN LOOP
C
C FIND PLOTS TO USE
C
	NMON=0					!# TO DO
	DO WHILE(NGCSTL(FCAOUT,SETS,NGF,NGFP,SNAM,LPOFF)) !GET PLOT
	  IF (NMON.LT.MXNMON) THEN
	    NMON=NMON+1				!COUNT
	    MONPT(NMON)=NGFP			!POINTER
	  END IF
	END DO
	IF (NMON.LE.0) GOTO 20			!READY, NEXT LOOP
C
C DO OUTPUT
C
 31	CONTINUE
	MONFIL=WNFFNM('NGC','MON')		!UNIQUE NAME
	IF (LFIRST) THEN			!FIRST LOOP
	  IF (.NOT.WNDPAR('MONGO_FILE',MONFIL,LEN(MONFIL),
	1			J0,MONFIL)) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 10	!RETRY FROM START
	    GOTO 31				!RETRY
	  END IF
	  IF (J0.LT.0) GOTO 31			!MUST SPECIFY
	  IF (J0.EQ.0) GOTO 10
 32	  CONTINUE
	  IF (.NOT.WNDPAR('PLOT_TYPE',OPTION,LEN(OPTION),
	1			J0,OPTION)) THEN !GET TYPE OF PLOT
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 31	!RETRY FILE
	    GOTO 32				!RETRY
	  END IF
	  IF (J0.LT.0) OPTION='COS'		!DEFAULT
	  IF (J0.EQ.0) GOTO 31			!RETRY FILE
	END IF
	LFIRST=.FALSE.				!NOT FIRST
C
C OPEN MONGO FILE
C
	CALL WNCFSV(F_0,F_LL,230)		!MAX. LINE LENGTH
	CALL WNCFSV(F_0,F_PL,0)			!NO PAGING
	CALL WNCFSV(F_0,F_DIS,F_YES)		!KEEP FILE
	CALL WNCFOP(F_0,MONFIL(1:WNCALN(MONFIL))) !OPEN OUTPUT FILE
C
C READ PLOT HEADERS
C
 33	CONTINUE
	DO I=1,NMON				!READ PLOT HEADERS
	  IF (.NOT.WNFRD(FCAOUT,NGFHDL,NGF(0,I),MONPT(I))) THEN
 36	    CONTINUE
	    CALL WNCTXT(F_TP,'!/Error reading plot file')
 34	    CONTINUE
	    CALL WNCFCL(F_0)			!CLOSE FILE
	    GOTO 900
	  END IF
	END DO
C
C FIND RANGE
C
	NPTS=NGFJ(NGF_SCN_J,1)			!INIT VALUES
	HA=NGFE(NGF_HAB_E,1)*360.
	HAINC=NGFE(NGF_HAI_E,1)*360.
	DO I=2,NMON
	  NPTS=MAX(NPTS,NGFJ(NGF_SCN_J,I))
	  HA=MIN(HA,NGFE(NGF_HAB_E,I)*360.)
	  HAINC=MIN(HAINC,NGFE(NGF_HAI_E,I)*360.)
	END DO
C
C GET DATA
C
	BUFL=LB_X*NMON*NPTS			!GET DATA BUFFER
	IF(.NOT.WNGGVA(BUFL,BUFAD)) THEN
	  CALL WNCTXT(F_TP,'!/Cannot get data buffer')
	  GOTO 34
	END IF
	BUFAD=(BUFAD-A_OB)/LB_X			!DATA POINTER
	DO I=1,NMON				!READ DATA
	  IF (.NOT.WNFRD(FCAOUT,LB_X*NGFJ(NGF_SCN_J,I),
	1		A_X(BUFAD+(I-1)*NPTS),NGFJ(NGF_DPT_J,I))) GOTO 36
	END DO
C
C DO ACTUAL OUTPUT
C
	DO I1=1,NPTS				!ALL DATA
	  MONDAT(0)=HA
	  DO I=1,NMON				!ALL SETS
	    R0=(HA/360.-NGFE(NGF_HAB_E,I))/NGFE(NGF_HAI_E,I) !OFFSET
	    I2=NINT(R0)				!SELECT POINT
	    IF (I2.LT.0 .OR. I2.GE.NGFJ(NGF_SCN_J,I)) THEN
	      MONDAT(I)=NGCDLC			!SET DELETED
	    ELSE
	      IF (REAL(A_X(BUFAD+(I-1)*NPTS+I2)).EQ.NGCDLC) THEN
	        MONDAT(I)=NGCDLC		!DELETED DATA
	      ELSE IF (OPT(1:1).EQ.'S') THEN
	        MONDAT(I)=AIMAG(A_X(BUFAD+(I-1)*NPTS+I2))
	      ELSE IF (OPT(1:1).EQ.'A') THEN
	        MONDAT(I)=ABS(A_X(BUFAD+(I-1)*NPTS+I2))
	      ELSE IF (OPT(1:1).EQ.'P') THEN
		IF (REAL(A_X(BUFAD+(I-1)*NPTS+I2)).EQ.0) THEN
	          MONDAT(I)=SIGN(PI/2.,AIMAG(A_X(BUFAD+(I-1)*NPTS+I2)))
	        ELSE
	          MONDAT(I)=ATAN2(AIMAG(A_X(BUFAD+(I-1)*NPTS+I2)),
	1			REAL(A_X(BUFAD+(I-1)*NPTS+I2)))
	        END IF
	      ELSE
	        MONDAT(I)=REAL(A_X(BUFAD+(I-1)*NPTS+I2))
	      END IF
	    END IF
	    IF (MONDAT(I).EQ.NGCDLC) GOTO 40	!SKIP LINE
	  END DO
	  CALL WNCTXT(F_0,'!Q1!15$#E6',NMON+1,MONDAT(0)) !WRITE LINE
 40	  CONTINUE
	  HA=HA+HAINC
	END DO
	CALL WNGFVA(BUFL,BUFAD*LB_X+A_OB)	!RELEASE MEMORY
	CALL WNCFCL(F_0)			!CLOSE FILE
C
C FINISH
C
	CALL WNCTXT(F_TP,'!/Mongo file !AS produced with !UJ columns, '//
	1		'!UJ lines!/',
	1		MONFIL,NMON+1,NPTS)
	GOTO 20					!LOOP
C
C READY
C
 900	CONTINUE
C
	RETURN
C
C
	END
