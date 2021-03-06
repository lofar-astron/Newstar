C+ NMACVL.FOR
C  WNB 910307
C
C  Revisions:
C	WNB 910730	Reverse order FFT
C	WNB 911009	Typo in declarations
C	WNB 911105	Change RAO/DECO definition
C	WNB 920406	Typo in map output UV plane
C	WNB 920423	Type correct set #
C	WNB 920423	Make max/min for UV output correct for size
C	WNB 920828	Update line frequencies and instrument type
C	WNB 921104	J2000
C	WNB 930127	New bandwidth
C	WNB 930510	Make sure even sized output
C
	SUBROUTINE NMACVL(FIN,FOUT,BAD,NP)
C
C  Do convolution to rectangular grid
C
C  Result:
C
C	CALL NMACVL ( FIN_J:I, FOUT_J:I, BAD_J(4,0:*):I, NP_J:I)
C					Do convolution to rectangular grid,
C					using sorted data in FIN, and writing
C					convolved data to FOUT. BAD is the
C					bin administration. NP is the
C					polarisation.
C
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'GFH_O_DEF'		!GENERAL FILE HEADER
	INCLUDE 'SGH_O_DEF'		!SUB-GROUP HEADER
	INCLUDE 'MPH_O_DEF'		!MAP HEADER
	INCLUDE 'NMA_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER FIN			!SORTED INPUT FILE
	INTEGER FOUT			!CONVOLVED OUTPUT FILE
	INTEGER BAD(4,0:*)		!SORT BIN ADMINISTRATION
	INTEGER NP			!POLARISATION TO DO (0,1..)
C
C  Function references:
C
	LOGICAL WNGGVA			!GET VIRTUAL MEMORY
	LOGICAL WNFOP			!OPEN FILE
	LOGICAL WNFRD			!READ FILE
	LOGICAL WNFWR,WNFWRS		!WRITE FILE
	INTEGER WNFEOF			!FILE POINTER
	LOGICAL WNDLNG,WNDLNF		!LINK SUB-GROUP
	LOGICAL WNDLNK			!LINK SET
	INTEGER WNMEJC			!CEIL(X)
	INTEGER WNMEJF			!FLOOR(X)
	CHARACTER*20 WNFFNM		!GET FILE NAME
C
C  Data declarations:
C
	INTEGER CVFLEN			!LENGTH CONVOL. TABLE (BYTES)
	INTEGER CVLFUN			!ADDRESS CONVOLUTION FUNCTION
	INTEGER MAPBUF,MAPBFX		!ADDRESS MAP CONVOL. BUFFER
	INTEGER APBUF,APBUFE		!ADDRESS AP CONVOL. BUFFER
	INTEGER FTBUF,FTBUFE,FTBUFX	!FFT BUFFER
	INTEGER WTBUF,WTBUFX		!FFT WEIGHT BUFFER
	INTEGER UVBUF(NOPT),UVBUFE(NOPT) !UV OUTPUT BUFFER
	INTEGER UVPTR(NOPT)		!UV OUTPUT POINTER
	INTEGER UOUT			!CURRENT U OUTPUT
	REAL UVMN(NOPT),UVMX(NOPT)	!MIN/MAX
	INTEGER MINR(NOPT),MAXR(NOPT),MIND(NOPT),MAXD(NOPT) !POS. MIN/MAX
	REAL BUF(0:MXSBJ-1)		!INPUT BUFFER
	  INTEGER JBUF(0:MXSBJ-1)
	  EQUIVALENCE (BUF,JBUF)
	BYTE MPH(0:MPHHDL-1)		!MAP HEADER
	  INTEGER*2 MPHI(0:MPHHDL/2-1)
	  INTEGER MPHJ(0:MPHHDL/4-1)
	  REAL MPHE(0:MPHHDL/4-1)
	  DOUBLE PRECISION MPHD(0:MPHHDL/8-1)
	  EQUIVALENCE (MPH,MPHI,MPHJ,MPHE,MPHD)
C-
C
C INIT
C
	UHIGH=2*((UVCMAX(0)-1)/2)+1		!MAXIMUM U COORDINATE
	VHIGH=UVCMAX(1)				!MAXIMUM V COORDINATE
	VLOW=-UVCMAX(1)				!MINIMUM V COORDINATE
	VSIZE=2*UVCMAX(1)+1			!LENGTH CONVOLUTION LINE
	USIZE=WNMEJC(BINSIZ+2*CVLWID(0))	!SIZE OF ONE U CONVOLUTION BUF.
	ULOB=0					!START WITH U=0
C
	SUM=0					!NORMALIZING SUM
C
C GET OUTPUT FILES
C
	IF (.NOT.WNFOP(FOUT,WNFFNM('NMA','TMP'),'WT')) THEN !OPEN OUTPUT FILE
	  CALL WNCTXT(F_TP,'Cannot open convolution output file')
	  CALL WNGEX				!FINISH PROGRAM
	END IF
C
	J=WNFEOF(FCAOUT)			!OUTPUT DISK POINTER
	DO I=3,NOPT				!OPEN UV PLANE OUTPUT FILES
	  IF (OUTOPT(I)) THEN			!THIS ONE WANTED
	    UVPTR(I)=J				!HERE TO WRITE
	    J=J+MPHHDL+LB_E*(VSIZE+1)*(UHIGH+1)	!NEXT OUTPUT POINTER
	  END IF
	END DO
C
C GET BUFFERS
C
C CONVOLUTION
C
	CVFLEN=LB_E*(WNMEJC(CVLWID(0)*CVLSTP)+1) !LENGTH CONVOLUTION TABLE
	JS=WNGGVA(CVFLEN,CVLFUN)		!CONVOLUTION FUNCTION TABLE
	IF (JS) JS=WNGGVA(LB_E*(OUTSIZ(0)/2+1),DECVB(0)) !U CORR. TABLE
	IF (JS) JS=WNGGVA(LB_E*(OUTSIZ(1)/2+1),DECVB(1)) !V CORR. TABLE
	IF (.NOT.JS) THEN
	  CALL WNCTXT(F_TP,'Cannot obtain convolution function buffer')
	  CALL WNGEX				!STOP PROGRAM
	END IF
	CALL NMACVF(CVFLEN/LB_E,CVLTYP,CVLSTP,FTSIZ,OUTSIZ,
	1		A_B(CVLFUN-A_OB),A_B(DECVB(0)-A_OB),
	1		A_B(DECVB(1)-A_OB))	!MAKE CONVOLUTION FUNCTION
C
C FFT
C
	JS=WNGGVA(LB_X*FTSIZ(1),FTBUF)		!GET FFT BUF
	FTBUFE=(FTBUF-A_OB)/LB_E
	FTBUFX=(FTBUF-A_OB)/LB_X
	IF (JS) JS=WNGGVA(LB_X*FTSIZ(1)/2,WTBUF) !GET FFT WEIGHT BUF
	WTBUFX=(WTBUF-A_OB)/LB_X
	IF (.NOT.JS) THEN
	  CALL WNCTXT(F_TP,'Cannot obtain FFT1 buffer')
	  CALL WNGEX				!STOP PROGRAM
	END IF
	DO I=0,FTSIZ(1)/2-1			!FILL FFT WEIGHT
	  R0=I*PI2/FTSIZ(1)
	  A_X(WTBUFX+I)=CMPLX(COS(R0),-SIN(R0))
	END DO
C
C MAP CONVOLUTION
C
	IF (MAKMAP) THEN
	  IF (.NOT.WNGGVA(LB_X*USIZE*(VSIZE+1),
	1		MAPBUF)) THEN		!GET MAP CONVOLUTION BUFFER
	    CALL WNCTXT(F_TP,'Cannot obtain map convolution buffer')
	    CALL WNGEX				!STOP PROGRAM
	  END IF
	  MAPBFX=(MAPBUF-A_OB)/LB_X
	  CALL WNGMVZ(LB_X*(VSIZE+1)*USIZE,A_X(MAPBFX)) !CLEAR BUF
	END IF
	IF (MAKAP) THEN
	  IF (.NOT.WNGGVA(LB_E*USIZE*(VSIZE+1),
	1		APBUF)) THEN		!GET AP CONVOLUTION BUFFER
	    CALL WNCTXT(F_TP,'Cannot obtain antenna pattern '//
	1			'convolution buffer')
	    CALL WNGEX				!STOP PROGRAM
	  END IF
	  APBUFE=(APBUF-A_OB)/LB_E
	  CALL WNGMVZ(LB_E*(VSIZE+1)*USIZE,A_E(APBUFE)) !CLEAR BUF
	END IF
C
C UV PLANES
C
	DO I=3,NOPT
	  IF (OUTOPT(I)) THEN			!UV PLANE OUTPUT
	    IF (.NOT.WNGGVA(LB_E*(VSIZE+1),UVBUF(I))) THEN  !GET BUFFER
	      CALL WNCTXT(F_TP,'Cannot obtain UV plane output buffers')
	      CALL WNGEX			!STOP PROGRAM
	    END IF
	    UVBUFE(I)=(UVBUF(I)-A_OB)/LB_E
	  END IF
	END DO
C
C DO CONVOLUTION
C
C INIT
C
	UOUT=0					!START U OUTPUT
	DO I=3,NOPT
	  UVMX(I)=-1E36				!MAX
	  UVMN(I)=1E36				!MIN
	END DO
C
C ALL BINS
C
	DO I=0,NBIN-1				!DO FOR ALL BINS
	  J=BAD(4,I)				!BLOCK IN BIN
	  DO WHILE (J.NE.-1)			!MORE DATA IN BIN
	    IF (WNFRD(FIN,MXSBB,BUF,J))	THEN	!READ BUF
	      J=JBUF(MXSBJ-1)			!POINTER TO NEXT BUF
	    ELSE
	      CALL WNCTXT(F_TP,'Read error sorted file')
	      CALL WNGEX			!STOP PROGRAM
	    END IF
	    DO I1=0,BAD(3,I)-1,3+2*NPOL		!DO ALL DATA POINTS
	      IF (JBUF(I1+2).EQ.0) GOTO 10	!BUFFER READY
	      CALL NMACVX(BUF(I1+2),BUF(I1),BUF(I1+3+2*NP),
	1			A_B(CVLFUN-A_OB),A_X(MAPBFX),
	1			A_E(APBUFE))	!DO CONVOLUTION
	    END DO				!END POINTS
 10	    CONTINUE
	  END DO				!MORE IN BIN
C
C OUTPUT DATA FOR A BIN
C
C UV
C
	  J2=MIN(WNMEJF((I+1)*BINSIZ-CVLWID(0)),UHIGH) !MAX. U TO OUTPUT
	  IF (I.EQ.NBIN-1) J2=UHIGH		!LAST BIN: DO REMAINDER
	  DO J1=UOUT,J2				!OUTPUT WHAT IS POSSIBLE
	    J0=J1-ULOB				!OFFSET IN BUF
	    IF (J0.GE.USIZE) J0=J0-USIZE	!WRAP
	    DO I5=3,NOPT
	      IF (OUTOPT(I5)) THEN		!OUTPUT UV PLANE
		A_E(UVBUFE(I5))=0		!CENTRE POINT
	        IF (I5.EQ.4) THEN		!REAL
	          CALL WNMARL(VSIZE,A_X(MAPBFX+J0*VSIZE),
	1			A_E(UVBUFE(I5)+1))
	        ELSE IF (I5.EQ.5) THEN		!IMAGINARY
	          CALL WNMAIM(VSIZE,A_X(MAPBFX+J0*VSIZE),
	1			A_E(UVBUFE(I5)+1))
	        ELSE IF (I5.EQ.6) THEN		!AMPLITUDE
	          CALL WNMAAM(VSIZE,A_X(MAPBFX+J0*VSIZE),
	1			A_E(UVBUFE(I5)+1))
	        ELSE IF (I5.EQ.7) THEN		!PHASE
	          CALL WNMAPH(VSIZE,A_X(MAPBFX+J0*VSIZE),
	1			A_E(UVBUFE(I5)+1))
	        ELSE				!COVER
	          CALL WNGMV(LB_E*VSIZE,
	1			A_E(APBUFE+J0*VSIZE),
	1			A_E(UVBUFE(I5)+1)) !FILL OUTPUT BUF
	        END IF
	        R0=-1E36			!FIND MAX/MIN
		R1=1E36
	        CALL WNMFMX(VSIZE+1,A_E(UVBUFE(I5)),1D0,R0,
	1				I3,R1,I4) !FIND MAX/MIN
	        IF (R0.GT.UVMX(I5)) THEN	!NEW MAX
		  UVMX(I5)=R0
		  MAXR(I5)=I3-(VSIZE+1)/2
		  MAXD(I5)=J1-(UHIGH+1)/2	!920423
	        END IF
	        IF (R1.LT.UVMN(I5)) THEN	!NEW MIN
		  UVMN(I5)=R1
		  MINR(I5)=I4-(VSIZE+1)/2
		  MIND(I5)=J1-(UHIGH+1)/2	!920423
		END IF
		IF (.NOT.WNFWR(FCAOUT,LB_E*(VSIZE+1),
	1		A_E(UVBUFE(I5)),
	1		UVPTR(I5)+MPHHDL+J1*LB_E*(VSIZE+1))) THEN !OUTPUT 920406
 20		  CONTINUE
		  CALL WNCTXT(F_TP,'Write error convolved data')
		  CALL WNGEX			!STOP PROGRAM
		END IF
	      END IF
	    END DO
C
C MAP
C
	    IF (OUTOPT(1)) THEN
	      CALL WNGMV(LB_X*(VHIGH+1),A_X(MAPBFX+J0*VSIZE-VLOW),
	1			A_X(FTBUFX))	!0...VMAX
	      CALL WNGMV(-LB_X*VLOW,A_X(MAPBFX+J0*VSIZE),
	1			A_X(FTBUFX+FTSIZ(1)+VLOW)) !VLOW...0
	      CALL WNGMVZ(LB_X*(FTSIZ(1)-VSIZE),
	1			A_X(FTBUFX+VHIGH+1)) !VHIGH...VLOW
	      CALL WNMFTC(FTSIZ(1),A_X(FTBUFX),A_X(WTBUFX)) !FFT
	      CALL WNMFCS(FTSIZ(1),A_X(FTBUFX))	!SWAP HALVES
	      IF (.NOT.WNFWRS(FOUT,LB_X*OUTSIZ(1),
	1			A_X(FTBUFX+(FTSIZ(1)-
	1			OUTSIZ(1))/2))) GOTO 20 !OUTPUT MAP LINE
	    END IF
	    IF (MAKMAP) CALL WNGMVZ(LB_X*VSIZE,
	1			A_X(MAPBFX+J0*VSIZE)) !CLEAR BUF
C
C AP
C
	    IF (OUTOPT(2)) THEN
	      CALL WNGMV(LB_E*(VHIGH+1),A_E(APBUFE+J0*VSIZE-VLOW),
	1			A_E(FTBUFE))	!0...VMAX
	      CALL WNGMV(-LB_E*VLOW,A_E(APBUFE+J0*VSIZE),
	1			A_E(FTBUFE+FTSIZ(1)+VLOW)) !VLOW...0
	      CALL WNGMVZ(LB_E*(FTSIZ(1)-VSIZE),
	1			A_E(FTBUFE+VHIGH+1)) !VHIGH...VLOW
	      CALL WNMFRC(FTSIZ(1),A_E(FTBUFE))	!REAL TO COMPLEX
	      CALL WNMFTC(FTSIZ(1),A_X(FTBUFX),A_X(WTBUFX)) !FFT
	      CALL WNMFCS(FTSIZ(1),A_X(FTBUFX))	!SWAP HALVES
	      IF (.NOT.WNFWRS(FOUT,LB_X*OUTSIZ(1),
	1			A_X(FTBUFX+(FTSIZ(1)-
	1			OUTSIZ(1))/2))) GOTO 20 !OUTPUT AP LINE
	    END IF
	    IF (MAKAP) CALL WNGMVZ(LB_E*VSIZE,
	1			A_E(APBUFE+J0*VSIZE)) !CLEAR BUF
	  END DO
	  UOUT=J2+1				!NEXT OUTPUT START
	  IF (UOUT-ULOB.GE.USIZE) ULOB=ULOB+USIZE !WRAPPING
	END DO					!END BINS
C
C FREE BUFFERS
C
	CALL WNGFVA(CVFLEN,CVLFUN)		!CONVOLUTION FUNCTION
	CALL WNGFVA(LB_X*FTSIZ(1),FTBUF)	!FFT BUFFER
	CALL WNGFVA(LB_X*FTSIZ(1)/2,WTBUF)	!FFT WEIGHT BUFFER
	IF (MAKMAP) CALL WNGFVA(LB_X*USIZE*(VSIZE+1),
	1		MAPBUF)			!RELEASE MAP BUFFER
	IF (MAKAP) CALL WNGFVA(LB_E*USIZE*(VSIZE+1),
	1		APBUF)			!RELEASE AP BUFFER
	DO I=3,NOPT
	  IF (OUTOPT(I)) THEN			!UV PLANE OUTPUT
	    CALL WNGFVA(LB_E*(VSIZE+1),UVBUF(I))
	  END IF
	END DO
C
C CLOSE UV PLANE OUTPUT
C
	DO I=3,NOPT
	  IF (OUTOPT(I)) THEN
	    CALL WNGMVZ(MPHHDL,MPH)		!CLEAR HEADER
	    MPHJ(MPH_MDP_J)=UVPTR(I)+MPHHDL	!DATA POINTER
	    MPHI(MPH_VER_I)=MPHHDV		!HEADER VERSION
	    MPHI(MPH_LEN_I)=MPHHDL		!HEADER LENGTH
	    MPHJ(MPH_NRA_J)=VSIZE+1		!FILL HEADER - SIZE RA
	    MPHJ(MPH_NDEC_J)=UHIGH+1		!SIZE DEC
	    MPHJ(MPH_NFRQ_J)=1			!SIZE FREQ.
	    MPHD(MPH_SUM_D)=SUM			!NORMALISATION
	    CALL WNGMFS(MPH_FNM_N,CNTCVL(0),MPH(MPH_FNM_1)) !FIELD NAME
	    MPHD(MPH_BDW_D)=CNTDVL(14)		!OBS. BANDWIDTH
	    MPHD(MPH_RA_D)=MAPCRD(0)		!RA
	    MPHD(MPH_DEC_D)=MAPCRD(1)		!DEC
	    IF (ABS(MAPCTP).EQ.2) THEN		!EPOCH
	      MPHD(MPH_RAO_D)=CNTDVL(0)		!RA
	      MPHD(MPH_DECO_D)=CNTDVL(1)	!DEC
	    ELSE				!DATE
	      MPHD(MPH_RAO_D)=CNTDVL(2)		!RA
	      MPHD(MPH_DECO_D)=CNTDVL(3)	!DEC
	    END IF
	    MPHD(MPH_FRQO_D)=CNTDVL(6)		!FREQ.
	    MPHD(MPH_FRQ_D)=CNTDVL(6)		!FREQ.
	    MPHI(MPH_ODY_I)=CNTJVL(0)		!DAY
	    MPHI(MPH_OYR_I)=CNTJVL(1)		!YEAR
	    MPHJ(MPH_INST_J)=CNTJVL(6)		!INSTRUMENT TYPE
	    MPHI(MPH_DCD_I)=5			!DATA CODE (E)
	    MPHI(MPH_PCD_I)=0			!PROGRAM CODE NMAP
	    MPHD(MPH_SRA_D)=FIELD(0)/FTSIZ(0)/PI2 !RA GRID STEP
	    MPHD(MPH_SDEC_D)=FIELD(1)/FTSIZ(1)/PI2 !DEC GRID STEP
	    MPHD(MPH_SHR_D)=SHIFT(0)/3600./360.	!FIELDSHIFT RA
	    MPHD(MPH_SHD_D)=SHIFT(1)/3600./360.	!FIELDSHIFT DEC
	    MPHJ(MPH_NPT_J)=CNTJVL(4)		!# POINTS
	    MPHJ(MPH_NBL_J)=CNTJVL(5)		!# BASELINES
	    MPHJ(MPH_NST_J)=CNTJVL(2)		!# SETS
	    MPHJ(MPH_VELC_J)=CNTJVL(3)		!VEL. CODE
	    MPHE(MPH_VEL_E)=CNTDVL(7)		!VELOCITY
	    MPHD(MPH_FRQC_D)=CNTDVL(9)		!REF. FREQ.
	    MPHE(MPH_VELR_E)=CNTDVL(8)		!REF. VELOCITY
	    MPHD(MPH_FRQV_D)=MPHD(MPH_FRQO_D)	!OBS. CHANNEL FREQUENCY
	    MPHD(MPH_FRQ0_D)=CNTDVL(12)		!REST FREQ.
	    MPHE(MPH_UNI_E)=5./1000.		!FACTOR TO GET JY
	    CALL WNGMFS(MPH_UCM_N,CNTCVL(1),MPH(MPH_UCM_1)) !USER COMMENT
	    IF (I.EQ.3) THEN			!SET OUTPUT TYPE
	      CALL WNGMFS(MPH_TYP_N,'COVER',MPH(MPH_TYP_1))
	    ELSE IF (I.EQ.4) THEN
	      CALL WNGMFS(MPH_TYP_N,'REAL',MPH(MPH_TYP_1))
	    ELSE IF (I.EQ.5) THEN
	      CALL WNGMFS(MPH_TYP_N,'IMAG',MPH(MPH_TYP_1))
	    ELSE IF (I.EQ.6) THEN
	      CALL WNGMFS(MPH_TYP_N,'AMPL',MPH(MPH_TYP_1))
	    ELSE IF (I.EQ.7) THEN
	      CALL WNGMFS(MPH_TYP_N,'PHASE',MPH(MPH_TYP_1))
	    END IF
	    CALL WNGMFS(MPH_POL_N,POLC(NP),MPH(MPH_POL_1)) !POL. CODE
	    MPHI(MPH_CD_I+0)=TAPTYP		!TAPER TYPE
	    MPHI(MPH_CD_I+1)=CVLTYP		!CONVOLUTION TYPE
	    IF (DECVL) THEN
	      MPHI(MPH_CD_I+2)=1		!DE-CONVOLVE
	    ELSE
	      MPHI(MPH_CD_I+2)=0		!NO DE-CONVOLVE
	    END IF
	    IF (CLIP) THEN
	      MPHI(MPH_CD_I+3)=1		!CLIP
	    ELSE
	      MPHI(MPH_CD_I+3)=0		!NO CLIP
	    END IF
	    IF (SUB) THEN
	      MPHI(MPH_CD_I+4)=1		!SOURCE SUBTRACTS
	    ELSE
	      MPHI(MPH_CD_I+4)=0		!NO SUBTRACTS
	    END IF
	    MPHI(MPH_CD_I+5)=DATTYP		!DATA TYPE
	    MPHI(MPH_CD_I+6)=UVCDT		!UV COORD. TYPE
	    MPHI(MPH_CD_I+7)=0			!DE-BEAM COUNT
	    IF (ABS(MAPCTP).EQ.2) THEN		!EPOCH
	      MPHI(MPH_EPT_I)=1			!EPOCH TYPE
	      MPHE(MPH_EPO_E)=CNTDVL(13)	!EPOCH
	      MPHE(MPH_OEP_E)=CNTDVL(4)		!OBS. EPOCH
	    ELSE				!DATE
	      MPHI(MPH_EPT_I)=0			!DATE TYPE
	      MPHE(MPH_EPO_E)=CNTDVL(4)		!EPOCH
	      MPHE(MPH_OEP_E)=CNTDVL(4)		!OBS. EPOCH
	    END IF
	    MPHE(MPH_FRA_E)=(OUTSIZ(0)-1)*MPHD(MPH_SRA_D) !FIELD SIZE RA
	    MPHE(MPH_FDEC_E)=(OUTSIZ(1)-1)*MPHD(MPH_SDEC_D) !FIELD SIZE DEC
	    MPHE(MPH_FFRQ_E)=0			!FIELD SIZE FREQ
	    CALL WNGMFS(MPH_TEL_N,'WSRT',MPH(MPH_TEL_1)) !TEL. NAME
	    MPHJ(MPH_FSR_J)=FTSIZ(0)		!FFT SIZES
	    MPHJ(MPH_FSD_J)=FTSIZ(1)
C
	    MPHE(MPH_MAX_E)=UVMX(I)		!MAX/MIN
	    MPHE(MPH_MIN_E)=UVMN(I)
	    MPHJ(MPH_MNR_J)=MINR(I)
	    MPHJ(MPH_MND_J)=MIND(I)
	    MPHJ(MPH_MXR_J)=MAXR(I)
	    MPHJ(MPH_MXD_J)=MAXD(I)
	    MPHJ(MPH_ZRA_J)=(VSIZE+1)/2		!CENTRE RA
	    MPHJ(MPH_ZDEC_J)=0			!CENTRE DEC
	    IF (.NOT.WNFWR(FCAOUT,MPHHDL,MPH,UVPTR(I))) THEN !WRITE HEADER
	      CALL WNCTXT(F_TP,'I/O error UV plane')
	    END IF
	    IF (.NOT.WNDLNF(SGPH(3)+SGH_LINKG_1,I-1,SGH_GROUPN_1,FCAOUT,
	1		SGPH(4),SGNR(4))) THEN	!FIND/CREATE SUB-GROUP
 30	      CONTINUE
	      CALL WNCTXT(F_TP,'Error creating sub-group')
	      CALL WNGEX			!STOP PROGRAM
	    END IF
	    IF (.NOT.WNDLNK(GFH_LINK_1,UVPTR(I),
	1		MPH_SETN_1,FCAOUT)) GOTO 30 !LINK THE SET
	    IF (.NOT.WNDLNG(SGPH(4)+SGH_LINKG_1,UVPTR(I),
	1		SGH_GROUPN_1,FCAOUT,SGPH(5),
	1		SGNR(5))) GOTO 30	!LINK THE SUB-GROUP
	    CALL WNCTXT(F_P,'!^')		!NEW PAGE
	    CALL WNCTXT(F_TP,'!2/Description of the !AL4'//
	1			' output produced:',MPH(MPH_TYP_1))
	    SGNR(6)=-1				!FINISH NAME
	    IF (.NOT.WNFRD(FCAOUT,MPHHDL,MPH,UVPTR(I))) GOTO 30 !REREAD HEADER
	    CALL NMAPMH(F_TP,MPH,SGNR,NODOUT)
	  END IF
	END DO
C
	RETURN
C
C
	END
