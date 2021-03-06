C+ NMASOR.FOR
C  WNB 910304
C
C  Revisions:
C	WNB 910927	Typo file loops
C	WNB 911025	Perfection mosaicking positions
C	HjV 920520	HP does not allow extended source lines
C	WNB 920828	Update for line velocities
C	WNB 920831	Correct logics if change of file, but not setname
C	WNB 920902	Typo in set change check
C       WNB 930127      Change weighting averages, proper bandwidth
C	WNB 930224	Correct for incorrect new weighting scheme
C	WNB 930414	Cater for empty set
C	HjV 930423	Change some text
C	WNB 930607	Delete INCLUDE SCH_O
C	WNB 930619	Change incorrect text
C	WNB 930825	Add dipole angles
C	WNB 930826	New model calculation; redundant baselines
C	WNB 930928	Multiple SCN files with model
C	WNB 931008	Add MINST
C	CMV 950314	Add MAXPOS (also works if RTP(13)=0)
C       WNB 950817	Use Pol. Int. buffers
C
	SUBROUTINE NMASOR(FCA)
C
C  Sort all input data into temporary file
C
C  Result:
C
C	CALL NMASOR ( FCA_J:I)		Sort all input data into a
C					temporary file defined by FCA.
C
C
C  PIN references:
C
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'STH_O_DEF'		!SET HEADER
	INCLUDE 'NMA_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER FCA			!FILE CONTROL SORTED OUTPUT
C
C  Function references:
C
	LOGICAL WNFOP			!OPEN FILE
	LOGICAL NSCSTL			!GET A SET
	LOGICAL NSCSIF			!GET INTERFEROMETER TABLE
	LOGICAL NMOMSL			!CALCULATE MODEL FOR SETS
	LOGICAL NMORDH			!GET SOURCE PARAMETERS
	CHARACTER*32 WNTTSG		!SHOW SET NAME
C
C  Data declarations:
C
	LOGICAL FIRST			!FIRST SET INDICATOR
	INTEGER FCAIN			!INPUT FILE
	REAL BASEL(0:STHIFR-1)		!BASELINES
	INTEGER IRED(0:STHIFR-1)	!REDUNDANT BASELINES
	REAL TAPER(0:STHIFR-1)		!TAPER
	INTEGER*2 IFRT(0:STHIFR-1)	!INTERFEROMETER TABLE
	INTEGER IFRA(0:1,0:STHIFR-1)
	REAL ANG(0:2,0:STHIFR-1)
	COMPLEX CDAT(0:STHIFR-1,0:3)	!DATA
	REAL APDAT(0:STHIFR-1)		!WEIGHT
	REAL UV(0:1,0:STHIFR-1)		!UV DATA
	REAL MAXPOS			!MAX. TELESCOPE POSITION
	INTEGER STP			!SOURCE TYPE
	DOUBLE PRECISION SRA,SDEC,SFRQ	!SOURCE RA, DEC, FREQ CENTRE
	DOUBLE PRECISION MLM(0:1)	!MAP CENTRE L,M
	REAL LM0(0:1)			!BASIC SOURCE DISPLACEMENT
	DOUBLE PRECISION FRQ0		!BASIC FREQUENCY
	REAL TF(0:1)			!SOURCE INTEGR. TIME, BANDWIDTH
	INTEGER MINST			!INSTRUMENT
	INTEGER STHP			!SET HEADER POINTER
	INTEGER SETNAM(0:7)		!SET ID
	INTEGER TSTNAM(0:7),CHKNAM(0:7)	!SET ID CHECK
	INTEGER BUFPP			!POINTER FT BUFFER POL INT
	BYTE STH(0:STH__L-1)		!SET HEADER
	  INTEGER*2 STHI(0:STH__L/LB_I-1)
	  INTEGER STHJ(0:STH__L/LB_J-1)
	  REAL STHE(0:STH__L/LB_E-1)
	  DOUBLE PRECISION STHD(0:STH__L/LB_D-1)
	  EQUIVALENCE (STH,STHI,STHJ,STHE,STHD)
C-
C
C PREPARE OUTPUT
C
	FIRST=.TRUE.				!SET FIRST SET
	CALL NMASOI(FCA,BUFPP)			!OPEN OUTPUT, PREPARE BUFFERS
	DO I=0,7				!SET CHECK SET ID
	  TSTNAM(I)=-1
	END DO
C
C GET ALL DATA
C
	DO I1=1,NFILE				!ALL INPUT FILES
	  IF (.NOT.WNFOP(FCAIN,FILIN(I1),'U')) THEN
	    CALL WNCTXT(F_TP,'Error reading node !AS',NODIN(I1))
	    CALL WNGEX				!STOP PROGRAM
	  END IF
	  CALL WNCTXT(F_TP,'Scan node !AS started at !%T',NODIN(I1))
C
C MODEL INIT
C
	  IF (SUB .OR. UVDTP.EQ.1) THEN		!MAKE MODEL
	    IF (.NOT.NMOMSL(FCAIN,SETS(0,0,I1),LPOFF)) THEN
 10	      CONTINUE
	      CALL WNCTXT(F_TP,'Error in model calculation')
	      CALL WNGEX			!STOP PROGRAM
	    END IF
	  END IF
C
C ALL SETS
C
	  DO WHILE (NSCSTL(FCAIN,SETS(0,0,I1),STH,STHP,
	1			SETNAM,LPOFF))	!DO ALL SETS
	    IF (STHJ(STH_SCN_J).LE.0) GOTO 20	!SKIP EMPTY SET
	    DO I=0,3
	      IF (SETNAM(I).NE.TSTNAM(I) .OR.
	1		TSTNAM(7).NE.I1) THEN	!NEW SET OR FILE
		DO I2=0,3			!SAVE IT
		  TSTNAM(I2)=SETNAM(I2)
		END DO
		TSTNAM(7)=I1			!FILE NUMBER
	        CALL WNCTXT(F_TP,'Sector !AS started at !%T',
	1		WNTTSG(TSTNAM,0))
	      END IF
	    END DO
C
C GET BASELINE TABLES
C
	    IF (.NOT.NSCSIF(FCAIN,STH,IFRT,IFRA,ANG)) THEN !READ IFR TABLES
	      CALL WNCTXT(F_TP,'Error reading interferometer tables')
	      CALL WNGEX			!END PROGRAM
	    END IF
	    CALL NSCMBL(STHE(STH_RTP_E),STHJ(STH_NIFR_J),IFRT,
	1			SIFRS(0,0,I1),BASEL) !MAKE BASELINES
	    CALL NCARRT(STHJ(STH_NIFR_J),BASEL,1E0,IRED,ANG) !GET REDUNDANT
	    UVSC=1./(3600.*DEG)			!SCALE FACTOR MAP SHIFT
	    MAXPOS=0				!FIND MAX. TEL.POS.
	    DO I=0,STHTEL-1
	      MAXPOS=MAX(MAXPOS,STHE(STH_RTP_E+I))
	    END DO
	    MAXPOS=MAXPOS-STHE(STH_RTP_E)
C
C MAKE TAPER
C
	    DO I=0,STHJ(STH_NIFR_J)-1		!MAKE TAPER
	     IF (BASEL(I).GE.0) THEN
	      IF (TAPTYP.EQ.1 .OR. TAPTYP.EQ.5) THEN
		TAPER(I)=EXP(-((BASEL(I)*FRQMAX/TAPVAL/STHD(STH_FRQ_D))**2))
	      ELSE IF (TAPTYP.EQ.2) THEN
		TAPER(I)=MAX(0D0,1D0-BASEL(I)*FRQMAX/TAPVAL/STHD(STH_FRQ_D))
	      ELSE
		TAPER(I)=1
	      END IF
	      IF ((TAPTYP.EQ.4 .OR. TAPTYP.EQ.5) .AND. BASEL(I).GT.0)
	1			TAPER(I)=TAPER(I)/(BASEL(I)*
	1				STHD(STH_FRQ_D)/FRQMAX) !CORRECT 1/R
	      IF (UWGT.EQ.1) TAPER(I)=TAPER(I)*BASEL(I)*STHD(STH_FRQ_D)*
	1				STHE(STH_HAV_E)/2./FRQMAX
	     ELSE
	      TAPER(I)=0			!DELETED SCAN
	     END IF
	    END DO
C
C SOURCE MODEL
C
	    IF (SUB .OR. UVDTP.EQ.1) THEN	!MODEL WANTED
	      IF (.NOT.NMORDH(6,STP,SRA,SDEC,SFRQ)) GOTO 10 !GET SOME INFO
	      CALL NMOMST(STP,SRA,SDEC,STH,LM0,FRQ0,TF,MINST) !GET SOME DATA
	    END IF
C
C GET STATISTICS
C
	    IF (FIRST) THEN			!FIRST SET
	      CALL NMASS1(FCAIN,STHP)		!INIT STATISTICS
	      DO I2=0,STHJ(STH_NIFR_J)-1	!COUNT BASELINES
	        IF (BASEL(I2).GE.0) CNTJVL(5)=CNTJVL(5)+1
	      END DO
	      DO I=0,3				!SAVE CHECKNAME
		CHKNAM(I)=SETNAM(I)
	      END DO
	      CHKNAM(7)=I1			!FILE #
C
C CALCULATE AN INTEGER MAP CENTRE COORDINATE
C
	      IF (MAPCTP.LT.0) THEN		!MOSAIC
		CALL WNMDRD(MAPCRD(0),MAPCRD(1),MLM(0),MLM(1),
	1		CNTDVL(-2-2*MAPCTP),
	1		CNTDVL(-1-2*MAPCTP))	!MAKE L,M
		DO I=0,1			!L,M
		  D0=ANINT(MLM(I)/(FIELD(I)/FTSIZ(I))) !CENTRE POSITION
		  MLM(I)=D0*(FIELD(I)/FTSIZ(I))
		END DO
		CALL WNMDLM(MAPCRD(0),MAPCRD(1),MLM(0),MLM(1),
	1		CNTDVL(-2-2*MAPCTP),
	1		CNTDVL(-1-2*MAPCTP))	!MAKE RA,DEC
	      END IF
	      FIRST=.FALSE.
	    ELSE
	      DO I=0,3				!CHECK FOR NEW
	        IF (SETNAM(I).NE.CHKNAM(I) .OR.
	1		CHKNAM(7).NE.I1) THEN	!NEW SET OR FILE
		  DO I2=0,3			!NEW TEST
		    CHKNAM(I2)=SETNAM(I2)
		  END DO
		  CHKNAM(7)=I1			!NEW FILE
	          DO I2=0,STHJ(STH_NIFR_J)-1	!COUNT BASELINES
	            IF (BASEL(I2).GE.0) CNTJVL(5)=CNTJVL(5)+1
	          END DO
	          FRQMAX=MAX(FRQMAX,REAL(STHD(STH_FRQ_D))) !MAX. FREQ.
	          FRQMIN=MIN(FRQMIN,REAL(STHD(STH_FRQ_D))) !MIN. FREQ.
	          UVMAX(0)=MAX(UVMAX(0),REAL(MAXPOS*
	1			STHD(STH_FRQ_D)/(CL*1E-6))) !MAX. U
	          UVMAX(1)=MAX(UVMAX(1),ABS(REAL(SIN(STHD(STH_DEC_D)*PI2)*
	1			MAXPOS*
	1			STHD(STH_FRQ_D)/(CL*1E-6)))) !MAX. V
	          UV1MAX(0)=MAX(UV1MAX(0),STHE(STH_RTP_E+STHTEL-1)) !MAX U BASHA
		  IF (MAPCTP.GE.0) THEN			!NON-MOSAIC
	            CNTDVL(0)=CNTDVL(0)*CNTJVL(2)+STHD(STH_RA_D) !RA
	            CNTDVL(1)=CNTDVL(1)*CNTJVL(2)+STHD(STH_DEC_D) !DEC
	            CNTDVL(2)=CNTDVL(2)*CNTJVL(2)+STHD(STH_RAE_D) !RA EPOCH
	            CNTDVL(3)=CNTDVL(3)*CNTJVL(2)+STHD(STH_DECE_D) !DEC EPOCH
		  END IF
	          CNTDVL(4)=CNTDVL(4)*CNTJVL(2)+STHE(STH_OEP_E) !OBS. EPOCH
		  D0=STHE(STH_BAND_E)*STHJ(STH_SCN_J)*STHE(STH_HAV_E) !WEIGHT
		  CNTDVL(14)=2*D0*CNTDVL(5)*ABS(STHD(STH_FRQ_D)-CNTDVL(6))/
	1			(CNTDVL(5)**2+D0**2)*(CNTDVL(5)+D0)+
	1			CNTDVL(5)*CNTDVL(14)+D0*
	1			STHE(STH_BAND_E) !BANDWIDTH
	          CNTDVL(6)=CNTDVL(5)*CNTDVL(6)+D0*
	1			STHD(STH_FRQ_D)	!FREQUENCY
	          CNTDVL(7)=CNTDVL(5)*CNTDVL(7)+D0*
	1			STHE(STH_VEL_E)	!VELOCITY
	          CNTDVL(8)=CNTDVL(5)*CNTDVL(8)+D0*
	1			STHE(STH_VELR_E) !REF. VEL.
	          CNTDVL(9)=CNTDVL(5)*CNTDVL(9)+D0*
	1			STHD(STH_FRQC_D) !BAND REF. FREQUENCY
	          CNTDVL(12)=CNTDVL(5)*CNTDVL(12)+D0*
	1			STHD(STH_FRQ0_D) !REST FREQUENCY
	          CNTDVL(5)=CNTDVL(5)+D0	!TOTAL WEIGHTED BAND
	          CNTDVL(6)=CNTDVL(6)/CNTDVL(5) !AVERAGE FREQUENCY
	          CNTDVL(7)=CNTDVL(7)/CNTDVL(5) !AVERAGE VELOCITY
	          CNTDVL(8)=CNTDVL(8)/CNTDVL(5) !AVERAGE REF. VEL.
	          CNTDVL(9)=CNTDVL(9)/CNTDVL(5) !AVERAGE REF. FREQ.
	          CNTDVL(12)=CNTDVL(12)/CNTDVL(5) !AVERAGE REST FREQ.
		  CNTDVL(14)=CNTDVL(14)/CNTDVL(5) !AVERAGE BANDWIDTH
	          CNTJVL(2)=CNTJVL(2)+1		!COUNT SETS
		  IF (MAPCTP.GE.0) THEN		!NON-MOSAIC
	            CNTDVL(0)=CNTDVL(0)/CNTJVL(2) !RA
	            CNTDVL(1)=CNTDVL(1)/CNTJVL(2) !DEC
	            CNTDVL(2)=CNTDVL(2)/CNTJVL(2) !RA EPOCH
	            CNTDVL(3)=CNTDVL(3)/CNTJVL(2) !DEC EPOCH
		  END IF
	          CNTDVL(4)=CNTDVL(4)/CNTJVL(2)	!OBS. EPOCH
	        END IF
	      END DO
	    END IF
C
C RESIDUAL MOSAIC SHIFT
C
	    IF (MAPCTP.LT.0) THEN		!MOSAIC
	      CALL WNMDRD(MAPCRD(0),MAPCRD(1),MLM(0),MLM(1),
	1		CNTDVL(-2-2*MAPCTP),
	1		CNTDVL(-1-2*MAPCTP))	!MAKE L,M MAP
	      IF (MAPCTP.EQ.-2) THEN		!B1950
	        CALL WNMDRD(MAPCRD(0),MAPCRD(1),CNTDVL(10),CNTDVL(11),
	1		STHD(STH_RAE_D),
	1		STHD(STH_DECE_D))	!MAKE L,M SET
	      ELSE				!APPARENT
	        CALL WNMDRD(MAPCRD(0),MAPCRD(1),CNTDVL(10),CNTDVL(11),
	1		STHD(STH_RA_D),
	1		STHD(STH_DEC_D))	!MAKE L,M SET
	      END IF
	      DO I=0,1				!SAVE SHIFT
		CNTDVL(10+I)=-CNTDVL(10+I)+MLM(I)
	      END DO
	    END IF
C
C DO ALL SCANS
C
	    DO I=0,STHJ(STH_SCN_J)-1
	      CALL NMASCN(FCAIN,HA(0,I1),STH,IFRT,ANG,
	1	  BASEL,IRED,TAPER,I,
	1	  STP,SRA,SDEC,LM0,FRQ0,TF,MINST,
	1	  CDAT,APDAT,UV,
	1	  A_X((BUFPP-A_OB)/LB_X+NPTRF),
	1	  A_X((BUFPP-A_OB)/LB_X),
	1	  A_X((BUFPP-A_OB)/LB_X+3*NPTRF),
	1	  A_X((BUFPP-A_OB)/LB_X+4*NPTRF),J0) !READ SCAN
	      IF (DODFT) THEN
		IF (POLTJ(-1,0).EQ.0) THEN
		  CALL NMADFT(STHJ(STH_NIFR_J),CDAT,UV,APDAT) !DO DFT
		ELSE
		  CALL NMADFT(J0,A_X((BUFPP-A_OB)/LB_X+NPTRF),
	1	      A_X((BUFPP-A_OB)/LB_X+3*NPTRF),
	1	      A_X((BUFPP-A_OB)/LB_X))	!DO DFT
		END IF
	      ELSE
		IF (POLTJ(-1,0).EQ.0) THEN	!STANDARD
		  CALL NMASOT(FCA,A_B(BINADM-A_OB),
	1	      A_B(BINBUF-A_OB),A_B(BINBUF-A_OB),
	1	      STHJ(STH_NIFR_J),NPOL,
	1	      CDAT,UV,APDAT)		!SORT AND OUTPUT
		ELSE				!POL. INT.
		  CALL NMASOT(FCA,A_B(BINADM-A_OB),
	1	      A_B(BINBUF-A_OB),A_B(BINBUF-A_OB),
	1	      J0,1,A_X((BUFPP-A_OB)/LB_X+NPTRF),
	1	      A_X((BUFPP-A_OB)/LB_X+3*NPTRF),
	1	      A_X((BUFPP-A_OB)/LB_X))	!SORT AND OUTPUT
		END IF
	      END IF
	    END DO
 20	    CONTINUE
	  END DO				!SETS
	  CALL WNFCL(FCAIN)			!CLOSE INPUT
	END DO					!FILES
C
C FINISH
C
	IF (DODFT) THEN
	  CALL NMADF1				!PRINT DFT
	ELSE
	  CALL NMASON(FCA,A_B(BINADM-A_OB),A_B(BINBUF-A_OB),
	1		BUFPP)			!FINISH OUTPUT
	END IF
C
	RETURN
C
C
	END
