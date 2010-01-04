C+ NCARPS.FOR
C  WNB 900312
C
C  Revisions:
C	WNB 910812	Add ALIGN
C	WNB 910930	Narrower check
C	WNB 911024	Overall running noise
C	WNB 9110245	Check zero division
C	HjV 920520	HP does not allow extended source lines
C	WNB 930826	New model data
C	JPH 940928	Comments
C       WNB 950613	New LSQ routines
C	WNB 980701	Add for new MIFR calculations
C	CMV 030116	Acommodated for unsorted IFR table
C
C
	LOGICAL FUNCTION NCARPS(MAR,NIFR,IFR,BASEL,NDEG,
	1			IRED,WGT,AWGT,CDAT,AMP,PHAS,CMOD,CWGT,
	1			CSOL,SOL,MU,ME)
C
C  Calculate redundancy phase solution
C
C  Result:
C
C	NCARPS_L = NCARPS( 
C		MAR_J:I, NIFR_J:I,IFR_I(0:*):I, BASEL_E(0:*):I, 
C		NDEG_J:IO,IRED_J(0:NIFR-1):I,
C		WGT_E(0:*,0:1):I,AWGT_E(0:*,0:1):I, 
C		CDAT_X(0:*,0:1):I,AMP_E(0:*,0:1):I,PHAS_E(0:*,0:1):I,
C		CMOD_X(0:*,0:3):I, CWGT_E(0:*):I,
C		CSOL_E(0:*,0:1,0:1):I,
C		SOL_E(0:*,0:1,0:1):O,MU_E:O, ME_E:O)
C
C  	Calculate the redundancy phase solution in SOL, using CSOL as 
C approximate solution, resulting in adjustment error MU and mean errors ME.
C
C	MAR is the solution area for the  telescopes, using NIFR interferometers
C tabulated in IFR with baselines BASEL and a degeneracy of NDEG.
C	IRED specifies the redundant baselines.
C	WGT is the weight, AWGT the amplitude-weighted weight
C	CDAT/AMP/PHAS sre the data. 
C	CMOD is the model with sqrt(weights) CWGT.
C
C	NCASPS_L = NCASPS( ...)
C					Use model for constraints (selfcal)
C	NCAAPS_L = NCAAPS( ..., NUK_J:I, ALEQ_E(0:*,0:*):I)
C					Use model for aligning NUK parameters
C					using equations ALEQ
C	NCARP1_L = NCARP1( ...)
C					Calculate X and Y simultaneous
C	NCARP2_L = NCARP2( ...)
C					Calculate X and Y simultaneous with Q=0
C	NCARPE_L = NCARPE( 
C		MAR_J:I, NIFR_J:I,IFR_I(0:*):I, BASEL_E(0:*):I,
C		NDEG_J:IO,IRED_J(0:NIFR-1):I,
C		WGT_E(0:*,0:1):I,AWGT_E(0:*,0:1):I,
C		CDAT_X(0:*,0:1):I,AMP_E(0:*,0:1):I, PHAS_E(0:*,0:1):I,
C		CMOD_X(0:*,0:3):I, CWGT_E(0:*):I,
C		CSOL_E(0:*,0:1,0:1):I,
C		SOL_E(0:*,0:1,0:1):I,MU_E:I, ME_E:I,
C		ARMS_E(0:2):I,
C		JAV_J(0:*,0:*,0:1):IO, EAV_E(0:*,0:*,0:1):IO,
C		DAV_D(0:*,0:*,0:1):IO)
C					Calculate all errors in the average
C					arrays JAV, EAV and DAV.
C					ARMS is the average amplitude of scan
C	NCARPC_L = NCARPC( ...)
C					Correct errors back
C	NCASPE_L = NCASPE( ...)
C					Calculate selfcal errors
C	NCASPC_L = NCASPC( ...)
C					Correct selfcal errors back
C	NCAAPE_L = NCAAPE( ...)
C					Calculate align errors
C	NCAAPC_L = NCAAPC( ...)
C					Correct align errors back
C
C				JAV, EAV, DAV contain:
C					*,*,0		gain
C					*,*,1		phase
C					0,0		noise per scan
C					1,0		inconsistency per scan
C					2,0		total noise
C					3,0		overall running noise
C					4,0		max. deviation in scan
C					5,0		total average noise
C					6,0		total average incons.
C					7,0		total average ampl.
C					*,1		inconsistency per ifr
C					*,2		average rms per ifr
C					*,3		gain per telescope
C!980701				*,4		weighted incons per ifr
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'LSQ_O_DEF'
	INCLUDE 'STH_O_DEF'			!SET HEADER
C
C  Entry points:
C
	LOGICAL NCARP1,NCARP2			!CALCULATE X/Y SIMULTANEOUS
	LOGICAL NCASPS				!CALCULATE SELFCAL
	LOGICAL NCAAPS				!CALCULATE ALIGN
	LOGICAL NCARPE,NCARPC			!CALCULATE ERRORS
	LOGICAL NCASPE,NCASPC			!CALCULATE SELFCAL ERRORS
	LOGICAL NCAAPE,NCAAPC			!CALCULATE ALIGN ERRORS
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER MAR				!SOLUTION AREA POINTER
	INTEGER NIFR				!TOTAL # OF INTERFEROMETERS
	INTEGER*2 IFR(0:*)			!INTERFEROMETER TELESCOPES
	INTEGER NDEG				!DEGENERACY LEVEL
	REAL BASEL(0:*)				!BASELINES
	INTEGER IRED(0:*)			!REDUNDANCY INDICATOR
	REAL WGT(0:STHIFR-1,0:*)		!DATA WEIGHT X,Y
	REAL AWGT(0:STHIFR-1,0:*)		!AMPLITUDE WEIGHTED WEIGHT X,Y
	COMPLEX CDAT(0:STHIFR-1,0:*)		!DATA COMPLEX X,Y
	REAL AMP(0:STHIFR-1,0:*)		!DATA AMPLITUDE X,Y
	REAL PHAS(0:STHIFR-1,0:*)		!DATA PHASE X,Y
	COMPLEX CMOD(0:STHIFR-1,0:*)		!MODEL COMPLEX X,Y
	REAL CWGT(0:*)				!MODEL WEIGHT**0.5
	REAL SOL(0:STHTEL-1,0:1,0:1)		!SOLUTION X,Y G,P
	REAL CSOL(0:STHTEL-1,0:1,0:1)		!CONTINUITY SOLUTION G,P X,Y
	REAL MU					!ADJUSTMENT ERROR
	REAL ME					!MEAN ERRORS SOLUTION
	INTEGER NUK				!# OF ALIGN EQUATIONS
	REAL ALEQ(0:STHTEL-1,0:*)		!ALIGN EQUATIONS
	REAL ARMS(0:2)				!AVERAGE AMPL.
	INTEGER JAV(0:STHIFR-1,0:4,0:1)		!COUNT FOR AVERAGES
	REAL EAV(0:STHIFR-1,0:4,0:1)		!SUM FOR AVERAGES
	REAL*8 DAV(0:STHIFR-1,0:4,0:1)		!SUM FOR RMS
C
C  Function references:
C
	REAL WNGENR				!ANGLE -180,+180
C
C  Data declarations:
C
	REAL CF(0:2*STHTEL-1),CG(0:2*STHTEL-1)	!COEFFICIENTS FOR SOLUTION
	INTEGER TW1,TE1,TW2,TE2			!TELESCOPES
	REAL W2,W22				!WEIGHTS
	REAL W4,W24
	REAL R2                                 !980701
	REAL CELES(0:STHIFR-1),WCELES(0:STHIFR-1) !CELESTIAL PHASES
	COMPLEX CX(0:STHIFR-1,0:1)		!SUM FOR GUESS
	REAL PHASX(0:STHIFR-1,0:1),PHASY(0:STHIFR-1,0:1) !CORRECTED PHASES
	REAL LSOL(0:STHTEL-1)			!LOCAL ALIGN SOLUTION
	INTEGER NR				!RANK SOLUTION
	INTEGER NU				!# UNKNOWNS
	LOGICAL DOQ0				!RG1/RG2 SWITCH
	LOGICAL DOXY				!XY SIMULTANEOUS SWITCH
	LOGICAL DOSC				!SELFCAL SWITCH
	LOGICAL DOAL				!ALIGN SWITCH
C-
C
C INIT
C
	NCARPS=.TRUE.					!ASSUME OK
	NU=STHTEL					!# OF UNKNOWNS
	DOXY=.FALSE.					!SEPARATE X/Y
	DOQ0=.FALSE.					!NO Q=0
	GOTO 20
C
C X/Y SIMULTANEOUS
C
	ENTRY NCARP1(MAR,NIFR,IFR,BASEL,NDEG,
	1			IRED,WGT,AWGT,CDAT,AMP,PHAS,CMOD,CWGT,
	1			CSOL,SOL,MU,ME)
C
	NCARP1=.TRUE.					!ASSUME OK
	NU=2*STHTEL					!# OF UNKNOWNS
	DOXY=.TRUE.					!COMBINE X/Y
	DOQ0=.FALSE.					!NO Q=0
	GOTO 20
C
C X/Y SIMULTANEOUS WITH Q=0
C
	ENTRY NCARP2(MAR,NIFR,IFR,BASEL,NDEG,
	1			IRED,WGT,AWGT,CDAT,AMP,PHAS,CMOD,CWGT,
	1			CSOL,SOL,MU,ME)
C
	NCARP2=.TRUE.					!ASSUME OK
	NU=2*STHTEL					!# OF UNKNOWNS
	DOXY=.TRUE.					!COMBINE X/Y
	DOQ0=.TRUE.					!Q=0
	GOTO 20
C
C SELFCAL SOLUTION
C
	ENTRY NCASPS(MAR,NIFR,IFR,BASEL,NDEG,
	1			IRED,WGT,AWGT,CDAT,AMP,PHAS,CMOD,CWGT,
	1			CSOL,SOL,MU,ME)
	NCASPS=.TRUE.					!ASSUME OK
	NU=STHTEL					!# OF UNKNOWNS
	DOXY=.FALSE.					!SEPARATE X/Y
	DOQ0=.FALSE.					!NO Q=0
	DOSC=.TRUE.					!SELFCAL
	GOTO 21
C
C ALIGN SOLUTION
C
	ENTRY NCAAPS(MAR,NIFR,IFR,BASEL,NDEG,
	1			IRED,WGT,AWGT,CDAT,AMP,PHAS,CMOD,CWGT,
	1			CSOL,SOL,MU,ME,NUK,ALEQ)
	NCAAPS=.TRUE.					!ASSUME OK
	NU=NUK						!# OF UNKNOWNS
	DOXY=.FALSE.					!SEPARATE X/Y
	DOQ0=.FALSE.					!NO Q=0
	DOSC=.FALSE.					!NO SELFCAL
	DOAL=.TRUE.					!ALIGN
	GOTO 22
C
C ZERO SOLUTION MATRIX
C
 20	CONTINUE
	DOSC=.FALSE.					!NOT SELFCAL
 21	CONTINUE
	DOAL=.FALSE.					!NOT ALIGN
 22	CONTINUE
	CALL WNMLIA(MAR,LSQ_I_ALL)			!FULL AREA
C
C MAKE GUESS FOR LOW LEVEL AMPLITUDES
C
	DO I=0,NIFR-1					!ZERO AVERAGE
	  CX(I,0)=0
	END DO
	IF (DOXY) THEN
	  DO I=0,NIFR-1					!ZERO AVERAGE Y
	    CX(I,1)=0
	  END DO
	END IF
	IF (.NOT.DOAL) THEN				!NOT FOR ALIGN
	 DO I=0,NIFR-1					!MAKE SUMS
	  IF (IRED(I).GT.0) THEN			!REDUNDANT
	    I1=IRED(I)					!REDUNDANT POINTER
	    TE1=IFR(I)/256				!TELESCOPES
	    TW1=MOD(IFR(I),256)
	    IF (WGT(I,0).GT.0) THEN
	      CX(I1,0)=CX(I1,0)+CDAT(I,0)*EXP(-CSOL(TW1,0,0)-
	1		CSOL(TE1,0,0))*CMPLX(COS(-CSOL(TW1,1,0)+
	1		CSOL(TE1,1,0)),SIN(-CSOL(TW1,1,0)+
	1		CSOL(TE1,1,0)))
	    END IF
	    IF (DOXY) THEN				!X/Y SIMULTANEOUS
	     IF (WGT(I,1).GT.0) THEN
	      IF (DOQ0) THEN				!Q=0
	        CX(I1,0)=CX(I1,0)+CDAT(I,1)*EXP(-CSOL(TW1,0,1)-
	1		CSOL(TE1,0,1))*CMPLX(COS(-CSOL(TW1,1,1)+
	1		CSOL(TE1,1,1)),SIN(-CSOL(TW1,1,1)+
	1		CSOL(TE1,1,1)))
	      ELSE
	        CX(I1,1)=CX(I1,1)+CDAT(I,1)*EXP(-CSOL(TW1,0,1)-
	1		CSOL(TE1,0,1))*CMPLX(COS(-CSOL(TW1,1,1)+
	1		CSOL(TE1,1,1)),SIN(-CSOL(TW1,1,1)+
	1		CSOL(TE1,1,1)))
	      END IF
	     END IF
	    END IF
	  END IF
	 END DO
	 DO I=0,NIFR-1					!MAKE GUESS
	  IF (ABS(CX(I,0)).NE.0) THEN
	    PHASY(I,0)=ATAN2(AIMAG(CX(I,0)),REAL(CX(I,0)))
	  ELSE
	    PHASY(I,0)=0
	  END IF
	 END DO
	 IF (DOXY .AND. .NOT.DOQ0) THEN
	  DO I=0,NIFR-1					!MAKE GUESS
	    IF (ABS(CX(I,1)).NE.0) THEN
	      PHASY(I,1)=ATAN2(AIMAG(CX(I,1)),REAL(CX(I,1)))
	    ELSE
	      PHASY(I,1)=0
	    END IF
	  END DO
	 END IF
	END IF
	DO I=0,NIFR-1					!CORRECT PHASES
	  I1=IRED(I)					!REDUNDANT POINTER
	  TE1=IFR(I)/256				!TELESCOPES
	  TW1=MOD(IFR(I),256)
	  IF (.NOT.DOAL .AND. IRED(I).GT.0) THEN	!REDUNDANT
	    PHASX(I,0)=WNGENR(PHAS(I,0)-CSOL(TW1,1,0)+
	1		CSOL(TE1,1,0)-PHASY(I1,0))
	    IF (DOXY) THEN
	      IF (DOQ0) THEN
	        PHASX(I,1)=WNGENR(PHAS(I,1)-CSOL(TW1,1,1)+
	1		CSOL(TE1,1,1)-PHASY(I1,0))
	      ELSE
	        PHASX(I,1)=WNGENR(PHAS(I,1)-CSOL(TW1,1,1)+
	1		CSOL(TE1,1,1)-PHASY(I1,1))
	      END IF
	    END IF
	  ELSE IF (DOSC .OR. DOAL) THEN
	    PHASX(I,0)=WNGENR(PHAS(I,0)-CSOL(TW1,1,0)+
	1		CSOL(TE1,1,0))
	    IF (DOXY) THEN
	      IF (DOQ0) THEN
	        PHASX(I,1)=WNGENR(PHAS(I,1)-CSOL(TW1,1,1)+
	1		CSOL(TE1,1,1))
	      ELSE
	        PHASX(I,1)=WNGENR(PHAS(I,1)-CSOL(TW1,1,1)+
	1		CSOL(TE1,1,1))
	      END IF
	    END IF
	  END IF
	END DO
C
C MAKE MATRIX
C
	I1=0						!TEST REDUNDANT BASELINE
	DO I=0,NIFR-1					!ALL IFRS
	  IF (.NOT.DOAL .AND. IRED(I).GT.0) THEN	!REDUNDANT
	    IF (IRED(I).GT.I1) THEN			!NEXT SET
	      IF (WGT(I,0).GT.0 .AND. (.NOT.DOXY .OR. (DOXY .AND.
	1			WGT(I,1).GT.0))) THEN	!CAN USE AS BASE
	        I1=IRED(I)				!NEW TEST VALUE
	        I4=I
	        TE1=IFR(I)/256				!TELESCOPES
	        TW1=MOD(IFR(I),256)
	        W2=AWGT(I,0)				!SAVE WEIGHT
	        IF (DOXY) W4=AWGT(I,1)
	        IF (DOQ0) THEN				!Q=0
	          DO I2=0,NU-1
	            CF(I2)=0				!ZERO COEFFICIENTS
	          END DO
	          CF(TW1)=CF(TW1)+1
	          CF(STHTEL+TW1)=CF(STHTEL+TW1)-1
	          CF(TE1)=CF(TE1)-1
	          CF(STHTEL+TE1)=CF(STHTEL+TE1)+1
	          CALL WNMLMN(MAR,LSQ_C_REAL,CF,W4*W2/(W4+W2),
	1		WNGENR(PHAS(I,0)-PHAS(I,1)-
	1		CSOL(TW1,1,0)+CSOL(TE1,1,0)
	1		+CSOL(TW1,1,1)-CSOL(TE1,1,1)))
	        END IF
	        IF (DOSC) THEN				!SELFCAL
		  W4=(CWGT(I)*ABS(CMOD(I,0)))**2	!MODEL WEIGHT
		  IF (W4.NE.0) THEN
	            DO I2=0,NU-1
	              CF(I2)=0				!ZERO COEFFICIENTS
	            END DO
	            CF(TW1)=CF(TW1)+1
	            CF(TE1)=CF(TE1)-1
	            CALL WNMLMN(MAR,LSQ_C_REAL,CF,W4*W2/(W4+W2),
	1		WNGENR(PHAS(I,0)-
	1		ATAN2(AIMAG(CMOD(I,0)),REAL(CMOD(I,0)))-
	1		CSOL(TW1,1,0)+CSOL(TE1,1,0)))
		  END IF
	        END IF
	        DO I3=I+1,NIFR-1				!FIND OTHERS 
	          IF (IRED(I3).EQ.I1.AND.
	1	    WGT(I3,0).GT.0 .AND. (.NOT.DOXY .OR. (DOXY .AND.
	1		WGT(I3,1).GT.0))) THEN	!CAN INCLUDE
	            TE2=IFR(I3)/256				!TELESCOPES
	            TW2=MOD(IFR(I3),256)
	            W22=AWGT(I3,0)				!WEIGHTS
		    IF (DOXY) W24=AWGT(I3,1)
	            DO I2=0,NU-1
	              CF(I2)=0				!ZERO COEFFICIENTS
	            END DO
	            CF(TW1)=CF(TW1)+1			!SET COEFFICIENTS
	            CF(TE1)=CF(TE1)-1
	            CF(TW2)=CF(TW2)-1
	            CF(TE2)=CF(TE2)+1
	            CALL WNMLMN(MAR,LSQ_C_REAL,CF,W2*W22/(W2+W22),
	1		PHASX(I4,0)-PHASX(I3,0))
	            IF (DOXY) THEN				!XY SIMULTANEOUS
	              DO I2=0,NU-1
	                CF(I2)=0				!ZERO COEFFICIENTS
	              END DO
	              CF(STHTEL+TW1)=CF(STHTEL+TW1)+1	!SET COEFFICIENTS
	              CF(STHTEL+TE1)=CF(STHTEL+TE1)-1
	              CF(STHTEL+TW2)=CF(STHTEL+TW2)-1
	              CF(STHTEL+TE2)=CF(STHTEL+TE2)+1
	              CALL WNMLMN(MAR,LSQ_C_REAL,CF,W4*W24/(W4+W24),
	1		PHASX(I4,1)-PHASX(I3,1))
		    END IF
	          END IF
	        END DO
	      END IF
	    END IF
	  ELSE IF (DOQ0 .AND. WGT(I,0).GT.0 .AND. WGT(I,1).GT.0) THEN
	    W22=AWGT(I,0)				!WEIGHTS
	    W24=AWGT(I,1)
	    TE2=IFR(I)/256				!TELESCOPES
	    TW2=MOD(IFR(I),256)
	    DO I2=0,NU-1
	      CF(I2)=0					!ZERO COEFFICIENTS
	    END DO
	    CF(TW2)=CF(TW2)+1
	    CF(STHTEL+TW2)=CF(STHTEL+TW2)-1
	    CF(TE2)=CF(TE2)-1
	    CF(STHTEL+TE2)=CF(STHTEL+TE2)+1
	    CALL WNMLMN(MAR,LSQ_C_REAL,CF,W24*W22/(W24+W22),
	1		WNGENR(PHAS(I,0)-PHAS(I,1)-
	1		CSOL(TW2,1,0)+CSOL(TE2,1,0)
	1		+CSOL(TW2,1,1)-CSOL(TE2,1,1)))
	  ELSE IF (DOSC .AND. WGT(I,0).GT.0) THEN
	    W22=AWGT(I,0)				!WEIGHTS
	    W24=(CWGT(I)*ABS(CMOD(I,0)))**2
	    TE2=IFR(I)/256				!TELESCOPES
	    TW2=MOD(IFR(I),256)
	    IF (W24.NE.0) THEN
	      DO I2=0,NU-1
	        CF(I2)=0				!ZERO COEFFICIENTS
	      END DO
	      CF(TW2)=CF(TW2)+1
	      CF(TE2)=CF(TE2)-1
	      CALL WNMLMN(MAR,LSQ_C_REAL,CF,W24*W22/(W24+W22),
	1		WNGENR(PHAS(I,0)-
	1		ATAN2(AIMAG(CMOD(I,0)),REAL(CMOD(I,0)))-
	1		CSOL(TW2,1,0)+CSOL(TE2,1,0)))
	    END IF
	  ELSE IF (DOAL .AND. WGT(I,0).GT.0) THEN	!ALIGN
	    W22=AWGT(I,0)				!WEIGHTS
	    W24=(CWGT(I)*ABS(CMOD(I,0)))**2
	    TE2=IFR(I)/256				!TELESCOPES
	    TW2=MOD(IFR(I),256)
	    IF (W24.NE.0) THEN
	      DO I2=0,NU-1
	        CF(I2)=ALEQ(TW2,I2)-ALEQ(TE2,I2)	!SET COEFFICIENTS
	      END DO
	      CALL WNMLMN(MAR,LSQ_C_REAL,CF,W24*W22/(W24+W22),
	1		WNGENR(PHAS(I,0)-
	1		ATAN2(AIMAG(CMOD(I,0)),REAL(CMOD(I,0)))-
	1		CSOL(TW2,1,0)+CSOL(TE2,1,0)))
	    END IF
	  END IF
	END DO
C
C INVERT NORMAL EQUATIONS
C
	CALL WNMLID(MAR)				!FIX MISSING TELESCOPES
	CALL WNMLTR(MAR,NR)				!LU DECOMP. + RANK
	NDEG=NU-NR					!DEGENERACY
C
C SOLVE
C
	CALL WNMLSN(MAR,SOL,MU,ME)			!GET SOLUTION
	DO I=0,NU-1					!CHECK FUNNY SOLUTION
	  IF (ABS(SOL(I,0,0)).GT.5.) NCARPS=.FALSE.
	END DO
	IF (NCARPS .AND. DOAL) THEN			!MAKE ALIGN SOLUTION
	  DO I=0,NU-1
	    LSOL(I)=SOL(I,0,0)				!SAVE SOLUTION
	  END DO
	  DO I=0,STHTEL-1				!SET ALIGN SOLUTION
	    SOL(I,0,0)=0
	    DO I1=0,NU-1
	      SOL(I,0,0)=SOL(I,0,0)+ALEQ(I,I1)*LSOL(I1)
	    END DO
	  END DO
	END IF
C
	RETURN						!READY
C
C ERROR CALCULATION
C
	ENTRY NCARPE(MAR,NIFR,IFR,BASEL,NDEG,
	1			IRED,WGT,AWGT,CDAT,AMP,PHAS,CMOD,CWGT,
	1			CSOL,SOL,MU,ME,
	1			ARMS,JAV,EAV,DAV)
C
	NCARPE=.TRUE.					!ASSUME OK
	J0=1						!ADD ERRORS
	DOSC=.FALSE.					!NO SELFCAL
	DOAL=.FALSE.					!NOT ALIGN
	GOTO 10
C
C ERROR CORRECTION
C
	ENTRY NCARPC(MAR,NIFR,IFR,BASEL,NDEG,
	1			IRED,WGT,AWGT,CDAT,AMP,PHAS,CMOD,CWGT,
	1			CSOL,SOL,MU,ME,
	1			ARMS,JAV,EAV,DAV)
C
	NCARPC=.TRUE.					!ASSUME OK
	J0=-1						!SUBTRACT ERROR
	DOSC=.FALSE.					!NO SELFCAL
	DOAL=.FALSE.					!NOT ALIGN
	GOTO 10
C
C SELFCAL ERROR CALCULATION
C
	ENTRY NCASPE(MAR,NIFR,IFR,BASEL,NDEG,
	1			IRED,WGT,AWGT,CDAT,AMP,PHAS,CMOD,CWGT,
	1			CSOL,SOL,MU,ME,
	1			ARMS,JAV,EAV,DAV)
C
	NCASPE=.TRUE.					!ASSUME OK
	J0=1						!ADD ERRORS
	DOSC=.TRUE.					!SELFCAL
	DOAL=.FALSE.					!NOT ALIGN
	GOTO 10
C
C SELFCAL ERROR CORRECTION
C
	ENTRY NCASPC(MAR,NIFR,IFR,BASEL,NDEG,
	1			IRED,WGT,AWGT,CDAT,AMP,PHAS,CMOD,CWGT,
	1			CSOL,SOL,MU,ME,
	1			ARMS,JAV,EAV,DAV)
C
	NCASPC=.TRUE.					!ASSUME OK
	J0=-1						!SUBTRACT ERROR
	DOSC=.TRUE.					!SELFCAL
	DOAL=.FALSE.					!NOT ALIGN
	GOTO 10
C
C ALIGN ERROR CALCULATION
C
	ENTRY NCAAPE(MAR,NIFR,IFR,BASEL,NDEG,
	1			IRED,WGT,AWGT,CDAT,AMP,PHAS,CMOD,CWGT,
	1			CSOL,SOL,MU,ME,
	1			ARMS,JAV,EAV,DAV)
C
	NCAAPE=.TRUE.					!ASSUME OK
	J0=1						!ADD ERRORS
	DOSC=.FALSE.					!NOT SELFCAL
	DOAL=.TRUE.					!ALIGN
	GOTO 10
C
C ALIGN ERROR CORRECTION
C
	ENTRY NCAAPC(MAR,NIFR,IFR,BASEL,NDEG,
	1			IRED,WGT,AWGT,CDAT,AMP,PHAS,CMOD,CWGT,
	1			CSOL,SOL,MU,ME,
	1			ARMS,JAV,EAV,DAV)
C
	NCAAPC=.TRUE.					!ASSUME OK
	J0=-1						!SUBTRACT ERROR
	DOSC=.FALSE.					!NOT SELFCAL
	DOAL=.TRUE.					!ALIGN
	GOTO 10
C
C GET CELESTIAL PHASES
C
 10	CONTINUE
	IF (DOSC .OR. DOAL) THEN			!SELFCAL/ALIGN
	  DO I=0,NIFR-1
	    IF (ABS(CMOD(I,0)).NE.0) THEN
	      CELES(I)=ATAN2(AIMAG(CMOD(I,0)),REAL(CMOD(I,0)))
	    ELSE
	      CELES(I)=0
	    END IF
	  END DO
	ELSE
	  DO I=0,NIFR-1					!ZERO CELESTIAL SOLUTION
	    CELES(I)=0
	    WCELES(I)=0
	  END DO
	  DO I=0,NIFR-1					!CALCULATE
	    IF (IRED(I).GT.0) THEN			!REDUNDANT
	      IF (WGT(I,0).GT.0) THEN			!CAN USE
	        I1=IRED(I)				!POINTER
	        TE1=IFR(I)/256				!TELESCOPES
	        TW1=MOD(IFR(I),256)
	        W2=AWGT(I,0)				!WEIGHT
	        CELES(I1)=CELES(I1)+W2*WNGENR(PHAS(I,0)-
	1		CSOL(TW1,1,0)+CSOL(TE1,1,0))	!SUM
	        WCELES(I1)=WCELES(I1)+W2		!SUM WEIGHT
	      END IF
	    END IF
	  END DO
	  DO I=0,NIFR-1					!SOLVE
	    IF (WCELES(I).GT.0) CELES(I)=CELES(I)/WCELES(I)
	  END DO
	END IF
C
C INIT
C
	JAV(0,0,1)=0					!NOISE
	EAV(0,0,1)=0
	DAV(0,0,1)=0
	JAV(1,0,1)=0					!FRACT. NOISE (INCONS.)
	EAV(1,0,1)=0
	DAV(1,0,1)=0
	JAV(4,0,1)=0					!MAX. DEVIATION
	EAV(4,0,1)=0
	IF (JAV(2,0,1).NE.0) THEN			!TOTAL COUNT
	  EAV(3,0,1)=SQRT(EAV(2,0,1)/JAV(2,0,1))	!OVERALL RUNNING NOISE
	ELSE
	  EAV(3,0,1)=0
	END IF
C
C DO FOR IFRS
C
	DO I=0,NIFR-1					!ALL IFRS
	  IF (DOSC .OR. DOAL .OR. IRED(I).GT.0) THEN	!REDUNDANT
	    IF (WGT(I,0).GT.0) THEN			!CAN USE
	      IF (DOSC .OR. DOAL) THEN			!SELFCAL/ALIGN
	        I1=I
	      ELSE
	        I1=IRED(I)				!REDUNDANT POINTER
	      END IF
	      TE1=IFR(I)/256				!TELESCOPES
	      TW1=MOD(IFR(I),256)
C
C ERROR
C
	      R0=WNGENR(PHAS(I,0)-CSOL(TW1,1,0)+CSOL(TE1,1,0)-CELES(I1)) !ERROR
	      IF (ABS(R0).GT.PI/2.) R0=R0-SIGN(PI,R0)	!LIMIT EXTREME
	      R1=SIN(R0)*AMP(I,0)			!FRACTIONAL ERROR
	      R2=AMP(I,0)                               !980701
C
C EXTREME
C
	      IF (ABS(R0).GT.ABS(EAV(4,0,1))) THEN	!MAX. DEVIATION
		EAV(4,0,1)=R0
		JAV(4,0,1)=TW1*16+TE1			!WHERE
	      END IF
C
C NOISE
C
	      JAV(0,0,1)=JAV(0,0,1)+J0			!COUNT
	      EAV(0,0,1)=EAV(0,0,1)+J0*(R1**2)		!NOISE
	      JAV(1,0,1)=JAV(1,0,1)+J0			!INCONSISTENCY
	      EAV(1,0,1)=EAV(1,0,1)+J0*(R0**2)
	      JAV(5,0,1)=JAV(5,0,1)+J0			!AVG NOISE
	      EAV(5,0,1)=EAV(5,0,1)+J0*R1
	      JAV(6,0,1)=JAV(6,0,1)+J0			!AVG INCONSISTENCY
	      EAV(6,0,1)=EAV(6,0,1)+J0*R0
	      JAV(2,0,1)=JAV(2,0,1)+J0			!AVG RMS
	      EAV(2,0,1)=EAV(2,0,1)+J0*(R1**2)
	      JAV(I,1,1)=JAV(I,1,1)+J0			!IFR AVG NOISE
	      DAV(I,1,1)=DAV(I,1,1)+J0*R1
	      EAV(I,1,1)=EAV(I,1,1)+J0*R0		!FRACT. NOISE
	      JAV(I,2,1)=JAV(I,2,1)+J0			!IFR AVG RMS
	      DAV(I,2,1)=DAV(I,2,1)+J0*(R1**2)
	      EAV(I,4,1)=EAV(I,4,1)+J0*R0*R2*R2         !980701
	      DAV(I,4,1)=DAV(I,4,1)+J0*R2*R2            !980701
	    END IF
	  END IF
	END DO
C
C NOISES AND TEL. PHASES
C
	IF (JAV(0,0,1).GT.0) THEN			!AVERAGE POSSIBLE
	  EAV(0,0,1)=SQRT(EAV(0,0,1)/JAV(0,0,1))	!NOISE
	  EAV(1,0,1)=SQRT(EAV(1,0,1)/JAV(1,0,1))	!INCONSISTENCY
	  DO I=0,STHTEL-1				!PER TELESCOPE
	    JAV(I,3,1)=JAV(I,3,1)+J0			!COUNT
	    EAV(I,3,1)=EAV(I,3,1)+J0*CSOL(I,1,0)	!AVERAGE CORRECTION
	    DAV(I,3,1)=DAV(I,3,1)+J0*CSOL(I,1,0)*CSOL(I,1,0) !PHASE RMS
	  END DO
	END IF
C
	RETURN
C
C
	END