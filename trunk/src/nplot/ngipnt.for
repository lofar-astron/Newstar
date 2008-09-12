C+ NGIPNT.FOR
C  WNB 930510
C
C  Revisions:
C	WNB 930514	Use NGIDPT; make use of MB2
C	WNB 930621	Add flagging
C	WNB 930803	CBITS_DEF
C	WNB 930820	Interchange East/West telescope in IFR
C	CMV 931026	Add re-open to allow for resizing (see NGIDAT.FOR)
C	AXC 010709      Linux port - DTC setting and checking
C
	SUBROUTINE NGIPNT(PTP)
C
C  Show information on map points
C
C  Result:
C       CALL NGIPNT( PTP_J:I)		Gives information on points/area in map
C					(PTP=0) or flags (PTP=1)
C					MB1(or 1) gives info
C					MB3(or 3) leaves
C					MB2(or 2) starts area (MB1 other point)
C					others nothing
C
C       
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'NGI_DEF'
	INCLUDE 'CBITS_DEF'
	INCLUDE 'MPH_O_DEF'
	INCLUDE 'STH_O_DEF'
	INCLUDE 'FLF_O_DEF'	!FLAG FILE ENTRY
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER PTP		!TYPE TO DO
C
C  Function references:
C
	LOGICAL WNFRD		!READ DISK
	LOGICAL NGIDPT		!GET POINT
	LOGICAL NFLFL1		!WRITE FLAG ENTRY
	LOGICAL NGIDOP		!OPEN CONNECTION TO GIDS WINDOW
	LOGICAL NGIDCL		!CLOSE CONNECTION TO GIDS WINDOW
C
C  Data declarations:
C
	REAL RBUF(0:1,0:1),RB(2) !PGPLOT DATA (X:Y,MB2:MB1)
	INTEGER BUT		!BUTTON PRESSED
	REAL LM(2),L,M		!L,M
	  EQUIVALENCE (L,LM(1)),(M,LM(2))
	DOUBLE PRECISION D00(0:1),D10(0:1) !COORDINATES
	REAL SCL(0:1)		!UV COORDINATES
	REAL DAT(0:1)		!DATA
	INTEGER UVTP		!UV DATA TYPE
	INTEGER LAR(0:3,0:1)	!LOCAL DEFINED AREA
	INTEGER ACNT		!AREA INPUT COUNT
	INTEGER ARSN		!AREA SEEN (1)
	CHARACTER*16 TSTR	!TELESCOPE NAMES
	  DATA TSTR /'0123456789ABCDEF'/
	BYTE MPH(0:MPHHDL-1)	!MAP HEADER
	  INTEGER*2 MPHI(0:MPHHDL/2-1)
	  INTEGER MPHJ(0:MPHHDL/4-1)
	  REAL MPHE(0:MPHHDL/4-1)
	  DOUBLE PRECISION MPHD(0:MPHHDL/8-1)
	  EQUIVALENCE (MPH,MPHI,MPHJ,MPHE,MPHD)
	BYTE FLF(0:FLFHDL-1)	!FLAG ENTRY
	  INTEGER*2 FLFI(0:FLFHDL/LB_I-1)
	  INTEGER FLFJ(0:FLFHDL/LB_J-1)
	  REAL FLFE(0:FLFHDL/LB_E-1)
	  EQUIVALENCE(FLF,FLFI,FLFJ,FLFE)
	CHARACTER*2 STR1,STR2
	CHARACTER*4 DTC
C-
C
C Re-open GIDS window
C
	IF (.NOT.NGIDOP(GID)) THEN
	   CALL WNCTXT (F_TP,'Error re-opening GIDS display')
	   GOTO 800
	END IF
C
C GET MAP INFO
C
	IF (.NOT.WNFRD(FCAIN,MPHHDL,MPH,PTR)) GOTO 11 !READ HEADER
	J0=MPHJ(MPH_TYP_1/LB_J)			!DATA TYPE
	DTC(4:4)=CHAR(J0/256/256/256)
	DTC(3:3)=CHAR(MOD(J0/256/256,256))
	DTC(2:2)=CHAR(MOD(J0/256,256))
	DTC(1:1)=CHAR(MOD(J0,256))
	IF (DTC.EQ.'COVE') THEN			!SET DATA TYPE
	  UVTP=1
	ELSE IF (DTC.EQ.'REAL') THEN
	  UVTP=2
	ELSE IF (DTC.EQ.'IMAG') THEN
	  UVTP=3
	ELSE IF (DTC.EQ.'AMPL') THEN
	  UVTP=4
	ELSE IF (DTC.EQ.'PHAS') THEN
	  UVTP=5
	ELSE
	  UVTP=0				!STANDARD MAP
	END IF
	IF (PTP.EQ.1 .AND. UVTP.EQ.0) THEN
	  CALL WNCTXT(F_TP,'Cannot FLAG on standard map; POINT assumed')
	END IF
	RB(1)=-1.				!POINT TO CENTRE
	RB(2)=-1.
	ACNT=0					!NO AREA POINT
C
C GET DATA POINTS FROM USER
C
	FLFJ(FLF_FLAG_J)=UFL			!FILL FLAG TO USE
 10	CONTINUE
	ARSN=0					!NO AREA SEEN
	IF (.NOT.NGIDPT(GID,COMPR,TEAR,RB,RBUF(0,ACNT),BUT))
	1		GOTO 800		!GET A POINT
	IF (BUT.EQ.2) ACNT=0			!START AREA
	IF (BUT.EQ.2 .OR. ACNT.NE.0) THEN	!AREA
	  DO I=0,1				!SET CORNER
	    LAR(I+2*ACNT,0)=RBUF(I,ACNT)
	  END DO
	  ACNT=ACNT+1				!COUNT
	  IF (ACNT.EQ.2) THEN			!HAVE TWO
	    ACNT=0				!RESET COUNT
	    DO I=1,2
	      LAR(I+1,1)=MAX(LAR(I-1,0),LAR(I+1,0)) !MAX 
	      LAR(I-1,0)=MIN(LAR(I-1,0),LAR(I+1,0)) !MIN
	      LAR(I+1,1)=LAR(I+1,1)-LAR(I-1,0)+1 !WIDTH
	      LAR(I-1,1)=LAR(I-1,0)+LAR(I+1,1)/2 !CENTRE
	    END DO
	    CALL WNCTXT(F_TP,'(!5$4SJ)',LAR(0,1)) !SHOW
	    ARSN=1				!HANDLE AREA
	  END IF
	  IF (ARSN.EQ.0 .OR. PTP.EQ.0) GOTO 10	!NO FLAGGING
	END IF
C
C GET DISK DATA
C
	IF (ARSN.EQ.0) THEN
	  J0=MPHJ(MPH_MDP_J)+(RBUF(1,0)+NDEC/2)*NRA*LB_E+
	1		(RBUF(0,0)+NRA/2)*LB_E	!GET DATA POINTER
	  IF (.NOT.WNFRD(FCAIN,2*LB_E,DAT,J0)) THEN !READ DATA
 11	    CONTINUE
	    CALL WNCTXT(F_TP,'Error reading data')
	    GOTO 800
	  END IF
	END IF
C
C SHOW/FLAG DATA
C
C STANDARD MAP
C
	IF (UVTP.EQ.0) THEN			!STANDARD MAP
	  IF (ARSN.EQ.0) THEN
	    L=RBUF(0,0)*(MPHD(MPH_SRA_D))+(MPHD(MPH_SHR_D)) !MAKE L,M
	    M=RBUF(1,0)*(MPHD(MPH_SDEC_D))+(MPHD(MPH_SHD_D))
	    CALL WNMCLM(((MPHD(MPH_RA_D))),
	1			((MPHD(MPH_DEC_D))),
	1			PI2*L,PI2*M,D0,D1) !RA/DEC
	    L=L*3600*360			!ARCSEC
	    M=M*3600*360
	    CALL WNCTXT(F_TP,'!5$2E: !15C!9$E9.2 WU at !31C(!9$2E10.2)'//
	1		'!54C(!11$DPF11.5, !11$DAF11.5)'//
	1		'!/!15C!9$E9.5 Jy!54C(!11$DHF8, !11$DDF7)',
	1		RBUF(0,0),DAT(0),LM,D0,D1,
	1		DAT(0)/200.,D0,D1)
	  END IF
C
C UV TYPE: GET SCALES AND COORDINATES
C
	ELSE					!UV TYPE
          DO I=0,1                          	!GET SCALES
            SCL(I)=MPHD(MPH_SRA_D+I)*MPHJ(MPH_FSR_J+I)*2*DPI
          END DO
          IF (MPHI(MPH_CD_I+6).NE.0) THEN
            DO I=0,1
              SCL(I)=SCL(I)*MPHD(MPH_FRQ_D)/(DCL*1D-6) !ABS. SCALE
            END DO
            SCL(1)=SCL(1)*DPI/180D0         	!MAKE DEGREES
          END IF
	  DO I2=0,ARSN
            D00(I2)=(RBUF(1,I2)+NDEC/2)/SCL(0)	!GET COORD
            D10(I2)=RBUF(0,I2)/SCL(1)
	  END DO
	  IF (ARSN.EQ.0) THEN
	    IF (UVTP.EQ.5) THEN			!PHASE
	      STR1='dg'
	      DAT(0)=DAT(0)*360			!MAKE DEGREES
	    ELSE IF (UVTP.EQ.1) THEN		!COVER
	      STR1='  '
	    ELSE				!AMPL. TYPE
	      STR1='WU'
	    END IF
	  END IF
C
C UV
C
          IF (MPHI(MPH_CD_I+6).EQ.0) THEN   	!UV
	    IF (ARSN.EQ.0 .AND. DAT(0).EQ.0.0) THEN
	      CALL WNCTXT(F_TP,'Blank data')
	    ELSE
	      IF (ARSN.EQ.0)
	1	CALL WNCTXT(F_TP,'!5$2E: !15C!9$E9.2 !2$AS at '//
	1               '!31C(!10$D10.1, !10$D10.1)'//
	1               '!56C(!10$D10.2, !10$D10.2)',
	1		RBUF(0,0),DAT(0),STR1,D00(0),D10(0),
	1		D00(0)/(MPHD(MPH_FRQ_D)/(DCL*1D-6)),
	1		D10(0)/(MPHD(MPH_FRQ_D)/(DCL*1D-6)))
	      IF (PTP.EQ.1) THEN		!FLAG ENTRY
	        FLFJ(FLF_FLAG_J)=IOR(IAND(FL_ALL,FLFJ(FLF_FLAG_J)),
	1			'01000000'X)
	        FLFI(FLF_POL_I)=-1
	        FLFJ(FLF_CHAN_J)=-1
		DO I2=0,ARSN
	          IF (MPHD(MPH_DEC_D).NE.0) D10(I2)=
	1		D10(I2)/SIN(DPI2*MPHD(MPH_DEC_D))
	          D00(I2)=D00(I2)/(MPHD(MPH_FRQ_D)/(DCL*1D-6))
	          D10(I2)=D10(I2)/(MPHD(MPH_FRQ_D)/(DCL*1D-6))
	          FLFI(FLF_IFR_I)=NINT(SQRT(ABS(D00(I2)*D00(I2)+
	1			D10(I2)*D10(I2))))
		  IF (ABS(D00(I2))+ABS(D10(I2)).NE.0) THEN
	            FLFE(FLF_HA_E)=ATAN2(-D10(I2),D00(I2))/DPI2
		  ELSE
	            FLFE(FLF_HA_E)=0.
		  END IF
		  IF (I2.EQ.0) THEN
		    FLFJ(FLF_FLAG_J)=IOR(FLFJ(FLF_FLAG_J),ARSN)
		  ELSE
		    FLFJ(FLF_FLAG_J)=IOR(FLFJ(FLF_FLAG_J),2)
		  END IF
	          IF (.NOT.NFLFL1(DFAR,FLF))
	1		CALL WNCTXT(F_TP,'Error writing FLF entry')
		  FLFJ(FLF_FLAG_J)=IAND(FLFJ(FLF_FLAG_J),NOT(3))
		END DO
	      END IF
	    END IF
C
C BAS-HA
C
	  ELSE IF (MPHI(MPH_CD_I+6).EQ.1) THEN !BAS-HA
	    IF (ARSN.EQ.0 .AND. DAT(0).EQ.0.0) THEN
	      CALL WNCTXT(F_TP,'Blank data')
	    ELSE
	      IF (ARSN.EQ.0)
	1	CALL WNCTXT(F_TP,'!5$2E: !15C!9$E9.2 !2$AS at '//
	1               '!31C(!10$D10.1, !10$D10.1)',
	1		RBUF(0,0),DAT(0),STR1,D00(0),D10(0))
	      IF (PTP.EQ.1) THEN		!FLAG ENTRY
	        FLFJ(FLF_FLAG_J)=IOR(IAND(FL_ALL,FLFJ(FLF_FLAG_J)),
	1			'01000000'X)
	        FLFI(FLF_POL_I)=-1
	        FLFJ(FLF_CHAN_J)=-1
		DO I2=0,ARSN
	          FLFI(FLF_IFR_I)=NINT(D00(I2))
	          FLFE(FLF_HA_E)=D10(I2)/360.
		  IF (I2.EQ.0) THEN
		    FLFJ(FLF_FLAG_J)=IOR(FLFJ(FLF_FLAG_J),ARSN)
		  ELSE
		    FLFJ(FLF_FLAG_J)=IOR(FLFJ(FLF_FLAG_J),2)
		  END IF
	          IF (.NOT.NFLFL1(DFAR,FLF))
	1		CALL WNCTXT(F_TP,'Error writing FLF entry')
		  FLFJ(FLF_FLAG_J)=IAND(FLFJ(FLF_FLAG_J),NOT(3))
		END DO
	      END IF
	    END IF
C
C IFR-HA
C
	  ELSE					!IFR-HA
	    IF (ARSN.EQ.0 .AND. DAT(0).EQ.0.0) THEN
	      CALL WNCTXT(F_TP,'Blank data')
	    ELSE
	      IF (PTP.EQ.1) THEN		!FLAG ENTRY
	        FLFJ(FLF_FLAG_J)=IAND(FL_ALL,FLFJ(FLF_FLAG_J))
	        FLFI(FLF_POL_I)=-1
	        FLFJ(FLF_CHAN_J)=-1
	      END IF
	      DO I2=0,ARSN
	        I0=NINT(D00(I2))		!IFR NUMBER
	        I=STHTEL			!FIND IFR NAME
	        I1=0
	        I0=MOD(I0,STHIFR)
	        DO WHILE (I0.GE.0)
	          I0=I0-I
	          I=I-1
	          I1=I1+1
	        END DO
	        I1=I1-1
	        I=I+1
	        I0=MIN(I0+I+I1,STHTEL)		!EAST TEL.
		I1=MIN(I1,STHTEL)		!WEST TELESCOPE
	        IF (ARSN.EQ.0 .AND. I2.EQ.0) THEN
	          STR2=TSTR(I1+1:I1+1)//TSTR(I0+1:I0+1)
		  CALL WNCTXT(F_TP,'!5$2E: !15C!9$E9.2 !2$AS at '//
	1               '!31C(!2$AS, !10$D10.1)',
	1		RBUF(0,0),DAT(0),STR1,STR2,D10(0))
		END IF
	        IF (PTP.EQ.1) THEN		!FLAG ENTRY
	          FLFI(FLF_IFR_I)=I0*256+I1	!EAST*256 + WEST
	          FLFE(FLF_HA_E)=D10(I2)/360.
		  IF (I2.EQ.0) THEN
		    FLFJ(FLF_FLAG_J)=IOR(FLFJ(FLF_FLAG_J),ARSN)
		  ELSE
		    FLFJ(FLF_FLAG_J)=IOR(FLFJ(FLF_FLAG_J),2)
		  END IF
	          IF (.NOT.NFLFL1(DFAR,FLF))
	1		CALL WNCTXT(F_TP,'Error writing FLF entry')
		  FLFJ(FLF_FLAG_J)=IAND(FLFJ(FLF_FLAG_J),NOT(3))
		END IF
	      END DO
	    END IF
	  END IF
	END IF
	GOTO 10					!CYCLE
C
 800	CONTINUE
C
C Close GIDS window again
C
	JS=NGIDCL(GID)
C
	RETURN
C
C
	END
