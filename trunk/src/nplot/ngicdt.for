C+ NGICDT.FOR
C  CMV 931123
C
C  Revisions:
C	CMV 931123	Created
C	WNB 931221	Double declared RDAT; wnctxt format error; Ampl/phaes
C	CMV 940218	Add option to blank flagged data,
C	CMV 940218	Changed sequence in NSCSCF
C	CMV 940415	Phases in range -180, 180
C	CMV 940622	Move DO_BLANK to calling routine
C
	LOGICAL FUNCTION NGICDT(ISCN,STH_IN,HEAD,IFRT,FLG,RDAT)
C
C  Initialise sector and scan, read corrected data and flags, 
C  get requested data type.
C
C  Result:
C
C       NGICDT_J = NGICDT(ISCN_J:I, STH_IN_B(0:*):I,
C	                  HEAD_L:O, IFRT_I(0:*):O, 
C	                  FLG(0:*,0:3)_J:O, RDAT(0:*,0:3)_E:O)
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'NGI_DEF'
	INCLUDE 'CBITS_DEF'
	INCLUDE 'STH_O_DEF'
	INCLUDE 'SCH_O_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER ISCN			!SCAN TO READ
	BYTE STH_IN(0:*)		!SET HEADER
	LOGICAL HEAD			!FLAG SET IN SCAN HEADER?
	INTEGER*2 IFRT(0:STHIFR-1)	!IFR TABLE
	INTEGER FLG(0:STHIFR-1,0:3)	!FLAGS AND WEIGHTS
	REAL RDAT(0:STHIFR-1,0:3)	!CORRECTED DATA
C
C  Entry points:
C
C
C  Function references:
C
	REAL WNGEFD			!FRACTIONS TO DEGREES
	LOGICAL NMORDH			!Read model header
	LOGICAL NSCSCF			!Read corrected data
	LOGICAL NSCSIF			!READ IFR TABLE
	CHARACTER*32 WNTTSG		!PRINT SET NAME
C
C  Data declarations:
C
	INTEGER STP				!Source type of model
	DOUBLE PRECISION SRA,SDEC,SFRQ		!Model info
	REAL UV0(0:3)				!Basic uv coordinates
	REAL LM0(0:1)				!Basic source displacement
	DOUBLE PRECISION FRQ0			!Basic frequency
	REAL TF(0:1)				!Integr. time, bandwidth
	INTEGER MINST				!Instrument
C
	SAVE STP,SRA,SDEC,SFRQ,UV0,LM0,FRQ0,TF,MINST
C
	COMPLEX CV1,CV2				!Complex data,model
	REAL WGT(0:STHIFR-1,0:3)		!Weights
	REAL DAT(0:1,0:STHIFR-1,0:3)		!Data
	  COMPLEX CDAT(0:STHIFR-1,0:3)
	  EQUIVALENCE(DAT,CDAT)
	COMPLEX CMOD(0:3,0:STHIFR-1)		!Model IQUV
	COMPLEX CAMOD(0:STHIFR-1,0:3)		!Model XYX
C
	INTEGER*2 IFRX(0:STHIFR-1)		!IFR TABLE
	INTEGER IFRA(0:1,0:STHIFR-1)            ! IFR TABLE
	REAL ANG(0:2,0:STHIFR-1)                ! DIPOLE ANGLES
C
	BYTE STH(0:STH__L-1)			!SET HEADER
	  INTEGER STHJ(0:STH__L/LB_J-1)
	  INTEGER*2 STHI(0:STH__L/LB_I-1)
	  REAL STHE(0:STH__L/LB_E-1)
	  EQUIVALENCE (STH,STHI,STHJ,STHE)
C
	BYTE SCH(0:SCHHDL-1)			!SCAN HEADER
	  INTEGER SCHJ(0:SCHHDL/4-1)
	  INTEGER*2 SCHI(0:SCHHDL/2-1)
	  REAL SCHE(0:SCHHDL/4-1)
	  EQUIVALENCE (SCH,SCHJ,SCHI,SCHE)
C-
C
	NGICDT=.TRUE.				! Assume ok
C
C	Make local copy of sector header, initialise if first sector
C
	CALL WNGMV(STH__L,STH_IN,STH)		! Move bytes
	IF (ISCN.EQ.0) THEN
	   IF (.NOT.NSCSIF(FCAIN,STH,IFRX,IFRA,ANG)) THEN !READ IFR TABLE
	      CALL WNCTXT(F_TP,'Error reading interferometer table')
	      GOTO 990
	   ELSE IF (OPT.EQ.'DAT'.AND.NSRC(0).GT.0) THEN		!MODEL
	      IF (.NOT.NMORDH(6,STP,SRA,SDEC,SFRQ)) THEN	!NEXT SET
	         CALL WNCTXT(F_TP,'Error: cannot initialise model')
	         CALL WNCTXT(F_TP,
	1	    'You may need write access to the SCN-file')
	         GOTO 990
	      ELSE
	         CALL NMOMST(STP,SRA,SDEC,STH,LM0,FRQ0,TF,MINST) !GET DATA
	      END IF
	   END IF
	END IF
	DO I=0,STHIFR-1
	   IFRT(I)=IFRX(I)
	END DO
C
C	Read data and flags, get model
C
	IF (.NOT.NSCSCF(FCAIN,STH,IFRX,ISCN,
     &	           CORAP,CORDAP,SCH,WGT,CDAT,FLG)) THEN !Get corrected data
	   GOTO 980					!Error correcting
	ELSE IF (NSRC(0).GT.0) THEN
	   CALL NMOMUV(STP,SRA,SDEC,STH,SCH,UV0)	!Get model UV data
	   CALL NMOMU4(0,FCAIN,ISCN,STH,UV0,LM0,FRQ0,
     &	               STHE(STH_RTP_E),STHI(STH_PLN_I),
     &	               STHJ(STH_NIFR_J),IFRT,TF,MINST,CMOD) ! Model data
	   CALL NMOCIX(STH,SCH,ANG,CAMOD,CMOD)		!Convert
	END IF
C
C	Check if Flags set in header
C
	HEAD=(IAND(FL_ALL,SCHJ(SCH_BITS_J)).NE.0)
C
C	Convert to proper datatype
C
	DO I1=0,STHJ(STH_NIFR_J)-1			! SCAN ALL IFRs
	  DO I2=0,3
	    CV1=CDAT(I1,I2)				! DATA
	    CV2=0					! NO MODEL
	    IF (NSRC(0).GT.0) CV2=CAMOD(I1,I2)		! MODEL
	    IF (DATTYP(1:3).EQ.'COS') THEN
	      RDAT(I1,I2)=REAL(CV1-CV2)
	    ELSE IF (DATTYP(1:3).EQ.'SIN') THEN
	      RDAT(I1,I2)=AIMAG(CV1-CV2)
	    ELSE IF (DATTYP(1:3).EQ.'AMP') THEN
 	      CALL WNMAAM(1,CV1-CV2,RDAT(I1,I2))	!MAKE AMPL
	    ELSE IF (DATTYP(1:3).EQ.'PHA') THEN	!No model subtraction yet
 	      CALL WNMAPH(1,CV1-CV2,RDAT(I1,I2))	!MAKE PHASE
 	      RDAT(I1,I2)=WNGEFD(RDAT(I1,I2))		!MAKE DEGREES
	      IF (RDAT(I1,i2).GT.180) RDAT(I1,I2)=RDAT(I1,I2)-360.  !-180..180	
	    END IF
	  END DO
	END DO
C
	RETURN
C
  980	CONTINUE
	NGICDT=.FALSE.					!ERROR	
	CALL WNCTXT(F_TP,'Error in scan !SJ',ISCN)
	RETURN
C
  990	CONTINUE
	NGICDT=.FALSE.					!ERROR	
	CALL WNCTXT(F_TP,'Error in sector !AS',WNTTSG(SETNAM,0))
	RETURN
C
	END
