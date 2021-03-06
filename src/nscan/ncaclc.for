C+ NCACLC.FOR
C  WNB 910814
C
C  Revisions:
C	WNB 921104	Full HA range
C	WNB 921217	Add CL1
C	HjV 930311	Change some text
C	WNB 930825	Add dipole positions
C	WNB 931213	Add code for possible phase ambiguities
C	CMV 940513	Option to give all sets equal weight
C	JPH 940913	Preempt singularity in logarithmic conversion
C			Comments
C       WNB 950611	Change least squares calls
C
C
	SUBROUTINE NCACLC(CSOL,CME)
C
C  Calculate average corrections in scan file.
C 

C
C  Result:
C
C	CALL NCACLC( CSOL_X(0:*,0:1):O, CME_X(0:*,0:1):O)
C		will calculate average log. corrections in scans
C	CALL NCACL1( CSOL_X(0:*,0:1):O, CME_X(0:*,0:1):O,
C			NSTHPI_J:I, STHPI_J(0:*):I)
C		will calculate average corrections for NSTHPI scans
C		whose file addresses are in STHPI	
C
C  The method is to first average sine and cosine values to get an initial
C  guess for the phase. Then, using this guess. the gains and phases are 
C  averaged.
C  This guarantees phase continuity except in pathological cases where the
C  averages would be meaningless anyway.
C
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'NCA_DEF'
	INCLUDE 'LSQ_O_DEF'
	INCLUDE 'STH_O_DEF'			!SET HEADER
	INCLUDE 'SCH_O_DEF'			!SCAN HEADER
C
C  Parameters:
C
C
C  Arguments:
C
	REAL CSOL(0:1,0:STHTEL-1,0:1)		!AVERAGE ERRORS G,P TEL X,Y
	REAL CME(0:1,0:STHTEL-1,0:1)		!AND M.E. (VARIANCES)
	INTEGER NSTHPI				!# OF INPUT SETS
	INTEGER STHPI(0:*)			!INPUT SETS
C
C  Function references:
C
	CHARACTER*32 WNTTSG			!SET NAME
	LOGICAL WNMLGA				!INIT. LSQ
	LOGICAL WNFRD				!READ DATA
	REAL WNGENR				!NORM. ANGLE
	LOGICAL NSCSTL				!GET A SET
	LOGICAL NSCSCT				!GET SCAN ERRORS
	LOGICAL NSCSIF				!GET INTERFEROMETER TABLE
C
C  Data declarations:
C
	LOGICAL DOCL1				!CL1/CLC SWITCH
	LOGICAL GUESS				!GUESS LOOP
	INTEGER CSTHI				!STH COUNT
	INTEGER MINSCN				!MINIMUM NUMBER OF SCANS
	INTEGER CNTSCN				!COUNT NUMBER OF SCANS
	INTEGER MAR				!LSQ AREA
	INTEGER SETNAM(0:7)			!FULL SET NAME
	REAL HA					!HA OF SCAN
	INTEGER*2 IFRT(0:STHIFR-1)		!IFR TABLES
	INTEGER IFRA(0:1,0:STHIFR-1)
	REAL ANG(0:2,0:STHIFR-1)
	REAL TCOR(0:1,0:STHTEL-1,0:1)		!ERRORS READ G,P TEL X,Y
	  COMPLEX CCOR(0:STHTEL-1,0:1)
	  EQUIVALENCE (TCOR,CCOR)
	REAL TMU				!M.E. READ DATA
	REAL CMU(0:1,0:STHTEL-1,0:1)		!M.E. PER WEIGHT
	REAL CMUD(0:1,0:STHTEL-1,0:1)		!M.E.
	INTEGER STHP				!POINTER TO SET HEADER
	BYTE STH(0:STHHDL-1)			!SET HEADER
	  INTEGER*2 STHI(0:STHHDL/2-1)
	  INTEGER STHJ(0:STHHDL/4-1)
	  REAL STHE(0:STHHDL/4-1)
	  DOUBLE PRECISION STHD(0:STHHDL/8-1)
	  EQUIVALENCE(STH,STHI,STHJ,STHE,STHD)
	BYTE SCH(0:SCHHDL-1)			!SCAN HEADER
	  INTEGER*2 SCHI(0:SCHHDL/2-1)
	  INTEGER SCHJ(0:SCHHDL/4-1)
	  REAL SCHE(0:SCHHDL/4-1)
	  DOUBLE PRECISION SCHD(0:SCHHDL/8-1)
	  EQUIVALENCE(SCH,SCHI,SCHJ,SCHE,SCHD)
C-
C
C NCACLC
C
	DOCL1=.FALSE.
	GOTO 40
C
C NCACL1
C
	ENTRY NCACL1(CSOL,CME,NSTHPI,STHPI)
C
	DOCL1=.TRUE.
	GOTO 40
C
C INIT
C
 40	CONTINUE
	GUESS=.FALSE.					!GUESS LOOP
	CORDAP=0					!NOTHING DE-APPLIED
	CORAP=-1					!ALL CORRECTIONS
	CORZE=0						!NO ZEROING
	MINSCN=-1					!NO LIMIT ON SCANS
C
C GUESS LOOP. Entry is with .NOT.GUESS, so the first time the loop at label 
C  20 is executed with GUESS true. When all sectors are done, control returns 
C  through label 50 to label 10. The loop at label 20 is then re-executed with
C  GUESS false and finally exits through label 50.
C
 10	CONTINUE
	GUESS=.NOT.GUESS				!SWAP GUESSING
	CSTHI=0						!STH COUNT
	IF (.NOT.WNMLGA(MAR,LSQ_T_REAL+LSQ_T_MULTIPLE,1,2*2*STHTEL)) THEN
							!GET LSQ AREA
	  CALL WNCTXT(F_TP,'Cannot obtain LSQ area')
	  CALL WNGEX					!STOP
	END IF
C
C DO SETS. This section contains two modes of loop control: Standard sector
C  search through NSCSTL for NCACLC; and direct reading of sector header
C  specified by file address for NCACL1. The same variables are set in both
C  modes, but in the latter the set name is necessarily an absolute number
C  rather than an index.
C
 20	CONTINUE
	IF (.NOT.DOCL1) THEN				!LOOP THROUGH SETS
	  IF (.NOT.NSCSTL(FCAINP,SETINP,STH(0),STHP,SETNAM,LPOFF))
	1		GOTO 50				!NO MORE SETS
	ELSE
	  STHP=STHPI(CSTHI)				!STH
	  CSTHI=CSTHI+1					!COUNT SETS
	  IF (CSTHI.GT.NSTHPI) GOTO 50			!NO MORE SETS GIVEN
	  IF (STHP.EQ.0) GOTO 20			!SKIP THIS ONE
	  IF (.NOT.WNFRD(FCAINP,STHHDL,STH(0),STHP)) THEN !READ SET HEADER
	    CALL WNCTXT(F_TP,'Error reading sector header')
	    GOTO 20					!NEXT SET
	  END IF
	  SETNAM(0)=STHJ(STH_SETN_J)			!PREPARE SET NAME
	  SETNAM(1)=-2					!INDICATE #
	END IF
C
C GET IFR TABLES
C
	IF (.NOT.NSCSIF(FCAINP,STH,IFRT,IFRA,ANG)) THEN
	  CALL WNDSTI(FCAINP,SETNAM)			!MAKE PROPER SET NAME
	  CALL WNCTXT(F_TP,'Error reading interferometer tables '//
	1		'sector(s) !AS',WNTTSG(SETNAM,0))
	  GOTO 20					!NEXT SET
	END IF
C
C DO SCANS
C
	CNTSCN=0					!NO SCANS YET
	DO I=0,STHJ(STH_SCN_J)-1			!ALL SCANS
C
C MAKE SETS OF EQUAL LENGTH
C
	   IF (.NOT.GUESS.AND.CEQUAL.GT.0.AND.
	1	CNTSCN.GE.MINSCN) GOTO 31		!DONE FOR THIS SECTOR
C
C GET SCAN ERRORS
C
	  IF (.NOT.NSCSCT(FCAINP,STH,IFRT,I,CORAP,CORDAP,
	1			SCH,CCOR,TMU)) THEN
	    HA=STHE(STH_HAB_E)+I*STHE(STH_HAI_E)
	    CALL WNCTXT(F_TP,'!7$EAF7.2 Error reading scan data',HA)
	    GOTO 31					!TRY NEXT SET
	  END IF
	  IF (TMU.GE.0) THEN				!NOT DELETED
	    CNTSCN=CNTSCN+1				!COUNT SCAN
	    IF (GUESS) THEN				!GUESS
	      DO I1=0,1					!X,Y
		DO I2=0,STHTEL-1			!TELESCOPES
		  CCOR(I2,I1)=EXP(CCOR(I2,I1))		!MAKE COS/SIN GAINS
		END DO
	      END DO
	    ELSE					!NOT GUESS
	      DO I1=0,1					!X,Y
	        DO I2=0,STHTEL-1			!TEL
		  TCOR(1,I2,I1)=WNGENR(TCOR(1,I2,I1)-CSOL(1,I2,I1))+
	1			CSOL(1,I2,I1)		!+- 180 DEG FROM GUESS
		END DO
	      END DO
	    END IF
	    CALL WNMLMN(MAR,0,1.,1.,TCOR)		!MAKE SUMS
	  END IF
C
C NEXT SCAN
C
 30	  CONTINUE
	END DO						!END SCANS
C
C NEXT SEctor
C
 31	CONTINUE
	IF (GUESS) THEN
	   IF (MINSCN.LT.0.OR.
	1	(CNTSCN.GT.0.AND.CNTSCN.LT.MINSCN)) THEN !FIRST OR SMALLER
	      MINSCN=CNTSCN				 !NEW MINIMUM
	   END IF
	ELSE
	   CALL WNDSTI(FCAINP,SETNAM)			!MAKE PROPER SET NAME
	   CALL WNCTXT(F_TP,'Input sector !AS\:!40C!UJ scans used',
	1		WNTTSG(SETNAM,0),CNTSCN)	!SHOW NUMBER USED
	END IF
	GOTO 20
C
C CALCULATE RESULT
C Note: The weird way of calculating an ATAN2 must be for portability reasons
C
 50	CONTINUE
	CALL WNMLTN(MAR)				!DE-COMPOSE
	CALL WNMLSN(MAR,CSOL,CMU,CMUD)			!GET SOLUTION
	CALL WNMLME(MAR,CME)				!VARIANCES
	CALL WNMLFA(MAR)				!CLEAR LSQ AREA
	IF (GUESS) THEN					!STILL GUESSING
	  DO I1=0,1					!X,Y
	    DO I2=0,STHTEL-1				!TEL.
	      IF (CSOL(0,I2,I1).NE.0 .AND. CSOL(1,I2,I1).NE.0)
	1		CSOL(1,I2,I1)=AIMAG(LOG(CMPLX(
	1		CSOL(0,I2,I1),CSOL(1,I2,I1))))	!GET GUESSED ANGLE
	    END DO
	  END DO
	  GOTO 10 					! end of main loop
	END IF
C
	RETURN						!READY
C
C
	END
