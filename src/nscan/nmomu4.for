C+ NMOMU4.FOR
C  WNB 900903
C
C  Revisions:
C	WNB 910319	Add NMOMU4
C	WNB 930819	Remove NMOMUM, rename to NMOMU4, remove L_
C	WNB 931006	Text
C	WNB 931008	Add MINST
C
	SUBROUTINE NMOMU4(STP,FCA,SCN,STHJ,UV0,LM0,FRQ0,RTP,NPOL,
	1		NIFR,IFR,TF,MINST,CMOD)
C
C  Calculate model fringes
C
C  Result:
C
C	CALL NMOMU4( STP_J:I, FCA_J:I, SCN_J:I, STHJ_J(0:*):I,
C			UV0_E(0:3):I, LM0_E(0:1):I, FRQ0_D:I,
C			RTP_E(0:*), NPOL_J:I, NIFR_J:I, IFR_I(0:*):I,
C			TF_E(0:1):I, MINST_J:I, CMOD_X(0:3,0:*):O)
C				Calculate model fringes in CMOD for all
C				specified NIFR's as detailed in interferometer
C				list IFR for a model with type STP for scan
C				number SCN with set header STHJ. Always 4
C				polarisations produced.
C				RTP are the telescope positions (m),
C				LM0 the l,m offsets (rad),
C				FRQ0 the the frequency of the model (MHz),
C				TF the integration time (fractions) and
C				the bandwidth (MHz), MINST instrument
C				and UV0 the UV coordinates for a baseline
C				of 1 m (resp. cos(t), -sin(t).sin(dec),
C				cos(t).sin(dec), -sin(t)) in wavelengths*2*pi.
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'NMO_DEF'
	INCLUDE 'STH_O_DEF'		!SET HEADER
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER STP			!SOURCE TYPE
	INTEGER FCA			!SCAN FILE POINTER
	INTEGER SCN			!# OF SCAN
	INTEGER STHJ(0:*)		!SET HEADER
	REAL UV0(0:3)			!UV COORDINATES
	REAL LM0(0:1)			!LM OFFSETS
	DOUBLE PRECISION FRQ0		!FREQUENCY
	REAL RTP(0:*)			!TEL. POSITIONS
	INTEGER NPOL			!# POL.
	INTEGER NIFR			!# OF INTERFEROMETERS
	INTEGER*2 IFR(0:*)		!INTERFEROMETERS
	REAL TF(0:1)			!INTEGR. TIME, BANDWIDTH
	INTEGER MINST			!INSTRUMENT
	COMPLEX CMOD(0:3,0:*)		!CALCULATED MODEL (I,Q,U,V)
C
C  Function references:
C
	LOGICAL WNFRD			!READ FILE
C
C  Data declarations:
C
C-
C
C MUST CALCULATE
C
	IF (STP.EQ.1) THEN
	  CALL NMOMUC(1,UV0,LM0,FRQ0,RTP,NPOL,NIFR,IFR,
	1		TF,MINST,CMOD)			!CALCULATE
	ELSE IF (STP.GT.1 .OR. STP.LT.0) THEN
	  CALL NMOMUC(1,UV0,LM0,FRQ0,RTP,NPOL,NIFR,IFR,
	1		TF,MINST,CMOD)			!CALCULATE
	ELSE IF(IAND(MODACT,NMO_SAV).NE.0) THEN		!ALL SAVED
	  IF (STHJ(STH_MDD_J).NE.0) THEN
	    IF (.NOT.WNFRD(FCA,4*LB_X*NIFR,CMOD,
	1		STHJ(STH_MDD_J)+SCN*4*LB_X*NIFR)) THEN !READ
 10	      CONTINUE
	      CALL WNCTXT(F_TP,'!/Error reading model data!/')
	      CALL WNGEX				!ERROR OUT
	    END IF
	  ELSE
	    GOTO 10					!ERROR
	  END IF
	ELSE IF(IAND(MODACT,NMO_USE).NE.0) THEN		!USE SOME
	  IF (STHJ(STH_MDD_J).NE.0) THEN
	    IF (.NOT.WNFRD(FCA,4*LB_X*NIFR,CMOD,
	1		STHJ(STH_MDD_J)+SCN*4*LB_X*NIFR)) THEN !READ
	      GOTO 10
	    END IF
	  ELSE
	    CALL WNGMVZ(4*LB_X*NIFR,CMOD)		!CLEAR MODEL
	  END IF
	  IF (IAND(MODACT,NMO_MER).NE.0) THEN		!MERGE
	    CALL NMOMUA(6,UV0,LM0,FRQ0,RTP,NPOL,NIFR,IFR,
	1		TF,MINST,CMOD)			!REST
	  ELSE						!ADD
	    CALL NMOMUA(0,UV0,LM0,FRQ0,RTP,NPOL,NIFR,IFR,
	1		TF,MINST,CMOD)			!REST
	  END IF
	ELSE						!NO USE
	  CALL NMOMUC(0,UV0,LM0,FRQ0,RTP,NPOL,NIFR,IFR,
	1		TF,MINST,CMOD)			!CALCULATE ALL
	END IF
C
	RETURN
C
C
	END
