C+ NSCSAD.FOR
C  WNB 910208
C
C  Revisions:
C	WNB 910820	Add extinction, refraction Faraday
C	WNB 910913	Different (de-)application get
C	GvD 920429	Declare WNDPAR as logical iso. integer
C	WNB 921201	Add gain/phase for zero
C	WNB 921217	Typo PHAS'e'
C	HjV 921217	WNDPAR returns NOPHASE; so use again PHASE
C	WNB 930602	Add  CLK
C	JPH 930615	Symbolic names for mask bits in CBITS_O_DEF
C	WNB 930803	CBITS_DEF
C	CMV 940429	Use CAP_* bits throughout, extra argument in NSCSAZ
C       AXC 040130      APZ byte array to logical array
C
	SUBROUTINE NSCSAD(CAP,CDAP)
C
C  Get the corrections to Apply and/or De-apply on data
C
C  Result:
C		CALL NSCSAD ( CAP_J:O, CDAP_J:O)
C	Fill CAP and CDAP with bits (as defined by CBITS.DSC) indicating which
C	corrections should be (de-)applied to the data

C
C		CALL NSCSAZ ( CAP_J:O, APZ(0:1)_L:O)
C	Fill CAP with bits (as defined by CBITS.DSC) indicating which
C	corrections should be zeroed. APSOL indicates which of gain and
C	phase should be zeroed.
C
C
C  Pin references:
C
C	ZERO
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'CBITS_DEF'
C
C  Parameters:
C
	INTEGER TXTL			!LENGTH INPUT DATA
	  PARAMETER (TXTL=16)
	INTEGER MAXDEF			!# OF INPUTS
	  PARAMETER (MAXDEF=13)
	INTEGER MXNAPP			!KNOWN APPLIED
	  PARAMETER (MXNAPP=11)
C
C  Arguments:
C
	INTEGER CAP			!APPLY CORRECTIONS
	INTEGER CDAP			!DE-APPLY CORRECTIONS
	LOGICAL APZ(0:1)		!ZERO GAIN/PHASE
C
C  Function references:
C
	LOGICAL WNDPAR			!GET USER DATA
C
C  Data declarations:
C
	CHARACTER*(TXTL) TXT(MAXDEF)	!INPUT DATA
	CHARACTER*5 TAPP(MXNAPP)	!APPLIED
	  DATA TAPP/'RED', 'ALG', 'OTH',
	1	    'EXT', 'REF', 'IREF', 'FAR', 'CLK',
	1	    'IFR', 'MIFR','SHIFT'/
	INTEGER SAPP(MXNAPP)
	  DATA SAPP/
	1	CAP_RED, CAP_ALG, CAP_OTH, 
	1	CAP_XTN, CAP_REF, CAP_IRE, CAP_FAR, CAP_CLK, 
	1	CAP_AIFR, CAP_MIFR, CAP_SHF/
C-
C
C GET APPLY/DE-APPLY
C
	CALL WNDDAP(CAP,CDAP)			!GET CURRENT VALUES
C
	RETURN
C
C GET ZERO
C
	ENTRY NSCSAZ(CAP,APZ)
C
 40	CONTINUE
	CAP=0					!ZERO NOTHING
	APZ(0)=.TRUE.				!ZERO GAIN AND PHASE
	APZ(1)=.TRUE.
	IF (.NOT.WNDPAR('ZERO',TXT,MAXDEF*TXTL,J0,'NONE')) THEN !GET INFO
	  IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 41	!READY
	  GOTO 40				!REPEAT
	END IF
C
	IF (J0.EQ.0) THEN
	  CAP=0					!ASSUME NONE
	ELSE IF (J0.LT.0) THEN			!ALL
	  CAP=CAP_ALLMSK
	ELSE
	  CAP=0
	  DO I=1,J0				!ALL INPUTS
	    IF (TXT(I).EQ.'NONE') THEN
	      CAP=0
	    ELSE IF (TXT(I).EQ.'ALL') THEN
	      CAP=CAP_ALLMSK
	    ELSE IF (TXT(I).EQ.'NOGAIN') THEN
	      APZ(0)=.FALSE.
	    ELSE IF (TXT(I).EQ.'NOPHASE') THEN
	      APZ(1)=.FALSE.
	    ELSE
	      DO I1=1,MXNAPP
	        IF (TXT(I).EQ.TAPP(I1)) THEN	!FOUND
		  CAP=IOR(CAP,SAPP(I1))		!SET
	          APZ(0)=.TRUE.			!SET GAIN/PHASE
	          APZ(1)=.TRUE.
		ELSE IF (TXT(I).EQ.'NO'//TAPP(I1)) THEN
		  CAP=IAND(CAP,.NOT.SAPP(I1))
		END IF
	      END DO
	    END IF
	  END DO
	END IF
C
 41	CONTINUE
C
	RETURN
C
C
	END
