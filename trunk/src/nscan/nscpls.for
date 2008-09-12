C+ NSCPLS.FOR
C  WNB 930825
C
C  Revisions:
C	JPH 940909	Clear dynamic prompts
C
C
	LOGICAL FUNCTION NSCPLS(TYP,SPOL)
C
C  Select polarisations to use/to do
C
C  Result:
C	NSCPLS_L = NSCPLS ( TYP_J:I, SPOL_J:O)
C				Get which polarisations to use by setting
C				in SPOL bits 0,1,2,3 for XX,XY,YX,YY
C				TYP:
C				0 = use SPOL to prompt
C				1 = use XYX to prompt
C				2 = use XY to prompt
C				3 = use YX
C				11= use IQUV to prompt
C				12= use IQ to prompt
C				13= use UV
C	NSCPL1_L = NSCPL1 ( TYP_J:I, SPOL_J:O, STHJ_J(0:*):I)
C				As PLS
C	NSCPL2_L = NSCPL2 ( TYP_J:I, SPOL_J:O)
C				Asks XYX or Stokes
C
C  Pin references:
C
C	SELECT_XYX
C	SELECT_IQXY
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'CBITS_DEF'
	INCLUDE 'STH_O_DEF'		!SET HEADER
C
C  Entry points:
C
	LOGICAL NSCPL1, NSCPL2
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER TYP			!SELECTION TYPE
	INTEGER SPOL			!SELECTION POL. WORD
	INTEGER STHJ(0:*)		!SET HEADER
C
C  Function references:
C
	LOGICAL WNDPAR			!GET USER DATA
C
C  Data declarations:
C
	LOGICAL PL2			!PL2 ENTRY
	CHARACTER*4 POLC,POLD		!POL. CODE
C-
C
C NSCPLS
C
	PL2=.FALSE.				!NOT PL2
	GOTO 100
C
C NSCPL1
C
	ENTRY NSCPL1(TYP,SPOL,STHJ)
C
	PL2=.FALSE.				!NOT PL2
	GOTO 100
C
C NSCPL2
C
	ENTRY NSCPL2(TYP,SPOL)
C
	PL2=.TRUE.				!PL2
	GOTO 100
C
C INIT
C
 100	CONTINUE
	NSCPLS=.TRUE.				!ASSUME OK
	IF (TYP.EQ.0) THEN			!USE SPOL TO PROMPT
	  IF (.NOT.PL2) SPOL=IAND(SPOL,NOT(STOKES_P)) !NO STOKES ALLOWED
	  IF (IAND(SPOL,IQUV_M).EQ.IQUV_M) THEN
	    POLD='IQUV'
	  ELSE IF (IAND(SPOL,IQ_M).EQ.IQ_M) THEN
	    POLD='IQ'
	  ELSE IF (IAND(SPOL,UV_M).EQ.UV_M) THEN
	    POLD='UV'
	  ELSE IF (IAND(SPOL,Q_M).EQ.Q_M) THEN
	    POLD='Q'
	  ELSE IF (IAND(SPOL,U_M).EQ.U_M) THEN
	    POLD='U'
	  ELSE IF (IAND(SPOL,V_M).EQ.V_M) THEN
	    POLD='V'
	  ELSE IF (IAND(SPOL,STOKES_P).EQ.STOKES_P) THEN
	    POLD='I'
	  ELSE IF (IAND(SPOL,XYX_M).EQ.XYX_M) THEN
	    POLD='XYX'
	  ELSE IF (IAND(SPOL,XY_M).EQ.XY_M) THEN
	    POLD='XY'
	  ELSE IF (IAND(SPOL,YX_M).EQ.YX_M) THEN
	    POLD='YX'
	  ELSE IF (IAND(SPOL,Y_M).EQ.Y_M) THEN
	    POLD='Y'
	  ELSE IF (IAND(SPOL,YYX_M).EQ.YYX_M) THEN
	    POLD='YYX'
	  ELSE IF (IAND(SPOL,XXY_M).EQ.XXY_M) THEN
	    POLD='YYX'
	  ELSE
	    POLD='X'
	  END IF
	ELSE IF (PL2 .AND. TYP.EQ.12) THEN
	  POLD='IQ'
	ELSE IF (TYP.EQ.2 .OR. TYP.EQ.12) THEN
	  POLD='XY'
	ELSE IF (PL2 .AND. TYP.EQ.13) THEN
	  POLD='UV'
	ELSE IF (TYP.EQ.3 .OR. TYP.EQ.13) THEN
	  POLD='YX'
	ELSE IF (PL2) THEN
	  POLD='IQUV'
	ELSE
	  POLD='XYX'				!USE XYX
	END IF
C
C GET USER DATA
C
 11	CONTINUE
	A_J(0)=1				! inhibit clearing of
						!  dynamic prompt
	IF (PL2) THEN
	  JS=WNDPAR('SELECT_IQXY',POLC,LEN(POLC),J0,POLD) !GET INFO
	ELSE
	  JS=WNDPAR('SELECT_XYX',POLC,LEN(POLC),J0,POLD) !GET INFO
	END IF
	IF (.NOT.JS) THEN
	  IF (E_C.EQ.DWC_ENDOFLOOP) THEN
	    NSCPLS=.FALSE.			!SHOW END
	    GOTO 20				!READY
	  END IF
	  GOTO 11				!REPEAT
	ELSE IF (J0.EQ.0) THEN
	  NSCPLS=.FALSE.			!SHOW END
	  GOTO 20				!READY
	ELSE IF (J0.LT.0) THEN			!ASSUME DEFAULT
	  POLC=POLD
	END IF
C
C ANALYSE
C
	IF (POLC.EQ.'XYX') THEN			!SET CODE
	  SPOL=XYX_M				!ALL FOUR
	ELSE IF (POLC.EQ.'XY') THEN
	  SPOL=XY_M				!XX,YY
	ELSE IF (POLC.EQ.'YX') THEN
	  SPOL=YX_M				!XY,YX
	ELSE IF (POLC.EQ.'Y') THEN
	  SPOL=Y_M				!YY
	ELSE IF (POLC.EQ.'YYX') THEN
	  SPOL=YYX_M				!YX
	ELSE IF (POLC.EQ.'XXY') THEN
	  SPOL=XXY_M				!XY
	ELSE IF (POLC.EQ.'IQUV') THEN
	  SPOL=IQUV_M
	ELSE IF (POLC.EQ.'IQ') THEN
	  SPOL=IQ_M
	ELSE IF (POLC.EQ.'UV') THEN
	  SPOL=UV_M
	ELSE IF (POLC.EQ.'I') THEN
	  SPOL=I_M
	ELSE IF (POLC.EQ.'Q') THEN
	  SPOL=Q_M
	ELSE IF (POLC.EQ.'U') THEN
	  SPOL=U_M
	ELSE IF (POLC.EQ.'V') THEN
	  SPOL=V_M
	ELSE
	  SPOL=X_M				!XX
	END IF
C
 20	CONTINUE
	CALL WNDPOHC				! clear dynamic prompt
C
	RETURN
C
C
	END
