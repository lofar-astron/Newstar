C+ WNMEJC.FOR
C  WNB 910305
C
C  Revisions:
C
	INTEGER FUNCTION WNMEJC(EVAL)
C
C  Get CEIL or FLOOR
C
C  Result:
C
C	WNMEJC_J = WNMEJC( EVAL_E:I)		Get CEIL(EVAL)
C	WNMEEC_E = WNMEEC( EVAL_E:I)		Get CEIL(EVAL)
C	WNMDJC_J = WNMDJC( DVAL_E:I)		Get CEIL(DVAL)
C	WNMDDC_D = WNMDDC( DVAL_E:I)		Get CEIL(DVAL)
C
C	WNMEJF_J = WNMEJF( EVAL_E:I)		Get FLOOR(EVAL)
C	WNMEEF_E = WNMEEF( EVAL_E:I)		Get FLOOR(EVAL)
C	WNMDJF_J = WNMDJF( DVAL_E:I)		Get FLOOR(DVAL)
C	WNMDDF_D = WNMDDF( DVAL_E:I)		Get FLOOR(DVAL)
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
	REAL EVAL				!INPUT VALUE
	DOUBLE PRECISION DVAL
C
C  Entry points:
C
	INTEGER WNMDJC,WNMEJF,WNMDJF
	REAL WNMEEC,WNMEEF
	DOUBLE PRECISION WNMDDC,WNMDDF
C
C  Function references:
C
C
C  Data declarations:
C
C-
C
C WNMEJC
C
	WNMEJC=INT(EVAL)
	IF (MOD(EVAL,1E0).NE.0) THEN
	  IF (EVAL.GT.0) WNMEJC=WNMEJC+1
	END IF
C
	RETURN
C
C WNMEEC
C
	ENTRY WNMEEC(EVAL)
C
	WNMEEC=AINT(EVAL)
	IF (MOD(EVAL,1E0).NE.0) THEN
	  IF (EVAL.GT.0) WNMEEC=WNMEEC+1
	END IF
C
	RETURN
C
C WNMDJC
C
	ENTRY WNMDJC(DVAL)
C
	WNMDJC=INT(DVAL)
	IF (MOD(DVAL,1D0).NE.0) THEN
	  IF (DVAL.GT.0) WNMDJC=WNMDJC+1
	END IF
C
	RETURN
C
C WNMDDC
C
	ENTRY WNMDDC(DVAL)
C
	WNMDDC=AINT(DVAL)
	IF (MOD(DVAL,1D0).NE.0) THEN
	  IF (DVAL.GT.0) WNMDDC=WNMDDC+1
	END IF
C
	RETURN
C
C WNMEJF
C
	ENTRY WNMEJF(EVAL)
C
	WNMEJF=INT(EVAL)
	IF (MOD(EVAL,1E0).NE.0) THEN
	  IF (EVAL.LT.0) WNMEJF=WNMEJF-1
	END IF
C
	RETURN
C
C WNMEEF
C
	ENTRY WNMEEF(EVAL)
C
	WNMEEF=AINT(EVAL)
	IF (MOD(EVAL,1E0).NE.0) THEN
	  IF (EVAL.LT.0) WNMEEF=WNMEEF-1
	END IF
C
	RETURN
C
C WNMDJF
C
	ENTRY WNMDJF(DVAL)
C
	WNMDJF=INT(DVAL)
	IF (MOD(DVAL,1D0).NE.0) THEN
	  IF (DVAL.LT.0) WNMDJF=WNMDJF-1
	END IF
C
	RETURN
C
C WNMDDF
C
	ENTRY WNMDDF(DVAL)
C
	WNMDDF=AINT(DVAL)
	IF (MOD(DVAL,1D0).NE.0) THEN
	  IF (DVAL.LT.0) WNMDDF=WNMDDF-1
	END IF
C
	RETURN
C
C
	END