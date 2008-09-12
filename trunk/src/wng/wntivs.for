C+ WNTIVS.FOR
C  WNB 930501
C
C  Revisions:
C
	LOGICAL FUNCTION WNTIVS(VAL,NAM,GI)
C
C  Set an integer value to variable
C
C  Result:
C
C	WNTIVS_L = WNTIVS( VAL_J:I, NAM_C*:I, GI_L:I)
C				Put value from STR in NAM. GI is the global
C				indicator
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'WNT_O_DEF'
	INCLUDE 'WNT_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER VAL			!INTEGER VALUE
	CHARACTER*(*) NAM		!NAME OF VALUE
	LOGICAL GI			!GLOBAL INDICATOR
C
C  Function references:
C
	INTEGER WNTIBP,WNTIBW		!SAVE BUFFER LINE
C
C  Data declarations:
C
	CHARACTER*(WNTV_STR_N) LVAL	!VALUE
	BYTE VALB(0:WNTVHDL-1)		!FULL VALUE
	  INTEGER VALJ(0:WNTVHDL/LB_J-1)
	  EQUIVALENCE (VALB,VALJ)
C-
	WNTIVS=.TRUE.				!ASSUME OK
	CALL WNGMFS(WNTV_NAM_N,NAM,VALB(WNTV_NAM_1)) !SAVE NAME
	IF (GI) THEN				!GLOBAL
	  VALJ(WNTV_TYP_J)=+2
	ELSE					!LOCAL
	  VALJ(WNTV_TYP_J)=+1
	END IF
	VALJ(WNTV_VAL_J)=VAL			!VALUE
	CALL WNCTXS(LVAL,'!SJ',VAL)		!MAKE STRING
	CALL WNGMFS(WNTV_STR_N,LVAL,VALB(WNTV_STR_1))
C
C SET IN LIST
C
	DO I=0,VBDES_J(WNTB_CNT_J)-1		!CHECK EXISTING NAMES
	  CALL WNGMTS(WNTV_NAM_N,
	1		A_B(VBDES_J(WNTB_BPTR_J)+I*WNTVHDL+WNTV_NAM_1),
	1		LVAL)			!READ NAME
	  IF (LVAL.EQ.NAM) THEN			!EXISTING
	    I1=WNTIBW(VBDES,VALB,I)		!OVERWRITE VALUE
	    GOTO 800				!READY
	  END IF
	END DO
	I1=WNTIBP(VBDES,VALB)			!SET VALUE
C
C READY
C
 800	CONTINUE
C
	RETURN
C
C
	END
