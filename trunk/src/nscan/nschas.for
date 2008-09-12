C+ NSCHAS.FOR
C  WNB 930825
C
C  Revisions:
C	JPH 940902	Remove NSCHA1 (incorrect and not used)
C	CMV 950206	Change action for * (use default, not +/- 180)
C
C
	LOGICAL FUNCTION NSCHAS(TYP,HA)
C
C  Select HA range to use/to do
C
C NOTE (JPH 940902)
C	If user specified a range with end < start, end is set to start. Since 
C  this is likely to be some kind of error, would it not be better to treat it 
C  as such? (But some users may now be relying on this as a "feature".)
C
C
C  Result:
C	NSCHAS_L = NSCHAS ( TYP_J:I, HA_E(0:1):IO)
C				Get HA range (in circles)
C				0 = use HA to prompt
C				1 = use * to prompt
C
C  Pin references:
C
C	HA_RANGE
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'CBITS_DEF'
	INCLUDE 'STH_O_DEF'		!SET HEADER
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER TYP			!SELECTION TYPE
	REAL HA(0:1)			!HA RANGE
C
C  Function references:
C
	REAL WNGENF			!NORM. ANGLE
	LOGICAL WNDPAR			!GET USER DATA
C
C  Data declarations:
C
	REAL LHA(0:1)			!LOCAL HA
C-
C   

C
C NSCHAS
C
	NSCHAS=.TRUE.				!ASSUME OK
	IF (TYP.EQ.0) THEN			!USE HA TO PROMPT
	  LHA(0)=WNGENF(HA(0))*360.
	  LHA(1)=WNGENF(HA(1))*360.
	  LHA(1)=MAX(LHA(1),LHA(0))		!CORRECT RANGE
	ELSE
	  LHA(0)=-179.99
	  LHA(1)=+179.99
	END IF
C
C GET USER DATA
C
 11	CONTINUE
	A_J(0)=1				! inhibit reset of dynamic
						!  prompt texts
	IF (LHA(0).LT.-179.9 .OR. LHA(1).GT.179.9) THEN
	  JS=WNDPAR('HA_RANGE',HA,2*LB_E,J0,'*')
	ELSE
	  JS=WNDPAR('HA_RANGE',HA,2*LB_E,J0,A_B(-A_OB),LHA,2)
	END IF
	IF (.NOT.JS) THEN
	  IF (E_C.EQ.DWC_ENDOFLOOP) THEN
	    NSCHAS=.FALSE.			!SHOW END
	    GOTO 20				!READY
	  END IF
	  GOTO 11				!REPEAT
	ELSE IF (J0.EQ.0) THEN
	  NSCHAS=.FALSE.			!SHOW END
	  GOTO 20				!READY
	ELSE IF (J0.LT.0) THEN			!ASSUME DEFAULT
	  IF (LHA(0).LT.-179.9 .OR. LHA(1).GT.179.9) THEN
	    HA(0)=-179.99
	    HA(1)=+179.99
          ELSE
	    HA(0)=LHA(0)
	    HA(1)=LHA(1)
	  END IF
	END IF
C
C ANALYSE
C
	HA(0)=WNGENF(HA(0)/360.)		!LIMIT RANGE
	HA(1)=WNGENF(HA(1)/360.)		!LIMIT RANGE
	HA(1)=MAX(HA(0),HA(1))
C
 20	CONTINUE
	CALL WNDPOHC
C
	RETURN
C
C
	END
