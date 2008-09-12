C+ NATXIB.FOR
C  WNB 920506
C
C  Revisions:
C
	LOGICAL FUNCTION NATXIB(BASEL,IFNO,UT,U,V,W)
C
C  Check for illegal baseline (based on ILLBASE)
C
C  Result:
C
C	NATXIB_L = NATXIB( BASEL_E:I, IFNO_J:I, UT_E:I, U_E:I, V_E:I, W_E:I)
C				Test for illegal BASEL and IFNO
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'RPF_DEF'	!RPFITS DATA AREA
C
C  Parameters:
C
	REAL BMAX,BMIN		!MAX, MIN BASEL ALLOWED
	  PARAMETER (BMAX=256*6+6,
	1		BMIN=256+1)
C
C  Arguments:
C
	REAL BASEL		!BASELINE TO CHECK
	INTEGER IFNO		!IF # TO CHECK
	REAL UT			!TIME TO CHECK
	REAL U,V,W		!BASELINE TO CHECK
C
C  Entry points:
C
C
C  Function references:
C
C
C  Data declarations:
C
	LOGICAL SYSC
C-
C
C Deal with floating point baseline first
C
	NATXIB=.FALSE.				!ASSUME LEGAL
	SYSC=(BASEL.GT.-1.0001 .AND. BASEL.LT.-0.9999)
	IF (.NOT.SYSC) THEN
	  IF (BASEL.GT.BMAX .OR. BASEL.LT.BMIN) THEN
	    NATXIB=.TRUE.
C
	    RETURN
	  ELSE IF (ABS(BASEL-NINT(BASEL)).GT.0.001) THEN
C
C This value is not close enough to an integer to be valid
C
	    NATXIB=.TRUE.
C
	    RETURN
	  END IF
	END IF
C
C Now check the rest
C
	NATXIB=(
	1	(UT.LT.0. .OR. UT.GT.172800.) .OR.
	1	(.NOT.SYSC .AND. 
	1		((IFNO.LT.0 .OR. IFNO.GT.MAX_IF) .OR.
	1		(U.LT.-1.E10 .OR. U.GT. 1.E10)  .OR.
	1		(V.LT.-1.E10 .OR. V.GT. 1.E10)  .OR.
	1		(W.LT.-7.E6  .OR. W.GT. 7.E6))) .OR.
	1	(SYSC .AND.
	1		((SC_ANT.LT.1 .OR. SC_ANT.GT.ANT_MAX) .OR.
	1		(SC_IF.LT.1  .OR. SC_IF.GT.MAX_IF) .OR.
	1		(SC_Q.LT.1   .OR. SC_Q.GT.100))))
C
	RETURN
C
C
	END
