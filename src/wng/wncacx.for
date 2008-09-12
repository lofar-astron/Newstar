C+ WNCACX.FOR
C  WNB 910211
C
C  Revisions:
C
	LOGICAL FUNCTION WNCACX(TXT,PT,BAS,XVAL)
C
C  Convert a text to a complex value
C
C  Result:
C
C	WNCACX_L = WNCACXD( TXT_C*:I, PT_J:IO, BAS_J:I, XVAL_D:O)
C				Convert the string TXT starting at
C				PT to a complex value VAL. Interprete
C				the string as a value of base
C				BAS (1, 2, ...., 16).
C				PT will be updated to beyond last character
C				read.
C				Possible numbers:
C				[number][+|-numberI]
C				WNCACX will be .false. if no digit present.
C				Then: VAL=0
C	WNCACY_L = WNCACY( TXT_C*:I, PT_J:IO, BAS_J:I, YVAL_E:O)
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
C
C  Parameters:
C
C
C  Entry points:
C
	LOGICAL WNCACY
C
C  Arguments:
C
	CHARACTER*(*) TXT		!INPUT STRING
	INTEGER PT			!STRING POINTER
	INTEGER BAS			!VALUE BASE
	COMPLEX XVAL			!OUTPUT VALUE
	DOUBLE COMPLEX YVAL
C
C  Function references:
C
	LOGICAL WNCASM			!SKIP CHARACTER
	LOGICAL WNCACD			!GET REAL VALUE
C
C  Data declarations:
C
	INTEGER TP			!VALUE TYPE
	DOUBLE COMPLEX VAL		!VALUE
C
C  Equivalences:
C
C
C  Commons:
C
C-
C
C INIT
C
	WNCACX=.FALSE.					!ASSUME NO DIGIT
	TP=1						!X
	GOTO 100
C
C WNCACY
C
	ENTRY WNCACY(TXT,PT,BAS,YVAL)
C
	WNCACY=.FALSE.
	TP=2						!Y
	GOTO 100
C
C INIT
C
 100	CONTINUE
	VAL=0						!OUTPUT VALUE
C
C GET VALUE
C
	IF (.NOT.WNCACD(TXT,PT,BAS,D0)) GOTO 10		!NO DIGITS
	WNCACX=.TRUE.					!DIGIT SEEN
	IF (WNCASM(TXT,PT,'iI')) THEN			!NO REAL PART
	  VAL=CMPLX(0D0,D0)
	ELSE
	  VAL=CMPLX(D0,0D0)				!REAL PART
	  CALL WNCASB(TXT,PT)				!SKIP SPACES
	  J1=PT						!SAVE POINTER
	  IF (.NOT.WNCACD(TXT,PT,BAS,D0)) THEN
	    PT=J1
	  ELSE
	    IF (WNCASM(TXT,PT,'iI')) THEN		!IMAG. PART
	      VAL=VAL+CMPLX(0D0,D0)
	    ELSE
	      PT=J1
	    END IF
	  END IF
	END IF
C
C OUTPUT
C
 10	CONTINUE
	IF (TP.EQ.1) THEN				!X
	  XVAL=VAL
	ELSE IF (TP.EQ.2) THEN				!Y
	  YVAL=VAL
	END IF
C
	RETURN
C
C
	END
