C+ WNCACU.FOR
C  WNB 91021
C
C  Revisions:
C
	LOGICAL FUNCTION WNCACU(TXT,PT,BAS,VAL,VC)
C
C  Convert a text to unsigned integer value
C
C  Result:
C
C	WNCACU_L = WNCACU( TXT_C*:I, PT_J:IO, BAS_J:I, VAL_D:O, VC_D:O)
C				Convert the string TXT
C				starting at PT to a value VAL. Interprete
C				the string as un unsigned integer of base
C				BAS (1, 2, ...., 16). VC will return the
C				BAS**(# of digits in value).
C				PT will be updated to beyond last character
C				read.
C				WNCACU will be .false. if no digit present.
C				Then: VAL=0, VC=1
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
	CHARACTER*(*) TXT		!INPUT STRING
	INTEGER PT			!STRING POINTER
	INTEGER BAS			!VALUE BASE
	DOUBLE PRECISION VAL		!OUTPUT VALUE
	DOUBLE PRECISION VC		!OUTPUT SCALE
C
C  Function references:
C
	CHARACTER*1 WNCAUP		!MAKE UC
C
C  Data declarations:
C
	INTEGER LT			!LENGTH OF STRING
	CHARACTER*16 SMOD		!DIGITS
	  DATA SMOD/'0123456789ABCDEF'/
	DOUBLE PRECISION MMOD(16)	!BASE
	  DATA MMOD/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16/
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
	WNCACU=.FALSE.					!ASSUME NO DIGIT
	J=MIN(16,MAX(1,BAS))				!LIMIT MODE
	VAL=0						!OUTPUT VALUE
	VC=1.						!OUTPUT SCALE
	LT=LEN(TXT)					!STRING LENGTH
	CALL WNCASB(TXT,PT)				!SKIP SPACES
C
C GET VALUE
C
	DO WHILE (PT.LE.LT)				!MORE CHAR
	  I1=INDEX(SMOD(:J),WNCAUP(TXT(PT:PT)))-1	!DIGIT
	  IF (I1.GE.0) THEN				!DIGIT
	    VAL=VAL*MMOD(J)+I1				!VALUE
	    VC=VC*MMOD(J)				!SCALE
	    PT=PT+1					!POINTER
	    WNCACU=.TRUE.				!DIGIT SEEN
	  ELSE
	    GOTO 10					!READY
	  END IF
	END DO
C
 10	CONTINUE
	RETURN
C
C
	END
