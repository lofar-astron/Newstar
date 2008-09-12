C+ WNCALO.FOR
C  HjV 931202      Splitted because of HP-UX 09.01 problem 
C                       with character entry
C
C  Revisions:
C
	CHARACTER*(*) FUNCTION WNCALO(TXT)
C
C  Convert character to lower case
C
C  Result:
C
C	C1 = WNCALO ( TXT_C*:I)	Convert first character in TXT to lowercase
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
	CHARACTER*(*) TXT			!INPUT STRING
C
C  Entry points:
C
C
C  Function references:
C
C
C  Data declarations:
C
	CHARACTER*26 LC				!LC TABLE
	CHARACTER*26 UC				!UC TABLE
	  DATA LC/'abcdefghijklmnopqrstuvwxyz'/
	  DATA UC/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
C
C  Equivalences:
C
C
C  Commons:
C
C-
	J=INDEX(UC,TXT(1:1))
	IF (J.NE.0) THEN
	  WNCALO=LC(J:J)
	ELSE
	  WNCALO=TXT(1:1)
	END IF
C
	RETURN
C
C
	END
