C+ NATXSF.FOR
C  WNB 920429
C
C  Revisions:
C
	INTEGER FUNCTION NATXSF(FCA,FCAPT,BUFFER)
C
C  Test for header or FG table (based on SIMPLE)
C
C  Result:
C
C	NATXSF_J = NATXSF( FCA_J:I, FCAPT_J:IO, BUFFER_E(20,32):IO)
C				Test for start new header or FG table. Return
C				0 (no), 1 (yes), 4 (FG)
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
	INTEGER FCA		!FILE CONTROL AREA
	INTEGER FCAPT		!FILE POINTER
	REAL BUFFER(20,32)	!CARD BUFFER
C
C  Entry points:
C
C
C  Function references:
C
C
C  Data declarations:
C
	LOGICAL ENDHDR		!END-OF-HEADER SEEN
	CHARACTER*80 M(32)	!BUFFER
C-
C
C NATXSF
C
C Assume not found
C
	NATXSF=0
C
C Write first 8 characters from buffer into character string m
C
	CALL WNGMTS(8,BUFFER(1,1),M(1))
	IF (M(1)(1:6).EQ.'SIMPLE') THEN
C
C Start of header.
C
	  NATXSF=1
	ELSE IF (M(1)(1:8).EQ.'FG TABLE') THEN
C
C Start of FG (flag) table.
C
	  ENDHDR=.FALSE.
	  DO I=1,32				!SET CHAR. BUFFER
	    CALL WNGMTS(80,BUFFER(1,I),M(I))
	  END DO
	  CALL NATRRT(FCA,FCAPT,M,-1,ENDHDR)
	  NATXSF=4
	END IF
C
	RETURN
C
C
	END
