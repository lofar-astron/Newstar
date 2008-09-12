C+ WNCFHD_X.FOR
C  WNB 890417
C
C  Revisions:
C
	SUBROUTINE WNCFHD_X(COD,N,TXT)
C
C  Set/reset header lines
C
C  Result:
C
C	CALL WNCFHD_X ( COD_J:I, N_J:I, TXT_C*:I)
C					Set (N>0) or reset (<0) header line N,
C					for files specified by COD, using TXT.
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'WNC_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER COD				!FILE CODE
	INTEGER N				!LINE NUMBER
	CHARACTER*(*) TXT			!HEADER LINE TEXT
C
C  Function references:
C
C
C  Data declarations:
C
C
C  Equivalences:
C
C
C  Commons:
C
C-
	IF (N.EQ.0 .OR. ABS(N).GT.CMPH) RETURN	!UNKNOWN HEADER LINE
	J=COD					!COPY CODE
	I=-1					!FILE COUNT
	DO WHILE (IAND(J,F_ALL).NE.0)		!ALL FILES
	  IF (I.EQ.-1) THEN			!SELECT CODE
	    J1=F_T
	  ELSE IF (I.EQ.0) THEN
	    J1=F_P
	  ELSE
	    J1=ISHFT(F_0,I-1)
	  END IF
	  IF (IAND(J,J1).NE.0) THEN		!TO DO
	    IF (N.GT.0) THEN			!SET HEADER LINE
	      CPH(N,I)=TXT			!SET LINE
	      CSPH(N,I)='1'			!SET SET
	      CHPH(I)=MAX(N,CHPH(I))		!HIGHEST LINE SET
	    ELSE				!RESET
	      CPH(-N,I)=' '			!EMPTY LINE
	      CSPH(-N,I)='0'			!SET NOT SET
	      IF (-N.EQ.CHPH(I)) THEN		!FIND NEW HIGHEST
		DO I1=-N,1,-1
		  IF (CSPH(I1,I).EQ.'0') THEN
		    CHPH(I)=I1-1		!NEXT HIGHEST
		  ELSE
		    GOTO 10
		  END IF
		END DO
 10		CONTINUE
	      END IF
	    END IF
	  END IF
C
	  J=IAND(J,NOT(J1))			!DELETE BIT
	  I=I+1					!COUNT
	END DO
C
	RETURN
C
C
	END
