C+ WNCAUC.FOR
C  WNB 890105
C
C  Revisions:
C	WNB 911115	DW DATA for CHARACTER problem
C
	SUBROUTINE WNCAUC(TXT)
C
C  Convert string to single case
C
C  Result:
C
C	CALL WNCAUC ( TXT_C*:IO)	Convert TXT to Uppercase
C	CALL WNCALC ( TXT_C*:IO)	Convert TXT to lowercase
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
C  Function references:
C
C
C  Data declarations:
C
	CHARACTER*26 LC				!LC TABLE
	CHARACTER*26 UC				!UC TABLE
	  DATA LC/'abcdefghijklmnopqrstuvwxyz'/
	  DATA UC/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
C-
	DO I=1,LEN(TXT)
	  J=INDEX(LC,TXT(I:I))
	  IF (J.NE.0) TXT(I:I)=UC(J:J)
	END DO
C
	RETURN
C
C To lowercase
C
	ENTRY WNCALC(TXT)
C
	DO I=1,LEN(TXT)
	  J=INDEX(UC,TXT(I:I))
	  IF (J.NE.0) TXT(I:I)=LC(J:J)
	END DO
C
	RETURN
C
C
	END
