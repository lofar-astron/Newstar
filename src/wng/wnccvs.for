C+ WNCCVS.FOR
C  WNB 890111
C
C  Revisions:
C	WNB 911115	DATA for CHARACTER problem
C	WNB 920303	SUN rearrangement for segmentation fault compiler
C			see also _X
C	WNB 930325	Cater for unaligned data
C	WNB 931216	Make **** appear less frequent
C	CMV 940203	Prevent HP from stripping 0 to blank
C
	SUBROUTINE WNCCDS(COUT,CLEN,VALD,COD1,COD2)
C
C  Convert a value to a string
C
C  Result:
C
C	CALL WNCCDS ( COUT_C*:O, CLEN_J:O, VALD_D:I, COD1_J:I, COD2_J:I)
C				Convert the  DOUBLE PRECISION value VALD to a
C				string in COUT, setting CLEN to the significant
C				length of COUT. The COD's indicate the
C				conversion type:
C				If COD2 < 0:	in G-type format, with:
C						COD1 <=0: enough signif. digits
C						COD1 >0:  COD1 signif. digits
C				   COD2 >=0:	F-type format with COD2 digits
C						behind dec. point, and COD1
C						total width
C	CALL WNCCES ( COUT_C*:O, CLEN_J:O, VALE_E:I, COD1_J:I, COD2_J:I)
C				As CDS for REAL value
C	CALL WNCCJS ( COUT_C*:O, CLEN_J:O, VALJ_J:I, COD1_J:I)
C				Convert INTEGER value VALJ to a string
C				in COUT, setting CLEN to the significant
C				length of COUT. COD1 indicates the conversion
C				type:
C				COD1=	1	signed decimal
C					2	unsigned decimal
C					3	octal
C					4	hexadecimal
C					5	zero filled decimal
C					6	logical (YES or NO)
C	CALL WNCCIS ( COUT_C*:O, CLEN_J:O, VALI_I:I, COD1_J:I)
C				As CJS for INTEGER*2 value
C	CALL WNCCKS ( COUT_C*:O, CLEN_J:O, VALK_K:I, COD1_J:I)
C				As CJS for INTEGER*2 value
C	CALL WNCCBS ( COUT_C*:O, CLEN_J:O, VALB_B:I, COD1_J:I)
C				As CJS for INTEGER*1 value
C	CALL WNCCAS ( COUT_C*:O, CLEN_J:O, VALC_B(*):I, COD1_J:I)
C				As CJS for character string in VALC, of length
C				COD1.
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
	CHARACTER*(*) COUT			!OUTPUT STRING
	INTEGER CLEN				!OUTPUT LENGTH
	DOUBLE PRECISION VALD			!INPUT VALUE
	REAL VALE
	INTEGER COD1,COD2			!CONVERSION CODES
C
C  Function references:
C
	INTEGER WNCALN				!LENGTH STRING
C
C  Data declarations:
C
	CHARACTER*16 F1000			!FORMAT STRING
C-
	J2=17					!INDICATE DOUBLE PRECISION
	CALL WNGMV(LB_D,VALD,D0)		!UNALIGNED POSSIBLE
	GOTO 10
C
C WNCCES
C
	ENTRY WNCCES(COUT,CLEN,VALE,COD1,COD2)
C
	J2=7					!INDICATE REAL
	CALL WNGMV(LB_E,VALE,R0)		!UNALIGNED POSSIBLE
	D0=R0
	GOTO 10
C
 10	CONTINUE
	COUT=' '				!RESULT
	L0=.FALSE.				!DO NOT TRIM
	IF (COD2.LT.0) THEN			!G-FORMAT
	  IF (COD1.GT.0) THEN
	    J2=COD1				!SIGNIFICANT # OF DIGITS
	  ELSE
	    L0=.TRUE.				!TRIM
	  END IF
	  D1=ABS(D0)				!CHECK RANGE
	  IF (D1.EQ.0D0) THEN			!0 VALUE, USE F-FORMAT
	    WRITE (UNIT=F1000,FMT=1000,ERR=20) 7+J2,J2 !MAKE F-FORMAT ITEM
	  ELSE IF (D1.LT.0.001 .OR. D1.GE.10.**(J2+3)) THEN !USE E-FORMAT
	    WRITE (UNIT=F1000,FMT=1010,ERR=20) 7+J2,J2 !MAKE E-FORMAT ITEM
	  ELSE					!IN F-FORMAT
	    I1=MAX(0,J2-1-INT(LOG10(D1)))
	    WRITE (UNIT=F1000,FMT=1000,ERR=20) 7+J2,I1 !MAKE F-FORMAT ITEM
	  END IF
	ELSE
	  IF (COD1.LT.COD2+3) THEN
	    J2=COD2+3				!MIN. F-FORMAT WIDTH
	    L0=.TRUE.				!AND TRIM
	  ELSE
	    J2=COD1
	  END IF
	  D1=ABS(D0)
	  IF (D1.LT.10.**(J2-COD2-2)) THEN	!CAN FIT
	    WRITE (UNIT=F1000,FMT=1000,ERR=20) J2,COD2 !MAKE F-FORMAT ITEM
	  ELSE IF (J2.GT.7) THEN		!TRY E
	    WRITE (UNIT=F1000,FMT=1010,ERR=20) J2,J2-7 !MAKE E-FORMAT ITEM
	  ELSE					!F AGAIN
	    WRITE (UNIT=F1000,FMT=1000,ERR=20) J2,COD2 !MAKE F-FORMAT ITEM
	  END IF
	END IF
	WRITE (UNIT=COUT,FMT=F1000,ERR=20) D0	!CONVERT VALUE
C
C FINISH
C
 20	CONTINUE
	IF (COUT.NE.' ') THEN
	  DO WHILE (COUT(:1).EQ.' ')		!DELETE LEADING SPACES
	    COUT=COUT(2:)
	  END DO
	END IF
	IF (L0) THEN				!MINIMIZE LENGTH
	  I=INDEX(COUT,'E')
	  IF (I.EQ.0) I=WNCALN(COUT)+1		!END
	  I1=INDEX(COUT,'.')
	  IF (I1.GT.0) THEN
	    DO J=I1+1,I-1
	      IF (COUT(J:J).NE.'0') I1=J
	    END DO
	    COUT(I1+1:)=COUT(I:)
	  END IF
	END IF
	I=INDEX(COUT,'.')			!DELETE TRAILING .
	IF (I.GT.0) THEN
	  IF (COUT(I:).EQ.'.') THEN
	     IF (I.EQ.1) THEN
	        COUT='0'			!If just . should be 0
	     ELSE
		COUT(I:)=' '
	     END IF
	  END IF
	END IF
	CLEN=WNCALN(COUT)			!SET OUTPUT LENGTH
C
	RETURN
C
C  Formats
C
 1000	FORMAT('(F',I3.3,'.',I3.3,')')		!CONVERT F_FORMAT
 1010	FORMAT('(E',I3.3,'.',I3.3,')')		!CONVERT G AS E
C
	END
