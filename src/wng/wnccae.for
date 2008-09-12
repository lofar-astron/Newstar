C+ WNCCAE.FOR
C  WNB 890111
C
C  Revisions:
C	WNB 930325	Cater for unaligned data
C
	SUBROUTINE WNCCAE(COUT,CLEN,ICOD1,ICOD2,VALE,COD1,COD2)
C
C  Convert a value to a string
C
C  Result:
C
C	CALL WNCCAE ( COUT_C*:O, CLEN_J:O, ICOD1_J:I, ICOD2_J:I,
C					VALE_E:I, COD1_J:I, COD2_J:I)
C				Convert the  REAL value VALE to a string
C				in COUT, setting CLEN to the significant
C				length of COUT. The COD's indicate the
C				conversion type:
C				ICOD1	=1:	input in fractions of circles
C					=2:	input in radians
C					=3:	input in degrees
C					=other:	input in radians
C				ICOD2	=2:	output in degrees (-180<<+180)
C					=3:	output in d.m.s
C					=4:	output in h:m:s
C					=5:	output in degrees (0<<360)
C					=other:	output in degrees (-180<<+180)
C				For output in degrees:
C				If COD2 < 0:	in G-type format, with:
C						COD1 <=0: enough signif. digits
C						COD1 >0:  COD1 signif. digits
C				   COD2 >=0:	F-type format with COD2 digits
C						behind dec. point, and COD1
C						total width
C				For output in h:m:s or d.m.s:
C				   COD1 <=0:	rounded h:m:s or d.m.s
C					< 3:	rounded h or d
C					< 5:	rounded h:m or d.m
C					< 7:	rounded h:m:s or d.m.s
C					>=7:	COD1-6 dec. places in s
C	CALL WNCCAD ( COUT_C*:O, CLEN_J:O, ICOD1_J:I, ICOD2_J:I,
C					VALD_D:I, COD1_J:I, COD2_J:I)
C				As CAE but for DOUBLE PRECISION
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
	INTEGER ICOD1,ICOD2			!CONVERSION TYPES
	DOUBLE PRECISION VALD			!INPUT VALUE
	REAL VALE
	INTEGER COD1,COD2			!CONVERSION CODES
C
C  Function references:
C
	INTEGER WNCALN				!GET STRING LENGTH
	DOUBLE PRECISION WNGDPD,WNGDND		!NORMALIZE ANGLES
C
C  Data declarations:
C
	CHARACTER*40 F1030			!FORMAT STRING
	CHARACTER*1 STR				!SEPARATOR CHARACTER
C-
C
C WNCCAE
C
	CALL WNGMV(LB_E,VALE,R0)		!UNALIGNED POSSIBLE
	D0=R0					!INPUT VALUE
	GOTO 10
C
C WNCCAD
C
	ENTRY WNCCAD(COUT,CLEN,ICOD1,ICOD2,VALD,COD1,COD2)
C
	CALL WNGMV(LB_D,VALD,D0)		!UNALIGNED POSSIBLE
	GOTO 10
C
 10	CONTINUE
	COUT=' '				!RESULT
	IF (ICOD1.EQ.1) THEN			!FRACTIONS
	  D0=D0*360D0
	ELSE IF (ICOD1.NE.3) THEN		!RADIANS
	  D0=D0*360D0/DPI2
	END IF
	IF (ICOD2.LT.3 .OR. ICOD2.GT.5) THEN	!+-DDD.DDDDD
	  CALL WNCCDS(COUT,CLEN,WNGDND(D0),COD1,COD2) !OUTPUT
	ELSE IF (ICOD2.EQ.5) THEN		!DDD.DDDDD
	  CALL WNCCDS(COUT,CLEN,WNGDPD(D0),COD1,COD2) !OUTPUT
	ELSE
	  IF (ICOD2.EQ.4) THEN			!HMS
	    D0=WNGDPD(D0)/15D0			!MAKE HOURS
	    I=1
	    STR=':'
	  ELSE					!DMS
	    D0=WNGDND(D0)			!MAKE DEGREES
	    IF (ABS(D0).GT.90D0) THEN		!MAKE -90<=ANGLE<=+90
	      IF (D0.GE.0) THEN
		D0=D0-180D0
	      ELSE
		D0=D0+180D0
	      END IF
	    END IF
	    I=2
	    STR='.'
	    IF (D0.LT.0) THEN
	      COUT(1:1)='-'
	      D0=ABS(D0)
	    END IF
	  END IF
	  IF (COD1.LT.3 .AND. COD1.GT.0) THEN	!ONLY HH
	    WRITE (UNIT=COUT(I:I+1),FMT=1000,ERR=11) INT(D0+.5D0)
 11	    CONTINUE
	  ELSE IF (COD1.LT.5 .AND. COD1.GT.0) THEN !HH.MM
	    D0=D0+1.D0/120.D0			!ROUND
	    I1=INT(D0)
	    I2=INT((D0-I1)*60.D0)
	    WRITE (UNIT=COUT(I:I+4),FMT=1010,ERR=12) I1,STR,I2
 12	    CONTINUE
	  ELSE IF (COD1.LT.7) THEN		!DD.MM.SS
	    D0=D0+1./7200.			!ROUND
	    I1=INT(D0)
	    D0=60.D0*(D0-I1)
	    I2=INT(D0)
	    I3=INT(60.D0*(D0-I2))
	    WRITE (UNIT=COUT(I:I+7),FMT=1020,ERR=13)
	1			I1,STR,I2,STR,I3
 13	    CONTINUE
	  ELSE
	    D0=D0+(10.D0**(6-COD1))/7200.D0	!ROUND
	    I1=INT(D0)
	    D0=60.D0*(D0-I1)
	    I2=INT(D0)
	    J=MIN(COD1-6,6)
	    J1=INT(60.D0*(10.D0**J)*(D0-I2))
	    WRITE (UNIT=F1030,FMT=1030,ERR=14) J,J
	    WRITE (UNIT=COUT(I:I+8+J),FMT=F1030,ERR=14)
	1			I1,STR,I2,STR,INT(J1/(10**J)),
	2			MOD(J1,10**J)
 14	    CONTINUE
	  END IF
	  CLEN=WNCALN(COUT)			!OUTPUT LENGTH
	END IF
C
	RETURN
C
C FORMATS
C
 1000	FORMAT(I2.2)
 1010	FORMAT(I2.2,A1,I2.2)
 1020	FORMAT(I2.2,A1,I2.2,A1,I2.2)
 1030	FORMAT('(I2.2,A1,I2.2,A1,I2.2,''.'',I',I2.2,'.',I2.2,')')
C
C
	END

