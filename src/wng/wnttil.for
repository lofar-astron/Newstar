C+ WNTTIL.FOR
C  WNB 900315
C
C  Revisions:
C	WNB 920122	Change swap type for DW
C	WNB 920306	Change byte values > 127 for Convex
C	GvD 920402	- VAX and IEEE floating is zero when exponent is zero
C			- Swap input IEEE only if different local byte order
C			- Do float byte swap on output opposite to input
C			- IEEE overflow when exponent > 128/1024 (was 127/1023)
C			- Do not exit immediately when equal data format, but
C			  different byte order
C	HjV 920522	Add translation type 8 (HP station)
C	WNB 930802	Check for open end-repeats
C	CMV 940822	Add module WNTTCH to check if translation needed
C
	SUBROUTINE WNTTIL(N,BUF,COD)
C
C  Translate buffers from one machine code to another
C
C  Result:
C
C	CALL WNTTIL( N_J:I, BUF_B(0:*):IO, COD_I(0:1,0:*):I)
C			Translate the data in BUF of length N bytes
C			from IBM to Local format, using the COD
C			table. The code table consists of pairs of values.
C			The first is a code, the second the # of elements
C			for that code. The codes are:
C				0=end
C				1=char(C)
C				2=I2(I)
C				3=I4(J)
C				4=R4(E)
C				5=R8(D)
C				6=repeat
C				7=end repeat
C				8=NOP
C				9=L1(B)
C				10=external repeat
C				11=start union
C				12=start map
C				13=end union
C				14=C8(X)
C				15=C16(Y)
C			Translation types:
C				-1=IBM360 with EBCDIC
C				0=NOP
C				1=VAX D-float
C				2=VAX G-float
C				3=Alliant
C				4=Convex
C				5=IEEE
C				6=DEC workstation
C				7=SUN workstation
C				8=HP  workstation
C	CALL WNTTLI( N_J:I, BUF_B(0:*):IO, COD_I(0:1,0:*):I)
C			Translate buffers from Local machine to IBM EBCDIC
C	CALL WNTTDL( N_J:I, BUF_B(0:*):IO, COD_I(0:1,0:*):I)
C			Translate buffers from DEC machine to Local
C	CALL WNTTLD( N_J:I, BUF_B(0:*):IO, COD_I(0:1,0:*):I)
C			Translate buffers from Local machine to DEC
C	CALL WNTTLT( N_J:I, BUF_B(0:*):IO, COD_I(0:1,0:*):I, TYP_J:I)
C			Translate buffers from local machine to TYP
C	CALL WNTTTL( N_J:I, BUF_B(0:*):IO, COD_I(0:1,0:*):I, TYP_J:I)
C			Translate buffers from TYP to local machine
C	CALL WNTTTT( N_J:I, BUF_B(0:*):IO, COD_I(0:1,0:*):I, TYP_J:I, OTYP_J:I)
C			Translate buffers from TYP to OTYP
C	CALL WNTTCH( TYP_J:I, OTYP_J:I, NEEDED_L:O )
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
C
C  Parameters:
C
	INTEGER MXRCNT			!MAXIMUM REPEAT CNT
	  PARAMETER(MXRCNT=64)
	INTEGER MXUCNT			!MAXIMUM UNION CNT
	  PARAMETER(MXUCNT=64)
	INTEGER MNTP,MXTP		!MIN/MAX TYPE RECOGNISED
	  PARAMETER(MNTP=-1,MXTP=8)	
C
C  Arguments:
C
	INTEGER N			!BUFFER LENGTH IN BYTES
	BYTE BUF(0:*)			!BUFFER TO TRANSLATE
	INTEGER*2 COD(0:1,0:*)		!TRANSLATION TABLE
	INTEGER TYP			!MACHINE TYPE
	INTEGER OTYP			!MACHINE TYPE
	INTEGER TYP_C			!MACHINE TYPE
	INTEGER OTYP_C			!MACHINE TYPE
	LOGICAL NEEDED			!TRANSLATION NEEDED
C
C  Function references:
C
C
C  Data declarations:
C
	INTEGER ICHK			!UNION CHECK
	INTEGER ITAB			!CODE POINTER
	INTEGER IPT			!DATA POINTER
	INTEGER RCNT			!REPEAT CNT
	INTEGER UCNT			!UNION CNT
	INTEGER ICOD			!TRANSLATION CODE
	INTEGER REP(3,MXRCNT)		!REPEAT INFO
	INTEGER UNI(3,MXUCNT)		!UNION INFO
	INTEGER ITP,OTP			!LOCAL INPUT/OUTPUT TYPE
	LOGICAL STP			!SWAP I2/I4 TYPE
	LOGICAL STPL			!LOCAL SWAP TYPE
	LOGICAL STPI			!INPUT SWAP TYPE
	LOGICAL STPO			!OUTPUT SWAP TYPE
	LOGICAL SWTYP(MNTP:MXTP)	!SWAP TABLE FOR TYPES
	  DATA SWTYP/.FALSE.,.FALSE.,.TRUE.,.TRUE.,.FALSE.,
	1		.FALSE.,.FALSE.,.TRUE.,.FALSE.,.FALSE./
	INTEGER XTR(MNTP:MXTP)		!SAME TRANSLATION
	  DATA XTR/-1,0,1,2,5,5,5,5,5,5/
	BYTE LI24(8)			!CONVERSION
	  INTEGER JI18,JI28,JI24
	  EQUIVALENCE(LI24(1),JI24,JI18),(LI24(5),JI28)
	BYTE EBC(0:255)			!TRANSLATION TABLE EBCDIC TO ASCII
	  DATA EBC	/0,1,2,3,0,9,0,127,0,0,0,11,12,13,14,15,
	1		16,17,18,0,0,0,8,0,24,25,0,0,28,29,30,31,
	1		0,0,28,0,0,10,23,27,0,0,0,0,0,5,6,7,
	1		0,0,24,0,0,30,0,4,0,0,0,19,20,21,0,26,
	1		32,0,0,0,0,0,0,0,0,0,0,46,60,40,43,124,
	1		38,0,0,0,0,0,0,0,0,0,33,36,42,41,59,94,
	1		45,47,0,0,0,0,0,0,0,0,124,44,37,95,62,63,
	1		0,0,0,0,0,0,0,0,0,96,58,35,64,39,61,34,
	1		0,97,98,99,100,101,102,103,104,105,0,0,0,0,0,0,
	1		0,106,107,108,109,110,111,112,113,114,0,0,0,0,0,0,
	1		0,126,115,116,117,118,119,120,121,122,0,0,0,0,0,0,
	1		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	1		123,65,66,67,68,69,70,71,72,73,0,0,0,0,0,0,
	1		125,74,75,76,77,78,79,80,81,82,0,0,0,0,0,0,
	1		92,0,83,84,85,86,87,88,89,90,0,0,0,0,0,0,
	1		48,49,50,51,52,53,54,55,56,57,124,0,0,0,0,0/
	BYTE FEBC(0:127)			!FROM ASCII TO EBCDIC
	  DATA FEBC	/0,1,2,3,55,45,46,47,
	1		22,5,37,11,12,13,14,15,
	1		16,17,18,59,60,61,50,38,
	1		24,25,63,39,28,29,30,31,
	1		64,90,127,123,91,108,80,125,
	1		77,93,92,78,107,96,75,97,
	1		-26,-25,-24,-23,-22,-21,-20,-19,
	1		-18,-17,122,94,76,126,110,111,
	1		124,-63,-62,-61,-60,-59,-58,-57,
	1		-56,-55,-47,-46,-45,-44,-43,-42,
	1		-41,-40,-39,-30,-29,-28,-27,-26,
	1		-25,-24,-23,-64,-32,-48,95,109,
	1		121,-127,-126,-125,-124,-123,-122,-121,
	1		-120,-119,-111,-110,-109,-108,-107,-106,
	1		-105,-104,-103,-94,-93,-92,-91,-90,
	1		-89,-88,-87,-86,-6,-48,-95,7/
	INTEGER JA,JB
	LOGICAL CHECK_ONLY			!FLAG WNTTCH
C-
C
C WNTTIL
C
	ITP=-1					!IBM IN
	OTP=PRGDAT				!LOCAL OUT
	GOTO 200
C
C WNTTLI
C
	ENTRY WNTTLI(N,BUF,COD)
C
	ITP=PRGDAT				!LOCAL IN
	OTP=-1					!IBM OUT
	GOTO 200
C
C WNTTDL
C
	ENTRY WNTTDL(N,BUF,COD)
C
	ITP=1					!DEC IN
	OTP=PRGDAT				!LOCAL OUT
	GOTO 200
C
C WNTTLD
C
	ENTRY WNTTLD(N,BUF,COD)
C
	ITP=PRGDAT				!LOCAL IN
	OTP=1					!DEC OUT
	GOTO 200
C
C WNTTLT
C
	ENTRY WNTTLT(N,BUF,COD,TYP)
C
	ITP=PRGDAT				!LOCAL IN
	OTP=TYP					!OUT
	GOTO 200
C
C WNTTTL
C
	ENTRY WNTTTL(N,BUF,COD,TYP)
C
	ITP=TYP					!IN
	OTP=PRGDAT				!LOCAL OUT
	GOTO 200
C
C WNTTTT
C
	ENTRY WNTTTT(N,BUF,COD,TYP,OTYP)
C
	ITP=TYP					!IN
	OTP=OTYP				!OUT
	GOTO 200
C
C WNTTCH
C
	ENTRY WNTTCH(TYP_C,OTYP_C,NEEDED)
C
	ITP=TYP_C				!IN
	OTP=OTYP_C				!OUT
	NEEDED=.FALSE.				!ASSUME NOT NEEDED
	CHECK_ONLY=.TRUE.			!JUST CHECK
	GOTO 201
C
C PRELIMINARIES
C
 200	CONTINUE
	CHECK_ONLY=.FALSE.			!DO TRANSLATE
 201	CONTINUE
	J=0					!ASSUME READY
	RCNT=0					!RESET REPEAT LEVEL
	IF (MIN(ITP,OTP).LT.MNTP .OR. MAX(ITP,OTP).GT.MXTP) GOTO 1000 !ERROR
	STPI=SWTYP(ITP)				!INPUT SWAP
	STPO=SWTYP(OTP)				!OUTPUT SWAP
	STP=STPI.XOR.STPO			!SET SWAP INPUT/OUTPUT
	JI24=1
	STPL=LI24(1).EQ.1			!LOCAL SWAP
	ITP=XTR(ITP)				!EQUALISE ALL
	OTP=XTR(OTP)
	IF (ITP*OTP.EQ.0) GOTO 1000 		!NOP, READY
	IF (ITP.EQ.OTP .AND. .NOT.STP) GOTO 1000 !SAME, READY
	IF (CHECK_ONLY) THEN			!IF ONLY TEST
	   NEEDED=.TRUE.			!FLAG TRANSLATION NEEDED
	   RETURN				!AND RETURN
	ENDIF
C
C START CONVERSION
C
 100	CONTINUE
	ICHK='7fffffff'X			!UNION CHECK
	J=N					!LENGTH TO DO
	ITAB=0					!TRANSLATION PTR
	IPT=0					!DATA PTR
	RCNT=0					!REPEAT LEVEL
	UCNT=0					!UNION LEVEL
C
C CONVERT
C
 10	CONTINUE
	IF (J.LE.0) GOTO 1000			!READY
	ICOD=COD(0,ITAB)			!CODE
	J1=COD(1,ITAB)				!# OF VALUES THIS CODE
	ITAB=ITAB+1				!CODE CNT
	IF (UCNT.LT.1) GOTO 11			!NORMAL
	IF (UNI(1,UCNT).NE.0) GOTO 11		!UNION IN EXECUTION
	GOTO (1080,1080,1080,1080,1080,1080,1080,1080,1080,1080,
	1		1080,1110,1120,1130,1080,1080) ICOD+1 !SKIP MAP
	GOTO 10
 11	GOTO (1000,1010,1020,1030,1040,1050,1060,1070,1080,1090,
	1		1100,1110,1120,1130,1140,1150) ICOD+1
C
C CODES
C
C
C NOP
C
 1080	GOTO 10					!UNKNOWN OR NOP
C
C END
C
 1000	CONTINUE
	IF (RCNT.GT.0) THEN			!STILL OPEN REPEAT
	  ITAB=ITAB-1				!POINT 1 BACK
	  GOTO 1070				!DO END REPEAT
	END IF
	IF (J.GT.0) CALL WNGMVZ(J,BUF(IPT))	!EMPTY REMAINDER
C
	RETURN					!READY
C
C B
C
 1090	CONTINUE				!L1
	J2=MIN(J1,J)				!# TO DO
 1011	IPT=IPT+J2				!BUF PTR
	J=J-J2					!CNT
	GOTO 10					!NEXT CODE
C
C C
C
 1010	J2=MIN(J1,J)				!CHAR TO DO
	IF (ITP.EQ.-1) THEN			!IBM IN
	  DO I=1,J2
	    J3=BUF(IPT)
	    J3=IAND(J3,255)
	    BUF(IPT)=EBC(J3)			!TRANSLATE
	    IPT=IPT+1				!INPUT PTR
	    J=J-1				!COUNT
	  END DO
	  GOTO 10				!NEXT CODE
	ELSE IF (OTP.EQ.-1) THEN		!IBM OUT
	  DO I=1,J2
	    J3=BUF(IPT)
	    J3=IAND(J3,127)
	    BUF(IPT)=FEBC(J3)			!TRANSLATE
	    IPT=IPT+1				!INPUT PTR
	    J=J-1				!COUNT
	  END DO
	  GOTO 10				!NEXT CODE
	END IF
	GOTO 1011				!NEXT CODE
C
C I
C
 1020	J2=2*MIN(J1,J/2)			!I2
	IF (STP) CALL WNGSWB(J2,BUF(IPT))	!SWAP BYTES
	GOTO 1011				!NEXT
C
C J
C
 1030	J2=4*MIN(J1,J/4)			!I4
	IF (STP) CALL WNGSWJ(J2,BUF(IPT))	!SWAP BYTES IN J VALUES
	GOTO 1011				!NEXT
C
C REPEAT
C
 1060	RCNT=RCNT+1				!REPEAT
	IF (RCNT.GT.MXRCNT) GOTO 1000		!TOO MANY, QUIT
	REP(1,RCNT)=0				!INLINE
	REP(2,RCNT)=J1				!REPEAT CNT
	REP(3,RCNT)=ITAB			!CODE PTR
	GOTO 10					!NEXT
C
C EXTERNAL REPEAT
C
 1100	RCNT=RCNT+1				!EXTERNAL REPEAT
	IF (RCNT.GT.MXRCNT) GOTO 1000		!TOO MANY, QUIT
	J2=COD(0,ITAB)				!OFFSET REFERENCED STRUCTURE
	ITAB=ITAB+1
	REP(1,RCNT)=ITAB			!CONTINUATION
	REP(2,RCNT)=J1				!CNT
	REP(3,RCNT)=ITAB-1+J2/2			!REFERENCED STRUCTURE
	ITAB=REP(3,RCNT)			!CODE TO DO
	GOTO 10					!NEXT
C
C END REPEAT
C
 1070	REP(2,RCNT)=REP(2,RCNT)-1		!END REPEAT
	IF (REP(2,RCNT).GT.0) THEN		!MORE
	  ITAB=REP(3,RCNT)			!GO BACK
	  GOTO 10				!NEXT
	END IF
	IF (REP(1,RCNT).NE.0) ITAB=REP(1,RCNT)	!EXTERNAL REPEAT ENDED
	RCNT=RCNT-1				!LOWER LEVEL
	GOTO 10					!NEXT
C
C START UNION
C
 1110	UCNT=UCNT+1				!START UNION
	IF (UCNT.GT.MXUCNT) GOTO 1000		!TOO MANY, QUIT
	UNI(1,UCNT)=0				!NOT IN EXECUTION
	UNI(2,UCNT)=ITAB			!PIECE TO DO
	UNI(3,UCNT)=ICHK			!REFERENCE CODE OFFSET
	GOTO 10					!NEXT
C
C START MAP
C
 1120	J2=UNI(1,UCNT)				!START MAP
 1122	IF (J2.NE.0) THEN			!IN EXECUTION
	  ITAB=J2				!CONTINUE
	  UCNT=UCNT-1
	  GOTO 10
	END IF
	J2=ICHK-J1				!NEW REFERENCE CODE OFFSET
	IF (J2.LT.0) THEN
	  IF (UNI(3,UCNT).GE.0 .OR. J2.LT.UNI(3,UCNT))
	1			GOTO 10		!OLD>0 OR NEW<OLD: LEAVE
 1121	  UNI(3,UCNT)=J2			!NEW REFERENCE
	  UNI(2,UCNT)=ITAB			!NEW CODE
	ELSE IF (UNI(3,UCNT).LT.0 .OR. J2.LE.UNI(3,UCNT)) THEN
	  GOTO 1121				!OLD<0 OR NEW<OLD: NEW
	END IF
	GOTO 10					!NEXT
C
C END UNION
C
 1130	J2=UNI(1,UCNT)				!END UNION
	IF (J2.NE.0) GOTO 1122			!IN EXECUTION
	UNI(1,UCNT)=ITAB			!SAVE CONTINUATION
	ITAB=UNI(2,UCNT)			!CODE TO EXECUTE
	GOTO 10					!NEXT
C
C X
C
 1140	CONTINUE				!C8
	J1=2*J1
C
C E
C
 1040	CONTINUE				!R4
	J2=4*MIN(J1,J/4)
C
C ONLY A SWAP IS NEEDED IF INPUT AND OUTPUT TYPE ARE EQUAL
C
	IF (ITP.EQ.OTP) THEN
	  CALL WNGSWJ(J2,BUF(IPT))
	  GOTO 1011				!NEXT CODE
	END IF
	DO J3=IPT,IPT+J2-4,4			!CONVERT
C
C CONVERT INPUT BYTES TO LOCAL ORDER
C
C  DEC IN
C
	  IF (ITP.EQ.1 .OR. ITP.EQ.2) THEN	!DEC D OR G-FLOAT IN
	    IF (STPL) THEN			!DEC MACHINE
	      LI24(1)=BUF(J3+2)			!GET NUMBER
	      LI24(2)=BUF(J3+3)
	      LI24(3)=BUF(J3+0)
	      LI24(4)=BUF(J3+1)
	    ELSE
	      LI24(1)=BUF(J3+1)			!GET NUMBER
	      LI24(2)=BUF(J3+0)
	      LI24(3)=BUF(J3+3)
	      LI24(4)=BUF(J3+2)
	    END IF
C
C  OTHER IN
C
	  ELSE
	    IF (STPL.XOR.STPI) THEN		!DIFFERENT LOCAL BYTE ORDER
	      LI24(1)=BUF(J3+3)			!GET NUMBER
	      LI24(2)=BUF(J3+2)
	      LI24(3)=BUF(J3+1)
	      LI24(4)=BUF(J3+0)
	    ELSE
	      LI24(1)=BUF(J3+0)			!GET NUMBER
	      LI24(2)=BUF(J3+1)
	      LI24(3)=BUF(J3+2)
	      LI24(4)=BUF(J3+3)
	    END IF
	  END IF
C
C GET SIGN, EXPONENT AND FRACTION
C
C  IBM IN
C
	  IF (ITP.EQ.-1) THEN			!IBM IN
	    JA=4*ISHFT(IAND(JI24,'7f000000'X),-24)-256 !UNBIASED POW.
	    JB=JI24				!SIGN
	    IF (IAND(JI24,'00ffffff'X).EQ.0) THEN !ZERO FRACTION -> ZERO NUMBER
	      JI24=0
	    ELSE
	      DO WHILE(IAND(JI24,2**23).EQ.0)	!NORMALIZE
	        JI24=ISHFT(IAND(JI24,'00ffffff'X),1)
	        JA=JA-1
	      END DO
	    END IF
	    JI24=ISHFT(IAND(JI24,'00ffffff'X),8) !PROPER FRACTION
C
C  DEC IN
C
	  ELSE IF (ITP.EQ.1 .OR. ITP.EQ.2) THEN	!DEC D OR G-FLOAT IN
	    JB=JI24				!SIGN
	    JA=ISHFT(IAND(JI24,'7f800000'X),-23) !EXPONENT
	    IF (JA.EQ.0) THEN			!ZERO EXPONENT -> ZERO NUMBER
	      JI24=0
	    ELSE
	      JA=JA-128				!UNBIASED EXPONENT
	      JI24=IOR('80000000'X,ISHFT(IAND(JI24,'007fffff'X),8)) !FRACTION
	    END IF
C
C  IEEE IN
C
	  ELSE					!IEEE IN
	    JB=JI24				!SIGN
	    JA=ISHFT(IAND(JI24,'7f800000'X),-23) !EXPONENT
	    IF (JA.EQ.0) THEN			!ZERO EXPONENT -> ZERO NUMBER
	      JI24=0
	    ELSE
	      JA=JA-126				!UNBIASED EXPONENT
	      JI24=IOR('80000000'X,ISHFT(IAND(JI24,'007fffff'X),8)) !FRACTION
	    END IF
	  END IF
C
C CONVERT TO OUTPUT FORMAT
C
C  IBM OUT
C
	  IF (OTP.EQ.-1) THEN			!IBM OUT
	    DO WHILE (IAND(JA,3).NE.0)
	      JI24=IAND(ISHFT(JI24,-1),'7fffffff'X) !NORMALISE
	      JA=JA+1
	    END DO
	    JA=JA/4				!CORRECT EXP
	    IF (JA.GT.63) THEN			!OVERFLOW
	      JI24=-1				!LARGEST FRACTION
	      JA=63
	    END IF
	    IF (JA.LE.-64 .OR. JI24.EQ.0) THEN	!UNDERFLOW
	      JI24=0
	      JA=-64
	      JB=0
	    END IF
	    JI24=IAND(JB,'80000000'X)+		!MAKE NUMBER
	1		IAND(ISHFT(JA+64,24),'7f000000'X)+
	1		IAND(ISHFT(JI24,-8),'00ffffff'X)
C
C  DEC OUT
C
	  ELSE IF (OTP.EQ.1 .OR. OTP.EQ.2) THEN	!DEC D OR G-FLOAT OUT
	    IF (JA.GT.127) THEN			!OVERFLOW
	      JI24=-1				!LARGEST FRACTION
	      JA=127
	    END IF
	    IF (JA.LE.-128 .OR. JI24.EQ.0) THEN	!UNDERFLOW
	      JI24=0
	      JA=-128
	      JB=0
	    END IF
	    JI24=IAND(JB,'80000000'X)+		!MAKE NUMBER
	1		IAND(ISHFT(JA+128,23),'7f800000'X)+
	1		IAND(ISHFT(JI24,-8),'007fffff'X)
C
C  IEEE OUT
C
	  ELSE 					!IEEE OUT
	    IF (JA.GT.128) THEN			!OVERFLOW
	      JI24=-1				!LARGEST FRACTION
	      JA=128
	    END IF
	    IF (JA.LE.-126 .OR. JI24.EQ.0) THEN	!UNDERFLOW
	      JI24=0
	      JA=-126
	      JB=0
	    END IF
	    JI24=IAND(JB,'80000000'X)+		!MAKE NUMBER
	1		IAND(ISHFT(JA+126,23),'7f800000'X)+
	1		IAND(ISHFT(JI24,-8),'007fffff'X)
	  END IF				!OTP
C
C CONVERT OUTPUT BYTES FROM LOCAL ORDER
C
C  DEC OUT
C
	  IF (OTP.EQ.1 .OR. OTP.EQ.2) THEN
	    IF (STPL) THEN			!DEC MACHINE
	      BUF(J3+2)=LI24(1)			!OUTPUT RESULT
	      BUF(J3+3)=LI24(2)
	      BUF(J3+0)=LI24(3)
	      BUF(J3+1)=LI24(4)
	    ELSE
	      BUF(J3+1)=LI24(1)			!OUTPUT RESULT
	      BUF(J3+0)=LI24(2)
	      BUF(J3+3)=LI24(3)
	      BUF(J3+2)=LI24(4)
	    END IF
C
C  OTHER OUT
C
	  ELSE
	    IF (STPL.XOR.STPO) THEN		!DIFFERENT BYTE ORDER
	      BUF(J3+3)=LI24(1)			!OUTPUT RESULT
	      BUF(J3+2)=LI24(2)
	      BUF(J3+1)=LI24(3)
	      BUF(J3+0)=LI24(4)
	    ELSE
	      BUF(J3+0)=LI24(1)			!OUTPUT RESULT
	      BUF(J3+1)=LI24(2)
	      BUF(J3+2)=LI24(3)
	      BUF(J3+3)=LI24(4)
	    END IF
	  END IF
	END DO					!NEXT VALUE
	GOTO 1011				!NEXT CODE
C
C Y
C
 1150	CONTINUE				!C16
	J1=2*J1
C
C D
C
 1050	CONTINUE				!R8
	J2=8*MIN(J1,J/8)
C
C ONLY A SWAP IS NEEDED IF INPUT AND OUTPUT TYPE ARE EQUAL
C
	IF (ITP.EQ.OTP) THEN
	  CALL WNGSWQ(J2,BUF(IPT))
	  GOTO 1011				!NEXT CODE
	END IF
	DO J3=IPT,IPT+J2-8,8			!CONVERT
C
C
C CONVERT INPUT BYTES TO LOCAL ORDER
C
C  DEC IN
C
	  IF (ITP.EQ.1 .OR. ITP.EQ.2) THEN	!DEC D OR G-FLOAT IN
	    IF (STPL) THEN			!DEC MACHINE
	      LI24(1)=BUF(J3+2)			!GET NUMBER
	      LI24(2)=BUF(J3+3)
	      LI24(3)=BUF(J3+0)
	      LI24(4)=BUF(J3+1)
	      LI24(5)=BUF(J3+6)
	      LI24(6)=BUF(J3+7)
	      LI24(7)=BUF(J3+4)
	      LI24(8)=BUF(J3+5)
	    ELSE
	      LI24(1)=BUF(J3+1)			!GET NUMBER
	      LI24(2)=BUF(J3+0)
	      LI24(3)=BUF(J3+3)
	      LI24(4)=BUF(J3+2)
	      LI24(5)=BUF(J3+5)
	      LI24(6)=BUF(J3+4)
	      LI24(7)=BUF(J3+7)
	      LI24(8)=BUF(J3+6)
	    END IF
C
C  OTHER IN
C
	  ELSE
	    IF (STPL.XOR.STPI) THEN		!DIFFERENT LOCAL BYTE ORDER
	      LI24(1)=BUF(J3+7)			!GET NUMBER
	      LI24(2)=BUF(J3+6)
	      LI24(3)=BUF(J3+5)
	      LI24(4)=BUF(J3+4)
	      LI24(5)=BUF(J3+3)
	      LI24(6)=BUF(J3+2)
	      LI24(7)=BUF(J3+1)
	      LI24(8)=BUF(J3+0)
	    ELSE				!SAME BYTE ORDER
	      LI24(1)=BUF(J3+0)			!GET NUMBER
	      LI24(2)=BUF(J3+1)
	      LI24(3)=BUF(J3+2)
	      LI24(4)=BUF(J3+3)
	      LI24(5)=BUF(J3+4)
	      LI24(6)=BUF(J3+5)
	      LI24(7)=BUF(J3+6)
	      LI24(8)=BUF(J3+7)
	    END IF
	    IF (STPL) THEN			!LOCAL DEC MACHINE
	      JB=JI18				!SWAP LONGWORDS
	      JI18=JI28
	      JI28=JB
	    END IF
	  END IF
C
C GET SIGN, EXPONENT AND FRACTION
C
C  IBM IN
C
	  IF (ITP.EQ.-1) THEN			!IBM IN
	    JA=4*ISHFT(IAND(JI18,'7f000000'X),-24)-256 !UNBIASED POW.
	    JB=JI18				!SIGN
	    IF (IAND(JI18,'00ffffff'X).EQ.0) THEN !ZERO FRACTION -> ZERO NUMBER
	      JI18=0
	      JI28=0
	    ELSE
	      DO WHILE(IAND(JI18,2**23).EQ.0)	!NORMALIZE
	        JI18=ISHFT(IAND(JI18,'00ffffff'X),1)
		IF (JI28.LT.0) JI18=JI18+1
		JI28=ISHFT(JI28,1)
	        JA=JA-1
	      END DO
	    END IF
	    JI18=ISHFT(IAND(JI18,'00ffffff'X),8)+
	1		IAND(ISHFT(JI28,-24),'000000ff'X) !PROPER FRACTION
	    JI28=ISHFT(JI28,8)
C
C  DEC-D IN
C
	  ELSE IF (ITP.EQ.1) THEN		!DEC D-FLOAT IN
	    JB=JI18				!SIGN
	    JA=ISHFT(IAND(JI18,'7f800000'X),-23) !EXPONENT
	    IF (JA.EQ.0) THEN			!ZERO EXPONENT -> ZERO NUMBER
	      JI18=0
	      JI28=0
	    ELSE
	      JA=JA-128				!UNBIASED EXPONENT
	      JI18=IOR('80000000'X,ISHFT(IAND(JI18,'007fffff'X),8))+
	1		IAND(ISHFT(JI28,-24),'000000ff'X) !PROPER FRACTION
	      JI28=ISHFT(JI28,8)
	    END IF
C
C  DEC-G IN
C
	  ELSE IF (ITP.EQ.2) THEN		!DEC G-FLOAT IN
	    JB=JI18				!SIGN
	    JA=ISHFT(IAND(JI18,'7ff00000'X),-20) !EXPONENT
	    IF (JA.EQ.0) THEN			!ZERO EXPONENT -> ZERO NUMBER
	      JI18=0
	      JI28=0
	    ELSE
	      JA=JA-1024			!UNBIASED EXPONENT
	      JI18=IOR('80000000'X,ISHFT(IAND(JI18,'000fffff'X),11))+
	1		IAND(ISHFT(JI28,-21),'000007ff'X) !PROPER FRACTION
	      JI28=ISHFT(JI28,11)
	    END IF
C
C IEEE IN
C
	  ELSE 					!IEEE IN
	    JB=JI18				!SIGN
	    JA=ISHFT(IAND(JI18,'7ff00000'X),-20) !EXPONENT
	    IF (JA.EQ.0) THEN			!ZERO EXPONENT -> ZERO NUMBER
	      JI18=0
	      JI28=0
	    ELSE
	      JA=JA-1022			!UNBIASED EXPONENT
	      JI18=IOR('80000000'X,ISHFT(IAND(JI18,'000fffff'X),11))+
	1		IAND(ISHFT(JI28,-21),'000007ff'X) !PROPER FRACTION
	      JI28=ISHFT(JI28,11)
	    END IF
	  END IF
C
C CONVERT TO OUTPUT FORMAT
C
C  IBM OUT
C
	  IF (OTP.EQ.-1) THEN			!IBM OUT
	    DO WHILE (IAND(JA,3).NE.0)		!NORMALIZE
	      JI28=IAND(ISHFT(JI28,-1),'7fffffff'X)+
	1		IAND(ISHFT(JI18,31),'80000000'X)
	      JI18=IAND(ISHFT(JI18,-1),'7fffffff'X) !NORMALISE
	      JA=JA+1
	    END DO
	    JA=JA/4				!CORRECT EXP
	    IF (JA.GT.63) THEN			!OVERFLOW
	      JI18=-1				!LARGEST FRACTION
	      JI28=-1
	      JA=63
	    END IF
	    IF (JA.LE.-64 .OR. JI18.EQ.0) THEN	!UNDERFLOW
	      JI18=0
	      JI28=0
	      JA=-64
	      JB=0
	    END IF
	    JI28=ISHFT(JI28,-8)+IAND(ISHFT(JI18,24),'ff000000'X) !MAKE NUMBER
	    JI18=IAND(JB,'80000000'X)+
	1		IAND(ISHFT(JA+64,24),'7f000000'X)+
	1		IAND(ISHFT(JI18,-8),'00ffffff'X)
C
C  DEC-D OUT
C
	  ELSE IF (OTP.EQ.1) THEN		!DEC D-FLOAT OUT
	    IF (JA.GT.127) THEN			!OVERFLOW
	      JI18=-1				!LARGEST FRACTION
	      JI28=-1
	      JA=127
	    END IF
	    IF (JA.LE.-128 .OR. JI18.EQ.0) THEN	!UNDERFLOW
	      JI18=0
	      JI28=0
	      JA=-128
	      JB=0
	    END IF
	    JI28=IAND(ISHFT(JI28,-8),'00ffffff'X)+
	1		IAND(ISHFT(JI18,24),'ff000000'X) 	  !MAKE NUMBER
	    JI18=IAND(JB,'80000000'X)+
	1		IAND(ISHFT(JA+128,23),'7f800000'X)+
	1		IAND(ISHFT(JI18,-8),'007fffff'X)
C
C  DEC-G OUT
C
	  ELSE IF (OTP.EQ.2) THEN		!DEC G-FLOAT OUT
	    IF (JA.GT.1023) THEN		!OVERFLOW
	      JI18=-1				!LARGEST FRACTION
	      JI28=-1
	      JA=1023
	    END IF
	    IF (JA.LE.-1024 .OR. JI18.EQ.0) THEN !UNDERFLOW
	      JI18=0
	      JI28=0
	      JA=-1024
	      JB=0
	    END IF
	    JI28=IAND(ISHFT(JI28,-11),'001fffff'X)+
	1		IAND(ISHFT(JI18,21),'ffe00000'X) 	  !MAKE NUMBER
	    JI18=IAND(JB,'80000000'X)+
	1		IAND(ISHFT(JA+1024,20),'7ff00000'X)+
	1		IAND(ISHFT(JI18,-11),'000fffff'X)
C
C  IEEE OUT
C
	  ELSE					!IEEE OUT
	    IF (JA.GT.1024) THEN		!OVERFLOW
	      JI18=-1				!LARGEST FRACTION
	      JI28=-1
	      JA=1024
	    END IF
	    IF (JA.LE.-1022 .OR. JI18.EQ.0) THEN !UNDERFLOW
	      JI18=0
	      JI28=0
	      JA=-1022
	      JB=0
	    END IF
	    JI28=IAND(ISHFT(JI28,-11),'001fffff'X)+
	1		IAND(ISHFT(JI18,21),'ffe00000'X) 	  !MAKE NUMBER
	    JI18=IAND(JB,'80000000'X)+
	1		IAND(ISHFT(JA+1022,20),'7ff00000'X)+
	1		IAND(ISHFT(JI18,-11),'000fffff'X)
	  END IF				!OTP
C
C CONVERT OUTPUT BYTES FROM LOCAL ORDER
C
C  DEC OUT
C
	  IF (OTP.EQ.1 .OR. OTP.EQ.2) THEN
	    IF (STPL) THEN			!DEC MACHINE
	      BUF(J3+2)=LI24(1)			!OUTPUT RESULT
	      BUF(J3+3)=LI24(2)
	      BUF(J3+0)=LI24(3)
	      BUF(J3+1)=LI24(4)
	      BUF(J3+6)=LI24(5)
	      BUF(J3+7)=LI24(6)
	      BUF(J3+4)=LI24(7)
	      BUF(J3+5)=LI24(8)
	    ELSE
	      BUF(J3+1)=LI24(1)			!OUTPUT RESULT
	      BUF(J3+0)=LI24(2)
	      BUF(J3+3)=LI24(3)
	      BUF(J3+2)=LI24(4)
	      BUF(J3+5)=LI24(5)
	      BUF(J3+4)=LI24(6)
	      BUF(J3+7)=LI24(7)
	      BUF(J3+6)=LI24(8)
	    END IF
C
C  OTHER OUT
C
	  ELSE
	    IF (STPL) THEN			!LOCAL DEC MACHINE
	      JB=JI18				!SWAP LONGWORDS
	      JI18=JI28
	      JI28=JB
	    END IF
	    IF (STPL.XOR.STPO) THEN		!DIFFERENT BYTE ORDER
	      BUF(J3+7)=LI24(1)			!OUTPUT RESULT
	      BUF(J3+6)=LI24(2)
	      BUF(J3+5)=LI24(3)
	      BUF(J3+4)=LI24(4)
	      BUF(J3+3)=LI24(5)
	      BUF(J3+2)=LI24(6)
	      BUF(J3+1)=LI24(7)
	      BUF(J3+0)=LI24(8)
	    ELSE				!SAME BYTE ORDER
	      BUF(J3+0)=LI24(1)			!OUTPUT RESULT
	      BUF(J3+1)=LI24(2)
	      BUF(J3+2)=LI24(3)
	      BUF(J3+3)=LI24(4)
	      BUF(J3+4)=LI24(5)
	      BUF(J3+5)=LI24(6)
	      BUF(J3+6)=LI24(7)
	      BUF(J3+7)=LI24(8)
	    END IF
	  END IF
	END DO					!NEXT VALUE
	GOTO 1011				!NEXT CODE
C
C
	END
