C+ WNCTXI_X.FOR
C  WNB 910211
C
C  Revisions:
C	WNB 911115	DW: DATA CHARACTER problems; \ problems
C	WNB 930520	Remove %VAL
C
	SUBROUTINE WNCTXI_X(OST,TXT,ARGL)
C
C  Read string with formatting (FAO with extensions) information.
C
C  Result:
C
C	CALL WNCTXI_X( OST_C*:O, TXT_C*:I, ARGL_J(-*:*):I)
C				Read string OST and convert it following the
C				format information in TXT to arguments ARG...
C
C	The WNCTXI call is in WNCTXT.FOR
C
C	The OST string will be interpreted following the format rules in
C	TXT, and the result will be put into the arguments ARG.
C	A format item has the form:
C		![w$][r]cd[v][.s][\]	where each field specifies:
C			!	start of format item
C			w	width of input field. If no width present,
C				fields are assumed to be separated with a ,
C				or blanks.
C			r	repeat factor. Each cd is repeated r times
C				assuming the possibly corresponding argument
C				to be an array. Fields generated with r>1
C				will be separated by ", ". r will always
C				be taken as max(r,1). Default: 0, i.e.
C				one value outputted.
C				r should be an unsigned integer.
C			v	optional cd modifier. Default: 0.
C				v should be an unsigned integer
C			s	optional cd specifier. Default: 0.
C				s should be an unsigned integer
C			\	optional format end indicator. Produces no
C				output, but necessary if next character in
C				TXT could be interpreted as part of current
C				format item.
C			#	the values w, r, v and s can be specified
C				as # i.s.o. an integer. The corresponding
C				value is then taken from the next argument
C				in the ARGL, interpreting it as an INTEGER
C				value.
C			##	As #, but interpretation will be an INTEGER*2.
C				NOTE: Order of arguments taken in list:
C					w,r,v,s,cd. If argument not present
C					for cd, it is an error, and conversion
C					stops. In the other cases 0 assumed.
C			cd	the operational code. It can be one of the
C				following format codes:
C				!		output !
C				*c		output character c
C				-		backskip argument in ARGL
C				+		forward skip argument in ARGL
C				/		insert new line (<CR><LF> TXS)
C				_		insert TAB (stops at 9,17...)
C				^		insert new page
C				%D		set date as dd-mmm-yy
C				%T		set time as hh:mm:ss
C				%DF		set date as dd-mmm-yyyy
C				%TF		set time as hh:mm:ss.ss
C				%DN		set date as yymmdd
C				%TN		set time as hhmmss
C				AC AD AF AZ AS	ASCII string: counted (AC),
C					    A	descriptor (AD), descriptor
C						with conversion to . of
C						non-printables (AF), address
C						of descriptor (AS), ASCIZ (AZ).
C						AS is the standard Fortran
C						string, AC=nnnncccc... with
C						n length string following,
C						AD,AD+1=cccc...,n(J), AF same,
C						AZ=ccccc....0.
C				ALv		ASCII string of length v at
C						specified address
C				SB SI SJ SK 	convert next arg of byte, word
C				   SW S	 SL	or longword type to signed
C				      		decimal
C				UB UI UJ UK	convert next arg to unsigned
C				   UW U  UL	decimal
C				      
C				OB OI OJ OK	convert next arg to octal
C				   OW O  OL
C				      
C				XB XI XJ XK	convert next arg to hexadecimal
C				   XW X  XL
C				      
C				ZB ZI ZJ ZK	convert next argument to zero
C				   ZW Z  ZL	filled decimal number
C				      
C				LB LI LJ LK	convert next arg to YES or NO
C				   LW L  LL	low order bit=0: no, =1:yes
C				      
C				E[v]		interprete next argument as
C						REAL, and convert it to G-like
C						format with v significant
C						digits. If v=0 enough digits
C						given to indicate real value.
C				E[[v].s]	interprete next argument as
C						REAL, and convert it to F-like
C						format with s digits after
C						decimal ., and v total digits.
C				D[v] D[[v].s]	convert next double precision
C						arg.
C				EC[v] EC[[v].s]	convert next COMPLEX arg.
C				DC[v] DC[[v].s]	convert next DOUBLE COMPLEX arg.
C				EAc[[v][.s]]	convert an angle of type c
C						to degrees (-180<angle<180)
C						and do as E.
C						c can be F (fraction of circles)
C						R (radians), D (degrees) or
C						empty (radians).
C				EPc[[v][.s]]	as EA but 0<=angle<360.
C				EDc[v]		convert an angle to d.m.s.tt...,
C						and round to d (v<3), m (v<5),
C						s (v<7) or tenths. (Def: v=6)
C				EHc[v]		convert an angle to h:m:s.tt...,
C						rounding as ED
C				DAc[[v][.s]]	double precision angles
C				DPc[[v][.s]]
C				DDc[v]
C				DHc[v]
C				F...		identical to all E formats
C				[r]C		Position at column r. (Def: r=1)
C				[w$][r]Q[v]	Format: Limit repeats to line
C						width w, and offset next line at
C						r. If v=1: no , separation.
C
C
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
C
C  Parameters:
C
	INTEGER OUTLEN				!SOME LENGHTS
	  PARAMETER (OUTLEN=512)
	CHARACTER*1 FF				!FF
C	  PARAMETER (FF=CHAR(12))               !Not allowed anymore
	CHARACTER*1 CR				!CR
C	  PARAMETER (CR=CHAR(13))               !Not allowed anymore
	CHARACTER*1 LF				!LF
C	  PARAMETER (LF=CHAR(10))               !Not allowed anymore
	CHARACTER*1 BACKS			!\
C	  PARAMETER (BACKS=CHAR(92))            !Not allowed anymore
C
C  Arguments:
C
	CHARACTER*(*) TXT			!TEXT TO BE CONVERTED, OUTPUT
	CHARACTER*(*) OST			!OUTPUT STRING FOR WNCTXS
	INTEGER ARGL(0:*)			!ARGUMENTS LIST
C
C  Function references:
C
	INTEGER WNCAL0				!ACTUAL LENGTH STRING
	INTEGER WNCALZ				!ACTUAL LENGTH ASCIZ STRING
	INTEGER WNCAJA				!GET INTEGER FROM STRING
	CHARACTER*1 WNCAUP			!CONVERT TO UC
	INTEGER WNGASA				!ADDRESS OF STRING
	INTEGER WNGASL				!LENGTH OF STRING
C
C  Data declarations:
C
	CHARACTER*(OUTLEN) CHLP			!INTERNAL CONVERSION
	CHARACTER*(OUTLEN) OUT			!OUTPUT STRING
	LOGICAL OPS				!SWITCH WRCTXT TYPE
	INTEGER LARG,PARG			!LENGTH/POINTER ARGUMENT LIST
	INTEGER LTXT,PTXT			!LENGTH/POINTER TXT
	INTEGER LOST,POST			!LENGTH/POINTER OST STRING
	INTEGER WVAL				!FIELD WIDTH
	INTEGER RVAL				!REPEAT VALUE
	INTEGER VVAL				!MODIFIER VALUE
	INTEGER SVAL				!SUBJECT VALUE
	LOGICAL DOT				!. SEEN
	INTEGER MIND				!MAIN TYPE INDEX
	INTEGER SIND,SIND2			!SECONDARY INDICES
	INTEGER LSPEC(4)			!Q FORMAT
	CHARACTER*20 GCOD			!MAIN CODE LIST
	CHARACTER*6 ASCOD			!A CODE LIST
	CHARACTER*6 SSCOD			!S,U,O,X,Z,L CODE LIST
	CHARACTER*5 ESCOD			!E,F,D CODE LIST
	CHARACTER*3 DSCOD			!ANGLE CODE LIST
	CHARACTER*2 CRLF			!CR AND LF
	  DATA GCOD/'!*-+/_^%CASUOXZLEFDQ'/
C		     12345678901234567890
	  DATA ASCOD/'CDFSZL'/
C		      123456
	  DATA SSCOD/'BWLIJK'/
C		      123456
	  DATA ESCOD/'CADHP'/
C		      12345
	  DATA DSCOD/'FRD'/
C		      123
C-
C
C WNCTXI_X
C
       FF = CHAR(12)
       CR = CHAR(13)
       LF = CHAR(10)
       CRLF = CR//LF
       BACKS = CHAR(92)

	OPS=.TRUE.				!INPUT STRING
	LOST=LEN(OST)				!LENGTH
	I=WNGASL(1,ARGL(0))			!INDICATE STRING FOR CONVEX
	POST=1					!POINTER
	GOTO 10
C
C Initialize
C
 10	CONTINUE
	I=WNGASL(2,ARGL(0))			!INDICATE STRING FOR CONVEX
	LARG=ARGL(0)				!# OF ARGUMENTS
	PARG=2					!POINTER IN ARGL
	LTXT=WNCAL0(TXT)			!LENGTH TEXT
	PTXT=0					!POINTER
	LSPEC(1)=0				!Q WIDTH
	LSPEC(2)=1				!Q OFFSET
	LSPEC(3)=0				!Q , SEPARATION
	LSPEC(4)=0
C
C Scan text
C
 20	CONTINUE
C
C If end
C
 23	IF (PTXT.GE.LTXT) THEN			!NO MORE
C
	  RETURN				!READY
	END IF
C
C Find format
C
	J=INDEX(TXT(PTXT+1:LTXT),'!')		!FIND FORMAT
	IF (J.EQ.0) THEN			!NO MORE FORMAT
 21	  CONTINUE
	  PTXT=LTXT
	  GOTO 23				!FINISH
	END IF
C
C Analyze code
C
	PTXT=PTXT+J				!! PTR
	J1=PTXT-1				!SAVE POSITION !
	J2=PARG					!SAVE ARG POSITION
	WVAL=WNCAJA(TXT,LTXT,PTXT,ARGL(0),LARG,PARG) !GET WIDTH
	IF (PTXT.LT.LTXT) THEN			!CAN BE OK
	  IF (TXT(PTXT+1:PTXT+1).EQ.'$') THEN	!REAL WIDTH
	    PTXT=PTXT+1				!SKIP $
	    RVAL=WNCAJA(TXT,LTXT,PTXT,ARGL(0),LARG,PARG) !GET REPEAT
	  ELSE
	    RVAL=WVAL				!SET REPEAT VALUE
	    WVAL=0
	  END IF
	ELSE
 22	  PTXT=J1				!RESTORE FOR FORMAT ERROR
	  PARG=J2
	  GOTO 21
	END IF
	RVAL=MIN(OUTLEN,MAX(1,RVAL))		!CORRECT REPEAT FACTOR
	WVAL=MAX(-OUTLEN,MIN(OUTLEN,WVAL))	!CORRECT WIDTH
	IF (PTXT.GE.LTXT) GOTO 22		!FORMAT ERROR
	MIND=INDEX(GCOD,WNCAUP(TXT(PTXT+1:PTXT+1))) !GET CODE
	IF (MIND.EQ.0) GOTO 22			!UNKNOWN CODE
	PTXT=PTXT+1				!SKIP CODE
	IF (MIND.EQ.1 .OR.			!!
	1		MIND.EQ.3 .OR.		!-
	2		MIND.EQ.4 .OR.		!+
	3		MIND.EQ.5 .OR.		!/
	4		MIND.EQ.6 .OR.		!_
	5		MIND.EQ.7 .OR.		!^
	6		MIND.EQ.20 .OR.		!Q
	7		MIND.EQ.9) THEN		!C READY
	ELSE IF (MIND.EQ.2) THEN		!*
	  IF (PTXT.GE.LTXT) GOTO 22		!FORMAT ERROR
	  PTXT=PTXT+1
	  SIND=ICHAR(TXT(PTXT:PTXT))		!SAVE CHARACTER
	ELSE IF (MIND.EQ.8) THEN		!%
	  IF (PTXT.GE.LTXT) GOTO 22		!FORMAT ERROR
	  PTXT=PTXT+1
	  IF (WNCAUP(TXT(PTXT:PTXT)).EQ.'D') THEN !%D
	    SIND=0
	  ELSE IF (WNCAUP(TXT(PTXT:PTXT)).EQ.'T') THEN !%T
	    SIND=3
	  ELSE
	    GOTO 22				!FORMAT ERROR
	  END IF
	  IF (PTXT.LT.LTXT) THEN
	    IF (WNCAUP(TXT(PTXT+1:PTXT+1)).EQ.'F') THEN
	      SIND=SIND+1
	      PTXT=PTXT+1
	    ELSE IF (WNCAUP(TXT(PTXT+1:PTXT+1)).EQ.'N') THEN
	      SIND=SIND+2
	      PTXT=PTXT+1
	    END IF
	  END IF
	ELSE IF (MIND.EQ.10) THEN		!A
	  SIND=0
	  IF (PTXT.LT.LTXT) THEN
	    SIND=INDEX(ASCOD,WNCAUP(TXT(PTXT+1:PTXT+1)))
	    IF (SIND.NE.0) PTXT=PTXT+1
	  END IF
	ELSE IF (MIND.EQ.11 .OR.		!S
	1		MIND.EQ.12 .OR.		!U
	2		MIND.EQ.13 .OR.		!O
	3		MIND.EQ.14 .OR.		!X
	4		MIND.EQ.15 .OR.		!Z
	5		MIND.EQ.16) THEN	!L
	  SIND=0
	  IF (PTXT.LT.LTXT) THEN
	    SIND=INDEX(SSCOD,WNCAUP(TXT(PTXT+1:PTXT+1)))
	    IF (SIND.NE.0) PTXT=PTXT+1
	  END IF
	ELSE IF (MIND.EQ.17 .OR.		!E
	1		MIND.EQ.18 .OR.		!F
	2		MIND.EQ.19) THEN	!D
	  SIND=0
	  IF (PTXT.LT.LTXT) THEN
	    SIND=INDEX(ESCOD,WNCAUP(TXT(PTXT+1:PTXT+1)))
	    IF (SIND.NE.0) PTXT=PTXT+1
	    IF (SIND.GT.1) THEN
	      SIND2=0
	      IF (PTXT.LT.LTXT) THEN
		SIND2=INDEX(DSCOD,WNCAUP(TXT(PTXT+1:PTXT+1)))
		IF (SIND2.NE.0) PTXT=PTXT+1
	      END IF
	    END IF
	  END IF
	ELSE
	  GOTO 22				!PROGRAMMING ERROR
	END IF
	VVAL=WNCAJA(TXT,LTXT,PTXT,ARGL(0),LARG,PARG) !GET MODIFIER
	DOT=.FALSE.
	IF (PTXT.LT.LTXT) THEN
	  IF (TXT(PTXT+1:PTXT+1).EQ.'.') THEN
	    DOT=.TRUE.				!SET . SEEN
	    PTXT=PTXT+1
	  END IF
	END IF
	SVAL=WNCAJA(TXT,LTXT,PTXT,ARGL(0),LARG,PARG) !GET SECONDARY MODIFIER
	VVAL=MAX(0,MIN(OUTLEN,VVAL))		!LIMIT VALUES
	SVAL=MAX(0,MIN(OUTLEN,SVAL))
	IF (PTXT.LT.LTXT) THEN
	  IF (TXT(PTXT+1:PTXT+1).EQ.BACKS) PTXT=PTXT+1 !SKIP TERMINATOR
	END IF
	IF (MIND.GE.10 .AND. MIND.NE.20) THEN	!USE ARGUMENT
	  IF (PARG.GE.LARG) GOTO 22		!FORMAT ERROR
	  PARG=PARG+1				!POINT TO CORRECT ARGUMENT
	  IF (ARGL(PARG).EQ.0) GOTO 22		!FORMAT ERROR
	  IF (MIND.EQ.10 .AND. (SIND.EQ.2 .OR. SIND.EQ.3)) THEN !NEED 2 ARGS
	    IF (PARG.GE.LARG) GOTO 22		!FORMAT ERROR
	    PARG=PARG+1				!POINT TO CORRECT ARGUMENT
	    IF (ARGL(PARG).EQ.0) GOTO 22	!FORMAT ERROR
	  END IF
	END IF
C
C Do actual input
C
	GOTO (110,120,130,140,150,160,170,180,190,
	1		200,210,210,210,210,210,210,270,270,290,300) MIND
	GOTO 22					!PROGRAM ERROR
C !
 110	J=RVAL
	DO I=1,J				!SKIP !
	  CALL WNCASC(OST,POST,'!')
	END DO
 111	CONTINUE
	GOTO 23					!GET NEXT FORMAT
C *
 120	J=RVAL
	DO I=1,J				!SKIP CHARACTERS
	  CALL WNCASB(OST,POST,CHAR(SIND))
	END DO
	GOTO 111
C -
 130	IF (PARG.LE.2) GOTO 22			!FORMAT ERROR
	PARG=PARG-1				!RESET ARGUMENT
	GOTO 23					!NEXT FORMAT
C +
 140	IF (PARG.GE.LARG) GOTO 22		!FORMAT ERROR
	PARG=PARG+1				!RESET ARGUMENT
	GOTO 23					!NEXT FORMAT
C /
 150	CONTINUE
	DO I=1,RVAL
	  CALL WNCASM(OST,POST,CRLF)		!SKIP CR/LF
	  CALL WNCASM(OST,POST,CRLF)
	END DO
	GOTO 20					!NEW LINE
C _
 160	CONTINUE
	CALL WNCASB(OST,POST)			!SKIP SPACES
	GOTO 23
C ^
 170	CONTINUE
	DO I=1,RVAL
	  CALL WNCASC(OST,POST,FF)		!SKIP FF
	END DO
	GOTO 20					!NEW LINE
C %
 180	CONTINUE
	DO I=1,RVAL
	  IF (WVAL.NE.0) THEN			!SKIP DATE
	    POST=POST+WVAL
	  ELSE
	    CALL WNCAFS(OST,POST,CHLP)
	  END IF
	END DO
	GOTO 23					!NEXT FORMAT
C C
 190	CONTINUE
	POST=MAX(1,RVAL)			!SET COLUMN
	GOTO 23					!NEXT FORMAT
C A
 200	IF (SIND.EQ.0 .OR. SIND.EQ.4) THEN	!AS
	  I1=WNGASA(PARG,ARGL(0))		!STRING ADDRESS
	  I2=WNGASL(PARG,ARGL(0))		!STRING LENGTH
	ELSE IF (SIND.EQ.1) THEN		!AC
	  I1=ARGL(PARG)+4			!STRING ADDRESS
	  CALL WNGMV(4,A_B(ARGL(PARG)-A_OB),I2)	!STRING LENGTH
	ELSE IF (SIND.EQ.2 .OR. SIND.EQ.3) THEN	!AD, AF
	  I1=ARGL(PARG-1)			!STRING ADDRESS
	  CALL WNGMV(4,A_B(ARGL(PARG)-A_OB),I2)	!STRING LENGTH
	ELSE IF (SIND.EQ.5) THEN		!AZ
	  I1=ARGL(PARG)				!STRING ADDRESS
	  I2=WNCALZ(A_B(ARGL(PARG)-A_OB))	!STRING LENGTH
	ELSE IF (SIND.EQ.6) THEN		!AL
	  I1=ARGL(PARG)				!STRING ADDRESS
	  I2=VVAL				!STRING LENGTH
	ELSE
	  GOTO 22				!PROGRAM ERROR
	END IF
	DO I=1,RVAL				!REPEAT
	  IF (WVAL.EQ.0) THEN			!GET STRING
	    CALL WNCAFS(OST,POST,CHLP)
	  ELSE
	    CHLP=OST(POST:POST+WVAL-1)
	  END IF
	  CALL WNGMFS(I2,CHLP,A_B(I1-A_OB))	!SET STRING
	  IF (SIND.EQ.0 .OR. SIND.EQ.4) THEN	!AS
	    I1=I1+I2				!STRING ADDRESS
	  ELSE IF (SIND.EQ.1) THEN		!AC
	    I1=I1+I2+4				!STRING ADDRESS
	    CALL WNGMV(4,A_B(I1-4-A_OB),I2)	!STRING LENGTH
	  ELSE IF (SIND.EQ.2 .OR. SIND.EQ.3) THEN !AD, AF
	    I1=I1+I2				!STRING ADDRESS
	  ELSE IF (SIND.EQ.5) THEN		!AZ
	    I1=I1+I2+1				!STRING ADDRESS
	    I2=WNCALZ(A_B(I1-A_OB))		!STRING LENGTH
	  ELSE IF (SIND.EQ.6) THEN		!AL
	    I1=I1+I2				!STRING ADDRESS
	  END IF
	END DO
	GOTO 23					!NEXT FORMAT
C SUOXZL
 210	I1=ARGL(PARG)				!DATA POINTER
	IF (SIND.EQ.0 .OR. SIND.EQ.5) THEN	!J
	  I2=L_J/L_B				!DATA LENGTH
	ELSE IF (SIND.EQ.2 .OR. SIND.EQ.4) THEN	!I
	  I2=L_I/L_B
	ELSE IF (SIND.EQ.3 .OR. SIND.EQ.6) THEN	!K
	  I2=L_K/L_B
	ELSE					!B
	  I2=L_B/L_B
	END IF
	IF (MIND.EQ.11 .OR. MIND.EQ.12 .OR. MIND.EQ.15) THEN !SUZ
	  I3=10
	ELSE IF (MIND.EQ.13) THEN		!O
	  I3=8
	ELSE					!X
	  I3=16
	END IF
	DO I=1,RVAL				!REPEAT
	  I4=POST
	  IF (I2.EQ.4) THEN			!K
	    CALL WNCACJ(OST,POST,I3,A_B(I1-A_OB))
	  ELSE IF (I2.EQ.2) THEN		!I
	    CALL WNCACI(OST,POST,I3,A_B(I1-A_OB))
	  ELSE IF (I2.EQ.1) THEN		!B
	    CALL WNCACB(OST,POST,I3,A_B(I1-A_OB))
	  ELSE					!J
	    CALL WNCACJ(OST,POST,I3,A_B(I1-A_OB))
	  END IF
	  CALL WNCASS(OST,POST)			!SKIP SEPARATOR
	  IF (WVAL.NE.0) POST=I4+ABS(WVAL)
	  I1=I1+I2				!NEXT POINTER
	END DO
	GOTO 23					!NEXT FORMAT
C EF
 270	I1=ARGL(PARG)				!DATA ADDRESS
	IF (DOT) THEN				!SET FORMAT
	  I2=SVAL
	ELSE
	  I2=-1
	END IF
	DO I=1,RVAL				!REPEAT
	  IF (SIND.EQ.0) THEN			!NORMAL
	    CALL WNCACE(OST,POST,10,A_B(I1-A_OB))
	  ELSE IF (SIND.EQ.1) THEN		!COMPLEX
	    CALL WNCACX(OST,POST,10,A_B(I1-A_OB))
	    I1=I1+4				!NEXT POINTER
	  ELSE					!ANGLE
	    IF (SIND2.EQ.0) SIND2=2
	    CALL WNCACE(OST,POST,10,R0)
	    IF (SIND2.EQ.1) THEN		!FRACTION
	      R0=R0/360
	    ELSE IF (SIND2.EQ.2) THEN		!RADIANS
	      R0=R0/PI2
	    END IF
	    CALL WNGMV(4,R0,A_B(I1-A_OB))
	  END IF
	  I1=I1+4				!NEXT POINTER
	END DO
	GOTO 23					!NEXT FORMAT
C D
 290	I1=ARGL(PARG)				!DATA ADDRESS
	IF (DOT) THEN				!SET FORMAT
	  I2=SVAL
	ELSE
	  I2=-1
	END IF
	DO I=1,RVAL				!REPEAT
	  IF (SIND.EQ.0) THEN			!NORMAL
	    CALL WNCACD(OST,POST,10,A_B(I1-A_OB))
	  ELSE IF (SIND.EQ.1) THEN		!COMPLEX
	    CALL WNCACY(OST,POST,10,A_B(I1-A_OB))
	    I1=I1+8				!NEXT POINTER
	  ELSE					!ANGLE
	    IF (SIND2.EQ.0) SIND2=2
	    CALL WNCACD(OST,POST,10,D0)
	    IF (SIND2.EQ.1) THEN		!FRACTION
	      D0=D0/360
	    ELSE IF (SIND2.EQ.2) THEN		!RADIANS
	      D0=D0/DPI2
	    END IF
	    CALL WNGMV(8,D0,A_B(I1-A_OB))
	  END IF
	  I1=I1+8				!NEXT POINTER
	END DO
	GOTO 23					!NEXT FORMAT
C Q
 300	LSPEC(1)=WVAL				!LINE WIDTH
	LSPEC(2)=RVAL				!LINE OFFSET
	LSPEC(3)=VVAL				!, SEPARATION ON/OFF
	LSPEC(4)=SVAL				!SPARE
	GOTO 23					!NEXT FORMAT
C
C
	END
