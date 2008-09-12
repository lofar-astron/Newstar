C+ WNFFNM.FOR
C  WNB 890202
C
C  Revisions:
C	WNB 911118	DW DATA statement problems
C	WNB 920902	SUN station compiler bug
C
	CHARACTER*(*) FUNCTION WNFFNM(PRE,POST)
C
C  Get a unique file name
C
C  Result:
C
C	WNFFNM_C* = WNFFNM ( PRE_C*:I, POST_C*:I)
C				Generate a unique file name starting with
C				first 3 char. of PRE, and ending in
C				.<first 3 char. of POST>.
C				The minimum length of WNFFNM is 16, the
C				normal length 20.
C				Format: PREyymmddhhmmssC.POS with C a letter.
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
	CHARACTER*(*) PRE			!NAME PREFIX
	CHARACTER*(*) POST			!NAME EXTENSION
C
C  Function references:
C
	INTEGER WNCALN				!STRING LENGTH
C
C  Data declarations:
C
	CHARACTER*23 FIELD			!SYSTEM DATE/TIME
	CHARACTER*36 MON			!MONTHS
	CHARACTER*24 MONN
	  DATA MON/'JanFebMarAprMayJunJulAugSepOctNovDec'/
	  DATA MONN/'010203040506070809101112'/
	CHARACTER*20 LFLD			!LOCAL RESULT
C-
	CALL WNGSYT(FIELD)			!GET DATE/TIME
	J=INDEX(MON,FIELD(4:6))/3		!MONTH NUMBER
	LFLD='ZZZ'				!DUMMY PREFIX
	LFLD(1:MIN(WNCALN(PRE),3))=PRE
	LFLD=LFLD(1:3)//FIELD(10:11)//MONN(2*J+1:2*J+2)//
	1		FIELD(1:2)//FIELD(13:14)//FIELD(16:17)//
	2		FIELD(19:20)//'@.'//
	3		POST(1:MIN(WNCALN(POST),3)) !GET A NAME
	DO WHILE (LFLD(16:16).NE.'Z')
	  LFLD(16:16)=CHAR(ICHAR(LFLD(16:16))+1) !TRY NEXT
	  INQUIRE (FILE=LFLD,EXIST=L0,ERR=10)
	  IF (.NOT.L0) GOTO 20			!READY
 10	  CONTINUE
	END DO
 20	CONTINUE
	WNFFNM=LFLD				!RESULT
C
	RETURN
C
C
	END
