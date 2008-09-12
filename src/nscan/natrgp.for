C+ NATRGP.FOR
C  WNB 920501
C
C  Revisions:
C     RPN 17/11/90
C
	SUBROUTINE NATRGP(FCA,FCAPT,GRPHDR,I_GRPHDR,GRPPTR,BUFPTR,BUFFER,
	1			PCOUNT,U,V,W,BASEL,
	1			UT,FLAG,BIN,IFNO,SRCNO)
C
C  Read header data (based on GETPARM)
C
C  Result:
C
C	CALL NATRGP( FCA_J:I, FCAPT_J:IO, GRPHDR_E(640):I,
C				I_GRPHDR_J(640):I, GRPPTR_J:IO, BUFPTR_J:IO,
C				BUFFER_E(20,32):IO, 
C				PCOUNT_J:O, U_E:O, V_E:O, W_E:O,
C				BASEL_J:O, UT_E:O, FLAG_J:O, BIN_J:O,
C				IFNO_J:O, SRCNO_J:O)
C				Read general header data
C				and check for legality.
C				If legal data is not found, then the data
C				is skipped until some legal data are found,
C				and then the new buffer and bufptr are returned.
C
C				E_C is 0 on exit for immediate success, -2 if
C				success was achieved after skipping data, -1
C				for a total lack of success
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'RPF_DEF'	!RPFITS DATA
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER FCA		!FILE CONTROL AREA
	INTEGER FCAPT		!FILE POINTER
	REAL GRPHDR(640)
	INTEGER I_GRPHDR(640)
	INTEGER GRPPTR
	INTEGER BUFPTR
	REAL BUFFER(20,32)	!CARD BUFFER
	INTEGER PCOUNT
	REAL U,V,W
	INTEGER BASEL
	REAL UT
	INTEGER FLAG
	INTEGER BIN
	INTEGER IFNO
	INTEGER SRCNO
C
C  Entry points:
C
C
C  Function references:
C
	LOGICAL NATXIB		!CHECK BASELINE
	INTEGER NATXCJ		!MAKE J
	REAL NATXCE		!MAKE E
	LOGICAL NATXST		!SKIP DATA
C
C  Data declarations:
C
	REAL RBASE		!BASELINE
C-
C
C First 5 parameters are always there (you hope!)
C
	U=NATXCE(GRPHDR(GRPPTR))
	V=NATXCE(GRPHDR(GRPPTR+1))
	W=NATXCE(GRPHDR(GRPPTR+2))
	RBASE=NATXCE(GRPHDR(GRPPTR+3))
	BASEL=NINT(RBASE)
	UT=NATXCE(GRPHDR(GRPPTR+4))
C
C Now look for syscal parameters
C
	IF (BASEL.EQ.-1) THEN
	  SC_UT=UT
	  SC_ANT=NATXCJ(I_GRPHDR(GRPPTR+5))
	  SC_IF=NATXCJ(I_GRPHDR(GRPPTR+6))
	  SC_Q=NATXCJ(I_GRPHDR(GRPPTR+7))
	  SC_SRCNO=NATXCJ(I_GRPHDR(GRPPTR+8))
C
C Else pick up remaining parameters
C
	ELSE IF (PCOUNT.GT.5) THEN
	  FLAG=NATXCJ(I_GRPHDR(GRPPTR+5))
	  BIN=NATXCJ(I_GRPHDR(GRPPTR+6))
	  IFNO=NATXCJ(I_GRPHDR(GRPPTR+7))
	  SRCNO=NATXCJ(I_GRPHDR( GRPPTR+8))
	END IF
C
C Check for illegal params.
C
	IF (NATXIB(RBASE,IFNO,UT,U,V,W) )THEN
C
C This can be caused by a bad block, so look for more data
C
	  CALL WNCTXT(F_TP,' Illegal data (or end of scan on older data)')
	  E_C=NATXST(FCA,FCAPT,BUFPTR,BUFFER)
C
	  RETURN
	END IF 
	E_C=0
C
	RETURN
C
C
	END
