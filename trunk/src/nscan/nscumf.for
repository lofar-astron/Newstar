C+ NSCUMF.FOR
C  WNB 910220
C
C  Revisions:
C
	LOGICAL FUNCTION NSCUMF(FCAOUT,TP,NAM,BUF,CMT)
C
C  Make FITS line
C
C  Result:
C
C	NSCUMF_L = NSCUMF_L( FCAOUT_J:I, TP_J:I, NAM_C*:I,
C			BUF(0:*)_B:I, CMT_C*:I)
C				Make a FITS line from data in BUF according
C				to type TP and comment in CMT
C	NSCUMS_L = NSCUMS_L( FCAOUT_J:I, TP_J:I, NAM_C*:I,
C			SBUF_C*:I, CMT_C*:I)
C				Use string in SBUF
C	NSCUMB_L = NSCUMB_L( FCAOUT_J:I, TP_J:I, IBUF_B(0:*):I, NVAL_J:I)
C				Make IEEE binary data of NVAL of type TP
C				in buffer IBUF, and write to output.
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
C
C  Parameters:
C
	INTEGER LRCLEN			!RECORD LENGTH FITS (CHANGE ALSO NSCUWB)
	  PARAMETER (LRCLEN=2880)
	INTEGER CDILEN			!CARD IMAGE LENGTH
	  PARAMETER (CDILEN=80)
	INTEGER NCDI			!# OF CARD IMAGES/RECORD
	  PARAMETER (NCDI=LRCLEN/CDILEN)
	INTEGER V_Z,V_L,V_I,V_J,V_C,V_E,V_D,V_T,
	1		V_XI,V_XJ	!CODES FOR FITS CARD LINES
	  PARAMETER (V_Z=0,V_L=1,V_I=2,V_J=3,V_C=4,V_E=5,V_D=6,
	1		V_T=7,V_XI=8,V_XJ=9)
C
C  Arguments:
C
	INTEGER FCAOUT			!FILE POINTER
	INTEGER TP			!DATA TYPE
	CHARACTER*(*) NAM		!FIELD NAME
	BYTE BUF(0:*)			!BUFFER WITH INFO
	CHARACTER*(*) SBUF		!STRING WITH INFO
	CHARACTER*(*) CMT		!COMMENTS
	BYTE IBUF(0:*)			!DATA BUFFER
	INTEGER NVAL			!# OF VALUES
C
C  Entry points:
C
	LOGICAL NSCUMS			!STRING INPUT
	LOGICAL NSCUMB			!BINARY OUTPUT
C
C  Function references:
C
	LOGICAL NSCUWB			!WRITE FITS LINE
	LOGICAL NSCUWL			!WRITE FITS DATA
	LOGICAL WNGGJ			!GET LOGICAL DATA
C
C  Data declarations:
C
	INTEGER*2 TRBUF(0:3)		!TRANSLATION BUFFER
	  DATA TRBUF/0,0,0,0/
	BYTE LBUF(0:CDILEN-1)		!LOCAL BUFFER
	  CHARACTER*(CDILEN) LBUFS
	  EQUIVALENCE (LBUF,LBUFS)
C-
C
C NSCUMF
C
	IF (TP.EQ.V_Z) THEN				!NO VALUE
	  CALL WNCTXS(LBUFS,'!-8$AS!9C/!AS',NAM,CMT)
	ELSE IF (TP.EQ.V_L) THEN			!LOGICAL
	  IF (WNGGJ(BUF)) THEN
	    CALL WNCTXS(LBUFS,'!-8$AS!9C\= !20$AS /!AS',NAM,'T',CMT)
	  ELSE
	    CALL WNCTXS(LBUFS,'!-8$AS!9C\= !20$AS /!AS',NAM,'F',CMT)
	  END IF
	ELSE IF (TP.EQ.V_I) THEN			!I
	  CALL WNCTXS(LBUFS,'!-8$AS!9C\= !20$SI /!AS',NAM,BUF,CMT)
	ELSE IF (TP.EQ.V_J) THEN			!J
	  CALL WNCTXS(LBUFS,'!-8$AS!9C\= !20$SJ /!AS',NAM,BUF,CMT)
	ELSE IF (TP.EQ.V_E) THEN			!E
	  CALL WNCTXS(LBUFS,'!-8$AS!9C\= !20$E12 /!AS',NAM,BUF,CMT)
	ELSE IF (TP.EQ.V_D) THEN			!D
	  CALL WNCTXS(LBUFS,'!-8$AS!9C\= !20$D12 /!AS',NAM,BUF,CMT)
	ELSE IF (TP.EQ.V_T) THEN			!DATE
	  CALL WNCTXS(LBUFS,'!-8$AS!9C\= ''!2$ZI/!2$ZI/!2$ZI'''//
	1		'!31C /!AS',NAM,BUF(0),BUF(2),BUF(4),CMT)
	ELSE IF (TP.EQ.V_XI) THEN			!XI
	  CALL WNCTXS(LBUFS,'!-8$AS!9C\= ''!-8$XI''!31C /!AS',NAM,BUF,CMT)
	ELSE IF (TP.EQ.V_XJ) THEN			!XJ
	  CALL WNCTXS(LBUFS,'!-8$AS!9C\= ''!-8$XJ''!31C /!AS',NAM,BUF,CMT)
	ELSE						!UNKNOWN
	  NSCUMF=.FALSE.
	  RETURN
	END IF
C
C WRITE
C
	NSCUMF=NSCUWB(FCAOUT,LBUF)
C
	RETURN
C
C NSCUMS
C
	ENTRY NSCUMS(FCAOUT,TP,NAM,SBUF,CMT)
C
	IF (TP.EQ.V_C) THEN				!CHARACTER
	  IF (LEN(SBUF).LE.8) THEN
	    CALL WNCTXS(LBUFS,'!-8$AS!9C\= ''!-8$AS!20C''!31C /!AS',
	1			NAM,SBUF,CMT)
	  ELSE
	    CALL WNCTXS(LBUFS,'!-8$AS!9C\= ''!-16$AS!28C''!31C /!AS',
	1			NAM,SBUF,CMT)
	  END IF
	  NSCUMS=NSCUWB(FCAOUT,LBUF)
	ELSE
	  NSCUMS=.FALSE.
	END IF
C
	RETURN
C
C NSCUMB
C
	ENTRY NSCUMB(FCAOUT,TP,IBUF,NVAL)
C
	IF (TP.EQ.V_C) THEN				!BYTE
	  J=NVAL*(L_B/L_B)				!BUF. LENGTH
	  J0=9						!TRANSLATION TYPE
	ELSE IF (TP.EQ.V_I) THEN			!I
	  J=NVAL*(L_I/L_B)				!BUF. LENGTH
	  J0=2						!TRANSLATION TYPE
	ELSE IF (TP.EQ.V_J) THEN			!J
	  J=NVAL*(L_J/L_B)				!BUF. LENGTH
	  J0=3						!TRANSLATION TYPE
	ELSE IF (TP.EQ.V_E) THEN			!E
	  J=NVAL*(L_E/L_B)				!BUF. LENGTH
	  J0=4						!TRANSLATION TYPE
	ELSE IF (TP.EQ.V_D) THEN			!D
	  J=NVAL*(L_D/L_B)				!BUF. LENGTH
	  J0=5						!TRANSLATION TYPE
	ELSE						!UNKNOWN
	  NSCUMB=.FALSE.
	  RETURN
	END IF
	TRBUF(0)=J0					!SET TRANSL. BUF
	TRBUF(1)=NVAL					!NUMBER
	CALL WNTTLT(J,IBUF,TRBUF,5)			!MAKE IEEE FORMAT
C
C WRITE
C
	NSCUMB=NSCUWL(FCAOUT,IBUF,J)
C
	RETURN
C
C
	END
