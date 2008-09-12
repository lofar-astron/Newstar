C+ WNCFSV.FOR
C  WNB 890417
C
C  Revisions:
C	HjV 940217	Change WNCFGC in WNCFGV
C
	SUBROUTINE WNCFSV(COD,TP,VAL)
C
C  Set/get file values
C
C  Result:
C
C	CALL WNCFSV ( COD_J:I, TP_J:I, VAL_J:I)
C				Set for the file(s) given by COD the
C				parameter TP to the value VAL. TP can be:
C					F_LL	line length
C					F_PL	page length (0= no paging)
C					F_LC	line count
C					F_PC	page count
C					F_DIS	disposition:
C						F_NO	delete file
C						F_YES	keep file
C						F_SP	spool file
C						F_CAT	concatenate to file
C	CALL WNCFGV ( COD_J:I, TP_J:I, VAL_J:O)
C				Get for the file given by COD the parameter
C				TP(see above) in VAL. If more than one file
C				specified, the last value will be given
C	CALL WNCFSN ( COD_J:I, NAM_C*:I)
C				Set the final filename for the specified files
C				in COD to the given name.
C	CALL WNCFGN ( COD_J:I, NAM_C*:O)
C				Get the file name (for last specified file)
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
	INTEGER TP				!VALUE TYPE
	INTEGER VAL				!VALUE TO SET/GET
	CHARACTER*(*) NAM			!NAME TO GET/SET
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
C
C WNCFSV
C
	L0=.TRUE.				!SET
	L1=.TRUE.				!VALUE
	GOTO 10
C
C WNCFGV
C
	ENTRY WNCFGV(COD,TP,VAL)
C
	L0=.FALSE.				!GET
	L1=.TRUE.				!VALUE
	GOTO 10
C
C WNCFSN
C
	ENTRY WNCFSN(COD,NAM)
C
	L0=.TRUE.				!SET
	L1=.FALSE.				!NAME
	GOTO 10
C
C WNCFGN
C
	ENTRY WNCFGN(COD,NAM)
C
	L0=.FALSE.				!GET
	L1=.FALSE.				!NAME
	GOTO 10
C
 10	CONTINUE
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
	    IF (L1) THEN			!VALUE
	      IF (L0) THEN			!SET
		IF (TP.EQ.F_PL) CPL(I)=VAL
		IF (TP.EQ.F_PC) CPC(I)=VAL
		IF (TP.EQ.F_LL) CLL(I)=VAL
		IF (TP.EQ.F_LC) CLC(I)=VAL
		IF (TP.EQ.F_DIS) THEN
		  IF (I.EQ.0) LOGCD=VAL		!LOG
		  CDIS(I)=VAL
		END IF
	      ELSE				!GET
		IF (TP.EQ.F_PL) VAL=CPL(I)
		IF (TP.EQ.F_PC) VAL=CPC(I)
		IF (TP.EQ.F_LL) VAL=CLL(I)
		IF (TP.EQ.F_LC) VAL=CLC(I)
		IF (TP.EQ.F_DIS) THEN
		  IF (I.EQ.0) THEN		!LOG
		    VAL=LOGCD
		  ELSE
		    VAL=CDIS(I)
		  END IF
		END IF
	      END IF
	    ELSE				!NAME
	      IF (L0) THEN			!SET
		CFFN(I)=NAM
	      ELSE				!GET
		NAM=CFFN(I)
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
