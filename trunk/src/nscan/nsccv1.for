C+ NSCCV1.FOR
C  WNB 900822
C
C  Revisions:
C	JPH 921130	DO-loop variable
C
	SUBROUTINE NSCCV1(FCA,CVT,DLEN,PDAT,DAT,MXNCHK,NCHK,CHK,TRANS)
C
C  Convert block to local format if necessary
C
C  Result:
C
C	CALL NSCCV1( FCA_J:I, CVT_J:I, DLEN_J:I, PDAT_J:I, DAT_B(DLEN),
C				MXNCHK_J:I, NCHK_J:IO,
C				CHK_J(0:*):IO, TRANS_I(0:*):I)
C					Convert data block of DLEN bytes
C					at PDAT in file FCA, using buffer
C					DAT, from VAX to local format.
C					To convert only once, a check is
C					made against a list CHK with a
C					maximum of MXNCHK entries, and
C					NCHK current entreis
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
	INTEGER FCA			!FILE POINTER
	INTEGER CVT			!CONVERSION INPUT TYPE
	INTEGER DLEN			!LENGTH DATA BLOCK
	INTEGER PDAT			!DATA BLOCK POINTER
	BYTE DAT(0:*)			!DATA BUFFER
	INTEGER MXNCHK			!MAX. LENGTH CHECK LIST
	INTEGER NCHK			!# IN CHECK LIST
	INTEGER CHK(0:*)		!CHECK LIST
	INTEGER*2 TRANS(0:*)		!TRANSLATION TABLE
C
C  Function references:
C
	LOGICAL WNFRD			!READ DATA
	LOGICAL WNFWR			!WRITE DATA
C
C  Data declarations:
C
C-
C
C TEST IF ALREADY DONE
C
	DO I=0,NCHK-1
	  IF (PDAT.EQ.CHK(I)) RETURN		!ALREADY DONE, READY
	END DO
C
C READ
C
	CHK(NCHK)=PDAT				!ADD NEW CHECK
	NCHK=NCHK+1
	IF (NCHK.GE.MXNCHK) THEN		!DELETE ONE
	  DO I=1,NCHK-1
	    CHK(I-1)=CHK(I)
	  END DO
	  NCHK=NCHK-1
	END IF
	IF (.NOT.WNFRD(FCA,DLEN,DAT,PDAT)) THEN	!READ BLOCK OF DATA
 10	  CONTINUE
	  CALL WNCTXT(F_TP,'Error converting data, continuing')
	  RETURN
	END IF
	CALL WNTTTL(DLEN,DAT,TRANS,CVT)		!TRANSLATE
	IF (.NOT.WNFWR(FCA,DLEN,DAT,PDAT)) GOTO 10 !WRITE BACK
C
	RETURN
C
C
	END
