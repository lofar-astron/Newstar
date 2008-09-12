C+ TWNF.FOR
C  WNB 890725
C
C  Revisions:
C	CMV 940117	Some more testing
C
	SUBROUTINE TWNF
C
C  Test WNF routines
C
C  Result:
C
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	EXTERNAL WNFMOU,WNFDMO
C
C  Parameters:
C
C
C  Arguments:
C
C
C  Function references:
C
	INTEGER WNFEOF				!GET EOF
C
C  Data declarations:
C
	INTEGER MCA,FCA
	INTEGER BUF(4)
	  CHARACTER*16 BUFC
	INTEGER RBUF(4)
	  CHARACTER*16 RBUFC
C
C  Equivalences:
C
	EQUIVALENCE (BUF,BUFC)
	EQUIVALENCE (RBUF,RBUFC)
C
C  Commons:
C
C-
	DATA BUFC/'ABCDEFGHIJKLMNOP'/
	DATA RBUFC/' '/

	CALL WNFOP(FCA,'A.TMP','W')
	CALL WNFWR(FCA,16,BUF,0)
C	TYPE *,'Written:   ',BUFC
	WRITE (*,'("Written:   ",A)') BUFC
C	TYPE *,'EOF after write: ',WNFEOF(FCA)
	WRITE (*,'("EOF after write: ",I6)') WNFEOF(FCA)
	CALL WNFCL(FCA)
C	TYPE *,'Open for reading'
	WRITE (*,'("Open for reading")')
	CALL WNFOP(FCA,'A.TMP','R')
C	TYPE *,'EOF after open: ',WNFEOF(FCA)
	WRITE (*,'("EOF after open: ",I6)') WNFEOF(FCA)
	CALL WNFRD(FCA,16,RBUF,0)
C	TYPE *,'Read back: ',RBUFC
	WRITE (*,'("Read back: ",A)') RBUFC
C	TYPE *,'EOF after read: ',WNFEOF(FCA)
	WRITE (*,'("EOF after read: ",I6)') WNFEOF(FCA)
	CALL WNFCL(FCA)
C	TYPE *,'Open for Update'
        WRITE (*,'("Open for Update")')
	CALL WNFOP(FCA,'A.TMP','U')
C	TYPE *,'EOF after open: ',WNFEOF(FCA)
	WRITE (*,'("EOF after open: ",I6)') WNFEOF(FCA)
	CALL WNFWR(FCA,16,BUF,0)
C	TYPE *,'Read back: ',RBUFC
	WRITE (*,'("Read back: ",A)') RBUFC
C	TYPE *,'EOF after read: ',WNFEOF(FCA)
	WRITE (*,'("EOF after read: ",I6)') WNFEOF(FCA)
C
	RETURN
C
C
	END
