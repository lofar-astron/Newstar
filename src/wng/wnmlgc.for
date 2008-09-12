C+ WNMLGC.FOR
C  WNB 950330
C
C  Revisions:
C
	SUBROUTINE WNMLGC(MAR,NR,CEQ)
C
C  Get constraint equations
C
C  Result:
C
C	CALL WNMLGC( MAR_J:I, NR_J:O, CEQ_E(0:N-1,0:NR-1):O)
C				Get the constraint equations CEQ and the
C				rank deficiency NR
C			Note: For complex solutions the rank deficiency
C       		      can be a maximum of 2N, and the returned
C			      equations will be:
C				CEQ_E(0:2N-1,0:NR-1):O
C
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'LSQ_O_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER MAR				!AREA POINTER
	INTEGER NR				!RETURNED RANK DEFICIENCY
	REAL CEQ(0:*)				!CONSTRAINT EQUATIONS
C
C  Function references:
C
	INTEGER WNMLGR				!GET ROW POINTER
C
C  Data declarations:
C
	INTEGER NUN				!# UNKNOWNS
	INTEGER PIVV				!PIVOT VECTOR
C-
	NUN=A_J(MAR+LSQ_NUN_J)			!N UNKNOWN
	NR=A_J(MAR+LSQ_R_J)			!RANK
	PIVV=A_J(MAR+LSQ_PIV_J)			!PIVOTS
	DO I=NR,NUN-1				!ALL EQUATIONS
	  I4=(I-NR)*NUN				!POINTER OUTPUT
	  R0=1					!NORMALISATION
	  DO I1=0,NR-1
	    I3=WNMLGR(MAR,I1)			!POINTER INPUT
	    CEQ(I4+A_J(PIVV+I1))=A_D(I3+I) 	!COPY CONSTRAINT
	    R1=ABS(CEQ(I4+A_J(PIVV+I1)))
	    IF (R1.GT.1E-6) R0=MIN(R0,R1) 	!NORMALISATION
	  END DO
	  DO I1=NR,NUN-1			!FINAL VALUES
	    CEQ(I4+A_J(PIVV+I1))=0
	  END DO
	  CEQ(I4+A_J(PIVV+I))=1			!UNIT EXTEND
	  DO I1=0,I				!NORMALISE
	    CEQ(I4+A_J(PIVV+I1))=CEQ(I4+A_J(PIVV+I1))/R0
	  END DO
	END DO
	NR=NUN-NR				!RETURN RANK DEFICIENCY
C
	RETURN
C
C
	END
