C+ WNMISN.FOR
C  WNB 900312
C
C  Revisions:
C	WNB 930503	Make N complex into 2N real
C	WNB 930506	Higher precision solution
C
	SUBROUTINE WNMISN(MAR,SOL,MU,ME)
C
C  Solve triangular normal equations with rank defects
C
C  Result:
C
C	CALL WNMISN( MAR_J:O, SOL_E(0:*,0:*):O, MU_E(0:*):O,
C			ME_E(0:*,0:*):O)
C				Solve triangular normal equations. MAR gives
C				the matrix.
C				The solution will be given in SOL, with the
C				adjustment error MU and the solution mean
C				errors in ME.
C	CALL WNMYSN( MAR_J:O, CSOL_X(0:*,0:*):O, MU_E(0:*):O,
C			CME_X(0:*,0:*):O)
C				As WNMISN but for complex solutions.
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
	INTEGER MAR				!AREA POINTER
	REAL SOL(0:*)				!SOLUTION
	REAL MU(0:*)				!ADJUSTMENT ERROR
	REAL ME(0:*)				!SOLUTION ERRORS
C
C  Entry points:
C
C
C  Function references:
C
C
C  Data declarations:
C
	INTEGER N				!# OF UNKNOWNS
	INTEGER M				!# OF KNOWNS
	INTEGER NR				!RANK
	DOUBLE PRECISION DMU,MU1		!FOR M.E.
	INTEGER I0B,I0R
C-
C
C WNMISN
C
	J=A_J(MAR)				!COMPLEX IND.
	IF (J.NE.0) RETURN			!WRONG TYPE
	GOTO 10
C
C WNMYSN
C
	ENTRY WNMYSN(MAR,SOL,MU,ME)
C
	J=A_J(MAR)				!COMPLEX IND.
	IF (J.EQ.0) RETURN			!WRONG TYPE
	GOTO 10
C
C INTRO
C
 10	CONTINUE
	N=A_J(MAR+1)				!# UNKNOWNS
	M=A_J(MAR+2)				!# KNOWNS
	NR=A_J(MAR+3)				!RANK
	J0=A_J(MAR+5)				!NORMAL EQUATIONS
	J1=A_J(MAR+6)				!KNOWN VECTOR
	J2=A_J(MAR+7)				!ERROR VECTOR
	J3=A_J(MAR+4)				!PIVOT TABLE
	J4=A_J(MAR+8)				!SOLUTION AREA
	DO I=0,M-1				!FOR ALL DATA VECTORS
	  I0B=J1+I*N				!POINTER KNOWN VECTOR
	  DO I1=0,NR-1				!ALL UNKNOWNS
	    A_D(J4+A_J(J3+I1))=A_D(I0B+A_J(J3+I1))
	    DO I2=0,I1-1
	      I3=J0+((2*N-I2-1)*I2)/2
	      A_D(J4+A_J(J3+I1))=A_D(J4+A_J(J3+I1))-
	1		A_D(I3+I1)*A_D(J4+A_J(J3+I2))/A_D(I3+I2) !STEP 1
	    END DO
	  END DO
	  DO I1=NR-1,0,-1
	    I3=J0+((2*N-I1-1)*I1)/2
	    DO I2=I1+1,NR-1
	      A_D(J4+A_J(J3+I1))=A_D(J4+A_J(J3+I1))-
	1		A_D(I3+I2)*A_D(J4+A_J(J3+I2)) !SOLUTION
	    END DO
	    A_D(J4+A_J(J3+I1))=A_D(J4+A_J(J3+I1))/A_D(I3+I1)
	  END DO
	  I0R=J2+3*I				!POINTER ERROR VECTOR
	  DMU=A_D(I0R+2)			![LL]
	  DO I1=0,NR-1
	    DMU=DMU-A_D(J4+A_J(J3+I1))*A_D(I0B+A_J(J3+I1)) !MAKE RMS
	  END DO
	  MU1=SQRT(MAX(0D0,DMU/MAX(1D0,A_D(I0R)-N)))
	  IF (A_D(I0R+1).GT.0D0) DMU=DMU/A_D(I0R+1) !PER WEIGHT
	  MU(I)=SQRT(MAX(0D0,DMU))		!RETURN ERROR PER WEIGHT
	  DO I1=0,NR-1
	    I3=J0+((2*N-I1-1)*I1)/2
	    ME(A_J(J3+I1)+I*N)=MU1/SQRT(A_D(I3+I1))
	  END DO
C
C MISSING RANK
C
	  DO I1=NR,N-1				!MAKE B2=-G1'*.X1'
	    A_D(I0B+A_J(J3+I1))=0
	    DO I2=0,NR-1
	      I3=J0+((2*N-I2-1)*I2)/2
	      A_D(I0B+A_J(J3+I1))=A_D(I0B+A_J(J3+I1))-
	1		A_D(J4+A_J(J3+I2))*A_D(I3+I1)
	    END DO
	  END DO
C
C SOLVE X2
C
	  DO I1=NR,N-1				!ALL UNKNOWNS
	    A_D(J4+A_J(J3+I1))=A_D(I0B+A_J(J3+I1))
	    DO I2=NR,I1-1
	      I3=J0+((2*N-I2-1)*I2)/2
	      A_D(J4+A_J(J3+I1))=A_D(J4+A_J(J3+I1))-
	1		A_D(I3+I1)*A_D(J4+A_J(J3+I2))/A_D(I3+I2) !STEP 1
	    END DO
	  END DO
	  DO I1=N-1,NR,-1
	    I3=J0+((2*N-I1-1)*I1)/2
	    DO I2=I1+1,N-1
	      A_D(J4+A_J(J3+I1))=A_D(J4+A_J(J3+I1))-
	1		A_D(I3+I2)*A_D(J4+A_J(J3+I2)) !SOLUTION
	    END DO
	    A_D(J4+A_J(J3+I1))=A_D(J4+A_J(J3+I1))/A_D(I3+I1)
	    ME(A_J(J3+I1)+I*N)=MU1/SQRT(A_D(I3+I1))
	  END DO
C
C FINAL X1
C
	  DO I1=0,NR-1
	    I3=J0+((2*N-I1-1)*I1)/2
	    DO I2=NR,N-1
	      A_D(J4+A_J(J3+I1))=A_D(J4+A_J(J3+I1))+
	1		A_D(J4+A_J(J3+I2))*A_D(I3+I2)
	    END DO
	  END DO
C
C ERRORS
C
	  I0R=J2+3*I				!POINTER ERROR VECTOR
	  DMU=A_D(I0R+2)			![LL]
	  DO I1=0,N-1
	    DMU=DMU-A_D(J4+A_J(J3+I1))*A_D(I0B+A_J(J3+I1)) !MAKE RMS
	  END DO
	  MU1=SQRT(MAX(0D0,DMU/MAX(1D0,A_D(I0R)-N)))
	  IF (A_D(I0R+1).GT.0D0) DMU=DMU/A_D(I0R+1) !PER WEIGHT
	  MU(I)=SQRT(MAX(0D0,DMU))		!RETURN ERROR PER WEIGHT
	  DO I1=0,N-1
	    I3=J0+((2*N-I1-1)*I1)/2
	    ME(A_J(J3+I1)+I*N)=MU1/SQRT(A_D(I3+I1))
	  END DO
	  DO I1=0,N-1				!RETURN SOLUTION
	    SOL(I1+I*N)=A_D(J4+I1)
	  END DO
	END DO
C
	RETURN
C
C
	END
