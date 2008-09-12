C+ WNMITR.FOR
C  WNB 900312
C
C  Revisions:
C	WNB 930503	Make N complex into 2N real
C
	LOGICAL FUNCTION WNMITR(MAR,CHKVL,NR)
C
C  Triangularize normal equations with rank/constraint determination
C
C  Result:
C
C	WNMITR_L = WNMITR( MAR_J:O, CHKVL_R:I, NR_J:O)
C				Triangularize normal equations and determine
C				the rank NR and the constraint equations.
C				CHKVL determines the dependancy level.
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
	REAL CHKVL				!CHECK LIMIT
	INTEGER NR				!RANK OF NORMAL EQUATIONS
C
C  Entry points:
C
C
C  Function references:
C
C
C  Data declarations:
C
	INTEGER N
C-
	N=A_J(MAR+1)
	J1=A_J(MAR+5)					!INDEX NORMAL EQUATIONS
C
C REAL
C
	WNMITR=.TRUE.					!ASSUME OK
	DO I=0,N-1					!DECOMPOSE
	 IF (I.LT.A_J(MAR+3)) THEN			!STILL RANK LEFT
	  I3=J1+((2*N-I-1)*I)/2
 10	  CONTINUE
	  D0=A_D(I3+I)					!GET COLLINEARITY
	  DO I2=0,I-1
	    I4=J1+((2*N-I2-1)*I2)/2
	    D0=D0-A_D(I4+I)*A_D(I4+I)/A_D(I4+I2)
	  END DO
	  IF (D0*D0/A_D(I3+I).LE.CHKVL*CHKVL) THEN	!DEPENDANCY
	    IF (I.LT.A_J(MAR+3)-1) THEN			!RANK LEFT
	      J0=A_J(MAR+3)-1				!RANK POINTER
	      DO I2=0,I-1				!SHIFT PIVOT
		I4=J1+((2*N-I2-1)*I2)/2
		D1=A_D(I4+I)
		A_D(I4+I)=A_D(I4+J0)
		A_D(I4+J0)=D1
	      END DO
	      D1=A_D(I3+I)
	      I4=J1+((2*N-J0-1)*J0)/2
	      A_D(I3+I)=A_D(I4+J0)
	      A_D(I4+J0)=D1
	      DO I2=I+1,J0-1
		I4=J1+((2*N-I2-1)*I2)/2
		D1=A_D(I3+I2)
		A_D(I3+I2)=A_D(I4+J0)
		A_D(I4+J0)=D1
	      END DO
	      I4=J1+((2*N-J0-1)*J0)/2
	      DO I2=J0+1,N-1				!SHIFT PIVOT
		D1=A_D(I3+I2)
		A_D(I3+I2)=A_D(I4+I2)
		A_D(I4+I2)=D1
	      END DO
	      A_J(MAR+3)=A_J(MAR+3)-1			!DECREASE RANK
	      J2=A_J(MAR+4)				!PIVOT TABLE
	      I1=A_J(J2+I)				!SWITCH PIVOTS
	      A_J(J2+I)=A_J(J2+J0)
	      A_J(J2+J0)=I1
	      GOTO 10					!RETRY
	    ELSE
	      A_J(MAR+3)=I				!SET RANK
	    END IF
	  END IF
	  A_D(I3+I)=D0					!DIAGONAL
	  DO I1=I+1,N-1					!LU DECOMPOSITION
	    DO I2=0,I-1
	      I4=J1+((2*N-I2-1)*I2)/2
	      A_D(I3+I1)=A_D(I3+I1)-A_D(I4+I)*A_D(I4+I1)/A_D(I4+I2)
	    END DO
	  END DO
	 END IF
	END DO
C
C CONSTRAINTS
C
	J0=A_J(MAR+3)					!RANK
	DO I1=J0,N-1
	  DO I=J0-1,0,-1
	    I3=J1+((2*N-I-1)*I)/2
	    DO I2=I+1,J0-1
	      I4=J1+((2*N-I2-1)*I2)/2
	      A_D(I3+I1)=A_D(I3+I1)+A_D(I3+I2)*A_D(I4+I1)
	    END DO
	    A_D(I3+I1)=-A_D(I3+I1)/A_D(I3+I)
	  END DO
	END DO
C
C RANK BASIS (A=I+G1'*.G1')
C
	DO I=J0,N-1
	  I3=J1+((2*N-I-1)*I)/2
	  DO I1=I,N-1
	    A_D(I3+I1)=0
	    DO I2=0,J0-1
	      I4=J1+((2*N-I2-1)*I2)/2
	      A_D(I3+I1)=A_D(I3+I1)+A_D(I4+I)*A_D(I4+I1)
	    END DO
	  END DO
	  A_D(I3+I)=1+A_D(I3+I)
	END DO
C
C TRIANGULAR A
C
	DO I=J0,N-1
	  I3=J1+((2*N-I-1)*I)/2
	  DO I1=I,N-1
	    DO I2=J0,I-1
	      I4=J1+((2*N-I2-1)*I2)/2
	      A_D(I3+I1)=A_D(I3+I1)-A_D(I4+I)*A_D(I4+I1)/A_D(I4+I2)
	    END DO
	  END DO
	END DO
C
	NR=A_J(MAR+3)					!RANK
C
	RETURN
C
C
	END
