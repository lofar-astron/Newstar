C+ WNMFTC.FOR
C  WNB 910318
C
C  Revisions:
C	WNB 921216	Make FUN
C	WNB 930818	Make FOR
C
	SUBROUTINE WNMFTC(N,A,W)
C
C  Do Complex FFT
C
C  Result:
C
C	CALL WNMFTC( N_J:I, A_X(0:N-1):IO, W_X(0:N/2-1):I)
C				Calculates the fast Fourier transform of
C				N (power of 2) complex points. A is the
C				input/output array of points, starting at
C				coordinate 0. W are the complex weights.
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
	INTEGER N
	COMPLEX A(0:*)
	COMPLEX W(0:*)
C
C  Function references:
C
C
C  Data declarations:
C
	INTEGER P
	COMPLEX C8,C10
C-
C
C BIT REVERSAL
C
	J0=0					!J
	J4=N-2					!N-2
	J3=N/2					!N/2
	J2=0
	DO WHILE (J2.LE.J4)			!I=0(1)N-2
	  IF (J2.LT.J0) THEN			!I>=J
	    C8=A(J0)				!INTERCHANGE A(I),A(J)
	    A(J0)=A(J2)
	    A(J2)=C8
	  END IF
	  J5=J3					!K
	  DO WHILE (J5.LE.J0)			!K>J
	    J0=J0-J5				!J=J-K
	    J5=J5/2				!K=K/2
	  END DO
	  J0=J0+J5				!J=J+K
	  J2=J2+1
	END DO
C
C INVERT
C
	P=N/2					!P=N/2
	J0=1					!M=1
	J1=P-1					!N/2-1
	DO WHILE (P.NE.0)
	  DO J2=0,J1,J0				!K=0(M)N/2-1
	    J3=2*J2				!KA=K
	    J4=J3+J0				!KM=KA+M
	    DO J5=0,J1,P			!L=0(P)N/2-1
	      C10=W(J5)*A(J4)			!T=W(L)*A(KM)
	      A(J4)=A(J3)-C10			!A(KM)=A(K)-T
	      A(J3)=A(J3)+C10			!A(KA)=A(KA)+T
	      J3=J3+1				!KA=KA+1
	      J4=J4+1				!KM=KM+1
	    END DO
	  END DO
	  J0=2*J0				!M=2*M
	  P=P/2					!P=P/2
	END DO
C
	RETURN
C
C
	END
