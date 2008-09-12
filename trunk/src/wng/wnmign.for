C+ WNMIGN.FOR
C  WNB 900312
C
C  Revisions:
C	WNB 930503	Change N complex into 2N real
C			Remove WNMYGR
C	WNB 930506	Higher precision solution
C
	LOGICAL FUNCTION WNMIGN(MAR,N,M)
C
C  Get/free/zero normal equations area
C
C  Result:
C
C	WNMIGN_L = WNMIGN( MAR_J:O, N_J:I, M_J:I)
C				Get an area for normal equations with N
C				unknowns, and M knowns. Return a pointer in
C				MAR, and zero the equations.
C	WNMYGN_L = WNMYGN( MAR_J:O, N_J:I, M_J:I)
C				As WNMIGN but for complex solutions.
C	WNMIFN_L = WNMIFN( MAR_J:IO)
C				Free the equations area
C	WNMIZN_L = WNMIZN( MAR_J:I)
C				Zero equations area
C	WNMIZK_L = WNMIZK( MAR_J:I)
C				Zero equations area known part
C	WNMIZU_L = WNMIZU( MAR_J:I)
C				Zero equations area unknown part
C	WNMINZ_L = WNMINZ( MAR_J:I)
C				Make non-zero diagonal
C	WNMIGR_L = WNMIGR( MAR_J:I, NR_J:O, CEQ_E(0:N-1,0:NR-1,0:M-1):O)
C				Get the constraint equations CEQ and the
C				rank deficiency NR
C
C			MAR layout:
C			MAR is index to A_J to address area. At offsets:
C			0: =0 for real, =1 for complex (then N = 2N)
C			1: N
C			2: M
C			3: Rank
C			4: Index to A_J for pivot table (N)
C			5: Index to A_D for normal equations (N*(N+1)/2)
C			6: Index to A_D for known part (N*M)
C			7: Index to A_D for error part (3*M)
C			8: Index to A_D for high precision solution
C			9: Total length in bytes
C			
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
C
C  Parameters:
C
	INTEGER LNHD				!# OF ELEMENTS IN HEADER
	  PARAMETER( LNHD=10)
C
C  Arguments:
C
	INTEGER MAR				!AREA POINTER
	INTEGER N				!# OF UNKNOWNS
	INTEGER M				!# OF KNOWNS
	INTEGER NR				!RETURNED RANK
	REAL CEQ(0:*)				!CONSTRAINT EQUATIONS
C
C  Entry points:
C
	LOGICAL WNMYGN
	LOGICAL WNMIFN
	LOGICAL WNMIZN,WNMIZK,WNMIZU
	LOGICAL WNMINZ
	LOGICAL WNMIGR
C
C  Function references:
C
	LOGICAL WNGGVA			!GET VIRTUAL MEMORY ALIGNED
	LOGICAL WNGFVA			!FREE VIRTUAL MEMORY
C
C  Data declarations:
C
C-
C
C WNMIGN
C
	WNMIGN=.TRUE.				!ASSUME OK
	J=0					!REAL
	J1=N
	GOTO 10					!FILL
C
C WNMYGN
C
	ENTRY WNMYGN(MAR,N,M)
C
	WNMYGN=.TRUE.				!ASSUME OK
	J=1					!COMPLEX
	J1=2*N					!# OF UNKNOWNS
 10	CONTINUE
	J2=(LNHD+J1)*LB_J+((J1*(J1+1))/2+M*J1+3*M+J1+1)*LB_D !LENGTH AREA
	WNMIGN=WNGGVA(J2,MAR)			!GET AREA
	IF (.NOT.WNMIGN) THEN
	  MAR=0
	  WNMIGN=.FALSE.
	  RETURN
	END IF
	MAR=(MAR-A_OB)/LB_J			!ARRAY OFFSET
C
C FILL DEFAULTS
C
	A_J(MAR)=J				!REAL/COMPLEX
	A_J(MAR+1)=J1				!# UNKNOWNS
	A_J(MAR+2)=M				!# KNOWNS
	A_J(MAR+3)=J1				!RANK
	A_J(MAR+4)=MAR+LNHD			!PIVOT AREA
	A_J(MAR+5)=((A_J(MAR+4)+J1)*LB_J+LB_D-1)/LB_D !NORMAL EQUATIONS
	A_J(MAR+6)=A_J(MAR+5)+(J1*(J1+1))/2	!KNOWN AREA
	A_J(MAR+7)=A_J(MAR+6)+J1*M		!ERROR PART
	A_J(MAR+8)=A_J(MAR+7)+3*M		!SOLUTION AID AREA
	A_J(MAR+9)=J2				!AREA LENGTH
	GOTO 20					!CLEAR
C
C WNMIZN
C
	ENTRY WNMIZN(MAR)
C
	WNMIZN=.TRUE.
 20	CONTINUE
	J=A_J(MAR)				!REAL/COMPLEX
	I1=A_J(MAR+1)				!N
	I2=A_J(MAR+2)				!M
	A_J(MAR+3)=I1				!ASSUME RANK=N
	I3=A_J(MAR+4)				!PIVOT INDEX
	DO I=0,I1-1				!INIT PIVOT
	  A_J(I3+I)=I
	END DO
	I3=A_J(MAR+5)				!NORMAL EQUATIONS
	I4=(I1*(I1+1))/2-1			!LENGTH
	DO I=I3,I3+I4
	  A_D(I)=0D0
	END DO
	GOTO 30					!DO KNOWN PART
C
C WNMIZK
C
	ENTRY WNMIZK(MAR)
C
	WNMIZK=.TRUE.
	J=A_J(MAR)				!REAL/COMPLEX
	I1=A_J(MAR+1)				!N
	I2=A_J(MAR+2)				!M
 30	CONTINUE
	I3=A_J(MAR+6)				!KNOWN PART
	I4=I2*I1-1				!LENGTH
	DO I=I3,I3+I4
	  A_D(I)=0D0
	END DO
	I3=A_J(MAR+7)				!ERROR PART
	I4=3*I2-1				!LENGTH
	DO I=I3,I3+I4
	  A_D(I)=0D0
	END DO
C
	RETURN
C
C WNMIZU
C
	ENTRY WNMIZU(MAR)
C
	WNMIZU=.TRUE.
	J=A_J(MAR)				!REAL/COMPLEX
	I1=A_J(MAR+1)				!N
	I2=A_J(MAR+2)				!M
	A_J(MAR+3)=I1				!ASSUME RANK=N
	I3=A_J(MAR+4)				!PIVOT INDEX
	DO I=0,I1-1				!INIT PIVOT
	  A_J(I3+I)=I
	END DO
	I3=A_J(MAR+5)				!NORMAL EQUATIONS
	I4=(I1*(I1+1))/2-1			!LENGTH
	DO I=I3,I3+I4
	  A_D(I)=0D0
	END DO
C
	RETURN
C
C WNMIFN
C
	ENTRY WNMIFN(MAR)
C
	J2=A_J(MAR+9)				!TOTAL LENGTH
	MAR=MAR*LB_J+A_OB			!AREA ADDRESS
	WNMIFN=WNGFVA(J2,MAR)			!FREE AREA
	MAR=0					!READY
C
	RETURN
C
C WNMINZ
C
	ENTRY WNMINZ(MAR,N)
C
	WNMINZ=.TRUE.				!ALWAYS OK
	J=A_J(MAR)				!REAL/COMPLEX
	I1=A_J(MAR+1)				!N
	I3=A_J(MAR+5)				!NORMAL EQUATIONS
	DO I=0,I1-1
	  I2=I3+((2*I1-I+1)*I)/2
	  IF (A_D(I2).LE.0D0) A_D(I2)=1D0
	END DO
C
	RETURN
C
C WNMIGR
C
	ENTRY WNMIGR(MAR,NR,CEQ)
C
	WNMIGR=.TRUE.				!ASSUME OK
	J=A_J(MAR)				!REAL/COMPLEX
	J1=A_J(MAR+1)				!N
	J2=A_J(MAR+2)				!M
	J3=A_J(MAR+3)				!RANK
	J4=A_J(MAR+5)				!NORMAL EQUATIONS
	J5=A_J(MAR+4)				!PIVOTS
	DO I2=0,J2-1				!ALL KNOWNS
	  DO I=J3,J1-1				!ALL EQUATIONS
	    I4=(I-J3)*J1+I2*J1*J1		!POINTER OUTPUT
	    R0=1				!NORMALISATION
	    DO I1=0,J3-1
	      I3=J4+((2*J1-I1-1)*I1)/2		!POINTER INPUT
	      CEQ(I4+A_J(J5+I1))=A_D(I3+I)	!COPY CONSTRAINT
	      R1=ABS(CEQ(I4+A_J(J5+I1)))
	      IF (R1.GT.1E-6) R0=MIN(R0,R1)	!NORMALISATION
	    END DO
	    DO I1=J3,J1-1			!FINAL VALUES
	      CEQ(I4+A_J(J5+I1))=0
	    END DO
	    CEQ(I4+A_J(J5+I))=1			!UNIT EXTEND
	    DO I1=0,I				!NORMALISE
	      CEQ(I4+A_J(J5+I1))=CEQ(I4+A_J(J5+I1))/R0
	    END DO
	  END DO
	END DO
	NR=J3-J1				!RETURN RANK
C
	RETURN
C
C
	END
