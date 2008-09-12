C+ WNMIMC.FOR
C  WNB 900312
C
C  Revisions:
C	WNB 930503	Convert N complex to 2N real
C	WNB 930506	Add XMC, XMU, XMK
C
	SUBROUTINE WNMIMC(MAR,CE,WT,OB)
C
C  Make normal equations from condition equations
C
C  Result:
C
C	CALL WNMIMC( MAR_J:I, CE_E(0:N-1):I, WT_E:I, OB_E(0:M-1):I)
C				Make normal equations in MAR area.
C				CE are the coefficients of the condition
C				equations, WT is the weight of the observation,
C				and OB are the observed values.
C	CALL WNMYMC( MAR_J:I, CCE_X(0:N-1):I, WT_E:I, COB_X(0:M-1):I)
C				Complex
C	CALL WNMXMC( MAR_J:I, CCE_X(0:N-1):I, WT_E:I, COB_X(0:M-1):I)
C				Complex, but separate equations real/complex
C	CALL WNMIMU( MAR_J:I, CE_E(0:N-1):I, WT_E:I, OB_E(0:M-1):I)
C				Make normal equations in MAR area,
C				only the unknown part.
C				CE are the coefficients of the condition
C				equations, WT is the weight of the observation,
C				and OB are the observed values.
C	CALL WNMYMU( MAR_J:I, CCE_X(0:N-1):I, WT_E:I, COB_X(0:M-1):I)
C				Complex
C	CALL WNMXMU( MAR_J:I, CCE_X(0:N-1):I, WT_E:I, COB_X(0:M-1):I)
C				Complex, but separate equations real/complex
C	CALL WNMIMK( MAR_J:I, CE_E(0:N-1):I, WT_E:I, OB_E(0:M-1):I)
C				Make normal equations in MAR area,
C				but only known part.
C				CE are the coefficients of the condition
C				equations, WT is the weight of the observation,
C				and OB are the observed values.
C	CALL WNMYMK( MAR_J:I, CCE_X(0:N-1):I, WT_E:I, COB_X(0:M-1):I)
C				Complex
C	CALL WNMXMK( MAR_J:I, CCE_X(0:N-1):I, WT_E:I, COB_X(0:M-1):I)
C				Complex, but separate equations real/complex
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
	REAL CE(0:*)				!CONDITION EQUATIONS
	REAL WT					!OBSERVING WEIGHT
	REAL OB(0:*)				!OBSERVED VALUES
	COMPLEX COB(0:*)
C
C  Entry points:
C
C
C  Function references:
C
C
C  Data declarations:
C
	INTEGER N,M				!UNKNOWNS, KNOWNS
	LOGICAL DODAT				!DO DATA AS WELL
C-
C
C EQUATIONS AND DATA
C
	DODAT=.TRUE.
	GOTO 10
C
C ONLY EQUATIONS
C
	ENTRY WNMIMU(MAR,CE,WT,OB)
C
	DODAT=.FALSE.
C
 10	CONTINUE
	N=A_J(MAR+1)					!# OF UNKNOWNS
	M=A_J(MAR+2)					!# OF KNOWNS
	I3=A_J(MAR+5)					!INDEX NORMAL EQUATIONS
	DO I=0,N-1
	  IF (CE(I).NE.0) THEN
	    I2=I3+((2*N-I-1)*I)/2
	    DO I1=I,N-1
	      IF (CE(I1).NE.0) THEN
	        A_D(I2+I1)=A_D(I2+I1)+CE(I)*CE(I1)*WT	!EQUATIONS
	      END IF
	    END DO
	  END IF
	END DO
C
	IF (.NOT.DODAT) RETURN			!READY
	GOTO 11
C
C DATA
C
	ENTRY WNMIMK(MAR,CE,WT,OB)
C
	N=A_J(MAR+1)					!# OF UNKNOWNS
	M=A_J(MAR+2)					!# OF KNOWNS
 11	CONTINUE
	I3=A_J(MAR+6)					!KNOWN VECTOR
	DO I=0,M-1
	  I2=I3+I*N
	  DO I1=0,N-1
	    IF (CE(I1).NE.0) THEN
	      A_D(I2+I1)=A_D(I2+I1)+CE(I1)*OB(I)*WT !DATA VECTOR
	    END IF
	  END DO
	  I4=A_J(MAR+7)+I*3
	  A_D(I4)=A_D(I4)+1				!CNT EQUATIONS
	  A_D(I4+1)=A_D(I4+1)+WT			!SUM WEIGHT
	  A_D(I4+2)=A_D(I4+2)+WT*OB(I)*OB(I)		!SUM RMS
	END DO
C
	RETURN
C
C COMPLEX
C
C
C WNMYMC
C
	ENTRY WNMYMC(MAR,CE,WT,COB)
C
C EQUATIONS AND DATA
C
	DODAT=.TRUE.
	GOTO 100
C
C ONLY EQUATIONS
C
	ENTRY WNMYMU(MAR,CE,WT,COB)
C
	DODAT=.FALSE.
C
 100	CONTINUE
 	N=A_J(MAR+1)					!# OF UNKNOWNS
	M=A_J(MAR+2)					!# OF KNOWNS
	I3=A_J(MAR+5)					!INDEX NORMAL EQUATIONS
	DO I=0,N-1
	  I2=I3+((2*N-I-1)*I)/2
	  IF (MOD(I,2).EQ.0) THEN			!REAL PART
	    DO I1=I,N-1
	      IF (MOD(I1,2).EQ.0) THEN			!REAL PART
	      	R0=CE(I)*CE(I1)
		R1=CE(I+1)*CE(I1+1)
	      ELSE					!IMAG. PART
		R0=-CE(I)*CE(I1)
		R1=CE(I+1)*CE(I1-1)
	      END IF
	      IF (R0.NE.0) THEN
	        A_D(I2+I1)=A_D(I2+I1)+R0*WT		!EQUATIONS
	      END IF
	      IF (R1.NE.0) THEN
	        A_D(I2+I1)=A_D(I2+I1)+R1*WT		!EQUATIONS
	      END IF
	    END DO
	  ELSE						!IMAG. PART
	    DO I1=I,N-1
	      IF (MOD(I1,2).EQ.0) THEN			!REAL PART
	        R0=-CE(I)*CE(I1)
		R1=CE(I-1)*CE(I1+1)
	      ELSE					!IMAG. PART
		R0=CE(I)*CE(I1)
		R1=CE(I-1)*CE(I1-1)
	      END IF
	      IF (R0.NE.0) THEN
	        A_D(I2+I1)=A_D(I2+I1)+R0*WT		!EQUATIONS
	      END IF
	      IF (R1.NE.0) THEN
	        A_D(I2+I1)=A_D(I2+I1)+R1*WT		!EQUATIONS
	      END IF
	    END DO
	  END IF
	END DO
C
	IF (.NOT.DODAT) RETURN				!READY
	GOTO 110
C
C DATA
C
	ENTRY WNMYMK(MAR,CE,WT,COB)
C
	N=A_J(MAR+1)					!# OF UNKNOWNS
	M=A_J(MAR+2)					!# OF KNOWNS
 110	CONTINUE
	I3=A_J(MAR+6)					!KNOWN VECTOR
	DO I=0,M-1
	  I2=I3+I*N
	  DO I1=0,N-1
	    IF (MOD(I1,2).EQ.0) THEN			!REAL PART
	      R0=CE(I1)
	      R1=CE(I1+1)
	    ELSE					!IMAG. PART
	      R0=-CE(I1)
	      R1=CE(I1-1)
	    END IF
	    IF (R0.NE.0) THEN
	      A_D(I2+I1)=A_D(I2+I1)+R0*REAL(COB(I))*WT
	    END IF
	    IF (R1.NE.0) THEN
	      A_D(I2+I1)=A_D(I2+I1)+R1*AIMAG(COB(I))*WT
	    END IF
	  END DO
	  I4=A_J(MAR+7)+I*3
	  A_D(I4)=A_D(I4)+2				!CNT EQUATIONS
	  A_D(I4+1)=A_D(I4+1)+2*WT			!SUM WEIGHT
	  A_D(I4+2)=A_D(I4+2)+WT*COB(I)*CONJG(COB(I))	!SUM RMS
	END DO
C
	RETURN
C
C SEPARABLE COMPLEX
C
C
C WNMXMC
C
	ENTRY WNMXMC(MAR,CE,WT,COB)
C
C EQUATIONS AND DATA
C
	DODAT=.TRUE.
	GOTO 200
C
C ONLY EQUATIONS
C
	ENTRY WNMXMU(MAR,CE,WT,COB)
C
	DODAT=.FALSE.
C
 200	CONTINUE
 	N=A_J(MAR+1)					!# OF UNKNOWNS
	M=A_J(MAR+2)					!# OF KNOWNS
	I3=A_J(MAR+5)					!INDEX NORMAL EQUATIONS
	DO I=0,N-1
	  I2=I3+((2*N-I-1)*I)/2
	  IF (MOD(I,2).EQ.0) THEN			!REAL PART
	    DO I1=I,N-2,2
	      R0=CE(I)*CE(I1)
	      IF (R0.NE.0) THEN
	        A_D(I2+I1)=A_D(I2+I1)+R0*WT		!EQUATIONS
	      END IF
	    END DO
	  ELSE						!IMAG. PART
	    DO I1=I,N-1,2
	      R0=CE(I)*CE(I1)
	      IF (R0.NE.0) THEN
	        A_D(I2+I1)=A_D(I2+I1)+R0*WT		!EQUATIONS
	      END IF
	    END DO
	  END IF
	END DO
C
	IF (.NOT.DODAT) RETURN				!READY
	GOTO 210
C
C DATA
C
	ENTRY WNMXMK(MAR,CE,WT,COB)
C
	N=A_J(MAR+1)					!# OF UNKNOWNS
	M=A_J(MAR+2)					!# OF KNOWNS
 210	CONTINUE
	I3=A_J(MAR+6)					!KNOWN VECTOR
	DO I=0,M-1
	  I2=I3+I*N
	  DO I1=0,N-1
	    R0=CE(I1)
	    IF (R0.NE.0) THEN
	      IF (MOD(I1,2).EQ.0) THEN			!REAL PART
	        A_D(I2+I1)=A_D(I2+I1)+R0*REAL(COB(I))*WT
	      ELSE					!IMAG. PART
	        A_D(I2+I1)=A_D(I2+I1)+R0*AIMAG(COB(I))*WT
	      END IF
	    END IF
	  END DO
	  I4=A_J(MAR+7)+I*3
	  A_D(I4)=A_D(I4)+2				!CNT EQUATIONS
	  A_D(I4+1)=A_D(I4+1)+2*WT			!SUM WEIGHT
	  A_D(I4+2)=A_D(I4+2)+WT*COB(I)*CONJG(COB(I))	!SUM RMS
	END DO
C
	RETURN
C
C
	END
