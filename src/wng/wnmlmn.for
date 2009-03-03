C+ WNMLMN.FOR
C  WNB 950330
C
C  Revisions:
C       WNB 950621      Typo in DCOMPLEX/CCOMPLEX; combine CCOMPLEX/DCOMPLEX
C
	SUBROUTINE WNMLMN(MAR,TYPE,CE,WT,OB)
C
C  Make normal equations from condition equations
C
C  Result:
C
C	CALL WNMLMN( MAR_J:I, TYPE_J:I, CE_E(0:N-1):I, WT_E:I, OB_E(0:M-1):I)
C				Make normal equations in MAR area.
C				CE are the coefficients of the condition
C				equations, WT is the weight of the observation,
C				and OB are the observed values.
C				For complex solutions the CE and OB are complex
C				values, except for LSQ_C_REAL CE is real.
C				The TYPE indicates the type of CE:
C       			LSQ_C_REAL: N real (default) for LSQ_T_REAL
C						and 2N real for LSQ_T_COMPLEX
C       			LSQ_C_COMPLEX: N complex (default) for
C						LSQ_T_COMPLEX
C				LSQ_C_CCOMPLEX: 2N complex for 	LSQ_T_COMPLEX
C					(i.e. separate for real and imag part
C					of solution)
C       			LSQ_C_NONORM: do not do normal equations
C       			LSQ_C_NOKNOWN: do not do known part
C	CALL WNMLMC( MAR_J:I, TYPE_J:I, CE_E(0:N-1,0:NC-1):I)
C       			Set constraint equations CE 
C				(CE_E(0:2*N-1,0:NC/2-1) for complex) in area
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
	INTEGER TYPE				!TYPE
	REAL CE(0:*)				!CONDITION EQUATIONS
	REAL WT					!OBSERVING WEIGHT
	REAL OB(0:*)				!OBSERVED VALUES
C
C  Function references:
C
	INTEGER WNMLGR				!GET ROW POINTER
	INTEGER WNMLGK				!GET KNOWN COLUMN
C
C  Data declarations:
C
	INTEGER N,M				!UNKNOWNS, KNOWNS
	DOUBLE COMPLEX DCI,DCJ			!COMPLEX AID
C-
C
C
C WNMLMN
C
	N=A_J(MAR+LSQ_NUN_J)		    	!# OF UNKNOWNS
	M=A_J(MAR+LSQ_M_J)			!# OF KNOWNS
C
C PREPARE FOR CCOMPLEX
C
	IF (IAND(A_J(MAR+LSQ_BITS_J),LSQ_T_COMPLEX).NE.0) THEN !COMPLEX
	   IF (IAND(TYPE,LSQ_C_CCOMPLEX).NE.0) THEN !COMPLEX CONJUGATE
	      DO I=0,N-1,2			!MAKE DCOMPLEX DATA
		 DO I1=0,1
		    R0=CE(2*I+I1)+CE(2*I+2+I1)
		    CE(2*I+2+I1)=CE(2*I+I1)-CE(2*I+2+I1)
		    CE(2*I+I1)=R0
		 END DO
	      END DO
	   END IF
	END IF
C
C NORMAL EQUATIONS
C
	IF (IAND(TYPE,LSQ_C_NONORM).EQ.0) THEN	!MAKE NORMAL EQUATIONS
C
C COMPLEX
C
	  IF (IAND(A_J(MAR+LSQ_BITS_J),LSQ_T_COMPLEX).NE.0) THEN !COMPLEX
C
C RCOMPLEX
C
	    IF (IAND(TYPE,LSQ_C_REAL).NE.0) THEN !REAL SEPARABLE
	      DO I=0,N-1,2
		I2=WNMLGR(MAR,I)
		R0=CE(I)*WT
		R1=CE(I+1)*WT
		IF (R0.NE.0) THEN
		  DO I1=I,N-1,2			!REAL PART
		    IF (CE(I1).NE.0) THEN
		      A_D(I2+I1)=A_D(I2+I1)+CE(I1)*R0 !EQUATIONS
		    END IF
		  END DO
		END IF
		I2=WNMLGR(MAR,I+1)		!NEXT LINE
		IF (R1.NE.0) THEN
		  DO I1=I+1,N-1,2 !IMAG PART
		    IF (CE(I1).NE.0) THEN
		      A_D(I2+I1)=A_D(I2+I1)+CE(I1)*R1 !EQUATIONS
		    END IF
		  END DO
		END IF
	      END DO
C
C D/CCOMPLEX
C
	    ELSE IF (IAND(TYPE,LSQ_C_DCOMPLEX+LSQ_C_CCOMPLEX).NE.0)
	1	   THEN				!SEPARABLE COMPLEX
	      DO I=0,N-1,2
		DCI=CMPLX(CE(2*I),CE(2*I+1))*WT
		IF (DCI.NE.0) THEN
		  I2=WNMLGR(MAR,I)		!ROW POINTER
		  DO I1=I,N-1,2
		    DCJ=CMPLX(CE(2*I1),-CE(2*I1+1))
		    A_D(I2+I1)=A_D(I2+I1)+
	1		DBLE(DCI*DCJ)
		    DCJ=CMPLX(CE(2*I1+2),-CE(2*I1+3))
		    A_D(I2+I1+1)=A_D(I2+I1+1)+
	1		DIMAG(DCI*DCJ)
		  END DO
		END IF
	      END DO
	      DO I=1,N-1,2
		DCI=CMPLX(CE(2*I),CE(2*I+1))*WT
		IF (DCI.NE.0) THEN
		  I2=WNMLGR(MAR,I)		!ROW POINTER
		  DO I1=I,N-2,2
		    DCJ=CMPLX(CE(2*I1),-CE(2*I1+1))
		    A_D(I2+I1)=A_D(I2+I1)+
	1		DBLE(DCI*DCJ)
		    DCJ=CMPLX(CE(2*I1+2),-CE(2*I1+3))
		    A_D(I2+I1+1)=A_D(I2+I1+1)-
	1		DIMAG(DCI*DCJ)
		  END DO
		  DO I1=N-1,N-1
		    DCJ=CMPLX(CE(2*I1),-CE(2*I1+1))
		    A_D(I2+I1)=A_D(I2+I1)+
	1		DBLE(DCI*DCJ)
		  END DO
		END IF
	      END DO
C
C COMPLEX
C
	    ELSE				!NORMAL COMPLEX
	      DO I=0,N-1,2
		DCI=CMPLX(CE(I),CE(I+1))*WT
		IF (DCI.NE.0) THEN
		   I2=WNMLGR(MAR,I) 		!ROW POINTER
		  DO I1=I,N-1,2
		    DCJ=CMPLX(CE(I1),-CE(I1+1))
		    A_D(I2+I1)=A_D(I2+I1)+
	1		 DBLE(DCI*DCJ) 	       	!REAL EQUATIONS
		    A_D(I2+I1+1)=A_D(I2+I1+1)+
	1		DIMAG(DCI*DCJ) 	      	!IMAG. EQUATIONS
		  END DO
		  I4=WNMLGR(MAR,I+1)		!NEXT LINE ROW POINTER
		  DO I1=I+1,N-1,2 		!DUPLICATE
		    A_D(I4+I1)=A_D(I2+I1-1)
		  END DO
		  DO I1=I+2,N-1,2
		    A_D(I4+I1)=-A_D(I2+I1+1)
		  END DO
		END IF
	      END DO
	    END IF
C
C REAL
C
	  ELSE					!REAL
	    DO I=0,N-1
	      IF (CE(I).NE.0) THEN
		I2=WNMLGR(MAR,I)		!ROW POINTER
		DO I1=I,N-1
		  IF (CE(I1).NE.0) THEN
		    A_D(I2+I1)=A_D(I2+I1)+CE(I)*CE(I1)*WT !EQUATIONS
		  END IF
		END DO
	      END IF
	    END DO
	  END IF
	END IF
C
C KNOWN PART
C
	IF (IAND(TYPE,LSQ_C_NOKNOWN).EQ.0) THEN	!MAKE KNOWN EQUATIONS
C
C COMPLEX
C
	  IF (IAND(A_J(MAR+LSQ_BITS_J),LSQ_T_COMPLEX).NE.0) THEN !COMPLEX
C
C RCOMPLEX
C
	    IF (IAND(TYPE,LSQ_C_REAL).NE.0) THEN !REAL SEPARABLE
	      DO I=0,M-1
		I2=WNMLGK(MAR,I)		!KNOWN COLUMN
		DO I1=0,N-1,2
		  R0=CE(I1)*WT
		  IF (R0.NE.0) THEN
		    A_D(I2+I1)=A_D(I2+I1)+R0*OB(2*I) !REAL PART
		  END IF
		  R0=CE(I1+1)*WT
		  IF (R0.NE.0) THEN
		    A_D(I2+I1+1)=A_D(I2+I1+1)+R0*OB(2*I+1) !IMAG. PART
		  END IF
		END DO
	      END DO
C
C D/CCOMPLEX
C
	    ELSE IF (IAND(TYPE,LSQ_C_DCOMPLEX+LSQ_C_CCOMPLEX).NE.0)
	1	   THEN				!SEPARABLE COMPLEX
	      DO I=0,M-1
		I2=WNMLGK(MAR,I)		!KNOWN COLUMN
		DCI=CMPLX(OB(2*I),OB(2*I+1))*WT
		DO I1=0,N-1,2
		  DCJ=CMPLX(CE(2*I1),-CE(2*I1+1))
		  A_D(I2+I1)=A_D(I2+I1)+
	1	      DBLE(DCJ*DCI)
		  DCJ=CMPLX(CE(2*I1+2),-CE(2*I1+3))
		  A_D(I2+I1+1)=A_D(I2+I1+1)+
	1	      DIMAG(DCJ*DCI)
		END DO
	      END DO
C
C COMPLEX
C
	    ELSE				!NORMAL COMPLEX
	      DO I=0,M-1
		I2=WNMLGK(MAR,I)		!KNOWN COLUMN
		DCI=CMPLX(OB(2*I),OB(2*I+1))*WT
		DO I1=0,N-1,2
		  DCJ=CMPLX(CE(I1),-CE(I1+1))
		  A_D(I2+I1)=A_D(I2+I1)+
	1	      DBLE(DCI*DCJ)
		  A_D(I2+I1+1)=A_D(I2+I1+1)+
	1	      DIMAG(DCI*DCJ)
		END DO
	      END DO
	    END IF
C
C COMPLEX ERRORS
C
	    DO I=0,M-1
	      I2=WNMLGK(MAR,I)			!KNOWN COLUMN
	      DCI=CMPLX(OB(2*I),OB(2*I+1))
	      I4=A_J(MAR+LSQ_ERROR_J)+I*LERR__N
	      A_D(I4+LERR_N_D)=A_D(I4+LERR_N_D)+2 !CNT EQUATIONS
	      A_D(I4+LERR_W_D)=A_D(I4+LERR_W_D)+2*WT !SUM WEIGHT
	      A_D(I4+LERR_LL_D)=A_D(I4+LERR_LL_D)+
	1	  WT*DBLE(DCI*CONJG(DCI)) 	!SUM RMS
	    END DO
C
C REAL
C
	  ELSE					!REAL
	    DO I=0,M-1
	      I2=WNMLGK(MAR,I)			!KNOWN COLUMN
	      DO I1=0,N-1
		IF (CE(I1).NE.0) THEN
		  A_D(I2+I1)=A_D(I2+I1)+CE(I1)*OB(I)*WT !DATA VECTOR
		END IF
	      END DO
	      I4=A_J(MAR+LSQ_ERROR_J)+I*LERR__N
	      A_D(I4+LERR_N_D)=A_D(I4+LERR_N_D)+1 !CNT EQUATIONS
	      A_D(I4+LERR_W_D)=A_D(I4+LERR_W_D)+WT !SUM WEIGHT
	      A_D(I4+LERR_LL_D)=A_D(I4+LERR_LL_D)+
	1		WT*OB(I)*OB(I)		!SUM RMS
	    END DO
	  END IF
	END IF
C
	RETURN
C
C WNMLMC
C
	ENTRY WNMLMC(MAR,TYPE,CE)
C
C COMPLEX
C
	IF (IAND(A_J(MAR+LSQ_BITS_J),LSQ_T_COMPLEX).NE.0 .AND.
	1    IAND(TYPE,LSQ_C_REAL).EQ.0) THEN
	  DO I=0,(A_J(MAR+LSQ_N_J)-A_J(MAR+LSQ_NUN_J))/2-1 !ALL
							   !CONSTRAINTS
	    IF (IAND(TYPE,LSQ_C_DCOMPLEX+LSQ_C_CCOMPLEX).NE.0) THEN
	      I3=2*I*A_J(MAR+LSQ_NUN_J) 	!POINTER CE ROW
	      DO I1=0,A_J(MAR+LSQ_NUN_J)-1,2 	!ALL UNKNOWNS
		I2=WNMLGR(MAR,I1) 		!ROW POINTER
C
C DCOMPLEX
C
		IF (IAND(TYPE,LSQ_C_DCOMPLEX).NE.0) THEN
		  A_D(I2+I+A_J(MAR+LSQ_NUN_J))=CE(I3+I1)
		  A_D(I2+I+A_J(MAR+LSQ_NUN_J)+1)=CE(I3+I1+1)
C
C CCOMPLEX
C
		ELSE IF (IAND(TYPE,LSQ_C_CCOMPLEX).NE.0) THEN
		  A_D(I2+I+A_J(MAR+LSQ_NUN_J))=CE(I3+I1)+CE(I3+I1+2)
		  A_D(I2+I+A_J(MAR+LSQ_NUN_J)+1)=CE(I3+I1+1)+CE(I3+I1+3)
		END IF
		I2=WNMLGR(MAR,I1+1) 		!ROW POINTER
C
C DCOMPLEX
C
		IF (IAND(TYPE,LSQ_C_DCOMPLEX).NE.0) THEN
		  A_D(I2+I+A_J(MAR+LSQ_NUN_J))=-CE(I3+I1+3)
		  A_D(I2+I+A_J(MAR+LSQ_NUN_J)+1)=CE(I3+I1+2)
C
C CCOMPLEX
C
		ELSE IF (IAND(TYPE,LSQ_C_CCOMPLEX).NE.0) THEN
		  A_D(I2+I+A_J(MAR+LSQ_NUN_J))=-CE(I3+I1+1)+CE(I3+I1+3)
		  A_D(I2+I+A_J(MAR+LSQ_NUN_J)+1)=CE(I3+I1)-CE(I3+I1+2)
		END IF
	      END DO
	    ELSE
C
C COMPLEX
C
	      I3=I*A_J(MAR+LSQ_NUN_J) 		!POINTER CE ROW
	      DO I1=0,A_J(MAR+LSQ_NUN_J)-1,2 	!ALL UNKNOWNS
		I2=WNMLGR(MAR,I1) 		!ROW POINTER
		A_D(I2+I+A_J(MAR+LSQ_NUN_J))=CE(I3+I1)
		A_D(I2+I+A_J(MAR+LSQ_NUN_J)+1)=CE(I3+I1+1)
		I2=WNMLGR(MAR,I1+1) 		!ROW POINTER
		A_D(I2+I+A_J(MAR+LSQ_NUN_J))=-CE(I3+I1+1)
		A_D(I2+I+A_J(MAR+LSQ_NUN_J)+1)=CE(I3+I1)
	      END DO
	    END IF
	  END DO
	ELSE
C
C REAL
C
	  DO I=A_J(MAR+LSQ_NUN_J),A_J(MAR+LSQ_N_J)-1 !ALL CONSTRAINTS
	    I3=(I-A_J(MAR+LSQ_NUN_J))*A_J(MAR+LSQ_NUN_J) !POINTER CE ROW
	    DO I1=0,A_J(MAR+LSQ_NUN_J)-1 	!ALL UNKNOWNS
	      I2=WNMLGR(MAR,I1)			!ROW POINTER
	      A_D(I2+I)=CE(I3+I1)
	    END DO
	  END DO
	END IF
C
	RETURN
C
C
	END