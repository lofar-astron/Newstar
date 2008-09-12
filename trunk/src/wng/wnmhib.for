C+ WNMHIB.FOR
C  WNB 911230
C
C  Revisions:
C	WNB 920103	Add HB7
C	WNB 920131	Add HB6
C	JPH 940224	Comments
C
C
	SUBROUTINE WNMHB0(HISBAD,TP,OTP)
C
C  Histogram handling for beams
C
C  Result:
C
C	CALL WNMHB0( HISBAD_J:O, TP_J:I, OTP_J:I)
C				Get histogram buffer, and return the control
C				area address in HISBAD for an area with length
C				OTP.
C				TP can be:
C				+1: histogram for absolute values
C				-1: histogram for values
C	CALL WNMHB9( HISBAD_J:I)
C				Release histogram buffer
C	CALL WNMHB1( HISBAD_J:I, N_J:I, BUF_E(0:N-1), NL_J:I)
C				Set BUF values in histogram for line NL
C	CALL WNMHB2( HISBAD_J:I, N_J:I)
C				Type/print histogram data on N
C	CALL WNMHB6( HISBAD_J:I, N_J:O, PBUF_J:O)
C				Return size of histo N, and a pointer to
C				the accumulated histo buffer
C	CALL WNMHB7( HISBAD_J:I, N_J:O, PBUF_J:O)
C				Return size of histo N, and a pointer to
C				the histo buffer
C
C	HISBAD layout (NOTE that it is not the same as for WNMHIS):
C		A_J(HISBAD+0)	type (J, 1,-1)
C		A_J(HISBAD+1)	current length for histo
C		A_J(HISBAD+2)	
C		A_J(HISBAD+3)	index in A_J for histogram (J)
C
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
C
C  Parameters:
C
	INTEGER MAXLIN			!NUMBER OF PRINT LINES
	  PARAMETER (MAXLIN=35)		!I.E. 10**7 * 5
C
C  Arguments:
C
	INTEGER TP			!HISTOGRAM TYPE (1)
	INTEGER HISBAD			!index of histogram header in A_J, A_E
	INTEGER OTP			!OUTPUT TYPE FOR PRINT, LENGTH AREA
	INTEGER N			!LENGTH INPUT BUFFER
	INTEGER PBUF			!HISTO BUFFER POINTER
	INTEGER NL			!LINE NUMBER INPUT BUFFER
	REAL BUF(0:*)			!INPUT BUFFER
C
C  Function references:
C
	LOGICAL WNGGVM			!GET VIRTUAL MEMORY
	INTEGER WNMEJF			!FLOOR(X)
C
C  Data declarations:
C
	CHARACTER*130 TXT(0:MAXLIN)	!PRINT LINES
	INTEGER SCAL			!DATA SCALE
C-
C
C HB0: Allocate and initialise histogram buffer
C
	HISBAD=0				!ASSUME ERROR
	IF (ABS(TP).EQ.1) THEN			!CORRECT TYPE
	  IF (WNGGVM(LB_J*4,J)) THEN		!AREA
	    J=(J-A_OB)/LB_J			!AREA POINTER
	    A_J(J)=TP				!TYPE
	    IF (WNGGVM(LB_E*(OTP+1),J1)) THEN	!BUFFER
	      A_J(J+1)=OTP			!MAX. LENGTH IN HISTO
	      A_J(J+3)=(J1-A_OB)/LB_E		!BUFFER POINTER
	      CALL WNGMVZ(LB_E*(OTP+1),A_B(J1-A_OB)) !EMPTY BUF
	      HISBAD=J				!return index to caller
	    END IF
	  END IF
	END IF
C
	RETURN
C
C RELEASE HISTOGRAM BUFFER (HB9)
C
	ENTRY WNMHB9(HISBAD)
C
	IF (HISBAD.NE.0) THEN
	  CALL WNGFVM(LB_E*(A_J(HISBAD+1)+1),
	1	A_J(HISBAD+3)*LB_J+A_OB) 	!free BUFFER
	  CALL WNGFVM(LB_J*4,HISBAD*LB_J+A_OB)	! and header 
	END IF
	HISBAD=0				!clear header pointer
C
	RETURN
C
C SET VALUES (HB1)
C
	ENTRY WNMHB1(HISBAD,N,BUF,NL)
C
	IF (HISBAD.NE.0) THEN			!CAN DO
	  J0=A_J(HISBAD+3)			!BUFFER POINTER
	  DO I1=0,N-1				!DO ALL POINTS
	    I2=MIN(ABS(I1-N/2),ABS(NL))		!DATA POINTER
	    IF (I2.LE.A_J(HISBAD+1))
	1		A_E(J0+I2)=MAX(A_E(J0+I2),ABS(BUF(I1))) !SET DATA VALUE
	  END DO
	END IF
C
	RETURN
C
C PRINT HISTOGRAM (HB2)
C
	ENTRY WNMHB2(HISBAD,N)
C
C INIT
C
	IF (IAND(N,F_P).NE.0) CALL WNCTXT(F_P,'!^') !FORMFEED
	CALL WNCTXT(N,'!2/!48CBeam histogram!2/')
C
C SCALE DATA
C
	SCAL=1					!SCALE
	IF (HISBAD.NE.0) THEN
	  I=A_J(HISBAD+1)			!LENGTH LINE
	  DO WHILE(I.GT.LEN(TXT(0))-4)
	    SCAL=SCAL*2
	    I=I/2
	  END DO
	ELSE
	  RETURN
	END IF
C
C INIT TEXT
C
	DO I=MAXLIN,0,-1			!INIT TEXT BUFFER
	  IF (I.EQ.0) THEN
	    DO I1=1,MIN(LEN(TXT(0)),A_J(HISBAD+1)/SCAL+4)
	      IF (MOD(I1-4,10).EQ.0) THEN
	        TXT(I)(I1:I1)='|'
	      ELSE
	        TXT(I)(I1:I1)='-'
	      END IF
	    END DO
	  ELSE
	    TXT(I)=' '
	  END IF
	  IF (MOD(I,5).EQ.0) THEN
	    CALL WNCTXS(TXT(I)(1:3),'!2$UJ%',I)
	  END IF
	  TXT(I)(4:4)='|'
	END DO
C
C SET DATA
C
	J0=A_J(HISBAD+3)			!BUFFER
	R0=A_E(J0+A_J(HISBAD+1))		!HIGH DATA
	DO I1=A_J(HISBAD+1),0,-1		!GET DATA
	  I2=I1/SCAL+4				!POS. IN LINE
	  IF (I2.LE.LEN(TXT(0))) THEN
	    I3=MIN(MAXLIN,NINT(A_E(J0+I1)*100.)) !LINE #
	    TXT(I3)(I2:I2)='+'
	    R0=MAX(R0,A_E(J0+I1))		!ACCUMULATE
	    I3=MIN(MAXLIN,NINT(R0*100.))	!LINE #
	    TXT(I3)(I2:I2)='*'
	  END IF
	END DO
C
C PRINT LINES
C
	DO I=MAXLIN,0,-1			!PRINT LINES
	  CALL WNCTXT(N,TXT(I))
	END DO
C
C BOTTOM LINES
C
	TXT(0)=' '				!BOTTOM LINE
	DO I=0,MIN(LEN(TXT(0))-4,A_J(HISBAD+1)/SCAL)/10
	  CALL WNCTXS(TXT(0)(10*I+1:10*I+4),'!4$UJ',10*I*SCAL)
	END DO
	CALL WNCTXT(N,TXT(0))
C
C ANNOTATION
C
	CALL WNCTXT(N,' ')
	IF (HISBAD.NE.0) THEN
	  CALL WNCTXT(N,'+=Individual!/\*=Accumulated (or both)')
	END IF
	CALL WNCTXT(N,'!1/')
C
	RETURN
C
C HB6
C
	ENTRY WNMHB6(HISBAD,N,PBUF)
C
	N=A_J(HISBAD+1)				!LENGTH BUFFER
	PBUF=A_J(HISBAD+3)			!BUFFER
	DO I=N-1,0,-1				!MAKE ACCUMULATED
	  A_E(PBUF+I)=MAX(A_E(PBUF+I),A_E(PBUF+I+1))
	END DO
C
	RETURN
C
C HB7
C
	ENTRY WNMHB7(HISBAD,N,PBUF)
C
	N=A_J(HISBAD+1)				!LENGTH BUFFER
	PBUF=A_J(HISBAD+3)			!BUFFER
C
	RETURN
C
C
	END
