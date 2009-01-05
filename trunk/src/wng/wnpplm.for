C+ WNPPLM.FOR
C  WNB 911125
C
C  Revisions:
C
C  Polymark routine
C
	LOGICAL FUNCTION WQPOLM(N,POS)
C
C  Result:
C
C	WQPOLM_L = WQPOLM( N_J:I, POS_E(2,N):I)
C	WQPOLM_IX_L = WQPOLM_IX( N_J:I, POS_E(2,N):I, IX_J:I)
C				Draw polymarks from given list; and set
C				polymark index IX.
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'WQG_DEF'		!GENERAL AREA
	INCLUDE 'WQD_O_DEF'		!DEVICE AREA
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER N			!NUMBER OF POINTS
	REAL POS(2,*)			!X,Y POSITIONS
	INTEGER IX			!LINE INDEX
C
C  Entry points:
C
	LOGICAL WQPOLM_IX		!SET ALSO INDEX
C
C  Function references:
C
	LOGICAL WQSPMI			!SET POLYLINE INDEX
	LOGICAL WNP_ALLOC		!GET AREAS
C
C  Data declarations:
C
	INTEGER VP(2)			!ARGUMENT LIST
	REAL MDAT(18,5)			!MARKS
	  DATA MDAT/18*0.,-.5,0.,+.5,0.,0.,-.5,0.,+.5,10*0.,
	1		-.5,0.,+.5,0.,-.5,-.5,+.5,+.5,-.5,+.5,+.5,-.5,6*0.,
	2		-.5,-.25,-.5,+.25,-.25,+.5,+.25,+.5,+.5,+.25,
	3		+.5,-.25,+.25,-.5,-.25,-.5,-.5,-.25,
	2		-.5,-.5,+.5,+.5,-.5,+.5,+.5,-.5,10*0./
	INTEGER JDAT(4,5)
	  DATA JDAT/2,0,0,0,2,2,0,0,2,2,2,0,9,0,0,0,2,2,0,0/ !MARK SEGMENTS
C-
	WQPOLM=.TRUE.				!ASSUME OK
	IF (WQG_STATE.LT.3) THEN
	  E_C=5					!WRONG STATE
 11	  CONTINUE
	  WQPOLM=.FALSE.
	  RETURN
	END IF
	GOTO 10
C
C POLM_IX
C
	ENTRY WQPOLM_IX(N,POS,IX)
C
	WQPOLM_IX=.TRUE.			!ASSUME OK
	IF (WQG_STATE.LT.3) THEN
	  E_C=5					!WRONG STATE
	  GOTO 11
	END IF
	IF (.NOT.WQSPMI(IX)) GOTO 11		!SET INDEX
	GOTO 10
C
C INIT
C
 10	CONTINUE
	IF (N.LT.1) THEN
	  E_C=100				!ERROR
	  GOTO 11
	END IF
	J=300*N+32				!MULTIPLE LIST LENGTH
	IF (.NOT.WNP_ALLOC(J)) THEN		!GET AREAS
	  E_C=100
	  GOTO 11
	END IF
	CALL WNP_MAKL(N,POS,A_B(WQG_OUT1-A_OB))	!MAKE MULTIPLE LIST
	IF (IAND(1,WQG_CLIP).NE.0) THEN
	  CALL WNP_NTRG(A_B(WQG_OUT1-A_OB),
	1		A_B(WQG_OUT3-A_OB))	!NORM. TRANSFORM
	  CALL WNP_PMCLP(A_B(WQG_OUT3-A_OB),
	1		A_B(WQG_OUT2-A_OB),
	1		WQG_NTR(0,2,WQG_CTR))	!CLIP VIEW
	  IF (A_J((WQG_OUT2-A_OB)/LB_J).LE.0) GOTO 900 !NONE LEFT
	ELSE
	  CALL WNP_NTRG(A_B(WQG_OUT1-A_OB),
	1		A_B(WQG_OUT2-A_OB))	!NORM. TRANSFORM
	END IF
C
C ON ALL ACTIVE DEVICES
C
	J=WQG_QOP				!START LIST
	DO WHILE (J.NE.0)
	  J0=(J-A_OB)/LB_J			!PTR
	  IF (A_J(J0+WQD_ACT_J).NE.0 .AND.
	1		IAND(A_J(J0+WQD_TYP_J),1).NE.0 .AND.
	1		IAND(A_J(J0+WQD_TYP_J),4).EQ.0) THEN !OUTPUT DEVICE
	    CALL WNP_PMCLP(A_B(WQG_OUT2-A_OB),
	1		A_B(WQG_OUT3-A_OB),
	1		A_E(J0+WQD_NTR_E+4))	!CLIP
	    IF (A_J((WQG_OUT3-A_OB)/LB_J).LE.0) GOTO 20 !NONE LEFT
	    CALL WNP_DNTR1(A_B(WQG_OUT3-A_OB),
	1		A_B(WQG_OUT1-A_OB),J)	!DEVICE TRANSF.
	    J1=WQG_CPOLMIX-1			!CURRENT INDEX
	    IF (J1.GT.A_J(J0+WQD_NPMIX_J)) J1=0	!DEFAULT
	    J1=MAX(0,J1)			!MAKE INDEX
C
C DRAW MARKS
C
	    R0=A_E(J0+WQD_NMPMS_E)		!NOMINAL SIZE
	    IF (A_E(J0+WQD_PMIX_E+3*J1+1).NE.0) THEN !ACTUAL SIZE
	      R0=R0*A_E(J0+WQD_PMIX_E+3*J1+1)
	      R0=MAX(2.,R0)
	    END IF
	    I=1					!IN POINTER
	    I1=0				!OUT POINTER
	    I2=NINT(A_E(J0+WQD_PMIX_E+3*J1+0))	!TYPE
	    DO I0=1,A_J((WQG_OUT1-A_OB)/LB_J)	!ALL POINTS
	      I3=1				!POINTER TABLE
	      J2=1
	      DO WHILE (JDAT(I3,I2).NE.0)	!FILL
	        A_J((WQG_OUT3-A_OB)/LB_J+I1)=JDAT(I3,I2) !# OF LINE SEGMENTS
		I1=I1+1				!OUT PTR
		DO I4=1,JDAT(I3,I2)		!ALL SEGMENTS
		  A_E((WQG_OUT3-A_OB)/LB_E+I1)=
	1		A_E((WQG_OUT1-A_OB)/LB_E+I)+R0*MDAT(J2,I2) !X
		  A_E((WQG_OUT3-A_OB)/LB_E+I1+1)=
	1		A_E((WQG_OUT1-A_OB)/LB_E+I+1)+R0*MDAT(J2+1,I2) !Y
		  J2=J2+2
		  I1=I1+2
		END DO
		I3=I3+1				!NEXT PIECE
	      END DO
	      I=I+2				!NEXT INPUT
	    END DO
	    A_J((WQG_OUT3-A_OB)/LB_J+I1)=0	!EOL
	    CALL WNP_PLCLP(A_B(WQG_OUT3-A_OB),
	1		A_B(WQG_OUT1-A_OB),
	1		A_E(J0+WQD_NTR_E+8))	!CLIP
	    VP(1)=0				!INDEX
	    VP(2)=WQG_OUT1			!LIST ADDRESS
	    CALL WNPDEX(3,J,VP)			!DRAW
	  END IF
 20	  CONTINUE
	  J=A_J((J-A_OB)/LB_J)			!NEXT DEVICE
	END DO
C
 900	CONTINUE
C
	RETURN
C
C
	END