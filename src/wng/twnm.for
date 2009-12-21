C+ TWNM.FOR
C  WNB 930504
C
C  Revisions:
C       WNB 950530	Remove Y test references
C
	SUBROUTINE TWNM
C
C  Test WNM routines
C
C  Result:
C
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'STH_O_DEF'
	INCLUDE 'LSQ_O_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
C
C  Function references:
C
C
C  Data declarations:
C
	INTEGER MAR,N,N1,M,NR
    	  PARAMETER (N=3)
    	  PARAMETER (N1=14)
	  PARAMETER (M=6)
	COMPLEX CMP1
	COMPLEX CD1
	COMPLEX CCE(0:N-1,0:M-1),COB(0:M-1),CSOL(0:2*N1-1),CX(0:N-1)
	REAL CV(0:4*N-1,0:4*N-1)
	REAL SOL(0:4*N1-1),ME(0:3*N-1)
	  EQUIVALENCE (SOL,CSOL)
	REAL WT(0:M-1),MU
	REAL CEQ(0:4*N1*N1-1,0:0)
	REAL DAT(0:N1-1,0:N1-1)
	REAL AMP(0:N1-1,0:N1-1)
	REAL PH(0:N1-1,0:N1-1)
	COMPLEX AP(0:N1-1,0:N1-1)
	REAL TPOS(0:N1-1)
	INTEGER TE1,TW1,TE2,TW2
	REAL W1,W2,W22
	COMPLEX CCF(0:3*N1-1)
	DATA DAT/
	1 0,1169,1181,1090,1207,1165,1178,1072,1165,1123,1215,1139,1108,1097,
	1 4,0,1176,1115,1078,1238,1141,1115,1075,1157,1107,1135,1182,1158,
	1 3,-2,0,1176,1172,1178,1277,1147,1188,1124,1193,1140,1181,1196,
	1 10,1,3,0,1139,1159,1103,1174,1112,1138,1139,1116,1143,1124,
	1 10,5,4,0,0,1178,1151,1058,1199,1118,1201,1098,1064,1111,
	1 8,5,7,0,0,0,1235,1150,1133,1246,1168,1179,1203,1195,
	1 1,5,9,5,1,1,0,1150,1155,1122,1212,1106,1096,1199,
	1 15,7,8,6,6,3,1,0,1128,1115,1117,1108,1142,1122,
	1 16,10,10,4,5,6,0,1,0,1151,1167,1134,1115,1117,
	1 12,9,11,4,2,4,3,-2,-2,0,1114,1133,1205,1133,
	1 13,6,12,6,4,2,4,-1,0,-3,0,1177,1100,1095,
	1 19,8,15,7,8,4,5,4,3,2,6,0,1147,1096,
	1 25,18,21,14,17,13,13,12,12,12,11,10,0,1175,
	1 22,21,20,18,13,17,10,14,10,13,11,7,2,0/
	INTEGER*2 IFRT(0:STHIFR-1)
	INTEGER IFRA(0:1,0:STHIFR)
	REAL BASEL(0:STHIFR-1)
	INTEGER IRED(0:STHIFR-1)
	INTEGER NIFR
	DATA NIFR/91/
	REAL ANG(0:2,0:STHIFR-1)
	DATA ANG/STHIFR*0.,STHIFR*0.,STHIFR*0./
C
	DATA CCE/(1,0),(1,0),(1,0),(1,0),(0,-1),(2,0),
	1   (1,0),(-2,0),(0,2),
	1	(1,0),(1,0),(1,0),(1,0),(0,-1),(2,0),
	1   (1,0),(-2,0),(0,2)/
	DATA COB/(6,4),(3,8),(-15,9),(6,4),(3,8),(-15,9)/
	DATA WT/1,5,2,7,3,4/
	DATA CX/(1,1),(3,-2),(2,5)/
C-
C
C PREPARE REAL DATA INFO
C
	DO I=0,N1-1
	  DO J=I,N1-1
	    IF (DAT(J,I).GT.0) THEN
	      AMP(J,I)=LOG(DAT(J,I))
	    ELSE
	      AMP(J,I)=0
	    END IF
	    AMP(I,J)=AMP(J,I)
	    PH(J,I)=PI2*DAT(I,J)/360.
	    PH(I,J)=PH(J,I)
	    AP(J,I)=CMPLX(AMP(J,I),PH(J,I))
	    AP(I,J)=AP(J,I)
	    DAT(J,I)=(DAT(J,I)/1200)**2
	    DAT(I,J)=DAT(J,I)
	  END DO
	END DO
	DO I=0,9
	  TPOS(I)=I*144
	END DO
	TPOS(10)=1332
	TPOS(11)=1404
	TPOS(12)=2628
	TPOS(13)=2700
	I0=0
	DO I=0,N1-1
	  DO J=I+1,N1-1
	    IFRA(0,I0)=J
	    IFRA(1,I0)=I
	    IFRT(I0)=256*J+I
	    BASEL(I0)=TPOS(IFRA(0,I0))-TPOS(IFRA(1,I0))
	    I0=I0+1
	  END DO
	END DO
	DO I=0,91-2
	  DO J=0,91-2-I
	    IF (BASEL(J).GT.BASEL(J+1)) THEN
	      I0=BASEL(J)
	      BASEL(J)=BASEL(J+1)
	      BASEL(J+1)=I0
	      I0=IFRT(J)
	      IFRT(J)=IFRT(J+1)
	      IFRT(J+1)=I0
	    END IF
	  END DO
	END DO
	CALL NCARRT(NIFR,BASEL,1.,IRED,ANG)
C
C COMPLEX TEST
C
C
C NEW
C
	CALL WNCTXT(F_T,'---------- NEW COMPLEX ----------')
	CALL WNMLGA(MAR,LSQ_T_COMPLEX,N)
	DO I=0,M-1
	  CALL WNMLMN(MAR,LSQ_C_COMPLEX,CCE(0,I),WT(I),COB(I))
	END DO
	CALL WNCTXT(F_T,'!70$12Q\Known   :  !16$#D4',6,
	1   A_D(A_J(MAR+LSQ_KNOWN_J)))
	CALL WNCTXT(F_T,'!70$12Q\Pre-tri :  !16$#D4',21,
	1   A_D(A_J(MAR+LSQ_NORM_J)))
	CALL WNMLTN(MAR)
	CALL WNCTXT(F_T,'!70$12Q\Post-tri:  !16$#D4',21,
	1   A_D(A_J(MAR+LSQ_NORM_J)))
	NR=N
	CALL WNCTXT(F_T,'N= !UJ  Rank= !UJ',
	1	N,NR)
	CALL WNMLSN(MAR,CSOL,MU,ME)
	CALL WNCTXT(F_T,'!70$12Q\Solution:  !16$#EC4',N,CSOL)
	CALL WNCTXT(F_T,'!70$12Q\ME:        !16$#E4',1,MU)
	CALL WNCTXT(F_T,'!70$12Q\SD:        !16$#E4',1,ME)
	CALL WNCTXT(F_T,'!70$12Q\Should be: !16$#EC4',N,CX)
	CALL WNMLCV(MAR,CV)
	CALL WNCTXT(F_T,'!70$13Q\Covariance: !16$#E4',4*N*N,CV)
	DO I=0,2*N-1
	  SOL(I)=0
	  DO I1=0,2*N-1
	    SOL(I)=SOL(I)+CV(I*(2*N)+I1,0)*
	1		A_D(A_J(MAR+LSQ_KNOWN_J)+I1)
	  END DO
	END DO
	CALL WNCTXT(F_T,'!70$13Q\Test sol:   !16$#E4',(2*N),SOL)
	CALL WNMLFA(MAR)
C
C USE WNMLTR
C
	CALL WNCTXT(F_T,'---------- NEW COMPLEX+RANK ----------')
	CALL WNMLGA(MAR,LSQ_T_COMPLEX,N,1)
	DO I=0,M-1
	  CALL WNMLMN(MAR,LSQ_C_COMPLEX,CCE(0,I),WT(I),COB(I))
	END DO
	CALL WNMLTR(MAR,NR)
	CALL WNCTXT(F_T,'N= !UJ  Rank= !UJ',
	1	N,NR)
	CALL WNMLSN(MAR,CSOL,MU,ME)
	CALL WNCTXT(F_T,'!70$12Q\Solution:  !16$#EC4',N,CSOL)
	CALL WNCTXT(F_T,'!70$12Q\ME:        !16$#E4',1,MU)
	CALL WNCTXT(F_T,'!70$12Q\SD:        !16$#E4',1,ME)
	CALL WNCTXT(F_T,'!70$12Q\Should be: !16$#EC4',N,CX)
	CALL WNMLFA(MAR)
C
C MISSING RANK
C
	CALL WNCTXT(F_T,'---------- NEW COMPLEX-RANK ----------')
	DO I=N-1,M-1,N
	  COB(I)=COB(I-1)
	  DO I1=0,N-1
	    CCE(I1,I)=CCE(I1,I-1)
	  END DO
	END DO
	CALL WNCTXT(F_T,'!70$13Q\Equations:  !16$#EC4',N*M,CCE)
	CALL WNMLGA(MAR,LSQ_T_COMPLEX,N,1)
	DO I=0,M-1
	  CALL WNMLMN(MAR,LSQ_C_COMPLEX,CCE(0,I),WT(I),COB(I))
	END DO
	CALL WNMLTR(MAR,NR)
	CALL WNCTXT(F_T,'N= !UJ  Rank= !UJ',
	1	N,NR)
	CALL WNMLSN(MAR,CSOL,MU,ME)
	CALL WNCTXT(F_T,'!70$13Q\Solution:   !16$#EC4',N,CSOL)
	CALL WNCTXT(F_T,'!70$12Q\ME:         !16$#E4',1,MU)
	CALL WNCTXT(F_T,'!70$12Q\SD:         !16$#E4',1,ME)
	CALL WNCTXT(F_T,'!70$13Q\Should be:  !16$#EC4',N,CX)
	DO I=0,M-1
	  CD1=0
	  DO I1=0,N-1
	    CD1=CD1+CSOL(I1)*CCE(I1,I)
	  END DO
	  CALL WNCTXT(F_T,'LIN= !16$EC4   LCHECK= !16$EC4',
	1	  COB(I),CD1)
	END DO
	CALL WNMLGC(MAR,I2,CEQ)
	CALL WNCTXT(F_T,'!70$13Q\Constraint: !16$#EC4',N*I2,CEQ)
	CD1=0
	DO I3=0,I2-1
	  DO I=0,N-1
	    CD1=CD1+REAL(CSOL(I))*CEQ(2*I,0)+
	1	IMAG(CSOL(I))*CEQ(2*I+1,0)
	  END DO
	  CALL WNCTXT(F_T,'!70$13Q\Gives:    !16$EC4',CD1)
	END DO
C
C INVERT
C
	CALL WNMLCV(MAR,CV)
	CALL WNCTXT(F_T,'!70$13Q\Covariance: !16$#E4',4*N*N,CV)
	DO I=0,2*N-1
	  SOL(I)=0
	  DO I1=0,2*N-1
	    SOL(I)=SOL(I)+CV(I*(2*N)+I1,0)*
	1		A_D(A_J(MAR+LSQ_KNOWN_J)+I1)
	  END DO
	END DO
	CALL WNCTXT(F_T,'!70$13Q\Knowns:   !16$#D4',2*N,
	1	A_D(A_J(MAR+LSQ_KNOWN_J)))
	CALL WNCTXT(F_T,'!70$13Q\Test sol:   !16$#E4',(2*N),SOL)
	CALL WNMLFA(MAR)
C
C ADD CONSTRAINTS
C
	CALL WNCTXT(F_T,'---------- COMPLEX+CONSTRAINTS ----------')
	CALL WNMLGA(MAR,LSQ_T_COMPLEX+LSQ_T_CONSTRAINT,N,1,I2)
	DO I=0,M-1
	  CALL WNMLMN(MAR,LSQ_C_COMPLEX,CCE(0,I),WT(I),COB(I))
	END DO
	CALL WNMLMC(MAR,LSQ_C_REAL,CEQ)		!FILL CONSTRAINT
	CALL WNCTXT(F_T,'!70$12Q\Pre-tri :  !16$#D4',36,
	1   A_D(A_J(MAR+LSQ_NORM_J)))
	CALL WNMLTN(MAR,NR)
	CALL WNCTXT(F_T,'!70$12Q\Post-tri : !16$#D4',36,
	1   A_D(A_J(MAR+LSQ_NORM_J)))
	CALL WNCTXT(F_T,'N= !UJ  Rank= !UJ',
	1	N,NR)
	CALL WNMLSN(MAR,CSOL,MU,ME)
	CALL WNCTXT(F_T,'!70$13Q\Solution:   !16$#EC4',N,CSOL)
	CALL WNCTXT(F_T,'!70$12Q\ME:         !16$#E4',1,MU)
	CALL WNCTXT(F_T,'!70$12Q\SD:         !16$#E4',1,ME)
	CALL WNCTXT(F_T,'!70$13Q\Should be:  !16$#EC4',N,CX)
	DO I=0,M-1
	  CD1=0
	  DO I1=0,N-1
	    CD1=CD1+CSOL(I1)*CCE(I1,I)
	  END DO
	  CALL WNCTXT(F_T,'LIN= !16$EC4   LCHECK= !16$EC4',
	1	  COB(I),CD1)
	END DO
	CD1=0
	DO I3=0,I2-1
	  DO I=0,N-1
	    CD1=CD1+REAL(CSOL(I))*CEQ(2*I,0)+
	1	IMAG(CSOL(I))*CEQ(2*I+1,0)
	  END DO
	  CALL WNCTXT(F_T,'!70$13Q\Gives:    !16$EC4',CD1)
	END DO
	CALL WNMLCV(MAR,CV)
	CALL WNCTXT(F_T,'!70$13Q\Covariance: !16$#E4',(2*N)*(2*N),CV)
	DO I=0,2*N-1
	  SOL(I)=0
	  DO I1=0,2*N-1
	    SOL(I)=SOL(I)+CV(I*(2*N)+I1,0)*
	1		A_D(A_J(MAR+LSQ_KNOWN_J)+I1)
	  END DO
	END DO
	CALL WNCTXT(F_T,'!70$13Q\Test sol:   !16$#E4',(2*N),SOL)
	CALL WNMLSN(MAR,CSOL,MU,ME)
	CALL WNCTXT(F_T,'!70$13Q\Sol again:  !16$#EC4',N,CSOL)
	CALL WNCTXT(F_T,'!70$12Q\ME:         !16$#E4',1,MU)
	CALL WNCTXT(F_T,'!70$12Q\SD:         !16$#E4',1,ME)
	CALL WNMLFA(MAR)
C
C REAL DATA
C
	CALL WNCTXT(F_T,'---------- NEW REAL DATA COMPLEX ----------')
	CALL WNMLGA(MAR,LSQ_T_COMPLEX,N1,1)
	I1=0
	DO I=0,NIFR-1
	  IF (IRED(I).GT.0) THEN
	    IF (IRED(I).NE.I1) THEN
	      I1=IRED(I)
	      I4=I
	      TE1=IFRT(I)/256
	      TW1=MOD(IFRT(I),256)
	      W2=DAT(TE1,TW1)
	    ELSE
	      TE2=IFRT(I)/256
	      TW2=MOD(IFRT(I),256)
	      W22=DAT(TE2,TW2)
	      DO I2=0,N1-1
		CCF(I2)=0
	      END DO
	      CCF(TW1)=CCF(TW1)+CMPLX(1,1)	!SET COEFFICIENTS
	      CCF(TE1)=CCF(TE1)+CMPLX(1,-1)
	      CCF(TW2)=CCF(TW2)+CMPLX(-1,-1)
	      CCF(TE2)=CCF(TE2)+CMPLX(-1,+1)
	      CALL WNMLMN(MAR,LSQ_C_REAL,CCF,W2*W22/(W2+W22),
	1		CMPLX(AMP(TE1,TW1)-AMP(TW2,TE2),
	1		PH(TE1,TW1)-PH(TW2,TE2)))
	    END IF
	  END IF
	END DO
	CALL WNMLTR(MAR,NR)
	CALL WNMLSN(MAR,SOL,MU,ME)
	CALL WNCTXT(F_T,'!70$13Q\Solution:   !16$#E4',2*N1,SOL)
	CALL WNCTXT(F_T,'!70$13Q\MU:         !16$E4',MU)
	CALL WNCTXT(F_T,'!70$13Q\ME:         !16$E4',ME)
	CALL WNMLGC(MAR,I2,CEQ)
	CALL WNCTXT(F_T,'!70$13Q\N,NR,MU: !UJ, !UJ, !E',N1,I2,MU)
	CALL WNCTXT(F_T,'!70$13Q\Constraints:!16$#E4',2*I2*N1,CEQ)
	CALL WNMLFA(MAR)
C
C NON-LINEAR
C
	CALL WNCTXT(F_T,'---------- NEW REAL DATA NONLIN ----------')
	CALL WNMLGA(MAR,LSQ_T_COMPLEX,N1,1)
	DO I3=0,2
	  I1=0
	  DO I=0,NIFR-1
	    IF (IRED(I).GT.0) THEN
	      IF (IRED(I).NE.I1) THEN
		I1=IRED(I)
		I4=I
		TE1=IFRT(I)/256
		TW1=MOD(IFRT(I),256)
		W2=DAT(TE1,TW1)
	      ELSE
		TE2=IFRT(I)/256
		TW2=MOD(IFRT(I),256)
		W22=DAT(TE2,TW2)
		DO I2=0,N1-1
		  CCF(I2)=0
		END DO
		CCF(TW1)=CCF(TW1)+CMPLX(1,1) !SET COEFFICIENTS
		CCF(TE1)=CCF(TE1)+CMPLX(1,-1)
		CCF(TW2)=CCF(TW2)+CMPLX(-1,-1)
		CCF(TE2)=CCF(TE2)+CMPLX(-1,+1)
		CMP1=EXP(CMPLX(AMP(TE1,TW1),PH(TE1,TW1))-
	1	    CMPLX(AMP(TE2,TW2),PH(TE2,TW2)))
		CMP1=CMP1*EXP(-CSOL(TW1)-CONJG(CSOL(TE1))+CSOL(TW2)+
	1	    CONJG(CSOL(TE2)))
		CALL WNMLMN(MAR,LSQ_C_REAL,CCF,W22*W22/(W2+W22),
	1	    CMP1-CMPLX(1,0))
	      END IF
	    END IF
	  END DO
	  CALL WNMLNN(MAR,NR,CSOL,MU,ME)
	  CALL WNCTXT(F_T,'!70$13Q\Solution:   !16$#E4',2*N1,SOL)
	  CALL WNCTXT(F_T,'!70$13Q\MU:         !16$E4',MU)
	  CALL WNCTXT(F_T,'!70$13Q\FIT:        !16$E4',ME)
	END DO
	CALL WNMLFA(MAR)
C
	RETURN
C
C
	END