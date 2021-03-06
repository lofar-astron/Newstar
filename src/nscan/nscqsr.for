C+ NSCQSR.FOR
C  WNB 940803
C
C  Revisions:
C	WNB 940812	Add POUT
C
	LOGICAL FUNCTION NSCQSR(QUA,FCA,AX1,AX2,
	1			CAP,CDAP,PWGT,PDAT,PMOD,POUT)
C
C  Get Qube scan
C
C  Result:
C
C	NSCQSR_L = NSCQSR( QUA_J:I, FCA_J:I, AX1_J:I, AX2_J:I,
C				CAP_J:I, CDAP_J:I,
C				PWGT_J:O, PDAT_J:O, PMOD_J:O, POUT_J:O)
C			Read a Qube scan at positions AX1, AX2, using
C			the apply/de-apply bits in CAP/CDAP.
C			The PWGT and PDAT describe the (ptr to) data
C			PMOD the ptr to the model data, POUT to ifr errors
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'CBITS_DEF'             !BIT DEFINITIONS
	INCLUDE 'QUB_O_DEF'		!QUBE DEFINITION
	INCLUDE 'STH_O_DEF'		!SCAN SET HEADER
	INCLUDE 'SCH_O_DEF'		!SCAN HEADER
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER QUA			!QUBE CONTROL AREA
	INTEGER FCA			!FILE
	INTEGER AX1,AX2			!AXES TO READ
	INTEGER CAP,CDAP		!APPLY/DE-APPLY BITS
	INTEGER PWGT			!DATA WEIGHT PTR (E(0:3,*))
	INTEGER PDAT			!DATA PTR (X(0:3,*))
	INTEGER PMOD			!MODEL PTR (X(0:3,*))
	INTEGER POUT			!OUTPUT PTR (X(0:3,*))
C
C  Function references:
C
	LOGICAL WNFRD			!READ DATA
	LOGICAL WNFWR			!WRITE DATA
	LOGICAL NSCSCR			!READ A SCAN
	LOGICAL NSCQR0			!FIND SET HEADER
C
C  Data declarations:
C
	INTEGER QUAD,QUAE		!DATA POINTERS
	LOGICAL SORT			!SORT FIRST
	INTEGER SCNP			!START SCAN POINTER
	INTEGER LAX1,LAX2,LAX3		!LENGTH AXIS 1,2,3
	INTEGER LPAX2			!LOOP AXIS 2
	INTEGER OFFAX2			!DISK OFFSET AXIS 2
	INTEGER L4DPL			!LENGTH ONE DATA POINT
	INTEGER L4DO,L4MO		!DATA, MODEL POINT OFFSET
	INTEGER SCNT			!SORT LINES PER PASS
	INTEGER NPASS			!# OF TRANSPOSE PASSES
	INTEGER BLINE			!START LINE FOR SORT
	INTEGER LLINE			!SORT PASS LENGTH
	INTEGER BOFF			!PASS LINE OFFSET
	REAL UV0(0:3)			!1M UV COORD
	REAL LWGT(0:STHIFR-1,0:3)	!LOCAL WEIGHT
	COMPLEX LCDAT(0:STHIFR-1,0:3)	!LOCAL DATA
	COMPLEX CQMOD(0:3,0:STHIFR-1)	!LOCAL Q MODEL
	COMPLEX CXMOD(0:STHIFR-1,0:3)	!LOCAL XY MODEL
	BYTE SCH(0:SCH__L-1)		!SCAN HEADER
	  INTEGER SCHJ(0:SCH__L/LB_J-1)
	  EQUIVALENCE (SCH,SCHJ)
C-
C
C INIT
C
	NSCQSR=.TRUE.				!ASSUME OK
	PWGT=A_J(QUA+QUA_PWGT_J)		!RETURN POINTERS
	PDAT=A_J(QUA+QUA_PDAT_J)
	PMOD=A_J(QUA+QUA_PMOD_J)
	POUT=A_J(QUA+QUA_POUT_J)
	QUAD=QUA*LB_J/LB_D			!REAL POINTERS
	QUAE=QUA*LB_J/LB_E
	L4DO=4*LB_E				!OFFSET DATA POINT
	L4MO=4*(LB_E+LB_X)			!TOTAL DATA POINT LENGTH
	L4DPL=L4MO
	SCNT=A_J(QUA+QUA_SCNT_J)/((LB_E+LB_X)/LB_E) !SORT LINES
	IF (IAND(A_J(QUA+QUA_ORDER_J),QUB_M).NE.0) THEN
	  L4DPL=L4DPL+4*LB_X
	  SCNT=A_J(QUA+QUA_SCNT_J)/((LB_E+LB_X+LB_X)/LB_E) !SORT LINES
	END IF
	LAX3=A_J(QUA+QUA_NDAT_J)		!LAST AXIS LENGTH
	IF (IAND(A_J(QUA+QUA_ORDER_J),QUB_I).NE.0) THEN !GIVE NORMAL SCAN
	  IF (IAND(A_J(QUA+QUA_ORDER_J),QUB_FTI).NE.0) THEN !FTI
	    I0=AX1				!FREQ POINT
	    I1=AX2				!TIME POINT
	  ELSE					!TFI
	    I0=AX2
	    I1=AX1
	  END IF
	  I2=LAX3-1				!IFR POINTS
	ELSE IF (IAND(A_J(QUA+QUA_ORDER_J),QUB_F).NE.0) THEN !GIVE FREQ SCAN
	  IF (IAND(A_J(QUA+QUA_ORDER_J),QUB_TIF).NE.0) THEN !TIF
	    I1=AX1				!TIME POINT
	    I2=AX2				!IFR POINT
	  ELSE					!ITF
	    I1=AX2
	    I2=AX1
	    LAX1=A_J(QUA+QUA_IIFR_J)
	    LAX2=A_J(QUA+QUA_IHA_J)
	  END IF
	  I0=LAX3-1				!FREQ POINT
	ELSE IF (IAND(A_J(QUA+QUA_ORDER_J),QUB_T).NE.0) THEN !GIVE HA SCAN
	  IF (IAND(A_J(QUA+QUA_ORDER_J),QUB_FIT).NE.0) THEN !FIT
	    I0=AX1				!FREQ POINT
	    I2=AX2				!IFR POINT
	  ELSE					!IFT
	    I0=AX2
	    I2=AX1
	    LAX1=A_J(QUA+QUA_IIFR_J)
	    LAX2=A_J(QUA+QUA_IFRQ_J)
	  END IF
	  I1=LAX3-1				!HA POINT
	ELSE					!UNKNOWN
	  NSCQSR=.FALSE.
C
	  RETURN
	END IF
	IF (I0.LT.0 .OR. I0.GE.A_J(QUA+QUA_IFRQ_J) .OR.
	1		I1.LT.0 .OR.
	1		I1.GE.A_J(QUA+QUA_IHA_J) .OR.
	1		I2.LT.0 .OR.
	1		I2.GE.A_J(QUA+QUA_IIFR_J)) GOTO 800 !ILLEGAL
C
C READ TFI/FTI SCAN
C
	IF (IAND(A_J(QUA+QUA_ORDER_J),QUB_I).NE.0) THEN !GIVE NORMAL SCAN
	  IF (.NOT.NSCQR0(QUA,FCA,I0,I1,SCNP)) THEN !READ SET HEADER
 10	    CONTINUE
	    A_J(QUA+QUA_CMAP_J)=-1		!SET NOTHING READ
	    CALL WNCTXT(F_TP,'Error reading Qube scan F=!UJ, T=!UJ',
	1			I0,I1)
	    GOTO 800
	  END IF
	  IF (.NOT.NSCSCR(FCA,A_B(A_J(QUA+QUA_CSTH_J)),
	1		A_I(A_J(QUA+QUA_PIFR_J)),
	1		I1-SCNP,CAP,CDAP,SCH,
	1		LWGT,LCDAT)) GOTO 10
	  IF (IAND(A_J(QUA+QUA_ORDER_J),QUB_M).NE.0) THEN !MODEL ASKED
	    CALL NMOMUV(A_J(QUA+QUA_STP_J),
	1		A_D(QUAD+QUA_SRA_D),A_D(QUAD+QUA_SDEC_D),
	1		A_B(A_J(QUA+QUA_CSTH_J)),SCH,UV0) !MAKE UV
	    CALL NMOMU4(0,FCA,I1-SCNP,
	1		A_B(A_J(QUA+QUA_CSTH_J)),UV0,
	1		A_E(QUAE+QUA_LM0_E),
	1		A_D(QUAD+QUA_FRQ0_D),
	1		A_B(A_J(QUA+QUA_CSTH_J)+STH_RTP_1),
	1		4,A_J(QUA+QUA_IIFR_J),
	1		A_I(A_J(QUA+QUA_PIFR_J)),
	1		A_E(QUAE+QUA_TF_E),A_J(QUA+QUA_MINST_J),
	1		CQMOD)			!MAKE QUV MODEL
	    CALL NMOCIX(A_B(A_J(QUA+QUA_CSTH_J)),
	1		SCH,
	1		A_E(A_J(QUA+QUA_PANG_J)),
	1		CXMOD,CQMOD)		!MAKE XYX MODEL
	  END IF
	  DO I=0,LAX3-1
	    DO I3=0,3
	      A_E(PWGT+4*I+I3)=LWGT(I,I3)
	      A_X(PDAT+4*I+I3)=LCDAT(I,I3)
	    END DO
	  END DO
	  IF (IAND(A_J(QUA+QUA_ORDER_J),QUB_M).NE.0) THEN !MODEL ASKED
	    DO I=0,LAX3-1
	      DO I3=0,3
	        A_X(PMOD+4*I+I3)=CXMOD(I,I3)
	      END DO
	    END DO
	  END IF
C
C READ TIF/FIT/ITF/IFT
C
	ELSE
	  SORT=.FALSE.				!ASSUME NO SORT
	  IF (IAND(A_J(QUA+QUA_ORDER_J),
	1			QUB_TIF+QUB_FIT).NE.0) THEN !TIF/FIT
	    IF (A_J(QUA+QUA_CMAP_J).NE.AX1) THEN !SORT FIRST
	      SORT=.TRUE.
	      A_J(QUA+QUA_CMAP_J)=-1		!SET NONE READ
	      LPAX2=AX1				!CURRENT MAIN AXIS
	      OFFAX2=0				!CURRENT DISK OFFSET
	    END IF
	  ELSE					!ITF/IFT
	    IF (A_J(A_J(QUA+QUA_CPMAP_J)+AX2).LE.0) THEN !SORT FIRST
	      SORT=.TRUE.
	      LPAX2=AX2
	      OFFAX2=A_J(QUA+QUA_CCNT_J)	!FILE OFFSET
	    END IF
	  END IF
	  IF (SORT) THEN			!SORT DATA
	    NPASS=(LAX3+SCNT-1)/SCNT		!# OF TRANSPOSE PASSES
	    DO I=0,NPASS-1			!ALL PASSES
	      BLINE=I*SCNT			!START LINE
	      LLINE=MIN(LAX3-BLINE,SCNT)	!LENGTH LINE
	      DO I3=BLINE,BLINE+LLINE-1		!LINES
		BOFF=I3-BLINE			!LINE OFFSET
		IF (IAND(A_J(QUA+QUA_ORDER_J),QUB_F).NE.0) THEN !ITF/TIF
		  I4=I3				!ORDER
		  I5=LPAX2
		ELSE				!IFT/FIT
		  I4=LPAX2
		  I5=I3
		END IF
		IF (.NOT.NSCQR0(QUA,FCA,I4,I5,
	1			SCNP)) GOTO 10	!GET STH
		IF (.NOT.NSCSCR(FCA,A_B(A_J(QUA+QUA_CSTH_J)),
	1		A_I(A_J(QUA+QUA_PIFR_J)),
	1		I5-SCNP,CAP,CDAP,SCH,
	1		A_B(A_J(QUA+QUA_SBPT_J)+
	1			L4DPL*STHIFR*BOFF),
	1		A_B(A_J(QUA+QUA_SBPT_J)+
	1			L4DPL*STHIFR*BOFF+
	1			L4DO*STHIFR))) GOTO 10 !SCAN
		IF (IAND(A_J(QUA+QUA_ORDER_J),QUB_M).NE.0) THEN !MODEL ASKED
		  CALL NMOMUV(A_J(QUA+QUA_STP_J),
	1		A_D(QUAD+QUA_SRA_D),A_D(QUAD+QUA_SDEC_D),
	1		A_B(A_J(QUA+QUA_CSTH_J)),SCH,UV0) !MAKE UV
		  CALL NMOMU4(0,FCA,I5-SCNP,
	1		A_B(A_J(QUA+QUA_CSTH_J)),UV0,
	1		A_E(QUAE+QUA_LM0_E),
	1		A_D(QUAD+QUA_FRQ0_D),
	1		A_B(A_J(QUA+QUA_CSTH_J)+STH_RTP_1),
	1		4,A_J(QUA+QUA_IIFR_J),
	1		A_I(A_J(QUA+QUA_PIFR_J)),
	1		A_E(QUAE+QUA_TF_E),A_J(QUA+QUA_MINST_J),
	1		CQMOD)			!MAKE QUV MODEL
		  CALL NMOCIX(A_B(A_J(QUA+QUA_CSTH_J)),
	1		SCH,
	1		A_E(A_J(QUA+QUA_PANG_J)),
	1		A_B(A_J(QUA+QUA_SBPT_J)+
	1			L4DPL*STHIFR*BOFF+
	1			L4MO*STHIFR),
	1		CQMOD)			!MAKE XYX MODEL
		END IF
	      END DO
	      DO I4=0,A_J(QUA+QUA_IIFR_J)-1	!WRITE TRANSPOSED DATA
		DO BOFF=0,LLINE-1		!LINES
		  DO I5=0,3
		    A_E(PWGT+4*BOFF+I5)=
	1			A_E(A_J(QUA+QUA_SBPT_J)/LB_E+
	1			L4DPL*STHIFR*BOFF/LB_E+I5*STHIFR+I4)
		    A_X(PDAT+4*BOFF+I5)=
	1			A_X(A_J(QUA+QUA_SBPT_J)/LB_X+
	1			L4DO*STHIFR/LB_X+
	1			L4DPL*STHIFR*BOFF/LB_X+I5*STHIFR+I4)
		  END DO
		END DO
		IF (.NOT.WNFWR(A_J(QUA+QUA_SFCA_J), !WEIGHT
	1		4*LB_E*LLINE,
	1		A_E(PWGT),
	1		L4DPL*OFFAX2*LAX3*LAX1+
	1		L4DPL*LAX3*I4+4*LB_E*I)) GOTO 10
		IF (.NOT.WNFWR(A_J(QUA+QUA_SFCA_J), !DATA
	1		4*LB_X*LLINE,
	1		A_X(PDAT),
	1		L4DPL*OFFAX2*LAX3*LAX1+
	1		L4DPL*LAX3*I4+4*LB_X*I+
	1			L4DO*LAX3)) GOTO 10
		IF (IAND(A_J(QUA+QUA_ORDER_J),QUB_M).NE.0) THEN !MODEL ASKED
		  DO BOFF=0,LLINE-1		!LINES
		    DO I5=0,3
		      A_X(PMOD+4*BOFF+I5)=
	1			A_X(A_J(QUA+QUA_SBPT_J)/LB_X+
	1			L4MO*STHIFR/LB_X+
	1			L4DPL*STHIFR*BOFF/LB_X+I5*STHIFR+I4)
		    END DO
		  END DO
		  IF (.NOT.WNFWR(A_J(QUA+QUA_SFCA_J), !MODEL
	1		4*LB_X*LLINE,
	1		A_X(PMOD),
	1		L4DPL*OFFAX2*LAX3*LAX1+
	1		L4DPL*LAX3*I4+4*LB_X*I+
	1			L4MO*LAX3)) GOTO 10
		END IF
	      END DO
	    END DO				!PASSES
	    IF (IAND(A_J(QUA+QUA_ORDER_J),
	1			QUB_TIF+QUB_FIT).NE.0) THEN !TIF/FIT
	      A_J(QUA+QUA_CMAP_J)=AX1		!SET SORTED
	    ELSE				!ITF/IFT
	      A_J(A_J(QUA+QUA_CPMAP_J)+AX2)=OFFAX2+1 !SET READ
	      A_J(QUA+QUA_CCNT_J)=A_J(QUA+QUA_CCNT_J)+1 !NEW OFFSET
	    END IF
	  END IF
	  IF (IAND(A_J(QUA+QUA_ORDER_J),
	1			QUB_TIF+QUB_FIT).NE.0) THEN !TIF/FIT
	    IF (A_J(QUA+QUA_CMAP_J.NE.AX1)) GOTO 10 !ERROR
	    OFFAX2=0				!DISK OFFSET
	  ELSE					!ITF/IFT
	    OFFAX2=A_J(A_J(QUA+QUA_CPMAP_J)+AX2)-1 !DISK OFFSET
	    IF (OFFAX2.LT.0) GOTO 10		!ERROR
	  END IF
	  IF (.NOT.WNFRD(A_J(QUA+QUA_SFCA_J),
	1		4*LB_E*LAX3,
	1		A_E(PWGT),
	1		L4DPL*OFFAX2*LAX3*LAX1+
	1		L4DPL*LAX3*I2)) GOTO 10	!READ WEIGHT
	  IF (.NOT.WNFRD(A_J(QUA+QUA_SFCA_J),
	1		4*LB_X*LAX3,
	1		A_X(PDAT),
	1		L4DPL*OFFAX2*LAX3*LAX1+
	1		L4DPL*LAX3*I2+
	1		L4DO*LAX3)) GOTO 10	!READ DATA
	  IF (IAND(A_J(QUA+QUA_ORDER_J),QUB_M).NE.0) THEN !MODEL ASKED
	    IF (.NOT.WNFRD(A_J(QUA+QUA_SFCA_J),
	1		4*LB_X*LAX3,
	1		A_X(PMOD),
	1		L4DPL*OFFAX2*LAX3*LAX1+
	1		L4DPL*LAX3*I2+
	1		L4MO*LAX3)) GOTO 10	!READ MODEL
	  END IF
	END IF
C
	RETURN
C
C ERROR
C
 800	CONTINUE
	DO I=0,LAX3
	  DO I3=0,3
	    A_E(PWGT+4*I+I3)=0			!SET NO POINTS
	  END DO
	END DO
	NSCQSR=.FALSE.				!INDICATE ERROR
C
	RETURN
C
C
	END
