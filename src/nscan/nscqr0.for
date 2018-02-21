C+ NSCQR0.FOR
C  WNB 940805
C
C  Revisions:
C
	LOGICAL FUNCTION NSCQR0(QUA,FCA,AX1,AX2,SCNP)
C
C  Get some scan read data
C
C  Result:
C
C	NSCQR0_L = NSCQR0( QUA_J:I, FCA_J:I, AX1_J:I, AX2_J:I,
C				SCNP_J:O)
C			Read the STH set header at frequency AX1, Ha AX2.
C			SCNP gives the start number of ha in sub-scan
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'CBITS_DEF'             !BIT DEFINITIONS
	INCLUDE 'QUB_O_DEF'		!QUBE DEFINITION
	INCLUDE 'STH_O_DEF'		!SCAN SET HEADER
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER QUA			!QUBE CONTROL AREA
	INTEGER FCA			!FILE
	INTEGER AX1,AX2			!AXES TO READ
	INTEGER SCNP			!HA # OF FIRST SCAN IN SUB-SCAN
C
C  Function references:
C
	LOGICAL WNFRD			!READ DATA
C
C  Data declarations:
C
	INTEGER STHP			!SET HEADER POINTER
C-
C
C INIT
C
	NSCQR0=.TRUE.				!ASSUME OK
C
C FIND IFR SCAN
C
	I=0					!FIND SCAN STH
	DO WHILE (A_J(A_J(QUA+QUA_IBPT_J)+
	1		2*(A_J(QUA+QUA_NBLK_J)*AX1+I)).LE.AX2)
	  I=I+1
	END DO
	IF (I.EQ.0) THEN			!START SCAN NUMBER
	  SCNP=0
	ELSE
	  SCNP=A_J(A_J(QUA+QUA_IBPT_J)+
	1		2*(A_J(QUA+QUA_NBLK_J)*AX1+I-1))
	END IF
	STHP=A_J(A_J(QUA+QUA_IBPT_J)+
	1		2*(A_J(QUA+QUA_NBLK_J)*AX1+I)+1) !STH POINTER
	IF (STHP.NE.A_J(QUA+QUA_CSTHP_J)) THEN
	  NSCQR0=WNFRD(FCA,STH__L,A_B(A_J(QUA+QUA_CSTH_J)),STHP)
	  IF (NSCQR0) THEN
	    A_J(QUA+QUA_CSTHP_J)=STHP		!NEW CURRENT
	  ELSE
	    A_J(QUA+QUA_CSTHP_J)=0
	  END IF
	END IF
C
	RETURN
C
C
	END