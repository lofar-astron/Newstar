C+ NMOCIX.FOR
C  WNB 900825
C
C  Revisions:
C	WNB 930826	Add CIY; remove CXI, remove WGT
C	WNB 930831	Correct model addition
C	WNB 930901	Correct Q/U interchange; WSRT sign U
C	WNB 931029	Convex does not accept complex parameter
C	CMV 940303	Change sign of V
C
	SUBROUTINE NMOCIX(STHJ,SCHE,ANG,CDAT,CMOD)
C
C  Convert Stokes Model to data XYX
C
C  Result:
C
C	CALL NMOCIX( STHJ_J(0:*):I, SCHE_E(0:*):I, ANG_E(0:2,0:*):I,
C			CDAT_X(0:*,0:3):O, CMOD_X(0:3,0:*):I)
C				Convert Stokes model data to proper XYX data,
C				using the STH set and SCH scan header.
C				CDAT are the XYX data, CMOD the Stokes data.
C				ANG contains:
C				0: the parallactic angle of W X-dipole (circles)
C				1: sin(E X-dipole - W X-dipole)
C				2: cos(...)
C	CALL NMOCIY( STHJ_J(0:*):I, SCHE_E(0:*):I, ANG_E(0:2,0:*):I,
C			CDAT_X(0:*,0:3):O, CMOD_X(0:3,0:*):I, NRINT_J:I)
C				Convert Stokes model data to XYX, averaging
C				with existing XYX data with weight NRINT if
C				NRINT>=0. I.e. out=(N*old+new)/(N+1)
C				If NRINT<0: out=(N*old+new)/N, e.g. if N==1:
C				out=old-new!
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'STH_O_DEF'		!SET HEADER
	INCLUDE 'SCH_O_DEF'		!SCAN HEADER
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER STHJ(0:*)		!SET HEADER
	REAL SCHE(0:*)			!SCAN HEADER
	REAL ANG(0:2,0:*)		!DIPOLE ANGLES
	COMPLEX CDAT(0:STHIFR-1,0:3)	!DATA
	COMPLEX CMOD(0:3,0:*)		!MODEL
	INTEGER NRINT			!NUMBER OF INTEGRATIONS
C
C  Function references:
C
C
C  Data declarations:
C
	REAL SX,CX		!SIN,COS(2.CHI+B)
	REAL SBS,CBS,SBR	!SIGN OF SIN,COS(BETA)
	COMPLEX CXI		!I
C-
C
C NMOCIX
C
	J0=0					!NO INTEGRATION
	GOTO 100
C
C NMOCIY
C
	ENTRY NMOCIY(STHJ,SCHE,ANG,CDAT,CMOD,NRINT)
C
	J0=NRINT				!INTEGRATION COUNT
	GOTO 100
C
C INIT
C
 100	CONTINUE
	CXI=CMPLX(0,1)
	J1=J0					!NO AVERAGE
	IF (J0.GE.0) J1=J0+1			!AVERAGE
C
C ALL DATA
C
	DO I=0,STHJ(STH_NIFR_J)-1			!ALL DATA POINTS
C
C INIT
C
	  SBS=1						!ASSUME POS. SIGN
	  CBS=1
	  SBR=1
	  IF (STHJ(STH_INST_J).EQ.0) THEN		!WSRT
	    IF (ANG(1,I).LE.0) SBS=-1			!REVERSE SIGN
	    IF (ANG(2,I).LT.0) CBS=-1
	    SBR=-SBS
	  END IF
	  R0=(2*(SCHE(SCH_PANG_E)+ANG(0,I)))*PI2	!2*CHI
	  R1=COS(R0)					!COS
	  R0=SIN(R0)					!SIN
	  SX=R0*ANG(2,I)+R1*ANG(1,I)			!SIN(2*CHI+BETA)
	  CX=R1*ANG(2,I)-R0*ANG(1,I)			!COS
	  R0=MAX(ABS(ANG(1,I)),ABS(ANG(2,I)))		!NORMALISATION
C
C CONVERT STOKES TO XYX
C
	  CDAT(I,0)=(CDAT(I,0)*J0+
	1	CBS/R0*( CMOD(0,I)*ANG(2,I)
	1		+CMOD(1,I)*CX
	1		+CMOD(2,I)*SX
	1		-CMOD(3,I)*ANG(1,I)*CXI))/J1	!XX
C
	  CDAT(I,1)=(CDAT(I,1)*J0+
	1	SBR/R0*(-CMOD(0,I)*ANG(1,I)
	1		-CMOD(1,I)*SX
	1		+CMOD(2,I)*CX
	1		-CMOD(3,I)*ANG(2,I)*CXI))/J1	!XY
C
	  CDAT(I,2)=(CDAT(I,2)*J0+
	1	SBS/R0*( CMOD(0,I)*ANG(1,I)
	1		-CMOD(1,I)*SX
	1		+CMOD(2,I)*CX
	1		+CMOD(3,I)*ANG(2,I)*CXI))/J1	!YX
C
	  CDAT(I,3)=(CDAT(I,3)*J0+
	1	CBS/R0*( CMOD(0,I)*ANG(2,I)
	1		-CMOD(1,I)*CX
	1		-CMOD(2,I)*SX
	1		-CMOD(3,I)*ANG(1,I)*CXI))/J1	!YY
C
	END DO
C
	RETURN
C
C
	END
