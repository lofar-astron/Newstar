C+ NMOCXI.FOR
C  WNB 930825
C
C  Revisions:
C	WNB 930827	Use SMOD properly
C	WNB 930901	Correct Q,U interchange; WSRT sign U
C	CMV 930910	Change sign V
C	CMV 930913	Calculate/shuffle weights as well as data
C	WNB 931029	Convex does not accept complex parameter
C
	SUBROUTINE NMOCXI(STHJ,SCHE,ANG,WGT,OWGT,CDAT,CMOD)
C
C  Convert data XYX to Stokes Model
C
C  Result:
C
C	CALL NMOCXI( STHJ_J(0:*):I, SCHE_E(0:*):I, ANG_E(0:2,0:*):I,
C			WGT_E(0:*,0:3), OWGT_J(0:*),
C			CDAT_X(0:*,0:3):I, CMOD_X(0:3,0:*):O)
C				Convert data XYX to proper Stokes data,
C				using the STH set and SCH scan header.
C				CDAT are the XYX data, CMOD the Stokes data,
C				and WGT the data weight.
C				ANG contains:
C				0: the parallactic angle of W X-dipole (circles)
C				1: sin(E X-dipole - W X-dipole)
C				2: cos(...)
C				If a Stokes cannot be made, the corresponding
C				item will be set to 0 and the OWGT.
C	CALL NMOCXX( STHJ_J(0:*):I, SCHE_E(0:*):I, ANG_E(0:2,0:*):I,
C			WGT_E(0:*,0:3):IO, OWGT_J(0:*,0:3):O,
C			CDAT_X(0:*,0:3):I, OCDAT_X(0:*,0:3):O,
C			NPOL_J:I, TYPE_J(0:NPOL-1):I)
C				Convert XYX to Stokes or XYX for NPOL (<=4)
C				codes TYPE (see CBITS). Data will be in
C				CDAT. OWGT is 1 if all requested pol's are
C				there, else 0. WGT(*,ipol) will have the 
C				weight corresponding to CDAT(*,ipol) at
C				output (0 if ipol could not be calculated).
C				CDAT maybe OCDAT. 
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'CBITS_DEF'
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
	REAL WGT(0:STHIFR-1,0:3)	!WEIGHT
	INTEGER OWGT(0:STHIFR-1)	!OUTPUT WEIGHT SET TO ZERO IF NO SUCCESS
	COMPLEX CDAT(0:STHIFR-1,0:3)	!DATA
	COMPLEX OCDAT(0:STHIFR-1,0:3)	!OUTPUT DATA
	COMPLEX CMOD(0:3,0:*)		!MODEL
	INTEGER NPOL			!# OF OUTPUT DESCRIPTORS (<= 4)
	INTEGER TYP(0:*)		!DESCRIPTORS
C
C  Function references:
C
C
C  Data declarations:
C
	LOGICAL SMOD		!SET MODEL/DATA
	INTEGER LN		!# TO DO
	INTEGER LC(0:3)		!LOCAL COUNT
	COMPLEX LDAT(0:3)	!LOCAL DATA
	REAL    LWGT(0:3)	!LOCAL WEIGHTS
	INTEGER DTYP(0:3)	!DEFAULT TYPES
	  DATA DTYP/I_M,Q_M,U_M,V_M/
	INTEGER LT(0:3)		!LOCAL TYPES
	REAL SX,CX		!SIN,COS(2.CHI+B)
	REAL SBS,CBS,SBR	!SIGN OF SIN,COS(BETA)
	REAL MAT(0:3,0:3)	!CONVERSION MATRIX
	INTEGER CM(0:3)		!COUNT OF DATA POINTS
	COMPLEX CXI		!I
C-
C
C NMOCXI
C
	LN=4					!# TO DO
	DO I=0,LN-1				!SET TYPES
	  LT(I)=DTYP(I)
	END DO
	SMOD=.TRUE.				!MAKE MODEL
	GOTO 100
C
C NMOCXX
C
	ENTRY NMOCXX(STHJ,SCHE,ANG,WGT,OWGT,CDAT,OCDAT,NPOL,TYP)
C
	LN=MIN(4,NPOL)				!# TO DO
	DO I=0,LN-1				!SET TYPES
	  LT(I)=TYP(I)
	END DO
	SMOD=.FALSE.				!MAKE DATA
	GOTO 100
C
C INIT
C
 100	CONTINUE
	CXI=CMPLX(0,1)
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
	  R0=SIN(R0)
	  SX=R0*ANG(2,I)+R1*ANG(1,I)			!SIN(2*CHI+BETA)
	  CX=R1*ANG(2,I)-R0*ANG(1,I)			!COS
	  R0=2*(ABS(ANG(1,I))+ABS(ANG(2,I)))		!NORMALISATION
	  MAT(0,0)=+ANG(2,I)*CBS			!MATRIX I
	  MAT(1,0)=-ANG(1,I)*SBR
	  MAT(2,0)=+ANG(1,I)*SBS
	  MAT(3,0)=+ANG(2,I)*CBS
	  MAT(0,1)=+CX*CBS				!MATRIX Q
	  MAT(1,1)=-SX*SBR
	  MAT(2,1)=-SX*SBS
	  MAT(3,1)=-CX*CBS
	  MAT(0,2)=+SX*CBS				!MATRIX U
	  MAT(1,2)=+CX*SBR
	  MAT(2,2)=+CX*SBS
	  MAT(3,2)=-SX*CBS
	  MAT(0,3)=+ANG(1,I)*CBS			!MATRIX V
	  MAT(1,3)=+ANG(2,I)*SBR
	  MAT(2,3)=-ANG(2,I)*SBS
	  MAT(3,3)=+ANG(1,I)*CBS
	  DO I1=0,3					!COUNT HOW MANY
	    CM(I1)=0					!SET 0
	    DO I2=0,3
	      IF (ABS(MAT(I2,I1)).LT.1E-6) MAT(I2,I1)=0	!MAKE SURE
	      IF (MAT(I2,I1).NE.0) CM(I1)=CM(I1)+1	!CNT HOW MANY TO USE
	    END DO
	  END DO
C
C MAKE STOKES
C
	  DO I1=0,LN-1					!ALL REQUESTS
	    LC(I1)=0					!COUNT POINTS FOUND
	    LDAT(I1)=0					!OUTPUT DATA
	    LWGT(I1)=0					!OUTPUT WEIGHT
C
C XYX
C
	    IF (IAND(LT(I1),STOKES_P).EQ.0) THEN	!NOT STOKES
	      IF (IAND(LT(I1),XX_P).NE.0) THEN		!XX ASKED
		I2=0
	      ELSE IF (IAND(LT(I1),XY_P).NE.0) THEN
		I2=1
	      ELSE IF (IAND(LT(I1),YX_P).NE.0) THEN
		I2=2
	      ELSE IF (IAND(LT(I1),YY_P).NE.0) THEN
		I2=3
	      ELSE
	        I2=-1
	      END IF
	      IF (I2.GE.0) THEN				!TO DO
		IF (WGT(I,I2).GT.0) THEN		!SELECTED
		  LC(I1)=LC(I1)+1			!COUNT
		  LDAT(I1)=LDAT(I1)+CDAT(I,I2)		!MAKE
		  LWGT(I1)=LWGT(I1)+WGT(I,I2)		!MAKE
		END IF
	      END IF
C
C STOKES
C
	    ELSE					!STOKES
	      IF (IAND(LT(I1),SI_P).NE.0) THEN		!I ASKED
		I2=0
	      ELSE IF (IAND(LT(I1),SQ_P).NE.0) THEN
		I2=1
	      ELSE IF (IAND(LT(I1),SU_P).NE.0) THEN
		I2=2
	      ELSE IF (IAND(LT(I1),SV_P).NE.0) THEN
		I2=3
	      ELSE
	        I2=-1
	      END IF
	      IF (I2.GE.0) THEN				!SOME ASKED
		DO I3=0,3				!SCAN MATRIX
		  IF (MAT(I3,I2).NE.0) THEN		!SHOULD TAKE THIS ONE
		    IF (WGT(I,I3).GT.0) THEN		!PRESENT
		      LC(I1)=LC(I1)+1			!COUNT
		      LDAT(I1)=LDAT(I1)+MAT(I3,I2)*CDAT(I,I3) !AND DATA
		      LWGT(I1)=LWGT(I1)+WGT(I,I3)	!MAKE
		    END IF
		  END IF
		END DO
	      END IF
	      IF (LC(I1).LT.CM(I2)) THEN		!NOT ENOUGH
		IF (IAND(LT(I1),LINE_P).EQ.0) LC(I1)=0	!ACCEPT ONLY LINE I
	      END IF
	      IF (LC(I1).GT.0) THEN			!NORMALISE
		LDAT(I1)=LDAT(I1)/R0*REAL(CM(I2))/REAL(LC(I1))
		LWGT(I1)=LWGT(I1)/REAL(LC(I1))
	      END IF
C CMV930909 Changed sign
	      IF (I2.EQ.3) LDAT(I1)=CXI*LDAT(I1)	!MAKE V
	    END IF					!STOKES
	    IF (IAND(LT(I1),IMAG_P).NE.0) LDAT(I1)=CXI*LDAT(I1) !IMAGINARY
	  END DO					!ALL REQUESTS
	  DO I1=LN,3					!DELETE REST
	    LC(I1)=-1
	  END DO
	  DO I1=0,3					!OUTPUT ALL
	    IF (LC(I1).GT.0) THEN
	      IF (SMOD) THEN
		CMOD(I1,I)=LDAT(I1)
	      ELSE
	        OCDAT(I,I1)=LDAT(I1)
		WGT(I,I1)=LWGT(I1)
	      END IF
	    ELSE
	      IF (SMOD) THEN
		CMOD(I1,I)=0
	      ELSE
	        OCDAT(I,I1)=0
		WGT(I,I1)=0
	      END IF
	      IF (LC(I1).EQ.0) OWGT(I)=0		!DELETE POINT
	    END IF
	  END DO
	END DO						!INTERFEROMETERS
C
	RETURN
C
C
	END
