C+ NMOMUV.FOR
C  WNB 900903
C
C  Revisions:
C	WNB 910805	Typo
C	WNB 911021	Change rotation signs
C	WNB 911023	Change rotation signs back
C	WNB 920118	Change precision rotation
C
	SUBROUTINE NMOMUV(BTP,RA,DEC,STHD,SCHE,UV0)
C
C  Calculate UV data for model calculation
C
C  Result:
C
C	CALL NMOMUV( BTP_J:I, RA_D:I, DEC_D:I, STHD_D(0:*):I,
C			SCHE_E(0:*):I, UV0_E(0:3):O)
C				Calculate for BTP (0=unknown, 1=apparent,
C				2=B1950) the UV for 1m baseline,
C				using the set header STHD,
C				the scan header SCHE and
C				the reference RA, DEC of map.
C				UV(0:1) = vector components in radians per m
C				UV(2:3) = components of perpendicular vector
C					   in radians per m
C
C  PIN references
C
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
	INTEGER BTP			!COORD. TYPE
	REAL*8 RA			!RA TO USE
	REAL*8 DEC			!DEC TO USE
	REAL*8 STHD(0:*)		!SET HEADER
	REAL SCHE(0:*)			!SCAN HEADER
	REAL UV0(0:3)			!UV DATA
C
C  Function references:
C
	INTEGER WNGARA			!ADDRESS
C
C  Data declarations:
C
	REAL R2,R3,R4
C-
	R0=COS(PI2*SCHE(SCH_HA_E))			!COS(HA)
	R1=SIN(PI2*SCHE(SCH_HA_E))			!SIN(HA)
	R2=SIN(DPI2*STHD(STH_DEC_D))			!SIN(DEC)
	R3=DPI2*STHD(STH_FRQ_D)*1D6/DCL			!SCALE
C
C  First calculate the two vectors in the equatorial plane
C   for observed HA in SCH, DEC in STH
C
	UV0(0)=R3*R0					!U
	UV0(1)=-R3*R1*R2				!V
	UV0(2)=R3*R0*R2					!SMEARING
	UV0(3)=-R3*R1
C
C  If epoch requested, rotate in the equatorial plane over (precession?) angle 
C   PHI from STH
C
	J=(WNGARA(STHD(0))-A_OB)/LB_E			!STHE OFFSET
	IF (BTP.EQ.2) THEN				!B1950 or J2000
	  R0=COS(PI2*A_E(J+STH_PHI_E))			!COS(PHI)
	  R1=SIN(PI2*A_E(J+STH_PHI_E))			!SIN(PHI)
	  R3=UV0(0)*R0+UV0(1)*R1			! ROTATE, save
	  UV0(1)=-UV0(0)*R1+UV0(1)*R0
	  UV0(0)=R3					! copy in place
	  R3=UV0(2)*R0+UV0(3)*R1			! ROTATE, save
	  UV0(3)=-UV0(2)*R1+UV0(3)*R0
	  UV0(2)=R3					! copy in place
	END IF
C
C  If either apparent or epoch, rotate from epoch/apparent to map reference\
C   in 3 steps: 
C   - Deproject by division through sin DEC (epoch/apparent);
C   - Rotate in equatorial plane from epoch/apparent to reference HA
C   - Project by multiplying with sin DEC (reference) 
C
	IF (BTP.NE.0) THEN
	  R3=SIN(DPI2*DEC)
	  IF (BTP.EQ.2) THEN
	    R0=COS(DPI2*(STHD(STH_RAE_D)-RA))		!COS(RA0-RA)
	    R1=SIN(DPI2*(STHD(STH_RAE_D)-RA))		!SIN
	    R2=SIN(DPI2*STHD(STH_DECE_D))		!SIN(DEC)
	  ELSE
	    R0=COS(DPI2*(STHD(STH_RA_D)-RA))		!COS(RA0-RA)
	    R1=SIN(DPI2*(STHD(STH_RA_D)-RA))		!SIN
	  END IF
	  R4=R0*UV0(0)+R1/R2*UV0(1)			! ROTATE, save
	  UV0(1)=-R1*R3*UV0(0)+R0*R3/R2*UV0(1)
	  UV0(0)=R4					! copy in place
	  R4=R0*UV0(2)+R1/R2*UV0(3)			! ROTATE, save
	  UV0(3)=-R1*R3*UV0(3)+R0*R3/R2*UV0(3)
	  UV0(2)=R4					! copy in place
	END IF
C
	RETURN
C
C
	END
