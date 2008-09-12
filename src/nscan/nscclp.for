C+ NSCCLP.FOR
C  WNB 900825
C
C  Revisions:
C	WNB 920813	Clean up
C	JEN 960412	Changed input RA,DEC from 1950 to apparent
C
	SUBROUTINE NSCCLP(FCA,STHJ,PHI)
C
C Get precession rotation angle
C
C  Result:
C
C	CALL NSCCLP( FCA_J:I, STHJ_J(0:*):I, PHI_E:O)	
C					Get PHI, the rotation angle due to
C					precession and nutation, in radians.
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'STH_O_DEF'		!SCAN SET HEADER
	INCLUDE 'OHW_O_DEF'		!OH BLOCK
	INCLUDE 'SCW_O_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER FCA			!FILE ID
	INTEGER STHJ(0:*)		!SCAN SET HEADER
	REAL PHI			!ROTATION ANGLE
C
C  Function references:
C
	LOGICAL WNFRD			!READ DISK
C
C  Data declarations:
C
	BYTE OHW(0:OHWHDL-1)		!OH
	  INTEGER OHWJ(0:OHWHDL/4-1)
	  REAL*8 OHWD(0:OHWHDL/8-1)
	  EQUIVALENCE (OHW,OHWJ,OHWD)
	BYTE SCW(0:SCWHDL-1)		!SC
	  INTEGER SCWJ(0:SCWHDL/4-1)
	  REAL SCWE(0:SCWHDL/4-1)
	  REAL*8 SCWD(0:SCWHDL/8-1)
	EQUIVALENCE (SCW,SCWJ,SCWE,SCWD)
	REAL*8 EXY(3),XY(3)             !APPARENT POS. VECTOR
	REAL*8 XYZ(3),XYZD(3)           !DIF. POS. VECTOR
	REAL*8 RA0,DEC0			!APPARENT POS.
	REAL*8 RA,DEC			!CALCULATED 1950 POS.
	REAL*8 ROT(3,3)			!ROTATION MATRIX
C-
	IF (STHJ(STH_OHP_J).NE.0 .AND. STHJ(STH_SCP_J).NE.0) THEN !CAN DO
	  JS=WNFRD(FCA,STHJ(STH_NOH_J),OHW,STHJ(STH_OHP_J)) !READ OH
	  IF (JS) JS=WNFRD(FCA,STHJ(STH_NSC_J),SCW,STHJ(STH_SCP_J)) !READ SC
	  IF (.NOT.JS) THEN
	    CALL WNCTXT(F_TP,'!/Error reading OH and/or SC')
	    GOTO 10
	  END IF
!
! Get the apparent position vector of the field centre (RA0, DEC0):
!
	  RA0 = DPI2*OHWD(OHW_RA0_D)    ! 1950 RA (RADIANS)
          DEC0 = DPI2*OHWD(OHW_DEC0_D)  ! 1950 DEC (RADIANS)
!
	  EXY(1)=COS(DEC0)*COS(RA0)	!GET POS. VECTOR 1950
	  EXY(2)=COS(DEC0)*SIN(RA0)
	  EXY(3)=SIN(DEC0)
!
! GET DIF. POS. VECTOR towards the north: 
! Note from JEN: This is a position vector for DEC = 90-DEC0 degr...
! This is a strange choice, because it will coincide with EXY for
! DEC0=45 degr, and be to the south for DEC0>45 degr.
! It would be better to use the pole itself, as appears to be done
! in the AIPS++ routine (also written by WNB), although that might
! cause problems for DEC<-45 degr.
! In any case there is no evidence of using an offset of 1 degree
! to the north, as WNB says he uses....
!
	  XYZ(2)=-SIN(DEC0)*SIN(RA0)
	  XYZ(1)=-SIN(DEC0)*COS(RA0)
	  XYZ(3)=COS(DEC0)
!
! Rotate the various position vectors to their current (MJD) positions:
!
	  DO I=1,3				!MAKE ROT. MATRIX
	    DO I1=1,3
	      ROT(I1,I)=0
	      DO I2=1,3
		ROT(I1,I)=ROT(I1,I)+SCWD(SCW_NUTA_D-4+3*I+I2)*
	1			SCWD(SCW_PREC_D-4+3*I2+I1)
	      END DO
	    END DO
	  END DO
!
	  DO I=1,3				!ROTATE VECTORS
	    XY(I)=0                             !field centre 
	    XYZD(I)=0                           !dif position
	    DO I1=1,3
	      XY(I)=XY(I)+EXY(I1)*ROT(I1,I)
	      XYZD(I)=XYZD(I)+XYZ(I1)*ROT(I1,I)
	    END DO
	  END DO
!
! Calculate the 1950 RA and DEC of the field centre:
! 
	  IF (XY(2).EQ.0) THEN			!RA DATE
	    IF (XY(1).GE.0) THEN
	      RA=0
	    ELSE
	      RA=DPI
	    END IF
	  ELSE
	    RA=ATAN2(XY(2),XY(1))		!RA DATE
	  END IF
	  DEC=ASIN(XY(3))                       !DEC DATE
!
! Calculate the field rotation angle PHI:
! Note from JEN: Should be PHI = ATAN(PHI)? Does not make a large
! difference, even at high DEC.
!
	  IF (XYZD(3).EQ.0) THEN
	    GOTO 10
	  ELSE IF (SIN(RA).EQ.0) THEN
	    PHI=(XYZD(2)*COS(DEC)+XYZD(3)*
	1		SIN(DEC)*SIN(RA))/
	2		(XYZD(3)*COS(RA))
	  ELSE IF (COS(RA).EQ.0) THEN
	    PHI=(XYZD(1)*COS(DEC)+XYZD(3)*
	1		SIN(DEC)*COS(RA))/
	2		(-XYZD(3)*SIN(RA))
	  ELSE
	    PHI=((XYZD(1)*COS(DEC)+XYZD(3)*
	1		SIN(DEC)*COS(RA))/
	2		(-XYZD(3)*SIN(RA))+
	3		(XYZD(2)*COS(DEC)+XYZD(3)*
	4		SIN(DEC)*SIN(RA))/
	5		(XYZD(3)*COS(RA)))/2
	  END IF
!
! If not possible to calculate PHI, return PHI=0 (?) 
!
	ELSE
 10	  PHI=0					!CANNOT DO
	END IF
!
!
	PHI=-PHI				!FLOP SIGN
C
! Some diagnostic output (JEN):
!
cc	CALL WNCTXT (F_TP,
cc     1       'NSCCLP: apparent pos RA0=!DA10.5  DEC0=!DA10.5 degr'
cc     1       ,RA0, DEC0)
cc	CALL WNCTXT (F_TP,
cc     1       ' calculated 1950 pos RA =!DA10.5  DEC =!DA10.5 degr'
cc     1       ,RA, DEC)
cc	CALL WNCTXT (F_TP,
cc     1       '  PHI=!EA10.5 degr  ATAN(PHI)=!EA10.5 '
cc     1       ,PHI, ATAN(PHI))
!
	RETURN
C
C
	END






