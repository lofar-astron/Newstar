C+ NSCGGN.FOR
C  CMV 940930
C
C  Revisions:
C	CMV 940930	Created
C
C
	SUBROUTINE NSCGGN(GNOUT,TSYS,GNCAL,IFH,STH,TPIN)
C
C  Return telescope gain etc. based on TPon/TPoff values
C
C  Result:
C
C	CALL NSCGGN(GNOUT_E(0:STHTEL-1,0:1):O, 
C		    TSYS_E(0:STHTEL-1,0:1):O, 
C	            GNCAL_E(0:STHTEL-1,0:1):O, 
C	            IFH_B(*):I,STH_B(*),
C		    TPIN_I(2,0:1,0:STHTEL-1):I)
C
C		Returns, for each telescope and for each dipole, 
C		the gain (in GNOUT), the system temperature (in TSYS)
C		and the correction method used (in GNCAL).
C		The calculation is based on constants defined in 
C		this routine and on the information in the IF-header
C		(IFH) and the total power data (in TPIN), for the
C		configuration described by STH.
C
C		The IF-header is described in IFH.DSC, the TPIN array
C		should contain TPon and TPoff
C
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'STH_O_DEF'			!SECTOR HEADER
	INCLUDE 'IFH_O_DEF'			!IF-SET HEADER
C
C  Parameters:
C
	REAL RAWSCALE				!Scale for raw corr.fractions
	  PARAMETER(RAWSCALE=23104.)
C
	REAL TCDCB,TCDLB			!Fraction TPon for DCB and DLB
	  PARAMETER(TCDCB=1./8.)
	  PARAMETER(TCDLB=1./16.)
C
	REAL EFF92,EFF49,EFF21,EFF18,EFF06,EFF03 !Aperture efficiencies
	  PARAMETER(EFF92=0.59)
	  PARAMETER(EFF49=0.59)
	  PARAMETER(EFF21=0.54)
	  PARAMETER(EFF18=0.54)
	  PARAMETER(EFF06=0.48)
	  PARAMETER(EFF03=0.48)
C
	REAL DIPP,DIPC				!Dipole factor parallel/crossed
	  PARAMETER(DIPP=1.0)
	  PARAMETER(DIPC=1.414214)
C
	REAL BOLZMAN				!Bolzmann constant
	  PARAMETER(BOLZMAN=1.3805E-23)
	REAL APERTURE				!Effective aperture
	  PARAMETER(APERTURE=491E0)		!PI*12.5**2
C
	REAL MINDCB,MAXDCB,MINDLB,MAXDLB	!Allowed range for TPoff
C**	  PARAMETER(MINDCB=600.)
	  PARAMETER(MINDCB=100.)
C**	  PARAMETER(MAXDCB=25000.)
	  PARAMETER(MAXDCB=30000.)
	  PARAMETER(MINDLB=100.)
	  PARAMETER(MAXDLB=12000.)
C
	REAL MINNOISE,MAXNOISE			!Range for noise factors
	  PARAMETER(MINNOISE=0.013333333)
	  PARAMETER(MAXNOISE=2.0)
C
C  Arguments:
C
	REAL GNOUT(0:STHTEL-1,0:1)		!Gain factors (output)
	REAL TSYS(0:STHTEL-1,0:1)		!System temperatures (output)
	REAL GNCAL(0:STHTEL-1,0:1)		!Correction method used (output)
	INTEGER*2 IFH(0:*)			!IF-header
	REAL STH(0:*)				!Sector header
	INTEGER*2 TPIN(2,0:1,0:STHTEL-1)	!TPon/off 
C
C  Function references:
C
	INTEGER WNGARA				!FIND ADDRESS
	INTEGER WNMEJC				!CEIL
	INTEGER WNMEJF				!FLOOR
C
C  Data declarations:
C
	REAL TPON,TPOFF				!TEMP. FOR TP-values
	REAL SCALE,GAMMA			!SCALE AND OFFSET FOR GAIN
	REAL LIM(2)				!LIMITS FOR TPON/OFF
C-
C
C INIT
C
C
	SCALE=4E28*BOLZMAN/APERTURE			!NOMINAL SCALE
C
C Settings dependent on frequency
C
	J=(WNGARA(STH(0))-A_OB)/LB_D   			!ADDRESS HEADER
	IF (A_D(J+STH_FRQ_D).LT.500.D0) THEN		! 92 cm
	   SCALE=SCALE/EFF92
	ELSE IF (A_D(J+STH_FRQ_D).LT.1000.D0) THEN	! 49 cm
	   SCALE=SCALE/EFF49
	ELSE IF (A_D(J+STH_FRQ_D).LT.1500.D0) THEN	! 21 cm
	   SCALE=SCALE/EFF21
	ELSE IF (A_D(J+STH_FRQ_D).LT.3000.D0) THEN	! 18 cm
	   SCALE=SCALE/EFF18
	ELSE IF (A_D(J+STH_FRQ_D).LT.6000.D0) THEN	!  6 cm
	   SCALE=SCALE/EFF06
	ELSE 						!  3 cm
	   SCALE=SCALE/EFF03
	END IF
C
C Settings dependent on dipoles (assume crossed or parallel)
C
	J=(WNGARA(STH(0))-A_OB)/LB_J			!ADDRESS HEADER
	I1=A_J(J+STH_DIPC_J)				!GET DIPOLE CODE
	IF (I1/'0ff00000'X .EQ. IAND(I1,'0ff'X)) THEN	!PARALLEL
	   SCALE=SCALE*DIPP
	ELSE						!CROSSED
	   SCALE=SCALE*DIPC
	END IF
C
C Settings dependent on backend
C
	J=(WNGARA(STH(0))-A_OB)/LB_I 	  		!ADDRESS HEADER
	IF (A_I(J+STH_BEC_I).GE.64.OR.
	1	A_I(J+STH_BEC_I).LT.80) THEN		!DCB
	   LIM(1)=MINDCB
	   LIM(2)=MAXDCB
	   GAMMA=TCDCB
	ELSE						!DLB/DXB
	   LIM(1)=MINDLB
	   LIM(2)=MAXDLB
	   GAMMA=TCDLB
	END IF
C
C CALCULATE BASED ON VALUES FOUND
C
C
C Method 0: Just scaled correlation coefficients
C Method 1: Use Noise source temp     Gain=(TPon-TPoff)/NoiseI
C Method 2: Use fixed receiver gain   Gain=GainI
C Method 3: No Gain correction, Tsys is fixed system temp.
C
C For method 1 and 2: Tsys=TPoff/Gain
C
C
C Change method using WSRT criteria:
C   Method 1 -> 2   if    (TPon - TPoff)/TPon  < 1/75   or   >= 2
C   Method 2 -> 3   if    TPoff                < 600    or   >= 25000  (DCB)
C                         TPoff                < 100    or   >= 15000  (else)
C
	J1=(WNGARA(IFH(0))-A_OB)/LB_I 	  	!ADDRESS HEADER
	J2=(WNGARA(IFH(0))-A_OB)/LB_E 	  	!ADDRESS HEADER
C
	DO I5=0,1				!X,Y
	  DO I4=0,STHTEL-1			!IF's
C
	    TPOFF=TPIN(1,I5,I4)
	    TPON =TPIN(2,I5,I4)
	    GNCAL(I4,I5)=A_I(J1+IFH_GNCAL_I+2*I4+I5)	!Take method from IFH
C
C	    IF (TPON .LT.LIM(1).OR.TPON .GE.LIM(2).OR.
C	1	TPOFF.LT.LIM(1).OR.TPOFF.GE.LIM(2)) THEN
C	       GNCAL(I4,I5)=3.0			 	!Bad TP, use method 3
C	    END IF
C
	    IF (NINT(GNCAL(I4,I5)).EQ.1) THEN
	       R0=(TPON-TPOFF)/TPOFF		 	!FACTOR
C	       IF (R0.LT.MINNOISE .OR. R0.GE.MAXNOISE) THEN
C		 GNCAL(I4,I5)=2.0			!NOISE SOURCE DIED
C	       ELSE
	         TSYS(I4,I5)=A_E(J2+IFH_TNOISEI_E+2*I4+I5)*
	1			(1./R0+GAMMA) 		!TSYS
	         GNOUT(I4,I5)=SCALE*TSYS(I4,I5)		!GAIN CORRECTION
C	       END IF
	    END IF
C
	    IF (NINT(GNCAL(I4,I5)).EQ.2) THEN
	       TSYS(I4,I5)=TPOFF/A_E(J2+IFH_RGAINI_E+2*I4+I5) !FIXED GAIN
	       GNOUT(I4,I5)=SCALE*TSYS(I4,I5)		!GAIN CORRECTION
C
	    ELSE IF (NINT(GNCAL(I4,I5)).EQ.3) THEN
	       TSYS(I4,I5)=A_E(J2+IFH_TSYSI_E+2*I4+I5)	!FIXED TSYS
	       GNOUT(I4,I5)=SCALE*TSYS(I4,I5)		!GAIN CORRECTION
C
	    ELSE IF (NINT(GNCAL(I4,I5)).EQ.0) THEN
	       TSYS(I4,I5)=A_E(J2+IFH_TSYSI_E+2*I4+I5)	!FIXED TSYS
	       GNOUT(I4,I5)=RAWSCALE			!JUST SCALE
	    END IF
C
C***	    GNOUT(I4,I5)=SQRT(GNOUT(I4,I5))		!FACTOR FOR IFR'S
	    GNOUT(I4,I5)=TPOFF/TSYS(I4,I5)
C
	  END DO
	END DO
C
	RETURN
C
C
	END
