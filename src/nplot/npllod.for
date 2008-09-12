C+ NPLLOD.FOR
C  HjV 940111
C       Combined parts of old version of NPLTEL and NPLRES
C       Solve small bug with PLUVO plot
C       
C  Revisions:
C       HjV 940530	Plot different datatypes on one page
C	CMV 940822	Option to abort during loop of plots
C	JPH 960622	Interchange order of data-type and polarisation loops
C	JPH 960726	Donot reenable control-C
C	JPH 960730	IFR_MODE INVERT
C	HjV 970723	Remove control-C stuff (commented out with CCC)
C
	SUBROUTINE NPLLOD
C
C  Load telescope/residual data/errors for NPLOT
C
C  Result:
C
C	CALL NPLLOD		Load telescope/residual data/errors
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'CBITS_DEF'
	INCLUDE 'STH_O_DEF'		!SET HEADER
	INCLUDE 'NPL_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
C
C  Function references:
C
	LOGICAL NMOMSL			!CALCULATE MODEL DATA
CCC	INTEGER WNGCCN			! acknowledge and enable control-C
C
C  Data declarations:
C
	INTEGER NHV(0:1)		!# OF PAGES
	INTEGER LDATTP			!CURRENT DATA TYPE
	INTEGER POLCOD(0:3)		!POLARISATION CODE
	  DATA POLCOD/XX_P,XY_P,YX_P,YY_P/
	INTEGER IPOL			!CURRENT POL. BITS
	INTEGER IFRS(0:1)		!CURRENT IFR'S
	BYTE STH(0:STH__L-1)		!SET HEADER
	  INTEGER*2 STHI(0:STH__L/LB_I-1)
	  INTEGER STHJ(0:STH__L/LB_J-1)
	  REAL STHE(0:STH__L/LB_E-1)
	  DOUBLE PRECISION STHD(0:STH__L/LB_D-1)
	  EQUIVALENCE (STH,STHI,STHJ,STHE,STHD)
C-
C
C INIT SOURCE MODEL
C
	IF (OPT.NE.'TEL') THEN
	  IF (NSRC(0).GT.0) THEN				!MODEL WANTED
	    IF (.NOT.NMOMSL(FCAIN,SETS,LPOFF)) THEN	!SET MODEL DATA
	      CALL WNCTXT(F_TP,'Error in model calculation')
	      RETURN
	    END IF
	  END IF
	END IF
C
C Load data ...
C
	DO IPOL=0,3
	  DO LDATTP=1,NDATTP
CCC	    CALL WNGCCD					! trap control-C, zero
							!  count
	    IF (IAND(SPOL,POLCOD(IPOL)).NE.0) THEN	!SELECTED THIS POL.
	      IF (IFR_MODE.EQ.'NORMAL' .OR.
	1	  IFR_MODE.EQ.'INVERT' .OR.
	1	  IFR_MODE.EQ.'SORT') THEN		!SCN FILE: (HA,IFRS)
C from SCN file as (HA,IFR) per CHAN
		 CALL NPLDIF (LDATTP,IPOL,NHV)
	      ELSE IF (IFR_MODE.EQ.'SPECTRAL') THEN	!SCN DATA: (HA,CHAN)
C from SCN file as (HA,CHAN) per IFR
	        DO I1=0,STHTEL-1
		  IF (OPT.EQ.'TEL') THEN
	            IF (STELS(I1)) THEN	        	!SELECTED
		        IFRS(0)=I1
		        CALL NPLDCH(LDATTP,IPOL,NHV,IFRS)
		    END IF
		    IF (NO_MORE) RETURN 
	          ELSE
	            DO I2=I1,STHTEL-1
	              IF (SIFRS(I1,I2)) THEN		!SELECTED
		        IFRS(0)=I1
		        IFRS(1)=I2
		        CALL NPLDCH(LDATTP,IPOL,NHV,IFRS)
		      END IF
	 	      IF (NO_MORE) RETURN 
	            END DO
		  END IF
		END DO
	      ELSE IF (IFR_MODE.EQ.'BAND') THEN	        !SCN FILE: (CHAN,IFR)
C from SCN file as (CHAN,IFR) per HA
		 CALL NPLDHA (LDATTP,IPOL,NHV)
	      ELSE                      		!ERROR
C No other options yet...
		CALL WNCTXT('Unknown IFR_MODE !AS',IFR_MODE)
	      ENDIF
	    END IF
CCC	    IF (WNGCCN().GT.1) NO_MORE=.TRUE.	! acknowledge double control-C
CCC 	    CALL WNGCCS(1)			! simulate (if none seen)
	    IF (NO_MORE) RETURN
	  END DO
	END DO
C
C CLOSE PLOT-DEVICE
C
	IF ((PPP(1).GT.1).OR.(PPP(2).GT.1)) THEN
	  CALL NPLCLO(DQID,NHV)
	  PPPNR=-1
	END IF
C
	RETURN
C
	END
