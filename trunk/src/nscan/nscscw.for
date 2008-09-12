C+ NSCSCW.FOR
C  WNB 900306
C
C  Revisions:
C	JPH 930901	Original split off from NSCSCR
C	JPH 931006	Fix dimension of DAT (was W:C)
C	JPH 940107	NSCSFW
C	JPH 940218	FLW I*2 --> I*4
C	JPH 960124	Correct description: FLW_I --> FLW_J
C	JPH 960604	Idem: SCH is input only for NSCSCW
C	JPH 960617	Fix error in calculation of MAXD  
C	CMV 031125	Correction of bug in scaling
C
	LOGICAL FUNCTION NSCSCW(FCA,STH0,IFRT,SCN,CAP,CDAP,SCHE)
C
C   NSCSCW_L = NSCSCW( FCA_J:I, STH0_B(0:*):I, 0, SCN_J:I,
C			0, 0, SCH_B(0:*):I)
C	Write the scan header only. The 0 arguments must be present but
C  are not used
C
C   NSCSDW_L = NSCSDW ( FCA_J:I, STH0_B(0:*):I, IFRT_I(0:*):I, SCN_J:I,
C			0, 0, SCH_B(0:*):IO,
C			WGT_E(0:*,0:3):I, CDAT_X(0:*,0:3):I)
C	Write scan number SCN to FCA, using the
C set header STH with interferometers IFRT. The scan header SCH, the data
C weight WGT and the complex data CDAT are given for all four polarisations. 
C NSCSDW converts them to WCS triplets for the number of polarisations actually C present; the FLAGS are all nulled. The SCH field SCH_MAX is set.
C If SCN is outside range, nothing is done and .FALSE. returned.
C
C   NSCSFW_L =  NSCSFW ( FCA_J:I, STH0_B(0:*):I, IFRT_I(0:*):I, SCN_J:I,
C			0, 0, SCH_B(0:*):IO,
C			WGT_E(0:*,0:3):I, CDAT_X(0:*,0:3):I,FLW_J(0:*,0:3):I )
C	As NSCSDW, but instead of using WGT copy FLW directly to flags/weight
C word in SCN file. WGT is used only in selecting points for calculating the
C maximum visibility value.
C
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'STH_O_DEF'		!SET HEADER
	INCLUDE 'SCH_O_DEF'		!SCAN HEADER
C
C  Entry points:
C
	LOGICAL		NSCSDW, NSCSFW
C
C  Parameters:
C
	INTEGER X,Y,XX,XY,YX,YY,W,C,S
	PARAMETER	( X=0,Y=1,
	1		XX=0,XY=1,YX=2,YY=3,
	1		W=0,C=1,S=2)
C
C  Arguments:
C
	INTEGER 	FCA			!FILE CONTROL AREA
	BYTE 		STH0(0:*)		!CURRENT SET HEADER
	INTEGER*2 	IFRT(0:*)		!INTERFEROMETER TABLE
	INTEGER 	SCN			!SCAN TO DO
	INTEGER 	CAP			!APPLY CORRECTIONS (not used)
	INTEGER 	CDAP			!DE-APPLY CORRECTIONS (not used)
	REAL 		SCHE(0:*)		!SCAN HEADER
	REAL 		WGT(0:STHIFR-1,XX:YY)	!WEIGHTS
	REAL 		RDAT(C:S,0:STHIFR-1,XX:YY)!DATA
	INTEGER		FLW(0:STHIFR-1,XX:YY)	!flags/weights (could be I*2!)
C NOTE:
C As long as only SCH E fields need to be addressed, we can use SCHE as a call
C  argument. When other data types are also addressed, we should make a local
C  copy as for STH.
C
C  Function references:
C
	LOGICAL WNFWR			!WRITE DATA
C
C  Data declarations:
C
	INTEGER*2	DAT(W:S,0:4*STHIFR-1)	!visibility write BUFFER:
						!triplets of weight/flags, 
C						!loop indices:
	INTEGER IPL				!polarisation
	INTEGER IFR				!interferometer
	INTEGER IPOL				!polsn number in RDAT
C
	INTEGER SCNP				!scan pointer
	INTEGER IOFS, INX			!offset, index in STH, SCH
C
	REAL	MAXD

	BYTE		STH(0:STHHDL-1)		!SET HEADER
	  INTEGER	STHJ(0:STHHDL/LB_J-1)
	  INTEGER*2	STHI(0:STHHDL/LB_I-1)
	  REAL		STHE(0:STHHDL/LB_E-1)
	  REAL*8	STHD(0:STHHDL/LB_D-1)
	  EQUIVALENCE	(STH,STHJ,STHI,STHE,STHD)
	INTEGER 	NIFR
	INTEGER 	PLN
	INTEGER 	SCNL
C
CC	BYTE		SCH(0:SCHHDL-1)		!SCAN HEADER
CC	  INTEGER	SCHJ(0:SCHHDL/LB_J-1)
CC	  INTEGER*2	SCHI(0:SCHHDL/LB_J-1)
CC	  REAL		SCHE(0:SCHHDL/LB_J-1)
CC	  EQUIVALENCE	(SCH,SCHJ,SCHI,SCHE)
C
	LOGICAL		E_SCW, E_SDW, E_SFW	!entry-point flags
C-
C
C NSCSCW
C
	E_SCW=.TRUE.
	E_SDW=.FALSE.
	E_SFW=.FALSE.
	GOTO 10
C
C
	ENTRY NSCSDW(FCA,STH0,IFRT,SCN,CAP,CDAP,SCHE,WGT,RDAT)
C
	E_SCW=.FALSE.
	E_SDW=.TRUE.
	E_SFW=.FALSE.
	GOTO 10
C
	ENTRY NSCSFW(FCA,STH0,IFRT,SCN,CAP,CDAP,SCHE,WGT,RDAT,FLW)
C
	E_SCW=.FALSE.
	E_SDW=.FALSE.
	E_SFW=.TRUE.
	GOTO 10
C
C Common code
C
10	CONTINUE
	NSCSCW=.TRUE.
	CALL WNGMV (STHHDL,STH0,STH)		!local copy of STH for simple
						!addressing
	IF (SCN.GE.0 .AND. 
	1	SCN.LT.STHJ(STH_SCN_J)) THEN
	  SCNL=STHJ(STH_SCNL_J)			!scan length
	  SCNP=STHJ(STH_SCNP_J)+SCN*SCNL	!file addr. of scan
	  NSCSCW=WNFWR(FCA,SCHHDL,SCHE,SCNP)
	ELSE
	  NSCSCW=.FALSE.			!SCN outside range
	ENDIF
	IF (E_SCW) GOTO 990			!done for NSCSCW
C
C Write data - NSCSDW/NSCSFW only (code adapted from NSCSCR)
C
 	MAXD=-1E30
C
C Make output scan data for PLN polsns from 4-polsn input
C
	PLN=STHI(STH_PLN_I)			!# of pol. in output
	NIFR=STHJ(STH_NIFR_J)
C
C Get maximum and check the scale parameter to prevent out of range integers
C
 	DO IPL=0,PLN-1				!all output polsns
	  IPOL=IPL				!XX[,XY,YX,YY]
	  IF (PLN.EQ.2) IPOL=3*IPL		!XX,YY
	  DO IFR=0,NIFR-1			!all IFRs
	    IF (WGT(IFR,IPOL).NE.0) THEN
	      MAXD=MAX(MAXD,ABS(RDAT(C,IFR,IPOL)))
	      MAXD=MAX(MAXD,ABS(RDAT(S,IFR,IPOL)))
 	    END IF
	  ENDDO !IFR
	ENDDO !IPL
	IF (MAXD/(SCHE(SCH_SCAL_E)+1.).GT.32760) THEN
	   SCHE(SCH_SCAL_E)=MAXD/32760.-1.
	END IF
	IF (MAXD.GT.0) THEN
	   SCHE(SCH_MAX_E)=MAXD
	ELSE
	   CALL WNCTXT(F_TP,'     Empty scan !UJ.',SCN);
	END IF
C
C Copy weights and scaled data
C
 	DO IPL=0,PLN-1				!all output polsns
	  IPOL=IPL				!XX[,XY,YX,YY]
	  IF (PLN.EQ.2) IPOL=3*IPL		!XX,YY
	  DO IFR=0,NIFR-1			!all IFRs
	    IF (E_SDW) DAT(W,IFR*PLN+IPL)=	!take WGT and scale
	1	WGT(IFR,IPOL)/(1.-STHE(STH_WFAC_E))
	    IF (E_SFW) DAT(W,IFR*PLN+IPL)=	!take FLW
	1	FLW(IFR,IPOL)
	    DAT(C,IFR*PLN+IPL)=RDAT(C,IFR,IPOL)
	1	/(SCHE(SCH_SCAL_E)+1.)
	    DAT(S,IFR*PLN+IPL)=RDAT(S,IFR,IPOL)
	1	/(SCHE(SCH_SCAL_E)+1.)
	  ENDDO !IFR
	ENDDO !IPL
C
C Rewrite SCH, write data
C
	IF (.NOT.WNFWR(FCA,SCHHDL,SCHE,SCNP)) GOTO 800
	IF (.NOT.WNFWR(FCA,PLN*3*LB_I*NIFR,
	1	DAT,SCNP+SCHHDL)) GOTO 800
	GOTO 990				!both writes succeeded
 800	  CONTINUE
	  NSCSCW=.FALSE.
	  GOTO 990
C
C Common exit. Return status is set before any branch to here is made
C
990	CONTINUE
	RETURN
C
C
	END	
