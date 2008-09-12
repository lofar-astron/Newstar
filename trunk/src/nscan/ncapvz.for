C+ NCAPVZ.FOR
C  WNB 910930
C
C  Revisions:
C	GvD 920429      Declare WNDPAR as logical iso. integer
C	WNB 921104      Full HA range
C	WNB 930504      Change for new complex solution
C	HjV 930518      Change some text
C	WNB 930825      Add dipole position
C	WNB 930826      New redundant
C	WNB 950611      New least squares
C	JPH 960625      Correct reporting text
C	                Make filler arguments of NSCSCW calls 0 for clarity
C	                Remove NSCMBL, NCARRT calls
C	JPH 960715      Iteration, double reading of scan data
C	                Local apply/deapply masks
C	                Use IF/ENDIF for block structure
C	JPH 960725      Do not add correction to 0 values
C	JPH 960806      Fix previous: Test sum RED+ALG+OTH i.s.o. OTH only
C	                Use cross-correlation XY.XY* to determine phase
C	                Apply phase correction also to dipole corrections
C	JPH 960815      Report gain-difference factor (no action yet)
C	                Emit message on change of algorithm
C	JPH 960824      Emit message only once
C	JPH 970311	atan2(cos,sin) --> atans (-cos,-sin): rotation by pi/2
C	JPH 970403	Add USIGN to XYDIF
C	JPH 971128	Add ifr selection: call NSCMBL, check BASEL(IFR)
C	JPH 980917	Use FCARD for calculation, FCAOUT for output (some 
C			 calc. input was still using FCAOUT)
C
C
	SUBROUTINE NCAPVZ(MDONE)
C
C  Calculate X-Y phase difference
C
C  Result:
C
C	CALL NCAPVZ     will calculate the X-Y phase difference assuming V=0
C	CALL NCAPVA     calculate and apply
C	CALL NCAPVM     apply value asked
C	CALL NCAPVQ     calculated, ask and apply
C
C  PIN reference:
C
C	VZERO_PHASE
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'CBITS_DEF'                     ! apply/deapply bits
	INCLUDE 'NCA_DEF'
	INCLUDE 'LSQ_O_DEF'
	INCLUDE 'STH_O_DEF'                     ! SET HEADER
	INCLUDE 'SCH_O_DEF'                     ! SCAN HEADER
C
C  Parameters:
C
	INTEGER          X,Y
	PARAMETER       (X=0,Y=1)
C
C
C  Arguments:
C
	LOGICAL MDONE                           ! 'message done' flag
C
C  Function references:
C
	LOGICAL WNFWR                     ! READ/write DISK
	CHARACTER*32 WNTTSG                     ! MAKE SUB-GROUP NAME
	REAL WNGENR                             ! NORM. ANGLE
	INTEGER WNGARA                          ! variable address
	LOGICAL WNDPAR                          ! GET USER DATA
	LOGICAL WNMLGA                          ! GET LSQ AREA
	LOGICAL NSCSTL                          ! GET A SET
	LOGICAL NSCSIF                          ! READ IFR TABLE
	LOGICAL NSCSCR                          ! READ DATA FROM SCAN
	LOGICAL NSCSCH                          ! READ SCAN HEADER
	LOGICAL NSCSCW                          ! WRITE SCAN HEADER
C
C  Data declarations:
C
	real a
	REAL BASEL(0:STHIFR-1)                  ! BASELINE TABLE
	COMPLEX CPOLC(0:STHTEL-1,0:1)           ! complex dipole corrns. X,Y
	  EQUIVALENCE (CPOLC,STHE(STH_POLC_E))
	COMPLEX CE(0:1)                         ! phase factor
	LOGICAL LCALC                           ! CALCULATE
	LOGICAL LAPPLY                          ! APPLY
	LOGICAL LASK                            ! ASK VALUE
	INTEGER LCAP                            ! apply/deapply masks
	INTEGER FCARD                           ! local copy of input FCA
	INTEGER INXRD                           ! pointer to corresponding SETS
	REAL XYDIF                              ! phase-zero diff
	INTEGER SETNAM(0:7)                     ! FULL SET NAME
	INTEGER CSTNAM(0:7)                     ! CHECK SET NAME
	  DATA CSTNAM/8*-1/
	INTEGER*2 IFRT(0:STHIFR-1)              ! INTERFEROMETER TABLE
	INTEGER IFRA(0:1,0:STHIFR-1)
	REAL ANG(0:2,0:STHIFR-1)
	REAL HA                                 ! HOUR-ANGLE
	REAL WGT(0:STHIFR-1,0:3)                ! DATA WEIGHTS XX,XY,YX,YY
	REAL DAT(0:1,0:STHIFR-1,0:3)            ! DATA XX,XY,YX,YY without
	  COMPLEX DATC(0:STHIFR-1,0:3)          !  dipole correction
	  EQUIVALENCE (DAT,DATC)
	COMPLEX CF
	  DATA CF/1/
	COMPLEX D(3)                            ! data XY.YX*, XY, YX
	COMPLEX CSOL(3),CME(3)                  ! solutions and ME
	REAL RSOL(2), RME(2)                    ! temporaries
	REAL MU(3),SD(3)
	INTEGER MAR                             ! LSQ AREA
	INTEGER STHP                            ! POINTER TO SET HEADER
	BYTE STH(0:STHHDL-1)                    ! SET HEADER
	  INTEGER*2 STHI(0:STHHDL/2-1)
	  INTEGER STHJ(0:STHHDL/4-1)
	  REAL STHE(0:STHHDL/4-1)
	  REAL*8 STHD(0:STHHDL/8-1)
	  EQUIVALENCE(STH,STHI,STHJ,STHE,STHD)
	BYTE SCH(0:SCHHDL-1)                    ! SCAN HEADER
	  INTEGER*2 SCHI(0:SCHHDL/2-1)
	  INTEGER SCHJ(0:SCHHDL/4-1)
	  REAL SCHE(0:SCHHDL/4-1)
	  REAL*8 SCHD(0:SCHHDL/8-1)
	  EQUIVALENCE(SCH,SCHI,SCHJ,SCHE,SCHD)
	INTEGER IFR, ISCN, ITL, IXY             ! loop indices
C-
	LCALC=.TRUE.
	LAPPLY=.FALSE.
	LASK=.FALSE.
	GOTO 10
C
C NCAPVA
C
	ENTRY NCAPVA(MDONE)
C
	LCALC=.TRUE.
	LAPPLY=.TRUE.
	LASK=.FALSE.
	GOTO 10
C
C NCAPVM
C
	ENTRY NCAPVM(MDONE)
C
	LCALC=.FALSE.
	LAPPLY=.TRUE.
	LASK=.TRUE.
	GOTO 10
C
C NCAPVQ
C
	ENTRY NCAPVQ(MDONE)
C
	LCALC=.TRUE.
	LAPPLY=.TRUE.
	LASK=.TRUE.
	GOTO 10
C
C CALCULATE
C
 10	CONTINUE
	LCAP=CAP_TELMSK+CAP_IFRMSK+CAP_POL
	XYDIF=0                                 ! ASSUME 0
	CALL WNCTXT (F_TP, ' ')
	IF (LCALC) THEN
C
C INIT
C
	  IF (.NOT.WNMLGA(MAR,LSQ_T_COMPLEX+LSQ_T_MULTIPLE,1,3)) THEN
						! GET LSQ AREA
	    CALL WNCTXT(F_TP,'ERROR: Cannot obtain work area')
	    GOTO 200
	  END IF
C
C Read FCA? INP if it is open (COPY) FCAOUT otherwise
C
	  IF (FCAINP.NE.0) THEN                 ! VZERO COPY
	    FCARD=FCAINP
	    INXRD=WNGARA(SETINP)
	  ELSE	                                ! VZERO CALC or APPLY
	    FCARD=FCAOUT
	    INXRD=WNGARA(SETS)
	  ENDIF
	  INXRD=(INXRD-A_OB)/LB_J
C
	  DO WHILE (NSCSTL(FCARD,A_J(INXRD),STH(0),STHP,SETNAM,LPOFF))
C
C GET IFR TABLES
C
	    IF (STHI(STH_PLN_I).NE.4) THEN      ! CANNOT USE
	      CALL WNCTXT(F_TP,'Sector !AS has only !UI polarisations',
	1	        WNTTSG(SETNAM,0),STHI(STH_PLN_I))
	      GOTO 20                           ! NEXT SET
	    END IF
	    IF (.NOT.NSCSIF(FCARD,STH,IFRT,IFRA,ANG)) THEN
						! READ IFR TABLE
	      CALL WNCTXT(F_TP,'!/Error reading IFR table !AS',
	1	        WNTTSG(SETNAM,0))
	      GOTO 20                           ! TRY NEXT SET
	    END IF
  	    CALL NSCMBL(STHE(STH_RTP_E),STHJ(STH_NIFR_J),IFRT,
  	1                       SIFRS,BASEL)    ! BASELine table, >=0 selected
 
C
C SHOW CURRENT SET
C
	    DO I1=0,3
	      IF (CSTNAM(I1).NE.SETNAM(I1)) THEN
		DO I2=0,3
		  CSTNAM(I2)=SETNAM(I2)
		END DO
		CALL WNCTXT(F_TP,'Source sector: !AS',WNTTSG(CSTNAM,0))
	      END IF
	    END DO
C
C DO SCANS
C
	    DO ISCN=0,STHJ(STH_SCN_J)-1         ! ALL SCANS
	      HA=STHE(STH_HAB_E)+ISCN*STHE(STH_HAI_E)
						! HA OF SCAN
	      IF (HA.LT.HARAN(1) .OR. HA.GT.HARAN(2)) GOTO 30
						! FORGET
C
C GET DATA
C
	      IF (.NOT.NSCSCR(FCARD,STH,IFRT,ISCN,
						! read with dipole corrn
	1	        LCAP,0,SCH,WGT,DATC)) THEN
		CALL WNCTXT(F_TP,'!7$EAF7.2 Error reading scan data',HA)
		GOTO 20                         ! TRY NEXT SET
	      END IF
	      DO IFR=0,STHJ(STH_NIFR_J)-1       ! ALL IFRS
		IF (BASEL(IFR).GT.0) THEN	! selected?
		  IF (WGT(IFR,1).GT.0 .AND. WGT(IFR,2).GT.0) THEN
		    D(1)=DATC(IFR,2)*CONJG(DATC(IFR,1))
						! XY.YX*
		    D(2)=DATC(IFR,1)              ! XY
		    D(3)=DATC(IFR,2)              ! YX
		    CALL WNMLMN(MAR,LSQ_C_COMPLEX,CF,1.,D)
						! accumulate
		  ENDIF
		ENDIF
	      END DO
 30	      CONTINUE
	    ENDDO                               ! scans
C
C NEXT SET
C
 20	    CONTINUE
	  END DO
C
C MAKE SOLUTION
C
	  CALL WNMLID(MAR)                      ! FIX MISSING TEL.

	  CALL WNMLTN(MAR)                      ! TRIANGLE
	  CALL WNMLSN(MAR,CSOL,MU,SD)           ! GET SOLUTION
	  CALL WNMLME(MAR,CME)                  ! GET M.E.
	  CALL WNMLFA(MAR)                      ! FREE MATRICES
C
C The solution for XYDIF has a PI ambiguity due to the factor .5; this is 
C  resolved by adding USIGN, which is 0 for Stokes U<0 and PI for U>0. USIGN is
C  set by NCADAT
C
 	  IF (ABS(CSOL(1)).NE.0) THEN
	    XYDIF=.5*ATAN2( -AIMAG(CSOL(1)),-REAL(CSOL(1)) ) + USIGN
	  ELSE
	    XYDIF=0
	  ENDIF
 	  RSOL(1)=ABS(CSOL(2))                  ! |XY| average
	  RSOL(2)=ABS(CSOL(3))                  ! |YX| average
	  RME(1)=REAL(CME(2))/RSOL(1)           ! XY rel. error
	  RME(2)=REAL(CME(3))/RSOL(2)           ! YX rel. error
	  RSOL(1)=SQRT(RSOL(1)/RSOL(2))         ! |XY/YX|
	  RME(1)=.5*(RME(1)+RME(2))*RSOL(1)     !  and its error
	  CALL WNCTXT(F_TP,'
	1!/Average of XY.YX*: !EC9.3 (!E9.3)
	1!/X-Y phase-zero difference to be added: !EAR9.1 deg
	1!/XY/YX gain-difference factor: !E9.3 (!E9.3)',
	1	CSOL(1),REAL(CME(1)), XYDIF, RSOL(1), RME(1) )
C
	ENDIF	                                ! calc
C
C ASK
C
	IF (LASK) THEN                          ! NO ASK
	  IF (.NOT.WNDPAR('VZERO_PHASE',XYDIF,LB_E,J0,A_B(-A_OB),
	1	        XYDIF*DEG,1)) THEN
	    RETURN                              ! READY
	  END IF
	  IF (J0.LE.0) RETURN                   ! STOP
	  XYDIF=WNGENR(XYDIF*RAD)               ! MAKE RADIANS
	ENDIF
C
C APPLY
C
	IF (LAPPLY) THEN
C
C Message
C
	  IF (.NOT.MDONE) THEN
	    CALL WNCTXT(F_TP,'
	1!/!4C\In July 1996 an error in Newstar polarisation processing was
	1!/!4C\discovered: It is necessary to adjust the dipole correction
	1!/!4C\terms (orientations and ellipticities) when a phase-zero
	1!/!4C\difference correction is applied. This was never done in Newstar
	1!/!4C\(nor probably in any prior WSRT software); the fact that this
	1!/!4C\error was not detected earlier suggests that its impact on
	1!/!4C\astronomical results has been limited.')
	    CALL WNCTXT(F_TP,'
	1!/!4C\NCALIB now does apply this correction. As a result, you may find
	1!/!4C\differences between present results and earlier ones obtained
	1!/!4C\with the same input data. !/')
	    MDONE=.TRUE.
	  ENDIF
C
C DO SETS
C
	  CE(X)=EXP(CMPLX(0.,-XYDIF))           ! factors for dipole
	  CE(Y)=CONJG(CE(X))                    !  corrections
	  CSTNAM(0)=-1                          ! reset
	  DO WHILE(NSCSTL(FCAOUT,SETS,STH(0),STHP,SETNAM,LPOFF))
						! NEXT SET
C
C SHOW CURRENT SET
C
	    DO IFR=0,3
	      IF (CSTNAM(IFR).NE.SETNAM(IFR)) THEN
		DO I2=0,3
		  CSTNAM(I2)=SETNAM(I2)
		END DO
		CALL WNCTXT(F_TP,'Target sector: !AS',WNTTSG(CSTNAM,0))
	      END IF
	    END DO
C
C Adjust dipole corrections
C
	    DO ITL=0,STHTEL-1
	      DO IXY=X,Y
		IF (CPOLC(ITL,IXY).NE.0)
	1	  CPOLC(ITL,IXY)=CPOLC(ITL,IXY)*CE(IXY)
	      ENDDO
	    ENDDO
	    IF (.NOT.WNFWR(FCAOUT,STHHDL,STH,STHP)) THEN
						! WRITE CORRECTIONS
	      CALL WNCTXT(F_TP,'Error writing dipole corrections in sector !AS',
	1	        WNTTSG(SETNAM,0))
	    END IF
C
C DO SCANS
C
	    DO ISCN=0,STHJ(STH_SCN_J)-1         ! ALL SCANS
C
C INIT
C
	      HA=STHE(STH_HAB_E)+ISCN*STHE(STH_HAI_E)
						! HA OF SCAN
	      IF (HA.LT.HARAN(1) .OR. HA.GT.HARAN(2)) GOTO 130
						! FORGET
C
C GET DATA
C
	      IF (.NOT.NSCSCH(FCAOUT,STH,IFRT,ISCN,CORAP,CORDAP,
	1	                SCH)) THEN      ! READ SCAN HEADER
		CALL WNCTXT(F_TP,'!7$EAF7.2 Error reading scan header',HA)
		GOTO 120                        ! TRY NEXT SET
	      END IF
C
C APPLY CORRECTION only to telescopes that are present
C
	      DO ITL=0,STHTEL-1                 ! ALL TELESCOPES
		IF     (SCHE(SCH_REDC_E+2*ITL+1)+
	1	        SCHE(SCH_ALGC_E+2*ITL+1)+
	1	        SCHE(SCH_OTHC_E+2*ITL+1).NE.0) THEN
		  SCHE(SCH_OTHC_E+2*ITL+1)=
	1	        WNGENR(SCHE(SCH_OTHC_E+2*ITL+1)-XYDIF)
						! APPLY ANGLE
		ENDIF
	      ENDDO
C
C WRITE SCAN HEADER
C
	      IF (.NOT.NSCSCW(FCAOUT,STH,IFRT,ISCN,0,0,
	1	                SCH)) THEN      ! WRITE SCAN HEADER
		CALL WNCTXT(F_TP,'!7$EAF7.2 Error writing scan header',HA)
		GOTO 120                        ! TRY NEXT SET
	      END IF
C
C NEXT SCAN
C
 130	      CONTINUE
	    END DO                              ! END SCANS
C
C NEXT SET
C
 120	    CONTINUE
	  END DO                                ! sectors
	ENDIF	                                ! apply
C
C READY
C
  200	CONTINUE
C
	RETURN	                                ! READY
C
C
	END

