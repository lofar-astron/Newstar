C+ NPLTEL.FOR
C  WNB 910617
C
C  Revisions:
C	WNB 910815	Phase scale
C	WNB 911009	Limit phase errors
C	WNB 911217	Convert to WQ
C	WNB 920130	New multiple pages
C	WNB 920403	Stop infinite loop for YY polarisation
C	WNB 920403	Rearrange pol. test 
C	WNB 920901	Add PLUVO
C	WNB 921104	Cater for large HA
C	HjV 930311	Change some text
C	WNB 930608	New flags
C	HJV 930618	Symbolic names for mask bits in CBITS_O_DEF
C	WNB 930803	CBITS_DEF
C	WNB 930824	New STELS
C	WNB 930825	Polarisation codes
C       HjV 940113      Removed some parts to new subroutines
C       HjV 940224      Add mosaik test, add filling of FNAM and OBSYD
C       HjV 940324      Better check for MOSAIK with BANDPASS
C       HjV 940413	Changed check for opening plot
C	CMV 940426	Add IFData option
C       HjV 940506	Changed place for opening plot
C	CMV 940822	Option to abort during loop of plots
C       WNB 950120      Correct Y polarisation (#135)
C       HjV 950206	Call NSCGF1 only when NO mosaik
C	JPH 960402	ST_MODE: Plotting versus ST i.s.o. HA
C       HjV 960415      Correct option to stop during loop or more plots per 
C			 page
C	JPH 960619	ST_MODE integer, copy to INIT. - Report progress
C	JPH 960726	SETNAM in NPLSST call
C	JPH 960805	ST_INIT in common
C	JPH 960814	Test WNGCCN >1 i.s.o. >0 at end of routine
C	JPH 9611..	HA integration
C	JPH 961204	100*(exp(gn)-1) --> 100*gn
C	JPH 961210	Set output undefined if no data after HA integration
C	JPH 970123	Use 100*(gain-1) for gain<0, 100*ln(gain) for gain>0.
C	JPH 970124	Call NPLPBE(.FALSE.,...) immediately after 
C	 		 NPLPBE(.TRUE.,...) to draw raster first
C	HjV 970723	Remove control-C stuff (commented out with CCC)
C	JPH 970825	Restore control-C. - NO_MORE exits through 1000
C	JPH 980113	Phase continuity
C	JPH 980127	Remove WNGCxx calls: control-C interaction abandoned
C			 because of problems with multiple plots per page
C       AXC 010709      Linux port - .NE.0 changed
C	CMV 030628	Removed phase continuity
C
	SUBROUTINE NPLTEL  (LDATTP,IPOL,NPLOT,PTXT,NHV,TABIFR,SCHAN,IFRS)
C
C  Fill buffers for plotting telescope errors of individual scans
C
C  Result:
C
C	CALL NPLTEL		Fill buffers for plotting telescope errors
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'CBITS_DEF'
	INCLUDE 'STH_O_DEF'		!SET HEADER
	INCLUDE 'SCH_O_DEF'		!SCAN HEADER
	INCLUDE 'NPL_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER LDATTP			!CURRENT DATA TYPE
	INTEGER IPOL
	INTEGER NPLOT			!# OF TELESCOPES TO DO
	INTEGER PTXT(MXNCHN)		!CROSS CHANNELS
	INTEGER NHV(0:1)		!# OF PAGES
	INTEGER TABIFR(0:STHTEL-1,0:STHTEL-1) !IFR PLOT POINTERS
	INTEGER SCHAN(0:MXNCHN-1)	!CHANNELS TO DO
	INTEGER IFRS(0:1)		!CURRENT TELESCOPE
C
C  Function references:
C
	REAL WNGENR			!LIMIT PHASE
	LOGICAL NSCSTL			!GET A SET
	LOGICAL NSCSCH			!READ SCAN HEADER
	LOGICAL NSCGIF			!Read IF data
	LOGICAL NSCGF1			!Get some data from IF header
	CHARACTER*32 WNTTSG		! ASCII sector name
CC	INTEGER WNGCCN			! control-C check
C
C  Data declarations:
C
	LOGICAL OPENSW			!PLOT OPEN OR CLOSED
	INTEGER NCHAN                   !# OF CHANNELS TO DO
	INTEGER IFRA(0:1,0:STHIFR-1)
	LOGICAL PLTIFR(0:STHIFR-1)      !.TRUE.= PLOT THIS INTERFEROMETER
	INTEGER UFL			!FLAGS TO DISCARD
	REAL LHA			!LOCAL HA
 	REAL PHASE			!PHASE CORRECTION
	REAL AMPL			!AMPLITUDE CORRECTION
	REAL HAB,HAI			!START HA, INCREMENT
	INTEGER NTINT			! # scans for ha integration
	INTEGER NSCN			!NUMBER OF SCANS
	INTEGER TCP			!TEL. CORR. POINTER
	INTEGER STHP			!SET HEADER POINTER
	BYTE STH(0:STH__L-1)		!SET HEADER
	  INTEGER*2 STHI(0:STH__L/LB_I-1)
	  INTEGER STHJ(0:STH__L/LB_J-1)
	  REAL STHE(0:STH__L/LB_E-1)
	  DOUBLE PRECISION STHD(0:STH__L/LB_D-1)
	  EQUIVALENCE (STH,STHI,STHJ,STHE,STHD)
	BYTE SCH(0:SCH__L-1)		!SCAN HEADER
	  INTEGER*2 SCHI(0:SCH__L/LB_I-1)
	  INTEGER SCHJ(0:SCH__L/LB_J-1)
	  REAL SCHE(0:SCH__L/LB_E-1)
	  DOUBLE PRECISION SCHD(0:SCH__L/LB_D-1)
	  EQUIVALENCE (SCH,SCHI,SCHJ,SCHE,SCHD)
	REAL IFBUF(0:STHTEL-1,0:1)	!BUFFER FOR IF-DATA
	REAL AMCO(2,0:STHIFR-1,0:MXNCHN-1)!DATA VALUES PER INTERF. PER HA
	INTEGER NRAMCO(0:STHIFR-1,0:MXNCHN-1)  
					!# OF POINTS IN AMCO FOR BAND OPTION
	REAL TMPVAL(2)                  !TEMP. VALUE A/P OR C/S
        INTEGER CSET(0:7,0:1)           !TEST SET NAMES
	LOGICAL REPORT			! 'new plot' flag
	INTEGER ISCN,ISCN0,ISCN1,ICS,ITL! loop indices
	INTEGER N			! averaging counter
	REAL SUM			! acumulator
	LOGICAL	FIRST			! 'first scan' flag
C-
C
C
	CALL WNDDUF(UFL)			        !GET UNFLAG DATA
	UFL=IAND(FL_ALL,NOT(UFL))		        !SELECTOR
C
C  INIT PLOT
C
	FIRST=.TRUE.			! no previous scan yet
	ST_INIT=ST_MODE
	REPORT=.TRUE.
	NCHAN=0
	CSET(0,0)=-1
	CSET(0,1)=-1
	OPENSW=.FALSE.
	IF (IFR_MODE.EQ.'BAND') THEN
	  DO I1=0,MXNCHN-1                         	!CLEAR
	    DO I2=0,STHIFR-1
	      AMCO(1,I2,I1)=1E20
	      AMCO(2,I2,I1)=1E20
	    END DO
	  END DO
	END IF
C
C  FILL TEL DATA IN BUFFER
C
	DO WHILE(NSCSTL(FCAIN,SETS,STH(0),STHP,SETNAM,LPOFF))  !NEXT SET
	  CALL WNDSTI(FCAIN,SETNAM)			!PROPER NAME
	  IF (REPORT) CALL WNCTXT(F_T,'Next plot, first sector: !AS',
	1	WNTTSG(SETNAM,0) )
	  REPORT=.FALSE.
	  CALL WNGMTS(STH_FIELD_N,STH(STH_FIELD_1),FNAM) !SET FIELD NAME
	  OBSDY(1)=STHI(STH_OBS_I)                      !OBS. DAY
	  OBSDY(2)=STHI(STH_OBS_I+1)                    !OBS. YEAR
	  IF (IFR_MODE.NE.'BAND') THEN
	    NCHAN=NCHAN+1
	    DO I1=0,MXNCHN-1				!CLEAR
	      DO I2=0,STHIFR-1
	        AMCO(1,I2,I1)=1E20
	        AMCO(2,I2,I1)=1E20
	      END DO
	    END DO
	  ELSE
	    IF (MOSAIK) THEN
	      IF (CSET(0,0).EQ.-1) THEN
                DO I1=0,7
                  CSET(I1,0)=SETNAM(I1)
                END DO
	      END IF
	      IF (CSET(0,1).EQ.-1) THEN
		NCHAN=NCHAN+1
		DO I2=0,STHIFR-1
	          NRAMCO(I2,NCHAN-1)=0
	        END DO
	      ELSE
                IF ((CSET(0,1).NE.SETNAM(0)).OR.
	1	      (CSET(1,1).NE.SETNAM(1)).OR.
	1	      (CSET(2,1).NE.SETNAM(2)).OR.
	1	      (CSET(3,1).NE.SETNAM(3))) THEN
		   NCHAN=NCHAN+1
		   DO I2=0,STHIFR-1
		     NRAMCO(I2,NCHAN-1)=0
		   END DO
                END IF
              END IF
	      DO I1=0,7
                CSET(I1,1)=SETNAM(I1)
	      END DO
	    ELSE
	      NCHAN=NCHAN+1
	      DO I2=0,STHIFR-1
	        NRAMCO(I2,NCHAN-1)=0
	      END DO
            END IF
	  END IF
	  IF (ST_MODE.NE.0) THEN
	    CALL NPLSST (STHD,STHE(STH_HAB_E),SETNAM,HAB)
	  ELSE
	    HAB=STHE(STH_HAB_E)	
	  ENDIF						!START HA
	  HAI=STHE(STH_HAI_E)				!HA INCREMENT
	  NSCN=STHJ(STH_SCN_J)
	  IF (.NOT.MOSAIK) THEN
	    IF (IF_MODE.NE.' ') JS=NSCGF1(FCAIN,STH,HAB,HAI,NSCN) !GET FROM IFH
	  END IF
	  DO I0=0,NSCN-1				!ALL SCANS
	    LHA=HAB+I0*HAI				!HA
	    IF (LHA.LT.HARA(0) .OR. LHA.GT.HARA(1)) GOTO 50 !NEXT SCAN
C
	    IF (IF_MODE.EQ.' ') THEN
	      IF (.NOT.NSCSCH(FCAIN,STH,0,I0,0,0,SCH)) THEN !READ SCAN HEADER
	         CALL WNCTXT(F_TP,'Error reading scan header')
	         GOTO 50				!NEXT SCAN
	      END IF
	      IF (IAND(SCHJ(SCH_BITS_J),UFL).NE.0) GOTO 50 !DELETE SCAN
	    ELSE					!GET IF DATA
	      IF (.NOT.NSCGIF(IF_MODE,FCAIN,STH,
	1		LHA,LHA,IFBUF)) GOTO 50 	!IGNORE SCAN
	    END IF
C
	    I5=0					!TEL COUNT
	    DO I1=0,STHTEL-1				!ALL TELESCOPES
	      IF ((.NOT.PLUVO .AND. STELS(I1).NE.0) .OR.
	1	       (PLUVO .AND. I1.EQ.IFRS(0))) THEN
	        IF (PLUVO) THEN
		  I4=SCHAN(SETNAM(3))			!PLOT POINTER
	        ELSE
		  I4=I5					!PLOT POINTER
	        END IF
	        PHASE=0.				!PHASE
	        AMPL=0.					!AMPLITUDE
C
	        IF (IF_MODE.EQ.' ') THEN		!GET CORRECTIONS 
		  TCP=2*STHTEL*(IPOL/2)+2*I1            !TEL. CORR. POINTER
		  IF (IAND(CORAP,1).NE.0) THEN		!RED. ASKED
		    AMPL=AMPL+SCHE(SCH_REDC_E+TCP+0)	!GAIN
		    PHASE=PHASE+SCHE(SCH_REDC_E+TCP+1)	!PHASE
		  END IF
		  IF (IAND(CORAP,2).NE.0) THEN		!ALIGN ASKED
		    AMPL=AMPL+SCHE(SCH_ALGC_E+TCP+0)	!GAIN
		    PHASE=PHASE+SCHE(SCH_ALGC_E+TCP+1)	!PHASE
		  END IF
		  IF (IAND(CORAP,4).NE.0) THEN		!OTHERS ASKED
		    AMPL=AMPL+SCHE(SCH_OTHC_E+TCP+0)	!GAIN
		    PHASE=PHASE+SCHE(SCH_OTHC_E+TCP+1)	!PHASE
		  END IF
		  IF (IAND(CORDAP,4).NE.0) THEN		!OTHERS DEAPPLY ASKED
		    AMPL=AMPL-SCHE(SCH_AOTHC_E+TCP+0)	!GAIN
		    PHASE=PHASE-SCHE(SCH_AOTHC_E+TCP+1)	!PHASE
		  END IF
		  PHASE=WNGENR(PHASE)*DEG		!PHASE IN DEGREES
C
C Use linear scale for ln(gain)<0, logarithmic for >0. For gain values close
C  to 0, ln becomes too large; on the large side, ln compresses the scale which
C  helps keeping large gains within plot range. For the most common case of
C  small gain errors, the two functions are approximately the same.
C
		  IF (AMPL.GT.0) AMPL=100*AMPL		! ln%
		  IF (AMPL.LT.0.) AMPL=100.*(EXP(AMPL)-1)! %
 	        ELSE					!GET IF DATA
		  AMPL=IFBUF(I1,IPOL/2)			!CAN BE ONLY X/Y
	        END IF
C
		IF ((DATTYP(LDATTP)(1:2).EQ.'AP').OR.
	1	    (DATTYP(LDATTP)(1:2).EQ.'AM').OR.
	1	    (DATTYP(LDATTP)(1:2).EQ.'PH')) THEN !AMP./PHASE
		  TMPVAL(1)=AMPL
C
C Phase continuity
C
C		  IF (FIRST) THEN
C		    FIRST=.FALSE.
C		  ELSE
C		    IF (PHASE-AMCO(2,I4,I0-1) .LT.-PI) PHASE=PHASE+PI2
C		    IF (PHASE-AMCO(2,I4,I0-1) .GT. PI) PHASE=PHASE-PI2
C		  ENDIF
		  TMPVAL(2)=PHASE
		ELSE IF ((DATTYP(LDATTP)(1:2).EQ.'CS').OR.
	1	    (DATTYP(LDATTP)(1:2).EQ.'CO').OR.
	1	    (DATTYP(LDATTP)(1:2).EQ.'SI')) THEN !COS/SIN
		  TMPVAL(1)=AMPL*COS(PHASE/360*PI2)     !COSINE
		  TMPVAL(2)=AMPL*SIN(PHASE/360*PI2)     !SINE
		END IF
		IF (IFR_MODE.EQ.'BAND') THEN
		  NRAMCO(I4,NCHAN-1)=NRAMCO(I4,NCHAN-1)+1
		  DO I2=1,2
		    IF (AMCO(I2,I4,NCHAN-1).EQ.1E20) THEN
		      AMCO(I2,I4,NCHAN-1)=TMPVAL(I2)
		    ELSE
		      AMCO(I2,I4,NCHAN-1)=AMCO(I2,I4,NCHAN-1)+TMPVAL(I2)
		    ENDIF
		  END DO
	        ELSE
		  AMCO(1,I4,I0)=TMPVAL(1)
		  AMCO(2,I4,I0)=TMPVAL(2)
		ENDIF
	        I5=I5+1
	      END IF
	    ENDDO				! telescopes
  50	    CONTINUE
CC	    I2=WNGCCN()				! nr of control-C seen
CC	    IF (I2.GT.1) NO_MORE=.TRUE.
CC	    IF (I2.NE.0) GOTO 501
	  ENDDO					! scans
C
C HA integration
C
	  IF (IFR_MODE.NE.'BAND') THEN
	    NTINT=MAX(1,				! scans to integrate
	1	NINT(HAINT/24./3600./STHE(STH_HAI_E)))
	    HAI=NTINT*HAI				! new HA increment
	    DO ITL=0,STHTEL-1
 	      DO ISCN0=0,STHJ(STH_SCN_J)-1,NTINT	! integration intervals
		ISCN1=MIN(ISCN0+NTINT,STHJ(STH_SCN_J))-1
 	        DO ICS=1,2
		  N=0
		  SUM=0
 	          DO ISCN=ISCN0,ISCN1			! integrate
		    IF (AMCO(ICS,ITL,ISCN).NE.1E20 .AND.
	1		AMCO(ICS,ITL,ISCN).NE.0) THEN	! defined?
 	              SUM=SUM+AMCO(ICS,ITL,ISCN)
 		      N=N+1
		    ENDIF
  	          ENDDO
		  IF (N.NE.0) THEN
		    SUM=SUM/N
		  ELSE
		    SUM=1E20				! set undefined
		  ENDIF
 	          DO ISCN=ISCN0,ISCN1
	            AMCO(ICS,ITL,ISCN)=SUM
 	          ENDDO
 	        ENDDO
	      ENDDO
	    ENDDO
C
C PLOT PER TEL
C
	    IF (.NOT.OPENSW) THEN
	      IF ((.NOT.MOSAIK.AND..NOT.PLOTAP).OR.
	1    	((MOSAIK.AND.(IFR_MODE.NE.'BAND'))))THEN
		CALL NPLOPN (LDATTP,IPOL,NHV,IFRS)	!OPEN PLOT, PLOT HEADING
                IF (NO_MORE) GOTO 1000			!USER SAID: STOP
		CALL NPLPBE (.TRUE., NPLOT,PTXT,NHV,NCHAN)
							!PLOT BEGIN-ANNOTATIONS
	        CALL NPLPBE (.FALSE.,NPLOT,PTXT,NHV,NCHAN)
							!PLOT END-ANNOTATIONS
		OPENSW=.TRUE.
	      END IF
	    END IF
            CALL NPLPLT (LDATTP,IPOL,NPLOT,PTXT,NHV,TABIFR,SCHAN,IFRS,
	1      NSCN,STHTEL,HAB,HAI,PLTIFR,IFRA,AMCO)
	    IF (NO_MORE) GOTO 1000				!USER SAID: STOP
	  ELSE						! mode= band
C CALCULATE AVERAGE
	    IF (.NOT.MOSAIK) THEN
	     DO I4=0,STHTEL-1
	      DO I2=1,2
	        IF ((AMCO(I2,I4,NCHAN-1).NE.1E20).AND.
	1	      (NRAMCO(I4,NCHAN-1).NE.0)) THEN
		  AMCO(I2,I4,NCHAN-1)=AMCO(I2,I4,NCHAN-1)/NRAMCO(I4,NCHAN-1)
		END IF
	      END DO
	     END DO
	    END IF
	  END IF
 	END DO						! sectors
 501	CONTINUE
	IF (NO_MORE) GOTO 1000 
C
	IF (IFR_MODE.EQ.'BAND') THEN			!CALCULATE AVERAGE
	  IF (MOSAIK) THEN
	    DO I1=0,NCHAN-1
	     DO I4=0,STHJ(STH_NIFR_J)-1
	      DO I2=1,2
	       IF ((AMCO(I2,I4,I1).NE.1E20).AND.(NRAMCO(I4,I1).NE.0)) 
	1	    AMCO(I2,I4,I1)=AMCO(I2,I4,I1)/NRAMCO(I4,I1)
	      END DO
	     END DO
	    END DO
	  END IF
	  IF (.NOT.OPENSW) THEN
	    IF ((.NOT.MOSAIK.AND..NOT.PLOTAP).OR.
	1      ((MOSAIK.AND.(IFR_MODE.NE.'BAND'))))THEN
	      CALL NPLOPN (LDATTP,IPOL,NHV,IFRS)	!OPEN PLOT, PLOT HEADING
	      IF (NO_MORE) GOTO 1000			!USER SAID: STOP
	      CALL NPLPBE (.TRUE., NPLOT,PTXT,NHV,NCHAN)!PLOT BEGIN-ANNOTATIONS
	      CALL NPLPBE (.FALSE.,NPLOT,PTXT,NHV,NCHAN)!PLOT END-ANNOTATIONS
	      OPENSW=.TRUE.
	    END IF
	  END IF
          CALL NPLPLT (LDATTP,IPOL,NPLOT,PTXT,NHV,TABIFR,SCHAN,IFRS,
	1      NCHAN,STHTEL,HAB,HAI,PLTIFR,IFRA,AMCO)
CC	  IF (WNGCCN().GT.1) NO_MORE=.TRUE.
	  IF (NO_MORE) GOTO 1000				!USER SAID: STOP
	END IF
	IF (OPENSW) THEN
	  IF ((.NOT.MOSAIK.AND..NOT.PLOTAP).OR.
	1    	((MOSAIK.AND.(IFR_MODE.NE.'BAND'))))THEN
cc	    CALL NPLPBE (.FALSE.,NPLOT,PTXT,NHV,NCHAN)	!PLOT END-ANNOTATIONS
C
 1000	    CONTINUE					! NO_MORE exit
	    IF ((PPP(1).EQ.1).AND.(PPP(2).EQ.1)) THEN
	       CALL NPLCLO(DQID,NHV)			!CLOSE PLOT
	       OPENSW=.FALSE.
	    END IF
	  END IF
	END IF
C
	RETURN
C
	END
