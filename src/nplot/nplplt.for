C+ NPLPLT.FOR
C  HjV 940119
C       Combined parts of NPLRES and NPLTEL
C
C  Revisions:
C       HjV 940224      Add mosaik test
C       HjV 940324      Better check for MOSAIK with BANDPASS
C       HjV 940413	Changed check for opening plot
C	CMV 940822	Option to abort during loop of plots
C	HjV 960415	Correct option to stop during loop or more plots per 
C			 page
C	JPH 960621	Control-C interrupt
C	JPH 960730	Recognise PGAIN data type
C	JPH 960802	No connection lines is SEQ mode
C	HJV 970123	I1 --> AXINT
C	HjV 970723	Remove control-C stuff (commented out with CCC)
C	AXC 010709      Linux port - .NE.0 changed
C
C
	SUBROUTINE NPLPLT (LDATTP,IPOL,NPLOT,PTXT,NHV,TABIFR,SCHAN,
	1    IFRS,NHACH,NIFRTEL,HAB,HAI,PLTIFR,IFRA,AMCO)
C
C  Plot residual/telescope errors of individual scans
CCC  Interruption by control-C: single = next plot, double = exit routine
C
C  Result:
C
C	CALL NPLPLT		Plot residual/telescope errors
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'STH_O_DEF'		!SET HEADER
	INCLUDE 'NPL_DEF'
C
C  Parameters:
C
	INTEGER LDATTP			!CURRENT DATA TYPE
	INTEGER IPOL
	INTEGER NPLOT			!# OF INTERFEROMETERS TO DO
	INTEGER PTXT(MXNCHN)		!CROSS CHANNELS
	INTEGER NHV(0:1)		!# OF PAGES
	INTEGER TABIFR(0:STHTEL-1,0:STHTEL-1) !IFR PLOT POINTERS
 	INTEGER SCHAN(0:MXNCHN-1)       !CHANNELS TO DO
	INTEGER IFRS(0:1)		!CURRENT IFR'S
	INTEGER NHACH                   !# OF HARA OR CHANNELS
	INTEGER NIFRTEL                 !# OF ALL INTERF. OR TELESC.
	REAL HAB                        !HA BEGIN
	REAL HAI                        !HA INCR.
	LOGICAL PLTIFR(0:STHIFR-1)          !.TRUE.= PLOT THIS INTERFEROMETER
	INTEGER IFRA(0:1,0:STHIFR-1)
	REAL AMCO(2,0:STHIFR-1,0:MXNCHN-1) !DATA values per interf. per HA/CHAN
C
C  Arguments:
C
C
C  Function references:
C
CCC	INTEGER WNGCCN			! check for control-C
C
C  Data declarations:
C
	REAL LHA			!LOCAL HA
	REAL MINMAX(2,2)                !MIN./MAX. AMPL/COS AND PHASE/SIN
	REAL LENMM
	REAL CPO                        !CURRENT POINT OFFSET
	REAL HPO                        !HALF POINT OFFSET
	REAL AXINT			!
C-
C
C PLOT PER IFR
C
	  IF (PLOTAP) THEN
	    I5=0
	    DO I1=0,NIFRTEL-1                            !ALL IFRS/TELS
	      IF (((OPT.EQ.'TEL').AND.
	1	 ((.NOT.PLUVO .AND. STELS(I1).NE.0) .OR.
	1	  (PLUVO .AND. I1.EQ.IFRS(0)))) .OR.
	1	 ((OPT.NE.'TEL').AND.(PLTIFR(I1)))) THEN   !PLOT THIS ONE
	        IF (PLUVO) THEN
		  I4=SCHAN(SETNAM(3))			 !PLOT POINTER
	        ELSE
		  IF (OPT.EQ.'TEL') THEN
		    I4=I5
		  ELSE
	            I4=TABIFR(IFRA(0,I1),IFRA(1,I1))	 !PLOT POINTER
		  END IF
	        END IF
		IF (OPT.EQ.'TEL') THEN
	          TXT(1)(1:1)=TELNAM(I1+1:I1+1)
	          TXT(2)(1:1)=' '
	        ELSE
		  TXT(1)(1:1)=TELNAM(IFRA(0,I1)+1:IFRA(0,I1)+1)
	          TXT(2)(1:1)=TELNAM(IFRA(1,I1)+1:IFRA(1,I1)+1)
		END IF
	        CALL NPLOPN (LDATTP,IPOL,NHV,IFRS)       !OPEN PLOT, PLOT HEADING
		IF (NO_MORE) RETURN			 !USER SAID: STOP
C
C DETERMINE MIN/MAX
	        MINMAX(1,1)=100000.                      !MIN AMPL OR COS
	        MINMAX(2,1)=-100000.                     !MAX AMPL OR COS
	        MINMAX(1,2)=100000.                      !MIN PHASE OR SIN
	        MINMAX(2,2)=-100000.                     !MAX PHASE OR SIN
	        DO I2=0,NHACH-1                          !ALL SCANS/CHANNELS
		  IF (AMCO(1,I4,I2).NE.1E20) THEN
		    IF (AMCO(1,I4,I2).LT.MINMAX(1,1)) MINMAX(1,1)=AMCO(1,I4,I2)
		    IF (AMCO(1,I4,I2).GT.MINMAX(2,1)) MINMAX(2,1)=AMCO(1,I4,I2)
		  END IF
		  IF (AMCO(2,I4,I2).NE.1E20) THEN
		    IF (AMCO(2,I4,I2).LT.MINMAX(1,2)) MINMAX(1,2)=AMCO(2,I4,I2)
		    IF (AMCO(2,I4,I2).GT.MINMAX(2,2)) MINMAX(2,2)=AMCO(2,I4,I2)
		  END IF
	        END DO  				 !NEXT SCAN
	        CALL NPLBAP(LDATTP,IPOL,PTXT,NHV,MINMAX,NHACH)   !PLOT BOX
	        DO I3=1,2                                ! LOOP AMPL/COS OR
							 ! PHASE/SIN
	          LENMM=MINMAX(2,I3)-MINMAX(1,I3)        !MAX-MIN
		  OLD(0)=1E20
		  DO I2=0,NHACH-1                        !ALL SCANS/CHANNELS
		    NEW(I2)=AMCO(I3,I4,I2)
		    OLD(I2+1)=NEW(I2)
	          END DO                                 !NEXT SCAN/CHAN
		  LHA=HAB-HAI*2.
		  CALL NPLTWO (I3,NHACH,LENMM,MINMAX,HAI,LHA) !PLOT DATA
                END DO                       	!NEXT I3
 	        IF ((PPP(1).EQ.1).AND.(PPP(2).EQ.1)) THEN
		  CALL NPLCLO(DQID,NHV)
		  IF (NO_MORE) RETURN			 !USER SAID: STOP
	        END IF
	        I5=I5+1
	      END IF
CCC	      IF (WNGCCN().NE.0) THEN			! control-C seen?
CCC		CALL WNGSLP(1)				!  allow for another one
CCC		GOTO 50
CCC	      ENDIF
	    END DO                 			!NEXT IFR
CCC 50	    CONTINUE
C
C Normal plot
C
	  ELSE
	    DO I1=0,NPLOT-1				 !SET OLD
	      NEW(I1)=1E20
	      OLD(I1)=1E20
	    END DO
	    IF (MOSAIK.AND.(IFR_MODE.EQ.'BAND')) THEN
	      CALL NPLOPN (LDATTP,IPOL,NHV,IFRS)         !OPEN, PLOT HEADING
	      IF (NO_MORE) RETURN			 !USER SAID: STOP
	      CALL NPLPBE (.TRUE.,NPLOT,PTXT,NHV,NHACH)  !PLOT BEGIN-ANNOTATIONS
	    END IF
	    IF ((DATTYP(LDATTP)(1:2).EQ.'AM').OR.
	1       (DATTYP(LDATTP)(1:2).EQ.'CO')) THEN      !AMPL. OR COS
	      I3=1
	    ELSE
	      I3=2
	    END IF
	    IF (DATTYP(LDATTP)(1:1).EQ.'P') THEN
	      R1=SCAL(2)
	    ELSE
	      R1=SCAL(1)
	    ENDIF
	    DO I0=0,NHACH-1		        	 !ALL SCANS/CHAN
	      I5=0
	      DO I1=0,NIFRTEL-1                          !ALL IFRS/TELS
	        IF (((OPT.EQ.'TEL').AND.
	1	   ((.NOT.PLUVO .AND. STELS(I1).NE.0) .OR.
	1	    (PLUVO .AND. I1.EQ.IFRS(0)))) .OR.
	1	   ((OPT.NE.'TEL').AND.(PLTIFR(I1)))) THEN   !PLOT THIS ONE
	          IF (PLUVO) THEN
		    I4=SCHAN(SETNAM(3)) 		 !PLOT POINTER
	          ELSE
		    IF (OPT.EQ.'TEL') THEN
		      I4=I5
		    ELSE
	              I4=TABIFR(IFRA(0,I1),IFRA(1,I1))	 !PLOT POINTER
		    END IF
	          END IF
		  OLD(I4)=NEW(I4)
	          IF (AMCO(I3,I4,I0).EQ.1E20) THEN
		    NEW(I4)=AMCO(I3,I4,I0)
		  ELSE
		    NEW(I4)=IFOFF(I4)+AMCO(I3,I4,I0)/R1*XFAC
		  END IF
		  I5=I5+1
		END IF
	      END DO                                     !NEXT IFR/TELS
	      IF (IFR_MODE.EQ.'BAND') THEN
		IF (NHACH.EQ.1) THEN
		  AXINT=PAREA(3)-PAREA(1)-YFAC*2*(15./PPP(2))
		ELSE
		  AXINT=(PAREA(3)-PAREA(1)-YFAC*2*(15./PPP(2)))/(NHACH-1)
		END IF
		CPO=PAREA(3)-YFAC*(15./PPP(2))-(I0*AXINT)
							! CURRENT POINT OFFSET
		IF ((I0.GT.0).AND.(I0.LT.NHACH)) THEN
		  HPO=AXINT/2
		ELSE
		  HPO=0.
		END IF
	      ELSE
	        LHA=HAB+I0*HAI                          ! HA
		CPO=PAREA(3)-YFAC*(15./PPP(2)+360.*(LHA-HARA(0))/HASC)
							! CURRENT POINT OFFSET
		HPO=180.*YFAC*HAI/HASC           	! HALF POINT OFFSET
	      END IF
	      IF ( (.NOT.MOSAIK) .AND. I0.EQ.0 .AND. ST_MODE.GE.0 )
	1	   CALL NPLCON (NPLOT,CPO,HPO,.TRUE.)  	!FIRST SCAN
	      CALL NPLONE (NPLOT,CPO,HPO)
CCC	      IF (WNGCCN().NE.0) THEN			! control-C seen?
CCC		CALL WNGSLP(1)				! yes: allow time
CCC							!  for another one
CCC		GOTO 100
CCC	      ENDIF
	    END DO					! scans
CCC 100	    CONTINUE
	    IF (IFR_MODE.EQ.'BAND') HPO=0.
            IF (.NOT.MOSAIK .AND. ST_MODE.GE.0) 
	1	CALL NPLCON (NPLOT,CPO,HPO,.FALSE.)	 ! connection lines
	    IF (MOSAIK.AND.(IFR_MODE.EQ.'BAND')) THEN
	      CALL NPLPBE (.FALSE.,NPLOT,PTXT,NHV,NHACH) ! END-ANNOTATIONS
	      IF ((PPP(1).EQ.1).AND.(PPP(2).EQ.1)) THEN
	        CALL NPLCLO(DQID,NHV)			 !CLOSE PLOT
	      END IF
	    END IF
	  END IF					! plotap
C
	RETURN
C
	END

