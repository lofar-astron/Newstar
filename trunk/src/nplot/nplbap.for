C+ NPLBAP.FOR
C  HjV 931115
C
C  Revisions:
C       HjV 940602	Add some more text when more than one plot per page
C
	SUBROUTINE NPLBAP (LDATTP,IPOL,PTXT,NHV,MINMAX,NHACH)
C
C  Plot box ampl./phase plots per IFR on same page (old PLOTAP)
C
C  Result:
C
C	CALL NPLBAP		Plot ampl./phase on same page
C
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'NPL_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER LDATTP			!CURRENT DATA TYPE
        INTEGER IPOL
	INTEGER PTXT(MXNCHN)            !CROSS CHANNELS
	INTEGER NHV(0:1)                !# PAGES
	REAL MINMAX(2,2)                !MIN./MAX. AMPL/COS AND PHASE/SIN
	INTEGER NHACH                   !# OF HARA OR CHANNELS
C
C  Function references:
C
	INTEGER WNMEJC			!CEIL(X)
	INTEGER WNMEJF			!FLOOR(X)
C
C  Data declarations:
C
	REAL STEP(2)			!X,Y DISTANCE BETWEEN POINTS
	LOGICAL UVPL			!UV-PLANE OUTPUT
	REAL YSIZ
	REAL R2,R3,R4
	REAL STVAL,LENYAS,LENMM,INCRYAS
	INTEGER AXINT                   !AXIS INTERVAL FOR BAND-OPTION
        CHARACTER*2 POLNAM(0:3)         !NAME OF POLARISATION
          DATA POLNAM/'XX','XY','YX','YY'/
C-
C
C INITIALIZE
C
C
	DO I0=1,2
	  POINXY(1,1)=PAREA(0)
	  POINXY(2,1)=PAREA(1)
	  POINXY(1,2)=PAREA(2)
	  POINXY(2,2)=PAREA(3)
	  CALL WQ_MPLR(DQID2,NHV,1,1,1.,0)              !SET LINE REPRESENTATION
	  CALL WQ_MPLR(DQID2,NHV,3,3,1.,0)
	  YSIZ=(POINXY(2,2)-POINXY(2,1))/2.+25./PPP(2)  !BOTTOM LEFT CORNER
C
C GRID
C
	  CALL WQSTXX(1.)	                        !CHARACTER EXPANSION
	  CALL WQSPLI(1)
	  IF (I0.EQ.1) THEN                             !AMPL
	    POINXY(2,1)=PAREA(1)+YSIZ                   !Y LEFT/RIGHT BOTTOM
	  ELSE                                          !PHASE
	    POINXY(2,2)=PAREA(3)-YSIZ                   !Y LEFT/RIGHT TOP
	  ENDIF
	  PG(1,1)=POINXY(1,1)	                        !X LEFT TOP/BOTTOM
	  PG(2,1)=POINXY(2,1)	                        !Y LEFT/RIGHT TOP
	  PG(1,2)=POINXY(1,2)	                        !X RIGHT TOP/BOTTOM
	  PG(2,2)=PG(2,1)
	  PG(1,3)=PG(1,2)
	  PG(2,3)=POINXY(2,2)	                        !Y LEFT/RIGHT BOTTOM
	  PG(1,4)=PG(1,1)
	  PG(2,4)=PG(2,3)
	  PG(1,5)=PG(1,1)
	  PG(2,5)=PG(2,1)
	  CALL WQ_MPLR(DQID2,NHV,1,1,2.,0)		!THICK LINE
	  CALL WQPOLL(5,PG)				!BOX
	  CALL WQ_MPLR(DQID2,NHV,1,1,1.,0)		!NORMAL LINE
C
C AMPL./COS. OR PHASE/SIN INFO
C
	  CALL WQSTXH(TXTHGT*2./PPP(1))                 !TEXT HEIGHT
	  TXTXY(1)=POINXY(1,2)+10./PPP(1)
	  TXTXY(2)=POINXY(2,2)-10./PPP(2)
          IF ((PPP(1).GT.1).OR.(PPP(2).GT.1)) THEN
	    TEXT=DATTYP(LDATTP)(I0:I0)//' '//TXT(1)(1:1)//
	1	  TXT(2)(1:1)//'-'//POLNAM(IPOL)
	    CALL WQTEXT(TXTXY,TEXT(1:7))
	  ELSE
	    CALL WNCTXS(TEXT,'!1$AS',DATTYP(LDATTP)(I0:I0))
	    CALL WQTEXT(TXTXY,TEXT(1:1))
	  END IF
	  CALL WQSTXH(TXTHGT/PPP(1))                    !TEXT HEIGHT STANDARD
	  CALL WNCTXS(TEXT,'MAX: !7$E7.2',MINMAX(2,I0))
	  TXTXY(1)=POINXY(1,2)+10./PPP(1)
	  TXTXY(2)=POINXY(2,2)-30./PPP(2)
	  CALL WQTEXT(TXTXY,TEXT(1:12))
	  CALL WNCTXS(TEXT,'MIN: !7$E7.2',MINMAX(1,I0))
	  TXTXY(1)=POINXY(1,2)+10./PPP(1)
	  TXTXY(2)=POINXY(2,2)-45./PPP(2)
	  CALL WQTEXT(TXTXY,TEXT(1:12))
	END DO
C
C  HA ANNOTATION (X-AXIS)
C
	IF (IFR_MODE.EQ.'BAND') THEN
          IF (NHACH.EQ.1) THEN
            AXINT=PAREA(2)-PAREA(0)
          ELSE
            AXINT=(PAREA(2)-PAREA(0))/(NHACH-1)
          END IF
	  J1=1
	  DO WHILE ((AXINT*J1).LT.15) 
	    J1=J1*2
	  END DO
	  CALL WQSTXH(TXTHGT/PPP(1))                   !TEXT HEIGHT STANDARD
	  POINXY(2,2)=PAREA(1)+YSIZ                    !Y LEFT/RIGHT BOTTOM AMPL
	  R2=5./PPP(2)
	  DO I0=0,NHACH-1,J1
	    CALL WNCTXS (TEXT,'!4$UJ',PTXT(I0+1))	!CHANNEL NAME
	    TXTXY(1)=PAREA(0)+I0*AXINT-TXTHGT/PPP(1)
	    TXTXY(2)=POINXY(2,2)-30./PPP(2)+R2
	    CALL WQTEXT(TXTXY,TEXT)
	    R2=R2*-1.
	    DO I1=0,1
	      PG(1,1)=POINXY(1,1)+I0*AXINT
	      PG(2,1)=POINXY(2,2)-(I1*40./PPP(2))       !PLOT 0.25 CM. LINE (Y)
	      PG(1,2)=PG(1,1)
	      PG(2,2)=PG(2,1)-10./PPP(2)
	      CALL WQPOLL(2,PG)			        !POLYLINE
	    END DO
	  END DO
	ELSE
	  R0=10.*HASC                      		!DEGREE PER CM
	  IF (R0.LE..1) THEN
	    R4=.1
	   ELSE IF (R0.LE..2) THEN
	    R4=.2
	   ELSE IF (R0.LE..5) THEN
	    R4=.5
	   ELSE IF (R0.LE.1.) THEN
	    R4=1.
	   ELSE IF (R0.LE.2.) THEN
	    R4=2.
	   ELSE IF (R0.LE.5.) THEN
	    R4=5.
	   ELSE IF (R0.LE.10.) THEN
	    R4=10.
	   ELSE IF (R0.LE.15.) THEN
	    R4=15.
	   ELSE IF (R0.LE.20.) THEN
	    R4=20.
	   ELSE IF (R0.LE.30.) THEN
	    R4=30.
	   ELSE IF (R0.LE.45.) THEN
	    R4=45.
	   ELSE
	    R4=90.
	  END IF
	  CALL WQSTXH(TXTHGT/PPP(1))                    !TEXT HEIGHT STANDARD
	  POINXY(2,2)=PAREA(1)+YSIZ                     !Y LEFT/RIGHT BOTTOM AMPL
	  R2=5./PPP(2)
	  DO R1=WNMEJC(360.*HARA(0)/R4)*R4,
	1	WNMEJF(360.*HARA(1)/R4)*R4,R4           !DRAW HA MARKS
	    CALL WNCTXS(TEXT,'!5$E5.1',R1)
	    TXTXY(1)=TAREA(0)+50./PPP(1)+(R1-360.*HARA(0))/HASC*
	1	 XFAC-TXTHGT/(18./7./PPP(1))
	    TXTXY(2)=POINXY(2,2)-30./PPP(2)+R2
	    CALL WQTEXT(TXTXY,TEXT(1:5))
	    R2=R2*-1.
	    DO I1=0,1
	      PG(1,1)=POINXY(1,1)+(R1-360.*HARA(0))/HASC*XFAC
	      PG(2,1)=POINXY(2,2)-(I1*40./PPP(2))       !PLOT 0.25 CM. LINE (Y)
	      PG(1,2)=PG(1,1)
	      PG(2,2)=PG(2,1)-10./PPP(2)
	      CALL WQPOLL(2,PG)			        !POLYLINE
	    END DO
	  END DO
	END IF
C
C  AMPL/PHASE ANNOTATION (Y-AXIS)
C
	DO I0=1,2
	  MINMAX(1,I0)=NINT(MINMAX(1,I0)-.5)*1.         !MIN.
	  MINMAX(2,I0)=NINT(MINMAX(2,I0)+.5)*1.         !MAX.
	END DO
	INCRYAS=50./PPP(2)
	DO I0=1,2
	  POINXY(1,1)=PAREA(0)
	  POINXY(2,1)=PAREA(1)
	  POINXY(1,2)=PAREA(2)
	  POINXY(2,2)=PAREA(3)
	  IF (I0.EQ.1) THEN                             !AMPL
	    POINXY(2,1)=PAREA(1)+YSIZ                   !Y LEFT/RIGHT BOTTOM
	  ELSE                                          !PHASE
	    POINXY(2,2)=PAREA(3)-YSIZ                   !Y LEFT/RIGHT TOP
	  ENDIF
	  LENYAS=YSIZ-50./PPP(2)                        !LENGTH Y-AX
	  LENMM=MINMAX(2,I0)-MINMAX(1,I0)               !MAX-MIN
	  R1=LENMM/LENYAS
	  R2=-1.
	  R3=.001
	  DO WHILE (R2.LT.0.)
	    R3=R3*10.
	    IF (R1.LE.(R3*.1)) THEN
	      R2=R3*.1
	    ELSE IF (R1.LE.(R3*.2)) THEN
	      R2=R3*.2
	    ELSE IF (R1.LE.(R3*.5)) THEN
	      R2=R3*.5
	    END IF
	  END DO
	  R1=((R2*LENYAS)-LENMM)/2.
	  MINMAX(1,I0)=MINMAX(1,I0)-R1
	  MINMAX(2,I0)=MINMAX(2,I0)+R1
	  LENMM=MINMAX(2,I0)-MINMAX(1,I0)                !MAX-MIN
	  STVAL=MOD((LENYAS/2.),INCRYAS)                 !START VALUE
	  DO R1=STVAL,LENYAS,INCRYAS			 !DRAW HA MARKS AND TEXT
	    R2=MINMAX(2,I0)-LENMM*((LENYAS-R1)/LENYAS)
	    CALL WNCTXS(TEXT,'!7$E7.2',R2)
	    TXTXY(1)=TAREA(0)
	    TXTXY(2)=POINXY(2,1)+R1
	    CALL WQTEXT(TXTXY,TEXT(1:7))
	    PG(1,1)=POINXY(1,1)
	    PG(2,1)=POINXY(2,1)+R1                       !PLOT 0.25 CM. LINE (Y)
	    PG(1,2)=PG(1,1)-10./PPP(1)
	    PG(2,2)=PG(2,1)
	    CALL WQPOLL(2,PG)                   	 !POLYLINE
	    CALL WQSPLI(3)                               !DOTTED
	    PG(1,1)=POINXY(1,1)
	    PG(2,1)=POINXY(2,1)+R1
	    PG(1,2)=POINXY(1,2)
	    PG(2,2)=PG(2,1)
	    CALL WQPOLL(2,PG)   		         !POLYLINE
	    CALL WQSPLI(1)                               !NORMAL
	  END DO
	END DO
C
C DRAW DOTTES LINES
C
C  X-AXIS
C
	CALL WQSPLI(3)                                 !DOTTED
	DO I0=1,2
	  POINXY(1,1)=PAREA(0)
	  POINXY(2,1)=PAREA(1)
	  POINXY(1,2)=PAREA(2)
	  POINXY(2,2)=PAREA(3)
	  IF (I0.EQ.1) THEN                            !AMPL
	    POINXY(2,1)=PAREA(1)+YSIZ                  !Y LEFT/RIGHT BOTTOM
	  ELSE                                         !PHASE
	    POINXY(2,2)=PAREA(3)-YSIZ                  !Y LEFT/RIGHT TOP
	  ENDIF
	  PG(2,1)=POINXY(2,1)
	  PG(2,2)=POINXY(2,2)
	  IF (IFR_MODE.EQ.'BAND') THEN
	    DO I1=0,NHACH-1
	      PG(1,1)=POINXY(1,1)+I1*AXINT
	      PG(1,2)=PG(1,1)
	      CALL WQPOLL(2,PG)		               !POLYLINE
	    END DO
	  ELSE
	    DO R1=WNMEJC(360.*HARA(0)/R4)*R4,
	1	WNMEJF(360.*HARA(1)/R4)*R4,R4
	      PG(1,1)=POINXY(1,1)+(R1-360.*HARA(0))/HASC*XFAC
	      PG(1,2)=PG(1,1)
	      CALL WQPOLL(2,PG)		               !POLYLINE
	    END DO
	  END IF
	END DO
	CALL WQSPLI(1)                                 !NORMAL
C
C
 	END
