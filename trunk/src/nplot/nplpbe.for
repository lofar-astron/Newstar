C+ NPLPBE.FOR
C  HjV 931112
C       Combined parts of old version of NPLTEL and NPLRES
C
C  Revisions:
C       HjV 940602	Add some more text when more than one plot per page
C	CMV 940705	Change AXINT to REAL (for 256 channels, 2x2 page)
C	CMV 940822	Option to abort during loop of plots
C	JPH 961118	Grid: dotted lines connecting HA ticks and tel/ifr ticks
C	JPH 961212	Make PLUVO telesc. annotation same as other (was 
C			 illegibly small)
C	JPH 970124	Remove NPLCLO, in order that raster can be drawn through
C			 end-annotation call before data plotting.
C
C
	SUBROUTINE NPLPBE (BEGIN,NPLOT,PTXT,NHV,NHACH)
C
C  Plot polyline and IFR/TEL line at begin and end of plot
C
C  Result:
C
C	CALL NPLPBE              	Plot polyline and IFR/TEL line at begin 
C					and end of plot
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
	LOGICAL BEGIN                   !BEGIN (TRUE) OR END (FALSE) OF PLOT
	INTEGER NPLOT			!# OF INTERFEROMETERS TO DO
	INTEGER PTXT(MXNCHN)            !CROSS CHANNELS
	INTEGER NHV(0:1)                !# OF PAGES
	INTEGER NHACH                   !# OF HA OR CHANNELS
C
C  Function references:
C
	LOGICAL NSCSTL			!GET A SET
	INTEGER WNMEJC			!CEIL
	INTEGER WNMEJF			!FLOOR
C
C  Data declarations:
C
	REAL UP1(2)			!DIRECTIONS
	  DATA UP1/1.,0./
	REAL UP0(2)
	  DATA UP0/0.,1./
	INTEGER START,END
	REAL YOFF, YOFF0                !Y OFFSET
	REAL AXINT                      !AXIS INTERVAL FOR BAND-OPTION
C-
	IF (BEGIN) THEN
	  R0=(TAREA(2)-TAREA(0)-80./PPP(1))/(NPLOT+1)	!IFR/TEL SPACING IN MM
	  DO I1=0,NPLOT-1
	    IFOFF(I1)=NINT(TAREA(0)+40./PPP(1)+((I1+1)*R0))  !OFFSETS IFR/TEL
	    NEW(I1)=1E20
	    OLD(I1)=NEW(I1)
	  END DO
	  START=0
	  END=1
	  IF (PLUVO) THEN
	    YOFF=PAREA(3)
	  ELSE
	    YOFF=PAREA(3)-YFAC*3./PPP(2)
	  END IF
	  YOFF0=YOFF
	ELSE! end
	  START=1
	  END=2
	  IF (IFR_MODE.EQ.'BAND') THEN
	    YOFF=PAREA(1)+YFAC*(11./PPP(2))
	  ELSE
	    YOFF=PAREA(3)-YFAC*(19./PPP(2)+360.*(HARA(1)-HARA(0))/HASC)
	  END IF
	END IF! begin/end
C
C
 10	IF (MOD(START,2).EQ.0) THEN                     ! PLOT IFR/TEL TEXT
CC	  IF (PLUVO) THEN
CC	    CALL WQSTXH(TXTHGT*2./3./PPP(2))		!SMALLER
CC	    CALL WQSTXU(UP1)				!CHANGE DIRECTION
CC	  ELSE
	    CALL WQSTXH(TXTHGT/PPP(2))	        	!SMALLER
CC	  END IF
	  TXTXY(2)=YOFF
	  DO I=0,NPLOT-1				! all tel. or ifr
	    IF (PLUVO) THEN
	      CALL WNCTXS(TEXT,'!4$UJ',PTXT(I+1))
	      TXTXY(1)=IFOFF(I)-TXTHGT/3./PPP(1)	!PLOT IFR FIRST LINE
	    ELSE
	      TEXT=TXT(1)(I+1:I+1)
	      TXTXY(1)=IFOFF(I)-TXTHGT/(18./7.)/PPP(1)	!PLOT TEL FIRST LINE
	    END IF
	    CALL WQTEXT(TXTXY,TEXT)			!TEXT
 	  END DO
	  IF (PLUVO) THEN
	    YOFF=YOFF-YFAC*6./PPP(2)
	  ELSE
	    YOFF=YOFF-YFAC*3./PPP(2)
	  ENDIF
	  TXTXY(2)=YOFF
	  IF (OPT.NE.'TEL') THEN
	    IF (.NOT.PLUVO) THEN
	      DO I=0,NPLOT-1
	        TEXT=TXT(2)(I+1:I+1)
	        TXTXY(1)=IFOFF(I)-TXTHGT/(18./7.)/PPP(1)!PLOT IFR 2ND LINE
	        CALL WQTEXT(TXTXY,TEXT)			!TEXT
	      END DO
	    END IF
	  END IF
	  YOFF=YOFF-YFAC
	  CALL WQSTXH(TXTHGT)				!TEXT HEIGHT STANDARD
	  CALL WQSTXU(UP0)				!STANDARD DIRECTION
C
C PLOT tickmarks and vertical raster lines
C
	ELSE! end 
	  POINXY(2,2)=YOFF-YFAC*4./PPP(2)
	  DO I=0,NPLOT-1
 	    POINXY(2,1)=YOFF
	    POINXY(1,1)=IFOFF(I)
	    POINXY(1,2)=IFOFF(I)
	    CALL WQPOLL(2,POINXY)			! tick mark
	    IF (.NOT.BEGIN) THEN
	      POINXY(2,1)=YOFF0
	      CALL WQSPLI(3)				! dotted
	      CALL WQPOLL(2,POINXY)			!  line
	      CALL WQSPLI(1)				! reset full-line
	    ENDIF
	  END DO
	  YOFF=YOFF-YFAC*7./PPP(2)
	ENDIF
	START=START+1
	IF (START.LE.END) GOTO 10
C
C  HA/CHANNEL ANNOTATION
C
	IF (BEGIN) THEN                                 !PLOT HA/CHAN ANNOTATION
	  CALL WQSTXH(TXTHGT/PPP(2))			!TEXT HEIGHT STANDARD
	  IF (IFR_MODE.EQ.'BAND') THEN
            IF (NHACH.EQ.1) THEN
	      AXINT=(PAREA(3)-PAREA(1)-YFAC*2*(15./REAL(PPP(2))))
            ELSE
	      AXINT=(PAREA(3)-PAREA(1)-YFAC*2*(15./REAL(PPP(2))))
	1				/REAL(NHACH-1)
            END IF
	    J1=1
	    DO WHILE ((AXINT*J1).LT.15)
	      J1=J1*2
	    END DO
	    DO I0=0,NHACH-1,J1
	      CALL WNCTXS (TEXT,'!4$UJ',PTXT(I0+1))	!CHANNEL NAME
	      TXTXY(1)=TAREA(0)
	      TXTXY(2)=PAREA(3)-(15./PPP(2)*YFAC+(I0*AXINT))-
	1			TXTHGT/(18./7.)/PPP(1)
	      CALL WQTEXT(TXTXY,TEXT)
	      POINXY(1,1)=TAREA(0)+35./PPP(1)		!PLOT 0.25 CM. LINE (X)
	      POINXY(2,1)=PAREA(3)-(15./PPP(2)*YFAC+(I0*AXINT))
	      POINXY(1,2)=TAREA(0)+45./PPP(1)
	      POINXY(2,2)=POINXY(2,1)
	      CALL WQPOLL(2,POINXY)			!POLYLINE
	      POINXY(1,1)=PAREA(2)-11./PPP(1)		!PLOT 0.25 CM. LINE (X)
	      POINXY(1,2)=PAREA(2)-1./PPP(1)
	      CALL WQPOLL(2,POINXY)			!POLYLINE
	    END DO
	  ELSE
 	    R0=10.*HASC	 				!DEGREE PER CM
	    IF (R0.LE.1.) THEN
	      I=1
	    ELSE IF (R0.LE.2.) THEN
	      I=2
	    ELSE IF (R0.LE.5.) THEN
	      I=5
	    ELSE IF (R0.LE.10.) THEN
	      I=10
	    ELSE IF (R0.LE.15.) THEN
	      I=15
	    ELSE IF (R0.LE.30.) THEN
	      I=30
	    ELSE IF (R0.LE.45.) THEN
	      I=45
	    ELSE
	      I=90
	    END IF
	    DO R1=WNMEJC(360.*HARA(0)/FLOAT(I))*FLOAT(I),
	1	  WNMEJF(360.*HARA(1)/FLOAT(I))*FLOAT(I),! DRAW HA MARKS
	1	  FLOAT(I)
	      CALL WNCTXS(TEXT,'!4$SJ',NINT(R1))
	      TXTXY(1)=TAREA(0)
	      TXTXY(2)=PAREA(3)-(15./PPP(2)+(R1-360.*HARA(0))/
	1			HASC)*YFAC-TXTHGT/(18./7.)/PPP(1)
	      CALL WQTEXT(TXTXY,TEXT(1:4))
	      POINXY(1,1)=TAREA(0)+35./PPP(1)		! PLOT 0.25 CM. LINE (X)
	      POINXY(2,1)=PAREA(3)-(15./PPP(2)+(R1-360.*HARA(0))/
	1			HASC)*YFAC
	      POINXY(1,2)=TAREA(0)+45./PPP(1)
	      POINXY(2,2)=POINXY(2,1)
	      CALL WQPOLL(2,POINXY)			! POLYLINE
	      POINXY(1,1)=PAREA(2)-11./PPP(1)		! PLOT 0.25 CM. LINE (X)
	      POINXY(1,2)=PAREA(2)-1./PPP(1)
	      CALL WQPOLL(2,POINXY)			! POLYLINE
	      POINXY(1,2)=TAREA(0)+45./PPP(1)
	      CALL WQSPLI(3)				! DOTTED
	      CALL WQPOLL(2,POINXY)
	      CALL WQSPLI(1)				! DRAWN
	    END DO
	  END IF
	  CALL WQSTXH(TXTHGT)				!TEXT HEIGHT STANDARD
C
C  CLOSE
C
	ELSE! end
	  CALL WQSTXH(TXTHGT)				!TEXT HEIGHT STANDARD
	  CALL WQSTXU(UP0)				!STANDARD DIRECTION
cc	  IF ((PPP(1).EQ.1).AND.(PPP(2).EQ.1)) THEN
cc	    CALL NPLCLO(DQID,NHV)			!CLOSE PLOT
cc	  END IF
	ENDIF
C
C
	END

