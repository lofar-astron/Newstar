C+ NPLTWO.FOR
C  HjV 931108
C       Combined parts of old version of NPLTEL and NPLRES
C
C  Revisions:
C
	SUBROUTINE NPLTWO (APCS,NHACH,LENMM,MINMAX,HAINCR,LHA)
C
C  Plot AP/CS scan/sets
C
C  Result:
C
C	CALL NPLTWO              	Plot scan/sets
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
	INTEGER APCS                    !1=AMPL/COS, 2=PHASE/SIN
	INTEGER NHACH			!# OF HA/CHAN TO DO
	REAL LENMM                      !MAX-MIN
	REAL MINMAX(2,2)                !MIN./MAX. AMPL/COS AND PHASE/SIN
	REAL HAINCR                     !HA INCREMENT
	REAL LHA			!LOCAL HA
	REAL CPO                        !CURRENT POINT OFFSET
	REAL HPO                        !HALF POINT OFFSET
C
C  Function references:
C
C
C  Data declarations:
C
	REAL YSIZ,LENYAS
C-
	POINXY(1,1)=PAREA(0)
	POINXY(2,1)=PAREA(1)
	POINXY(1,2)=PAREA(2)
	POINXY(2,2)=PAREA(3)
	YSIZ=(POINXY(2,2)-POINXY(2,1))/2.+25./PPP(2)   !BOTTOM LEFT CORNER
	IF (APCS.EQ.1) THEN	                       !AMPL
	  POINXY(2,1)=PAREA(1)+YSIZ                    !Y LEFT/RIGHT BOTTOM
	ELSE	    		                       !PHASE
	  POINXY(2,2)=PAREA(3)-YSIZ                    !Y LEFT/RIGHT TOP
	ENDIF
	LENYAS=YSIZ-50./PPP(2)                         !LENGTH Y-AX
	IF (IFR_MODE.EQ.'BAND') THEN
	  IF (NHACH.EQ.1) THEN
	    I1=PAREA(2)-PAREA(0)
	  ELSE
	    I1=(PAREA(2)-PAREA(0))/(NHACH-1)
	  END IF
	END IF
	DO I0=0,NHACH-1
	  IF (IFR_MODE.EQ.'BAND') THEN
	    CPO=PAREA(0)-I1+I0*I1                      !CURRENT POINT OFFSET
	    IF ((I0.GT.0).AND.(I0.LT.NHACH)) THEN
	      HPO=I1/2
	    ELSE
	      HPO=0.
	    END IF
	  ELSE
	    LHA=LHA+HAINCR
	    CPO=PAREA(0)+XFAC*(360.*(LHA-HARA(0))/HASC)  !CURRENT POINT OFFSET
	    HPO=180*XFAC*HAINCR/HASC	               !HALF POINT OFFSET
	  END IF
c
c This version will not plot points with deleted points directly 
c before and after it 
c	  IF ((NEW(I0).NE.1E20).AND.(OLD(I0).NE.1E20)) THEN
c	    PG(1,1)=CPO
c	    PG(2,1)=POINXY(2,2)-LENYAS*
c	1	     ((MINMAX(2,APCS)-OLD(I0))/LENMM)
c	    PG(1,2)=CPO+2*HPO
c	    PG(2,2)=POINXY(2,2)-LENYAS*
c	1	     ((MINMAX(2,APCS)-NEW(I0))/LENMM)
c	    CALL WQPOLL(2,PG)
c	  END IF
c The version below will plot them
	  IF (NEW(I0).EQ.1E20) THEN
	    IF (OLD(I0).NE.1E20) THEN
	      PG(1,1)=CPO
	      PG(2,1)=POINXY(2,2)-LENYAS*
	1	     ((MINMAX(2,APCS)-OLD(I0))/LENMM)
	      PG(1,2)=CPO+HPO
	      PG(2,2)=POINXY(2,2)-LENYAS*
	1	     ((MINMAX(2,APCS)-OLD(I0))/LENMM)
	      CALL WQPOLL(2,PG)
	    END IF
	  ELSE
	    IF (OLD(I0).NE.1E20) THEN
	      PG(1,1)=CPO
	      PG(2,1)=POINXY(2,2)-LENYAS*
	1	     ((MINMAX(2,APCS)-OLD(I0))/LENMM)
	      PG(1,2)=CPO+2*HPO
	      PG(2,2)=POINXY(2,2)-LENYAS*
	1	     ((MINMAX(2,APCS)-NEW(I0))/LENMM)
	      CALL WQPOLL(2,PG)
	    ELSE
	      PG(1,1)=CPO+2*HPO
	      PG(2,1)=POINXY(2,2)-LENYAS*
	1	     ((MINMAX(2,APCS)-NEW(I0))/LENMM)
	      PG(1,2)=CPO+HPO
	      PG(2,2)=POINXY(2,2)-LENYAS*
	1	     ((MINMAX(2,APCS)-NEW(I0))/LENMM)
	      CALL WQPOLL(2,PG)
	    END IF
	  END IF
	END DO 		                               !PLOT IFR/TEL
C
C
	END
