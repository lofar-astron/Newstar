C+ NPLCON.FOR
C  HjV 931108
C
C  Revisions:
C	JPH 9711..	Changes in connecting dotted lines (replacing a mess by
C			 somethging which is somewhat better ...)
C
C
	SUBROUTINE NPLCON (NPLOT,CPO,HPO,BEGIN)
C
C  Plot connection lines
C
C  Result:
C
C	CALL NPLCON              	Plot connection lines
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
	INTEGER NPLOT			!# OF INTERFEROMETERS TO DO
	REAL CPO                        !CURRENT POINT OFFSET
	REAL HPO                        !HALF POINT OFFSET
	LOGICAL BEGIN                   !.TRUE.=FIRST SCAN, .FALSE.=LAST SCAN
C
C  Function references:
C
C
C  Data declarations:
C
C-
	IF (BEGIN) THEN                                 !AT BEGIN PLOT
	  CALL WQSPLI(4)                  		!DOTTED =3
cc	  POINXY(2,1)=PAREA(3)-YFAC*11./PPP(2)
	  POINXY(2,1)=CPO+YFAC*4./PPP(2)
	  POINXY(2,2)=CPO	! +HPO
	  DO I=0,NPLOT-1
	    IF (NEW(I).NE.1E20) THEN
	      POINXY(1,1)=IFOFF(I)
	      POINXY(1,2)=NEW(I)
              CALL WQPOLL(2,POINXY)
	    END IF
	  END DO
	ELSE             				!AT END PLOT
          DO I2=0,NPLOT-1				!LAST BIT SCAN
	    IF (NEW(I2).NE.1E20) THEN
	      POINXY(1,1)=NEW(I2)
	      POINXY(2,1)=CPO
	      POINXY(1,2)=NEW(I2)
	      POINXY(2,2)=CPO-HPO
	      CALL WQPOLL(2,POINXY)
	    END IF
	  END DO
	  CALL WQSPLI(4)	                        !DOTTED =3
	  POINXY(2,1)=CPO
	  POINXY(2,2)=CPO-YFAC*4./PPP(2)
	  DO I=0,NPLOT-1
	    IF (NEW(I).NE.1E20) THEN
	      POINXY(1,1)=NEW(I)
	      POINXY(1,2)=IFOFF(I)
              CALL WQPOLL(2,POINXY)
	    END IF
	  END DO
	ENDIF
	CALL WQSPLI(1)		                        !FULL
cc	print*, parea(3), ppp(2), yfac, begin, cpo,hpo,poinxy(2,1),poinxy(2,2)
C
C
	END
