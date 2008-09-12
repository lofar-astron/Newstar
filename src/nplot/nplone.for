C+ NPLONE.FOR
C  HjV 931108
C       Combined parts of old version of NPLTEL and NPLRES
C
C  Revisions:
C
	SUBROUTINE NPLONE (NPLOT,CPO,HPO)
C
C  Plot A/P/C/S scan/sets
C
C  Result:
C
C	CALL NPLONE              	Plot scan/sets
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
C
C  Function references:
C
C
C  Data declarations:
C
C-
	DO I=0,NPLOT-1
	   IF (NEW(I).EQ.1E20) THEN
	      IF (OLD(I).NE.1E20) THEN
		 POINXY(1,1)=OLD(I)
		 POINXY(2,1)=CPO+2*HPO
		 POINXY(1,2)=OLD(I)
		 POINXY(2,2)=CPO+HPO
		 CALL WQPOLL(2,POINXY)
	      END IF
	   ELSE
	      IF (OLD(I).NE.1E20) THEN
		 POINXY(1,1)=OLD(I)
		 POINXY(2,1)=CPO+2*HPO
		 POINXY(1,2)=NEW(I)
		 POINXY(2,2)=CPO
		 CALL WQPOLL(2,POINXY)
	      ELSE
		 POINXY(1,1)=NEW(I)
		 POINXY(2,1)=CPO+HPO
		 POINXY(1,2)=NEW(I)
		 POINXY(2,2)=CPO
		 CALL WQPOLL(2,POINXY)
	      END IF
	   END IF
	END DO 		           	!PLOT IFR/TEL
C
C
	END
