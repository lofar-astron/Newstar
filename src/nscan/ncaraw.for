C+ NCARAW.FOR
C  WNB 910207
C
C  Revisions:
C
	SUBROUTINE NCARAW(MWGT,MWGTD,NIFR,BASEL,RAWGT)
C
C  Calculate align weights
C
C  Result:
C
C	CALL NCARAW( MWGT_J:I, MWGTD_E(0:1):I, NIFR_J:I,
C				BASEL(0:*):I, RAWGT_R(0:*):O)
C					Calculate the align weights RAWGT for
C					NIFR interferometers at baselines (m)
C					BASEL.
C					MWGT is the type:
C					1= step; 2= gaussian; 3= triangle
C					-1,...= 1-function.
C					MWGTD specifies (m) the centre (0) and
C					the halfwidth (1) of the function.
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'STH_O_DEF'			!SET HEADER
C
C  Entry points:
C
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER MWGT				!TYPE OF WEIGHT
	REAL MWGTD(0:1)				!WEIGTH CENTRE, HALFWIDTH
	INTEGER NIFR				!# OF INTERFEROMETERS
	REAL BASEL(0:*)				!BASELINES
	REAL RAWGT(0:*)				!WEIGHT CALCULATED
C
C  Function references:
C
C
C  Data declarations:
C
C-
	DO I=0,NIFR-1					!ALL BASELINES
	  R0=ABS(BASEL(I)-MWGTD(0))			!BASELINE OFFSET
	  IF (ABS(MWGT).EQ.2) THEN			!GAUSSIAN
	    RAWGT(I)=EXP(-LOG(2.)*((R0/MWGTD(1))**2))
	  ELSE IF (ABS(MWGT).EQ.3) THEN			!TRIANGLE
	    IF (R0.LE.2*MWGTD(1)) THEN			!IN TRIANGLE
	      RAWGT(I)=1-R0
	    ELSE
	      RAWGT(I)=0
	    END IF
	  ELSE						!STEP
	    IF (R0.LE.MWGTD(1)) THEN			!IN STEP
	      RAWGT(I)=1
	    ELSE
	      RAWGT(I)=0
	    END IF
	  END IF
	  IF (MWGT.LT.0) RAWGT(I)=1.-RAWGT(I)		!INVERT
	END DO
C
	RETURN						!READY
C
C
	END
