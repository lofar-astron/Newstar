C+ NSCMBL.FOR
C  WNB 900306
C
C  Revisions:
C
	SUBROUTINE NSCMBL(RTP,NIFR,IFRT,IFS,BASEL)
C
C  Make baselines
C
C  Result:
C
C	CALL NSCMBL( RTP_E(0:*):I, NIFR_J:I, IFRT_I(0:NIFR-1):I,
C			IFS_B(0:*,0:*), BASEL_E(0:*)
C				Make a baseline table BASEL using the telescope
C				positions RTP, and the interferometer table
C				IFRT of length NIFR. IFS is a selection table.
C				The BASEL will be negative for unselected
C				interferometers.
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'STH_O_DEF'		!SET HEADER
C
C  Parameters:
C
C
C  Arguments:
C
	REAL RTP(0:*)			!TEL. POSITIONS
	INTEGER NIFR			!# OF INTERFEROMETERS
	INTEGER*2 IFRT(0:*)		!INTERFEROMETER TABLE
	BYTE IFS(0:STHTEL-1,0:STHTEL-1)	!SELECTION TABLE
	REAL BASEL(0:*)			!BASELINE TABLE
C
C  Function references:
C
C
C  Data declarations:
C
C-
	DO I=0,NIFR-1				!MAKE TABLE
	  J=IFRT(I)				!IFR
          DO I1=0,NIFR-1
	    IF (I1.NE.I .AND. J.EQ.IFRT(I1)) J=-1
	  END DO
          IF (J.GE.0) THEN
	    I1=J/256				!E TEL
	    I2=MOD(J,256)				!W TEL
	    IF (IFS(I1,I2)) THEN
	      BASEL(I)=RTP(I1)-RTP(I2)
	    ELSE
	      BASEL(I)=-1
	    END IF
	  ELSE
	    BASEL(I)=-1
	  END IF
	END DO
C
	RETURN
C
C
	END
