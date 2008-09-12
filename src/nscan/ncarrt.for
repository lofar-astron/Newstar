C+ NCARRT.FOR
C  WNB 900312
C
C  Revisions:
C	WNB 930826	Add dipole angle check
C       CMV 030116	Acommodated IFR tables that are not sorted 
C
	SUBROUTINE NCARRT(NIFR,BAS,BASDEV,IRED,ANG)
C
C  Get redundancy table
C
C  Result:
C
C	CALL NCARRT( NIFR_J:I, BAS_E(0:*):I, BASDEV_E:I,
C			IRED_J(0:*):O, ANG_E(0:2,0:*):I)
C					Get the table of redundant baselines
C					in IRED, using NIFR interferometers
C					with baselines BAS. BASDEV gives the
C					maximum deviation for redundancy.
C					ANG is the dipole position
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER NIFR			!# OF INTERFEROMETERS
	REAL BAS(0:*)			!BASELINES (<0: FORGET)
	REAL BASDEV			!BASELINE DEVIATION
	INTEGER IRED(0:*)		!REDUNDANCY VECTOR
	REAL ANG(0:2,0:*)		!DIPOLE POSITIONS
C
C  Function references:
C
C
C  Data declarations:
C
	REAL DA(0:2)		!ANGLE CHECKS
C-
C
C FIND BASELINES (NO SINGLES WILL BE COUNTED)
C
	I=0						!BASELINE COUNT
	DO I1=0,NIFR-1					!INITIALIZE
	  IRED(I1)=-1					!DEFAULT NON-REDUNDANT
	END DO
C
C CHECK EACH BASELINE AGAINST ALL OTHERS, COUNT IF AT LEAST ONE MATCH
C
	DO I1=0,NIFR-1					!ALL BASELINES
	  IF (IRED(I1).EQ.-1.AND.BAS(I1).GE.0) THEN	!NOT YET CHECKED
	    R0=BAS(I1)					!NEW CHECK BASELINE
	    DA(0)=ANG(0,I1)
	    DA(1)=ANG(1,I1)
	    DA(2)=ANG(2,I1)
	    DO I2=I1+1,NIFR-1
	      IF (ABS(BAS(I2)-R0).LE.BASDEV .AND.
	1	  ABS(DA(0)-ANG(0,I2)).LE.1E-6 .AND.
	1	  ABS(DA(1)-ANG(1,I2)).LE.1E-6 .AND.
	1	  ABS(DA(2)-ANG(2,I2)).LE.1E-6) THEN !FOUND ONE
		IF (IRED(I1).EQ.-1) THEN		!FIRST MATCH
	           I=I+1				!COUNT
		   IRED(I1)=I
		END IF
	        IRED(I2)=I
	      END IF
	    END DO
	  END IF	  
	END DO
C
	RETURN						!READY
	END
