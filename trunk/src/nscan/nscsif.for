C+ NSCSIF.FOR
C  WNB 910208
C
C  Revisions:
C	WNB 930825	Add ANG
C	WNB 940227	Add NSCSIA
C
	LOGICAL FUNCTION NSCSIF(FCA,STHJ,IFRT,IFRA,ANG)
C
C  Read interferometers for a set
C
C  Result:
C
C	NSCSIF_L = NSCSIF( FCA_J:I, STHJ_B(0:*):I, IFRT_I(0:*):O,
C			IFRA_J(0:1,0:*):O, ANG_E(0:2,0:*):O)
C				Read the interferometer table belonging to
C				set with set header STH from file FCA into
C				the interferometer table (west+256*east) IFRT.
C				In addition fill IFRA array with west(0) and
C				east(1), and ANG will be filled with
C				parallactic angle X-dipole W telescope in
C				circles(0); the sine of the E telescope X dipole
C				offset from W X-dipole (1); and its cosine (2)
C	NSCSIA_L = NSCSIA( FCA_J:I, STHJ_B(0:*):I, IFRT_I(0:*):I,
C			IFRA_J(0:1,0:*):O, ANG_E(0:2,0:*):O)
C				Using IFRT as input, calculate IFRA and ANG
C				as above. FCA not used
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'STH_O_DEF'		!SET HEADER
C
C  Parameters:
C
C
C  Entry points:
C
	LOGICAL NSCSIA
C
C  Arguments:
C
	INTEGER FCA			!FILE CONTROL AREA
	INTEGER STHJ(0:*)		!CURRENT SET HEADER
	INTEGER*2 IFRT(0:*)		!INTERFEROMETER TABLE
	INTEGER IFRA(0:1,0:*)		!INTERFEROMETER ARRAY
	REAL ANG(0:2,0:*)		!DIPOLE ANGLES
C
C  Function references:
C
	LOGICAL WNFRD			!READ DATA
C
C  Data declarations:
C
C-
C
C INIT
C
	NSCSIF=.TRUE.					!ASSUME OK
C
C READ
C
	IF (.NOT.WNFRD(FCA,2*STHJ(STH_NIFR_J),IFRT, 	!READ TABLE
	1		STHJ(STH_IFRP_J))) THEN
	  NSCSIF=.FALSE.				!ERROR READING TABLE
C
	  RETURN
	END IF
	GOTO 10
C
C NSCSIA
C
	ENTRY NSCSIA(FCA,STHJ,IFRT,IFRA,ANG)
C
	NSCSIA=.TRUE.
	GOTO 10
C
C MAKE IFRA
C
 10	CONTINUE
	DO I=0,STHJ(STH_NIFR_J)-1			!MAKE ARRAY
	  IFRA(0,I)=MOD(IFRT(I),256)			!WEST TEL.
	  IFRA(1,I)=IFRT(I)/256				!EAST TEL.
	END DO
C
C MAKE ANG
C
	DO I=0,STHJ(STH_NIFR_J)-1
	  ANG(0,I)=IAND(3,ISHFT(STHJ(STH_DIPC_J),
	1		-2*IFRA(0,I)))/8.		!ANGLE W TELESCOPE
	  ANG(2,I)=IAND(3,ISHFT(STHJ(STH_DIPC_J),
	1		-2*IFRA(1,I)))/8.-ANG(0,I)	!DIFFERENCE E TELESCOPE
	  ANG(1,I)=SIN(ANG(2,I)*PI2)			!ITS SINE
	  ANG(2,I)=COS(ANG(2,I)*PI2)			!AND COSINE
	END DO
C
	RETURN
C
C
	END
