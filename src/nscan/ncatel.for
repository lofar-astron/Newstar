C+ NCATEL.FOR
C  CMV 940428
C
C  Revisions:
C	CMV 940428	Created based on 'T' option in NFLPRT.FOR
C       HjV 940516	Remove R0 declaration
C	CMV 940518	Add option to select interferometers
C	JPH 940909	Comments
C	WNB 981022	Make usable for loops
C
	SUBROUTINE NCATEL(FCA,SETS1,HA1,HA2,SIFRS1,PCGAN1,PCPHS1,OUT)
C
C  Get initial estimates for gain and phase
C
C  Result:
C
C	CALL NCATEL(FCA_J:I,SETS1(0:7,0:*)_J:I,
C			HA1_E:I, HA2_E:I, SIFRS1(0:STHTEL-1,0:STHTEL-1)_B:I,
C			PCGAN1(0:STHTEL-1,0:1)_E:O,
C			PCPHS1(0:STHTEL-1,0:1)_E:O),OUT_J:I
C
C		Will find average gains in PCGAN1 and phase offsets in PCPHS1
C		for the data in the specified SETS1 in FCA, in hour-angle 
C		range HA1..HA2 and for the interferometers in SIFRS.
C		Print result on OUT if >0 (F_T,F_TP).
C
C	CALL NCATL1(FCA_J:I,STH(0:*)_B:I,
C			HA1_E:I, HA2_E:I, SIFRS1(0:STHTEL-1,0:STHTEL-1)_J:I,
C			PCGAN1(0:STHTEL-1,0:1)_E:O,
C			PCPHS1(0:STHTEL-1,0:1)_E:O,OUT_J:I)
C
C		Idem but for single sector described by STH
C
C
C  PIN references:
C
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'NCA_DEF'
	INCLUDE 'CBITS_DEF'
	INCLUDE 'STH_O_DEF'		!SET HEADER
	INCLUDE 'SCH_O_DEF'		!SCAN HEADER
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER FCA                     !FILE TO SEARCH
	INTEGER SETS1(0:7,0:*)           !SETS TO DO
	BYTE STH_IN(0:*)		!HEADER OF SINGLE SET
	REAL HA1,HA2			!HOUR ANGLE RANGE
	BYTE SIFRS1(0:STHTEL-1,0:STHTEL-1) !IFR SELECTION
	REAL PCGAN1(0:STHTEL-1,0:1)	!RETURN GAIN CORRECTIONS
	REAL PCPHS1(0:STHTEL-1,0:1)	!RETURN PHASE OFFSETS
	INTEGER OUT			!OUTPUT FOR PRINT
C
C  Function references:
C
	LOGICAL WNFRD			!READ FILE
	LOGICAL NSCSTL			!GET SETS TO DO
	CHARACTER*32 WNTTSG		!SET NAME
	REAL WNGEFD			!FRACTIONS TO DEGREES
	LOGICAL NSCSIF			!READ IFR TABLE
C
C  Data declarations:
C
	LOGICAL SINGLE			!SINGLE SET
	LOGICAL MORE			!MORE SETS TO DO
	REAL HA				!HA OF SCAN
	COMPLEX C0			!COMPLEX BUFFER
	INTEGER SNAM(0:7)		!SET NAME
	CHARACTER*78 TEXT(-1:STHTEL)	!TEXT LINES
	CHARACTER*(STHTEL) TELS1		!TELESCOPE NAMES
	  DATA TELS1/'0123456789ABCD'/
C
	INTEGER*2 IFRT(0:STHIFR-1)	!IFR TABLE
	INTEGER IFRA(0:1,0:STHIFR-1)    ! IFR TABLE
	REAL ANG(0:2,0:STHIFR-1)        ! DIPOLE ANGLES
C
	INTEGER GPN(0:STHTEL-1,0:1)	!TEL. GAIN COUNTS
	COMPLEX GPV(0:STHTEL-1,0:1)	!TEL. GAINS
C
	INTEGER STHP			!SET HEADER POINTER
	INTEGER SCHP			!SCAN HEADER POINTER
	BYTE STH(0:STHHDL-1)		!SET HEADER
	  INTEGER STHJ(0:STHHDL/4-1)
	  INTEGER*2 STHI(0:STHHDL/2-1)
	  REAL STHE(0:STHHDL/4-1)
	  EQUIVALENCE (STH,STHJ,STHI,STHE)
	BYTE SCH(0:SCHHDL-1)		!SCAN HEADER
	INTEGER*2 ODAT(0:2,0:800)	!DATA
C-
C
	SINGLE=.FALSE.				!NOT SINGLE SET
	MORE=NSCSTL(FCA,SETS1,STH,STHP,SNAM,LPOFF)	!GET FIRST SET
	GOTO 10
C
	ENTRY NCATL1(FCA,STH_IN,HA1,HA2,SIFRS1,PCGAN1,PCPHS1,OUT)
C
	SINGLE=.TRUE.				!SINGLE SET
	MORE=.TRUE.				!ONE SET MORE TO DO
	CALL WNGMV(STHHDL,STH_IN,STH)		!COPY SET HEADER
	GOTO 10
C
  10	CONTINUE
C
C	Initialise buffers
C
	DO I1=0,1				!X,Y
	  DO I2=0,STHTEL-1			!TELESCOPES
	    GPN(I2,I1)=0			!NO DATA
	    GPV(I2,I1)=CMPLX(1.,0.)		!ASSUME PERFECT GAIN
	    PCGAN1(I2,I1)=1			! IDEM
	    PCPHS1(I2,I1)=0			! IDEM
	  END DO
	END DO
C
C	Loop over all sectors
C
	DO WHILE (MORE)
	  IF (.NOT.NSCSIF(FCA,STH,IFRT,IFRA,ANG)) THEN !READ IFR TABLE
	     CALL WNCTXT(F_TP,'Error reading interferometer table')
	     GOTO 100				!TRY NEXT SET
	  END IF
C
C	Loop over all scans per sector
C
	  DO I3=0,STHJ(STH_SCN_J)-1			!ALL SCANS
	    HA=STHE(STH_HAB_E)+I3*STHE(STH_HAI_E) 	!HA OF SCAN
	    IF (HA.GE.HA1-STHE(STH_HAI_E)/2+1E-5 .AND.
	1		HA.LE.HA2+STHE(STH_HAI_E)/2-1E-5) THEN !DO
	      SCHP=STHJ(STH_SCNP_J)+I3*STHJ(STH_SCNL_J) !SCAN HEADER POINTER
	      IF (.NOT.WNFRD(FCA,SCHHDL,SCH,SCHP)) THEN !READ SCAN
	         CALL WNCTXT(F_TP,'Error reading scan header')
	      ELSE IF (.NOT.WNFRD(FCA,
	1	  3*LB_I*STHI(STH_PLN_I)*STHJ(STH_NIFR_J),
	1	  ODAT(0,0),SCHP+SCHHDL)) THEN		!READ DATA
	          CALL WNCTXT(F_TP,'Error reading scan header')
	      ELSE
C
C	Loop over all interferometers and polarisations
C
	        DO I2=0,STHJ(STH_NIFR_J)-1		!ALL INTERFEROMETERS
	          DO I1=0,1				! xx, yy
	           IF (I1.EQ.0) THEN
	             I=STHI(STH_PLN_I)*I2		! xx DATA POINTER
	           ELSE IF (STHI(STH_PLN_I).EQ.2) THEN
	             I=STHI(STH_PLN_I)*I2+1		! yy DATA POINTER
	           ELSE IF (STHI(STH_PLN_I).EQ.4) THEN
	             I=STHI(STH_PLN_I)*I2+3		! yy DATA POINTER
		   ELSE
	             I=-1				!NOT PRESENT
	           END IF
		   IF (I.GE.0) THEN			!POL. PRESENT
	             IF (ODAT(0,I).NE.0) THEN		!DATA PRESENT
		       I5=ODAT(0,I)			!WEIGHT/FLAGS
	 	       IF (IAND(FL_ALL,I5).EQ.0) THEN	!USE DATA
C
		        I4=IFRT(I2)/256			!EAST
		        I5=MOD(IFRT(I2),256)		!WEST
			IF (SIFRS1(I5,I4)) THEN		!SELECTED
		          C0=CMPLX(ODAT(1,I),ODAT(2,I))	!MAKE COMPLEX
		          C0=C0/GPV(I4,I1)/CONJG(GPV(I5,I1)) !CORRECT FOR KNOWN
		          IF (GPN(I4,I1).EQ.0) THEN	!DISTRIBUTE
		             IF (GPN(I5,I1).EQ.0) THEN
		                C0=SQRT(C0)		!TO USE
		                GPV(I4,I1)=GPV(I4,I1)*C0
		                GPV(I5,I1)=GPV(I5,I1)*CONJG(C0)
		             ELSE
		                GPV(I4,I1)=GPV(I4,I1)*C0
		             END IF
		          ELSE
		             IF (GPN(I5,I1).EQ.0) THEN
		                GPV(I5,I1)=GPV(I5,I1)*CONJG(C0)
		             ELSE
		                C0=SQRT(C0)		!TO USE
		                GPV(I4,I1)=GPV(I4,I1)*C0
		                GPV(I5,I1)=GPV(I5,I1)*CONJG(C0)
		             END IF
		          END IF
		          GPN(I4,I1)=1			!INDICATE USED
		          GPN(I5,I1)=1
		        END IF
		       END IF
	             END IF
	           END IF
	          END DO
	        END DO
	      END IF
	    END IF
	  END DO
C
  100	  CONTINUE
	  IF (SINGLE) THEN				!SINGLE SET: DONE
	     MORE=.FALSE.
	  ELSE
	     MORE=NSCSTL(FCA,SETS1,STH,STHP,SNAM,LPOFF)	!GET NEXT SET
	  END IF
	END DO
C
C	Calculate average gain and phase offset
C
	DO I1=0,1				!X,Y
	  R0=0
	  I3=0
	  DO I2=0,STHTEL-1			!DETERMINE AVERAGE GAIN
	    IF (GPN(I2,I1).NE.0) THEN
	      R0=R0+ABS(GPV(I2,I1))
	      I3=I3+1
	    END IF
	  END DO
	  IF (I3.NE.0) THEN			!APPLY AVERAGE
	    R0=R0/I3
	    DO I2=0,STHTEL-1
	      IF (GPN(I2,I1).NE.0) GPV(I2,I1)=GPV(I2,I1)/R0
	      PCGAN1(I2,I1)=ABS(GPV(I2,I1))			!GAIN
	      PCPHS1(I2,I1)=-1*DEG*ATAN2(AIMAG(GPV(I2,I1)),
	1				 REAL(GPV(I2,I1))) 	!PHASE
	    END DO
	  END IF
	END DO
C
C Print at request
C
	IF (OUT.NE.0) THEN
	  TEXT(-1)=' '				!HEADING
	  DO I=0,STHTEL-1
	    TEXT(-1)(I*5+12:I*5+12)=TELS1(I+1:I+1)
	    PCGAN1(I,0)=PCGAN1(I,0)*100		!Make %
	    PCGAN1(I,1)=PCGAN1(I,1)*100
	  END DO
C
	  CALL WNCTXT(OUT,'!/!#$AS',LEN(TEXT(0)),TEXT(-1)) !SHOW HEADING
	  CALL WNCTXT(OUT,'!Q1\Gain(%):!10C!5$#E10.0',
	1		STHTEL,PCGAN1(0,0))			  !GAIN X
	  CALL WNCTXT(OUT,'!Q1\!10C!5$#E10.0',STHTEL,PCGAN1(0,1))  !GAIN Y
	  CALL WNCTXT(OUT,'!Q1\Phase(d):!10C!5$#E10.0',
	1		STHTEL,PCPHS1(0,0))			  !PHASE X
	  CALL WNCTXT(OUT,'!Q1\!10C!5$#E10.0',STHTEL,PCPHS1(0,1))  !PHASE Y
	  CALL WNCTXT(OUT,' ')
C
	  DO I=0,STHTEL-1
	    PCGAN1(I,0)=PCGAN1(I,0)/100		!Back to fractions
	    PCGAN1(I,1)=PCGAN1(I,1)/100
	  END DO
	END IF
C
	RETURN
	END
