C+ NCLUC1.FOR
C  WNB 920106
C
C  Revisions:
C	WNB 931006	Text
C	WNB 931124	Limit source positions to within map
C	JPH 940224	Comments - Check map limits also for non-clean 
C			 components
C
C
	SUBROUTINE NCLUC1(STSRC,NDSRC,NLIN,MLIN,BLIN,WLIN1,WLIN2,
	1		MDHJ,MPH,APH,EXBUF)
C
C  Clean/restore 1 map line using transformed beam
C
C  Result:
C	CALL NCLUC1 ( STSRC_J:I, NDSRC_J:I, NLIN_J:I, MLIN_X(0:*):IO,
C			BLIN_E(0:*):I, WLIN1_X(0:*):I, WLIN2_X(0:*):I,
C			MDHJ_J(*):I, MPH_B(*):I, APH_B(*):I,
C			EXBUF_E(*):O)
C				Clean/restore sources STSRC until NDSRC using
C				the beam line in BLIN from the map line in
C				MLIN. 
C				NLIN is the line number. WLIN1 is the
C				FFT cos/sin buffer, WLIN2 its conjugate
C				(sin --> -sin), EXBUF a buffer that can be used
C				for extended-source weights.
C				MPH and APH are the map and beam headers,
C				MDH the model header
C
C  NOTES: 
C	This routine is part of NCLUCL and can only be understood in
C connection with it. It is used first to subtract source components and later
C to restore them.
C	NCLUCL calls this routine with MLIN cleared.
C	The transformed beam is in transposed format, so BLIN in fact contains
C a column of the beam. Correspondingly, MLIN is used to hold a column of
C the map and WLINB1/2 contain cos/sin tables for the FFT in the DEC direction.
C	It is assumed that the source model may contain non-clean point sources.
C These are forced onto the nearest grid point.
C
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'MPH_O_DEF'	!MAP HEADER
	INCLUDE 'MDH_O_DEF'	!MODEL HEADER
	INCLUDE 'MDL_O_DEF'	!MODEL LINE
	INCLUDE 'NCL_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER STSRC		!START SOURCE #
	INTEGER NDSRC		!END SOURCE #
	INTEGER NLIN		!MAP LINE #
	COMPLEX MLIN(0:*)	!MAP LINE
	REAL BLIN(0:*)		!BEAM LINE
	COMPLEX WLIN1(0:*)	!FFT WEIGHT LINE
	COMPLEX WLIN2(0:*)	!FFT CONJUGATE WEIGHT LINE
	INTEGER MDHJ(0:*)	!MODEL HEADER
	BYTE MPH(0:*)		!MAP HEADER
	BYTE APH(0:*)		!BEAM HEADER
	REAL EXBUF(0:*)		!WEIGHT FOR EXTENDED SOURCES
C
C  Function references:
C
	INTEGER WNGGJ		!GET J VALUE
	DOUBLE PRECISION WNGGD	!GET D VALUE
C
C  Data declarations:
C
	INTEGER JNRA,JNDC	!MAP SIZES
	INTEGER JRA2,JDC2,JRAM1,JDCM1
	REAL BMS(3)		!EXTENSIONS
	COMPLEX C0,C1,C2
	REAL R2
C-
	JNDC=WNGGJ(APH(MPH_NDEC_1))		!# OF LINES
	JNRA=WNGGJ(APH(MPH_NRA_1))		!LINE LENGTH
	JRA2=-JNRA/2				!MAP LIMITS from -n to +n-1
	JDC2=-JNDC/2
	JRAM1=-JRA2-1
	JDCM1=-JDC2-1
C
C Do a slow Fourier transform of the clean components in the l direction
C  using buffer MLIN to hold a single column of the source transform 
C Check that the source is within the map, lest sources outside are aliased into
C  it by the FFT
C
	DO I=MAX(0,STSRC-1),NDSRC-1		!ALL SOURCES
	  J0=(MDHJ(MDH_MODP_J)+
	1	I*MDLHDL-A_OB)/LB_E 		!MODEL POINTER
	  I0=A_B(J0*LB_E+MDL_TP_B)		!BITS
	  IF (A_E(J0+MDL_I_E).NE.0 .AND.
	1	IAND(I0,MDLCLN_M).NE.0) THEN 	!NOT DELETED AND a CLEAN comp.
	    I1=NINT((A_E(J0+MDL_M_E)/PI2-	!SRC POS. M LINE
	1	WNGGD(MPH(MPH_SHD_1)))/		! SHD= DEC shift
	1	WNGGD(MPH(MPH_SDEC_1)))		! SDEC= DEC grid spacing	
	    I2=NINT((A_E(J0+MDL_L_E)/PI2-
	1	WNGGD(MPH(MPH_SHR_1)))/
	1	WNGGD(MPH(MPH_SRA_1)))		!L
	    IF (I1.GT.JDC2 .AND. I1.LT.JDCM1
	1	.AND. I2.GT.JRA2 
	1	.AND. I2.LT.JRAM1) THEN 	!SOURCE IN MAP
	      R0=PI2*I2*NLIN/JNRA		!PHASE OF SOURCE
	      C0=-A_E(J0+MDL_I_E)*
	1	CMPLX(COS(R0),SIN(R0)) 		!SOURCE COMPONENT
	      IF (I1.LT.0) I1=JNDC+I1		!SWAP
	      MLIN(I1)=MLIN(I1)+C0		!PUT SRC IN LINE (SWAPPED!)
	    END IF
	  END IF
	END DO
C
C TRANSFORM SOURCE column TO PROPER UV: m-direction transform
C
	CALL WNMFTC(APH(MPH_NDEC_1),MLIN,WLIN1)	!DO FFT
C
C DO non-clean POINT SOURCES - similar to above. Th
C Such point sources may have been entered in the model by other mechanisms
C  than Clean and therefore are not necessarily on grid points. 

C
	DO I=MAX(0,STSRC-1),NDSRC-1		!ALL SOURCES
	  J0=(MDHJ(MDH_MODP_J)+I*MDLHDL-A_OB)/LB_E !MODEL POINTER
	  I0=A_B(J0*LB_E+MDL_TP_B)		!BITS
	  IF (A_E(J0+MDL_I_E).NE.0 .AND.
	1		IAND(I0,MDLCLN_M).EQ.0) THEN !NOT DELETED AND NOT CLEAN
	    R1=(A_E(J0+MDL_M_E)/PI2-
	1		WNGGD(MPH(MPH_SHD_1)))/
	1		WNGGD(MPH(MPH_SDEC_1))	!SRC POS. M LINE
	    R2=(A_E(J0+MDL_L_E)/PI2-
	1		WNGGD(MPH(MPH_SHR_1)))/
	1		WNGGD(MPH(MPH_SRA_1))	!L
	    IF (R1.GT.JDC2 .AND. R1.LT.JDCM1
	1	 .AND.R2.GT.JRA2 
	1	.AND. R2.LT.JRAM1) THEN 	!SOURCE IN MAP
	      R0=PI2*R2*NLIN/JNRA		!L PHASE OF SOURCE
	      C0=-A_E(J0+MDL_I_E)*
	1	CMPLX(COS(R0),SIN(R0)) 		!L SOURCE COMPONENT
	      C1=EXP(CMPLX(0.,PI2*R1/JNDC))	!M PHASE STEP
	      C2=C0
	      IF (A_E(J0+MDL_EXT_E+2).EQ.0 .AND.!NOT EXTENDED
	1		A_E(J0+MDL_EXT_E+1)
	1	-A_E(J0+MDL_EXT_E).EQ.0) THEN
	        MLIN(0)=MLIN(0)+C0		!SET SOURCE
	        DO I1=1,JNDC/2
	          C0=C0*C1			!NEXT DATA
	          C2=C2*CONJG(C1)
	          MLIN(I1)=MLIN(I1)+C0		!PUT SOURCE IN LINE
	          MLIN(JNDC-I1)=MLIN(JNDC-I1)+C2
	        END DO
	      ELSE				!EXTENDED
	        CALL NMOEXT(A_E(J0))		!EXTERNAL FORMAT
	        R0=2.*3600.*360.*SQRT(LOG(2.))	!CONVERSION ARCSEC/INTERNAL
	        BMS(1)=0.5*PI*A_E(J0+MDL_EXT_E+0)
	1		/R0/WNGGD(APH(MPH_SRA_1))/
	1		JNRA			!MAKE UNITS
	        BMS(2)=0.5*PI*A_E(J0+MDL_EXT_E+1)
	1		/R0/WNGGD(APH(MPH_SDEC_1))/
	1		JNDC
	        BMS(3)=-A_E(J0+MDL_EXT_E+2)
	1		*PI/180. 		!RADIANS
	        IF (NLIN.EQ.0) THEN		!GET NORM. WEIGHT FIRST
		  A_E(J0+MDL_RS_E)=0		!START NORM
		  DO I1=0,JNRA/2
		    CALL NCLFBM(I1,JNDC,BMS(1),
	1		BMS(2),BMS(3),
	1		EXBUF,A_E(J0+MDL_RS_E))	!MAKE WEIGHTS
		  END DO
		  A_E(J0+MDL_RS_E)=JNRA*JNDC/4
	1		/A_E(J0+MDL_RS_E) 	!NORM. WEIGHT
	        END IF
	        CALL NCLFBM(NLIN,JNDC,BMS(1),
	1		BMS(2),BMS(3),
	1		EXBUF,R1)		!MAKE WEIGHTS
	        C0=C0*A_E(J0+MDL_RS_E)
	        C2=C2*A_E(J0+MDL_RS_E)
	        MLIN(0)=MLIN(0)+EXBUF(JNDC/2)*C0!SET SOURCE
	        DO I1=1,JNDC/2
	          C0=C0*C1			!NEXT DATA
	          C2=C2*CONJG(C1)
	          MLIN(I1)=MLIN(I1)+EXBUF(JNDC/2+I1)*C0 !PUT SOURCE IN LINE
	          MLIN(JNDC-I1)=MLIN(JNDC-I1)+EXBUF(JNDC/2-I1)*C2
	        END DO
	        CALL NMOEXF(A_E(J0))		!INTERNAL FORMAT
	      END IF
	    END IF
	  END IF
	END DO
C
C WEIGHT column with beam column
C
	CALL WNMFCS(APH(MPH_NDEC_1),MLIN)	!SWAP HALVES
	DO I=0,JNDC-1
	  MLIN(I)=MLIN(I)*BLIN(I)
	END DO
C
C FIRST PART MAP TRANSFORM
C
	CALL WNMFCS(APH(MPH_NDEC_1),MLIN)	!SWAP HALVES
	CALL WNMFTC(APH(MPH_NDEC_1),MLIN,WLIN2)	!TRANSFORM
	CALL WNMFCS(APH(MPH_NDEC_1),MLIN)	!SWAP HALVES
C
	RETURN
C
C
	END
