C+ NCLUVT.FOR
C  WNB 920103
C
C  Revisions:
C	JPH 940223	Comments
C	CMV 940419	Pass outsize to NMACVF (TSIZ may be larger than that)
C
	SUBROUTINE NCLUVT(FTMP,FOUT,APH)
C
C  Transform beam to UV cover. If possible, the map taper that copmpensates
C  for the gridding convolution is removed first so the UV coverage obtained
C  is the one from which the beam was originally made.
C
C  Result:
C	CALL NCLUVT ( FTMP_J:I, FOUT_J:I, APH_B(*):I)
C				Transform beam to UV cover, and output
C				to specified file FOUT.
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'MPH_O_DEF'	!MAP HEADER
	INCLUDE 'NCL_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER FTMP		!TEMP. FILE
	INTEGER FOUT		!OUTPUT FILE
	BYTE APH(0:*)		!BEAM HEADER
C
C  Function references:
C
	LOGICAL WNFOP		!OPEN FILE
	LOGICAL WNFRD		!READ DISK
	LOGICAL WNFWR		!WRITE DISK
	LOGICAL WNGGVA		!GET VIRTUAL MEMORY
	LOGICAL WNGFVA		!FREE VIRTUAL MEMORY
	INTEGER WNMEJC		!CEIL(X)
	INTEGER WNGGJ		!GET J VALUE
	INTEGER*2 WNGGI		!GET I VALUE
	REAL WNGGE		!GET E VALUE
C
C  Data declarations:
C
	INTEGER WTBUF		!FFT WEIGHT BUF ADDRESS
	INTEGER FTBUF		!FFT BUFFER ADDRESSES
	INTEGER CVBF		!CONVOLUTION BUFFER
	INTEGER LSIZE,LSIZ8	!SIZE ONE TRANSPOSE STAGE
	INTEGER LSTEP		!TRANSPOSE INPUT DISK STEP
	INTEGER BUFPTR		!TRANSPOSE BUF ADDRESS
	INTEGER OSIZ(2)		!COPY OUTSIZE = MAP SIZES
	INTEGER TSIZ(2)		!COPY FTSIZE
	INTEGER JNRA,JNDC	!MAP SIZES
C-
C
C Set up size parameters, get buffers, set up tables
C
	JNRA=WNGGJ(APH(MPH_NRA_1))		!MAP SIZES
	JNDC=WNGGJ(APH(MPH_NDEC_1))
	OSIZ(1)=JNRA				!COPY TO PASS TO NMACVF
	OSIZ(2)=JNDC
	CALL WNGMV(2*LB_J,APH(MPH_FSR_1),TSIZ(1)) !FFT SIZES
	J=2*LB_E*JNRA				!LENGTH FFT BUFFER
	JS=WNGGVA(J,FTBUF)			!FFT BUFFER
	IF (JS) JS=WNGGVA(J/2,WTBUF)		!FFT cos/sin ("weight") BUFFER
	IF (JS) JS=WNGGVA((J+16)/4,CVBFU)	!DECONVOLUTION BUFFERS
	J=(LB_E/2)*JNDC+LB_E
	IF (JS) JS=WNGGVA(J,CVBFV)
	I1=3*32+1				!CONV. LENGTH
	IF (JS) JS=WNGGVA(LB_E*I1,CVBF)		!CONVOLUTION BUFFER
	IF (.NOT.JS) THEN
	  CALL WNCTXT(F_TP,'Cannot obtain UV FFT buffers')
	  CALL WNGEX				!STOP
	END IF
C
C Make cos/sin tables for FFT
C
	DO I=0,JNRA/2-1				!FILL FFT WEIGHT
	  CALL WNGMV(LB_E,COS(I*PI2/JNRA),A_B(WTBUF-A_OB+2*LB_E*I))
	  CALL WNGMV(LB_E,SIN(I*PI2/JNRA),A_B(WTBUF-A_OB+LB_E+2*LB_E*I))
	END DO
C
C The map may have been tapered to componsate for the effect of the convolution
C  function in the gridding process. We will undo this taper if we can
C
	I2=32					!CONV. STEP
	I3=WNGGI(APH(MPH_CD_1+2*LB_I))		!CONVOLUTION TYPE
	IF (I3.NE.0 .AND.			!DECONVOLUTION
	1	WNGGJ(APH(MPH_FSR_1)).EQ.JNRA .AND.
	1	WNGGJ(APH(MPH_FSD_1)).EQ.JNDC .AND.
	1	APDCV) THEN
	  CALL NMACVF(I1,I3,I2,TSIZ,OSIZ,A_B(CVBF-A_OB),
	1	A_B(CVBFU-A_OB),A_B(CVBFV-A_OB))!make convolution-functn tables
	  CALL WNCTXT(F_TP,
	1	'!/Correction for deconvolution '//
	1	'effects applied!/')
	ELSE
	  CALL NMACVF(I1,0,I2,TSIZ,OSIZ,A_B(CVBF-A_OB),
	1	A_B(CVBFU-A_OB),A_B(CVBFV-A_OB))!make dummy table (all 1s)
	  CALL WNCTXT(F_TP,
	1	'!/No correction for possible '//
	1	'deconvolution effects applied!/')
	END IF
	JS= WNGFVA(I1,CVBF)			!we do not need this one
	CALL NCLF1D(A_B(CVBFU-A_OB),JNRA/2+1)	!INVERT DECONVOLUTION BUFS
	CALL NCLF1D(A_B(CVBFV-A_OB),JNDC/2+1)
C
C DO FIRST PASS FFT: horizontal
C
	J=LB_E*JNRA				!LINE LENGTH
	J=WNGGJ(APH(MPH_MDP_1))+J*JNDC/2	!PTR TO INPUT LINE ZERO
	J2=0					!OUTPUT PTR
	DO J1=0,JNDC/2				!ALL LINES
	  IF (J1.EQ.JNDC/2) THEN		!LAST LINE
	    J=WNGGJ(APH(MPH_MDP_1))		!LAST LINE == FIRST
	  END IF
	  IF (.NOT.WNFRD(FCAMAP,LB_E*JNRA,A_B(FTBUF-A_OB),J)) THEN !READ LINE
	    CALL WNCTXT(F_TP,'Read error beam')
	    CALL WNGEX				!STOP
	  END IF
	  J=J+LB_E*JNRA				!NEXT LINE PTR
	  IF (J1.EQ.0) THEN
	    CALL NCLFD2(A_B(FTBUF-A_OB),JNRA)	!divide first and last
	  ELSE IF (J1.EQ.JNDC/2) THEN		!
	    CALL NCLFD2(A_B(FTBUF-A_OB),JNRA)	! lines by 2
	  END IF
	  I4=JNRA/2
C
C DECONVOLVE by multiplying with the inverted horizontal and vertical tapers
C
	  CALL WNMFSN(I4,A_B(FTBUF-A_OB+LB_E*I4),!right half with 1/taper
	1	A_B(CVBFU-A_OB),	
	1	A_B(CVBFV-A_OB+LB_E*J1))
 	  CALL WNMFIN(I4,A_B(FTBUF-A_OB),	!left half with 1/taper in 
	1	A_B(CVBFU-A_OB+LB_E),		! reverse direction
	1	A_B(CVBFV-A_OB+LB_E*J1))
	  CALL WNMFRC(JNRA,A_B(FTBUF-A_OB))	!convert REAL TO COMPLEX
	  CALL WNMFCS(JNRA,A_B(FTBUF-A_OB))	!SWAP HALVES
	  CALL WNMFTC(JNRA,A_B(FTBUF-A_OB),
	1	A_B(WTBUF-A_OB)) 		!in-place FFT
	  IF (.NOT.WNFWR(FTMP,2*LB_E*(JNRA/2+1),
	1	A_B(FTBUF-A_OB),J2)) THEN 	!OUTPUT PART LINE
	    CALL WNCTXT(F_TP,'Write error temporary UV file')
	    CALL WNGEX				!STOP
	  END IF
	  J2=J2+2*LB_E*(JNRA/2+1)		!NEXT OUTPUT PTR
	END DO
C
C CLEAR BUFFERS
C
	J=2*LB_E*JNRA				!FFT BUFFER
	JS= WNGFVA(J,FTBUF)
	JS= WNGFVA(J/2,WTBUF)			!FFT cos/sin BUFFER
C
C SECOND PASS FFT: vertical
C
C GET BUFFERS
C
	LSIZE=MIN(WNMEJC(MEMSIZ/(REAL(2*LB_E)*(JNDC/2+1))),JNRA/2+1)
						!LENGTH ONE STAGE
	LSIZ8=2*LB_E*LSIZE
	JS=WNGGVA(LSIZ8*(JNDC/2+1),BUFPTR)	!TRANSPOSE BUF
	J=2*LB_E*JNDC				!FFT BUFFER
	IF (JS) JS=WNGGVA(J,FTBUF)
	IF (JS) JS=WNGGVA(J/2,WTBUF)		!FFT cos/sin ("WEIGHT") BUF
	IF (.NOT.JS) THEN
	  CALL WNCTXT(F_TP,'Cannot obtain UV cover transpose buffers')
	  CALL WNGEX				!STOP
	END IF
	DO I=0,JNDC/2-1				!FILL WEIGHT BUF
	  CALL WNGMV(LB_E,COS(I*PI2/JNDC),A_B(WTBUF-A_OB+2*LB_E*I))
	  CALL WNGMV(LB_E,SIN(I*PI2/JNDC),A_B(WTBUF-A_OB+LB_E+2*LB_E*I))
	END DO
	LSTEP=2*LB_E*(JNRA/2+1)			!DISK INPUT STEP
C
C DO ALL LINES in "stages" of LSIZE lines
C
	DO I2=0,JNRA/2,LSIZE			!DO STAGES
	  J0=MIN(LSIZE,JNRA/2+1-I2)		!LENGTH TO DO IN STAGE
	  DO J=0,JNDC/2				!READ A STAGE
	    JS=WNFRD(FTMP,2*LB_E*J0,
	1	A_B(BUFPTR-A_OB+J*LSIZ8),
	1	J*LSTEP+2*LB_E*I2)
	  END DO
	  DO J1=0,J0-1				!ALL LINES IN STAGE
	    DO J=0,JNDC/2
	      CALL WNGMV(LB_X,A_B(BUFPTR-A_OB+J*LSIZ8+LB_X*J1),
	1		A_B(FTBUF-A_OB+LB_X*J))	!TRANSPOSE
	    END DO
	    J2=LB_X*(JNDC/2-1)			!ZERO LENGTH
	    CALL WNGMVZ(J2,
	1	A_B(FTBUF-A_OB+LB_X*(JNDC/2+1)))!ZERO BUF
	    CALL WNMFTC(JNDC,A_B(FTBUF-A_OB),
	1	A_B(WTBUF-A_OB)) !FFT
	    CALL WNMFCS(JNDC,A_B(FTBUF-A_OB))	!SWAP HALVES
	    CALL WNMFCR(JNDC,A_B(FTBUF-A_OB))	!MAKE REAL
	    IF (I2+J1.EQ.0) CALL 
	1	NCLFD2(A_B(FTBUF-A_OB),JNDC) 	!HALve VALUES
	    IF (.NOT.WNFWR(FOUT,LB_E*JNDC,
	1	A_B(FTBUF-A_OB),-1)) THEN 	!OUTPUT LINE
	      CALL WNCTXT(F_TP,'Write error UV cover')
	      CALL WNGEX			!STOP
	    END IF
	  END DO				!END LINES
	END DO					!END STAGE
C
C CLEAR BUFFERS
C
	JS= WNGFVA(LSIZ8*(JNDC/2+1),BUFPTR)	!TRANSPOSE BUF
	J=2*LB_E*JNDC				!FFT BUFFER
	JS= WNGFVA(J,FTBUF)
	JS= WNGFVA(J/2,WTBUF)			!FFT WEIGHT BUF
	CALL NCLF1D(A_B(CVBFU-A_OB),JNRA/2+1)	!INVERT DECONVOLUTION BUFS,
	CALL NCLF1D(A_B(CVBFV-A_OB),JNDC/2+1)	! save for later
C
	RETURN
C
C
	END
