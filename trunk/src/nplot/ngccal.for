C+ NGCCAL.FOR
C  WNB 920821
C
C  Revisions:
C	WNB 930504	Change for new complex solution
C	HjV 930518	Change name of some keywords
C	WNB 930630	Add SHIFT option
C	WNB 930707	Change sign SHIFT option; allow average option
C	WNB 930826	New HA range
C	CMV 931220	Pass FCA of input file to WNDXLP and WNDSTA/Q
C	CMV 940518	Correct typo in pol.subtraction
C	CMV 940805	Add TRTYP=2 and CPOLY
C	CMV 940811	Option to fit only some coefficients
C       WNB 950621	New LSQ routines
C
	SUBROUTINE NGCCAL
C
C  Calculate something for a plot
C
C  Result:
C
C	CALL NGCCAL			Calculate something for plot
C
C
C  Pin references:
C
C	CALC_TYPE	Type of calculation
C	HA_WIDTH	Smoothing width
C	NGF_SETS	Plots to do
C	POLY_N		Polynomial degree
C	POLY_USE	Polynomial coefficients to fit
C	POLY_COEF	Polynomial coefficients
C	SHIFT		Shift values
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'LSQ_O_DEF'
	INCLUDE 'GFH_O_DEF'		!GENERAL FILE HEADER
	INCLUDE 'SGH_O_DEF'		!SUB-GROUP HEADER
	INCLUDE 'NGF_O_DEF'		!PLOT HEADER
	INCLUDE 'NGC_DEF'
C
C  Parameters:
C
	INTEGER MXPOLY			!MAX. POLYNOMIAL DEGREE
		PARAMETER (MXPOLY=10)
C
C  Arguments:
C
C
C  Function references:
C
	INTEGER WNMEJC			!CEIL
	INTEGER WNMEJF			!FLOOR
	LOGICAL WNDPAR			!GET USER DATA
	LOGICAL WNDSTA			!ASK SETS
	LOGICAL WNDLNF,WNDLNG,WNDLNK	!LINK DATA SET
	LOGICAL WNFRD			!READ DISK
	LOGICAL WNFWR			!WRITE DISK
	LOGICAL WNGGVA			!GET MEMORY
	INTEGER WNFEOF			!EOF POINTER
	LOGICAL WNMLGA			!GET LSQ AREA
	LOGICAL WNMLTN			!MATRIX INVERSION
	CHARACTER*32 WNTTSG		!SHOW SET
	LOGICAL NSCHAS			!HET HA RANGE
	LOGICAL NGCSTG			!GET A PLOT
C
C  Data declarations:
C
	INTEGER NGFP			!PLOT HEADER PTR
	INTEGER MAREA			!LSQ AREA
	INTEGER SNAM(0:7)		!PLOT NAME
	INTEGER NPLOT			!# OF PLOTS
	INTEGER PLOTS(MXNPLT)		!PLOT LIST
	INTEGER BUFAD			!DATA BUFFER ADDRESS
	INTEGER BLEN			!DATA BUFFER LENGTH
	INTEGER POLYN			!POLYNOMIAL DEGREE
	LOGICAL ILTEST			!INDEFINITE LOOP TEST
	LOGICAL CPOLY			!TEST ON FIT/RESIDUAL
	INTEGER DOPOLY(0:MXPOLY)	!COEF's to FIT
	REAL SOLP(0:MXPOLY)
	COMPLEX CSOL,CRMS		!SOLUTION
	REAL MU
	COMPLEX CCSOL(0:MXPOLY),CCRMS(0:MXPOLY)
	COMPLEX C2SOL(0:MXPOLY)		!SOLUTION FOR HA in hours
	COMPLEX CP(0:MXPOLY)
	REAL HARA(2)			!HA RANGE
	REAL HAST			!HA SMOOTHING WIDTH
	REAL CBL			!CURRENT BASELINE
	REAL SHFT(0:1)			!SHIFT VALUES
	REAL UV0(0:1)			!U,V VALUES
	BYTE NGF(0:NGFHDL-1)		!PLOT HEADER
	  INTEGER*2 NGFI(0:NGFHDL/2-1)
	  INTEGER NGFJ(0:NGFHDL/4-1)
	  REAL NGFE(0:NGFHDL/4-1)
	  EQUIVALENCE (NGF,NGFI,NGFJ,NGFE)
	REAL R2,R3,R4
	COMPLEX C2,C3
	CHARACTER*40 TX40
C-
C
C GET CALCULATION TYPE
C
 10	CONTINUE
	IF (.NOT.WNDPAR('CALC_TYPE',OPTION,LEN(OPTION),J0,'QUIT')) THEN
	  IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 900
	  GOTO 10
	END IF
	IF (J0.LE.0) OPT='QUIT'
	IF (OPT.EQ.'QUI') GOTO 900
C
C GET SOLUTION VALUES
C
 12	CONTINUE
	IF (OPT.EQ.'SMO') THEN
	  IF (.NOT.WNDPAR('HA_WIDTH',HAST,LB_E,J0,'.25')) THEN !SMOOTH WIDTH
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 10
	    GOTO 12
	  END IF
	  IF (J0.EQ.0) GOTO 10
	  IF (J0.LT.0) HAST=0.25
	  HAST=HAST/360.				!CIRCLES
	ELSE IF (OPT.EQ.'POL'.OR.OPT.EQ.'CPO') THEN
	  CPOLY=(OPT.EQ.'CPO')				!OUTPUT RESIDUALS/FIT
	  IF (.NOT.WNDPAR('POLY_N',POLYN,LB_J,J0,'3')) THEN !POLYNOMIAL DEGREE
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 10
	    GOTO 12
	  END IF
	  IF (J0.EQ.0) GOTO 10
	  IF (J0.LT.0) POLYN=3
	  POLYN=MIN(MXPOLY,POLYN)			!DEGREE	  
C
	  DO I=0,POLYN
	    DOPOLY(I)=I
	  END DO
	  IF (.NOT.WNDPAR('POLY_USE',DOPOLY,LB_J*(MXPOLY+1),
	1		J0,A_B(-A_OB),DOPOLY,POLYN+1)) THEN !COEFF. TO USE
	     IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 10
	     GOTO 12
	  END IF
	  IF (J0.LE.0) THEN				!DEFAULT: ALL
	     DO I=0,POLYN
	       DOPOLY(I)=I
	     END DO
	  END IF	        
	  CALL WNCTXT(F_TP,'Fitting !UJ: !1$#UJ',
	1	   POLYN,POLYN+1,DOPOLY)
C
	ELSE IF (OPT.EQ.'DPO') THEN
	  CPOLY=.FALSE.					!OUTPUT RESIDUALS
	  IF (.NOT.WNDPAR('POLY_COEF',SOLP,LB_E*(MXPOLY+1),
	1		POLYN,'""')) THEN		!COEFF.
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 10
	    GOTO 12
	  END IF
	  IF (POLYN.LE.0) GOTO 12
	  POLYN=MIN(MXPOLY,POLYN-1)			!DEGREE
	ELSE IF (OPT.EQ.'NUL') THEN
	  IF (.NOT.NSCHAS(1,HARA)) GOTO 10		!GET HA RANGE
	ELSE IF (OPT.EQ.'SHI') THEN
 13	  CONTINUE
	  IF (.NOT.WNDPAR('SHIFT',SHFT,2*LB_E,J0,'0.,0.')) THEN !SHIFTS
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 10
	    GOTO 12
	  END IF
	  IF (J0.EQ.0) GOTO 10
	  IF (J0.LT.0) GOTO 13			!MUST SPECIFY
	ELSE IF (OPT.EQ.'AVE') THEN
	ELSE
	  CALL WNCTXT(F_TP,'Unknown calculation option')
	  GOTO 900
	END IF
C
C GET PLOTS
C
 11	CONTINUE
	IF (.NOT.WNDSTA('NGF_SETS',MXNPLT,SETS(0,0),FCAOUT)) GOTO 10 !GET PLOTS TO DO
	ILTEST=.TRUE.				!INDEFINITE LOOP TEST
C
C DO FOR ALL PLOTS
C
 20	CONTINUE
	IF (.NOT.NGCSTG(FCAOUT,SETS,NGF,NGFP,SNAM)) GOTO 10 !READY
	CALL WNDSTI(FCAOUT,SNAM)		!MAKE PROPER NAME
 21 	CONTINUE
	IF (SNAM(0).GE.SGNR(0)) THEN		!CAN BE INFINITE LOOP
	  IF (.NOT.ILTEST) GOTO 10		!INFINITE LOOP
	  IF (.NOT.WNDLNG(GFH_LINKG_1,0,SGH_GROUPN_1,FCAOUT,
	1		SGPH(0),SGNR(0))) THEN	!CREATE JOB SET
	    CALL WNCTXT(F_TP,'!/Cannot create sub-group')
	    GOTO 10				!REPEAT
	  END IF
	  ILTEST=.FALSE.			!CAN BE LOOP
	  GOTO 21
	END IF
C
C ACTION
C
 30	CONTINUE
	BLEN=2*LB_X*NGFJ(NGF_SCN_J)		!LENGTH DATA BUFFER
	IF (.NOT.WNGGVA(BLEN,BUFAD)) THEN
	  CALL WNCTXT(F_TP,'!/Cannot get file databuffer')
	  GOTO 900
	END IF
	BUFAD=(BUFAD-A_OB)/LB_X
	IF (.NOT.WNFRD(FCAOUT,LB_X*NGFJ(NGF_SCN_J),A_X(BUFAD),
	1		NGFJ(NGF_DPT_J))) THEN	!READ DATA
 31	  CONTINUE
	  CALL WNCTXT(F_TP,'!/Error reading/writing plot data')
 32	  CONTINUE
	  CALL WNGFVA(BLEN,BUFAD*LB_X+A_OB)	!FREE BUFFER
	  GOTO 10
	END IF
C
C AVERAGE
C
	IF (OPT.EQ.'AVE') THEN
	  IF (.NOT.WNMLGA(MAREA,LSQ_T_COMPLEX,1)) THEN
 33	    CONTINUE
	    CALL WNCTXT(F_TP,'!/No memory for solution')
	    GOTO 32
	  END IF
	  I1=0					!COUNT
	  DO I=0,NGFJ(NGF_SCN_J)-1		!AVERAGE
	    IF (REAL(A_X(BUFAD+I)).NE.NGCDLC) THEN
	      CALL WNMLMN(MAREA,LSQ_C_REAL,CMPLX(1.,1.),1.,
	1			A_X(BUFAD+I))	!USE POINT
	      I1=I1+1				!COUNT
	    END IF
	  END DO
	  IF (WNMLTN(MAREA)) THEN		!INVERT
	    CALL WNMLSN(MAREA,CSOL,MU,R0)	!SOLVE
	    CALL WNMLME(MAREA,CRMS)		!ERRORS
	    CALL WNCTXT(F_TP,'!/Average for plot !AS: '//
	1		'!24$EC5(!24$EC5)'//
	1		' for !5$UJ points',
	1		WNTTSG(SNAM,0),CSOL,CRMS,I1)
	  END IF
	  CALL WNMLFA(MAREA)			!FREE AREA
C
C SMOOTH
C
	ELSE IF (OPT.EQ.'SMO') THEN
	  NGFE(NGF_MAX_E)=-1E20			!INIT MAX/MIN
	  NGFE(NGF_MIN_E)=1E20
	  NGFJ(NGF_DEL_J)=0			!COUNT DELETED POINTS
	  DO I=0,NGFJ(NGF_SCN_J)-1		!ALL POINTS
	    R0=NGFE(NGF_HAB_E)+I*NGFE(NGF_HAI_E) !CURRENT HA
	    R1=0.				!WEIGHT
	    C2=0.				!SUM
	    DO I1=WNMEJC(-HAST/NGFE(NGF_HAI_E))+I,
	1		WNMEJF(HAST/NGFE(NGF_HAI_E))+I !SMOOTH
	      IF (I1.GE.0 .AND. I1.LT.NGFJ(NGF_SCN_J)) THEN !PRESENT
		IF (REAL(A_X(BUFAD+I1)).NE.NGCDLC) THEN
		    R4=1.-ABS(I-I1)*NGFE(NGF_HAI_E)/HAST !WEIGHT
		    R1=R1+R4
		    C2=C2+R4*A_X(BUFAD+I1)	!SUM
		END IF
	      END IF
	    END DO
	    IF (R1.NE.0) THEN
		C3=C2/R1			!SMOOTHED VALUE
		NGFE(NGF_MAX_E)=MAX(NGFE(NGF_MAX_E),ABS(C3)) !NEW MAX/MIN
		NGFE(NGF_MIN_E)=MIN(NGFE(NGF_MIN_E),ABS(C3))
	    ELSE
		C3=CMPLX(NGCDLC,NGCDLC)		!DELETED
	        NGFJ(NGF_DEL_J)=NGFJ(NGF_DEL_J)+1 !COUNT
	    END IF
	    A_X(BUFAD+I+NGFJ(NGF_SCN_J))=C3	!SAVE VALUE
	  END DO
	  CALL WNDSTI(FCAOUT,SNAM)		!MAKE PROPER NAME
	  CALL WNCTXS(TX40,'SMOOTH #!AS IN !E9.2 DEG',
	1		WNTTSG(SNAM,0),360.*HAST) !DATA TYPE
	  CALL WNGMFS(NGF_TYP_N,TX40,NGF(NGF_TYP_1))
 50	  CONTINUE
	  IF (.NOT.WNDLNF(SGPH(0)+SGH_LINKG_1,SNAM(1),
	1		SGH_GROUPN_1,FCAOUT,SGPH(1),SGNR(1))) THEN !LINK
 51	    CONTINUE
	    CALL WNCTXT(F_TP,'!/Cannot link sub-group')
	    GOTO 32
	  END IF
	  IF (.NOT.WNDLNF(SGPH(1)+SGH_LINKG_1,SNAM(2),
	1		SGH_GROUPN_1,FCAOUT,SGPH(2),SGNR(2))) GOTO 51
	  IF (.NOT.WNDLNF(SGPH(2)+SGH_LINKG_1,SNAM(3),
	1		SGH_GROUPN_1,FCAOUT,SGPH(3),SGNR(3))) GOTO 51
	  IF (.NOT.WNDLNF(SGPH(3)+SGH_LINKG_1,SNAM(4),
	1		SGH_GROUPN_1,FCAOUT,SGPH(4),SGNR(4))) GOTO 51
	  J=WNFEOF(FCAOUT)			!OUTPUT POINTER
	  NGFJ(NGF_DPT_J)=J+NGFHDL		!DATA POINTER
	  IF (.NOT.WNFWR(FCAOUT,NGFHDL,NGF,J)) GOTO 31 !WRITE HEADER
	  IF (.NOT.WNFWR(FCAOUT,LB_X*NGFJ(NGF_SCN_J),
	1		A_X(BUFAD+NGFJ(NGF_SCN_J)),J+NGFHDL)) GOTO 31 !DATA
	  IF (.NOT.WNDLNK(GFH_LINK_1,J,
	1		NGF_SETN_1,FCAOUT)) GOTO 51 !LINK DATA
	  IF (.NOT.WNDLNG(SGPH(4)+SGH_LINKG_1,J,
	1		SGH_GROUPN_1,FCAOUT,SGPH(5),SGNR(5))) GOTO 51 !INDEX
	  CALL NGCSPH(SGNR,NGF)			!SHOW NEW PLOT
C
C POLYNOMIAL
C
	ELSE IF (OPT.EQ.'POL'.OR.OPT.EQ.'CPO') THEN
	  IF (.NOT.WNMLGA(MAREA,LSQ_T_COMPLEX,POLYN+1)) GOTO 33 !GET SOL. AREA
	  DO I=0,NGFJ(NGF_SCN_J)-1		!AVERAGE
	    R1=NGFE(NGF_HAB_E)+I*NGFE(NGF_HAI_E) !HA (CIRCLES)
	    DO I1=0,POLYN			!COEFFICIENTS
	      CP(I1)=R1**DOPOLY(I1)
	    END DO
	    IF (REAL(A_X(BUFAD+I)).NE.NGCDLC) THEN
	      IF (NGFJ(NGF_TRTYP_J).EQ.2) THEN
	         CALL WNCTXT(F_T,'Using !7$E6.1 m baseline',R1*360.*10.)
	      END IF
	      CALL WNMLMN(MAREA,LSQ_C_COMPLEX,CP,1.,A_X(BUFAD+I)) !USE POINT
	    END IF
	  END DO
	  IF (WNMLTN(MAREA)) THEN		!INVERT
	    CALL WNMLSN(MAREA,CCSOL,MU,R0)	!SOLVE
	    CALL WNMLME(MAREA,CCRMS)		!ERRORS
	    CALL WNCTXT(F_TP,'!/Polynomial coefficients for !AS:!/'//
	1		'      !24$#EC5!/m.e.  !24$#EC5',
	1			WNTTSG(SNAM,0),POLYN+1,CCSOL,
	1			POLYN+1,CCRMS)
	    CALL WNCTXT(F_TP,' (in W.U. as function of HA in circles)')
	    IF (NGFJ(NGF_TRTYP_J).EQ.0) THEN
	      DO I1=0,POLYN			!CALCULATE PER HOUR
	         R0=(1./24.)**DOPOLY(I1)
	         C2SOL(I1)=CCSOL(I1)*R0
	      END DO
	      CALL WNCTXT(F_TP,'      !24$#EC5',POLYN+1,C2SOL)
	      CALL WNCTXT(F_TP,' (in W.U. as function of HA in hours)')
	    ELSE IF (NGFJ(NGF_TRTYP_J).EQ.2) THEN
	      DO I1=0,POLYN			!CALCULATE PER METER
	         R0=(1./360./10.)**DOPOLY(I1)
	         C2SOL(I1)=CCSOL(I1)*R0
	      END DO
	      CALL WNCTXT(F_TP,'HA: !9$EAF9.4  !24$#EC5',
	1	      NGFE(NGF_TRHA_E),POLYN+1,C2SOL)
	      CALL WNCTXT(F_TP,' (in W.U. as function of BASELINE in M)')
	    END IF
	  ELSE
	    CALL WNCTXT(F_TP,'!/Cannot solve polynomial for !AS',
	1		WNTTSG(SNAM,0))
	  END IF
	  CALL WNMLFA(MAREA)			!FREE AREA
 40	  CONTINUE
	  NGFE(NGF_MAX_E)=-1E20			!INIT MAX/MIN
	  NGFE(NGF_MIN_E)=1E20
	  NGFJ(NGF_DEL_J)=0			!COUNT DELETED POINTS
	  DO I=0,NGFJ(NGF_SCN_J)-1		!ALL POINTS
	    R0=NGFE(NGF_HAB_E)+I*NGFE(NGF_HAI_E) !CURRENT HA
	    C2=0
	    DO I1=0,POLYN
	      C2=C2+CCSOL(I1)*(R0**DOPOLY(I1))
	    END DO
C	    DO I1=POLYN,0,-1			!CALCULATE FIT
C	       C2=C2*R0+CCSOL(I1)
C	    END DO
	    IF (.NOT.CPOLY) THEN		!CALCULATE RESIDUAL
	      IF (REAL(A_X(BUFAD+I)).NE.NGCDLC) THEN
	         C2=A_X(BUFAD+I)-C2		!RESIDUAL
	      ELSE
	         C2=CMPLX(NGCDLC,NGCDLC)		!DELETED
	      END IF
	    END IF
            IF (REAL(C2).NE.NGCDLC) THEN
	      NGFE(NGF_MAX_E)=MAX(NGFE(NGF_MAX_E),ABS(C2)) !NEW MAX/MIN
	      NGFE(NGF_MIN_E)=MIN(NGFE(NGF_MIN_E),ABS(C2))
	    ELSE
	      NGFJ(NGF_DEL_J)=NGFJ(NGF_DEL_J)+1 !COUNT
	    END IF
	    A_X(BUFAD+I+NGFJ(NGF_SCN_J))=C2	!SAVE VALUE
	  END DO
	  CALL WNDSTI(FCAOUT,SNAM)		!MAKE PROPER NAME
	  CALL WNCTXS(TX40,'DPOLY #!AS, !UJ DEGREE',
	1		WNTTSG(SNAM,0),POLYN)	!DATA TYPE
	  CALL WNGMFS(NGF_TYP_N,TX40,NGF(NGF_TYP_1))
	  GOTO 50
C
C DPOLY
C
	ELSE IF (OPT.EQ.'DPO') THEN
	  DO I=0,POLYN				!SET COEFFICIENTS
	    CCSOL(I)=SOLP(I)
	    DOPOLY(I)=I
	  END DO
	  GOTO 40
C
C NULL
C
	ELSE IF (OPT.EQ.'NUL') THEN
	  NGFE(NGF_MAX_E)=-1E20			!INIT MAX/MIN
	  NGFE(NGF_MIN_E)=1E20
	  NGFJ(NGF_DEL_J)=0			!COUNT DELETED POINTS
	  I1=NINT((HARA(1)-NGFE(NGF_HAB_E))/
	1		NGFE(NGF_HAI_E))	!START DELETE
	  I2=NINT((HARA(2)-NGFE(NGF_HAB_E))/
	1		NGFE(NGF_HAI_E))	!END DELETE
	  DO I=0,NGFJ(NGF_SCN_J)-1		!ALL POINTS
	    IF (REAL(A_X(BUFAD+I)).NE.NGCDLC) THEN !IF NOT DELETED
	      IF (I.GE.I1 .AND. I.LE.I2) THEN	!DELETE POINT
		C2=CMPLX(NGCDLC,NGCDLC)
		NGFJ(NGF_DEL_J)=NGFJ(NGF_DEL_J)+1 !COUNT
	      ELSE				!LEAVE POINT
		C2=A_X(BUFAD+I)
	        NGFE(NGF_MAX_E)=MAX(NGFE(NGF_MAX_E),ABS(C2)) !NEW MAX/MIN
	        NGFE(NGF_MIN_E)=MIN(NGFE(NGF_MIN_E),ABS(C2))
	      END IF
	    ELSE
	      C2=A_X(BUFAD+I)			!DELETED
	      NGFJ(NGF_DEL_J)=NGFJ(NGF_DEL_J)+1 !COUNT
	    END IF
	    A_X(BUFAD+I+NGFJ(NGF_SCN_J))=C2	!SAVE VALUE
	  END DO
	  CALL WNDSTI(FCAOUT,SNAM)		!MAKE PROPER NAME
	  CALL WNCTXS(TX40,'NULL #!AS (!2EAF9.2) DEGREES',
	1		WNTTSG(SNAM,0),HARA)	!DATA TYPE
	  CALL WNGMFS(NGF_TYP_N,TX40,NGF(NGF_TYP_1))
	  GOTO 50
C
C SHIFT
C
	ELSE IF (OPT.EQ.'SHI') THEN
	  DO I=0,NGFJ(NGF_SCN_J)-1		!ALL POINTS
	    CBL=NGFE(NGF_BLN_E)
	    R0=NGFE(NGF_HAB_E)+I*NGFE(NGF_HAI_E) !CURRENT HA
	    IF (NGFJ(NGF_TRTYP_J).EQ.0) THEN	!NORMAL
	      R1=NGFE(NGF_FRQ_E)		!CURRENT FREQUENCY
	    ELSE IF (NGFJ(NGF_TRTYP_J).EQ.2) THEN !BASELINE
	      CBL=R0*360.			!CURRENT BASELINE
	      R1=NGFE(NGF_FRQ_E)		!CURRENT FREQUENCY
	      R0=NGFE(NGF_TRHA_E)		!CURRENT HA
	    ELSE				!TRANSPOSE
	      R1=NGFE(NGF_TRFB_E)+R0/FRHACV*NGFE(NGF_TRFI_E) !CURRENT FREQ.
	      R0=NGFE(NGF_TRHA_E)		!CURRENT HA
	    END IF
	    UV0(0)=PI2*R1/CL/1E-6*COS(PI2*R0)	!U
	    UV0(1)=-PI2*R1/CL/1E-6*SIN(PI2*R0)*SIN(PI2*NGFE(NGF_DEC_E))
	    DO I1=0,1
	      UV0(I1)=1/(3600.*DEG)*SHFT(I1)*UV0(I1)*CBL !SHIFT PHASE
	    END DO
	    C2=A_X(BUFAD+I)			!VALUE
	    IF (REAL(A_X(BUFAD+I)).NE.NGCDLC) THEN !NOT DELETED
	      C2=C2*EXP(CMPLX(0.,-UV0(0)-UV0(1))) !NEW VALUE
	    END IF
	    A_X(BUFAD+I+NGFJ(NGF_SCN_J))=C2	!SAVE VALUE
	  END DO
	  CALL WNDSTI(FCAOUT,SNAM)		!MAKE PROPER NAME
	  CALL WNCTXS(TX40,'SHIFT #!AS (!2E9.2) ARCSEC',
	1		WNTTSG(SNAM,0),SHFT)	!DATA TYPE
	  CALL WNGMFS(NGF_TYP_N,TX40,NGF(NGF_TYP_1))
	  GOTO 50
	END IF
C
	CALL WNGFVA(BLEN,BUFAD*LB_X+A_OB)	!FREE BUFFER
	GOTO 20					!NEXT PLOT
C
C READY
C
 900	CONTINUE
	RETURN
C
C
	END