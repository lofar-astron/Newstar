C+ NFIUVL.FOR
C  WNB 940729
C
C  Revisions:
C	WNB 940812	Change QSR call
C	WNB 940825	Test IFR errors
C       WNB 950630	Create proper UVLIN
C
	SUBROUTINE NFIUVL
C
C  UVLIN calculation
C
C  Result:
C
C	CALL NFIUVL	try UVLIN
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'CBITS_DEF'
	INCLUDE 'LSQ_O_DEF'
	INCLUDE 'STH_O_DEF'
	INCLUDE 'NFI_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
C
C  Function references:
C
	LOGICAL WNMLGA			!GET LSQ AREA
	LOGICAL NSCQOP		        !PRODUCE A DATA QUBE
	LOGICAL NSCQFN			!GET NEXT FIELD
	LOGICAL NSCQSR			!READ SCAN DATA
	LOGICAL NSCQWA			!WRITE ADDITIVE IFR ERRORS
	LOGICAL NMOMSL			!CALC MODEL IN SCN FILE
C
C  Data declarations:
C-
	INTEGER INFO(QINFO__L:QINFO__H) !QUBE MAX. INFORMATION
	INTEGER FINFO(QINFO__L:QINFO__H) !QUBE FIELD INFO
	INTEGER PINFO(QINFO__L:QINFO__H) !QUBE FIELD TABLE POINTERS
	INTEGER PWGT,PDAT,PMOD,POUT	!WEIGHT, DATA, MODEL, ERROR BUFFER PTRS
	BYTE STH(0:STH__L-1)		!SET HEADER
	INTEGER TW,TE			!TELESCOPES	
	INTEGER LAR			!LEAST SQUARES AREA
	INTEGER TPOL(0:3)		!POL CHECK BITS
	  DATA TPOL/XX_P,XY_P,YX_P,YY_P/
	REAL EQ(0:MXPOLY)		!CONDITION EQUATIONS
	REAL SOL(0:(MXPOLY+1)*2*4-1)	!SOLUTION (POLY,C/S,POL)
	REAL MU(0:2*4-1),SD(0:2*4-1)	!ERROR,S.D. (C/S,POL)
	REAL R2
C
C INIT
C
	IF (NSRC(0).GT.0) THEN
	   IF (.NOT.NMOMSL(FCAOUT,SETS,LPOFF)) GOTO 800 !MAKE SCAN MODEL
	END IF
C       
C PRODUCE QUBE
C       
	IF (.NOT.NSCQOP(QUAIN,FCAOUT,SETS,LPOFF,INFO)) GOTO 800
	CALL WNCTXT(F_TP,'Working on !UJ cube(s), with max dimensions:',
	1    INFO(QINFO_FLD))
	CALL WNCTXT(F_TP,'           !UJx!UJx!UJ (f,ha,ifr)',
	1    INFO(QINFO_F),INFO(QINFO_T),INFO(QINFO_I))
C
C PREPARE SOLUTION
C
	IF (.NOT.WNMLGA(LAR,LSQ_T_REAL+LSQ_T_MULTIPLE,NPOLY+1,8)) THEN
	   CALL WNCTXT(F_TP,'No memory for solution')
	   GOTO 800
	END IF
C
C LOOP OVER FIELDS AND READ TIF
C
	DO WHILE (NSCQFN(QUAIN,FCAOUT,QUB_TIF+QUB_M+QUB_OUT,
	1    STH,FINFO,PINFO))
	   CALL WNCTXT(F_TP,'Working on cube !UJ, with dimensions:',
	1	FINFO(QINFO_FLD))
	   CALL WNCTXT(F_TP,'           !UJx!UJx!UJ (f,ha,ifr)',
	1	FINFO(QINFO_F),FINFO(QINFO_T),FINFO(QINFO_I))
	   CALL WNCTXT(F_TP,'!_Frequencies:')
	   CALL WNCTXT(F_TP,'!72$8Q!#D12.6',
	1	FINFO(QINFO_F),A_D(PINFO(QINFO_F)))
C
C LOOP OVER ALL HOUR ANGLES AND INTERFEROMETERS
C
	   DO I=0,FINFO(QINFO_T)-1 !ALL HOUR ANGLES
	      IF (A_E(PINFO(QINFO_T)+I).LT.HARAN(0) .OR.
	1	   A_E(PINFO(QINFO_T)+I).GT.HARAN(1)) GOTO 20 !FORGET
	      DO I1=0,FINFO(QINFO_I)-1 		!ALL INTERFEROMETERS
		 TW=MOD(A_I(PINFO(QINFO_I)+I1),256) !WEST TELESCOPE
		 TE=A_I(PINFO(QINFO_I)+I1)/256 	!EAST TELESCOPE
		 IF (.NOT.SIFRS(TW,TE)) GOTO 30 !FORGET
		 IF (NSCQSR(QUAIN,FCAOUT,I,I1,
	1	      CORAP,CORDAP,PWGT,PDAT,PMOD,POUT)) THEN !GET FREQ. LINE
		    DO I2=0,4*FINFO(QINFO_F)-1 	!CORRECT FOR MODEL
		       A_X(PDAT+I2)=A_X(PDAT+I2)-A_X(PMOD+I2)
		    END DO
		    CALL WNMLIA(LAR,LSQ_I_ALL) 	!INIT LSQ
		    EQ(0)=1
C
C LOOP OVER FREQUENCIES
C
		    DO I2=0,FINFO(QINFO_F)-1 	!MAKE EQUATIONS
		       R0=A_D(PINFO(QINFO_F)+I2) !FREQUENCY
		       DO I3=0,NSEL-1		!SELECTED?
			  IF (R0.GE.FSEL(0,I3) .AND. R0.LE.FSEL(1,I3)) THEN
			     R0=R0/A_D(PINFO(QINFO_F)) !FREQUENCY NORMALISED
			     R1=R0
			     DO I4=1,NPOLY
				EQ(I4)=R1
				R1=R0*R1
			     END DO
			     CALL WNMLMN(LAR,LSQ_C_REAL,1.,EQ,
	1			  A_X(PDAT+4*I2))
			     GOTO 10
			  END IF
		       END DO
 10		       CONTINUE
		    END DO
		    CALL WNMLTR(LAR,I0) 	!SOLVE
		    CALL WNMLSN(LAR,SOL,MU,SD)
C
C SAVE CORRECTIONS
C
		    DO I2=0,FINFO(QINFO_F)-1	!FREQUENCIES
		       R0=A_D(PINFO(QINFO_F)+I2) !FREQUENCY
		       R0=R0/A_D(PINFO(QINFO_F)) !FREQUENCY NORMALISED
		       DO I3=0,3		!POL
			  IF (IAND(SPOL,TPOL(I3)).NE.0) THEN !SELECTED
			     R1=SOL((NPOLY+1)*2*I3+NPOLY) !COS TERM
			     R2=SOL((NPOLY+1)*2*I3+NPOLY+1) !SIN TERM
			     DO I4=NPOLY-1,0,-1
				R1=R1*R0+SOL((NPOLY+1)*2*I3+I4)
				R2=R2*R0+SOL((NPOLY+1)*2*I3+I4+1)
			     END DO
			     A_X(POUT+4*I2+I3)=CMPLX(R1,R2)
			  ELSE
			     A_X(POUT+4*I2+I3)=0 !CLEAR ERROR
			  END IF
		       END DO
		    END DO
		    IF (.NOT.NSCQWA(QUAIN,FCAOUT,I,I1,CORAP,CORDAP)) THEN
		       CALL WNCTXT(F_TP,'Error writing interferometer'//
	1		    'errors for HA, ifr = !8$EAF12.2, !XI',
	1		    A_E(PINFO(QINFO_T)+I),
	1		    A_I(PINFO(QINFO_I)+I1))
		    END IF
		 END IF
C
C NEXT INTERFEROMETER
C
 30		 CONTINUE
	      END DO				!INTERFEROMETERS
C
C NEXT HOUR ANGLE
C
 20	      CONTINUE
	   END DO				!HOUR ANGLES
C
C NEXT FIELD
C
	END DO					!FIELD
C
C READY
C
 800	CONTINUE
	CALL WNMLFA(LAR)			!FREE LSQ AREA
	CALL NSCQCL(QUAIN,FCAOUT,SETS) 		!REMOVE QUBE INFO
C
	RETURN
C
C
	END
