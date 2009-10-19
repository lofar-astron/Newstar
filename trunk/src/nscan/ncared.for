C+ NCARED.FOR                                   
C  WNB 900306                                   
C                                               
C  Revisions:                                   
C       WNB 910613      Average data per channel
C       WNB 910808      Change call to NCARMD   
C       WNB 910812      Add ALIGN               
C       WNB 910816      Add complex             
C       WNB 910820      Add Histograms          
C       WNB 910930      Check funny continuity  
C       WNB 910930      Check extreme values    
C       GvD 920501      JS is now logical iso. integer (use SWDIS now as well)
C       WNB 921104      Cater for full HA range 
C       WNB 921120      Include scan integration
C       WNB 930504      Proper complex solutions
C       HjV 930518      Change some text        
C       WNB 930708      Allow more (8 i.s.o. 3) complex iterations; mesage
C       WNB 930825      Add dipole position     
C       WNB 930826      Use new model data calculation; new redundant
C       WNB 931008      Add MINST               
C       WNB 931123      Use RIN for complex loop determination
C       WNB 931126      Add XOSOL test          
C       CMV 940215      Add tags in printout for reduction group batch files
C       CMV 940301      Extra space between HA and X/Y
C       CMV 940331      Pass (dummy) telescopes array to NSCSWU
C       JPH 940912      Make graphics print depend on SHLV (SHOW_LEVEL 
C                        parameter). Make level 2 the highest.
C       JPH 940927      Automatically flag header of bad scan
C       JPH 941129      Undo 940927 - this should be done through NFLAG
C       JPH 950111      Correct scan number (I --> I2) in reading model scans
C       JPH 950126      Restore printing to condition prior to 940912
C       HjV 950609      Add MIFR part           
C       WNB 950613      Change to new LSQ routines 
C       WNB 950614      New non-linear routines; change max loop
C       WNB 950628      Better logic MIFR errors
C       WNB 950629      Correct logics for MIFR write missing interferometers
C       JPH 950926      Iteration count         !UJ i.s.o.
C       JPH 960528      Alphabetise declarations. - Meaningful names for loop
C                        variables. - CBITS_DEF, CAP_<xxx>             
C                       Compress output of gain/phases and also show on 
C                        terminal. - All HA printout in .01 deg (most were .1).
C                        - Make print headings/leaders more informative.
C	JPH 960607	Merge HjV: Include autocorrelations in MIFR save
C	JPH 960802	FL_WGT flag: Reduce data weight
C	JPH 960812	No histogram output for SHLVL < 2
C	JPH 961023	CSTNAM=-1 at entry
C			Comment out FL_WGT code
C	JPH 961212	NSCSCF --> NSCSCR (F does not set weights!)
C			Skip solution if No data in scan 
C	WNB 980701	New MIFR calculations
C	WNB 000725	Add variability calculation options polarisation
C       WNB 070814      CHANGE MIFR
C       WNB 091019      Back to pre-070814
C
C                                               
	SUBROUTINE NCARED                       
C                                               
C  Calculate redundancy solution                
C                                               
C  Result:                                      
C                                               
C       CALL NCARED     will calculate the redundancy solution(s)
C                                               
C  Include files:                               
C                                               
	INCLUDE 'WNG_DEF'
	INCLUDE 'CBITS_DEF'                       
	INCLUDE 'NCA_DEF'                       
	INCLUDE 'LSQ_O_DEF'                     
	INCLUDE 'STH_O_DEF'                     !sector HEADER
	INCLUDE 'SCH_O_DEF'                     !SCAN HEADER
C                                               
C  Parameters:                                  
C                                               
	INTEGER MXITCT                          !DEFAULT COMPLEX ITERATION COUNT
	  PARAMETER (MXITCT=50)                 
	INTEGER         X, Y, XX, XY, YX, YY, G, P
	PARAMETER       (X=0,Y=1, XX=0,XY=1,YX=2,YY=3, G=0,P=1)
						! loop indices
C                                               
C  Arguments:                                   
C                                               
C                                               
C  Function references:                         
C                                               
	LOGICAL WNFRD                           !READ DISK
	LOGICAL WNFWR                           !WRITE DATA
	CHARACTER*32 WNTTSG                     !MAKE SUB-GROUP NAME
	REAL WNGENR                             !NORMALISE ANGLE
	LOGICAL WNMLGA                          !GET LSQ AREA
	LOGICAL NSCSTL                          !GET A sector
	LOGICAL NSCSIF                          !READ IFR TABLE
	LOGICAL NSCSCR,NSCSCF                   !READ DATA FROM SCAN
	LOGICAL NSCSWU                          !WRITE CORRECTION RESULTS
	LOGICAL NCARMD                          !GET DATA IN CORRECT FORMAT
	LOGICAL NCARGS,NCARG2                   !GAIN SOLUTION
	LOGICAL NCARPS,NCARP2                   !PHASE SOLUTION
	LOGICAL NCASGS,NCASPS                   !SELFCAL SOLUTION
	LOGICAL NCAAGS,NCAAPS                   !ALIGN SOLUTION
	LOGICAL NCARCS,NCASCS,NCAACS            !COMPLEX SOLUTION
	LOGICAL NMOMSL                          !MODEL CALCULATION IN SCAN
	LOGICAL NMORDH                          !GET MODEL HEADER DATA
C                                               
C  Data declarations:                           
C                                               
	INTEGER ICS, IFR, IGP, ISCN, ISCN1, ITL, ITL1
C                                               
	REAL ANG(0:2,0:STHIFR-1)                
	REAL ARMS(0:2,0:1)                      !AVER. AMPL. XX,YY
	REAL CSRMS(0:2,0:3,0:2)			!AVER. XX,..., A,C,S
	REAL AWGT(0:STHIFR-1,0:1)               !AMPLITUDE WEIGHTED WEIGHTS X,Y
	REAL BASEL(0:STHIFR-1)                  !BASELINE TABLE
	BYTE BTMP                               
	COMPLEX CAMOD(0:STHIFR-1,0:3)           !SOURCE MODEL XYX
	COMPLEX CDAT(0:STHIFR-1,0:1)            !DATA COMPLEX X,Y
	REAL CEQ(0:STHTEL-1,0:STHTEL-1,0:1,0:1) !CONSTRAINT EQUATIONS G,P X,Y
	COMPLEX CMOD(0:3,0:STHIFR-1)            !SOURCE MODEL I,Q,U,V
	REAL CSOL(0:STHTEL-1,0:1,0:1)           !CONTINUITY SOLUTION G,P X,Y
	INTEGER CSTNAM(0:7)                     !CHECK sector NAME
	  DATA CSTNAM/8*-1/                     
	REAL DAMP(0:STHIFR-1,0:1)               ! DATA AMPL. X,Y
	REAL DAT(0:1,0:STHIFR-1,0:3)            ! DATA XX,XY,YX,YY
	REAL DAT1(0:1,0:STHIFR-1,0:3)           ! DATA XX,XY,YX,YY
	INTEGER FLW(0:STHIFR-1,0:3)         	! flags XX,XY,YX,YY
	REAL DPHAS(0:STHIFR-1,0:1)              ! DATA PHASE X,Y
	REAL DWGT(0:STHIFR-1,0:1)               ! DATA WEIGHT X,Y
	INTEGER DAV0(0:STHIFR-1)                ! scaled copy for printing
	INTEGER EAV0(0:STHIFR-1)                ! scaled copy for printing 
	LOGICAL FLGERR(0:3,0:1)                 ! ERRORS G,P,C X,Y
	DOUBLE PRECISION FRQ0                   ! BASIC FREQUENCY
	CHARACTER*8 GPTXT(0:1)                  ! GAIN/PHASE
	  DATA GPTXT/'gain','phase'/            
	REAL HA                                 !HA OF SCAN
	INTEGER HISBAD(0:2,0:1)                 !HISTOGRAMS X,Y
	INTEGER IFRA(0:1,0:STHIFR-1)            
	INTEGER*2 IFRT(0:STHIFR-1)              !INTERFEROMETER TABLE
	INTEGER IXY                             !POL. COUNT
	CHARACTER*1 IPC(0:1)                    !CURRENT DIPOLE
	  DATA IPC/'X','Y'/                     
	INTEGER IRED(0:STHIFR-1)                !REDUNDANT INTERFEROMETERS
	INTEGER ITCNT,ITCNT1                    !ITERATION COUNT
	REAL LM0(0:1)                           !BASIC SOURCE DISPLACEMENT
	INTEGER MAR(0:1),CMAR                   !MATRIX AREAS GAIN/PHASE
	INTEGER MAR1(0:1)                       
	INTEGER MAR2(0:1),CMAR2(0:1)            
	REAL ME                                 !M.E.
	REAL MENS(0:1,0:1)                      !SCAN NOISE G,P X,Y
	INTEGER MINST                           !INSTRUMENT
	REAL MU                                 !ADJUSTMENT ERROR
	INTEGER NDEG(0:1,0:1,0:1)               !DEGENERACY GAIN,PHASE X,Y
	INTEGER NPOL                            !# OF POL.
	INTEGER NRINT                           !# OF SCANS INTEGRATED
	INTEGER NTINT                           !# OF SCANS TO INTEGRATE
	REAL RAWGT(0:STHIFR-1)                  !SELFCAL WEIGHTS
 	REAL SAVSOL(0:STHTEL-1,0:1)             !SAVE SOLUTION G,P
	BYTE SCH(0:SCH__L-1)                    !SCAN HEADER
	  INTEGER*2 SCHI(0:SCH__L/LB_I-1)       
	  INTEGER SCHJ(0:SCH__L/LB_J-1)         
	  REAL SCHE(0:SCH__L/LB_E-1)            
	  DOUBLE PRECISION SCHD(0:SCH__L/LB_D-1)
	  EQUIVALENCE(SCH,SCHI,SCHJ,SCHE,SCHD)  
	INTEGER SETNAM(0:7)                     !FULL sector NAME
	REAL SOL(0:STHTEL-1,0:1,0:1)            !SOLUTION X,Y GAIN,PHASE
	DOUBLE PRECISION SRA,SDEC,SFRQ          !SOURCE RA, DEC, FREQ
	BYTE STH(0:STH__L-1)                    !sector HEADER
	  INTEGER*2 STHI(0:STH__L/LB_I-1)       
	  INTEGER STHJ(0:STH__L/LB_J-1)         
	  REAL STHE(0:STH__L/LB_E-1)            
	  DOUBLE PRECISION STHD(0:STH__L/LB_D-1)
	  EQUIVALENCE(STH,STHI,STHJ,STHE,STHD)  
	INTEGER STHP                            ! STH, SCH pointers
	INTEGER STP                             !SOURCE TYPE
	LOGICAL SWDIS                           !DISCARD?
	CHARACTER*16 TELNAM                     !TEL. NAMES
	  CHARACTER*1 TELNMA(0:15)              
	  EQUIVALENCE (TELNAM,TELNMA)           
	  DATA TELNAM/'0123456789ABCDEF'/       
	CHARACTER*20	TEXT			! message text buffer
	REAL TF(0:1)                            !INTEGR. TIME, BANDWIDTH
	REAL UV0(0:3)                           !BASIC UV COORDINATES
	REAL WGT(0:STHIFR-1,0:3)                !DATA WEIGHTS XX,XY,YX,YY
	REAL WGT1(0:STHIFR-1,0:3)               !DATA WEIGHTS XX,XY,YX,YY
	INTEGER N		                !DATA COUNTS
	REAL R2,R3
C!091019	COMPLEX C0                              !WNB070814
C-                                              
C                                               
C INIT                                          
C                                               
	JS=.TRUE.
	CSTNAM(0)=-1                               
!= I=IGP                                         
	DO IGP=0,1                              
	   MAR(IGP)=0                           
	   IF (JS) JS=WNMLGA(MAR(IGP),LSQ_T_REAL,STHTEL)
						!MATRIX AREAS
	   MAR1(IGP)=0                          
	   IF (JS) JS=WNMLGA(MAR1(IGP),LSQ_T_REAL,2*STHTEL)
	END DO                                  
	CMAR=0                                  
	IF (JS) JS=WNMLGA(CMAR,LSQ_T_COMPLEX,STHTEL)
	IF (.NOT.JS) THEN                       
	   CALL WNCTXT(F_TP,'ERROR: no memory for works paces')
	   GOTO 901                             
	END IF                                  
!= I=IGP I1=ITL                                  
	DO ITL=0,1                              
	  DO IGP=0,2                            !HISTO AREAS
	    CALL WNMHS8(HISBAD(IGP,ITL),1,1.)   
	  END DO                                
	END DO                                  
!                                               
	DO I1=0,STHTEL-1                        
	  TELS(I1)=.TRUE.                       
	END DO                                  
C                                               
C HEADINGS                                      
C                                               
	CALL WNCFHD(F_P,3,'Node:!AS        File:!AS',NODOUT,FILOUT)
	IF (APSOL(0) .OR. APSOL(1)) THEN
	   CALL WNCFHD(F_P,5,  '    HA    Rk  A(%) P(deg) A(WU)  P(WU) '//
	1	' Amax   Aavg  Arms dAmax(%) dPmax(d) I')
				!HEADER
	ELSE
	   CALL WNCFHD(F_P,5,  '    HA       Amax   Aavg  Arms'//
	1	'   Cmax   Cavg  Crms   Smax   Savg  Srms')
	END IF
	CALL WNCFHD(F_P,6,' ')                  
	IF (IAND(SHLV(2),F_T).NE.0) THEN        !LIST WANTED
	   IF (APSOL(0) .OR. APSOL(1)) THEN
	      CALL WNCTXT(F_T,'!/    HA    Rk  A(%) P(deg) A(WU)  P(WU) '//
	1	   ' Amax   Aavg  Arms dAmax(%) dPmax(d) I!/')
	   ELSE
	      CALL WNCTXT(F_T,'!/    HA       Amax   Aavg  Arms'//
	1	   '   Cmax   Cavg  Crms   Smax   Savg  Srms!/')
	   END IF
	END IF                                  
	CALL WNCTXT(F_P,'!^')                 !NEW PAGE
C                                               
C CLEAR ARRAYS                                  
C                                               
!= I=IFR I2=IGP I3=IXY                           
	DO IXY=0,1                              !CLEAR NOISES X,Y
	  DO IGP=0,1                            !GAIN/PHASE
	    DO I1=0,4                           !DATA TYPES
	      DO IFR=0,STHIFR-1                 !# IFRS
		JAV(IFR,I1,IGP,IXY)=0           
		EAV(IFR,I1,IGP,IXY)=0E0         
		DAV(IFR,I1,IGP,IXY)=0D0         
	      END DO                            
	    END DO                              
	  END DO                                
	END DO                                  
!= I1=IGP I2=IXY                                 
	DO IGP=0,1                              !CLEAR RANK CHECK G/P
	  DO IXY=0,1                            !X,Y
	    NDEG(IXY,1,IGP)=0                   
	  END DO                                
	END DO 
C                                               
C FORCED PHASES                                 
C                                               
!= I1=ITL I2=IGP I3=IXY                          
	DO IXY=0,1                              !CONTINUITY SOL. X,Y
	  DO IGP=0,1                            !G,P
	    DO ITL=0,STHTEL-1                   !TEL.
	      IF (IGP.EQ.0) THEN                !GAIN
		CSOL(ITL,IGP,IXY)=0.            
	      ELSE                              !PHASE
		CSOL(ITL,IGP,IXY)=FORPER(ITL)   
	      END IF                            
	    END DO                              
	  END DO                                
	END DO                                   
C                                               
C INIT ERROR FLAGS                              
C                                               
!= I1=IXY I2=IGP                                 
	DO IXY=0,1                              !X,Y
	  DO IGP=0,3                            !G,P,C
	    FLGERR(IGP,IXY)=.TRUE.              
	  END DO                                
	END DO                                  
C                                               
C MODEL INIT                                    
C                                               
	IF (DOALG) THEN                         
	  IF (.NOT.NMOMSL(FCAOUT,SETS,LPOFF)) THEN
						!CALCULATE MODEL
	    CALL WNCTXT(F_TP,'!/Error in model calculation')
	    GOTO 900                            !STOP
	  END IF                                
	END IF                                  
C                                               
C ALIGN INIT                                    
C                                               
	JS=.TRUE.                               
!= I=IGP                                         
	DO IGP=0,1                              !CLEAR LSQ AREAS
	   MAR2(IGP)=0                          
	   CMAR2(IGP)=0                         
	END DO                                  
	IF (DOALG .AND. .NOT.DOSCAL) THEN       !ALIGN
!= I=IGP I1=ITL I2=ITL1                          
	  DO IGP=0,1                            !GAIN/PHASE
	    DO ITL=0,STHTEL-1                   !ZERO EQUATIONS
	      DO ITL1=0,STHTEL-1                
		CEQ(ITL1,ITL,IGP,0)=0           
		CEQ(ITL1,ITL,IGP,1)=0           
	      END DO                            
	    END DO                              
	    IF (FORFRE(IGP)) THEN               !FORCE FREEDOM
	      J=0                               !COUNT EQUATIONS
	      DO ITL=0,STHTEL-1                 !ALL FREEDOMS
		IF (FREGPH(ITL,IGP).LE.STHTEL .AND.
	1               FREGPH(ITL,IGP).GT.0) THEN
						!SELECTED
		  J=MAX(0,FREGPH(ITL,IGP))      !FIND # OF EQUATIONS
		  CEQ(ITL,FREGPH(ITL,IGP)-1,IGP,0)=1
		  CEQ(ITL,FREGPH(ITL,IGP)-1,IGP,1)=1
		END IF                          
	      END DO                            
	      NDEG(IGP,1,0)=J                   !SAVE # OF EQUATIONS
	      NDEG(IGP,1,1)=J                   !FOR Y
	      IF (JS) JS=WNMLGA(MAR2(IGP),LSQ_T_REAL,NDEG(IGP,1,0))
						!GET LSQ AREA
	      IF (JS) JS=WNMLGA(CMAR2(IGP),LSQ_T_COMPLEX,NDEG(IGP,1,0))
	    END IF                              
	  END DO                                
 	END IF                                  
	IF (.NOT.JS) THEN                       
	   CALL WNCTXT(F_TP,'ERROR: No space for aligning')
	   GOTO 902                             
	END IF                                  
                                               
C                                               
C DO sectors                                    
C                                               
	DO WHILE(NSCSTL(FCAOUT,SETS,STH(0),STHP,SETNAM,LPOFF))
						!NEXT sector
C                                               
C GET IFR TABLES                                
C                                               
	  IF (.NOT.NSCSIF(FCAOUT,STH,IFRT,IFRA,ANG)) THEN
						!READ IFR TABLE
	    CALL WNCTXT(F_TP,'!/Error reading IFR table !AS',
	1               WNTTSG(SETNAM,0))       
	    GOTO 20                             !TRY NEXT SET
	  END IF                                
	  CALL NSCMBL(STHE(STH_RTP_E),STHJ(STH_NIFR_J),IFRT,
	1                       SIFRS,BASEL)    !MAKE BASEL.
	  CALL NCARRT(STHJ(STH_NIFR_J),BASEL,BASDEV,IRED(0),ANG)
						!GET REDUN.
	  IF (DOALG)                            !GET SELFCAL WEIGHTS
	1       CALL NCARAW(MWGT,MWGTD,STHJ(STH_NIFR_J),BASEL,RAWGT)
C                                               
C SHOW CURRENT sector                           
C                                               
	  DO I1=0,3                             
	    IF (CSTNAM(I1).NE.SETNAM(I1)) THEN  
	      DO I2=0,3                         
		CSTNAM(I2)=SETNAM(I2)           
	      END DO                            
	      CALL WNCTXT(SHLV(1),'Sector: !AS',WNTTSG(CSTNAM,3))
	    END IF                              
	  END DO                                
C                                               
C SOURCE MODEL                                  
C                                               
	  NPOL=STHI(STH_PLN_I)                  !# OF POL.
	  IF (DOALG) THEN                       !MODEL PRESENT
	    IF (.NOT.NMORDH(6,STP,SRA,SDEC,SFRQ)) GOTO 20
						!MODEL PARAMETERS
	    CALL NMOMST(STP,SRA,SDEC,STH,LM0,FRQ0,TF,MINST)
						!GET SOME DATA
	  END IF                                
C                                               
C ALIGN DATA                                    
C                                               
	  IF (DOALG .AND. .NOT.DOSCAL) THEN     !ALIGN
	    IF (FORFRE(1)) THEN                 !FORCED PHASE FREEDOM
!= I2=ITL                                        
	      DO I1=0,NDEG(1,1,0)-1             !SET ALL EQUATIONS
		DO ITL=0,STHTEL-1               
		  IF (CEQ(ITL,I1,1,0).NE.0) THEN!SET CORRECT SLOPE
		    CEQ(ITL,I1,1,0)=(STHE(STH_RTP_E+ITL)-STHE(STH_RTP_E))/72.
		    CEQ(ITL,I1,1,1)=CEQ(ITL,I1,1,0)
		  END IF                        
		END DO                          
	      END DO                            
	    END IF                              
	  END IF                                
C                                               
C DO SCANS                                      
C                                               
	  NTINT=MAX(1,NINT(HAINT/24./3600./STHE(STH_HAI_E)))
						!# TO INTEGRATE
!= I=ISCN                                        
	  DO ISCN=0,STHJ(STH_SCN_J)-1,NTINT     !ALL SCANS
C                                               
C INIT                                          
C                                               
	    HA=STHE(STH_HAB_E)+ISCN*STHE(STH_HAI_E)
						!HA OF first SCAN
	    IF (HA.LT.HARAN(1) .OR. HA.GT.HARAN(2)) GOTO 30
						!FORGET
C                                               
C GET FIRST DATA                                
C                                               
	    IF (.NOT.NSCSCF(FCAOUT,STH,IFRT,ISCN,CORAP,CORDAP,
	1	SCH,WGT,DAT,FLW)) THEN		! flags, first scan
 19	      CONTINUE
	      CALL WNCTXT(F_TP,' !6$EAF7.2 Error reading scan data',HA)
	      GOTO 20                           ! TRY NEXT sector
	    END IF                              
	    IF (.NOT.NSCSCR(FCAOUT,STH,IFRT,ISCN,CORAP,CORDAP,
	1	SCH,WGT,DAT)) GOTO 19		! weighted data, first scan
CC	    DO IXY=0,3,3 
CC	      DO IFR=0,STHJ(STH_NIFR_J)-1 	
CC		IF (IAND(FLW(IFR,IXY),FL_WGT)	! honour FL_WGT flag
CC	1	.NE.0) WGT(IFR,IXY)=WGT(IFR,IXY)*DOWNWT
CC	      END DO                        
CC	    END DO 
C                                               
C GET FIRST SOURCE MODEL                        
C                                               
	    IF (DOALG) THEN                     !MODEL PRESENT
	      CALL NMOMUV(STP,SRA,SDEC,STH,SCH,UV0)
						!GET UV DATA
	      CALL NMOMU4(0,FCAOUT,ISCN,STH,UV0,LM0,FRQ0,
	1               STHE(STH_RTP_E),NPOL,STHJ(STH_NIFR_J),
	1               IFRT,TF,MINST,CMOD)     !GET MODEL
	      CALL NMOCIX(STHJ,SCHE,ANG,CAMOD,CMOD)
						!MAKE XYX MODEL DATA
	    END IF                              
C                                               
C INTEGRATE                                     
C                                               
!= I=ISCN I2=ISCN1 I1=IFR I4=ICS IP=IXY          
	    NRINT=1                             ! 1 done
	    DO ISCN1=ISCN+1,ISCN+NTINT-1        ! read following scans
	      IF (NSCSCF(FCAOUT,STH,IFRT,ISCN1,CORAP,CORDAP,
	1	SCH,WGT1,DAT1,FLW)) THEN	! ... flags 
	      IF (NSCSCR(FCAOUT,STH,IFRT,ISCN1,CORAP,CORDAP,
	1	SCH,WGT1,DAT1)) THEN		! ... data
		 DO IXY=0,3	! XX AND YY -- WNB ADD XY AND YX
		  DO IFR=0,STHJ(STH_NIFR_J)-1 
		    IF (WGT(IFR,IXY).LE.0 .OR. 
	1	  	WGT1(IFR,IXY).LE.0) THEN
 		      DO ICS=0,1 
			DAT(ICS,IFR,IXY)=0.     
		      END DO                    
		      WGT(IFR,IXY)=0            
		    ELSE
CC		      IF (IAND(FLW(IFR,IXY),FL_WGT).NE.0)
CC	1		WGT1(IFR,IXY)=WGT1(IFR,IXY)*DOWNWT
CC						! honour FL_WGT flag
		      DO ICS=0,1
			DAT(ICS,IFR,IXY)=(WGT(IFR,IXY)*DAT(ICS,IFR,IXY)+
	1                       WGT1(IFR,IXY)*DAT1(ICS,IFR,IXY))/
	1                       (WGT(IFR,IXY)+WGT1(IFR,IXY))
		      END DO                    
		      WGT(IFR,IXY)=WGT(IFR,IXY)+WGT1(IFR,IXY)
						!NEW WEIGHT
		    END IF                      
		  END DO                        
		END DO                          
		IF (DOALG) THEN                 !MODEL PRESENT
		  CALL NMOMUV(STP,SRA,SDEC,STH,SCH,UV0)
						!GET UV DATA
		  CALL NMOMU4(0,FCAOUT,ISCN1,STH,UV0,LM0,FRQ0,
	1               STHE(STH_RTP_E),NPOL,STHJ(STH_NIFR_J),
	1               IFRT,TF,MINST,CMOD)     !GET MODEL
		  CALL NMOCIY(STHJ,SCHE,ANG,CAMOD,CMOD,
	1               NRINT,1)                !AVERAGE MODEL XYX
		END IF                          
		NRINT=NRINT+1                   !COUNT INTEGRATIONS
	      ELSE
	        GOTO 21
	      ENDIF
	      ELSE                              
		GOTO 21                         !NO MORE
	      END IF                            
	    END DO                              
 21         CONTINUE                            
	    HA=STHE(STH_HAB_E)+(ISCN+(NRINT-1.)/2.)*
	1                       STHE(STH_HAI_E) !HA OF INTEGRATED SCAN
C                                               
C MAKE CORRECT DATA                             
C
!= I=ISCN IP=IXY                                 
	    DO IXY=0,1                          !X,Y
	      ARMS(0,IXY)=0                     !sector NOT PRESENT
	      IF (IXY.EQ.0) THEN                !X
		IF (XYSOL(0).EQ.0) GOTO 10      !NO X WANTED
	      ELSE                              !Y
		IF (XYSOL(1).EQ.0) GOTO 10      !NO Y WANTED
		IF (STHI(STH_PLN_I).LE.1) GOTO 10
						!NO YY PRESENT
	      END IF                            
	      IF (.NOT.NCARMD(STHJ(STH_NIFR_J),IFRT,3*IXY,WGT,DAT,
	1               SIFRS,WGTMIN,           
	1               DWGT(0,IXY),AWGT(0,IXY),CDAT(0,IXY),DAMP(0,IXY),
	1               DPHAS(0,IXY),ARMS(0,IXY))) THEN
						!MAKE DATA
 		TEXT='scan'
		IF (NTINT.GT.1)
	1		CALL WNCTXS(TEXT,'!UJ integrated scans', NTINT)
		CALL WNCTXT(SHLV(1),' !6$EAF7.2 !AS '//
	1               'No data in !AS',HA,IPC(IXY),TEXT)
 	      END IF                            
 10           CONTINUE                          ! NEXT POL.
	    END DO
C
C WNB 000725 -- Make various averages
C
C
C GET WEIGHTS
C
	    DO I1=0,2		!MAX, AVER, RMS
	       DO IXY=0,3	!XX etc
		  DO I2=0,2	!A, C, S   
		     CSRMS(I1,IXY,I2)=0	!CLEAR
		  END DO
	       END DO
	    END DO
	    DO IXY=0,3		!XX,XY,YX,YY
	       DO I2=0,2	!A,C,S
		  D0=0		!RMS
		  R0=0		!MAX. WEIGHT
		  R1=0
		  N=0		!COUNT
		  DO I=0,STHJ(STH_NIFR_J)-1 !ALL IFRS
		     R2=0	!ASSUME ZERO WEIGHT
		     R3=0 
		     IF (WGT(I,IXY).GT.0 .AND. !DATA PRESENT
	1	  SIFRS(IFRT(I)/256,MOD(IFRT(I),256)).NE.0) THEN !IFR SELECTED
			IF (I2.EQ.0) THEN
			   D1=ABS(CMPLX(DAT(0,I,IXY),DAT(1,I,IXY))) !AMPL
			ELSE IF (I2.EQ.1) THEN
			   D1=DAT(0,I,IXY) !COS
			ELSE
			   D1=DAT(1,I,IXY) !SIN
			END IF
			R2=SQRT(WGT(I,IXY)) !WEIGHT
			IF (R2.GT.0) THEN
			   R3=R2*D1
			   R0=MAX(R0,R2) !GET MAXIMA
			   R1=MAX(R1,R3)
			   CSRMS(0,IXY,I2)=MAX(CSRMS(0,IXY,I2),ABS(D1))
			   CSRMS(1,IXY,I2)=CSRMS(1,IXY,I2)+D1 !AVERAGE
			   D0=D0+D1*D1 !RMS
			   N=N+1 !COUNT
			END IF
		     END IF
		  END DO
C       
C       CHECK DATA PRESENCE
C       
		  IF (CSRMS(0,IXY,I2).NE.0) THEN !DATA PRESENT
C       
C       NORMALISE WEIGHT
C       
		     DO I=0,STHJ(STH_NIFR_J)-1
			IF (R2.GT.0) THEN
			   R2=(R2/R0)**2
			   R3=R3/R1
			   IF (R3.LT.WGTMIN) THEN !FORGET POINT
			      R2=0
			      R3=0
			      N=N-1 !CORRECT AVERAGES
			      CSRMS(1,IXY,I2)=CSRMS(1,IXY,I2)-D1
			      D0=D0-D1*D1
			   ELSE
			      R3=R3**2
			   END IF
			END IF
		     END DO
C       
C       CALCULATE AMPL. STATISTICS
C       
		     IF (N.LE.0) THEN !NO DATA LEFT
			CSRMS(0,IXY,I2)=0
		     ELSE
			CSRMS(1,IXY,I2)=CSRMS(1,IXY,I2)/N !AVER. AMPL
			CSRMS(2,IXY,I2)=SQRT(ABS(D0-
	1		     CSRMS(1,IXY,I2)*CSRMS(1,IXY,I2)*N)/N) !RMS
		     END IF
		  END IF
	       END DO
	    END DO
C
C
C NOTE (JPH 961212):
C	The course to take in the absence of data is not clear. As is, the code
C copies the corrections from the previous integration interval (at least in
C some cases that I have seen). Since the error is reported correctly, this
C behaviour is probably intended. It is also in tune with what happens for a
C no-data scan in an integration interval.
C	One may argue that instead it would be better to be honest and leave
C the corrections undefined. At any rate, it is not clear what implications a 
C change in the code might have...                        
C                                               
C RESET CONTINUITY                              
C User parameter CONTINUITY: If NO, CSOLVE=.false..
C  If this is so, the gain and phase values from a 
C  previous run are discarded by resetting CSOL(*,0, ) to 0 (phase) and 
C  CSOL(*,1, ) to the forcing phases. The same is done if the
C  previous run left an error status.           
C                                               
C                                               
	    IF (.NOT.CSOLVE) THEN               !NO CONTINUITY
!= I=ISCN I1=IXY I2=IGP I3=ITL                   
	      DO IXY=0,1                        !X,Y
 		DO ITL=0,STHTEL-1             !TELESCOPES
 		  CSOL(ITL,0,IXY)=0         
 		  CSOL(ITL,1,IXY)=FORPER(ITL)
                END DO                          
	      END DO                            
	    ELSE                                !RESET AFTER ERRORS
	      DO IXY=0,1                        !X,Y
		IF (.NOT.FLGERR(0,IXY) .OR. .NOT.FLGERR(1,IXY)) THEN
						!RESET
		  DO ITL=0,STHTEL-1             
		    CSOL(ITL,0,IXY)=0           !GAIN
		    CSOL(ITL,1,IXY)=FORPER(ITL) !PHASE
		  END DO                        
		END IF                          
	      END DO                            
	    END IF                              
C                                               
C INIT ERROR FLAGS                              
C                                               
!= I=ISCN I1=IXY I2=IGP                          
	    DO IXY=0,1                          !X,Y
	      DO IGP=0,3                        !G,P,C
		FLGERR(IGP,IXY)=.TRUE.          
	      END DO                            
	    END DO                                               
!=
C                                               
C           XYSOL(X=0:Y=1) indicates which of XX and YY polarisations
C           must be processed
C APSOL(gn=0:ph=1) does the same for gain, phase
C XOSOL indicates complex-only processing       
C DOALG: align or selfcal                       
C DOSCAL: selfcal                               
C SWDIS: A local switch to check if any solution branch is taken
C FORFRE:                                       
C                                               
C CSOL is the accumulated solution that is used as starting point, SOL is the
C  increment calculated in the present iteration
C                                               
C The routines called are organised in families NCA<x><y><z> where <x> is R, S 
C  or A for Redun/Selfcal/Align, <y> is G, P, C for gain/phase/complex,
C  <z> has no specific meaning                  
C                                               
C SOLVE WITH Stokes Q=0                         
C                                               
	    ITCNT1=0                            !ITERATION COUNT
	    IF (XYSOL(0).NE.0 .AND. XYSOL(1).NE.0 .AND.   
	1               ARMS(0,0).NE.0 .AND.    
	1               ARMS(0,1).NE.0) THEN    !XX AND YY PRESENT
	      IF (APSOL(0) .AND. .NOT.DOALG .AND. .NOT.XOSOL) THEN
						!DO GAIN
		ITCNT1=ITCNT1+1                 
		IF (.NOT.NCARG2(MAR1(0),STHJ(STH_NIFR_J),IFRT,BASEL,
	1               I1,IRED(0),DWGT(0,0),AWGT(0,0),CDAT(0,0),
	1               DAMP(0,0),DPHAS(0,0),CAMOD(0,0),RAWGT(0),
	1               CSOL(0,0,0),SOL(0,0,0),MU,ME)) THEN
						!GET GAIN SOLUTION
		ELSE                            
!= I=ISCN I1=IXY I2=ITL                          
		  DO IXY=0,1                    !MAKE CONTINUITY X/Y
		    DO ITL=0,STHTEL-1           
		      CSOL(ITL,0,IXY)=CSOL(ITL,0,IXY)+SOL(ITL,IXY,0)
		    END DO                      
		  END DO                        
		END IF
!                          
	      END IF                            
	      IF (APSOL(1) .AND. .NOT.DOALG .AND. .NOT.XOSOL) THEN
						!DO PHASE
		ITCNT1=ITCNT1+1                 
		IF (.NOT.NCARP2(MAR1(1),STHJ(STH_NIFR_J),IFRT,BASEL,
	1               IXY,IRED(0),DWGT(0,0),AWGT(0,0),CDAT(0,0),
	1               DAMP(0,0),DPHAS(0,0),CAMOD(0,0),RAWGT(0),
	1               CSOL(0,0,0),SOL(0,0,0),MU,ME)) THEN
						!GET PHASE SOLUTION
		ELSE                            
!= I=ISCN I1=IXY I2=ITL                          
		  DO IXY=0,1                    !MAKE CONTINUITY
		    DO ITL=0,STHTEL-1           
		      CSOL(ITL,1,IXY)=CSOL(ITL,1,IXY)+SOL(ITL,IXY,1)
		    END DO                      
		  END DO                        
!                                               
		END IF                          
	      END IF                            
	    END IF                              
C                                               
C SOLVE X, Y                                    
C                                               
C The following IXY loop contains a gain section followed by a phase section,
C  the two being identical except for a few index values changing from 0 to 1 
C  and the use of either NCA<x>G<z> or NCA<x>P<z> routines
C                                               
!= IP=IXY                                        
	    DO IXY=0,1                          
!!	print *,ixy                             
	      ITCNT=ITCNT1                      
	      IF (ARMS(0,IXY).NE.0) THEN        !CAN DO
C                                               
C Gain                                          
C                                               
		IF (APSOL(0) .AND. .NOT.XOSOL) THEN
						!DO GAIN
		  SWDIS=.TRUE.                  !DISCARD
C Redun gain                                    
		  IF (.NOT.DOALG) THEN          
		    SWDIS=.FALSE.               
		    JS=NCARGS(MAR(0),STHJ(STH_NIFR_J),IFRT,BASEL,
	1               NDEG(0,0,IXY),IRED(0),DWGT(0,IXY),AWGT(0,IXY),
	1               CDAT(0,IXY),DAMP(0,IXY),DPHAS(0,IXY),
	1               CAMOD(0,3*IXY),RAWGT(0),
	1               CSOL(0,0,IXY),SOL(0,IXY,0),MU,ME)
C Selfcal gain                                  
		  ELSE IF (DOSCAL) THEN         
		    SWDIS=.FALSE.               
		    JS=NCASGS(MAR(0),STHJ(STH_NIFR_J),IFRT,BASEL,
	1               NDEG(0,0,IXY),IRED(0),DWGT(0,IXY),AWGT(0,IXY),
	1               CDAT(0,IXY),DAMP(0,IXY),DPHAS(0,IXY),
	1               CAMOD(0,3*IXY),RAWGT(0),
	1               CSOL(0,0,IXY),SOL(0,IXY,0),MU,ME)
C Full align gain                               
		  ELSE IF (.NOT.FORFRE(0)) THEN 
		    SWDIS=.FALSE.               
		    JS=NCARGS(MAR(0),STHJ(STH_NIFR_J),IFRT,BASEL,
	1               NDEG(0,0,IXY),IRED(0),DWGT(0,IXY),AWGT(0,IXY),
	1               CDAT(0,IXY),DAMP(0,IXY),DPHAS(0,IXY),
	1               CAMOD(0,3*IXY),RAWGT(0),
	1               CSOL(0,0,IXY),SOL(0,IXY,0),MU,ME)
						!GET GAIN SOLUTION
		  END IF                        
C                                               
		  IF (.NOT.SWDIS) THEN          ! valid solution?         
		    ITCNT=ITCNT+1               
		    IF (.NOT.JS) THEN           
		      IF (SHLV(1).NE.0)         
	1               CALL WNCTXT(SHLV(1),    
	1                ' !6$EAF7.2 !AS Cannot solve gain',HA,IPC(IXY))
		      FLGERR(0,IXY)=.FALSE.     
		    ELSE                        !SET ERRORS
!= I=ISCN I1=IXY I2=ITL IP=IXY                   
		      DO ITL=0,STHTEL-1         !MAKE CONTINUITY
			CSOL(ITL,0,IXY)=CSOL(ITL,0,IXY)+SOL(ITL,IXY,0)
		      END DO                    
!= IP=IXY                                        
		    END IF                      
		  END IF                        
		END IF                          
C                                               
C Phase                                         
C                                               
		IF (APSOL(1) .AND. .NOT.XOSOL) THEN
						!DO PHASE
		  SWDIS=.TRUE.                  !DISCARD
C Redun phase                                   
		  IF (.NOT.DOALG) THEN          
		    SWDIS=.FALSE.               
		    JS=NCARPS(MAR(1),STHJ(STH_NIFR_J),IFRT,BASEL,
	1               NDEG(1,0,IXY),IRED(0),DWGT(0,IXY),AWGT(0,IXY),
	1               CDAT(0,IXY),DAMP(0,IXY),DPHAS(0,IXY),
	1               CAMOD(0,3*IXY),RAWGT(0),
	1               CSOL(0,0,IXY),SOL(0,IXY,1),MU,ME)
C Selfcal phase                                 
		  ELSE IF (DOSCAL) THEN         
		    SWDIS=.FALSE.               
		    JS=NCASPS(MAR(1),STHJ(STH_NIFR_J),IFRT,BASEL,
	1               NDEG(1,0,IXY),IRED(0),DWGT(0,IXY),AWGT(0,IXY),
	1               CDAT(0,IXY),DAMP(0,IXY),DPHAS(0,IXY),
	1               CAMOD(0,3*IXY),RAWGT(0),
	1               CSOL(0,0,IXY),SOL(0,IXY,1),MU,ME)
C Full align phase                              
		  ELSE IF (.NOT.FORFRE(1)) THEN 
		    SWDIS=.FALSE.               
		    JS=NCARPS(MAR(1),STHJ(STH_NIFR_J),IFRT,BASEL,
	1               NDEG(1,0,IXY),IRED(0),DWGT(0,IXY),AWGT(0,IXY),
	1               CDAT(0,IXY),DAMP(0,IXY),DPHAS(0,IXY),
	1               CAMOD(0,3*IXY),RAWGT(0),
	1               CSOL(0,0,IXY),SOL(0,IXY,1),MU,ME)
		  END IF                        
C                                               
		  IF (.NOT.SWDIS) THEN          ! valid solution?          
		    ITCNT=ITCNT+1               
		    IF (.NOT.JS) THEN           
		      IF (SHLV(1).NE.0)         
	1               CALL WNCTXT(SHLV(1),    
	1               ' !6$EAF7.2 !AS Cannot solve phase',HA,IPC(IXY))
		      FLGERR(1,IXY)=.FALSE.     
		    ELSE                        !SET ERRORS
!= I=ISCN I2=ITL                                 
		      DO ITL=0,STHTEL-1         !MAKE CONTINUITY
			CSOL(ITL,1,IXY)=CSOL(ITL,1,IXY)+SOL(ITL,IXY,1)
		      END DO                    
!                                               
		    END IF                      
		  END IF                        
		END IF                          
C                                               
C GET CONSTRAINTS                               
C                                               
		IF (DOALG .AND. .NOT.DOSCAL .AND.
	1                       FORFRE(0)) THEN
	        ELSE                            ! GET CONSTRAINTS
		  IF (NDEG(0,0,IXY).NE.NDEG(0,1,IXY)) THEN
						! GAIN
		    CALL WNMLGC(MAR(0),J,CEQ(0,0,0,IXY))
						! GET CONSTRAINT EQS
		    IF (SHLV(2).NE.0) THEN      ! SHOW
		      CALL WNCTXT(SHLV(2),      
	1                ' !6$EAF7.2 !AS New gain constraints:',
	1                       HA,IPC(IXY))    
		      DO I1=0,NDEG(0,0,IXY)-1   
			CALL WNCTXT(SHLV(2),'!80$10Q1!10C\!4$#E7.0',
	1                       STHTEL,CEQ(0,I1,0,IXY))
		      END DO                    
		    END IF                      
		    NDEG(0,1,IXY)=NDEG(0,0,IXY) !NEW CHECK
		  END IF                        
		  IF (NDEG(1,0,IXY).NE.NDEG(1,1,IXY)) THEN
						!PHASE
		    CALL WNMLGC(MAR(1),J,CEQ(0,0,1,IXY))
						!GET CONSTRAINT EQS
		    IF (SHLV(2).NE.0) THEN      !SHOW
		      CALL WNCTXT(SHLV(2),      
	1                  ' !6$EAF7.2 !AS New phase constraints:',
	1                       HA,IPC(IXY))    
		      DO I1=0,NDEG(1,0,IXY)-1   
			CALL WNCTXT(SHLV(2),'!80$10Q1!10C\!4$#E7.0',
	1                       STHTEL,CEQ(0,I1,1,IXY))
		      END DO                    
		    END IF                      
		    NDEG(1,1,IXY)=NDEG(1,0,IXY) !NEW CHECK
		  END IF                        
		END IF                          
C                                               
C ALIGN only                                    
C  Init ALIGN                                   
C                                               
		IF (DOALG .AND. .NOT.DOSCAL) THEN
		   JS=.TRUE.                    
!= I=ISCN I2=IGP                                 
		   DO IGP=0,1                   
		      IF (APSOL(IGP)) THEN      !DO GAIN/PHASE
			 IF (.NOT.FORFRE(IGP)) THEN
						!NEED LSQ AREA
			    IF (JS) JS=WNMLGA(MAR2(IGP),LSQ_T_REAL,
	1                        NDEG(IGP,1,IXY))
						!GET LSQ AREA
			    IF (JS) JS=WNMLGA(CMAR2(IGP),LSQ_T_COMPLEX,
	1                        NDEG(IGP,1,IXY))
			 END IF                 
		      END IF                    
		   END DO                       
!                                               
		   IF (.NOT.JS) THEN            
		      CALL WNCTXT(F_TP,'ERROR: Cannot align: no workspace')
		      GOTO 902                  
		   END IF                       
!!              END IF                          
C                                               
						
!!              IF (DOALG .AND. .NOT.DOSCAL) THEN
		  IF (APSOL(0) .AND. .NOT.XOSOL) THEN
C                                               
C  Do ALIGN gain                                      
C                   ITCNT=ITCNT+1               
		    IF (.NOT.NCAAGS(MAR2(0),STHJ(STH_NIFR_J),IFRT,BASEL,
	1               J,IRED(0),DWGT(0,IXY),AWGT(0,IXY),
	1               CDAT(0,IXY),DAMP(0,IXY),DPHAS(0,IXY),
	1               CAMOD(0,3*IXY),RAWGT(0),
	1               CSOL(0,0,IXY),SOL(0,IXY,0),MU,ME,
	1               NDEG(0,1,IXY),CEQ(0,0,0,IXY))) THEN
						!GET GAIN SOLUTION
		      IF (SHLV(1).NE.0) CALL WNCTXT(SHLV(1),
	1               ' !6$EAF7.2 !AS Cannot solve gain',HA,IPC(IXY))
		      FLGERR(0,IXY)=.FALSE.     
		    ELSE                        !SET ERRORS
!= I=ISCN I2=ITL                                 
		      DO ITL=0,STHTEL-1         !MAKE CONTINUITY
			CSOL(ITL,0,IXY)=CSOL(ITL,0,IXY)+SOL(ITL,IXY,0)
		      END DO                    
		    END IF                      
!                                               
		  END IF                        
C                                               
C  Do ALIGN phase                               
C                                               
		  IF (APSOL(1) .AND. .NOT.XOSOL) THEN
		    ITCNT=ITCNT+1               
		    IF (.NOT.NCAAPS(MAR2(1),STHJ(STH_NIFR_J),IFRT,BASEL,
	1               J,IRED(0),DWGT(0,IXY),AWGT(0,IXY),
	1               CDAT(0,IXY),DAMP(0,IXY),DPHAS(0,IXY),
	1               CAMOD(0,3*IXY),RAWGT(0),
	1               CSOL(0,0,IXY),SOL(0,IXY,1),MU,ME,
	1               NDEG(1,1,IXY),CEQ(0,0,1,IXY))) THEN
						!GET PHASE SOL.
		      IF (SHLV(1).NE.0) CALL WNCTXT(SHLV(1),
	1               ' !6$EAF7.2 !AS Cannot solve phase',HA,IPC(IXY))
		      FLGERR(1,IXY)=.FALSE.     
		    ELSE                        !SET ERRORS
!= I=ISCN I2=ITL                                 
		      DO ITL=0,STHTEL-1         !MAKE CONTINUITY
			CSOL(ITL,1,IXY)=CSOL(ITL,1,IXY)+SOL(ITL,IXY,1)
		      END DO                    
!                                               
		    END IF                      
		  END IF                        
		END IF                          ! align                          
C                                               
C DO COMPLEX                                    
C                                               
		IF (XSOLVE.NE.0 .AND. 
	1	  FLGERR(0,IXY) .AND. FLGERR(1,IXY)) THEN
!= I=ISCN I2=ITL I4=IGP                          
C                                               
C  Save the present solution for the present polarisation IXY
C                                               
		  DO IGP=0,1                    
		    DO ITL=0,STHTEL-1           
		      SAVSOL(ITL,IGP)=CSOL(ITL,IGP,IXY)
		    END DO                      
		  END DO                        
!                                               
		  J=MIN(MXITCT,NINT(RIN(1)))    !MAX. LOOPS
C Redun                                         
		  IF (.NOT.DOALG) THEN          
		     JS=NCARCS(CMAR,STHJ(STH_NIFR_J),IFRT,BASEL,
	1                 J,IRED(0),DWGT(0,IXY),AWGT(0,IXY),
	1                 CDAT(0,IXY),DAMP(0,IXY),DPHAS(0,IXY),
	1                 CAMOD(0,3*IXY),RAWGT(0),
	1                 CSOL(0,0,IXY),SOL(0,IXY,0),MU,ME)
C Selfcal                                       
		  ELSE IF (DOSCAL) THEN         
		     JS=NCASCS(CMAR,STHJ(STH_NIFR_J),IFRT,BASEL,
	1                 J,IRED(0),DWGT(0,IXY),AWGT(0,IXY),
	1                 CDAT(0,IXY),DAMP(0,IXY),DPHAS(0,IXY),
	1                 CAMOD(0,3*IXY),RAWGT(0),
	1                 CSOL(0,0,IXY),SOL(0,IXY,0),MU,ME)
C Align                                         
		  ELSE                          
		     I4=1                       !ASSUME MORE PHASE
		     IF (NDEG(0,1,IXY).GT.NDEG(1,1,IXY)) I4=0
		     JS=NCAACS(CMAR2(I4),STHJ(STH_NIFR_J),IFRT,BASEL,
	1                 J,IRED(0),DWGT(0,IXY),AWGT(0,IXY),
	1                 CDAT(0,IXY),DAMP(0,IXY),DPHAS(0,IXY),
	1                 CAMOD(0,3*IXY),RAWGT(0),
	1                 CSOL(0,0,IXY),SOL(0,IXY,0),MU,ME,
	1                 NDEG(0,1,IXY),CEQ(0,0,0,IXY))
						!GET COMPLEX
		  END IF                        
		  ITCNT=ITCNT+MIN(MXITCT,NINT(RIN(1)))-J
						!COUNT DONE
		  IF (.NOT.JS) THEN             
		     IF (SHLV(1).NE.0) CALL WNCTXT(SHLV(1),
	1                 ' !6$EAF7.2 !AS Cannot solve complex',HA,IPC(IXY))
		     FLGERR(2,IXY)=.FALSE.      
		  ELSE                          !SET ERRORS
!= I=ISCN I2=ITL I4=IGP                          
		     DO IGP=0,1                 
			DO ITL=0,STHTEL-1       !MAKE CONTINUITY
			   CSOL(ITL,IGP,IXY)=CSOL(ITL,IGP,IXY)+SOL(ITL,IXY,IGP)
			END DO                  
		     END DO                     
!                                               
		     IF (ME.GT.0 .OR. ME.LT.-.001) GOTO 51
						!NOT FINISHED
		  END IF                        
		  GOTO 50                       !NO MORE
 51               CONTINUE                      
		  IF (SHLV(1).NE.0) CALL WNCTXT(SHLV(1),
	1               ' !6$EAF7.2 !AS Complex solution too slow',
	1                       HA,IPC(IXY))    
		  FLGERR(2,IXY)=.FALSE.         
 50               CONTINUE                      
		  IF (.NOT.FLGERR(2,IXY)) THEN  !RESTORE SOL
!= I=ISCN I2=ITL I4=IGP                          
		    DO IGP=0,1                  !G,P
		      DO ITL=0,STHTEL-1         
			CSOL(ITL,IGP,IXY)=SAVSOL(ITL,IGP)
		      END DO                    
		    END DO                      
!                                               
		  END IF                        
		END IF                          
C                                               
C EXIT ALIGN                                    
C                                               
		IF (DOALG .AND. .NOT.DOSCAL) THEN
!= I=ISCN I2=IGP                                 
		   DO IGP=0,1                   
		      IF (APSOL(IGP)) THEN      !DO GAIN
			 IF (.NOT.FORFRE(IGP)) THEN
						!NEED LSQ AREA
			    CALL WNMLFA(MAR2(IGP))
						!FREE
			    CALL WNMLFA(CMAR2(IGP))
			 END IF                 
		      END IF                    
		   END DO                       
!                                               
		END IF                          
C                                               
C GET ERRORS                                    
C                                               
!= I=ISCN I2=ITL                                 
		DO ITL=0,STHTEL-1               !NORMALIZE CORRECTIONS
		  CSOL(ITL,1,IXY)=WNGENR(CSOL(ITL,1,IXY))
		END DO                          
!                                               
		IF (APSOL(0) .AND. APSOL(1)) THEN
						!COMPLEX ERRORS
		  IF (.NOT.DOALG) THEN          
		    CALL NCARCE(MAR(0),STHJ(STH_NIFR_J),IFRT,BASEL,
	1               NDEG(0,0,IXY),IRED(0),DWGT(0,IXY),AWGT(0,IXY),
	1               CDAT(0,IXY),DAMP(0,IXY),DPHAS(0,IXY),
	1               CAMOD(0,3*IXY),RAWGT(0),
	1               CSOL(0,0,IXY),SOL(0,IXY,0),MU,ME,ARMS(0,IXY),
	1               JAV(0,0,0,IXY),EAV(0,0,0,IXY),DAV(0,0,0,IXY))
		  ELSE IF (DOSCAL) THEN         !SELFCAL
		    CALL NCASCE(MAR(0),STHJ(STH_NIFR_J),IFRT,BASEL,
	1               NDEG(0,0,IXY),IRED(0),DWGT(0,IXY),AWGT(0,IXY),
	1               CDAT(0,IXY),DAMP(0,IXY),DPHAS(0,IXY),
	1               CAMOD(0,3*IXY),RAWGT(0),
	1               CSOL(0,0,IXY),SOL(0,IXY,0),MU,ME,ARMS(0,IXY),
	1               JAV(0,0,0,IXY),EAV(0,0,0,IXY),DAV(0,0,0,IXY))
		  ELSE                          !ALIGN
		    CALL NCAACE(MAR(0),STHJ(STH_NIFR_J),IFRT,BASEL,
	1               NDEG(0,0,IXY),IRED(0),DWGT(0,IXY),AWGT(0,IXY),
	1               CDAT(0,IXY),DAMP(0,IXY),DPHAS(0,IXY),
	1               CAMOD(0,3*IXY),RAWGT(0),
	1               CSOL(0,0,IXY),SOL(0,IXY,0),MU,ME,ARMS(0,IXY),
	1               JAV(0,0,0,IXY),EAV(0,0,0,IXY),DAV(0,0,0,IXY))
		  END IF                        
		ELSE                            
		  IF (APSOL(0)) THEN            
		    IF (.NOT.DOALG) THEN        
		      CALL NCARGE(MAR(0),STHJ(STH_NIFR_J),IFRT,BASEL,
						!GAIN ERR
	1               NDEG(0,0,IXY),IRED(0),DWGT(0,IXY),AWGT(0,IXY),
	1               CDAT(0,IXY),DAMP(0,IXY),DPHAS(0,IXY),
	1               CAMOD(0,3*IXY),RAWGT(0),
	1               CSOL(0,0,IXY),SOL(0,IXY,0),MU,ME,ARMS(0,IXY),
	1               JAV(0,0,0,IXY),EAV(0,0,0,IXY),DAV(0,0,0,IXY))
		    ELSE IF (DOSCAL) THEN       !SELFCAL
		      CALL NCASGE(MAR(0),STHJ(STH_NIFR_J),IFRT,BASEL,
						!GAIN ERR
	1               NDEG(0,0,IXY),IRED(0),DWGT(0,IXY),AWGT(0,IXY),
	1               CDAT(0,IXY),DAMP(0,IXY),DPHAS(0,IXY),
	1               CAMOD(0,3*IXY),RAWGT(0),
	1               CSOL(0,0,IXY),SOL(0,IXY,0),MU,ME,ARMS(0,IXY),
	1               JAV(0,0,0,IXY),EAV(0,0,0,IXY),DAV(0,0,0,IXY))
		    ELSE                        !ALIGN
		      CALL NCAAGE(MAR(0),STHJ(STH_NIFR_J),IFRT,BASEL,
						!GAIN ERR
	1               NDEG(0,0,IXY),IRED(0),DWGT(0,IXY),AWGT(0,IXY),
	1               CDAT(0,IXY),DAMP(0,IXY),DPHAS(0,IXY),
	1               CAMOD(0,3*IXY),RAWGT(0),
	1               CSOL(0,0,IXY),SOL(0,IXY,0),MU,ME,ARMS(0,IXY),
	1               JAV(0,0,0,IXY),EAV(0,0,0,IXY),DAV(0,0,0,IXY))
		    END IF                      
		  END IF                        
		  IF (APSOL(1)) THEN            
		    IF (.NOT.DOALG) THEN        
		      CALL NCARPE(MAR(1),STHJ(STH_NIFR_J),IFRT,BASEL,
						!PHASE ERR
	1               NDEG(1,0,IXY),IRED(0),DWGT(0,IXY),AWGT(0,IXY),
	1               CDAT(0,IXY),DAMP(0,IXY),DPHAS(0,IXY),
	1               CAMOD(0,3*IXY),RAWGT(0),
	1               CSOL(0,0,IXY),SOL(0,IXY,1),MU,ME,ARMS(0,IXY),
	1               JAV(0,0,0,IXY),EAV(0,0,0,IXY),DAV(0,0,0,IXY))
		    ELSE IF (DOSCAL) THEN       !SELFCAL
		      CALL NCASPE(MAR(1),STHJ(STH_NIFR_J),IFRT,BASEL,
						!PHASE ERR
	1               NDEG(1,0,IXY),IRED(0),DWGT(0,IXY),AWGT(0,IXY),
	1               CDAT(0,IXY),DAMP(0,IXY),DPHAS(0,IXY),
	1               CAMOD(0,3*IXY),RAWGT(0),
	1               CSOL(0,0,IXY),SOL(0,IXY,1),MU,ME,ARMS(0,IXY),
	1               JAV(0,0,0,IXY),EAV(0,0,0,IXY),DAV(0,0,0,IXY))
		    ELSE                        !ALIGN
		      CALL NCAAPE(MAR(1),STHJ(STH_NIFR_J),IFRT,BASEL,
						!PHASE ERR
	1               NDEG(1,0,IXY),IRED(0),DWGT(0,IXY),AWGT(0,IXY),
	1               CDAT(0,IXY),DAMP(0,IXY),DPHAS(0,IXY),
	1               CAMOD(0,3*IXY),RAWGT(0),
	1               CSOL(0,0,IXY),SOL(0,IXY,1),MU,ME,ARMS(0,IXY),
	1               JAV(0,0,0,IXY),EAV(0,0,0,IXY),DAV(0,0,0,IXY))
		    END IF                      
		  END IF                        
		END IF                          
C                                               
C SHOW RESULT                                   
C                                               
		IF (SHLV(2).NE.0) THEN
		   IF (APSOL(0) .OR. APSOL(1)) THEN
		      CALL WNCTXT(SHLV(2),
				!SHOW LINE
	1		   '!6$EAF7.2 !AS !1$XJ!1$XJ !5$E7.1 !5$EAR7.1 '//
	1		   '!6$E7.1 !6$E7.1 !5$E7.0 !7$E8.1 !4$E7.0 '//
	1		   '!5$E7.0 !2$XJ !6$EAR7.1 !2$XJ !2$UJ',
	1		   HA,IPC(IXY),            
	1		   NDEG(0,0,IXY),NDEG(1,0,IXY),
	1		   100*EAV(1,0,0,IXY),EAV(1,0,1,IXY),
	1		   EAV(0,0,0,IXY),EAV(0,0,1,IXY),
	1		   ARMS(0,IXY),ARMS(1,IXY),ARMS(2,IXY),
	1		   100*EAV(4,0,0,IXY),JAV(4,0,0,IXY),
	1		   EAV(4,0,1,IXY),JAV(4,0,1,IXY),ITCNT)
		   ELSE
		      CALL WNCTXT(SHLV(2),
	1		   '!6$EAF7.2 !AS!AS !7$E8.1 !7$E8.1 !5$E8.1'//
	1		   ' !7$E8.1 !7$E8.1 !5$E8.1'//
	1		   ' !7$E8.1 !7$E8.1 !5$E8.1',
	1		   HA,IPC(IXY),IPC(0),
	1		   CSRMS(0,2*IXY,0),CSRMS(1,2*IXY,0),CSRMS(2,2*IXY,0),
	1		   CSRMS(0,2*IXY,1),CSRMS(1,2*IXY,1),CSRMS(2,2*IXY,1),
	1		   CSRMS(0,2*IXY,2),CSRMS(1,2*IXY,2),CSRMS(2,2*IXY,2))
		      CALL WNCTXT(SHLV(2),
	1		   '!6$EAF7.2 !AS!AS !7$E8.1 !7$E8.1 !5$E8.1'//
	1		   ' !7$E8.1 !7$E8.1 !5$E8.1'//
	1		   ' !7$E8.1 !7$E8.1 !5$E8.1',
	1		   HA,IPC(IXY),IPC(1),
	1		   CSRMS(0,2*IXY+1,0),CSRMS(1,2*IXY+1,0),
	1		   CSRMS(2,2*IXY+1,0),
	1		   CSRMS(0,2*IXY+1,1),CSRMS(1,2*IXY+1,1),
	1		   CSRMS(2,2*IXY+1,1),
	1		   CSRMS(0,2*IXY+1,2),CSRMS(1,2*IXY+1,2),
	1		   CSRMS(2,2*IXY+1,2))
		   END IF
		END IF
		IF (SHLV(3).NE.0) THEN !SHOW SOLUTIONS
		   IF (APSOL(0) .OR. APSOL(1)) THEN
		      CALL WNCTXT(SHLV(3),'!80$15Q1!10C\Gain:!6$#E9.3',
	1		   STHTEL,CSOL(0,0,IXY))
		      CALL WNCTXT(SHLV(3),'!80$15Q1!10C\Phase:!6$#E9.3',
	1		   STHTEL,CSOL(0,1,IXY))
		   END IF           
		END IF               
C                                               
C CHECK RESULT                                  
C                                               
!= I=ISCN I2=IGP                                 
		DO IGP=0,1                      !G,P
		  IF (APSOL(IGP).NE.0 .AND. FLGERR(IGP,IXY)) THEN
						!DO CHECK
		    IF (EAV(0,0,IGP,IXY).GT.RIN(3)*EAV(3,0,IGP,IXY) .AND.
	1                       JAV(2,0,IGP,IXY).GT.2*STHJ(STH_NIFR_J))
	1                               THEN    !OUT OF LIMITS
		      IF (SHLV(1).NE.0) CALL WNCTXT(SHLV(1),
	1                       '!7$EAF7.2 !AS Bad scan !AS',
	1                       HA,IPC(IXY),GPTXT(IGP))
		      FLGERR(IGP,IXY)=.FALSE.   !INDICATE ERROR
		    END IF                      
		  END IF                        
		END DO                          
!                                               
C                                               
C RESET ERROR                                   
C                                               
		IF (APSOL(0) .AND. APSOL(1)) THEN
						!COMPLEX ERRORS
		  IF (.NOT.FLGERR(0,IXY) .OR. .NOT.FLGERR(1,IXY)) THEN
		    FLGERR(0,IXY)=.FALSE.       !BOTH
		    FLGERR(1,IXY)=.FALSE.       
		    IF (.NOT.DOALG) THEN        
		      CALL NCARCC(MAR(0),STHJ(STH_NIFR_J),IFRT,BASEL,
	1               NDEG(0,0,IXY),IRED(0),DWGT(0,IXY),AWGT(0,IXY),
	1               CDAT(0,IXY),DAMP(0,IXY),DPHAS(0,IXY),
	1               CAMOD(0,3*IXY),RAWGT(0),
	1               CSOL(0,0,IXY),SOL(0,IXY,0),MU,ME,ARMS(0,IXY),
	1               JAV(0,0,0,IXY),EAV(0,0,0,IXY),DAV(0,0,0,IXY))
		    ELSE IF (DOSCAL) THEN       !SELFCAL
		      CALL NCASCC(MAR(0),STHJ(STH_NIFR_J),IFRT,BASEL,
	1               NDEG(0,0,IXY),IRED(0),DWGT(0,IXY),AWGT(0,IXY),
	1               CDAT(0,IXY),DAMP(0,IXY),DPHAS(0,IXY),
	1               CAMOD(0,3*IXY),RAWGT(0),
	1               CSOL(0,0,IXY),SOL(0,IXY,0),MU,ME,ARMS(0,IXY),
	1               JAV(0,0,0,IXY),EAV(0,0,0,IXY),DAV(0,0,0,IXY))
		    ELSE                        !ALIGN
		      CALL NCAACC(MAR(0),STHJ(STH_NIFR_J),IFRT,BASEL,
	1               NDEG(0,0,IXY),IRED(0),DWGT(0,IXY),AWGT(0,IXY),
	1               CDAT(0,IXY),DAMP(0,IXY),DPHAS(0,IXY),
	1               CAMOD(0,3*IXY),RAWGT(0),
	1               CSOL(0,0,IXY),SOL(0,IXY,0),MU,ME,ARMS(0,IXY),
	1               JAV(0,0,0,IXY),EAV(0,0,0,IXY),DAV(0,0,0,IXY))
		    END IF                      
		  END IF                        
		ELSE                            
		  IF (.NOT.FLGERR(0,IXY)) THEN  
		    IF (.NOT.DOALG) THEN        
		      CALL NCARGC(MAR(0),STHJ(STH_NIFR_J),IFRT,BASEL,
						!GAIN ERR
	1               NDEG(0,0,IXY),IRED(0),DWGT(0,IXY),AWGT(0,IXY),
	1               CDAT(0,IXY),DAMP(0,IXY),DPHAS(0,IXY),
	1               CAMOD(0,3*IXY),RAWGT(0),
	1               CSOL(0,0,IXY),SOL(0,IXY,0),MU,ME,ARMS(0,IXY),
	1               JAV(0,0,0,IXY),EAV(0,0,0,IXY),DAV(0,0,0,IXY))
		    ELSE IF (DOSCAL) THEN       !SELFCAL
		      CALL NCASGC(MAR(0),STHJ(STH_NIFR_J),IFRT,BASEL,
						!GAIN ERR
	1               NDEG(0,0,IXY),IRED(0),DWGT(0,IXY),AWGT(0,IXY),
	1               CDAT(0,IXY),DAMP(0,IXY),DPHAS(0,IXY),
	1               CAMOD(0,3*IXY),RAWGT(0),
	1               CSOL(0,0,IXY),SOL(0,IXY,0),MU,ME,ARMS(0,IXY),
	1               JAV(0,0,0,IXY),EAV(0,0,0,IXY),DAV(0,0,0,IXY))
		    ELSE                        !ALIGN
		      CALL NCAAGC(MAR(0),STHJ(STH_NIFR_J),IFRT,BASEL,
						!GAIN ERR
	1               NDEG(0,0,IXY),IRED(0),DWGT(0,IXY),AWGT(0,IXY),
	1               CDAT(0,IXY),DAMP(0,IXY),DPHAS(0,IXY),
	1               CAMOD(0,3*IXY),RAWGT(0),
	1               CSOL(0,0,IXY),SOL(0,IXY,0),MU,ME,ARMS(0,IXY),
	1               JAV(0,0,0,IXY),EAV(0,0,0,IXY),DAV(0,0,0,IXY))
		    END IF                      
		  END IF                        
		  IF (.NOT.FLGERR(1,IXY)) THEN  
		    IF (.NOT.DOALG) THEN        
		      CALL NCARPC(MAR(1),STHJ(STH_NIFR_J),IFRT,BASEL,
						!PHASE ERR
	1               NDEG(1,0,IXY),IRED(0),DWGT(0,IXY),AWGT(0,IXY),
	1               CDAT(0,IXY),DAMP(0,IXY),DPHAS(0,IXY),
	1               CAMOD(0,3*IXY),RAWGT(0),
	1               CSOL(0,0,IXY),SOL(0,IXY,1),MU,ME,ARMS(0,IXY),
	1               JAV(0,0,0,IXY),EAV(0,0,0,IXY),DAV(0,0,0,IXY))
		    ELSE IF (DOSCAL) THEN       !SELFCAL
		      CALL NCASPC(MAR(1),STHJ(STH_NIFR_J),IFRT,BASEL,
						!PHASE ERR
	1               NDEG(1,0,IXY),IRED(0),DWGT(0,IXY),AWGT(0,IXY),
	1               CDAT(0,IXY),DAMP(0,IXY),DPHAS(0,IXY),
	1               CAMOD(0,3*IXY),RAWGT(0),
	1               CSOL(0,0,IXY),SOL(0,IXY,1),MU,ME,ARMS(0,IXY),
	1               JAV(0,0,0,IXY),EAV(0,0,0,IXY),DAV(0,0,0,IXY))
		    ELSE                        !ALIGN
		      CALL NCAAPC(MAR(1),STHJ(STH_NIFR_J),IFRT,BASEL,
						!PHASE ERR
	1               NDEG(1,0,IXY),IRED(0),DWGT(0,IXY),AWGT(0,IXY),
	1               CDAT(0,IXY),DAMP(0,IXY),DPHAS(0,IXY),
	1               CAMOD(0,3*IXY),RAWGT(0),
	1               CSOL(0,0,IXY),SOL(0,IXY,1),MU,ME,ARMS(0,IXY),
	1               JAV(0,0,0,IXY),EAV(0,0,0,IXY),DAV(0,0,0,IXY))
		    END IF                      
		  END IF                        
		END IF                          
C                                               
C SET HISTOGRAMS                                
C                                               
!= I=ISCN I2=IGP                                 
		DO IGP=0,1                      !G,P
		  IF (FLGERR(IGP,IXY)) THEN     
		    CALL WNMHS1(HISBAD(IGP,IXY),1,EAV(0,0,IGP,IXY))
		  END IF                        
		END DO                          
!                                               
		CALL WNMHS1(HISBAD(2,IXY),1,ARMS(0,IXY))
	      END IF                            
	    END DO                              ! IXY polarisation loop X, Y
C                                               
C WRITE RESULTS: corrections in scan header     
C                                               
!= I=ISCN I1=IXY I2=IGP                          
	    DO IXY=0,1                          !X,Y
	      DO IGP=0,1                        !G,P
		MENS(IGP,IXY)=EAV(0,0,IGP,IXY)  !SAVE SCAN NOISES
	      END DO                            
	    END DO                              
!                                               
	    IF (DOALG) THEN                     !SELECT SELFCAL
	      I1=CAP_ALG                        ! 2                              
	      I2=0                              !NOTHING ZEROED
	    ELSE                                !SELECT REDUN
	      I1=CAP_RED                        ! 1                              
	      I2=CAP_ALG                        ! 2                    
	    END IF                              
!= I=ISCN                                       
	    DO I3=ISCN,ISCN+NRINT-1   		!WRITE INTEGRATED
	      IF (.NOT.NSCSWU(FCAOUT,STH,I3,CSOL,I1,APSOL,XYSOL,TELS,
	1                       CORAP,CORDAP,I2,MENS)) THEN
		CALL WNCTXT(F_TP,'!7$EAF7.2 Error writing scan data',HA)
		GOTO 20                         !TRY NEXT sector
	      END IF                            
	    END DO                              
!                                              
C                                               
 30         CONTINUE                            
	  END DO! iscn: scans
 20       CONTINUE!                               
	END DO! sectors
C                                               
C SHOW OVERALL INFO                             
C                                               
	CALL WNCFHD(F_TP,4,' ')                 !RESET HEADERS
	CALL WNCFHD(F_TP,5,' ')                 
	CALL WNCTXT(F_TP,' ')                   
C                                               
C PRINT AVERAGES                                
C                 
	IF (APSOL(0) .OR. APSOL(1)) THEN
	   DO IXY=0,1		!BOTH POL.
	      IF (JAV(7,0,0,IXY).GT.0) THEN !DATA PRESENT
		 CALL WNCTXT(F_TP,'!AS average amplitude= !E10.3 (!D10.3)',
	1	      IPC(IXY),EAV(7,0,0,IXY)/JAV(7,0,0,IXY),
	1	      SQRT(ABS(DAV(7,0,0,IXY)-EAV(7,0,0,IXY)*
	1	      EAV(7,0,0,IXY)/JAV(7,0,0,IXY))/JAV(7,0,0,IXY)))
	      END IF                                
	   END DO                                  
	   CALL WNCTXT(F_TP,'!/!Q1!7C!5$#AS',STHTEL,TELNMA(0))
				!HEADING
!= I1=ITL I2=IGP                                 
	   DO IXY=0,1		!BOTH POL.
	      DO ITL=0,STHTEL-1	!ALL TEL.
		 DO IGP=0,1	!GAIN/PHASE
		    IF (JAV(ITL,3,IGP,IXY).GT.0) THEN !PRESENT
		       EAV(ITL,3,IGP,IXY)=EAV(ITL,3,IGP,IXY)/JAV(ITL,3,IGP,IXY)
				!AVERAGE
		       DAV(ITL,3,IGP,IXY)=             
	1		    SQRT(ABS(DAV(ITL,3,IGP,IXY)
	1		    -JAV(ITL,3,IGP,IXY)     
	1		    *EAV(ITL,3,IGP,IXY)*EAV(ITL,3,IGP,IXY))
	1		    /JAV(ITL,3,IGP,IXY))    
				!RMS
		       IF (IGP.EQ.0) THEN !GAIN
			  EAV(ITL,3,IGP,IXY)=100*(EXP(EAV(ITL,3,IGP,IXY))-1)
			  DAV(ITL,3,IGP,IXY)=100*(EXP(DAV(ITL,3,IGP,IXY))-1)
		       ELSE	!PHASE
			  EAV(ITL,3,IGP,IXY)=DEG*EAV(ITL,3,IGP,IXY)
			  DAV(ITL,3,IGP,IXY)=DEG*DAV(ITL,3,IGP,IXY)
		       END IF                          
		    END IF                            
		 END DO                              
	      END DO                                
!                                               
	      IF (XYSOL(IXY)) THEN !TO DO
		 DO ITL=0,STHTEL-1                   
		    EAV0(ITL)=NINT(10*EAV(ITL,3,0,IXY))
		    DAV0(ITL)=NINT(10*DAV(ITL,3,0,IXY))
		 ENDDO                               
		 CALL WNCTXT(F_TP,'!Q1!AS\g(.1%) !5$#SJ5!/!2C\(M.E.) !5$#SJ5',
	1	      IPC(IXY),STHTEL,EAV0(0),STHTEL,DAV0(0))
		 DO ITL=0,STHTEL-1                   
		    EAV0(ITL)=NINT(10*EAV(ITL,3,1,IXY))
		    DAV0(ITL)=NINT(10*DAV(ITL,3,1,IXY))
		 ENDDO                               
		 CALL WNCTXT(F_TP,'!Q1!AS\p(.1d) !5$#SJ5!/!2C\(M.E.) !5$#SJ5',
	1	      IPC(IXY),STHTEL,EAV0(0),STHTEL,DAV0(0))
	      END IF                                
	   END DO		! IXY
	END IF
C                                               
C CALCULATE AVERAGES                            
C                                               
!= I1=IFR I2=IGP                                 
	DO IXY=0,1                              !BOTH POL.
	  I4=0                                  !COUNT
	  DO IFR=0,STHIFR-1                     !ALL IFRS
	    IF (JAV(IFR,1,0,IXY).GT.0 .OR. JAV(IFR,1,1,IXY).GT.0) THEN
	      DO IGP=0,1                        !GAIN/PHASE
		IF (JAV(IFR,1,IGP,IXY).GT.0 .AND.
	1		DAV(IFR,4,IGP,IXY).GT.0) THEN !980701
						!PRESENT
		  JAV(I4,2,IGP,IXY)=IFR         !BASELINE
		  DAV(I4,1,IGP,IXY)=DAV(IFR,1,IGP,IXY)/JAV(IFR,1,IGP,IXY)
						!AVER. ERROR
		  DAV(I4,2,IGP,IXY)=SQRT(MAX(0D0,DAV(IFR,2,IGP,IXY)/
	1                       JAV(IFR,1,IGP,IXY)-DAV(I4,1,IGP,IXY)**2))
		  IF (IGP.EQ.0) THEN            !GAIN
C!980701		    EAV(I4,1,IGP,IXY)=100*(EXP(EAV(IFR,1,IGP,IXY)/
C!980701	1                       JAV(IFR,1,IGP,IXY))-1)
C!070814	    EAV(I4,1,IGP,IXY)=100*(EXP(EAV(IFR,4,IGP,IXY)/  !980701
C!070814	1                       DAV(IFR,4,IGP,IXY))-1) !980701
		    EAV(I4,1,IGP,IXY)=100*(EXP(EAV(IFR,4,IGP,IXY)/ !091019
	1		DAV(IFR,4,IGP,IXY))-1)                     !091019
C!091019	    C0=CMPLX(EAV(IFR,4,IGP,IXY)/DAV(IFR,4,IGP,IXY),
C!091019	1		    EAV(IFR,4,1,IXY)/DAV(IFR,4,1,IXY)) !070814
C!091019	    IF (ABS(C0).NE.0) THEN                          !070814
C!091019	      EAV(IFR,4,IGP,IXY)=100*REAL(LOG(C0))          !070814
C!091019	      EAV(IFR,4,1,IXY)=DEG*AIMAG(LOG(C0)) !070814
C!091019	    ELSE
C!091019	      EAV(IFR,4,IGP,IXY)=0                          !070814
C!091019	      EAV(IFR,4,1,IXY)=0                            !070814
C!091019	    END IF
		  ELSE                          !PHASE
C!980701	EAV(I4,1,IGP,IXY)=DEG*EAV(IFR,1,IGP,IXY)/JAV(IFR,1,IGP,IXY)
C!070814	    EAV(I4,1,IGP,IXY)=DEG*EAV(IFR,4,IGP,IXY)/DAV(IFR,4,IGP,IXY)
		    EAV(I4,1,IGP,IXY)=DEG*EAV(IFR,4,IGP,IXY)/DAV(IFR,4,
	1		IGP,IXY)                                    !091019
		  END IF                        
		  EAV(I4,2,IGP,IXY)=DAV(I4,2,IGP,IXY)
		  EAV(I4,3,IGP,IXY)=DAV(I4,1,IGP,IXY)
		ELSE                            
		  JAV(I4,2,IGP,IXY)=IFR         
		  EAV(I4,1,IGP,IXY)=0           
		  EAV(I4,2,IGP,IXY)=0           
		  EAV(I4,3,IGP,IXY)=0           
		END IF                          
	      END DO                            
	      I4=I4+1                           !COUNT
	    END IF                              
	  END DO                                
	  JAV(0,1,0,IXY)=I4                     !SAVE COUNT
	END DO                                  
!                                               
	CALL WNCTXT(F_TP,' ')                   
	DO IXY=0,1                              
	  IF (JAV(2,0,0,IXY)+JAV(2,0,1,IXY).GT.0) THEN
!= I2=IGP                                        
	    DO IGP=0,1                          
	      IF (JAV(2,0,IGP,IXY).GT.0)        
	1               EAV(2,0,IGP,IXY)=SQRT(EAV(2,0,IGP,IXY)/JAV(2,0,IGP,IXY))
	    END DO                              
!                                               
	  END IF                                
	END DO                                  
C                                               
C SAVE MIFR ERRORS                              
C                                               
	IF (DOMIFR) THEN                        
	   CALL WNGMVZ(4*LB_X*STHIFR,IFRCOR)    !EMPTY CORRECTIONS
	   DO IXY=0,1                           !X,Y
	      I=-1                              
!= I1=ITL I2=ITL1                                
	      DO ITL=0,STHTEL-1                 
		 DO ITL1=ITL,STHTEL-1         
		    I=I+1                    !NEXT ENTRY IFRCOR
		    IF (CIFRS(ITL,ITL1)) THEN   !WRITE SELECTED
		       I4=0                     !INDEX INPUT
		       DO WHILE (I4.LT.JAV(0,1,0,IXY))
						!FIND IFR INDEX
			  IF (IFRT(JAV(I4,2,0,IXY)).EQ.ITL*256+ITL1 .OR.
	1                      IFRT(JAV(I4,2,0,IXY)).EQ.ITL1*256+ITL) THEN
						!FOUND
			     R0=EAV(I4,1,0,IXY) !GAIN (%)
			     R0=LOG(ABS(R0/100+1))
						!GAIN
			     R1=EAV(I4,1,1,IXY)/DEG
						!PHASE
			     IFRCOR(I,3*IXY)=CMPLX(R0,R1)
						!FOR EXP(-CMPLX)
			     GOTO 60            
			  END IF                
			  I4=I4+1               
		       END DO                   
 60                    CONTINUE                 
		    END IF                      
		 END DO                         
	      END DO                            
!                                               
	   END DO                               !X,Y
C                                               
C SWAP SIFRS AND CIFRS FOR USE BY NCASTX        
C                                               
!= I1=ITL I2=ITL1                                
	   DO ITL=0,STHTEL-1                    
	      DO ITL1=ITL,STHTEL-1            
		 BTMP=SIFRS(ITL,ITL1)           
		 SIFRS(ITL,ITL1)=CIFRS(ITL,ITL1)
		 CIFRS(ITL,ITL1)=BTMP           
	      END DO                            
	   END DO                               
	   CALL NCASTX(COR_MIFR)                !SAVE CORRECTIONS
	   DO ITL=0,STHTEL-1                    
	      DO ITL1=ITL,STHTEL-1            
		 BTMP=SIFRS(ITL,ITL1)           
		 SIFRS(ITL,ITL1)=CIFRS(ITL,ITL1)
		 CIFRS(ITL,ITL1)=BTMP           
	      END DO                            
	   END DO                               
!                                               
	END IF                                  
C                                               
C SAVE OVERALL NOISE                            
C                                               
	DO WHILE(NSCSTL(FCAOUT,SETS,STH(0),STHP,SETNAM,LPOFF))
						!NEXT sector
	  DO IXY=0,1                            !POL.
	    IF (XYSOL(IXY)) THEN                !TO DO
!= I1=IGP                                        
	      DO IGP=0,1                        !GAIN/PHASE
		IF (APSOL(IGP)) THEN            !TO DO
		  I4=IXY*2+IGP                  
		  IF (.NOT.DOALG) THEN          
		    STHE(STH_REDNS_E+I4)=EAV(2,0,IGP,IXY)
						!SET REDUN
		  ELSE                          
		    STHE(STH_ALGNS_E+I4)=EAV(2,0,IGP,IXY)
						!SET SELFCAL
		  END IF                        
		END IF                          
	      END DO                            
!                                               
	    END IF                              
	  END DO                                
	  IF (.NOT.WNFWR(FCAOUT,STH__L,STH(0),STHP)) THEN
						!RESET HEADER
	    CALL WNCTXT(F_TP,'Error rewriting sector header')
	  END IF                                
	END DO                                  
	DO IXY=0,1                              
	 IF (JAV(2,0,0,IXY)+JAV(2,0,1,IXY).GT.0) THEN
	  CALL WNCTXT(F_TP,'!AS overall noise '//
	1               '(gain, phase in W.U.): !8$E8.1 !8$E8.1',
	1               IPC(IXY),EAV(2,0,0,IXY),EAV(2,0,1,IXY))
	 END IF                                 
	END DO                                  
	CALL WNCTXT(F_TP,' ')                   
C                                               
C PRINT GRAPHICS                                
C                                               
	IF (SHLV(2).NE.0 .AND. 
	1	(APSOL(0).NE.0 .OR. APSOL(1).NE.0)) THEN
  	  DO IXY=0,1                              !X,Y
	     IF (XYSOL(IXY)) THEN                 !TO DO
	        CALL WNCFHD(F_P,4,'Polarisation:!AS',IPC(IXY))
	        CALL WNCFHD(F_P,5,'Overall noise (gain, phase in W.U.): '//
	1          '!8$E8.1!8$E8.1',EAV(2,0,0,IXY),EAV(2,0,1,IXY))
	        CALL WNCTXT(F_P,'!^')           
	        CALL NCARGR('Average residual error '//
	1          IPC(IXY)//' (W.U.)',JAV(0,2,0,IXY),IFRT,
	1          EAV(0,3,0,IXY),EAV(0,3,1,IXY),
	1          JAV(0,1,0,IXY),APSOL)        
	        CALL NCARGR('Average residual error '//
	1          IPC(IXY)//' (%, deg)',JAV(0,2,0,IXY),IFRT,
	1          EAV(0,1,0,IXY),EAV(0,1,1,IXY),
	1          JAV(0,1,0,IXY),APSOL)        
	        CALL NCARGR('RMS '//IPC(IXY)//' (W.U.)',JAV(0,2,0,IXY),IFRT,
	1          EAV(0,2,0,IXY),EAV(0,2,1,IXY),
	1          JAV(0,1,0,IXY),APSOL)
 		CALL WNMHS2(HISBAD(0,IXY),3,SHLV(2))  !PRINT HISTOGRAM
	        CALL WNCTXT(F_P,
	1		'+= amplitude noise, *= phase noise, o= max. amplitude')
 	     ENDIF                               
	  END DO                                  
	ENDIF                 
C                                               
C READY                                         
C                                               
 900    CONTINUE                                
	CALL WNCFHD(F_P,-4,' ')                 !CLEAR HEADERS
	CALL WNCFHD(F_P,-5,' ')                 
	CALL WNCFHD(F_P,-6,' ')
 902    CONTINUE                                
!= I=IGP                                         
	DO IGP=0,1                              
	   CALL WNMLFA(MAR2(IGP))               
	   CALL WNMLFA(CMAR2(IGP))              
	END DO                                  
	DO I2=0,1                               
	  DO I1=0,2                             
	    CALL WNMHS9(HISBAD(I1,I2))          !CLEAR HISTO
	  END DO                                
	END DO                                  
 901    CONTINUE                                
	DO IGP=0,1                              !FREE MATRICES
	   CALL WNMLFA(MAR(IGP))                
	   CALL WNMLFA(MAR1(IGP))               
	END DO                                  
!                                               
	CALL WNMLFA(CMAR)                       
C                                               
	RETURN                                  !READY
C                                               
C                                               
	END
