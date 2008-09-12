C+ NPLRES.FOR                                   
C  WNB 910618                                   
C                                               
C  Revisions:                                   
C       WNB 911025      Change ampl/phase residuals
C       WNB 911217      Change to WQ            
C       WNB 920130      Multipage change        
C       WNB 920403      Rearrange pol. test     
C       WNB 920811      Change R+A+O text (Unix problem)
C       WNB 920831      Add PLUVO               
C       WNB 920902      Typo in weight check    
C       WNB 921104      Cater for large HA      
C       HjV 930311      Change some text        
C       WNB 930608      New flags               
C       HJV 930618      Symbolic names for mask bits in CBITS_O_DEF
C       HjV 930722      Add IFR_MODE (sort baselines)
C       WNB 930803      CBITS_DEF               
C       WNB 930825      Add dipole positions; polarisation codes
C       WNB 930826      New model calculation; redundant baselines
C       WNB 931008      Add MINST               
C       HjV 940112      Removed some parts to new subroutines,
C                       Add 'PLOTAP' plots, more plots per page
C       HjV 940224      Add mosaik test, add filling of FNAM and OBSYD
C       HjV 940324      Better check for MOSAIK with BANDPASS
C       HjV 940413      Changed check for opening plot
C       HjV 940506      Changed place for opening plot
C       CMV 940822      Option to abort during loop of plots
C       HjV 950503      Plot all PHASE residuals in W.U.
C       HjV 950522      Set PLTIFR to false for every set
C       JPH 960411      ST_MODE: plotting versus ST i.s.o. HA
C       HjV 960415      Correct option to stop during loop or more plots per
C                        page                   
C       JPH 960619      ST_MODE integer, copy to ST_INIT. - Report progress
C       JPH 960622      Acknowledge control-C   
C       JPH 960726      SETNAM in NPLSST call.  
C                       Check CC after each scan
C                       Comments                
C       JPH 960730      AGAIN, PGAIN data types 
C       JPH 860805      ST_INIT in COMMON. Convert loop-index variable names
C       JPH 960806      Fix an index variable
C	JPH 960814	Use I i.s.o. ICS for WNGCCN argument
C	JPH 970124	Call NPLPBE(.FALSE.,...) immediately after 
C	 		 NPLPBE(.TRUE.,...) to draw raster first
C	HjV 970723	Add PLUVO-check in HA-integration part.
C			Remove control-C stuff (commented out with CCC)
C                                               
C                                               
	SUBROUTINE NPLRES (LDATTP,IPOL,NPLOT,PTXT,NHV,TABIFR,SCHAN,IFRS)
C                                               
C  Fill buffers for plotting residual errors of individual scans
C                                               
C  Result:                                      
C                                               
C       CALL NPLRES             Fill buffers for plotting residual errors
C                                               
C  Include files:                               
C                                               
	INCLUDE 'WNG_DEF'                       
	INCLUDE 'CBITS_DEF'                     
	INCLUDE 'STH_O_DEF'                     ! SET HEADER
	INCLUDE 'SCH_O_DEF'                     ! SCAN HEADER
	INCLUDE 'NPL_DEF'                       
C                                               
C  Parameters:                                  
C                                               
	INTEGER LDATTP                          ! CURRENT DATA TYPE
	INTEGER IPOL                            
	INTEGER NPLOT                           ! # OF INTERFEROMETERS TO DO
	INTEGER PTXT(MXNCHN)                    ! CROSS CHANNELS
	INTEGER NHV(0:1)                        ! # OF PAGES
	INTEGER TABIFR(0:STHTEL-1,0:STHTEL-1)   ! IFR PLOT POINTERS
	INTEGER SCHAN(0:MXNCHN-1)               ! CHANNELS TO DO
	INTEGER IFRS(0:1)                       ! CURRENT IFR'S'
C                                               
C  Arguments:                                   
C                                               
C                                               
C  Function references:                         
C                                               
	LOGICAL NSCSIF                          ! READ IFR TABLE
	LOGICAL NSCSTL                          ! GET A SET
	LOGICAL NSCSCR                          ! READ SCAN DATA
	LOGICAL NSCSCI                          ! READ SCAN CORRECTIONS
	LOGICAL NMORDH                          ! READ MODEL HEADER
	CHARACTER*32 WNTTSG                     ! character sector name
CCC	INTEGER WNGCCN                          ! check control-C interrupts
	REAL WNGENR                             ! convert angle
C                                               
C  Data declarations:                           
C                                               
	LOGICAL OPENSW                          ! PLOT OPEN OR CLOSED
	INTEGER NCHAN                           ! # CHANNELS TO DO
	INTEGER UFL                             ! FLAGS TO DISCARD
	INTEGER*2 IFRT(0:STHIFR-1)              ! IFR TABLE
	INTEGER IFRA(0:1,0:STHIFR-1)            
	REAL ANG(0:2,0:STHIFR-1)                
	REAL BASEL(0:STHIFR-1)                  ! BASELINES
	REAL HAB                                ! loc. copy of HAB or ST
	INTEGER BTEL(0:1,0:STHIFR-1)            ! TEL. TO PLOT
	INTEGER IRED(0:STHIFR-1)                ! REDUNDANT INDICATORS
	REAL WGT(0:STHIFR-1,0:3)                ! DATA WEIGHT
	REAL DAT(0:1,0:STHIFR-1,0:3)            ! DATA
	  COMPLEX CDAT(0:STHIFR-1,0:3)          
	  EQUIVALENCE (DAT,CDAT)                
	INTEGER STP                             ! SOURCE TYPE
	DOUBLE PRECISION SRA,SDEC,SFRQ          ! MODEL INFO
	REAL UV0(0:3)                           ! BASIC UV COORDINATES
	REAL LM0(0:1)                           ! BASIC SOURCE DISPLACEMENT
	DOUBLE PRECISION FRQ0                   ! BASIC FREQUENCY
	REAL TF(0:1)                            ! INTEGR. TIME, BANDWIDTH
	INTEGER MINST                           ! INSTRUMENT
	COMPLEX CMOD(0:3,0:STHIFR-1)            ! SOURCE MODEL I,Q,U,V
	COMPLEX CAMOD(0:STHIFR-1,0:3)           ! XYX SOURCE MODEL
	REAL MWGT(0:STHIFR-1)                   ! CELESTIAL WEIGHTS
	REAL LHA                                ! LOCAL HA
	REAL PHASE                              ! PHASE CORRECTION
	  REAL AMPL                             ! AMPLITUDE CORRECTION
	  REAL VAL(0:1)                         
	  COMPLEX CVAL                          
	  EQUIVALENCE (CVAL,VAL)                
	  EQUIVALENCE (AMPL,VAL(0))             
	  EQUIVALENCE (PHASE,VAL(1))            
	COMPLEX CRES                            ! DATA MODEL
	  REAL RRES(0:1)                        
	  EQUIVALENCE (CRES,RRES)               
	INTEGER NPOL                            ! # OF POLAR.
	INTEGER NTINT				! # of scans to integrate
	INTEGER STHP                            ! SET HEADER POINTER
	BYTE STH(0:STH__L-1)                    ! SET HEADER
	  INTEGER*2 STHI(0:STH__L/LB_I-1)       
	  INTEGER STHJ(0:STH__L/LB_J-1)         
	  REAL STHE(0:STH__L/LB_E-1)            
	  DOUBLE PRECISION STHD(0:STH__L/LB_D-1)
	  EQUIVALENCE (STH,STHI,STHJ,STHE,STHD) 
	BYTE SCH(0:SCH__L-1)                    ! SCAN HEADER
	  INTEGER*2 SCHI(0:SCH__L/LB_I-1)       
	  INTEGER SCHJ(0:SCH__L/LB_J-1)         
	  REAL SCHE(0:SCH__L/LB_E-1)            
	  DOUBLE PRECISION SCHD(0:SCH__L/LB_D-1)
	  EQUIVALENCE (SCH,SCHI,SCHJ,SCHE,SCHD) 
	REAL AMCO(2,0:STHIFR-1,0:MXNCHN-1)      ! DATA VALUES PER INTERF. PER HA
	INTEGER NRAMCO(0:STHIFR-1,0:MXNCHN-1)   ! # OF POINTS IN AMCO FOR
							! BAND OPTION
	REAL TMPVAL(2)                          ! TEMP. A/P OR C/S
	LOGICAL PLTIFR(0:STHIFR-1)              ! .TRUE.= PLOT THIS IFR
	INTEGER CSET(0:7,0:1)                   ! TEST SET NAMES
	LOGICAL REPORT                          ! 'new plot' flag
	INTEGER ICH, ICS, IFR, ISCN,ISCN0,ISCN1	! loop indices
	INTEGER N				! integration counter
	REAL SUM				! accumulator
	INTEGER	PLTPTR, REDPTR			! data pointers
C-                                              
C                                               
	CALL WNDDUF(UFL)                        ! GET UNFLAG DATA
	UFL=IAND(FL_ALL,NOT(UFL))               ! SELECTOR
C                                               
C  INIT PLOT                                    
C                                               
	ST_INIT=ST_MODE                         
	NCHAN=0                                 
	CSET(0,0)=-1                            
	CSET(0,1)=-1                            
	OPENSW=.FALSE.                          
	IF (IFR_MODE.EQ.'BAND') THEN            
!= I1=ICH I2=IFR                                
	  DO ICH=0,MXNCHN-1                     
							! CLEAR
	    DO IFR=0,STHIFR-1                   
	      AMCO(1,IFR,ICH)=1E20              
	      AMCO(2,IFR,ICH)=1E20              
	    END DO                              
	  END DO                                
	END IF                                  
!=                                              
C                                               
C PLOT SETS                                     
C                                               
	REPORT=.TRUE.                           
	DO WHILE(NSCSTL(FCAIN,SETS,STH(0),STHP,SETNAM,LPOFF))
							! NEXT SET
	  CALL WNDSTI(FCAIN,SETNAM)             ! PROPER NAME
	  IF (REPORT) CALL WNCTXT(F_T,'Next plot, first sector: !AS',
	1       WNTTSG(SETNAM,0) )              
	  REPORT=.FALSE.                        
	  CALL WNGMTS(STH_FIELD_N,STH(STH_FIELD_1),FNAM)
							! SET FIELD NAME
	  IF (ST_MODE.NE.0) THEN                
	    CALL NPLSST(STHD,STHE(STH_HAB_E),SETNAM,HAB)
	  ELSE                                  
	    HAB=STHE(STH_HAB_E)                 
	  ENDIF                                 
	  OBSDY(1)=STHI(STH_OBS_I)              ! OBS. DAY
	  OBSDY(2)=STHI(STH_OBS_I+1)            ! OBS. YEAR
!= I2=IFR                                       
	  DO IFR=0,STHIFR-1                     
	    PLTIFR(IFR)=.FALSE.                 
	  END DO                                
C                                               
C GET IFR TABLES/MODEL                          
C                                               
	  IF (.NOT.NSCSIF(FCAIN,STH,IFRT,IFRA,ANG)) THEN
						! READ IFR TABLE
	    CALL WNCTXT(F_TP,'Read error IFR table')
	    GOTO 51                             
	  END IF                                
	  NPOL=STHI(STH_PLN_I)                  ! # OF POL.
	  IF (NSRC(0).GT.0) THEN                ! MODEL WANTED
	    IF (.NOT.NMORDH                     
	1       (6,STP,SRA,SDEC,SFRQ)) GOTO 51  ! NEXT SET
	    CALL NMOMST                         
	1       (STP,SRA,SDEC,STH,LM0,FRQ0,TF,MINST)
						! GET SOME DATA
	  ELSE IF (OPT.EQ.'RES'.AND.NSRC(0).EQ.0) THEN
						! REDUNDANT DATA
	    CALL NSCMBL(STHE(STH_RTP_E),        
	1       STHJ(STH_NIFR_J),IFRT,SIFRS,BASEL)
						! MAKE BASELINES
	    CALL NCARRT                         
	1       (STHJ(STH_NIFR_J),BASEL,1E0,IRED,ANG)
						! GET REDUNDANT
	  END IF                                
C                                               
C DO ALL SCANS                                  
C                                               
	  IF (IFR_MODE.NE.'BAND') THEN          
	    NCHAN=NCHAN+1                       
!= I1=ICH I2=IFR                                
	    DO ICH=0,MXNCHN-1                   ! CLEAR
	      DO IFR=0,STHIFR-1                 
		AMCO(1,IFR,ICH)=1E20            
		AMCO(2,IFR,ICH)=1E20            
	      END DO                            
	    END DO                              
!= I2=IFR                                       
	  ELSE! ifr_mode=band                                  
	    IF (MOSAIK) THEN                    
	      IF (CSET(0,0).EQ.-1) THEN         
		DO I1=0,7                       
		  CSET(I1,0)=SETNAM(I1)         
		END DO                          
	      END IF                            
	      IF (CSET(0,1).EQ.-1) THEN         
		NCHAN=NCHAN+1                   
		DO IFR=0,STHIFR-1               
		  NRAMCO(IFR,NCHAN-1)=0         
		END DO                          
	      ELSE                              
		IF ((CSET(0,1).NE.SETNAM(0)).OR.
	1             (CSET(1,1).NE.SETNAM(1)).OR.
	2             (CSET(2,1).NE.SETNAM(2)).OR.
	3             (CSET(3,1).NE.SETNAM(3))) THEN
		   NCHAN=NCHAN+1                
		   DO IFR=0,STHIFR-1            
		     NRAMCO(IFR,NCHAN-1)=0      
		   END DO                       
		END IF                          
	      END IF                            
	      DO I1=0,7                         
		CSET(I1,1)=SETNAM(I1)           
	      END DO                            
	    ELSE                                
	      NCHAN=NCHAN+1                     
	      DO IFR=0,STHIFR-1                 
		NRAMCO(IFR,NCHAN-1)=0           
	      END DO                            
	    END IF                              
	  END IF                                
 	  DO ISCN=0,STHJ(STH_SCN_J)-1           
							! ALL SCANS
	    LHA=HAB+ISCN*STHE(STH_HAI_E)        
							! HA
	    IF (LHA.LT.HARA(0) .OR. LHA.GT.HARA(1)) GOTO 50
							! NEXT SCAN
	    IF (OPT.EQ.'INT') THEN              ! GET CORRECTIONS
	       JS=NSCSCI(FCAIN,STH,IFRT,ISCN,CORAP,CORDAP,
	1                  SCH,WGT,DAT)         ! READ
	    ELSE                                ! GET CORRECTED DATA
	       JS=NSCSCR(FCAIN,STH,IFRT,ISCN,CORAP,CORDAP,
	1                  SCH,WGT,DAT)         ! READ
	    END IF                              
	    IF (.NOT.JS) THEN                   ! ERROR
	      CALL WNCTXT(F_TP,'Error reading scan')
	      GOTO 50                           ! NEXT SCAN
	    END IF                              
	    IF (IAND(SCHJ(SCH_BITS_J),UFL).NE.0) GOTO 50
						! DELETE SCAN
C                                               
C If model accessed, get it                     
C                                               
	    IF (NSRC(0).GT.0) THEN              ! GET SOURCE MODEL
	      CALL NMOMUV(STP,SRA,SDEC,STH,SCH,UV0)
						! GET UV DATA
	      CALL NMOMU4(0,FCAIN,ISCN,STH,UV0,LM0,FRQ0,
	1               STHE(STH_RTP_E),NPOL,STHJ(STH_NIFR_J),
	1               IFRT,TF,MINST,CMOD)     ! GET MODEL DATA
	      CALL NMOCIX(STHJ,SCHE,ANG,CAMOD,CMOD)
						! XYX in CAMOD
C                                               
C NOTE: In the following, NSCR(0) is used as a flag set by NPLDAT: <= 0 means
C  no model required, < 0 means INTERNAL model is used
C                                               
C Redundancy: The averages of redundant interferometer groups are used as
C  reference against which the residuals are calculated
C                                               
	    ELSE IF (OPT.EQ.'RES'.AND.NSRC(0).EQ.0) THEN
							! REDUNDANT
	      DO I1=0,STHJ(STH_NIFR_J)-1        ! ZERO CELESTIAL DATA
		CAMOD(I1,IPOL)=0                ! use CAMOD as buffer
		MWGT(I1)=0                      
	      END DO                            
 	      DO IFR=0,STHJ(STH_NIFR_J)-1       
		IF (IRED(IFR).GT.0) THEN        ! REDUNDANT
		  IF (WGT(IFR,IPOL).GT.0) THEN  ! CAN USE
		    REDPTR=IRED(IFR)            ! redundancy POINTER
		    CAMOD(REDPTR,IPOL)=CAMOD(REDPTR,IPOL)+
	1               WGT(IFR,IPOL)*CDAT(IFR,IPOL)
						! weighted sum
		    MWGT(REDPTR)=MWGT(REDPTR)+WGT(IFR,IPOL)
		  END IF                        
		END IF                          
	      END DO                            
	      DO IFR=0,STHJ(STH_NIFR_J)-1       ! make average
		IF (MWGT(IFR).GT.0) CAMOD(IFR,IPOL)=
	1               CAMOD(IFR,IPOL)/MWGT(IFR)
	      END DO                            
	    END IF                              
C                                               
C CAMOD now contains either the model or the redundancy averages.
C Now we fill the plot buffer                   
C                                               
 	    DO IFR=0,STHJ(STH_NIFR_J)-1         
	      IF ((.NOT.PLUVO .AND.             
	1           TABIFR(IFRA(0,IFR),IFRA(1,IFR)).GE.0 .AND.
	1           WGT(IFR,IPOL).GT.0) .OR.    
	1           (PLUVO .AND. IFRA(0,IFR).EQ.IFRS(0) .AND.
	1           IFRA(1,IFR).EQ.IFRS(1) .AND.
	1           WGT(IFR,IPOL).GT.0)) THEN   
		PLTIFR(IFR)=.TRUE.              	! PLOT THIS ONE
		IF (PLUVO) THEN                 
		  PLTPTR=SCHAN(SETNAM(3))       	! PLOT POINTER
		ELSE                            
		  PLTPTR=TABIFR(IFRA(0,IFR),IFRA(1,IFR))  ! PLOT POINTER
		END IF                          
C                                               
C First we copy the visibility to CVAL and the comparison reference to CRES
C                                               
		IF (OPT.EQ.'DAT'.OR. OPT.EQ.'INT' .OR.
						! visib., intfr corrns
	1          (OPT.EQ.'RES'.AND.NSRC(0).LT.0)) THEN
						!  or internal model
		  CVAL=CDAT(IFR,IPOL)           ! visibility
		  CRES=0                        ! no reference
C                                               
		ELSE IF (OPT.EQ.'MOD') THEN     
		  CVAL=CAMOD(IFR,IPOL)          ! model visib.
		  CRES=0                        ! no reference
C                                               
		ELSE IF (OPT.EQ.'RES') THEN     
		  CVAL=CDAT(IFR,IPOL)           ! visibility
C                                               
		  IF (NSRC(0).GT.0) THEN        ! model residual
		    CRES=CAMOD(IFR,IPOL)        ! model visib.
C                                               
		  ELSE IF (NSRC(0).EQ.0) THEN   ! redundancy residual
		    CRES=CAMOD(IRED(IFR),IPOL)  ! average of red. ifrs
C                                               
		  ELSE                          ! USED DEAPPLY=MODEL
		    CRES=0                      !  (JPH: ??)
		  END IF                        
		END IF                          
C                                               
C CVAL = AMPL + i PHASE contains visibility, CRES the reference
C                                               
		IF (OPT.EQ.'RES' .AND. NSRC(0).EQ.0 .AND.
							! skip non-redundant
	1               IRED(IFR).LE.0) THEN    ! ifr if redundancy
C                                               
		ELSE                            ! int. or ext. model
C                                               
C Plot-data selection for ampl/phase            
C                                               
		  IF ((DATTYP(LDATTP)(1:2).EQ.'AP').OR.
	1        (DATTYP(LDATTP)(1:1).EQ.'A').OR.
	1        (DATTYP(LDATTP)(1:1).EQ.'P')) THEN
						! ampl and/or phase plot
C                                               
C For all cases, ampl. is the same; phase is defined per case
C                                               
		    TMPVAL(1)=(ABS(CVAL)-ABS(CRES))
						! vis. ampl - ref. ampl.
C                                               
C Model residual: phase in WU = model ampl * arg (visib/model)
C                                               
		    IF (OPT.EQ.'RES'.AND.NSRC(0).GE.0) THEN
		      IF (DATTYP(LDATTP)(1:2).EQ.'AG' .OR.
							! instr. g/ph. resid.
	1                 DATTYP(LDATTP)(1:2).EQ.'PG') THEN
			CVAL=LOG(CVAL/CRES)     ! visib./model
			TMPVAL(1)=100*AMPL      ! gain
			R0=WNGENR(PHASE)*DEG    ! phase
		      ELSE                      
			R0=ABS(CRES)            ! model amplitude
			IF (R0.GT.1E-6) THEN    
			  CVAL=CVAL/CRES        ! visib./model =
							!  complex gain resid
			  R0=R0*ATAN2(PHASE,AMPL)
						! * model ampl.
			ELSE                    
			  R0=0                  ! model too weak
			ENDIF                   
		      END IF                    
C                                               
C Internal-model residual: differential model residual = visib. - model:
C  phase in WU = ampl * arg(res)                
C                                               
		    ELSE IF (OPT.EQ.'RES') THEN 
		      R0=ABS(CVAL)              ! abs (visib - model)
		      IF (R0.GT.1E-6) THEN      
			R0=R0*ATAN2(PHASE,AMPL) ! (JPH: looks dubious)
		      ELSE                      
			R0=0                    
		      END IF                    
C                                               
C Measured visibility or edundancy residual     
C                                               
		    ELSE                        
		      R0=ABS(CVAL)              ! visibility
		      IF (R0.GT.1E-6) THEN      
			R0=ATAN2(PHASE,AMPL)*DEG! true phase
		      ELSE                      
			R0=0                    
		      END IF                    
		    END IF                      
C                                               
		    TMPVAL(2)=R0                ! PHASE
C                                               
C                                               
C Data selection for cos/sin plots: Always visib - ref
C                                               
		  ELSE IF ((DATTYP(LDATTP)(1:2).EQ.'CS').OR.
	1             (DATTYP(LDATTP)(1:2).EQ.'CO').OR.
	1             (DATTYP(LDATTP)(1:2).EQ.'SI')) THEN
		    TMPVAL(1)=REAL(CVAL-CRES)   ! visib - reference
		    TMPVAL(2)=AIMAG(CVAL-CRES)  
		  END IF                        
		  IF (IFR_MODE.EQ.'BAND') THEN  
		    NRAMCO(PLTPTR,NCHAN-1)=NRAMCO(PLTPTR,NCHAN-1)+1
		    DO ICS=1,2                  
		      IF (AMCO(ICS,PLTPTR,NCHAN-1).EQ.1E20) THEN
			AMCO(ICS,PLTPTR,NCHAN-1)=TMPVAL(ICS)
		      ELSE                      
			AMCO(ICS,PLTPTR,NCHAN-1)=
	1		AMCO(ICS,PLTPTR,NCHAN-1)+TMPVAL(ICS)
		      ENDIF                     
		    END DO                      
		  ELSE				! mode#band
		    AMCO(1,PLTPTR,ISCN)=TMPVAL(1)
		    AMCO(2,PLTPTR,ISCN)=TMPVAL(2)
		  ENDIF                         
		END IF                          
	      END IF                            
	    END DO				! IFR
 50         CONTINUE                            
CCC	    I=WNGCCN()                        	! nr of control-C seen
CCC	    IF (I.GT.1) NO_MORE=.TRUE.        
CCC	    IF (I.NE.0) GOTO 501              
	  END DO				! scans
CCC 501      CONTINUE                              
	  IF (NO_MORE) RETURN
C
C HA integration
C                                          
	  IF (IFR_MODE.NE.'BAND') THEN          
	    NTINT=MAX(1,			! scans to integrate
	1	NINT(HAINT/24./3600./STHE(STH_HAI_E)))
 	    DO IFR=0,STHJ(STH_NIFR_J)-1		! new HA increment
	      IF (PLUVO) THEN                 
		 PLTPTR=SCHAN(SETNAM(3))       	! PLOT POINTER
	      ELSE                            
		 PLTPTR=TABIFR(IFRA(0,IFR),IFRA(1,IFR))  ! PLOT POINTER
	      END IF                          
	      DO ISCN0=0,STHJ(STH_SCN_J)-1,NTINT
		ISCN1=MIN(ISCN0+NTINT,STHJ(STH_SCN_J))-1
	        DO ICS=1,2
		  N=0
		  SUM=0
 	          DO ISCN=ISCN0,ISCN1
		    IF (AMCO(ICS,PLTPTR,ISCN).NE.1E20) THEN 	            
		      SUM=SUM+AMCO(ICS,PLTPTR,ISCN)
		      N=N+1
		    ENDIF
  	          ENDDO                 
		  IF (N.NE.0) THEN
	            SUM=SUM/N
 	            DO ISCN=ISCN0,ISCN1
	              AMCO(ICS,PLTPTR,ISCN)=SUM
 	            ENDDO
		  ENDIF
	        ENDDO
	      ENDDO
	    ENDDO
C
C PLOT PER IFR                                  
C
 	    IF (.NOT.OPENSW) THEN               
	      IF ((.NOT.MOSAIK.AND..NOT.PLOTAP).OR.
	1       ((MOSAIK.AND.(IFR_MODE.NE.'BAND'))))THEN
		CALL NPLOPN (LDATTP,IPOL,NHV,IFRS)
						! OPEN PLOT, PLOT HEADING
		IF (NO_MORE) RETURN             ! USER SAID: STOP
		CALL NPLPBE (.TRUE.,NPLOT,PTXT,NHV,NCHAN)
						! PLOT BEGIN-ANNOTATIONS
	        CALL NPLPBE (.FALSE.,NPLOT,PTXT,NHV,NCHAN)
						! PLOT END-ANNOTATIONS
		OPENSW=.TRUE.                   
	      END IF                            
	    END IF                              
	    CALL NPLPLT (LDATTP,IPOL,NPLOT,PTXT,NHV,TABIFR,SCHAN,IFRS,
	1      STHJ(STH_SCN_J),STHJ(STH_NIFR_J),HAB,
	1      STHE(STH_HAI_E),PLTIFR,IFRA,AMCO)
	    IF (NO_MORE) RETURN                 ! USER SAID: STOP
	  ELSE					! mode=band
C                                  
C CALCULATE AVERAGE                             
C
	    IF (.NOT.MOSAIK) THEN               
	      DO PLTPTR=0,STHJ(STH_NIFR_J)-1    
	       DO ICS=1,2                       
		IF ((AMCO(ICS,PLTPTR,NCHAN-1).NE.1E20).AND.
	1               (NRAMCO(PLTPTR,NCHAN-1).NE.0)) THEN
		 AMCO(ICS,PLTPTR,NCHAN-1)=
	1		AMCO(ICS,PLTPTR,NCHAN-1)/NRAMCO(PLTPTR,NCHAN-1)
		END IF                          
	       END DO                           
	      END DO                            
	    END IF                              
	  END IF                                ! band
 51       CONTINUE                              
CCC	  I=WNGCCN()                          	! nr of control-C seen
CCC	  IF (I.GT.1) NO_MORE=.TRUE.          
CCC	  IF (I.NE.0) GOTO 52                 
	END DO                                  ! sectors
CCC 52     CONTINUE                                
CCC	IF (NO_MORE) RETURN                     ! USER SAID: STOP
C                                               
	IF (IFR_MODE.EQ.'BAND') THEN            ! CALCULATE AVERAGE
	  IF (MOSAIK) THEN                      
!= I1=ICH I4=IFR I2=ICS                         
	    DO ICH=0,NCHAN-1                    
	     DO IFR=0,STHJ(STH_NIFR_J)-1        
	      DO ICS=1,2                        
	       IF ((AMCO(ICS,IFR,ICH).NE.1E20).AND.(NRAMCO(IFR,ICH).NE.0))
	1           AMCO(ICS,IFR,ICH)=AMCO(ICS,IFR,ICH)/NRAMCO(IFR,ICH)
	      END DO                            
	     END DO                             
	    END DO                              
	  END IF                                
	  IF (.NOT.OPENSW) THEN                 
	    IF ((.NOT.MOSAIK.AND..NOT.PLOTAP).OR.
	1      ((MOSAIK.AND.(IFR_MODE.NE.'BAND'))))THEN
	      CALL NPLOPN (LDATTP,IPOL,NHV,IFRS)! OPEN PLOT, PLOT HEADING
	      IF (NO_MORE) RETURN               ! USER SAID: STOP
	      CALL NPLPBE (.TRUE.,NPLOT,PTXT,NHV,NCHAN)
						! PLOT BEGIN-ANNOTATIONS
	      CALL NPLPBE (.FALSE.,NPLOT,PTXT,NHV,NCHAN)
						! PLOT END-ANNOTATIONS
	      OPENSW=.TRUE.                     
	    END IF                              
	  END IF                                
	  CALL NPLPLT (LDATTP,IPOL,NPLOT,PTXT,NHV,TABIFR,SCHAN,IFRS,
	1         NCHAN,STHJ(STH_NIFR_J),HAB,   
	2         STHE(STH_HAI_E),PLTIFR,IFRA,AMCO)
CCC	  IF (WNGCCN().GT.1) NO_MORE=.TRUE.     
	  IF (NO_MORE) RETURN                   ! USER SAID: STOP
	END IF                                  
	IF (OPENSW) THEN                        
	  IF ((.NOT.MOSAIK.AND..NOT.PLOTAP).OR. 
	1       ((MOSAIK.AND.(IFR_MODE.NE.'BAND'))))THEN
CC	    CALL NPLPBE (.FALSE.,NPLOT,PTXT,NHV,NCHAN)
CC						! PLOT END-ANNOTATIONS
	    IF ((PPP(1).EQ.1).AND.(PPP(2).EQ.1)) THEN
	      CALL NPLCLO(DQID,NHV)             ! CLOSE PLOT
	      OPENSW=.FALSE.                    
	    END IF                              
	  END IF                                
	END IF                                  
C                                               
	RETURN                                  
C                                               
	END                                     

