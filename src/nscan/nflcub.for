C+ NFLCUB.FOR
C  JEN 931116
C
C  Revisions:
C	CMV 931220	Pass FCA of input file to WNDXLP and WNDSTA/Q
C	CMV 940926	Close old file before asking new one
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	LOGICAL FUNCTION NFLCUB (ACTION,NAME,IVAL,
     1                           SELHA,SELPOL,SELIFR)
C
C  Deal with uv-data `hyper-cube' and its sub-cube.
C  Also: hide the rather strange polarisation selection scheme.
C
C  Result:
C
C	JS = NFLCUB (ACTION_C(*):I,NAME_C(*):I,IVAL_J:I,
C                    SELHA_R(0:1):O,SELPOL_B(0:3):O,
C                    SELIFR_B(0:STHTEL-1,0:STHTEL-1):O)
C
C  JS = NFLCUB ('SPECIFY','NODE',0,0,0,0)
C       Specify input SCN-file (NODIN and FCAIN stored in Common)
C  JS = NFLCUB ('SPECIFY','SETS',0,0,0,0)       (alternative: 'SECTORS')
C       Specify Sets of Sectors (SETS stored in Common)
C
C  JS = NFLCUB ('SPECIFY','HYPERCUBE',0,0,0,0)
C       Specify the hypercube (HA,POL,IFR)
C       NB: This definition is kept internally. Use SELECT HYP to get it.
C  JS = NFLCUB ('SELECT','HYPERCUBE',0,SELHA,SELPOL,SELIFR)
C       Reset the selection (SEL) to the current hypercube.
C
C  JS = NFLCUB ('SPECIFY','SUBCUBE',0,SELHA,SELPOL,SELIFR)
C       Specify a `sub-cube', within the current hypercube. 
C  JS = NFLCUB ('SPECIFY','SUB_POL',0,SELHA,SELPOL,SELIFR)
C  JS = NFLCUB ('SPECIFY','SUB_HA',0,SELHA,SELPOL,SELIFR)
C  JS = NFLCUB ('SPECIFY','SUB_IFR',ICODE,SELHA,SELPOL,SELIFR)
C	ICODE = 0: Use input SELIFR as default, and type SELIFR on screen.
C	ICODE = 1: Pre-select all ifrs, and do not type SELIFR on screen.
C	ICODE = 4: Pre-select no  ifrs, and do not type SELIFR on screen.
C  JS = NFLCUB ('SELECT','SUBCUBE',0,SELHA,SELPOL,SELIFR)
C       Reset the selection (SEL) to the sub-cube that was specified last.
C
C  JS = NFLCUB ('ADJUST','SELPOL',NPOLS,SELHA,SELPOL,SELIFR)
C       Adjust SELPOL as a function of the nr of pols (NPOLS) in a Sector.
C       NB: The input value is NPOLS = STHI(STH_PLN_I) from the Sector header.
C	NB: This (regretfully) has to be done after reading each Sector header.
C	NB: The default SELPOL in the other calls is for NPOLS=4
C
C PIN references:
C
C	SUB_CUBE		!Ask whether sub-cube to be specified
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'NFL_DEF'
	INCLUDE 'CBITS_DEF'
	INCLUDE 'STH_O_DEF'
	INCLUDE 'SCH_O_DEF'
C
C  Parameters:
C
	INTEGER XX,XY,YX,YY
	  PARAMETER (XX=0, XY=1, YX=2, YY=3)
C
C  Arguments:
C
	CHARACTER ACTION*(*)		!ACTION TO BE PERFORMED
	CHARACTER NAME*(*)		!EXTRA INFORMATION (if any)
	INTEGER*2 IVAL                  !INTEGER INPUT VALUE (if any)
C
	REAL    SELHA(0:1)              !SELECTED HA-RANGE (circles)
	LOGICAL SELPOL(0:3)             !SELECTED POL-RANGE
	BYTE    SELIFR(0:STHTEL-1,0:STHTEL-1) !SELECTED IFRS (RTW,RTE)
C
C  Function references:
C
	LOGICAL WNDPAR			!GET DWARF PARAMETER
	LOGICAL WNDNOD			!GET NODE
	LOGICAL WNFOP			!OPEN FILE
	LOGICAL WNFRD			!READ DATA
	LOGICAL WNFWR			!WRITE DATA
	CHARACTER*32 WNTTSG		!SET NAME
	LOGICAL WNDSTQ			!GET SETS
	INTEGER WNCAJ			!GET INTEGER FROM TEXT
	LOGICAL NSCSTG			!GET A SET
	LOGICAL NSCIFS			!GET INTERFEROMETER SELECTION
	LOGICAL NSCPLS			!SELECT POLARISATION
	LOGICAL NSCHAS			!SELECT HA
	CHARACTER*80  ARGSTR
C
C  Data declarations:
C
C  Look-up table to find existence and offset for polarisations
C  depending on the number of polarisations present in the data
C
	INTEGER PPOL(XX:YY,1:4,0:1)	!POL. SELECT XX,XY,YX,YY FOR 
					! NPOLS=1:4:
 	  DATA  PPOL/XX_P,0,0,0, XX_P,0,0,YY_P, 0,0,0,0,
     1		     XX_P,XY_P,YX_P,YY_P,                !BITS
     1	             0,0,0,0, 0,0,0,1, 0,0,0,0, 0,1,2,3/ !OFFSETS
C
C OLD:	  DATA  PPOL/1,0,0,0, 1,0,0,8, 0,0,0,0, 1,2,4,8, !BITS
C
	CHARACTER*2 POLNAM(0:3)
	  DATA POLNAM /'XX','XY','YX','YY'/
C
C  Selection of hypercube (NODIN and SETS are in common block)
C
	INTEGER 	HYPPOL		!POL. CODE (BITS)
	BYTE HYPIFR(0:STHTEL-1,0:STHTEL-1) !IFR SELECTION
	REAL 		HYPHA(0:1)	!HA RANGE
C
C  Saved sub0cube selection:
C
	INTEGER 	SPOL		!POL. CODE (BITS)
	BYTE SIFR(0:STHTEL-1,0:STHTEL-1) !IFR SELECTION
	REAL 		SHA(0:1)	!HA RANGE
C
	LOGICAL 	ALLIF		!TRUE: All ifrs selected
	INTEGER         NPOLS           !NR OF POLS IN SSN-FILE 
	LOGICAL 	SPECIFR         !TRUE: Specify ifrs
	LOGICAL 	SPECPOL         !TRUE: Specify pols
	LOGICAL 	SPECHAR         !TRUE: Specify ha-range
	CHARACTER*24	SUBCUB          !Option of keyword SUB_CUBE
	LOGICAL		PRINTSUB	!Print summary of sub-cube
C
        CHARACTER*80 TXT80              !TEXT BUFFER
	INTEGER LPOFF(0:7)              !CURRENT OFFSETS (SECTOR LOOPS)
C
C  Commons:
C
	COMMON /NFLCUBE/ ALLIF,
     1                   HYPPOL,HYPHA,HYPIFR,
     1                   SPOL,SHA,SIFR
C-
C******************************************************************************
C******************************************************************************
C
	NFLCUB = .TRUE.                         !ASSUME OK
	PRINTSUB = .FALSE.
CCCC	CALL WNCTXT (F_T,'NFLCUB: '//ACTION(:5)//NAME)
C
C***************************************************************************
C
	IF (ACTION(:4).EQ.'SPEC') THEN
C
C----------------------------------------------------------------------------
C  SPECIFY SCN-NODE: 
C
	  IF (NAME(:3).EQ.'NOD') THEN
	    CALL WNDXLI(LPOFF)			! Clear offsets (loops)
	    SETS(0,0)=0				!DEFAULT SETS (NONE)
	    HYPPOL=XYX_M			!ASSUME ALL POLS
	    ALLIF=.TRUE.			!ASSUME ALL IFRS
	    HYPHA(0)=-179.99/360.		!ASSUME FULL HA RANGE
	    HYPHA(1)=+179.99/360.               !  (CIRCLES)
C
 11         CONTINUE   
	    CALL WNFCL(FCAIN)			!CLOSE FIRST
	    IF (.NOT.WNDNOD('INPUT_SCN_NODE',NODIN,'SCN','U',
     1		       NODIN,IFILE)) THEN	!NODE
	      IF (E_C.EQ.DWC_ENDOFLOOP) THEN    !READY 
	        NFLCUB = .FALSE.                !NOT SUCCESSFUL
		GOTO 800                        !EXIT
	      END IF
	      CALL WNCTXT(F_TP,'Node does not exist')
	      GOTO 11                           !TRY AGAIN
	    ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN
	      NFLCUB = .FALSE.                  !NOT SUCCESSFUL
	      GOTO 800				!EXIT
	    ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
	      GOTO 11				!MUST SPECIFY
	    END IF
C
	    IF (.NOT.WNFOP(FCAIN,IFILE,'U')) THEN	!OPEN SCAN FILE
	      CALL WNCTXT(F_TP,'Cannot open file attached to scan node')
	      GOTO 11
	    END IF
C
C----------------------------------------------------------------------------
C  SPECIFY SETS OF SECTORS:
C
	  ELSE IF (NAME(:3).EQ.'SET') THEN
C
 14	    CONTINUE
	    IF (.NOT.WNDSTQ('SCN_SETS',MXNSET,SETS,FCAIN)) THEN
	      CALL WNFCL(FCAIN)
	      NFLCUB = .FALSE.                !NOT SUCCESSFUL
              GOTO 800                        !EXIT
	    END IF
C
	    IF (SETS(0,0).EQ.0) THEN	      !NONE SPECIFIED
	      CALL WNFCL(FCAIN)
	      NFLCUB = .FALSE.                !NOT SUCCESSFUL
              GOTO 800                        !EXIT
            END IF
C
C----------------------------------------------------------------------------
C  SPECIFY HYPERCUBE (use existing selection as default):
C
	  ELSE IF (NAME(:3).EQ.'HYP') THEN
C
 12	    CONTINUE
 	    IF (.NOT.NSCPLS(0,HYPPOL)) THEN	!SELECT POL.
	      CALL WNFCL(FCAIN)
	      NFLCUB = .FALSE.                !NOT SUCCESSFUL
              GOTO 800                        !EXIT
            END IF
C
 15	    CONTINUE
            IF (ALLIF) THEN
 	      IF (.NOT.NSCIFS(1,HYPIFR)) THEN   !PRE-SELECT ALL IFRS
	        NFLCUB = .FALSE.                !NOT SUCCESSFUL
                GOTO 800                        !EXIT
              END IF
            ELSE
 	      IF (.NOT.NSCIFS(0,HYPIFR)) THEN   !USE IFRS AS GIVEN, AND SHOW
	        NFLCUB = .FALSE.                !NOT SUCCESSFUL
                GOTO 800                        !EXIT
              END IF
            END IF
	    ALLIF=.TRUE.		  
	    DO I=0,STHTEL-1		
	      DO I1=I,STHTEL-1
	        IF (.NOT.HYPIFR(I1,I)) ALLIF=.FALSE. !NOT ALL IFRS SELECTED
	      END DO
	    END DO
C
 13	    CONTINUE
 	    IF (.NOT.NSCHAS(0,HYPHA)) THEN	  !SELECT HA RANGE
	      NFLCUB = .FALSE.                    !NOT SUCCESSFUL
              GOTO 800                            !EXIT
            END IF
C
C----------------------------------------------------------------------------
C  SPECIFY SUB-CUBE (i.e. a new subset of the current hyper-cube):
C  NB: Use the input selection as default, except if IVAL<>0.
C
	  ELSE IF (NAME(:3).EQ.'SUB') THEN
	    PRINTSUB = .FALSE.                    !PRINT SUMMARY OF SUB-CUBE
	    SPECIFR = .FALSE.
	    SPECPOL = .FALSE.
	    SPECHAR = .FALSE.
	    IF (INDEX(NAME,'IFR').GT.0) THEN
              SPECIFR = .TRUE.
	    ELSE IF (INDEX(NAME,'POL').GT.0) THEN
              SPECPOL = .TRUE.
	    ELSE IF (INDEX(NAME,'HA').GT.0) THEN
              SPECHAR = .TRUE.
            ELSE
 100	      CONTINUE
	      IF (.NOT.WNDPAR('SUB_CUBE',SUBCUB,
     1                        LEN(SUBCUB),J0,'NO')) THEN
	        IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 800	!READY
	        GOTO 100				!ERROR
	      END IF
	      IF (J0.EQ.0) GOTO 100		!EMPTY STRING, TRY AGAIN
	      IF (J0.LT.0) GOTO 100		!WILDCARD (?)
	      IF (SUBCUB(:3).EQ.'YES') THEN
                SPECPOL = .TRUE.
                SPECIFR = .TRUE.
                SPECHAR = .TRUE.
              END IF
	      IF (SUBCUB(:3).EQ.'POL') SPECPOL = .TRUE.
	      IF (SUBCUB(:3).EQ.'IFR') SPECIFR = .TRUE.
	      IF (SUBCUB(:2).EQ.'HA') SPECHAR = .TRUE.
	    END IF
C
	    IF (SPECPOL) THEN
 110          CONTINUE
 	      IF (.NOT.NSCPLS(0,SPOL)) THEN            !POLS
                GOTO 110
              END IF
              SPOL = IAND(SPOL,HYPPOL)                 !KEEP INSIDE HYPERCUBE
	      DO I3=0,3
                SELPOL(I3) = (IAND(PPOL(I3,NPOLS,0),SPOL).NE.0) 
	      END DO
            END IF
C
	    IF (SPECIFR) THEN
 120          CONTINUE
	      I = IVAL                                 !INPUT PARAMETER
 	      IF (.NOT.NSCIFS(I,SIFR)) THEN            !IFRS
                GOTO 120
              END IF
              DO I=0,STHTEL-1                          !KEEP INSIDE HYPERCUBE
                DO I1=0,STHTEL-1
                  SIFR(I,I1) = (SIFR(I,I1).AND.HYPIFR(I,I1))
                  SELIFR(I,I1) = SIFR(I,I1)            !OUTPUT
                END DO
              END DO
            END IF
C
	    IF (SPECHAR) THEN
 130          CONTINUE
 	      IF (.NOT.NSCHAS(0,SHA)) THEN           !HA-RANGE
                GOTO 130
	      END IF
              SHA(0) = MAX(SHA(0),HYPHA(0))          !KEEP INSIDE HYPERCUBE
              SHA(1) = MIN(SHA(1),HYPHA(1))
	      SELHA(0) = SHA(0)                      !OUTPUT
	      SELHA(1) = SHA(1)                      !OUTPUT
	    END IF
C
C----------------------------------------------------------------------------
C  NAME NOT RECOGNISED:
C
	  ELSE
	    ARGSTR='NFLCUB '//ACTION(:3)//' Name not recognised: '//NAME
	    CALL WNCTXT (F_TP,ARGSTR)
            NFLCUB = .FALSE.                     !NOT SUCCESSFUL
	  END IF
C
C***************************************************************************
C  SELECT HYPERCUBE (i.e. set SELxxx to the hypercube selection):
C
	ELSE IF (ACTION(:3).EQ.'SEL') THEN
	  PRINTSUB = .FALSE.                    !PRINT SUMMARY OF SUB-CUBE
C
C--------------------------------------------------------------------------
C  SELECT HYPERCUBE (i.e. set SELxxx to the hypercube selection):
C
	  IF (NAME(:3).EQ.'HYP') THEN
	    SELHA(0)=HYPHA(0)      !SELECTED HA-RANGE EQUAL TO HYPERCUBE
	    SELHA(1)=HYPHA(1)
	    SHA(0) = SELHA(0)      !KEEP FOR LATER
	    SHA(1) = SELHA(1)      !KEEP FOR LATER
C
            NPOLS = 4                           !DEFAULT: 4 POLS IN SCN-FILE
	    DO I3=0,3
              SELPOL(I3) = (IAND(PPOL(I3,NPOLS,0),HYPPOL).NE.0) 
	    END DO
	    SPOL = HYPPOL                       !KEEP FOR LATER
C
            DO I=0,STHTEL-1
              DO I1=0,STHTEL-1
                SELIFR(I,I1) = HYPIFR(I,I1)   !IDEM IFR-SELECTION
		SIFR(I,I1) = SELIFR(I,I1)     !KEEP FOR LATER
              END DO
            END DO
C
C--------------------------------------------------------------------------
C  SELECT SUB-CUBE (i.e. set SELxxx to the last sub-cube specs):
C  NB: This is useful to get beck to a known selection.
C
	  ELSE IF (NAME(:3).EQ.'SUB') THEN
	    SELHA(0)=SHA(0)      !SELECTED HA-RANGE EQUAL TO HYPERCUBE
	    SELHA(1)=SHA(1)
C
            NPOLS = 4                     !DEFAULT: 4 POLS IN SCN-FILE
	    DO I3=0,3
              SELPOL(I3) = (IAND(PPOL(I3,NPOLS,0),SPOL).NE.0) 
	    END DO
            DO I=0,STHTEL-1
              DO I1=0,STHTEL-1
                SELIFR(I,I1) = SIFR(I,I1)   !IDEM IFR-SELECTION
              END DO
            END DO
C
C----------------------------------------------------------------------------
C  NAME NOT RECOGNISED:
C
	  ELSE
            ARGSTR='NFLCUB '//ACTION(:3)//' Name not recognised: '//NAME
	    CALL WNCTXT (F_TP,ARGSTR)
            NFLCUB = .FALSE.                     !NOT SUCCESSFUL
	  END IF
C
C***************************************************************************
C  ADJUST SELPOL (as a function of NPOLS from the Sector header):
C
	ELSE IF (ACTION(:3).EQ.'ADJ') THEN
	  PRINTSUB = .FALSE.                    !PRINT SUMMARY OF SUB-CUBE
	  NPOLS = IVAL                         !INPUT VALUE
	  IF (NPOLS.LT.0.OR.NPOLS.GT.4) THEN
            CALL WNCTXT (F_TP,'NFLCUB ADJUST SELPOL: '
     1                  //' NPOLS out of range, =!SJ',NPOLS)
            NFLCUB = .FALSE.
            GOTO 800
          ELSE
            DO I3=0,3                   !DECODE POL SELECTION BITS OF SPOL
              SELPOL(I3) = (IAND(PPOL(I3,NPOLS,0),SPOL).NE.0) 
            END DO  
	  END IF
C
C***************************************************************************
C  ACTION NOT RECOGNISED:
C
	ELSE
	  CALL WNCTXT (F_TP,'NFLCUB: Action '//ACTION(:8)
     1           //' not recognised (name='//NAME(:5)//')')
          NFLCUB = .FALSE.                     !NOT SUCCESSFUL
	END IF
C
C****************************************************************************
C  Finished:
C
 800    CONTINUE
C
C  Print summary of sub-cube, if required:
C
	IF (PRINTSUB) THEN
	  I4 = 1
	  TXT80(I4:) = 'NFLCUB sub-cube:'
	  I4 = I4+16
	  CALL WNCTXS (TXT80(I4:),' SELHA=!EAF6.1<>!EAF6.1'
     1		                  ,SELHA(0),SELHA(1))
	  I4 = I4+20
	  TXT80(I4:) = ' SELPOL:'
	  DO I3 = 0,3
	    IF (SELPOL(I3)) THEN
	      TXT80(3*I3+I4+8:) = POLNAM(I3)
	    ELSE
	      TXT80(3*I3+I4+8:) = '-'
	    END IF
          END DO
	  I4 = I4+20
          I2 = 0
          I3 = 0
	  DO I=0,STHTEL-1		
	    DO I1=I,STHTEL-1
              IF (I.EQ.I1) THEN              !AUTOCORR
	        IF (SELIFR(I1,I)) I3=I3+1   
              ELSE                           !CROSSCORR
	        IF (SELIFR(I1,I)) I2=I2+1
              END IF
	    END DO
	  END DO
	  CALL WNCTXS (TXT80(I4:),' SELIFR:!SJ(!SJ) ifrs',I2,I3)
	  CALL WNCTXT (F_T,TXT80)
	END IF
C
	RETURN
	END
