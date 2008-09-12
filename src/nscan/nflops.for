C+ NFLOPS.FOR
C  JEN 931115 (used to be part of nflflg.for)
C
C  Revisions:
C
C	JEN 940216	Add DODRYRUN switch
C	JEN 940216	Add QXY option (this required taking the 
C                       data-flag modification out of the POL (I3) loop)
C	JEN 940217	N_SCA: If one HA given, only one Scan flagged (not 3).
C	JEN 940303	Add YXY option
C       JEN 940701      Replace NFLST2 with improved NFLST1
C	CMV 940707	Add HARANGE option
C	CMV 940717	Add ELEVATION option
C       JEN 940901      Take reset of CDATLAST/HALAST out of Sector-loop
C       JEN 940901      Add option RT1 (=DT1 for residues (data-model))
C       JEN 960226      Add options UXY and VXY       
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C NFLOPS.LOGIC:
C
C  What determines the flagging (JEN/CMV):
C
C  Each uv-data point, and each HA-Scan header, contains a `flagbyte'.
C  The 8 bits of a flagbyte represent 8 types of flags that can be set.
C  Header and data flagbytes have the same structure. A flag set in a
C    Scan header is entirely equivalent to the same flag being set in
C    all uv-data of that Scan. The header flags may save time.
C  The flagbytes are modified with the help of a `FLAGBYTE'.
C
C  The main parameters that control the flags in headers and/or uv-data
C  all have three levels: A global default, a local default, and an
C  actual value. The latter may be changed safely when circumstances 
C  require it, but will revert back to a known default value afterwards.
C  NB: This scheme seems complex, but it is consistent and safe, while
C  giving full control over the various (and rather diverse) operations.
C
C    SELFLAG   Global (default) FLAGBYTE (8 flagbits) to modify flagbytes. 
C              Each flagging operation has its own default FLAG (i.e. the 
C              flag-type(s) to be affected). This default may be overridden
C              with a user-defined FLAGBYTE: FLAG=UFL if UFL<>0.
C    FLAGH_DFLT    Default FLAGBYTE for Scan headers (default=FLAG) 
C    FLAGH     Actual FLAGBYTE for this Scan header (default=FLAGH_DFLT) 
C    FLAGD_DFLT    Default FLAGBYTE for individual uv-data (default=FLAG) 
C    FLAGD     Actual FLAGBYTE for this uv-data point (default=FLAGD_DFLT) 
C
C    SETFLAG   Global flag/unflag switch (flagging mode): 
C              If true:  flagbyte = IOR (flagbyte,FLAGBYTE)  
C              If false: flagbyte = IAND(flagbyte,NOT(FLAGBYTE))
C    SETFH_DFLT    Default switch to be used for Scan headers (default=SETFLAG) 
C    SETFH     Actual switch for this Scan header (default=SETFH_DFLT) 
C    SETFD_DFLT    Default switch for individual uv-data (default=SETFLAG) 
C    SETFD     Actual switch for this uv-data point (default=SETFD_DFLT) 
C
C    MODFH_DFLT    If true, modify flagbyte in Scan headers in principle.
C    MODFH     If true, modify flagbyte in this Scan header (deflt=MODFH_DFLT).
C    MODFD_DFLT    If true, modify flagbyte in individual uv-data in principle.
C    MODFD     If true, modify flagbyte in this uv-data pnt (deflt=MODFD_DFLT).
C
C    CORTP     Global default switch (flagging mode).
C    CORRDAT   Local switch: if true, actually use corrected uv-data.
C
C    WRSCH     If true, write Scan header (flagbyte has been modified)
C    WRSCN     If true, write Scan data (flagbyte(s) have been modified)
C
C    SELECT    Switch used in `criterion-operations' (headers or uv-data).
C              Example: SELECT=true if residue < specified limit (ARESID).
C              - MODFH=.true. (or MODFD=.true., depending on operation)
C              - SETFH=SELECT (or SETFD=SELECT, depending on operation)
C              This logic, combined with the SETFLAG logic above, will 
C              set flags if the criterion is met, and resets them if not 
C              (i.e. the UNFLAG mode is irrelevant here):
C
C Logic diagram of the subroutine NFLOPS.FOR (JEN,CMV)
C
C   Initialise
C     SETFLAG = .true.
C   Specify data hypercube (node,sets,HYPPOL,HYPIFR,HYPHA)
C   Get flagging option (mode, operations, inspect, statistics) 
C
C   CORRDAT = CORTP                       !default
C   SELIFR may be a subset of HYPIFR   
C   SELPOL may be a subset of HYPPOL   
C   SELHA may be a subset of HYPHA   
C
C   Operate on the selected data (node,sets,SELPOL,SELIFR,SELHA):
C     Zero the various flag counters.
C     Zero the various statistics buffers.   
C     MODFH_DFLT = .false.
C     SETFH_DFLT = SETFLAG
C     FLAGH_DFLT = FLAG
C     Do for all the (sets of) Sectors in the hypercube:
C       Read Sector header
C	Do for all the specified HA-Scans (SELHA):
C         Read Scan header 
C         MODFH = MODFH_DFLT
C	  SETFH = SETFH_DFLT
C         FLAGH = FLAGH_DFLT
C         MODFD_DFLT = .false.
C         SETFD_DFLT = SETFLAG
C	  FLAGD_DFLT = FLAGH_DFLT
C	  WRSCH = .false.
C	  WRSCN = .false.
C         SELECT = .false.
C
C         Handle various options: this may change the local switches above.
C         Goto 40 if the individual uv-data can be ignored.  
C
C         Read Scan uv-data 
C         Do for all specified ifrs (SELIFR) and pols (SELPOL)
C           WFDAT(I3) = IAND('0000FFFF'X,LDAT)    !weightbyte,flagbyte
C           if (WFDAT(I3).ne.0) then              !nonzero weight, no flags!
C             MODFD = MODFD_DFLT
C	      SETFD = SETFD_DFLT
C             FLAGD = FLAGD_DFLT
C             SELECT=.false.
C
C             Handle various options: this may change the local switches above.
C
C 60          if (MODFD) then              !modify flagbyte of uv-data point
C               if (SETFD) then flagbyte = IOR (flagbyte,FLAGD)  
C                          else flagbyte = IAND(flagbyte,NOT(FLAGD))
C               WRSCN=.true. 
C	      endif
C             if (CNTFD) Count the data flags of the specified type(s).
C             if (STACCD) Accumulate statistics of data.
C           end if (WFDAT(I3)) 
C         end do (SELPOL,SELIFR) 
C
C 40      if (MODFH) then            !modify flagbyte of Scan header
C           if (SETFH) then flagbyte = IOR (flagbyte,FLAGH)  
C                      else flagbyte = IAND(flagbyte,NOT(FLAGH))
C	     WRSCH=.true.
C	  endif
C         if (CNTFH) Count the header flags of the specified type(s).
C         if (STACCH) Accumulate statistics of header info.
C	  if (WRSCH) write the modified Scan header
C	  if (WRSCN) write the modified Scan data
C       Next Scan (SELHA)
C     Next Sector (sets)
C
C     Print flag summary (several 1D projections through hypercube)
C
C   Back to flagging Operation.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	SUBROUTINE NFLOPS (OPER,USERFLAG,CORTP,
     1                     SHOW_CNT,TRACE,DODRYRUN)
C
C  (Un)-flag scan data
C
C  Result:
C
C	CALL NFLOPS (OPER_C(*):I,USERFLAG_J:I,CORTP_L:I,
C                    SHOW_CNT_L:I,TRACE_L:I)
C
C PIN references:
C
C	SELECT_FLAG
C	HA
C	LIMIT
C	LIMITS
C	FLAG_LIMIT
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'NFL_DEF'
	INCLUDE 'CBITS_DEF'
	INCLUDE 'STH_O_DEF'		!SET HEADER
	INCLUDE 'SCH_O_DEF'		!SCAN HEADER
C
C  Parameters:
C
	INTEGER XX,XY,YX,YY		!POL. POINTERS
	  PARAMETER (XX=0, XY=1, YX=2, YY=3)
C
        INTEGER CBIBMAX                 !NR OF POINTS IN CBI BEAMSHAPE
          PARAMETER (CBIBMAX=100)
C
C  Codes for the flagging operations
C
	INTEGER N_RT1,N_QUI,N_NEX,N_YXY,N_NOD,
     1          N_PBA,N_SCA,N_CBI,N_QXY,N_RNO,
     1          N_ANO,N_XRN,N_YRN,N_XAN,N_YAN,
     1          N_MAX,N_UVD,N_AMP,N_SHA,N_TOH,
     1          N_TOD,N_TOT,N_GET,N_PUT,N_RED,
     1          N_NON,N_RRE,N_ARE,N_ACC,N_ACH,
     1          N_COU,N_TOP,N_DT1,N_DT2,N_CLE,
     1          N_COS,N_SIN,N_CLH,N_CLD,N_ACD,
     1		N_HAR,N_ELE,N_UXY,N_VXY
C
	  PARAMETER (N_RT1=1, N_QUI=2, N_NEX=3, N_YXY=4, N_NOD=5)
	  PARAMETER (N_PBA=6, N_SCA=7, N_CBI=8, N_QXY=9, N_RNO=10)
	  PARAMETER (N_ANO=11,N_XRN=12,N_YRN=13,N_XAN=14,N_YAN=15)
	  PARAMETER (N_MAX=16,N_UVD=17,N_AMP=18,N_SHA=19,N_TOH=20)
	  PARAMETER (N_TOD=21,N_TOT=22,N_GET=23,N_PUT=24,N_RED=25)
	  PARAMETER (N_NON=26,N_RRE=27,N_ARE=28,N_ACC=29,N_ACH=30)
	  PARAMETER (N_COU=31,N_TOP=32,N_DT1=33,N_DT2=34,N_CLE=35)
	  PARAMETER (N_COS=36,N_SIN=37,N_CLH=38,N_CLD=39,N_ACD=40)
	  PARAMETER (N_HAR=41,N_ELE=42,N_UXY=43,N_VXY=44)
C
	INTEGER MXNOPER			!# OF OPERATION
	  PARAMETER (MXNOPER=44)
C
C  Codes for the flag-types
C
        INTEGER MXNFLTYP                !# of flag types (JEN)
          PARAMETER (MXNFLTYP=9)
C
C Misc:
C
        REAL VERYLARGE                  !Initial value for max/min
          PARAMETER (VERYLARGE=1.E38)
C
        REAL DRYLIMIT                   !High limit value for Dry Run
C
C  Arguments:
C
	CHARACTER	OPER*(*)	!SELECTED FLAGGING OPERATION
	INTEGER		USERFLAG		!USER-DEFINED FLAGBYTE (OVERRIDE)
	LOGICAL		CORTP		!CORRECT DATA/NO CORRECT DATA
C
	LOGICAL		SHOW_CNT        !SHOW THE COUNTED FLAGS AFTER OPS
	LOGICAL		TRACE		!TRACE THE FLAGGING OPERATION
	LOGICAL		DODRYRUN	!ESTIMATE DEFAULT CLIP LIMITS
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
	LOGICAL NSCSCH,NSCSCW		!READ/WRITE SCAN HEADER
	LOGICAL NSCSIF			!READ INTERFEROMETER TABLE
	LOGICAL NSCIFS			!GET INTERFEROMETER SELECTION
	LOGICAL NSCSCR,NSCSCM		!READ CORRECTED SCAN DATA or MODEL
	LOGICAL NMOMSL			!CALCULATE MODEL DATA
	LOGICAL NMORDH			!READ MODEL HEADER
	LOGICAL NSCPLS			!SELECT POLARISATION
	LOGICAL NSCHAS			!SELECT HA
        DOUBLE PRECISION NMOBMF,NMOBMV  !TELESCOPE BEAM CALCULATION
        REAL NFLST1                     !STATISTICS
        LOGICAL NFLCNT                  !FLAG COUNTING
        LOGICAL NFLCUB                  !UV-DATA CUBE
C
C  Data declarations:
C
C-----------------------------------------------------------------------
C
C  "Static variables"
C
C  List of flagging operation names and corresponding codes:
C  NB: ... indicates operations that no longer exist in NFLOPS.
C
	CHARACTER*3 CNOPER(MXNOPER)	      !OPERATION NAMES
	  DATA CNOPER/
     1      'RT1','...','...','YXY','...',
     1      'PBA','SCA','CBI','QXY','RNO',
     1      'ANO','XRN','YRN','XAN','YAN',
     1      'MAX','UVD','AMP','SHA','TOH',
     1      'TOD','TOT','...','...','RED',
     1      'NON','RRE','ARE','ACC','ACH',
     1      'COU','TOP','DT1','DT2','CLE',
     1      'COS','SIN','CLH','CLD','ACD',
     1	    'HAR','ELE','UXY','VXY'/
	  INTEGER NNOPER(MXNOPER)
	  DATA NNOPER/
     1        N_RT1,N_QUI,N_NEX,N_YXY,N_NOD,
     1        N_PBA,N_SCA,N_CBI,N_QXY,N_RNO,
     1        N_ANO,N_XRN,N_YRN,N_XAN,N_YAN,
     1        N_MAX,N_UVD,N_AMP,N_SHA,N_TOH,
     1        N_TOD,N_TOT,N_GET,N_PUT,N_RED,
     1        N_NON,N_RRE,N_ARE,N_ACC,N_ACH,
     1        N_COU,N_TOP,N_DT1,N_DT2,N_CLE,
     1        N_COS,N_SIN,N_CLH,N_CLD,N_ACD,
     1	      N_HAR,N_ELE,N_UXY,N_VXY/
C
C  Look-up table to find existence and offset for polarisations
C  depending on the number of polarisations present in the data
C
	INTEGER PPOL(XX:YY,1:4,0:1)	!POL. SELECT XX,XY,YX,YY FOR 
					! NPOL=1:4:
C OLD	  DATA  PPOL/1,0,0,0, 1,0,0,8, 0,0,0,0, 1,2,4,8, !BITS
 	  DATA  PPOL/XX_P,0,0,0, XX_P,0,0,YY_P, 0,0,0,0,
     1		     XX_P,XY_P,YX_P,YY_P,                !BITS
     1	             0,0,0,0, 0,0,0,1, 0,0,0,0, 0,1,2,3/ !OFFSETS
C
C  Names of polarisations, telescopes:
C
        CHARACTER*2 POLNAME(0:3)        !POL NAMES (XX, XY ETC)
        DATA POLNAME /'P0','P1','P2','P3'/    !TEMPORARY, SEE BELOW
C
        CHARACTER*1 TELNAME(0:STHTEL-1) !TEL NAMES (0,1,2,A, ETC)
        DATA TELNAME /'0','1','2','3','4','5','6',
     1                '7','8','9','A','B','C','D'/
C
C  Array with flag-types and flag-codes
C
        CHARACTER*4 FLAGNAME(0:MXNFLTYP-1)
        DATA FLAGNAME /'MAN','CLIP','NOIS','ADD','SHAD',
     1                 'U3','U2','U1','ALL'/
        INTEGER FLAGTYPE(0:MXNFLTYP-1)
        DATA FLAGTYPE /FL_MAN,FL_CLIP,FL_NOIS,FL_ADD,FL_SHAD,
     1                 FL_3,FL_2,FL_1,FL_ALL/
C
C-----------------------------------------------------------------------
C
C   Variables with user-input, defaults and direct derivatives
C NB: The variables CHARCTER OPTION*24 and OPT*3 are defined in a common.
C
	INTEGER		NOPER		!CURRENT OPERATION NR
C
C   Data selection (maybe subset of the specified hypercube):
C
        LOGICAL         SELPOL(0:3)     !POLARISATION SELECTION
	BYTE SELIFR(0:STHTEL-1,0:STHTEL-1) !IFR SELECTION 
	REAL 		SELHA(0:1)      !HA-RANGE
	INTEGER         IFRMIN,IFRMAX   !MIN,MAX IFR NR
	INTEGER         POLMIN,POLMAX   !MIN,MAX POL NR
C
C   Data correction:
C
	INTEGER		CAP,CDAP	!Global APPLY/DEAPPLY BITS (WNDDAP)
	INTEGER		DUFLAG          !Global User Flag (WNDDUF)
C
C   Flagging operations
C
C
	INTEGER         I6,I7,I8,I9,I10 !GENERAL INTEGER VARIABLES
	REAL            R2,R3,R4,R5     !GENERAL REAL VARIABLES
        CHARACTER*80    TXT80           !GENERAL TEXT BUFFER
	INTEGER		TOH_COUNT       !FLAG COUNTER  (FOR OPER. TOHEAD)
        INTEGER         RTW,RTE         !WEST,EAST TELESCOPE NRS
        REAL            DHA,DHA1,DHA2   !HA-DIFFERENCE (FOR OPER. DT1)
	LOGICAL         SELECT	        !SELECTED BY CRITERION
	INTEGER		CHCUR		!CURRENT CHANNEL NR
        REAL		HACUR,HAINC	!CURRENT HA (circles) and increment
	REAL 	  MXLIM(0:1)	  	!LIMITS 
	REAL 	  PBASLIM(0:1)	  	!LIMITS (Option PBAS) 
 	REAL      HAR(0:1)		!LIMITS (Option HAR)
	INTEGER	  TOH_LIMIT		!FLAG LIMIT # (FOR TOHEAD)
	REAL	  ELELIMIT	  	!ELEVATION LIMIT (circles)
	REAL	  SHADIAM	  	!SHADOW DIAMETER (FOR SHA, =25m)
        REAL      ELEV,WSRTLAT    	!ELEVATION,LATITUDE (DEGR?)
        REAL      LREL,MREL       	!L,M RELATIVE TO FIELD CENTRE (ARCSEC?)
        REAL      CBIBEAM(0:CBIBMAX)    !CBI `BEAMSHAPE' (FUNCTION OF R)
        REAL      CBIBINC         	!INCREMENT (RADIANS) OF R IN CBIBEAM
        REAL      CBIBPAR(0:1)    	!CBI BEAMSHAPE PARAMETERS
        REAL      CBDIR,CBDIST    	!DIRECTION, DISTANCE TO CONTROL BLDNG
        REAL      CBIFACT(0:STHTEL-1)   !CONTROL BUILDING INTERFERENCE FACTOR 
        LOGICAL   SHATEL(0:STHTEL-1)    !SHADOWED TELESCOPES   
C
C   Flow control
C
	INTEGER	 	SELFLAG		!SELECTED FLAG-TYPE(S) TO BE USED
	INTEGER		CFLAG		!LOCAL OVERRIDE (e.g. for CLEAR)
C
        INTEGER         FLAGH_DFLT      !DEFAULT FOR FLAGH
        INTEGER         FLAGH           !FLAGBYTE FOR SCAN HEADER
        INTEGER         FLAGD_DFLT      !DEFAULT FOR FLAGD
        INTEGER         FLAGD(0:3)      !FLAGBYTE FOR UV-DATUM
C
	LOGICAL		VALIDAT(0:3)	!VALID DATA POINT
	INTEGER		LDOFF(0:3)	!OFFSET IN ARRAY LDAT
	INTEGER		WFDAT(0:3)	!WGT/FLAG BYTES
C
	LOGICAL		SETFLAG		!FLAG/UNFLAG
        LOGICAL         SETFH_DFLT      !DEFAULT FOR SETFH
        LOGICAL         SETFH           !SET FLAG IN HEADER
        LOGICAL         SETFD_DFLT      !
        LOGICAL         SETFD(0:3)      !SET FLAG IN UV-DATUM
C
        LOGICAL         MODFH_DFLT      !DEFAULT FOR MODFH
        LOGICAL         MODFH           !MODIFY FLAG(S) IN HEADER
        LOGICAL         MODFD_DFLT      !DEFAULT FOR MODFD
        LOGICAL         MODFD(0:3)      !MODIFY FLAG(S) IN UV-DATUM
C
        LOGICAL         CNTFH_DFLT      !
        LOGICAL         CNTFH           !COUNT FLAG(S) IN HEADER
        LOGICAL         CNTFD_DFLT      !
        LOGICAL         CNTFD(0:3)      !COUNT FLAG(S) IN UV-DATUM
C
        LOGICAL         STACCH_DFLT     !
        LOGICAL         STACCH          !ACCUM.STATISTICS FLAG(S) IN HEADER
        LOGICAL         STACCD_DFLT     !
        LOGICAL         STACCD(0:3)     !ACCUM.STATISTICS FLAG(S) IN UV-DATUM
C
	LOGICAL		CHKDATA		!TEST INDIV. UV-DATA POINTS OF SCAN
        LOGICAL		CORRDAT		!APPLY UV-DATA CORRECTIONS 
C
	LOGICAL		WRSCH		!REWRITE (MODIFIED) SCAN HEADER
	LOGICAL		WRSCN		!REWRITE (MODIFIED) SCAN DATA
C
	LOGICAL 	SHOW_STAT	!Show summary of statistics
C
        LOGICAL DRYRUN                  !TO ESTIMATE DEFAULT LIMITS ETC
        INTEGER NDRYSCANS,MAXDRYSCANS   !NR OF SCANS IN DRY RUN
C
C  Data needed to call NMO (MODEL) routines:
C
	INTEGER NSRC(0:2)		!Source counts
	INTEGER LPOFF(0:7)		!Current offsets (loops)
        INTEGER NPOL                    !# OF POLS IN SECTOR
	INTEGER STP			!SOURCE TYPE
	DOUBLE PRECISION SRA,SDEC,SFRQ	!MODEL INFO (FROM MDL FILE)
	REAL UV0(0:3)			!BASIC UV COORDINATES
	REAL LM0(0:1)			!BASIC SOURCE DISPLACEMENT
	DOUBLE PRECISION FRQ0		!BASIC FREQUENCY (FROM MDL FILE)?
	REAL TF(0:1)			!INTEGR. TIME, BANDWIDTH
        INTEGER MINST                   !INSTRUMENT
	DOUBLE PRECISION RA,DEC,FRQ	!FROM SECTOR HEADER
        DOUBLE PRECISION BEMLIM,BEAMFAC !BEAM CALC 
C
C  Sector header and Scan header:
C
	INTEGER SNAM(0:7)		!SET NAME
	INTEGER STHP			!SET HEADER POINTER
        BYTE STH(0:STH__L-1)            !SET HEADER
          INTEGER*2 STHI(0:STH__L/LB_I-1)
          INTEGER STHJ(0:STH__L/LB_J-1)
          REAL STHE(0:STH__L/LB_E-1)
          DOUBLE PRECISION STHD(0:STH__L/LB_D-1)
          EQUIVALENCE (STH,STHJ,STHI,STHE,STHD)
        BYTE SCH(0:SCH__L-1)            !SCAN HEADER
          INTEGER*2 SCHI(0:SCH__L/LB_I-1)
          INTEGER SCHJ(0:SCH__L/LB_J-1)
          REAL SCHE(0:SCH__L/LB_E-1)
          EQUIVALENCE (SCH,SCHJ,SCHI,SCHE)
C
C Telescope configuration:
C
	INTEGER*2 IFRT(0:STHIFR-1)	!INTERFEROMETER TABLE
	INTEGER IFRA(0:1,0:STHIFR-1)    !RTWEST(0), RTEAST(1)
	REAL ANG(0:2,0:STHIFR-1)	!DIPOLE ANGLE INFORMATION
	REAL BASEL(0:STHIFR-1)		!BASELINE TABLE
	INTEGER IRED(0:STHIFR-1)	!REDUNDANT INDICATORS
	REAL PBAS,PBAS0			!PROJECTION BASELINE FACTOR
C
C uv-data:
C
	INTEGER*2 LDAT(0:2,0:4*STHIFR-1) !DATA
	INTEGER*2 LDAT1(0:2,0:4*STHIFR-1) !DATA COPY
	REAL WGT(0:STHIFR-1,0:3)	!DATA WEIGHT
	COMPLEX CDAT(0:STHIFR-1,0:3)	!DATA
	  REAL DAT(0:1,0:STHIFR-1,0:3)
	  EQUIVALENCE (CDAT,DAT)
	COMPLEX CMOD(0:3,0:STHIFR-1)	!IQUV MODEL
 	COMPLEX CAMOD(0:STHIFR-1,0:3)	!XYX MODEL
	REAL MWGT(0:3,0:STHIFR-1)	!MODEL WEIGHT
C
C Things kept for later use in flagging
C    NB: 0=last unflagged point, 1=last point (flagged or not)
C
        COMPLEX CDATLAST(0:STHIFR-1,0:3,0:1)   !EARLIER DATA
          REAL DATLAST(0:1,0:STHIFR-1,0:3,0:1) !(0=C,1=S)
          EQUIVALENCE (DATLAST,CDATLAST)
        REAL HALAST(0:STHIFR-1,0:3,0:1)        !HA OF EARLIER DATA
C
C Statistics:
C
        REAL CRITVALD(0:STHIFR-1,0:3)   !CRITERION VALUE PER UV-POINT
        REAL STATWGTD(0:STHIFR-1,0:3)    !STATISTICS WEIGHT PER UV-POINT
        REAL S_VAL(0:STHIFR),S_WGT(0:STHIFR) !TRANSFER BUFFERS
	REAL CRITVALH,STATWGTH          !SAME FOR HEADER VALUES
	REAL WTOT                       !
C
C  Flag counts (input for routine NFLCNT)
C
        INTEGER FLACC(0:STHIFR-1,0:3)   !DATA FLAGS FOR A SCAN
        INTEGER MASK(0:STHIFR-1,0:3)    !FLAGBYTES USED
C-
C******************************************************************************
C******************************************************************************
C
C INIT
C
        POLNAME(XX) = 'XX'                      ! Should be defined centrally
        POLNAME(XY) = 'XY'
        POLNAME(YX) = 'YX'
        POLNAME(YY) = 'YY'
C
        SETFLAG = .TRUE.                        !SET FLAGS
	SHOW_STAT = .FALSE.			!DO NOT SHOW STATISTICS
	STHE(STH_HAI_E)=0.0			!NO INCREMENT KNOWN
C
	MXLIM(0)=0				!CLIP/MAX/RESID LIMITS
	MXLIM(1)=100000
	PBASLIM(0)=0				!PBAS LIMITS (M)
	PBASLIM(1)=100
        SHADIAM=25                                !SHADOW DIAMETER (M)
	TOH_LIMIT=1					!FLAG LIMIT
C
        DRYRUN = .FALSE.                        !SWITCH FOR DRY RUN
        MAXDRYSCANS = 25                        !NR OF SCANS IN DRY RUN
C
        CALL NFLST0 ('INIT',' ',0,0.,0.)     !Initialise Statistics bookkeeping
        R0 = NFLST1 ('INIT',' ',' ',0,0.,0.) !Initialise Statistics buffers
C
C Read in the data hypercube definition, specified in nflflg:
C
	JS = NFLCUB ('SELECT','HYPERCUBE',0,SELHA,SELPOL,SELIFR)
C
C*****************************************************************************
C*****************************************************************************
C*****************************************************************************
C*****************************************************************************
C FLAGGING OPERATIONS:
C
 200    CONTINUE
C
C  Translate option name into option number (safer):
C
	NOPER=-1					!ASSUME UNKNOWN
	DO I=1,MXNOPER				!MAKE NUMERIC OPTION
	  IF (OPER(:3).EQ.CNOPER(I)) THEN
	    NOPER=NNOPER(I)
	  END IF
	END DO
C
C Do some initialising:
C
        DRYRUN = .FALSE.                        !NOT A DRY RUN
	SELFLAG = USERFLAG                      !SELECTED FLAG TYPES
	CFLAG = 0                               !OVERRIDE FOR CLEAR
	CORRDAT = CORTP				!APPLY CORRECTIONS IF ASKED
C
C------------------------------------------------------------------------
C
!*** CLEAR ALL FLAGS
C
	IF (NOPER.EQ.N_CLE .OR.
     1      NOPER.EQ.N_CLD .OR.
     1      NOPER.EQ.N_CLH) THEN
          CALL WNCTXT (F_TP,' ')
	  IF (NOPER.EQ.N_CLE) THEN
	    CALL WNCTXT(F_TP,'All flags of the specified type(s)'
     1           //' will be removed, from data AND Scan headers.')
	  ELSE IF (NOPER.EQ.N_CLH) THEN
	    CALL WNCTXT(F_TP,'All flags of the specified type(s)'
     1           //' will be removed from the Scan headers ONLY.')
	  ELSE IF (NOPER.EQ.N_CLD) THEN
	    CALL WNCTXT(F_TP,'All flags of the specified type(s)'
     1           //' will be removed from the uv-data ONLY.')
          END IF
	  CALL WNCTXT(F_TP,'Beware: much work can be undone'
     1         //' this way! Escape by specifying NONE.')
          CALL WNCTXT (F_TP,' ')
C
          CFLAG = 0                             !DEFAULT: NONE
          CALL WNCTXT (F_T,'Clear only flags of the following type(s):')
	  CALL WNDDA3('SELECT_FLAG',CFLAG) 	!SELECT FLAG TYPE(S)
	  IF (CFLAG.EQ.0) GOTO 700              !NONE: ESCAPE
C
          IF (NOPER.EQ.N_CLE .OR. NOPER.EQ.N_CLD) THEN
	    IF (.NOT.NFLCUB ('SPECIFY','SUBCUBE',0,
     1                     SELHA,SELPOL,SELIFR)) GOTO 800  !SUB-CUBE
          ELSE IF (NOPER.EQ.N_CLH) THEN
	    IF (.NOT.NFLCUB ('SPECIFY','SUBCUBE_HA',0,
     1                     SELHA,SELPOL,SELIFR)) GOTO 800  !SUB-CUBE
          END IF
C
!*** TODATA
C
	ELSE IF (NOPER.EQ.N_TOD) THEN
	  CFLAG=0                              !DEFAULT: NONE
          CALL WNCTXT (F_TP,'Copy only flags of the following type(s):')
	  CALL WNDDA3('SELECT_FLAG',CFLAG)
	  IF (CFLAG.EQ.0) GOTO 700              !NONE: ESCAPE
C
!*** TOHEAD
C
	ELSE IF (NOPER.EQ.N_TOH) THEN        
	  CFLAG=0                              !DEFAULT: NONE
          CALL WNCTXT (F_TP,'Copy only flags of the following type(s):')
	  CALL WNDDA3('SELECT_FLAG',CFLAG)
	  IF (CFLAG.EQ.0) GOTO 700              !NONE: ESCAPE
C
 221      CONTINUE
	  IF (.NOT.WNDPAR('TOH_LIMIT',TOH_LIMIT,LB_J,
     1                         J0,A_B(-A_OB),TOH_LIMIT,1)) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 800
	    GOTO 221
	  END IF
	  IF (J0.EQ.0) GOTO 700                  !EMPTY STRING: ESCAPE
	  IF (J0.LT.0) TOH_LIMIT=1               !WILDCARD (*): USE DEFAULT?
C
!*** TOTEL,TOPOL
C
	ELSE IF (NOPER.EQ.N_TOT .OR. NOPER.EQ.N_TOP) THEN
	  CFLAG=0                              !DEFAULT: NONE
          CALL WNCTXT (F_TP,'Copy only flags of the following type(s):')
	  CALL WNDDA3('SELECT_FLAG',CFLAG)
	  IF (CFLAG.EQ.0) GOTO 700              !NONE: ESCAPE
C
!*** COUNT (flags in data and Scan headers)
C    NB: A sub-cube of the specified data-cube may be selected.
C
        ELSE IF (NOPER.EQ.N_COU) THEN
          CFLAG = FL_ALL                      !DEFAULT: ALL TYPES
          CALL WNCTXT (F_T,'Count only flags of the following type(s):')
	  CALL WNDDA3('SELECT_FLAG',CFLAG) 	!SELECT FLAG TYPE(S)
          IF (CFLAG.EQ.0) GOTO 700		!NONE: ESCAPE
	  IF (.NOT.NFLCUB ('SPECIFY','SUBCUBE',1,
     1                     SELHA,SELPOL,SELIFR)) GOTO 800  !SUB-CUBE
C
!*** ACCUMULATE STATISTICS (of data and/or header info)
C    NB: A sub-cube of the specified data-cube may be selected.
C
        ELSE IF (NOPER.EQ.N_ACC .OR.
     1           NOPER.EQ.N_ACH .OR.
     1           NOPER.EQ.N_ACD) THEN
          SELFLAG = FL_ALL                      !DEFAULT: ALL TYPES
          CALL WNCTXT (F_T,'Ignore data/scans that are flagged'
     1                 //' with the following flag type(s):')
	  CALL WNDDA3('SELECT_FLAG',SELFLAG) 	!SELECT FLAG TYPE(S)
C
	  SHOW_STAT = .TRUE.                    !SHOW STATISTICS
          IF (NOPER.EQ.N_ACC .OR. NOPER.EQ.N_ACD) THEN
	    IF (.NOT.NFLCUB ('SPECIFY','SUBCUBE',0,
     1                     SELHA,SELPOL,SELIFR)) GOTO 800  !SUB-CUBE
          ELSE IF (NOPER.EQ.N_ACH) THEN
	    IF (.NOT.NFLCUB ('SPECIFY','SUBCUBE_HA',0,
     1                     SELHA,SELPOL,SELIFR)) GOTO 800  !SUB-CUBE
          END IF
C
C-----------------------------------------------------------------------------
!*** SCANS: Manual flagging of individual Scans:
C
	ELSE IF (NOPER.EQ.N_SCA) THEN
	  SELFLAG = FL_MAN		    !DEFAULT FLAG TYPE: MANUAL
	  IF (.NOT.NFLCUB ('SPECIFY','SUBCUBE_HA',0,
     1                     SELHA,SELPOL,SELIFR)) GOTO 800  !SUB-CUBE
C
!*** IFR: Manual flagging of individual ifrs/pols
C
	ELSE IF (NOPER.EQ.N_UVD) THEN
	  SELFLAG=FL_MAN			!DEFAULT FLAG TYPE: MANUAL
	  IF (.NOT.NFLCUB ('SPECIFY','SUBCUBE',4,
     1                     SELHA,SELPOL,SELIFR)) GOTO 800  !SUB-CUBE
C
!*** HAR: Manual flagging of ha-ranges
C
	ELSE IF (NOPER.EQ.N_HAR) THEN
	  SELFLAG=FL_MAN			!DEFAULT FLAG TYPE: MANUAL
	  CALL WNCTXT(F_T,'After the Sub-cube prompt, '//
     1		'you will be repeatedly prompted for a HA-range')
	  IF (.NOT.NFLCUB ('SPECIFY','SUBCUBE',4,
     1                     SELHA,SELPOL,SELIFR)) GOTO 800  !SUB-CUBE
 321	  CONTINUE
	  CALL WNCTXT(F_T,'Choose * to stop asking for HA-range')
	  IF (.NOT.NSCHAS(1,HAR)) GOTO 700
	  IF (HAR(0).LE.-0.499.AND.HAR(1).GE.0.499) GOTO 700
C
!*** (X/Y)RNOISE, (X/Y)ANOISE
C
	ELSE IF (NOPER.EQ.N_RNO .OR. NOPER.EQ.N_ANO .OR.
     1		 NOPER.EQ.N_XRN .OR. NOPER.EQ.N_YRN .OR.
     1		 NOPER.EQ.N_XAN .OR. NOPER.EQ.N_YAN) THEN
	  SELFLAG=FL_NOIS			!DEFAULT FLAG TYPE: NOISE
          DRYRUN = .TRUE.      !START WITH A DRY RUN, FOR DEFAULT LIMITS
	  DRYRUN = DODRYRUN  !NOT IF INHIBITED 
          IF (DRYRUN) THEN
	    CONTINUE                            !DO A DRY RUN
          ELSE
 213        DRYRUN = .FALSE.                    !RETURN POINT AFTER DRY RUN
	    TXT80 = 'CRIT_'//CNOPER(NOPER)      !CRITERION NAME
            MXLIM(0) = 0                        !DEFAULT LOWER LIMIT
            MXLIM(1) = 0                        !DEFAULT UPPER LIMIT
            IF (DODRYRUN) THEN
              MXLIM(1) = 3 * NFLST1('CALC',TXT80,'RMS',1,R0,WTOT) 
	      CALL WNCTXT (F_T,'Default upper threshold is 3*rms '
     1                     //' of dry-run values.')
            END IF
	    IF (.NOT.WNDPAR('CLIP_LIMITS',MXLIM,2*LB_E,
     1                             J0,A_B(-A_OB),MXLIM,2)) THEN
	      IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 800  !ESCAPE
	      GOTO 213
	    END IF
	    IF (MXLIM(0).GE.MXLIM(1)) GOTO 700  !ESCAPE
          END IF
          R0 = NFLST1 ('ASSIGN','CRIT_'//CNOPER(NOPER),
     1                 'UNIT=W.U.',1,0.,0.)       !Assign statistics group
C
!*** MAX
C
	ELSE IF (NOPER.EQ.N_MAX) THEN
	  SELFLAG=FL_CLIP			!DEFAULT FLAG TYPE: CLIP
          DRYRUN = .TRUE.      !START WITH A DRY RUN, FOR DEFAULT LIMITS
	  DRYRUN = DODRYRUN  !NOT IF INHIBITED 
          IF (DRYRUN) THEN
	    CONTINUE                            !DO A DRY RUN
          ELSE
 214        DRYRUN = .FALSE.                    !RETURN POINT AFTER DRY RUN
	    TXT80 = 'CRIT_'//CNOPER(NOPER)      !CRITERION NAME
            MXLIM(0) = 0                        !DEFAULT LOWER LIMIT
            MXLIM(1) = 0                        !DEFAULT UPPER LIMIT
            IF (DODRYRUN) THEN
              MXLIM(1) = 3 * NFLST1('CALC',TXT80,'RMS',1,R0,WTOT) 
	      CALL WNCTXT (F_T,'Default upper threshold is 3*rms '
     1                     //' of dry-run values.')
            END IF
	    IF (.NOT.WNDPAR('ABCS_LIMITS',MXLIM,2*LB_E,
     1                             J0,A_B(-A_OB),MXLIM,2)) THEN
	      IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 800  !ESCAPE
	      GOTO 214
	    END IF
	    IF (MXLIM(0).GT.MXLIM(1)) GOTO 700  !ESCAPE
          END IF
          R0 = NFLST1 ('ASSIGN','CRIT_'//CNOPER(NOPER),
     1                 'UNIT=W.U.',1,0.,0.)       !Assign statistics group
C
!*** CLIP (AMPL,COS,SIN):
C
	ELSE IF (NOPER.EQ.N_AMP .OR.
     1           NOPER.EQ.N_COS .OR.
     1           NOPER.EQ.N_SIN) THEN
	  SELFLAG=FL_CLIP			!DEFAULT FLAG TYPE: CLIP
          DRYRUN = .TRUE.      !START WITH A DRY RUN, FOR DEFAULT LIMITS
	  DRYRUN = DODRYRUN  !NOT IF INHIBITED 
          IF (DRYRUN) THEN
	    CONTINUE                            !DO A DRY RUN
          ELSE
 215        DRYRUN = .FALSE.                    !RETURN POINT AFTER DRY RUN
	    TXT80 = 'CRIT_'//CNOPER(NOPER)      !CRITERION NAME
            MXLIM(0) = 0                        !DEFAULT LOWER LIMIT
            MXLIM(1) = 0                        !DEFAULT UPPER LIMIT
            IF (DODRYRUN) THEN
              MXLIM(1) = 3 * NFLST1('CALC',TXT80,'RMS',1,R0,WTOT) 
	      CALL WNCTXT (F_T,'Default upper threshold is 3*rms '
     1                     //' of dry-run values.')
            END IF
	    IF (.NOT.WNDPAR('CLIP_LIMITS',MXLIM,2*LB_E,
     1                             J0,A_B(-A_OB),MXLIM,2)) THEN
	      IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 800  !ESCAPE
	      GOTO 215
	    END IF
	    IF (MXLIM(0).GE.MXLIM(1)) GOTO 700  !ESCAPE
          END IF
          R0 = NFLST1 ('ASSIGN','CRIT_'//CNOPER(NOPER),
     1                 'UNIT=W.U.',1,0.,0.)       !Assign statistics group
C
!*** DT1, RT1
C
	ELSE IF ((NOPER.EQ.N_DT1) .OR.
     1           (NOPER.EQ.N_RT1)) THEN
	  SELFLAG=FL_CLIP			!DEFAULT FLAG TYPE: CLIP
	  IF (NOPER.EQ.N_RT1) THEN
            CORRDAT = .TRUE.                      !CORRECTED DATA NEEDED!
            CALL WNCTXT (F_T,'Corrected uv-data will be used')
          END IF
          DRYRUN = .TRUE.      !START WITH A DRY RUN, FOR DEFAULT LIMITS
	  DRYRUN = DODRYRUN  !NOT IF INHIBITED 
          IF (DRYRUN) THEN
	    CONTINUE                            !DO A DRY RUN
          ELSE
 216        DRYRUN = .FALSE.                    !RETURN POINT AFTER DRY RUN
	    TXT80 = 'CRIT_'//CNOPER(NOPER)      !CRITERION NAME
	    MXLIM(0) = 0
            MXLIM(1) = 0                        !DEFAULT UPPER LIMIT
            IF (DODRYRUN) THEN
              MXLIM(1) = 3 * NFLST1('CALC',TXT80,'RMS',1,R0,WTOT) 
	      CALL WNCTXT (F_T,'Default threshold is 3*rms'
     1                  //' of scan-to-scan (=time) variations.')
            END IF
	    IF (.NOT.WNDPAR('DT1_LIMIT',MXLIM(1),LB_E,
     1                           J0,A_B(-A_OB),MXLIM(1),1)) THEN
	      IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 800
	      GOTO 216
	    END IF
	    IF (MXLIM(0).GE.MXLIM(1)) GOTO 700  !ESCAPE
          END IF
          R0 = NFLST1 ('ASSIGN','CRIT_'//CNOPER(NOPER),
     1                 'UNIT=W.U.',1,0.,0.)       !Assign statistics group
C
!*** RRESID,ARESID,QXY,UXY,VXY,YXY,
C
	ELSE IF (NOPER.EQ.N_RRE .OR. 
     1           NOPER.EQ.N_ARE .OR.
     1           NOPER.EQ.N_YXY .OR.
     1           NOPER.EQ.N_UXY .OR.
     1           NOPER.EQ.N_VXY .OR.
     1           NOPER.EQ.N_QXY) THEN
	  SELFLAG=FL_CLIP			!DEFAULT FLAG TYPE: CLIP
          CORRDAT = .TRUE.                      !CORRECTED DATA NEEDED!
          CALL WNCTXT (F_T,'Corrected uv-data will be used')
          DRYRUN = .TRUE.      !START WITH A DRY RUN, FOR DEFAULT LIMITS
	  DRYRUN = DODRYRUN  !NOT IF INHIBITED 
          IF (DRYRUN) THEN
            CONTINUE
          ELSE
 217        DRYRUN = .FALSE.                    !RETURN POINT AFTER DRY RUN
	    TXT80 = 'CRIT_'//CNOPER(NOPER)      !CRITERION NAME
            MXLIM(0) = 0                        !DEFAULT LOWER LIMIT
            MXLIM(1) = 0                        !DEFAULT UPPER LIMIT
            IF (DODRYRUN) THEN
              MXLIM(1) = 3 * NFLST1('CALC',TXT80,'RMS',1,R0,WTOT) 
	      CALL WNCTXT (F_T,'Default threshold is 3*rms'
     1                     //' of (unflagged) dry-run residues.')
            END IF
	    IF (.NOT.WNDPAR('CLIP_LIMIT',MXLIM(1),LB_E,
     1                             J0,A_B(-A_OB),MXLIM(1),1)) THEN
	      IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 800  !ESCAPE
	      GOTO 217
	    END IF
	    IF (MXLIM(0).GE.MXLIM(1)) GOTO 700  !ESCAPE
          END IF
          R0 = NFLST1 ('ASSIGN','CRIT_'//CNOPER(NOPER),
     1                 'UNIT=W.U.',1,0.,0.)       !Assign statistics group
C
!*** ELEVATION
C
	ELSE IF (NOPER.EQ.N_ELE) THEN
	  SELFLAG=FL_SHAD			!DEFAULT FLAG TYPE: SHADOW
          ELELIMIT = 10.                        !DEFAULT 10 DEG
 228      CONTINUE
	  IF (.NOT.WNDPAR('ELEVATION_LIMIT',ELELIMIT,LB_E,
     1                           J0,A_B(-A_OB),ELELIMIT,1)) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 800
	    GOTO 228
	  END IF
	  ELELIMIT=ELELIMIT/360.		!MAKE CIRCLES
C
!*** SHADOW
C
	ELSE IF (NOPER.EQ.N_SHA) THEN
	  SELFLAG=FL_SHAD			!DEFAULT FLAG TYPE: SHADOW
          SHADIAM = 25                          !WSRT TEL DIAMETER (M)
 218      CONTINUE
	  IF (.NOT.WNDPAR('SHADOW_DIAM',SHADIAM,LB_E,
     1                           J0,A_B(-A_OB),SHADIAM,1)) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 800
	    GOTO 218
	  END IF
C
!*** PROJECTED BASELINE:
C
	ELSE IF (NOPER.EQ.N_PBA) THEN
	  SELFLAG=FL_SHAD			!DEFAULT FLAG TYPE: SHADOW
 219      CONTINUE
	  IF (.NOT.WNDPAR('PBAS_LIMITS',PBASLIM,2*LB_E,
     1                           J0,A_B(-A_OB),PBASLIM,2)) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 800
	    GOTO 219
	  END IF
C
!*** CONTROL BUILDING INTERFERENCE:
C
	ELSE IF (NOPER.EQ.N_CBI) THEN
	  SELFLAG=FL_SHAD			!DEFAULT FLAG TYPE: SHADOW
	  CALL WNCTXT(F_TP,'Operation not available yet.')
          GOTO 700                              !ESCAPE
C
!*** RED, NORED
C
	ELSE IF (NOPER.EQ.N_RED .OR. NOPER.EQ.N_NON) THEN
	  SELFLAG=FL_SHAD
C
!*** UNKNOWN FLAGGING OPERATION:
C
	ELSE
	  CALL WNCTXT(F_T,'NFLOPS: Unknown flagging operation')
	  GOTO 800                              !ESCAPE 
	END IF
C
C----------------------------------------------------------------------------
C  DRYRUN:
C
	IF (DRYRUN) THEN
	  NDRYSCANS = 0                         !RESET COUNTER
	END IF
C
C----------------------------------------------------------------------------
C  CORRECTED DATA: 
C
	IF (CORRDAT) THEN
          CALL WNDDAP(CAP,CDAP)             !GET GLOBAL (DE-)APPLY BITS
	  DUFLAG = FL_ALL                   !INHIBIT DATA SELECTION BY FLAGS
	  CALL WNDDUF_SET(DUFLAG)           !OVERRIDE GLOBAL USER FLAG
        END IF
C
	IF (TRACE) CALL WNCTXT (F_T,'NFLOPS: CORRDAT=!LJ',CORRDAT)
C
C----------------------------------------------------------------------------
C SELFLAG: The default flag-type(s) for each flagging operation 
C can be overridden by user-selected flag-type(s), given as the
C input argument USERFLAG (if USERFLAG.NE.0).

C  
	IF (USERFLAG.NE.0) SELFLAG = USERFLAG      !OVERRIDE WITH USER FLAG
	IF (CFLAG.NE.0) SELFLAG = CFLAG            !OVERRIDE WITH LOCAL FLAG
        CFLAG = 0                                  !ALWAYS
	IF (SELFLAG.EQ.0) THEN
	  CALL WNCTXT (F_TP,
     1      'No flag type(s) selected for this operation.')
	  GOTO 700                                 !ESCAPE
	END IF
C
        TXT80 = ' '
        I=1
        DO I6=0,MXNFLTYP-1
          IF (IAND(SELFLAG,FLAGTYPE(I6)).NE.0) THEN
            CALL WNCTXS (TXT80(I:),' !4$AS',FLAGNAME(I6)) 
            IF (FLAGTYPE(I6).EQ.FL_ALL) TXT80(I:) = ' '
            I=I+5
          END IF
        END DO
        IF (TRACE) CALL WNCTXT (F_TP,'Flag-types used: !AS',TXT80)
C
C*****************************************************************************
C*****************************************************************************
C*****************************************************************************
C ACT ON HYPERCUBE
C
 300    CONTINUE
C   
C Reset the flag-count buffers and the data statistics accumulators: 
C
        JS = NFLCNT ('RESET',' ',0,0,0,0,0)          !FLAG COUNTERS
C
C Reset the data statistics accumulators: 
C Assign some named accumulator slots that might be needed.
C (NB: Other slots will be assigned when necessary below)
C
        R0 = NFLST1('RESET','#ALLGROUPS',' ',0,0.,0.)   !Reset all accum. grps
C
        R0 = NFLST1('ASSIGN','MAXABCS','UNIT=W.U.',1,0.,0.) 
        R0 = NFLST1('ASSIGN','REDNS_GX','UNIT=W.U.',1,0.,0.) 
        R0 = NFLST1('ASSIGN','REDNS_PX','UNIT=degr',1,0.,0.) 
        R0 = NFLST1('ASSIGN','REDNS_GY','UNIT=W.U.',1,0.,0.) 
        R0 = NFLST1('ASSIGN','REDNS_PY','UNIT=degr',1,0.,0.) 
        R0 = NFLST1('ASSIGN','ALGNS_GX','UNIT=W.U.',1,0.,0.) 
        R0 = NFLST1('ASSIGN','ALGNS_PX','UNIT=degr',1,0.,0.) 
        R0 = NFLST1('ASSIGN','ALGNS_GY','UNIT=W.U.',1,0.,0.) 
        R0 = NFLST1('ASSIGN','ALGNS_PY','UNIT=degr',1,0.,0.) 
	DO I3=0,3
          R0 = NFLST1('ASSIGN','DAT_A_'//POLNAME(I3),
     1                'UNIT=W.U.',STHIFR,0.,0.) 
          R0 = NFLST1('ASSIGN','DAT_P_'//POLNAME(I3),
     1                'UNIT=degr',STHIFR,0.,0.) 
          R0 = NFLST1('ASSIGN','DAT_C_'//POLNAME(I3),
     1                'UNIT=W.U.',STHIFR,0.,0.) 
          R0 = NFLST1('ASSIGN','DAT_S_'//POLNAME(I3),
     1                'UNIT=W.U.',STHIFR,0.,0.) 
        END DO
C
C  Initialise the buffers for keeping data and HA of the last Scan,
C  in such a way that they will be ignored for the first Scan.
C  NB: This used to be inside the Sector-loop, because there is
C      usually a discontinuity between Sectors. However, for mosaic
C      data, all HA-Scans belonging to a particular pointing centre 
C      are stored in separate Sectors, and the WENSS team wishes to
C      look for "un-physical" jumps in the residues.......
C      Some more thought may be needed here.   
C
          DO I3=0,3                               !ALL POLS
            DO I1=0,STHIFR-1                      !ALL IFRS
              DO I=0,1                            !
                HALAST(I1,I3,I) = VERYLARGE       !HA OF LAST SCAN
                CDATLAST(I1,I3,I) = 0             !DATA IN LAST SCAN
              END DO
            END DO
          END DO
C
C Flow control for Scan headers (for data, see below):
C
        MODFH_DFLT = .false.                  !MODIFY SWITCH FOR HEADERS
        SETFH_DFLT = SETFLAG                  !SET/CLEAR SWITCH
        FLAGH_DFLT = SELFLAG                  !FLAGBYTE FOR HEADERS
	CNTFH_DFLT = .FALSE.                  !COUNT FLAGS IN HEADERS
	STACCH_DFLT = .FALSE.                 !ACCUM. STATISTICS FROM HEADERS
C
C=============================================================================
C Read Set(s) of Sectors:
C  
	DO WHILE (NSCSTG(FCAIN,SETS,STH,STHP,SNAM)) !ALL SETS
	  IF (.NOT.NSCSIF(FCAIN,STH,IFRT,IFRA,ANG)) THEN !READ IFR TABLE
	    CALL WNCTXT(F_TP,'Error reading interferometer table')
	    GOTO 30				!NEXT SET
	  END IF
C
C---------------------------------------------------------------------------
C  Do some initialising per Sector:
C
	  CHCUR = STHI(STH_CHAN_I)		!CURRENT CHANNEL
	  HAINC = STHE(STH_HAI_E)               !HA increment (circles)
C
C  Adjust the polarisation selection according to how many polarisations
C  are available in this Sector. For security, return to the sub-cube
C  selection first.....(?):
C
          JS = NFLCUB ('SELECT','SUBCUBE',0,SELHA,SELPOL,SELIFR)
          JS = NFLCUB ('ADJUST','SELPOL',STHI(STH_PLN_I),
     1                 SELHA,SELPOL,SELIFR)
C
C  Read baselines
C
	  CALL NSCMBL(STHE(STH_RTP_E),STHJ(STH_NIFR_J),IFRT,
     1		SELIFR,BASEL)                   
C
C Bookkeeping for statistics:
C
	  CALL NFLST0 ('SET','IFRTABLE',2*STHIFR,IFRA,0.)
	  CALL NFLST0 ('SET','BASEL',STHIFR,0,BASEL)     !Baselines (m)
	  CALL NFLST0 ('SET','DIPOS',STHIFR,0,ANG)       !Dipole angles
	  CALL NFLST0 ('ADD','CHAN',1,CHCUR,0.)          !Channel nr
C
C  Determine IFRMIN,MAX and POLMIN,MAX, to minimise processing later:
C
          POLMIN = 3
          POLMAX = 0
          DO I3=0,3                               !ALL POLS
            IF (SELPOL(I3)) THEN                  !SELECTED POL
              POLMIN = MIN(POLMIN,I3)
              POLMAX = MAX(POLMAX,I3)
            END IF
          END DO
C
	  IFRMIN = STHIFR-1
          IFRMAX = 0
          DO I1=0,STHJ(STH_NIFR_J)-1            !ALL IFRS
            RTW = IFRA(0,I1)                    !WEST TEL
            RTE = IFRA(1,I1)                    !EAST TEL
            IF (SELIFR(RTW,RTE)) THEN           !SELECTED IFR
              IFRMIN = MIN(IFRMIN,I1)
              IFRMAX = MAX(IFRMAX,I1)
            END IF
          END DO
C
C Progress message (if TRACE):
C
	  TXT80 = WNTTSG(SNAM,-1)               !SET NAME STRING
	  IF (TRACE) CALL WNCTXT (F_T,'NFLOPS: '//TXT80(:10)//':'
     1                //' IFRMIN,MAX=!SJ:!SJ '
     1                //' POLMIN,MAX=!SJ:!SJ '
     1                ,IFRMIN,IFRMAX,POLMIN,POLMAX)    !Temporary
C
C---------------------------------------------------------------------------
C  Get extra information for some flagging actions:
C
!*** SHADOW, PROJECTED BASELINE:
C
	  IF (NOPER.EQ.N_SHA .OR. NOPER.EQ.N_PBA) THEN
	    CALL NSCMBL(STHE(STH_RTP_E),STHJ(STH_NIFR_J),IFRT,
     1		SELIFR,BASEL)                    !GET BASELINES
	    PBAS0=COS(STHD(STH_DEC_D)*DPI2)**2	!COS(DEC)**2
	    IF (TRACE) CALL WNCTXT (F_T,' SHA:'
     1                 //' PBAS0=!E6.3 DEC=!DAF6.1'
     1                 ,PBAS0,STHD(STH_DEC_D))
	  ELSE IF (NOPER.EQ.N_ELE) THEN
	    DEC=STHD(STH_DEC_D)*DPI2
C
!*** CONTROL BUILDING INTERFERENCE:
C    NB: The interference gets in via the telescope beam side-lobes,
C        so we cannot use the theoretical (COS**6) expression here.
C        The correct `beamshape' will be developed by experimentation later.
C
	  ELSE IF (NOPER.EQ.N_CBI) THEN
	    CALL NSCMBL(STHE(STH_RTP_E),STHJ(STH_NIFR_J),IFRT,
     1	                        SELIFR,BASEL)   !GET BASELINES
            DEC = STHD(STH_DEC_D)*DPI2          !POINTING DEC (RAD) 
	    PBAS0=COS(DEC)**2	                !BASELINE PROJECTION FACTOR
            WSRTLAT = 52 * (360/PI2)            !WSRT LATITUDE (RAD)
            FRQ = STHD(STH_FRQ0_D)              !FREQUENCY (MHZ)..???
CCCCCCCC    BEAMFAC = NMOBMF (FRQ)              !For theoretical (COS**6) beam
            CBIBPAR(0) = 1                      !CBI `BEAMSHAPE' PARAM
            CBIBINC = 1                         !INCREMENT IN CBIBEAM
            DO I1=0,CBIBMAX
              R0 = EXP(-(I1*CBIBINC/CBIBPAR(0))**2) !GAUSSIAN? (SINC?)
              CBIBEAM(I1) = R0                   !RADIAL CBI `BEAM-SHAPE'
            END DO
C
!*** QXY: Only if both XX and YY are available, and selected:
C
	  ELSE IF (NOPER.EQ.N_QXY) THEN
	    IF (SELPOL(XX).AND.SELPOL(YY)) THEN
              CONTINUE
	    ELSE
	      CALL WNCTXT (F_TP,'NFLOPS option QXY:'
     1                     //' Both XX and YY data are needed!')
              GOTO 30                            !ESCAPE, NEXT SECTOR
            END IF
C
!*** YXY,UXY,VXY: Only if both XY and YX are available, and selected:
C
	  ELSE IF (NOPER.EQ.N_YXY .OR.
     1             NOPER.EQ.N_UXY .OR.
     1             NOPER.EQ.N_VXY) THEN
	    IF (SELPOL(XY).AND.SELPOL(YX)) THEN
              CONTINUE
	    ELSE
	      CALL WNCTXT (F_TP,'NFLOPS option YXY,UXY,VXY:'
     1                     //' Both XY and YX data are needed!')
              GOTO 30                            !ESCAPE, NEXT SECTOR
            END IF
C
!*** RED, NORED, RRESID
C
	  ELSE IF (NOPER.EQ.N_RED .OR. 
     1             NOPER.EQ.N_NON .OR.
     1	           NOPER.EQ.N_RRE) THEN	            !GET REDUNDANT BASELINES
	    CALL NSCMBL(STHE(STH_RTP_E),STHJ(STH_NIFR_J),IFRT,
     1		SELIFR,BASEL)
	    CALL NCARRT(STHJ(STH_NIFR_J),BASEL,1E0,IRED,ANG)
C
	  END IF	  
C
C******************************************************************************
C ACT ON HA-SCANS
C
 400      CONTINUE
C
	  DO I=0,STHJ(STH_SCN_J)-1		                !ALL SCANS
	    HACUR=STHE(STH_HAB_E)+I*STHE(STH_HAI_E)             !HA OF SCAN
            IF (HACUR.GE.(SELHA(0)-HAINC/2+1E-5) .AND.
     1		HACUR.LE.(SELHA(1)+HAINC/2-1E-5)) THEN          !SELECTED 
C
              IF (.NOT.NSCSCH(FCAIN,STH,IFRT,I,0,0,SCH)) THEN !READ HEADER
		CALL WNCTXT(F_TP,'Error reading scan header !UJ',I)
		GOTO 30
              END IF
C
C Bookkeeping for statistics:
C
	      CALL NFLST0 ('ADD','HACIR',1,0,HACUR)
C
C Reset the actual switches for treatment of Scan header flags:
C
              MODFH  = MODFH_DFLT		!MODIFY HEADER FLAGS
	      FLAGH  = FLAGH_DFLT		!FLAG TO BE USED FOR HEADER
	      SETFH  = SETFH_DFLT		!SET/CLEAR FOR HEADER
	      CNTFH  = CNTFH_DFLT		!COUNT FLAG IN HEADER
	      STACCH = STACCH_DFLT              !ACCUM HEADER STATISTICS
C
C Set default switches for treatment of uv-data flags:
C
	      MODFD_DFLT  = .FALSE.		!NO MODIFICATION OF DATA FLAGS
	      FLAGD_DFLT  = SELFLAG		!FLAG TO USE FOR DATA
	      SETFD_DFLT  = SETFLAG		!SET/CLEAR FOR DATA
	      CNTFD_DFLT  = .FALSE.		!NO COUNT FLAG IN DATA
	      STACCD_DFLT = .FALSE.             !NO ACCUM DATA STATISTICS
C
C Some initial settings per Scan:
C
	      SELECT = .FALSE.                  !SELECTION CRITERION NOT MET
	      CHKDATA=.TRUE.			!DEFAULT: CHECK DATA (???)
              WRSCH=.FALSE.			!NO REWRITE SCAN HEADER
              WRSCN=.FALSE.			!NO REWRITE SCAN DATA
	      CRITVALH = 0                      !FLAGGING CRITERION VALUE
	      STATWGTH = 0                      !ACCUMULATION WEIGHT
C
!*** DRY RUN:
C
	      IF (DRYRUN) THEN
		STACCH = .TRUE.                 !ACCUM. HEADER STATISTICS    
		STACCD_DFLT = .TRUE.            !ACCUM. DATA STATISTICS       
	      END IF
C
!*** COUNT (flags):
C
	      IF (NOPER.EQ.N_COU) THEN
		CNTFH = .TRUE.                  !COUNT HEADER FLAG
		CNTFD_DFLT = .TRUE.                 !COUNT DATA FLAGS
C
!*** STATISTICS (of header info and /or uv-data):
C
	      ELSE IF (NOPER.EQ.N_ACC) THEN
		STACCH = .TRUE.                  !       
		STACCD_DFLT = .TRUE.                 !       
		CNTFH = .TRUE.                  !COUNT HEADER FLAG
		CNTFD_DFLT = .TRUE.                 !COUNT DATA FLAGS
	      ELSE IF (NOPER.EQ.N_ACH) THEN
                CHKDATA = .FALSE.               !HEADERS ONLY
		STACCH = .TRUE.                  !       
		CNTFH = .TRUE.                  !COUNT HEADER FLAG
	      ELSE IF (NOPER.EQ.N_ACD) THEN
		STACCD_DFLT = .TRUE.                 !       
		CNTFD_DFLT = .TRUE.                 !COUNT DATA FLAGS
C
!*** CLEAR FLAGS
C
	      ELSE IF (NOPER.EQ.N_CLE .OR.
     1                 NOPER.EQ.N_CLD .OR.
     1                 NOPER.EQ.N_CLH) THEN
                MODFH = .TRUE.              !MODIFY SCAN HEADER FLAGS
                MODFD_DFLT = .TRUE.             !MODIFY ALL UV-DATA FLAGS
		CHKDATA = .FALSE.           !WITHOUT EVEN CHECKING IT
                SETFH = .FALSE.             !CLEAR HEADER FLAG(S)
                SETFD_DFLT = .FALSE.            !CLEAR DATA FLAG(S)
	        IF (NOPER.EQ.N_CLH) MODFD_DFLT = .FALSE.    !HEADERS ONLY
	        IF (NOPER.EQ.N_CLD) MODFH = .FALSE.     !DATA ONLY
C
!*** TOHEAD
C
		ELSE IF (NOPER.EQ.N_TOH) THEN
		  TOH_COUNT = 0			!RESET FLAG COUNTER
		  FLAGH = 0			!INITIALISE NEW HEADER FLAG 
C
!*** TODATA
C
		ELSE IF (NOPER.EQ.N_TOD) THEN
		  FLAGH = SCHJ(SCH_BITS_J)      !COPY HEADER FLAG(S)
		  FLAGD_DFLT = IAND(FLAGH,SELFLAG) !SELECTED FLAG TYPES ONLY
                  MODFD_DFLT = .TRUE.           !MODIFY DATA FLAGS always
                  CHKDATA = .FALSE.             !DO NOT LOOK AT DATA ITSELF
                  MODFH = .TRUE.                !MODIFY HEADER FLAG(S)
                  SETFH = .FALSE.               !CLEAR HEADER FLAG(S)
C
!*** SHADOW, PROJECTED BASELINE:
C
		ELSE IF (NOPER.EQ.N_SHA .OR. NOPER.EQ.N_PBA) THEN
                  PBAS=SQRT(MAX(0.,1.-PBAS0*
     1		       (SIN(HACUR*PI2)**2)))	!BASELINE PROJECTION FACTOR
C
                  IF (NOPER.EQ.N_SHA) THEN      !SHADOW ONLY
                    DO I1=0,STHTEL-1
                      SHATEL(I1)=.FALSE.        !TEL NOT SHADOWED
                    END DO
                    I6=0                        !WEST TELS ARE SHADOWED
                    IF (HACUR.GE.0) I6=1        !EAST TELS ARE SHADOWED
                    DO I1=0,STHJ(STH_NIFR_J)-1  !ALL IFRS
		      IF (BASEL(I1).GT.0) THEN  !BASELINE LENGTH (M)
                        IF ((BASEL(I1)*PBAS).LE.SHADIAM) THEN
                          SHATEL(IFRA(I6,I1))=.TRUE.   !TEL IS SHADOWED
                        END IF
                      END IF
                    END DO
                  END IF
C
!*** ELEVATION
C
		ELSE IF (NOPER.EQ.N_ELE) THEN
                  ELEV = ASIN(SIN(DEC)*SLATW + 
	1		      COS(DEC)*COS(HACUR*PI2)*CLATW)    !ELEVATION
	          MODFD_DFLT=(ELEV.LT.ELELIMIT*PI2)		!TOO LOW?
	 	  CHKDATA = .FALSE.                   !DO NOT LOOK AT DATA
C
!*** CONTROL BUILDING INTERFERENCE: Figure out the angle (rad) between the
!    the control building (CB) and the pointing centre of each telescope.
!    as a function of telescope position, source HA and elevation. 
!    Then calculate the beam gain factor (max=1) for each telescope,
!    using a tentative CBI `beamshape', which includes the sidelobes. 
!    NB: The CB (i.e. correlator-cage) is at 760m east of RT0, and 60m south.
!    NB: This algorithm is still experimental, and will be worked upon.
C
                ELSE IF (NOPER.EQ.N_CBI) THEN
                  ELEV = DEC + ((PI/2)-WSRTLAT)*COS(HACUR*PI2)  !ELEVATION
                  DO I4 = 0,STHTEL-1                 !ALL TELESCOPES
                    R0 = STHE(STH_RTP_E+I4)          !RT POSITION (M)
                    CBDIR = ATAN2(60.,R0-760.)      !DIRECTION OF CB ....
                    CBDIST = SQRT(60.**2+(R0-760.)**2) !DISTANCE TO CB
                    LREL = CBDIR-HACUR              !ARCSEC?
                    MREL = ELEV                     !ARCSEC?
CCCC                NB: L,M should be rotated by angle (90-WSRTLAT)*COS(HA) 
CCCC                CBIFACT(I4) = NMOBMV (FRQ,BEAMFAC,LREL,MREL,
CCCC 1                 BEMLIM,.TRUE.)  !Theoretical (COS**6) beam: do not use!
                    CBIFACT(I4) = 0                  !TELESCOPE CBI GAIN FACTOR
                    R1 = ATAN2(LREL,MREL)           !BEAM ANGLE (RAD) OF CB ?? 
                    J1 = NINT(ABS(R1)/CBIBINC)      !INDEX IN CBIBEAM
                    IF (J1.LE.CBIBMAX) CBIFACT(I4) = CBIBEAM(J1)
                    CBIFACT(I4) = CBIFACT(I4)/(CBDIST**2) !CORRECT FOR DISTANCE
                  END DO
C
!*** SCANS:
C
		ELSE IF (NOPER.EQ.N_SCA) THEN
                  MODFH = .TRUE.                    !MODIFY HEADER FLAGS    
	 	  CHKDATA = .FALSE.                 !DO NOT LOOK AT DATA
C
!*** DATA:
C
                ELSE IF (NOPER.EQ.N_UVD) THEN
                  MODFD_DFLT = .TRUE.               !MODIFY UV-DATA FLAGS
	 	  CHKDATA = .FALSE.                 !DO NOT LOOK AT DATA
C
!*** HA-RANGE:
C
	        ELSE IF (NOPER.EQ.N_HAR) THEN
	          MODFD_DFLT=
     1		     (HACUR.GE.(HAR(0)-HAINC/2+1E-5) .AND.
     1		      HACUR.LE.(HAR(1)+HAINC/2-1E-5)) !MODIFY UV-DATA FLAGS
	 	  CHKDATA = .FALSE.                   !DO NOT LOOK AT DATA
C
!*** MAX (ABS(COS),ABS(SIN)):
C
		ELSE IF (NOPER.EQ.N_MAX) THEN
	          CHKDATA=.FALSE.		!SKIP DATA
		  STACCH = .TRUE.               !ACCUMULATE STATISTICS
		  CRITVALH = SCHE(SCH_MAX_E)
                  STATWGTH = 1.                 !ACCUMULATION WEIGHT
		  IF (CRITVALH.LT.MXLIM(0)) SELECT = .TRUE.  !TOO SMALL
		  IF (CRITVALH.GT.MXLIM(1)) SELECT = .TRUE.  !TOO LARGE
                  MODFH = .TRUE.     !MODIFY HEADER FLAGS (ALWAYS)
                  SETFH = SELECT     !SET FLAG IF SELECTED, CLEAR OTHERWISE    
C
!*** RED/ALG Noises:
C
		ELSE IF (
     1              NOPER.EQ.N_RNO .OR. NOPER.EQ.N_ANO .OR.
     1              NOPER.EQ.N_XRN .OR. NOPER.EQ.N_YRN .OR.
     1              NOPER.EQ.N_XAN .OR. NOPER.EQ.N_YAN) THEN
C
                  MODFH = .TRUE.                !MODIFY HEADER FLAGS (ALWAYS)
	          CHKDATA=.FALSE.		!DO NOT LOOK AT UV-DATA
		  STACCH = .TRUE.               !ACCUMULATE HEADER STATISTICS
                  CRITVALH = 0                  !CRITERION VALUE (HEADER)
                  STATWGTH = 1.                 !CRITVALH STATISTICS WEIGHT 
C
		  IF (NOPER.EQ.N_RNO) THEN
                    DO I4=0,3                   !Gain/phase, X,Y
		      CRITVALH = MAX(CRITVALH,SCHE(SCH_REDNS_E+I4))
		      IF (SCHE(SCH_REDNS_E+I4).LT.0) SELECT=.TRUE.
                    END DO
		  ELSE IF (NOPER.EQ.N_XRN) THEN
                    DO I4=0,1                   !Gain/phase, X only
		      CRITVALH = MAX(CRITVALH,SCHE(SCH_REDNS_E+I4))
		      IF (SCHE(SCH_REDNS_E+I4).LT.0) SELECT=.TRUE.
                    END DO
		  ELSE IF (NOPER.EQ.N_YRN) THEN
                    DO I4=2,3                   !Gain/phase, Y only
		      CRITVALH = MAX(CRITVALH,SCHE(SCH_REDNS_E+I4))
		      IF (SCHE(SCH_REDNS_E+I4).LT.0) SELECT=.TRUE.
                    END DO
C
		  ELSE IF (NOPER.EQ.N_ANO) THEN
                    DO I4=0,3                   !Gain/phase, X,Y
		      CRITVALH = MAX(CRITVALH,SCHE(SCH_ALGNS_E+I4))
		      IF (SCHE(SCH_ALGNS_E+I4).LT.0) SELECT=.TRUE.
                    END DO
		  ELSE IF (NOPER.EQ.N_XAN) THEN
                    DO I4=0,1                   !Gain/phase, X only
		      CRITVALH = MAX(CRITVALH,SCHE(SCH_ALGNS_E+I4))
		      IF (SCHE(SCH_ALGNS_E+I4).LT.0) SELECT=.TRUE.
                    END DO
		  ELSE IF (NOPER.EQ.N_YAN) THEN
                    DO I4=2,3                   !Gain/phase, Y only
		      CRITVALH = MAX(CRITVALH,SCHE(SCH_ALGNS_E+I4))
		      IF (SCHE(SCH_ALGNS_E+I4).LT.0) SELECT=.TRUE.
                    END DO
		  END IF
C
		  IF (CRITVALH.LT.MXLIM(0)) SELECT = .TRUE.  !TOO SMALL
		  IF (CRITVALH.GT.MXLIM(1)) SELECT = .TRUE.  !TOO LARGE
                  SETFH = SELECT     !SET FLAG IF SELECTED, CLEAR OTHERWISE    
C
	        END IF
C
C*****************************************************************************
C READ SCAN DATA (If required):
C
                IF ((.NOT.CHKDATA).AND.(.NOT.MODFD_DFLT)) GOTO 40  !SKIP
C
		IF (.NOT.WNFRD(FCAIN,STHJ(STH_SCNL_J)-SCH__L,
     1              LDAT,STHJ(STH_SCNP_J)+I*STHJ(STH_SCNL_J)+
     1              SCH__L)) THEN                     !READ SCAN DATA
		  CALL WNCTXT(F_TP,'Error reading scan !UJ',I)
		  GOTO 800
		END IF
C
!*** TOTEL,TOPOL: Make a copy (LDAT1) of the uv-data (LDAT) for later use.
C
	        IF (NOPER.EQ.N_TOT .OR. NOPER.EQ.N_TOP) THEN
		  CALL WNGMV(STHJ(STH_SCNL_J)-SCH__L,LDAT,LDAT1) !COPY DATA
		END IF
C
!*** APPLY/DE-APPLY CORRECTIONS TO THE UV-DATA (if required):
C
		IF (CORRDAT) THEN
	          IF (.NOT.NSCSCR(FCAIN,STH,IFRT,I,
     1                  CAP,CDAP,SCH,WGT,CDAT)) THEN
		    CALL WNCTXT(F_TP,
     1                  'Error reading scan !UJ (data)',I)
		    GOTO 30                         !NEXT SECTOR (?)
		  END IF
		END IF
C
!*** ARESID, RT1:
C NSCSCM reads the stored uv-model into CAMOD! (parallel dipoles only!) 
C NB: The model is NEGATIVE!
C
		IF ((NOPER.EQ.N_ARE) .OR.      
     1              (NOPER.EQ.N_RT1)) THEN    
	          IF (.NOT.NSCSCM(FCAIN,STH,IFRT,I,
     1                  CAP,CDAP,SCH,WGT,CAMOD)) THEN
		    CALL WNCTXT(F_TP,
     1                  'Error reading scan !UJ (model)',I)
		    GOTO 30                         !NEXT SECTOR (?)
		  END IF
C
!*** RRESID
C
		ELSE IF (NOPER.EQ.N_RRE) THEN		!CALCULATE "MODEL"
		  DO I1=0,STHJ(STH_NIFR_J)-1	!ZERO CELESTIAL DATA
		    DO I3=0,3,3			!XX,YY
		      CAMOD(I1,I3)=0
		      MWGT(I3,I1)=0
		    END DO
		  END DO
		  DO I1=0,STHJ(STH_NIFR_J)-1
		    I2=IRED(I1)			!REDUNDANT?
		    IF (I2.GT.0) THEN		!CAN USE
		      DO I3=0,3,3		!XX,YY
		        IF (WGT(I1,I3).GT.0) THEN !CAN USE
			  CAMOD(I2,I3)=CAMOD(I2,I3)+WGT(I1,I3)*CDAT(I1,I3) !SUM
			  MWGT(I3,I2)=MWGT(I3,I2)+WGT(I1,I3)
			END IF
		      END DO
		    END IF
		  END DO
		  DO I1=0,STHJ(STH_NIFR_J)-1	!SOLVE
		    DO I3=0,3,3
		      IF (MWGT(I3,I1).GT.0)
     1			CAMOD(I1,I3)=CAMOD(I1,I3)/MWGT(I3,I1)
		    END DO
		  END DO
C
		END IF
C
C******************************************************************************
C ACT ON UV-DATA
C
 500            CONTINUE
C
C Set flag-count and statistics accumulation buffers to undefined:
C NB: The flags and values per uv-data point are stored per Scan, 
C     so that they can be processed in a single call to NFLCNT/NFLSTn.
C     (Separate calls for each uv-data point take too much time).
C
		DO I3=0,3				   !ALL POLS
		  VALIDAT(I3) = .FALSE.
		  DO I1=0,STHIFR-1	                   !ALL IFRS
                    FLACC(I1,I3) = 0                       !Flag counts
                    MASK(I1,I3) = 0                        !Masks used
                    STATWGTD(I1,I3) = 0.                   !Statistics weight
                    CRITVALD(I1,I3) = 0.                   !Criterion value
                  END DO
                END DO
C
C Go through the uv-data:
C
		DO I1=IFRMIN,IFRMAX	                   !ALL relevant IFRS
                  RTW = IFRA(0,I1)                         !WEST TELESCOPE
                  RTE = IFRA(1,I1)                         !EAST TELESCOPE
CC		  RTW = MOD(IFRT(I1),256)                  !Alternative...
CC		  RTE = IFRT(I1)/256                       !Alternative...
		  IF (SELIFR(RTW,RTE)) THEN                !SELECTED IFR 
		    I2=STHI(STH_PLN_I)*I1	           !DATA POINTER
		    DO I3=POLMIN,POLMAX			   !ALL POLS
                      VALIDAT(I3) = .FALSE.
                      IF (SELPOL(I3)) THEN                 !SELECTED POL
		        LDOFF(I3)=I2+PPOL(I3,STHI(STH_PLN_I),1) !OFFSET
		        WFDAT(I3)=LDAT(0,LDOFF(I3))             !WEIGHT/FLAGS
		        WFDAT(I3)=IAND('0000ffff'X,WFDAT(I3))   !WEIGHT/FLAGS 
		        IF (WFDAT(I3).NE.0) THEN      !DATA PRESENT (WEIGHT<>0)
                          VALIDAT(I3) = .TRUE.	      !VALID DATA
C
                          MODFD(I3)  = MODFD_DFLT     !MODIFY FLAGS, OR NOT
                          FLAGD(I3)  = FLAGD_DFLT     !FLAGBYTE TO BE USED
                          SETFD(I3)  = SETFD_DFLT     !SET/CLEAR FLAG(S)
			  CNTFD(I3)  = CNTFD_DFLT     !COUNT FLAGS
			  STACCD(I3) = STACCD_DFLT    !ACCUM. DATA STATISTICS
C
                          SELECT = .FALSE.           !ASSUME CRITERION NOT MET
                          IF (.NOT.CORRDAT) THEN     !UV-DATA ALWAYS IN CDAT!
                            DAT(0,I1,I3) = REAL(LDAT(1,LDOFF(I3)))  !COS
                            DAT(1,I1,I3) = REAL(LDAT(2,LDOFF(I3)))  !SIN
                          END IF
			  IF (NOPER.EQ.N_RT1) THEN
			    CDAT(I1,I3)=CDAT(I1,I3)+CAMOD(I1,I3)  !RESIDUE!!!
                          END IF
                          STATWGTD(I1,I3) = 1.   !STATISTICS WEIGHT NON-ZERO
C
!  Check the current uv-data value for various flagging operations:
C
			  IF (.NOT.CHKDATA) THEN
                            CONTINUE                  !NO NOT CHECK UV-DATA 
C
!*** CLIP
C
			  ELSE IF (NOPER.EQ.N_AMP .OR.
     1                             NOPER.EQ.N_COS .OR.
     1                             NOPER.EQ.N_SIN) THEN
                            IF (NOPER.EQ.N_AMP) R1=ABS(CDAT(I1,I3))  !AMPLITUDE
                            IF (NOPER.EQ.N_COS) R1=ABS(DAT(0,I1,I3)) !ABS(COS)
                            IF (NOPER.EQ.N_SIN) R1=ABS(DAT(1,I1,I3)) !ABS(SIN)
                            CRITVALD(I1,I3) = R1         !STORE FOR STATISTICS
			    IF (R1.LT.MXLIM(0)) SELECT=.TRUE.      !TOO SMALL
			    IF (R1.GT.MXLIM(1)) SELECT=.TRUE.      !TOO LARGE
                            MODFD(I3) = .TRUE.      !MODIFY DATA FLAGS (ALWAYS)
                            SETFD(I3) = SELECT      !SET/CLEAR DATA FLAGS    
C
!*** ARESID
C
			  ELSE IF (NOPER.EQ.N_ARE) THEN
			    R1=ABS(CDAT(I1,I3)+CAMOD(I1,I3))
                            CRITVALD(I1,I3) = R1      !STORE FOR STATISTICS
			    IF (R1.GT.MXLIM(1)) SELECT=.TRUE.  !DO CLIP 
                            MODFD(I3) = .TRUE.     !MODIFY DATA FLAGS (ALWAYS)
                            SETFD(I3) = SELECT     !SET/CLEAR DATA FLAGS    
C
                            IF (TRACE) THEN        !TRACE WHAT HAPPENS
                              CALL WNCTXT (F_T,'ARESID: HA=!7$EA7.2:'
     1                          //' DAT=!12$EC6.0'
     1                          //' MOD=!12$EC6.0'
     1                          //' R1=!6$E6.0(!5$E5.0) !LJ'
     1                          ,HACUR,CDAT(I1,I3),-CAMOD(I1,I3)
     1                          ,R1,MXLIM(1),IAND(WFDAT(I3),FL_ALL))
                            END IF
C
!*** RRESID
C
			  ELSE IF (NOPER.EQ.N_RRE) THEN
			    IF (IRED(I1).GT.0) THEN    !REDUNDANT BASELINE
			      IF (I3.LT.3) THEN	       !CHECK XX
				R1=ABS(CDAT(I1,0)-CAMOD(IRED(I1),0))
                                CRITVALD(I1,I3) = R1    !STORE FOR STATISTICS
			        IF (R1.GT.MXLIM(1)) SELECT=.TRUE. !DO CLIP 
			      END IF
			      IF (I3.GT.0) THEN	       !CHECK YY
				R1=ABS(CDAT(I1,3)-CAMOD(IRED(I1),3))
                                CRITVALD(I1,I3) = R1    !STORE FOR STATISTICS
			        IF (R1.GT.MXLIM(1)) SELECT=.TRUE. !DO CLIP 
			      END IF
                            END IF
                            MODFD(I3) = .TRUE.     !MODIFY DATA FLAGS (ALWAYS)
                            SETFD(I3) = SELECT     !SET/CLEAR DATA FLAGS    
C
                            IF (TRACE) THEN           !TRACE WHAT HAPPENS
                              CALL WNCTXT (F_T,'RRESID: HA=!6$EA6.1:'
     1                          //' DAT=!12$EC6.0'
     1                          //' MOD=!12$EC6.0'
     1                          //' R1=!6$E6.0(!5$E5.0)',HACUR
     1                          ,CDAT(I1,I3),CAMOD(IRED(I1),0)
     1                          ,R1,MXLIM(1))
                            END IF
C
!*** DT1: Compare the amplitude (?) of the current uv-point with that of its
!         predecessor in the last selected Scan, and calculate dA/dHA.
!         If the difference is greater than the specified limit (i.e. if
!         it is an `unphysical jump'), the uv-point is `selected'.
!         NB: Selected points are flagged, un-selected points are unflagged!
!         Experimentally, the difference with the last UNFLAGGED uv-point
!         is also considered, to take care of all kinds of situations.
C*** RT1: Same as DT1, but for residues (i.e. data-model). CDAT already
C         contains the residues (done above). 
C
                          ELSE IF ((NOPER.EQ.N_DT1) .OR.
     1                             (NOPER.EQ.N_RT1)) THEN
			    R1=ABS(CDAT(I1,I3)-CDATLAST(I1,I3,1)) !LAST POINT
                            DHA=(HACUR-HALAST(I1,I3,1))/HAINC     !HA DIFF
                            IF (DHA.LE.0) R1 = 0                  !???
                            IF (DHA.GT.0) R1 = R1/DHA             !dA/dHA
C
			    R0=ABS(CDAT(I1,I3)-CDATLAST(I1,I3,0)) !LAST UNFL.
                            DHA=(HACUR-HALAST(I1,I3,0))/HAINC      !HA DIFF
                            IF (DHA.LE.0) R0 = 0
                            IF (DHA.GT.0) R0 = R0/DHA             !dA/dHA
C
                            R2 = MIN(R0,R1)             !TAKE THE SMALLEST ???
                            CRITVALD(I1,I3) = R2        !STORE FOR STATISTICS
			    IF (R1.GT.MXLIM(1)) SELECT=.TRUE. 
                            MODFD(I3) = .TRUE.      !MODIFY DATA FLAGS (ALWAYS)
                            SETFD(I3) = SELECT      !SET/CLEAR DATA FLAGS    
C
                            IF (TRACE) THEN           !TRACE WHAT HAPPENS
                              CALL WNCTXT (F_T,'DT1/RT1: HA=!6$EA6.1:'
     1                          //' CDAT=!12$EC6.0'
     1                          //' R1=!6$E6.0'
     1                          //' R0=!6$E6.0(!4$E4.1)'
     1                          //' SEL=!LJ'
     1                          ,HACUR,CDAT(I1,I3)
     1                          ,R1,R0,DHA
     1                          ,SELECT)
                            END IF
C
!*** SHADOW: Set the specified flag type(s) if one of the two telescopes
!            of this interferometer is shadowed by another, as given by
!            the array SHATEL (see above).
C
			  ELSE IF (NOPER.EQ.N_SHA) THEN
                            IF (SHATEL(RTW) .OR.
     1                          SHATEL(RTE)) SELECT=.TRUE.
                            SETFD(I3) = SELECT           !SET/CLEAR FLAGS
                            MODFD(I3) = .TRUE.           !MODIFY UV-DATA FLAGS
C
!*** PROJ.BASELINE: Set the specified flag type(s) if the projected baseline is
!                   between the two specified limits (PBASLIM).
!                   NB: Earlier flags are not cleared here, because one might
!                       want to flag multiple rings in the uv-plane.
C
			  ELSE IF (NOPER.EQ.N_PBA) THEN
			    R1 = BASEL(I1)*PBAS   
                            CRITVALD(I1,I3) = R1      !STORE FOR STATISTICS
                            IF (R1.GE.PBASLIM(0) .AND.
     1                          R1.LE.PBASLIM(1)) SELECT = .TRUE. 
                            SETFD(I3) = SELECT           !SET/CLEAR FLAGS
                            MODFD(I3) = .TRUE.           !MODIFY UV-DATA FLAGS
C
!*** CONTROL BUILDING INTERFERENCE: For each interferometer, calculate the
!    product of the telescope beam gains in the direction of the control
!    building (correlator cage). 
C
			  ELSE IF (NOPER.EQ.N_CBI) THEN
                            R1 = CBIFACT(RTW) * CBIFACT(RTE)
                            CRITVALD(I1,I3) = R1      !STORE FOR STATISTICS
                            IF (R1.GT.MXLIM(1)) SELECT = .TRUE.
                            SETFD(I3) = SELECT           !SET/CLEAR FLAGS
                            MODFD(I3) = .TRUE.           !MODIFY UV-DATA FLAGS
C
!*** RED: Set the specified flag type(s) if the baseline is redundant.
C
			  ELSE IF (NOPER.EQ.N_RED) THEN
			    IF (IRED(I1).GT.0) MODFD(I3)=.TRUE.    !REDUNDANT
C
!*** NONRED: Idem for non-redundant baselines.
C
			  ELSE IF (NOPER.EQ.N_NON) THEN
			    IF (IRED(I1).LE.0) MODFD(I3)=.TRUE. !NON-REDUNDANT
C
!*** TOTEL: Copy selected flags FROM other ifrs that share a telescope
!    with the current ifr. 
!    Check the other ifrs (in a copy LDAT1 of the uv-data) for flags of 
!    the specified type(s) (SELFLAG). If these ifrs share a telescope
!    with the current ifr, add the flags to the flagbyte (FLAGD(I3))
C
			  ELSE IF (NOPER.EQ.N_TOT) THEN
			    FLAGD(I3)=0		            !NOTHING
			    DO I6=0,STHJ(STH_NIFR_J)-1      !CHECK ALL IFR
                              IF ((RTW.EQ.IFRA(0,I6)) .OR.
     1                            (RTE.EQ.IFRA(1,I6))) THEN !SHARED TEL
			        I7=STHI(STH_PLN_I)*I6         !DATA POINTER
			        I7=I7+PPOL(I3,STHI(STH_PLN_I),1) !DATA OFFSET
			        I8=LDAT1(0,I7)	            !WEIGTH/FLAG
			        I8=IAND('0000ffff'X,I8)       !WEIGHT
			        IF (IAND(SELFLAG,I8).NE.0) THEN  !FLAG SELECTED
				  FLAGD(I3)=IOR(FLAGD(I3),IAND(SELFLAG,I8))
				  MODFD(I3)=.TRUE.
				END IF
			      END IF
			    END DO
C
!*** TOPOL: Copy selected flags FROM the other polarisations of the
!    current ifr. 
!    Check the other polarisations of this ifr (in a copy LDAT1 of 
!    the uv-data) for flags of the specified type(s) (SELFLAG). If they
!    are present, add them to the flagbyte for this ifr (FLAGD(I3))
C
			  ELSE IF (NOPER.EQ.N_TOP) THEN
			    FLAGD(I3)=0		           !NOTHING
			    DO I6=0,3                      !CHECK ALL POLS
                              IF (I6.NE.I3) THEN           !NOT ITSELF
			        I7=STHI(STH_PLN_I)*I1      !DATA POINTER
			        I7=I7+PPOL(I6,STHI(STH_PLN_I),1) !DATA OFFSET
			        I8=LDAT1(0,I7)	           !COPY OF THE DATA
			        I8=IAND('0000ffff'X,I8)    !WEIGHT/FLAGS
			        IF (IAND(SELFLAG,I8).NE.0) THEN !FLAG SELECTED
				  FLAGD(I3)=IOR(FLAGD(I3),IAND(SELFLAG,I8))
				  MODFD(I3)=.TRUE.
				END IF
                              END IF
			    END DO
C
!*** TOHEAD: Count the number of uv-data points in this Scan that have been
!            flagged with the specified flag type(s). If this number exceeds
!            a specified limit, set the same flag type(s) in the Scan header.
C
			  ELSE IF (NOPER.EQ.N_TOH) THEN
			    IF (IAND(WFDAT(I3),SELFLAG).NE.0) THEN  !FLAGGED
			      FLAGH=IOR(FLAGH,IAND(WFDAT(I3),SELFLAG)) !BYTE
			      TOH_COUNT=TOH_COUNT+1	        !COUNT FLAGS
			    END IF
                            IF (TOH_COUNT.GT.TOH_LIMIT) GOTO 50 !ENOUGH, ESCAPE
C
C
			  END IF                                !END OF NOPER
C
C NEXT POL (I3):
C
			END IF					!VALID DATA
		      END IF					!POL. SELECT
		    END DO					!POLS (I3)
C
C----------------------------------------------------------------------------
C Operations (e.g. N_QXY) that depend on more than one pol: 
C
!*** QXY: Compare XX and YY (i.e. Q if parallel dipoles):
C
	            IF (NOPER.EQ.N_QXY) THEN
                      IF (VALIDAT(XX).AND.VALIDAT(YY)) THEN     !BOTH VALID
			R1=ABS(CDAT(I1,XX)-CDAT(I1,YY))
			IF (R1.GT.MXLIM(1)) SELECT=.TRUE.  !DO CLIP 
                        DO I3=0,3                          !ALL POLS (?)
                          MODFD(I3) = .TRUE.   !MODIFY DATA FLAGS (ALWAYS)
                          SETFD(I3) = SELECT   !SET/CLEAR DATA FLAGS    
                        END DO
                        CRITVALD(I1,XX) = R1   !STORE FOR STATISTICS 
                        STATWGTD(I1,XY) = 0.   !NO STATISTICS FOR XY
                        STATWGTD(I1,YX) = 0.   !NO STATISTICS FOR YX
                        STATWGTD(I1,YY) = 0.   !NO STATISTICS FOR YY
		      END IF
C
!*** YXY, UXY: Compare XY and YX (i.e. U if parallel dipoles):
!    NB: YXY and UXY are exactly the same (at the moment):
C
	            ELSE IF (NOPER.EQ.N_YXY .OR. 
     1                       NOPER.EQ.N_UXY) THEN
                      IF (VALIDAT(XY).AND.VALIDAT(YX)) THEN     !BOTH VALID
			R1=ABS(CDAT(I1,XY)-CDAT(I1,YX))
			IF (R1.GT.MXLIM(1)) SELECT=.TRUE.  !DO CLIP 
                        DO I3=0,3                          !ALL POLS (?)
                          MODFD(I3) = .TRUE.   !MODIFY DATA FLAGS (ALWAYS)
                          SETFD(I3) = SELECT   !SET/CLEAR DATA FLAGS    
                        END DO
                        STATWGTD(I1,XX) = 0.   !NO STATISTICS FOR XY
                        CRITVALD(I1,XY) = R1   !STORE FOR STATISTICS 
                        STATWGTD(I1,YX) = 0.   !NO STATISTICS FOR YX
                        STATWGTD(I1,YY) = 0.   !NO STATISTICS FOR YY
		      END IF
C
C
!*** VXY: ABS(j(XY+YX)) (i.e. |V| if parallel dipoles):
C
	            ELSE IF (NOPER.EQ.N_VXY) THEN
                      IF (VALIDAT(XY).AND.VALIDAT(YX)) THEN     !BOTH VALID
			R1=ABS(CDAT(I1,XY)+CDAT(I1,YX))    !IGNORE J
			IF (R1.GT.MXLIM(1)) SELECT=.TRUE.  !DO CLIP 
                        DO I3=0,3                          !ALL POLS (?)
                          MODFD(I3) = .TRUE.   !MODIFY DATA FLAGS (ALWAYS)
                          SETFD(I3) = SELECT   !SET/CLEAR DATA FLAGS    
                        END DO
                        STATWGTD(I1,XX) = 0.   !NO STATISTICS FOR XY
                        CRITVALD(I1,XY) = R1   !STORE FOR STATISTICS 
                        STATWGTD(I1,YX) = 0.   !NO STATISTICS FOR YX
                        STATWGTD(I1,YY) = 0.   !NO STATISTICS FOR YY
		      END IF
C
		    END IF                                !END OF NOPER
C
C----------------------------------------------------------------------------
C Go through the pols again, to modify data flags, count set flags and
C accumulate statistics, according to the switches that have been set above:
C 
		    DO I3=POLMIN,POLMAX				!ALL POLS
		      IF (VALIDAT(I3)) THEN			!VALID DATA
C
C----------------------------------------------------------------------------
C MODIFY UV-DATA FLAGS (IF REQUIRED):
C
                        IF (DRYRUN) MODFD(I3) = .FALSE.    !NOT IF DRY RUN
C
			IF (MODFD(I3)) THEN		   !MODIFY UV-DATA
			  IF (SETFD(I3)) THEN	           !SET FLAG
			    WFDAT(I3)=IOR(WFDAT(I3),FLAGD(I3))
			  ELSE		                   !RESET FLAG
			    WFDAT(I3)=IAND(WFDAT(I3),NOT(FLAGD(I3)))
			  END IF
		          IF (IAND(WFDAT(I3),'00008000'X).NE.0)
     1			      WFDAT(I3)=IOR(WFDAT(I3),'ffff0000'X)  !OVERFLOW 
		          LDAT(0,LDOFF(I3))=WFDAT(I3)      !MODIFIED FLAG-BYTE
		          WRSCN=.TRUE.	                   !REWRITE SCAN DATA
			  CNTFD(I3) = .TRUE.               !COUNT FLAGS
			END IF
C
C----------------------------------------------------------------------------
!*** Count flags (i.e. store them for later counting):
C
			IF (CNTFD(I3)) THEN
                          FLACC(I1,I3) = WFDAT(I3)     !DATA FLAG
                          MASK(I1,I3) = FLAGD(I3)      !FLAGGING MASK USED
			END IF
C
!*** STATISTICS: If the uv-data point is flagged with the selected
!    flag type(s) (specified in FLAGD(I3)), then set the statistics weight 
!    to zero, so it will be ignored:
C
                        IF (IAND(FLAGD(I3),WFDAT(I3)).NE.0) THEN
                          STATWGTD(I1,I3) = 0.
                        END IF
C
C Store current data value (and its HA) for later use (e.g. DT1):
C
                        CDATLAST(I1,I3,1) = CDAT(I1,I3)   !LAST UV-POINT
                        HALAST(I1,I3,1) = HACUR           !ITS HA
C
                        IF (IAND(WFDAT(I3),FL_ALL).EQ.0) THEN  !IF NO FLAGS SET
                          CDATLAST(I1,I3,0) = CDAT(I1,I3) !LAST UNFLAGGED PNT
                          HALAST(I1,I3,0) = HACUR         !ITS HA
                        END IF
C----------------------------------------------------------------------------
C
C NEXT POL (I3):
C
		      END IF					!VALID DATA
		    END DO					!POLS (I3)
C
C----------------------------------------------------------------------------
C----------------------------------------------------------------------------
C NEXT IFR (I1):
C
		  END IF					!IFR SELECT
		END DO						!IFRS (I1)
C
  50            CONTINUE                        !FOR ESCAPE FROM IFR-LOOP
C
C COUNT data flags (always, even if CNTFD_DFLT is FALSE: CNTFD(I3) may have 
C been TRUE for some individual data, for some reason):
C
                JS = NFLCNT ('ACC','DATA',FLACC,MASK,IFRA,CHCUR,HACUR)
C
C ACCUMULATE DATA STATISTICS (if required):
C NB: NFLST1 takes 1 or more values that are all accumulated in separate
C     slots (e.g. per ifr). In the future this information will be used
C     to calculate separate flagging limits per ifr (for instance).
C NB: STATWGTD(I1,I3) gets corrupted, probably during Dryrun's only,
C     and not the first time. I cannot find the cause, and therefore
C     have added a kludge: since in the present incarnation STATWGTD
C     can only be 1 or 0, S_WGT is set to Zero if it is not One.
C
                IF (STACCD_DFLT) THEN 
                  DO I3=0,3                             !POLS
                    IF (SELPOL(I3)) THEN                !POL SELECTED        
                      DO I1=0,STHIFR-1                  !ALL IFRS
                        S_WGT(I1) = STATWGTD(I1,I3)     !STATISTICS WEIGHT
                      END DO 
C
                      DO I1=IFRMIN,IFRMAX                   
                        S_VAL(I1) = DAT(0,I1,I3)                 !COS
                      END DO
                      R0 = NFLST1 ('ACC','DAT_C_'//POLNAME(I3),
     1                             ' ',STHIFR,S_VAL,S_WGT)
C
                      DO I1=IFRMIN,IFRMAX                  
                        S_VAL(I1) = DAT(1,I1,I3)                 !SIN
                      END DO
                      R0 = NFLST1 ('ACC','DAT_S_'//POLNAME(I3),
     1                             ' ',STHIFR,S_VAL,S_WGT)
C
                      DO I1=IFRMIN,IFRMAX                  
                        S_VAL(I1) = ABS(CDAT(I1,I3))             !AMPL
                      END DO
                      R0 = NFLST1 ('ACC','DAT_A_'//POLNAME(I3),
     1                             ' ',STHIFR,S_VAL,S_WGT)
C
                      DO I1=IFRMIN,IFRMAX                  
                        S_VAL(I1) = (360/PI2)* 
     1                        ATAN2(DAT(1,I1,I3),DAT(0,I1,I3))   !PHASE(DEGR)
                      END DO
                      R0 = NFLST1 ('ACC','DAT_P_'//POLNAME(I3),
     1                             ' ',STHIFR,S_VAL,S_WGT)
C
                      IF (DRYRUN) THEN
                        DO I1=IFRMIN,IFRMAX                   
                          S_VAL(I1) = CRITVALD(I1,I3)          !CRITERION VALUE
                        END DO
C
                        R0 = NFLST1 ('ACC','CRIT_'//CNOPER(NOPER),
     1                             ' ',STHIFR,S_VAL,S_WGT)
C
                        R0 = NFLST1 ('ACC',
     1                       'CRIT_'//CNOPER(NOPER)//'_'//POLNAME(I3),
     1                       ' ',STHIFR,S_VAL,S_WGT)         !PER POL
                      END IF
C
                    END IF                              !POL SELECT
                  END DO                                !POLS
                END IF
C
C MODIFY SCAN HEADER FLAGS (IF REQUIRED):
C
 40	        CONTINUE                            !TARGET IF SKIPPING DATA
C
		IF (DRYRUN) MODFH = .FALSE.	    !NOT IF DRY RUN
C
	        IF (MODFH) THEN
                  IF (SETFH) THEN	            !SET FLAG
	            SCHJ(SCH_BITS_J)=
     1                 IOR(SCHJ(SCH_BITS_J),FLAGH)
	          ELSE                              !CLEAR FLAG
	            SCHJ(SCH_BITS_J)=
     1                 IAND(SCHJ(SCH_BITS_J),NOT(FLAGH))
	          END IF
                  WRSCH = .TRUE.		    !RE-WRITE SCAN HEADER
		  CNTFH = .TRUE.                    !COUNT HEADER FLAGS
                END IF
C
!*** STATISTICS: Put criterion value and header info into statistics 
C    accumulators. NB: Only if the Scan header is not flagged with
C    the specified flag type(s), specified by FLAGH
C
                IF (STACCH) THEN
                  IF (IAND(FLAGH,SCHJ(SCH_BITS_J)).EQ.0) THEN
                    R0 = NFLST1 ('ACC','CRIT_'//CNOPER(NOPER),
     1                           ' ',1,CRITVALH,STATWGTH)  
                    R0 = NFLST1 ('ACC','CRIT_'//CNOPER(NOPER),
     1                           ' ',1,CRITVALH,STATWGTH)  
                    R0 = SCHE(SCH_MAX_E)                      
                         R0 = NFLST1('ACC','MAXABCS',' ',1,R0,1.) 
                    R0 = SCHE(SCH_REDNS_E+0)                      
                    IF (R0.GT.0) 
     1                   R0 = NFLST1('ACC','REDNS_GX',' ',1,R0,1.) 
                    R0 = SCHE(SCH_REDNS_E+1)                      
                    IF (R0.GT.0) 
     1                   R0 = NFLST1('ACC','REDNS_PX',' ',1,R0,1.) 
                    R0 = SCHE(SCH_REDNS_E+2)                      
                    IF (R0.GT.0) 
     1                   R0 = NFLST1('ACC','REDNS_GY',' ',1,R0,1.) 
                    R0 = SCHE(SCH_REDNS_E+3)                      
                    IF (R0.GT.0) 
     1                   R0 = NFLST1('ACC','REDNS_PY',' ',1,R0,1.) 
                    R0 = SCHE(SCH_ALGNS_E+0)                      
                    IF (R0.GT.0) 
     1                   R0 = NFLST1('ACC','ALGNS_GX',' ',1,R0,1.) 
                    R0 = SCHE(SCH_ALGNS_E+1)                      
                    IF (R0.GT.0) 
     1                   R0 = NFLST1('ACC','ALGNS_PX',' ',1,R0,1.) 
                    R0 = SCHE(SCH_ALGNS_E+2)                      
                    IF (R0.GT.0) 
     1                   R0 = NFLST1('ACC','ALGNS_GY',' ',1,R0,1.) 
                    R0 = SCHE(SCH_ALGNS_E+3)                      
                    IF (R0.GT.0) 
     1                   R0 = NFLST1('ACC','ALGNS_PY',' ',1,R0,1.) 
                  END IF
                END IF
C
C Count header flags (if required):
C
	        IF (CNTFH) THEN
                  FLACC(0,0) = SCHJ(SCH_BITS_J)             !HEADER FLAGS
                  MASK(0,0)  = FLAGH                        !FLAGBYTE USED
                  JS = NFLCNT ('ACC','HEAD',FLACC,MASK,IFRA,CHCUR,HACUR)
		END IF
C
C REWRITE SCAN HEADER (IF REQUIRED):
C
                IF (WRSCH) THEN
	          IF (.NOT.NSCSCW(FCAIN,STH,IFRT,I,0,0,SCH)) THEN !WRITE HEADER
	            CALL WNCTXT(F_TP,'Error writing scan header !UJ',I)
	 	    GOTO 30
	          END IF
	        END IF
C
C REWRITE SCAN DATA (IF REQUIRED):
C
	        IF (WRSCN) THEN			!REWRITE SCAN
	          IF (.NOT.WNFWR(FCAIN,STHJ(STH_SCNL_J)-SCH__L,
     1                  LDAT,STHJ(STH_SCNP_J)+I*STHJ(STH_SCNL_J)+
     1                  SCH__L)) THEN !WRITE DATA
	            CALL WNCTXT(F_TP,'Error writing scan !UJ',I)
		    GOTO 30			!NEXT SET
	          END IF
	        END IF
C
C DRY RUN: See whether it is time to stop. 
C
                IF (DRYRUN) THEN
                  NDRYSCANS = NDRYSCANS + 1              !INCREMENT COUNTER
                  IF (NDRYSCANS.GE.MAXDRYSCANS) GOTO 30  !ESCAPE 
                END IF                
C
C NEXT SCAN
C
	    END IF				!HA RANGE
	  END DO				!NEXT SCAN
C
C NEXT SECTOR (if any):
C NB: Do NOT jump out of the DO WHILE ... Sector loop, but let it end
C     itself. Otherwise the system gets confused.
C
 30	  CONTINUE
	END DO					!NEXT SECTOR
C
C ------------------------------------------------------------------------
C  DRY RUN: Display the result of the accumulated statistics.
C           To help the user to give a good flagging threshold value.
C
        IF (DRYRUN) THEN
          R0 = NFLST1 ('SHOW','#SEPAR',' ',0,0.,0.)
          CALL WNCTXS (TXT80,
     1        'Dry-Run: Statistics of the relevant criterion,'
     1            //' over !UJ Scans.',NDRYSCANS)
          R0 = NFLST1 ('SHOW','#TEXT',TXT80,0,0.,0.)
          R0 = NFLST1 ('SHOW','#SEPAR',' ',0,0.,0.)
          R0 = NFLST1 ('SHOW','#HEADER',' ',0,0.,0.)
          R0 = NFLST1 ('SHOW','##CRIT_'//CNOPER(NOPER),' ',1,0.,0.)
          R0 = NFLST1 ('SHOW','#SEPAR',' ',0,0.,0.)
C
          IF (NOPER.EQ.N_RNO .OR.
     1        NOPER.EQ.N_XRN .OR.
     1        NOPER.EQ.N_YRN) THEN
            R0 = NFLST1 ('SHOW','##REDNS',' ',1,0.,0.)
            R0 = NFLST1 ('SHOW','#SEPAR',' ',0,0.,0.)
            GOTO 213                             !BACK FOR `WET' RUN
          ELSE IF (NOPER.EQ.N_ANO .OR.
     1             NOPER.EQ.N_XAN .OR.
     1             NOPER.EQ.N_YAN) THEN
            R0 = NFLST1 ('SHOW','##ALGNS',' ',1,0.,0.)
            R0 = NFLST1 ('SHOW','#SEPAR',' ',0,0.,0.)
            GOTO 213                             !BACK FOR `WET' RUN
          ELSE IF (NOPER.EQ.N_MAX) THEN
            R0 = NFLST1 ('SHOW','#SEPAR',' ',0,0.,0.)
            GOTO 214                             !BACK FOR `WET' RUN
          ELSE IF (NOPER.EQ.N_AMP .OR.
     1             NOPER.EQ.N_COS .OR.
     1             NOPER.EQ.N_SIN) THEN
CCC         R0 = NFLST1 ('SHOW','##DAT_',' ',1,0.,0.)
            R0 = NFLST1 ('SHOW','#SEPAR',' ',0,0.,0.)
            GOTO 215                             !BACK FOR `WET' RUN
          ELSE IF (NOPER.EQ.N_DT1 .OR.
     1             NOPER.EQ.N_RT1) THEN
            R0 = NFLST1 ('SHOW','#SEPAR',' ',0,0.,0.)
            GOTO 216                             !BACK FOR `WET' RUN
          ELSE IF (NOPER.EQ.N_RRE .OR.
     1             NOPER.EQ.N_ARE .OR.
     1             NOPER.EQ.N_YXY .OR.
     1             NOPER.EQ.N_UXY .OR.
     1             NOPER.EQ.N_VXY .OR.
     1             NOPER.EQ.N_QXY) THEN
            R0 = NFLST1 ('SHOW','#SEPAR',' ',0,0.,0.)
            GOTO 217                             !BACK FOR `WET' RUN
	  ELSE                                   !UNSPECIFIED OPERATION 
            R0 = NFLST1 ('SHOW','#ALLGROUPS',' ',1,0.,0.)
            R0 = NFLST1 ('SHOW','#SEPAR',' ',0,0.,0.)
          END IF                                 !OPERATION
        END IF                                   !
C--------------------------------------------------------------------------
C  Display a summary of flags, if required:
C
        IF (SHOW_CNT) THEN
          JS = NFLCNT ('SHOW','FTYP',0,SELFLAG,0,0,0)
        END IF
C
C--------------------------------------------------------------------------
C  STATISTICS: 
C
        IF (SHOW_STAT) THEN
          R0 = NFLST1 ('SHOW','#SEPAR',' ',0,0.,0.)
          CALL WNCTXS (TXT80,
     1        'Statistics over UNFLAGGED data/headers.')
          R0 = NFLST1 ('SHOW','#TEXT',TXT80,0,0.,0.)
          R0 = NFLST1 ('SHOW','#SEPAR',' ',0,0.,0.)
          R0 = NFLST1 ('SHOW','#HEADER',' ',0,0.,0.)
          R0 = NFLST1 ('SHOW','#SINGLES',' ',1,0.,0.)
          R0 = NFLST1 ('SHOW','##DAT_',' ',1,0.,0.)
          R0 = NFLST1 ('SHOW','#SEPAR',' ',0,0.,0.)
        END IF
C
C**************************************************************************
C**************************************************************************
C FINISHED NORMALLY
C
 600    CONTINUE
C
	IF (NOPER.EQ.N_HAR) GOTO 321	!LOOP BACK FOR HA-RANGES
C
	RETURN
C
C**************************************************************************
C NO ACTION
C
 700	CONTINUE
        CALL WNCTXT (F_TP,'Operation '//CNOPER(NOPER)
     1              //': No action.')
        RETURN
C
C**************************************************************************
C ERROR
C
 800	CONTINUE
        CALL WNCTXT (F_TP,'Operation '//CNOPER(NOPER)
     1              //': Error.')
        RETURN
C
	END
