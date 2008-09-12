!+ NGI.DSC
!  HJV 920827
!
!  Revisions:
!
%REVISION=CMV=940817="Add DO_CLIP"
%REVISION=CMV=940218="Add DO_BLANK"
%REVISION=CMV=940203="Add ALL_POL"
%REVISION=WNB=931221="Alignment problem; use NSTAR_DSF"
%REVISION=CMV=931123="Add parameters for NGIDS DATA and RAW option"
%REVISION=WNB=930803="Remove .INCLUDE"
%REVISION=WNB=930621="Add DFAR; UFL"
%REVISION=WNB=930510="Major revision"
%REVISION=HJV=930219="Add OPTION"
%REVISION=HJV=920827="Original version"
!
!	Layout of overall include file (NGI.DEF)
!
%COMMENT="NGI.DEF is an INCLUDE file for the NGIDS program"
%COMMENT=" "
!
%VERSION=3
%SYSTEM=1
%USER=HJV
%%DATE
%%NAME
!
%ALIGN						!Align structures
!
%INCLUDE=NSTAR_DSF				!# OF TELESCOPES ETC
%GLOBAL=MXNSET=16				!MAX. # OF SETS PER INPUT JOB
%GLOBAL=MXNSEQ=256				!MAX. # OF GIDS SEQUENCES
!-
.DEFINE
  .DATA
!
!  Parameters
!
  .PARAMETER
!
!  Local variables:
!
  .COMMON
!
!  Values defined in NGIDAT
!
	OPTION	C24				!PROGRAM OPTION
	OPT=OPTION C3
        MAPTYP  C16     / /                     !MAP/IFRS/CHAN
!
	NODIN	C80				!NODE
	FILIN	C160				!FILE NAME
	FCAIN	J				!FILE IDENTIFIER
	SETS	J(0:7,0:MXNSET)			!SETS TO SHOW
	SETNAM  J(0:7)                          !SET NAME
	RANGE	E(2)				!MINIMUM/MAXIMUM
!
	TAREA	J(0:3)				!AREA TO SHOW
	PAREA	J(0:3)
	TEAR	J(0:3)
	PEAR	J(0:3)
	MXAREA	J(0:3)
	FAREA	J(0:3)
	NRA	J				!X-DIMENSION
	NDEC	J				!Y-DIMENSION
	COMPR	J				!COMPRESSION
!
	HARAN   E(0:1)				!HOUR ANGLE RANGE
	HAINC	E				!HOUR ANGLE INCREMENT
        SIFRS   B(0:NSTAR_TEL-1,0:NSTAR_TEL-1)  !SELECT INTERFEROMETERS
        STELS   B(0:NSTAR_TEL-1)                !SELECT TELESCOPES
        SPOL    J                               !POLARISATION BITS
	RCHAN   J(0:1)                          !RANGE IN CHANNELS
	NIFR	J				!NUMBER OF IFR'S
        DATTYP  C16                             !DATA TYPES TO DO
	DO_BLANK L		    		!BLANK FLAGGED DATA
!
	IDXLUT  J(0:NSTAR_TEL-1,0:NSTAR_TEL-1)	!FIND LINE NO FOR EACH IFR
	IFRLUT  J(0:NSTAR_TEL*NSTAR_TEL-1)	!FIND IFR FOR EACH LINE NO
	TELPOS  E(0:NSTAR_TEL-1)		!TELESCOPE POSITIONS
!
        CORAP   J                               !CORRECTIONS TO APPLY
        CORDAP  J                               !CORRECTIONS TO DE-APPLY
        NSRC    J(0:2)                          !SOURCE COUNTS IN MODEL
!
	DO_FLAG L				!FLAG DATA AFTER LOAD (regions)
	DO_CLIP L				!FLAG DATA AFTER LOAD (clipping)
	ALLCH	L		    /.TRUE./	!FLAG IN ALL CHANNELS
	ALLPOL	L		    /.TRUE./	!FLAG IN ALL POLARISATIONS
	UFL	J			/0/	!FLAG TO SET
!
! These variables describe the state of the GIDS display
!
	GID	J				!GIDS DISPLAY ID
	DEFIMG	L	/.TRUE./		!Call GDI_DEFIMG required?
	NRMAP	J				!#MAPS LOADED
	DMAP	J			/0/	!MAP PLANE AREA
	DFLG	J			/0/	!FLAG PLANE AREA
	IPTR	J				!Pointer in areas
	LBUF	J				!Size of areas
	CSCALE	E				!FOR SCALING DATA
	CZERO	E
	BLANK	J				!BLANK VALUE IN GIDS
	MAXCOL	J				!Colorrange in GIDS
	MINCOL	J
	XSIZ	J				!Size of GIDS window
	YSIZ	J
	AVGFAC	E				!Averaging factor
!
! Some general things
!
        TELNAM  CNSTAR_TEL /0123456789ABCD/     !TEL. NAMES
	DFAR	J			/0/	!FLAG FILE AREA
	PTR	J			/0/	!Pointer to header
.END
