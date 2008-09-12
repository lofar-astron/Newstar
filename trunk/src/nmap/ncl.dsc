!+ NCL.DSC
!  WNB 910809
!
!  Revisions:
!
%REVISION=HjV=950512="Add DATAFAC"
%REVISION=WNB=930803="Remove .INCLUDE"
%REVISION=WNB=921216="Add GRFAC"
%REVISION=WNB=910809="Original version"
!	  JPH 940223	Comments
!
!	Layout of overall include file (NCL.DEF)
!
%COMMENT="NCL.DEF is an INCLUDE file for the NCLEAN program"
%COMMENT=" "
!
%VERSION=1
%SYSTEM=1
%USER=WNB
%%DATE
%%NAME
%LOCAL=MXNSET=16				!MAX. # OF SET DEFINITIONS
%LOCAL=MXNAR=32					!MAX. # OF SUB-AREAS
!-
.DEFINE
  .PARAMETER
	MXNSET	J		/MXNSET/	!MAX. # OF MAP SETS
	MXNAR	J		/MXNAR/		!MAX. # OF AREAS
	MNBPAT	J		/3/		!MIN. BEAM PATCH SIZE
	MXBPAT	J		/128/		!MAX. BEAM PATCH SIZE
  .DATA
!
!  Local variables:
!
  .COMMON
	OPTION	C24				!PROGRAM OPTION
	OPT=OPTION C3
	MEMSIZ	J				!SIZE OF DYNAMIC MEMORY TO USE
	NODMAP	C80				!MAP NODE
	FILMAP	C160				!MAP FILE
	FCAMAP	J				!MAP FCA
	MSETS	J(0:7,0:MXNSET)			!MAP SETS
	NODAP	C80				!AP NODE
	FILAP	C160				!AP FILE
	FCAAP	J				!AP FCA
	ASETS	J(0:7,0:MXNSET)			!AP SETS
	APDCV	L				!APPLY DECONVOLUTION
!
	CMPLOG	J(2)				!LOG CODE COMPON_LOG:
!		Each CMPLOG(1)th component will be reported on terminal.
!		Each CMPLOG(2)th component will be reported in log file.
!
	CLLIM	E				!CLEAN LIMIT
	CLFAC	E				!CLEAN LOOP FACTOR
	SRCLIM	J				!# OF SOURCES LIMIT
!
	TAREA	J(0:3,0:1)			!TOTAL AREA
	PAREA	J(0:3,MXNAR,0:1)		!PARTIAL AREAS
	NAREA	J				!NUMBER OF AREAS
!		Area format: 	0,0	hor. centre	0,1	left edge
!				1,0	vert. centre	1,1	right edge
!				2,0	hor. width	2,1	bottom edge
!				3,0	vert. width	3,1	top edge
!
	PRHAT	E				!PRUSSIAN HAT VALUE
	RESMDL	L				!OUTPUT RESIDUAL MODEL SWITCH
	RSTMDL	L				!RESTORED OUTPUT SWITCH
	RONMDL	L				!ONLY RESTORE SWITCH
	MPDEP	E				!CYCLE DEPTH
	GRFAC	E				!GRATING FACTOR
	DATAFAC	E				!DATACLEAN FACTOR
!
!  History
!
	CURMAX	E				!CURRENT MAP MAX.
	CURMXP	J(2)				!POS. CURRENT MAX.
	MAPNAM	J(0:7)				!CURRENT MAP NAME
	APNAM	J(0:7)				!CURRENT AP NAME
	BEMPAT	J				!SIZE BEAM PATCH
	MAPPAT	J				!# OF POINTS IN MAP PATCH
	MAPLIM	E				!MAP DATA LIMIT IN PATCH
	CLBXLM	E				!MAX. CORRECTION OUTSIDE PATCH
	CURPMX	J				!CURRENT MAX. POINTER
	MINLIM	E				!MAP INPUT MAXIMUM
	CVBFU	J				!U CONVOLUTION FUNCTION ptr
	CVBFV	J				!V CONVOLUTION FUNCTION ptr
	RESDL	E				!RESTORE BEAM L
	RESDM	E				!RESTORE BEAM M
	RESDAN	E				!RESTORE BEAM SKEW ANGLE
	MPHAD	J				!MAP HISTOGRAM AREA ptr
	BMHAD	J				!BEAM HISTOGRAM AREA ptr
	MPHMXI	E				!MAX. IN MAP HISTOGRAM
!
.END
