!+ NMO.DSC
!  WNB 900327
!
!  Revisions:
!
%REVISION=WNB=931008="Add beam mask"
%REVISION=WNB=931005="Add mask names"
%REVISION=JPH=930825="Comments"
%REVISION=WNB=930803="Remove .INCLUDE; use NSTAR.DSF"
%REVISION=WNB=930602="Add BEMLIM"
%REVISION=WNB=911007="Add instrum. polarisation"
%REVISION=WNB=910731="Add source find info"
%REVISION=WNB=900327="Original version"
!
!	Layout of overall include file (NMO.DEF)
!
%COMMENT="NMO.DEF is an INCLUDE file for the NMODEL program"
%COMMENT=" "
!
%VERSION=1
%SYSTEM=1
%USER=WNB
%%DATE
%%NAME
!
! Get number of telescopes
!
%INCLUDE=NSTAR_DSF
!
%LOCAL=MDH=64				!Length MDH (see MDH.DSC)
%LOCAL=MXNSET=64			!Max. sets(maps) that can be done
%LOCAL=MXNAR=16				!Max. # of simultaneous areas
!-
.DEFINE
  .PARAMETER
	MXNSET	J	/MXNSET/	!MAX. # OF SETS/MAPS
	MXNAR	J	/MXNAR/		!MAX. # OF SUB-AREAS
	BEMLIM	D	/0.01/		!LOWEST BEAM VALUE USED
	NMO	M*:	/USE,MERGE,ADD,SAVE,,,,,BAND,TIME,IPOL,BEAM/ !ACTION TYPES
	NMO	NF*:(NMO_USE+NMO_MER+NMO_ADD+NMO_SAV, \
			NMO_BAN+NMO_TIM+NMO_IPO+NMO_BEA) \
				/USAGE,SMEAR/ !MANIPULATE
  .DATA
!
!  Local variables:
!
  .COMMON
	ACTION	C24				!PROGRAM ACTION
	ACT=ACTION C3
	OPTION	C24				!PROGRAM OPTION
	OPT=OPTION C3
	FCAOUT	J				!OUTPUT FCB
	FILOUT	C160				!FILE NAME
	NODOUT	C80				!NODE NAME
	FCAIN	J				!INPUT FCB
	FILIN	C160				!FILE NAME
	NODIN	C80				!NODE NAME
!
! The model headers are identical to those in the SCN file. They point to source
!  lists that are allocated dynamically in core. The conventions for the use of
!  the second index (the model "type", cf. NMOUP0) are:
!    -1		"general header"
!     0		standard (=clean?)
!     1		extended sources
!     2		spectral index
!    ...
!     7		use as scratch buffer (NMOMSG)
	GMDH	B(0:MDH-1,-1:7)			!MODEL HEADERS
	GMDHJ=GMDH J(0:MDH/LB_J-1,-1:7)
	GMDHE=GMDH E(0:MDH/LB_E-1,-1:7)
	GMDHD=GMDH D(0:MDH/LB_D-1,-1:7)
!
	GDES=GMDH B(0:MDH-1)			!GENERAL SOURCE DESCRIPTOR
	GDESJ=GMDH J(0:MDH/LB_J-1)
	GDESE=GMDH E(0:MDH/LB_E-1)
	GDESD=GMDH D(0:MDH/LB_D-1)
!
	NSRCM	J			/0/	!# OF LINES IN GDES
	SORRAN	E(0:1)			/0,0/	!OFFSET FOR DISTANCE SORT
	SORTYP	J				!SORT INCR.(-1)/DECR(1)
	SOROFF	J				!OFFSET FOR SORT
	MODACT	J				!ACTION ON SCAN:
						! 1= use, 2= merge, 4= add,
						! 8= save, 256= band, 512= time
						! 1024= instr.pol.
	MAPLIM	E				!RELATIVE LIMIT OF SOURCE FIND
	MAXSRN	J				!MAX. # OF SOURCES TO FIND
	IDEN	J				!START ID FOR SOURCES FOUND
	NAREA	J				!# OF AREAS FOUND
	SETS	J(0:7,0:MXNSET)			!SETS/MAPS TO DO
	FAREA	J(0:3)				!FULL MAP AREA
	TAREA	J(0:3,0:1)			!TOTAL AREA (0=NORM, 1=EDGE)
	PAREA	J(0:3,MXNAR,0:1)		!PARTIAL AREAS
	SIFRS	B(0:NSTAR_TEL-1,0:NSTAR_TEL-1)	!SELECTED INTERFEROMETERS
	SPOL	J				!SELECTED POLARISATIONS
	NSRC	J(0:2)				!SOURCES TO USE
	HARAN	E(2)				!HA RANGE
	CORAP	J				!CORRECTIONS TO APPLY
	CORDAP	J				!CORRECTIONS TO DE-APPLY
	INPOL	E(0:9,0:2,0:6)			!INSTRUM. POL.
	INPOLF	E(0:6)		/(7)0/		!FREQUENCY LIMITS
.END
