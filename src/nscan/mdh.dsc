!+ MDH.DSC
!  WNB 900327
!
!  Revisions:
!
%REVISION=JPH=960612="Correct RA edit: DPF i.s.o. DAF"
%REVISION=WNB=940228="Add unknown-flux bit"
%REVISION=WNB=931215="Add some edit formats"
%REVISION=WNB=930928="Add instrument"
%REVISION=WNB=930819="Some text"
%REVISION=WNB=900327="Original header"
!
!	Define Model header
!
%COMMENT="MDH.DSC defines the model header"
!
!
%VERSION=1					!VERSION
%SYSTEM=1
%USER=WNB
%%DATE
%%NAME
!
%GLOBAL=MDHNIN=8		!Number of instruments that can be used
				!(see also BMD.DSC)
%ALIGN
!-
.PARAMETER
	MDHINS_M J	/MDHNIN-1/ !Mask for instrument in BITS
	MDHUNF_M J	/65536/	   !Mask for "unknown flux" bit in BITS
.BEGIN=MDH
	LINK	J(0:1)	<,1>	!Link models (not used)
	MID	J	<,1>	!Model ID (not used)
	MODL	J	<,1>	!Max. # of lines in model or disk version
	MODP	J	<XJ,1,,P:MDL> !Pointer to model
	NSRC	J	<,1>	!# of sources in model
	TYP	J		!Type of model (0: no ra,dec, 1=app, 2=epoch)
	EPOCH	E	<E12.2>	!Epoch (e.g. 1950) if TYP=2
	RA	D	<DPF12.7> !Model centre RA (circles)
	DEC	D	<DAF12.7> !Model centre DEC (circles)
	FRQ	D	<D12.6>	!Model centre FRQ (MHz)
	ACT	J	<XJ>	!Model action (only on disk)
	BITS	J	<XJ>	!Detailed data:
				!	lowest 3 bits (MDHINS_M): instrument
				!	bit 17 (MDHUNF_M): model has
				!	components with unknown flux.
				!	(feature used in NCALIB)
.END					!END DEFINITION
!-
