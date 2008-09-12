!+ MDU.DSC
!  WNB 950622
!
!  Revisions:
!
%REVISION=WNB=950622="Original definition"
%REVISION=WNB=950630="Add few types"
%REVISION=WNB=950706="Add LOOP"
%REVISION=WNB=990729="Add X types"
!
!	Define Model update area
!
%COMMENT="MDU.DSC defines a model update area"
!
!
%VERSION=1					!VERSION
%SYSTEM=1
%USER=WNB
%%DATE
%%NAME
!-
%ALIGN
!
.PARAMETER
	MDU_T	MF:	\		!Update type bits:
			/ILM, \		!Intensity, LM-position,
			EXT,  \		!Extended
			SILM, \		!Spectral index, I, L, M
			QUV,  \		!Polarisation
			LM,   \		!LM-poosition
			I,    \         !Intensity
			PEST, \		!Polarisation estimate
			X00,  \		!Extra type's (tbd)
			X01,  \
			X02,  \
			X03/
!
	MDU_M	MF:(2*MDU_T__H) \	!Mode bits
			/CLUST, \	!Clustered sources
			COMBI, \	!Combined
			CONSTR, \	!Use constraints
			LOOP, \		!Loop solution
			ELOOP/		!Last loop
	MXLCNT	J	/20/		!Loop count
!
.BEGIN=MDU
	TYPE	J			!Type of update for source(s)
	LEN	J			!Length CEQ+SOL area (bytes)
	NUN	J			!# of unknowns per source
	NSRC	J			!# of sources covered by area
	OFF	J			!Offset of this source in list
	OFFS	J			!Offset of this solution in list
	LAR	J	<XJ,1>		!Pointer to LSQ area
	CEQ	J	<XJ,1>		!Pointer to condition equation area(E)
	SOL	J	<XJ,1>		!Pointer to solution/m.e. vector(E)
	MOD	J	<XJ,1>		!Pointer to model save area (4*IFR,X)
	RAR	J	<XJ,1>		!Reference to actual solution area(MDU)
.END					!END DEFINITION
!-
