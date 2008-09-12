!+ CBITS.DSC
!  JPH 930615
!
!  Revisions:
!
%REVISION=WNB=940812="Add QUB_OUT"
%REVISION=WNB=940809="Add QUB_M"
%REVISION=WNB=940803="Add QUB and QINFO"
%REVISION=CMV=940506="Removed ZAP bits (use CAP instead)"
%REVISION=WNB=930904="Add INStrument and IFr"
%REVISION=WNB=930902="Use R:, F: and *:"
%REVISION=WNB=930826="Some more Polarisation bits"
%REVISION=WNB=930825="Add Polarisation bits"
%REVISION=WNB=930803="Use WNTINC options"
%REVISION=JPH=930618="FL_ bits from sch.dsc. - CAP_SHFT, rearrange "
%REVISION=JPH=930618="  definitions of xxx_TELMSK to work around wntab bug "
%REVISION=JPH=930615="Original version"
!
!       Define bits in CAP, CDAP, ZAP bitmasks
!
%COMMENT="Define bits in CAP, CDAP, ZAP bitmasks"
!
%VERSION=1
%SYSTEM=1
%USER=JPH
%%DATE
%%NAME
!
!-
.PARAMETER
!
! Apply/deapply mask bits 
!
	  CAP	M*:	/REDUN,ALGN,OTHER,XTNCTION,REFRACTION, \ !APPLY/DEAPPLY BITS
			IREFRACTION,CLKCORR,PH,POLAR,FARADAY, \
			SHFT,,MODEL,AIFR,MIFR/
					!bit 0:  redundancy corrections
					!bit 1:  align corrections
					!bit 2:  other telescope corrections
					!bit 3:  extinction
					!bit 4:  refraction
					!bit 5:  ionos. refraction
					!bit 6:  clock correction
					!bit 8:  instrum. polarisn
					!bit 9:  Faraday rotation
					!bit 10: shift
					!bit 12: source model
					!bit 13: additive ifr
					!bit 14: multiplicative ifr
	  CAP	MF*:	/,,,XTNC,REFR,	\	!Extended names
			IREF,,,,,	\
			SHFT,,,AIFR,MIFR/
	  CAP	NF*:(CAP_RED+CAP_ALG+CAP_OTH+CAP_XTN+CAP_REF+ \
				CAP_IRE+CAP_CLK+CAP_SHF, \ !All telescope corr.
			CAP_POL+CAP_FAR,	\	!All pol. corr.
			CAP_MOD+CAP_AIF+CAP_MIF) \	!All ifr-based corr.
			/TELMSK,POLMSK,IFRMSK/
	  CAP	NF*:(CAP_TELMSK+CAP_POLMSK+CAP_IFRMSK) \ !All corrections
		    /ALLMSK/
!
! Delete/flagging bits in SCH block - in cbits.dsc because wnddab uses them
!
	FL	M*:(256)	/1,2,3,4,5,6,7,8/	!FLAG BITS
						!FLAG2 1,2,3 ARE PURE USER TYPE
	FL	NF*:(FL_8+FL_8-FL_1,	\	!ALL, MANUAL, OLD, CLIP,
			FL_8,FL_8,FL_7,	\	!  NOISE, ADD, SHADOW TYPE
			FL_6,FL_5,FL_4)	\
		     /ALL,		\
			MAN,OLD,CLIP,	\
			NOIS,ADD,SHAD/
!
! Polarisation bits
!
	P	MR*:(1)	/XX,XY,YX,YY/		!POLARISATION BITS (_P)
	P	MR*:(1)	/SI,SQ,SU,SV/		!STOKES BITS (_P)
	P	MRF*:(16) /STOKES,IMAG,LINE/	!STOKES, IMAGINARY, LINE IND.
	P	A:(0)	/XX,XY,YX,YY/		!POLARISATION OFFSETS (P_)
	PS	A:(0)	/I,Q,U,V/		!STOKES OFFSTES (PS_)
	M	NRF*:(XX_P,YY_P,		\	!VARIOUS MASKS: (_M)
					\	! X:    XX	Y:   YY
					\	! XYX:  XX XY YX YY
					\	! XY:   XX YY	YX:  XY YX
					\	! XXY:  XY	YYX: YX
					\	! IQUV: I Q U V
					\	! IQ:   I Q	UV:  U V
					\	! I:    I	Q:   Q
					\	! U:    U	V:   V
			  XX_P+YY_P+XY_P+YX_P,	\
			  XX_P+YY_P,XY_P+YX_P,	\
 			  XY_P,YX_P,		\
			  SI_P+SQ_P+SU_P+SV_P+STOKES_P, \
			  SI_P+SQ_P+STOKES_P,	\
			  SU_P+SV_P+STOKES_P,	\
			  SI_P+STOKES_P,SQ_P+STOKES_P, \
			  SU_P+STOKES_P,SV_P+STOKES_P) \
			/X,Y,			\
			  XYX,			\
			  XY,YX,		\
			  XXY,YYX,		\
			  IQUV,			\
			  IQ,			\
			  UV,			\
			  I,Q,			\
			  U,V/
!
! Instruments
!
	INS	A:(0)	/WSRT,ATCA/		!WSRT and ATCA
!
! Interferometer
!
	IFJ	A:(0)	/WT,ET,IFR/		!W telescope, E telescope, IFR
	IFE	A:(0)	/ANG,SB,CB/		!W X-dipole angle (circles),
						!sin(beta=E X-dipole offset)
						!cos(beta)
!
! Qube
!
	QUB	M*:(1)	/FTI,TFI,FIT,IFT,TIF,ITF, \
			  M,OUT/ 		!Order of search through a
						!data cube(t=ha,f=freq,i=ifr)
						!and model wanted (M), and
						!ifr errors output (OUT)
	QUB	N*:(QUB_FTI+QUB_TFI,	\	!Last axes
			  QUB_FIT+QUB_IFT, \
			  QUB_TIF+QUB_ITF) \
			/I,T,F/
	QINFO	A:(0)	/FLD,F,T,I/		!Offset in INFO from NSCQOP
!-
