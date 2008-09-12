!+ RPF.DSC
!  WNB 920428
!
!  Revisions:
!
%REVISION=WNB=930803="Remove .INCLUDE"
%REVISION=HM=920319="Added sc_srcno"
%REVISION=WNB=920428="Original version"
!
!        rpn 7/11/88     added IF table and IF common
!        rpn 8/11/88     inserted AN table and extended antenna common
!                        to include polarisation
!        rpn 9/11/88     major change in treatment of if's. For multi-IF
!                        data, rpfits should be called once per IF (i.e.
!                        several times per integration), with a formal
!                        parameter if_no varying from 1 to n_if.
!                        A new group will be written for each IF.
!                        PTI data will continue to be written with
!                        NSTOK = 2.
!        rpn 9/11/88     added su and fg tables
!        rpn 8/2/89      dates changed from AEST to UT
!        rpn 10/2/89     changed INTEGER*4 declaration to INTEGER for
!                        AIPS.
!        rpn 17/2/89     Put in INDEX common
!        rpn 24/5/89     Put in VERSION string
!        rpn 27/7/89     Put in if_sampl, ant_mount, changed names of
!                        pressure etc to ant_...
!        rpn 10/10/89    put in su_found, if_found, etc.
!        rpn 11/10/89    put in MT commons
!        rpn 8/11/89     put in longer strings for first 4 variables in
!                        /NAMES/
!        rpn 8/11/89     put in su_rad, su_decd
!        rpn 20/3/90     put in su_num, if_num, and changed ant_no to
!                        ant_num.  Added sc common.  Put in write_wt,
!                        if_ref.
!        rpn 22/3/90     put in CU common
!        hm 11/5/90      removed tabs and changed real*4 to real
!                        also cut lines down to 72 characters
!        hm 2/7/90       removed unused variables and changed real*8
!                        to double precision.
!        hm 14/11/91     Added if_sumul and if_chain arrays to IF table 
!                        - to handle sumiltaneous frequencies.
!        hm 11/3/92      Increased max_su from 16 to 500 to allow
!                        for mosaicing of up to 500 sources per scan.
!                        Allow for separate phase and pointing centres
!                        by adding new arrays for pointing centres -
!                        su_pra, su_pdec, su_prad, su_pdecd
!        hm 19/3/92      Added sc_srcno
!
!	Layout of RPFITS common block (RPF.DEF)
!
%COMMENT="RPF.DEF is an INCLUDE file for the "rpfitsin"  program"
%COMMENT=" "
!
%VERSION=1
%SYSTEM=1
%USER=WNB
%%DATE
%%NAME
!
%LOCAL=ANT_MAX=6
%LOCAL=MAX_CARD=256
%LOCAL=MAX_IF=8
%LOCAL=POL_MAX=8
%LOCAL=MAX_SU=500
%LOCAL=MAX_FG=32
%LOCAL=MAX_NX=256
%LOCAL=MAX_MT=256
%LOCAL=MAX_SC=16
%LOCAL=MAX_CU=32
!-
.DEFINE
  .PARAMETER
	ANT_MAX		J		/ANT_MAX/
	MAX_CARD	J		/MAX_CARD/
	MAX_IF		J		/MAX_IF/
	POL_MAX		J		/POL_MAX/
	MAX_SU		J		/MAX_SU/
	MAX_FG		J		/MAX_FG/
	MAX_NX		J		/MAX_NX/
	MAX_MT		J		/MAX_MT/
	MAX_SC		J		/MAX_SC/
	MAX_CU		J		/MAX_CU/
  .DATA
!
!  Local variables:
!
  .COMMON
! 
! PARAM
!
	NSTOK	J
	NFREQ	J
	NCOUNT	J
	INTIME	J
	RA	D
	DEC	D
	FREQ	D
	DFREQ	D
	NSCAN	J
	COORD	C8
	WRITE_WT L
!
! NAMES
!
	OBJECT		C16
	INSTRUMENT	C16
	CAL		C16
	RP_OBSERVER	C16
	DATOBS		C8
	DATWRIT		C8
	FILE		C80
	DATSYS		C8
	VERSION		C8
!
! SPECT
!
	IVELREF	J
	RFREQ	D
	VEL1	D
!
! ANTEN
!
	NANT		J
	X		D(ANT_MAX)
	Y		D(ANT_MAX)
	Z		D(ANT_MAX)
	STA		C8(ANT_MAX)
	X_ARRAY		D
	Y_ARRAY		D
	Z_ARRAY		D
	ANT_NUM		J(ANT_MAX)
	AXIS_OFFSET	D(ANT_MAX)
	ANT_MOUNT	J(ANT_MAX)
	FEED_TYPE	C2(2,ANT_MAX)
	FEED_PA		D(2,ANT_MAX)
	FEED_CAL	D(ANT_MAX,MAX_IF,POL_MAX)
	AN_FOUND	L
!
! CARDS
!
	NCARD	J
	CARD	C80(MAX_CARD)
!
! EPHEM
!
	RP_DEFEAT	J
	RP_UTCMTAI	D
	RP_C		D(12)
	RP_DJMREFP	D
	RP_DJMREFT	D
!
! IF
!
	N_IF		J
	IF_FREQ		D(MAX_IF)
	IF_INVERT	J(MAX_IF)
	IF_BW		D(MAX_IF)
	IF_NFREQ	J(MAX_IF)
	IF_NSTOK	J(MAX_IF)
	IF_CSTOK	C2(4,MAX_IF)
	IF_SAMPL	J(MAX_IF)
	IF_FOUND	L
	IF_NUM		J(MAX_IF)
	IF_REF		D(MAX_IF)
	IF_SIMUL	J(MAX_IF)
	IF_CHAIN	J(MAX_IF)
!
! SU
!
	N_SU		J
	SU_NAME		C16(MAX_SU)
	SU_RA		D(MAX_SU)
	SU_DEC		D(MAX_SU)
	SU_CAL		C4(MAX_SU)
	SU_FOUND	L
	SU_RAD		D(MAX_SU)
	SU_DECD		D(MAX_SU)
	SU_NUM		J(MAX_SU)
	SU_PRA		D(MAX_SU)
	SU_PDEC		D(MAX_SU)
	SU_PRAD		D(MAX_SU)
	SU_PDECD	D(MAX_SU)
!
! FG
!
	N_FG		J
	FG_ANT		J(2,MAX_FG)
	FG_UT		D(2,MAX_FG)
	FG_IF		J(2,MAX_FG)
	FG_CHAN		J(2,MAX_FG)
	FG_STOK		J(2,MAX_FG)
	FG_REASON	C24(MAX_FG)
	FG_FOUND	L
!
! NX
!
	N_NX		J
	NX_REC		J(MAX_NX)
	NX_DATE		C8(MAX_NX)
	NX_UT		D(MAX_NX)
	NX_SOURCE	C16(MAX_NX)
	NX_FOUND	L
!
! MT
!
	N_MT		J
	MT_ANT		J(MAX_MT)
	MT_UT		D(MAX_MT)
	MT_PRESS	D(MAX_MT)
	MT_TEMP		D(MAX_MT)
	MT_HUMID	D(MAX_MT)
	MT_FOUND	L
!
!INDEX
!
	RP_IOSTAT	J
!
! SC
!
	SC_UT		E
	SC_ANT		J
	SC_IF		J
	SC_Q		J
	SC_CAL		E(MAX_SC,MAX_IF,ANT_MAX)
	SC_SRCNO	J	
!
!CU
!
	N_CU		J
	CU_UT		D(MAX_CU)
	CU_ANT		J(MAX_CU)
	CU_IF		J(MAX_CU)
	CU_CAL1		D(MAX_CU)
	CU_CAL2		D(MAX_CU)
	CU_CH1		J(MAX_CU)
	CU_CH2		J(MAX_CU)
	CU_FOUND	L
.END
