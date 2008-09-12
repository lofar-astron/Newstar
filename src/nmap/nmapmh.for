C+ NMAPMH.FOR
C  WNB 910320
C
C  Revisions:
C	WNB 911105	Change RAO/DECO definition
C	WNB 920114	Add Set name on type at end
C	HjV 930311	Change some text
C	CMV 931123	Make bottomline fit on single line
C	CMV 931206	Change text (#of input sectors)
C	CMV 940523	Add space in output text
C
	SUBROUTINE NMAPMH(T,MPH,MNAM,WMPNOD)
C
C  Print/type Map-header
C
C  Result:
C
C	CALL NMAPMH (T_J:I, MPH_B(0:*):I, MNAM_J(0:7):I, WMPNOD_C*:I)
C				Show on output T the map header
C				MPH with name MNAM in node WMPNOD.
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'MPH_O_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER T			!PRINTING TYPE
	BYTE MPH(0:*)			!MAP HEADER
	INTEGER MNAM(0:7)		!MAP NAME
	CHARACTER*(*) WMPNOD		!NODE NAME
C
C  Function references:
C
	CHARACTER*32 WNTTSG		!GET MAP SET NAME
	DOUBLE PRECISION WNGDFD		!CONVERT ANGLE
	INTEGER*2 WNGGI			!GET I VALUE
	DOUBLE PRECISION WNGGD		!GET D VALUE
C
C  Data declarations:
C
	CHARACTER*8 TAPCOD(0:5)		!TAPERS
	  DATA TAPCOD/'No','Gaussian','Linear','Natural',
	1		'Overr','Rgauss'/
	CHARACTER*12 CVLCOD(0:5)	!CONVOLUTIONS
	  DATA CVLCOD/'No','Gaussian','Box','Prolate 4*4',
	1		'Expsinc','Prolate 6*6'/
	CHARACTER*5 CCVCOD(0:1)		!DE-CONVOLVE
	  DATA CCVCOD/'not c','c'/
	CHARACTER*3 CLPCOD(0:1)		!CLIP
	  DATA CLPCOD/'Not',' '/
	CHARACTER*2 SSBCOD(0:1)		!SOURCE SUBTRACT
	  DATA SSBCOD/'No',' '/
C-
C
C FILE INFO
C
	CALL WNCTXT(T,'!/!AS(#!UJ) type !AL4 in node !AS',
	1		WNTTSG(MNAM,0),	MPH(MPH_SETN_1),
	1		MPH(MPH_TYP_1),WMPNOD)
C
C GENERAL INFO
C
	CALL WNCTXT(T,'!/Field: !AL12!38C\User comment: !AL24',
	1		MPH(MPH_FNM_1),MPH(MPH_UCM_1))
	CALL WNCTXT(T,'RA: !DPF10.5 deg  Dec: !DAF10.5 deg  '//
	1		'Epoch: !E6.1  Frequency: !D6.0 MHz',
	1		MPH(MPH_RA_1),MPH(MPH_DEC_1),
	1		MPH(MPH_EPO_1),MPH(MPH_FRQ_1))
C
C DATA DESCRIPTION
C
	CALL WNCTXT(T,'!/RA (!E12.0)!12C!10$DPF10.5 deg!56C'//
	1		'Obs.day!72C!4$UI',
	1		MPH(MPH_EPO_1),
	1		MPH(MPH_RAO_1),MPH(MPH_ODY_1))
	CALL WNCTXT(T,'Dec(!E12.0)!12C!10$DAF10.5 deg!56C'//
	1		'Obs.year!72C!4$UI',
	1		MPH(MPH_EPO_1),
	1		MPH(MPH_DECO_1),MPH(MPH_OYR_1))
	CALL WNCTXT(T,'Frequency!12C!10$D10.5 MHz!56C'//
	1		'Epoch!69C!7$E7.2',
	1		MPH(MPH_FRQO_1),MPH(MPH_OEP_1))
	CALL WNCTXT(T,'Bandwidth!12C!10$D10.5 MHz!56C'//
	1		'Map epoch!69C!7$E7.2',
	1		MPH(MPH_BDW_1),MPH(MPH_EPO_1))
C
C MAP DESCRIPTION
C
	CALL WNCTXT(T,'!/Type: !AL4(!AL2)!38CSize: !UJ*!UJ'//
	1		'!56CFFT size: !UJ*!UJ',
	1		MPH(MPH_TYP_1),MPH(MPH_POL_1),
	1		MPH(MPH_NRA_1),MPH(MPH_NDEC_1),
	1		MPH(MPH_FSR_1),MPH(MPH_FSD_1))
	CALL WNCTXT(T,'Fieldsize: !EAF8.4*!EAF8.4 deg!38C'//
	1		'Grid step: !D8.2*!D8.2 arcsec',
	1		MPH(MPH_FRA_1),MPH(MPH_FDEC_1),
	1		3600*WNGDFD(MPH(MPH_SRA_1)),
	1		3600*WNGDFD(MPH(MPH_SDEC_1)))
	CALL WNCTXT(T,'!38C\Fieldshift: !D12.2*!D12.2 arcsec',
	1		WNGGD(MPH(MPH_SHR_1))*3600.*360,
	1		WNGGD(MPH(MPH_SHD_1))*3600.*360.)
	CALL WNCTXT(T,'Maximum: !E9.2 W.U. at !SJ,!SJ!38C'//
	1		'Minimum: !E9.2 W.U.at !SJ,!SJ',
	1		MPH(MPH_MAX_1),MPH(MPH_MXR_1),MPH(MPH_MXD_1),
	1		MPH(MPH_MIN_1),MPH(MPH_MNR_1),MPH(MPH_MND_1))
	CALL WNCTXT(T,'!/Input baselines: !UJ!30C'//
	1		'Input sectors: !UJ!56C\Input points: !UJ!/'//
	1		'Normalisation: !D6'//
	1		'!30C\Noise: !E9.3 W.U.!/',
	1		MPH(MPH_NBL_1),MPH(MPH_NST_1),MPH(MPH_NPT_1),
	1		MPH(MPH_SUM_1),MPH(MPH_NOS_1))
	CALL WNCTXT(T,'!AS taper; !AS conv.(!AS\orrected); '//
	1		'!AS clipped; !AS subtract; !SI de-beam!/',
	1		TAPCOD(WNGGI(MPH(MPH_CD_1+0*LB_I))),
	1		CVLCOD(WNGGI(MPH(MPH_CD_1+1*LB_I))),
	1		CCVCOD(WNGGI(MPH(MPH_CD_1+2*LB_I))),
	1		CLPCOD(WNGGI(MPH(MPH_CD_1+3*LB_I))),
	1		SSBCOD(WNGGI(MPH(MPH_CD_1+4*LB_I))),
	1		MPH(MPH_CD_1+7*LB_I))
C
C FILE INFO
C
	I=IAND(T,-F_P-1)
	IF (I.NE.0) THEN
	  CALL WNCTXT(I,'!AS(#!UJ) type !AL4 in node !AS!/',
	1		WNTTSG(MNAM,0),	MPH(MPH_SETN_1),
	1		MPH(MPH_TYP_1),WMPNOD)
	END IF
C
	RETURN
C
C
	END
