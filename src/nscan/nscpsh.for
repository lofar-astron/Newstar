C+ NSCPSH.FOR
C  WNB 900810
C
C  Revisions:
C	WNB 920609	Typo in MJD printout
C	WNB 921102	Cater for full HA range
C	HjV 930311	Change some text
C	WNB 930819	Show dipole code; remove L_
C	CMV 930821	Show set header version number
C	CMV 940107	Change width of FREQ field
C	CMV 940209	Pass and print Category code
C
	SUBROUTINE NSCPSH(PTYPE,STH,SNAM,CATEG)
C
C  Show SCN set header
C
C  Result:
C
C	CALL NSCPSH ( PTYPE_J:I, STH_B(0:*):I, SNAM_J(0:7):I, CATEG_C*(*):I)
C					Show on output PTYPE the set header
C					STH with name SNAM. CATEG is a 
C					character string printed as the
C					catagory of the observation.
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'STH_O_DEF'		!SET HEADER
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER PTYPE			!PRINT TYPE (F_P, F_T ETC)
	BYTE STH(0:*)			!SET HEADER
	INTEGER SNAM(0:*)		!SET NAME
	CHARACTER CATEG*(*)		!Category
C
C  Function references:
C
	INTEGER WNGGJ			!GET J VALUE
	REAL WNGGE			!GET E VALUE
	CHARACTER*32 WNTTSG		!GET SET NAME
C
C  Data declarations:
C
C-
C
C SHOW HEADER
C
	CALL WNCTXT(PTYPE,'!/Sector !AS(#!UJ) - !AD - '//
	1		'Channel !UI - !UJ scans - '//
	1		'!UI pols - Version !UI!/',
	1		WNTTSG(SNAM,0),STH(STH_SETN_1),
	1		STH(STH_FIELD_1),STH_FIELD_N,
	1		STH(STH_CHAN_1),STH(STH_SCN_1),
	1		STH(STH_PLN_1),STH(STH_VER_1))
	CALL WNCTXT(PTYPE,'RA (date)!12C!9$DPF9.4 deg'//
	1		'!29C\HA(start)!41C!7$EAF7.2 deg'//
	1		'!56CObs. yy.day!71C!3$UI\.!3$ZI',
	1		STH(STH_RA_1),STH(STH_HAB_1),
	1		STH(STH_OBS_1+LB_I),STH(STH_OBS_1))
	CALL WNCTXT(PTYPE,'DEC(date)!12C!9$DAF9.4 deg'//
	1		'!29C\HA(end)!41C!7$EAF7.2 deg'//
	1		'!56CDipoles!66C!8$XJ',
	1		STH(STH_DEC_1),WNGGE(STH(STH_HAB_1))+
	1		(WNGGJ(STH(STH_SCN_1))-1)*
	1		WNGGE(STH(STH_HAI_1)),
	1		STH(STH_DIPC_1))
	CALL WNCTXT(PTYPE,'RA (!E6.0)!12C!9$DPF9.4 deg'//
	1		'!29C\HA(step)!42C!6$EAF6.2 deg'//
	1		'!56CEpoch!70C!7$E7.2',
	1		STH(STH_EPO_1),STH(STH_RAE_1),
	1		STH(STH_HAI_1),STH(STH_OEP_1))
	CALL WNCTXT(PTYPE,'DEC(!E6.0)!12C!9$DAF9.4 deg'//
	1		'!29C\HA(average)!42C!6$EAF6.2 deg'//
	1		'!56CVolgnummer!66C!8$UJ',
	1		STH(STH_EPO_1),STH(STH_DECE_1),STH(STH_HAV_1),
	1		STH(STH_VNR_1))
	CALL WNCTXT(PTYPE,'Frequency!11C!10$D10.4 MHz'//
	1		'!29C\# of ifrs!42C!3$UJ'//
	1		'!56CBackend!70C!4$UI',
	1		STH(STH_FRQ_1),STH(STH_NIFR_1),STH(STH_BEC_1))
	CALL WNCTXT(PTYPE,'Bandwidth!12C!9$E9.4 MHz'//
	1		'!29C\Prec. rot.!42C!6$EAF6.2 deg'//
	1		'!56CPointing Set!70C!4$UI',
	1		STH(STH_BAND_1),STH(STH_PHI_1),STH(STH_PTS_1))
	CALL WNCTXT(PTYPE,'Category!12C!AS'//
	1	        '!56C\MJD(start)!69C!11$D12.5',
	1		CATEG,STH(STH_MJD_1))
	CALL WNCTXT(PTYPE,'!/Telescope positions 9, A, B, C, D = '//
	1		'!5E5.0',
	1		STH(STH_RTP_1+9*LB_E))
	CALL WNCTXT(PTYPE,'REDUN M.E. !8$4E8.1',
	1		STH(STH_REDNS_1))
	CALL WNCTXT(PTYPE,'ALIGN M.E. !8$4E8.1!/',
	1		STH(STH_ALGNS_1))
C
	RETURN
C
C
	END
