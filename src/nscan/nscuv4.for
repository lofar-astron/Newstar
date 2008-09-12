C+ NSCUV4.FOR
C  WNB 910220
C
C  Revisions:
C
	LOGICAL FUNCTION NSCUV4(OMCA,NPOL,NFRQ,STH,OHW,SCW)
C
C  Write UVFITS Gain table header
C
C  Result:
C
C	NSCUV4_L = NSCUV4_L( OMCA_J:I, NPOL_J:I, NFRQ_J:I, STH_B(0:*):I,
C				OHW_B(0:*):I, SCW_B(0:*):I)
C				will write the UVFITS SN header.
C				OMCA is the output file, NPOL and NFRQ the
C				number of polarisation and frequency channels.
C				STH, OHW and SCW are the Set header, the OH
C				block and the SC block.
C
C PIN references:
C
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'STH_O_DEF'		!SET HEADER
	INCLUDE 'OHW_O_DEF'		!OH BLOCK
	INCLUDE 'SCW_O_DEF'		!SC BLOCK
C
C  Parameters:
C
	INTEGER LRCLEN			!RECORD LENGTH FITS (CHANGE ALSO NSCUWB)
	  PARAMETER (LRCLEN=2880)
	INTEGER CDILEN			!CARD IMAGE LENGTH
	  PARAMETER (CDILEN=80)
	INTEGER NCDI			!# OF CARD IMAGES/RECORD
	  PARAMETER (NCDI=LRCLEN/CDILEN)
	INTEGER DBLEN			!DATA BUFFER LENGTH
	  PARAMETER (DBLEN=512)		!BYTES	END IF
	INTEGER V_Z,V_L,V_I,V_J,V_C,V_E,V_D,V_T,
	1		V_XI,V_XJ	!CODES FOR FITS CARD LINES
	  PARAMETER (V_Z=0,V_L=1,V_I=2,V_J=3,V_C=4,V_E=5,V_D=6,
	1		V_T=7,V_XI=8,V_XJ=9)
C
C  Arguments:
C
	INTEGER OMCA			!FILE POINTER
	INTEGER NPOL			!# OF POL. TO DO
	INTEGER NFRQ			!# OF FREQ. TO DO
	BYTE STH(0:*)			!SET HEADER
	BYTE OHW(0:*)			!OH BLOCK
	BYTE SCW(0:*)			!SC BLOCK
C
C  Function references:
C
	LOGICAL NSCUWF			!FILL FITS LINE
	LOGICAL NSCUMF,NSCUMS		!MAKE FITS LINE
C
C  Data declarations:
C
C-
C
C INIT
C
	NSCUV4=.TRUE.					!ASSUME OK
C
C GAIN TABLE (SN)
C
	IF (.NOT.NSCUMS(OMCA,V_C,'XTENSION','A3DTABLE',
	1				'Extension type')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_J,'BITPIX',8,
	1				'Binary data')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_J,'NAXIS',2,
	1				'Matrix')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_J,'NAXIS1',64,
	1				'Table width')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_J,'NAXIS2',STHTEL,
	1				'Table length')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_J,'PCOUNT',0,
	1				'# random parameters')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_J,'GCOUNT',1,
	1				'# of groups')) GOTO 910
C
	IF (.NOT.NSCUMS(OMCA,V_C,'EXTNAME','AIPS SN',
	1				'Type')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_J,'EXTVER',1,
	1				'Version')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_J,'EXTLEVEL',1,
	1				'Hierarchy')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'AUTHOR','WNB',
	1				'Produced by')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'REFERENC','NFRA-1',
	1				'Local contact')) GOTO 910
C
	IF (.NOT.NSCUMF(OMCA,V_J,'TFIELDS',18,
	1				'Fields per row')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TFORM1','1D',
	1				'Fortran format')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TTYPE1','TIME',
	1				'Name field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TUNIT1','DAYS',
	1				'Units field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TFORM2','1E',
	1				'Fortran format')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TTYPE2','TIME INTERVAL',
	1				'Name field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TUNIT2','DAYS',
	1				'Units field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TFORM3','1I',
	1				'Fortran format')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TTYPE3','SOURCE ID',
	1				'Name field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TUNIT3',' ',
	1				'Units field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TFORM4','1I',
	1				'Fortran format')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TTYPE4','ANTENNA NO.',
	1				'Name field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TUNIT4',' ',
	1				'Units field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TFORM5','1I',
	1				'Fortran format')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TTYPE5','SUBARRAY',
	1				'Name field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TUNIT5',' ',
	1				'Units field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TFORM6','1I',
	1				'Fortran format')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TTYPE6','NODE NO.',
	1				'Name field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TUNIT6',' ',
	1				'Units field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TFORM7','1E',
	1				'Fortran format')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TTYPE7','REAL1',
	1				'Name field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TUNIT7',' ',
	1				'Units field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TFORM8','1E',
	1				'Fortran format')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TTYPE8','IMAG1',
	1				'Name field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TUNIT8',' ',
	1				'Units field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TFORM9','1E',
	1				'Fortran format')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TTYPE9','DELAY 1',
	1				'Name field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TUNIT9','SECONDS',
	1				'Units field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TFORM10','1E',
	1				'Fortran format')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TTYPE10','RATE 1',
	1				'Name field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TUNIT10','SEC/SEC',
	1				'Units field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TFORM11','1E',
	1				'Fortran format')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TTYPE11','WEIGHT 1',
	1				'Name field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TUNIT11',' ',
	1				'Units field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TFORM12','1I',
	1				'Fortran format')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TTYPE12','REFANT 1',
	1				'Name field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TUNIT12',' ',
	1				'Units field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TFORM13','1E',
	1				'Fortran format')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TTYPE13','REAL 2',
	1				'Name field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TUNIT13',' ',
	1				'Units field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TFORM14','1E',
	1				'Fortran format')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TTYPE14','IMAG 2',
	1				'Name field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TUNIT14',' ',
	1				'Units field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TFORM15','1E',
	1				'Fortran format')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TTYPE15','DELAY 2',
	1				'Name field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TUNIT15','SECONDS',
	1				'Units field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TFORM16','1E',
	1				'Fortran format')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TTYPE16','RATE 2',
	1				'Name field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TUNIT16','SEC/SEC',
	1				'Units field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TFORM17','1E',
	1				'Fortran format')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TTYPE17','WEIGHT 2',
	1				'Name field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TUNIT17',' ',
	1				'Units field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TFORM18','1I',
	1				'Fortran format')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TTYPE18','REFANT 2',
	1				'Name field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TUNIT18',' ',
	1				'Units field')) GOTO 910
C
	IF (.NOT.NSCUMF(OMCA,V_J,'NO_ANT',STHTEL,
	1				'# of antennas')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_J,'NO_POL',2,
	1				'# of polarisations')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_J,'NO_IF',1,
	1				'# of IF pairs')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_J,'NO_NODES',0,
	1				'# of nodes')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_E,'MGMOD',0E0,
	1				'Mean gain modulus')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_L,'APPLIED',.TRUE.,
	1				'If table applied to data')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_J,'TYPE',2,
	1				'Type of table (gain)')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_E,'RA_OFF1',0E0,
	1				'RA offset node 1')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_E,'DEC_OFF1',0E0,
	1				'DEC offset node 1')) GOTO 910
C
	IF (.NOT.NSCUMF(OMCA,V_Z,'END',J,
	1				' ')) GOTO 910
	IF (.NOT.NSCUWF(OMCA)) GOTO 910			!FILL RECORD
C
C END
C
	GOTO 900
C
C ERROR
C
 910	CONTINUE
	NSCUV4=.FALSE.
 900	CONTINUE
C
	RETURN						!READY
C
C
	END
