C+ NSCUV5.FOR
C  WNB 910220
C
C  Revisions:
C
	LOGICAL FUNCTION NSCUV5(OMCA,NPOL,NFRQ,STH,OHW,SCW)
C
C  Write UVFITS Bandpass table header
C
C  Result:
C
C	NSCUV5_L = NSCUV5_L( OMCA_J:I, NPOL_J:I, NFRQ_J:I, STH_B(0:*):I,
C				OHW_B(0:*):I, SCW_B(0:*):I)
C				will write the UVFITS BP header.
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
	NSCUV5=.TRUE.					!ASSUME OK
C
C BANDPASS TABLE (BP)
C
	IF (.NOT.NSCUMS(OMCA,V_C,'XTENSION','A3DTABLE',
	1				'Extension type')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_J,'BITPIX',8,
	1				'Binary data')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_J,'NAXIS',2,
	1				'Matrix')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_J,'NAXIS1',50,
	1				'Table width')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_J,'NAXIS2',STHTEL,
	1				'Table length')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_J,'PCOUNT',0,
	1				'# random parameters')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_J,'GCOUNT',1,
	1				'# of groups')) GOTO 910
C
	IF (.NOT.NSCUMS(OMCA,V_C,'EXTNAME','AIPS BP',
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
	IF (.NOT.NSCUMF(OMCA,V_J,'TFIELDS',13,
	1				'Fields per row')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TFORM1','1D',
	1				'Fortran format')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TTYPE1','TIME',
	1				'Name field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TUNIT1','DAYS',
	1				'Units field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TFORM2','1E',
	1				'Fortran format')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TTYPE2','INTERVAL',
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
	IF (.NOT.NSCUMS(OMCA,V_C,'TTYPE4','SUBARRAY',
	1				'Name field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TUNIT4',' ',
	1				'Units field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TFORM5','1I',
	1				'Fortran format')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TTYPE5','ANTENNA',
	1				'Name field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TUNIT5',' ',
	1				'Units field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TFORM6','1E',
	1				'Fortran format')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TTYPE6','BANDWIDTH',
	1				'Name field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TUNIT6','HZ',
	1				'Units field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TFORM7','1D',
	1				'Fortran format')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TTYPE7','IF FREQ',
	1				'Name field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TUNIT7','HZ',
	1				'Units field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TFORM8','1I',
	1				'Fortran format')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TTYPE8','REFANT 1',
	1				'Name field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TUNIT8',' ',
	1				'Units field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TFORM9','1E',
	1				'Fortran format')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TTYPE9','REAL 1',
	1				'Name field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TUNIT9',' ',
	1				'Units field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TFORM10','1E',
	1				'Fortran format')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TTYPE10','IMAG 1',
	1				'Name field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TUNIT10',' ',
	1				'Units field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TFORM11','1I',
	1				'Fortran format')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TTYPE11','REFANT 2',
	1				'Name field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TUNIT11',' ',
	1				'Units field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TFORM12','1E',
	1				'Fortran format')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TTYPE12','REAL 2',
	1				'Name field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TUNIT12',' ',
	1				'Units field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TFORM13','1E',
	1				'Fortran format')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TTYPE13','IMAG 2',
	1				'Name field')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TUNIT13',' ',
	1				'Units field')) GOTO 910
C
	IF (.NOT.NSCUMF(OMCA,V_J,'NO_ANT',STHTEL,
	1				'# of antennas')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_J,'NO_POL',2,
	1				'# of polarisations')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_J,'NO_IF',1,
	1				'# of IF pairs')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_J,'NO_CHAN',1,
	1				'# of channels')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_J,'STRT_CHN',1,
	1				'Start channel')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_J,'ISORTORD',1,
	1				'Sort order')) GOTO 910
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
	NSCUV5=.FALSE.
 900	CONTINUE
C
	RETURN						!READY
C
C
	END
