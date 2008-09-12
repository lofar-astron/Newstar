C+ NSCUV1.FOR
C  WNB 910220
C
C  Revisions:
C       CMV 970130      Minor changes in formats
C
	LOGICAL FUNCTION NSCUV1(OMCA,NPOL,NFRQ,FRQTB,STPTB,STH,OHW,SCW,
	1		IATOFF,DATB,UT1UTC,GSTIAT)
C
C  Write UVFITS Antenna table header
C
C  Result:
C
C	NSCUV1_L = NSCUV1_L( OMCA_J:I, NPOL_J:I, NFRQ_J:I,
C				FRQTB_D:I, STPTB_D:I, STH_B(0:*):I,
C				OHW_B(0:*):I, SCW_B(0:*):I,
C				IATOFF_D:I, DATB_I(0:2):I, UT1UTC_D:I,
C				GSTIAT_D:I)
C				will write the UVFITS AN header.
C				OMCA is the output file, NPOL and NFRQ the
C				number of polarisation and frequency channels.
C				STH, OHW and SCW are the Set header, the OH
C				block and the SC block.
C				IATOFF: jump seconds IAT-UTC in days
C				DATB: reference date as y,m,d
C				UT1UTC: UT1-UTC in days
C				GSTIAT: GST at IAT=0 in days
C				NFRQ: # of frequency points
C				FRQTB: first frequency
C				STPTB: step in frequency
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
	DOUBLE PRECISION FRQTB		!FIRST FREQUENCY
	DOUBLE PRECISION STPTB		!STEP IN FREQUENCY
	BYTE STH(0:*)			!SET HEADER
	BYTE OHW(0:*)			!OH BLOCK
	BYTE SCW(0:*)			!SC BLOCK
	DOUBLE PRECISION IATOFF		!LEAP SECONDS (DAYS)
	INTEGER*2 DATB(0:2)		!Y,M,D
	DOUBLE PRECISION UT1UTC		!UT1-UTC (DAYS)
	DOUBLE PRECISION GSTIAT		!GST FOR IAT=0 (DAYS)
C
C  Function references:
C
	DOUBLE PRECISION WNGGD		!GET VALUE
	LOGICAL NSCUWF			!FILL FITS LINE
	LOGICAL NSCUMF,NSCUMS		!MAKE FITS LINE
C
C  Data declarations:
C
C-
C
C INIT
C
	NSCUV1=.TRUE.					!ASSUME OK
C
C ANTENNA TABLE (AN)
C
	IF (.NOT.NSCUMS(OMCA,V_C,'XTENSION','A3DTABLE',
	1				'Extension type')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_J,'BITPIX',8,
	1				'Binary data')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_J,'NAXIS',2,
	1				'Matrix')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_J,'NAXIS1',74,
	1				'Table width')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_J,'NAXIS2',STHTEL,
	1				'Table length')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_J,'PCOUNT',0,
	1				'# random parameters')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_J,'GCOUNT',1,
	1				'# of groups')) GOTO 910
C
	IF (.NOT.NSCUMS(OMCA,V_C,'EXTNAME','AIPS AN',
	1				'Type')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_J,'EXTVER',1,
	1				'Version')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_J,'EXTLEVEL',1,
	1				'Hierarchy')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'AUTHOR','NSCAN',
	1				'Produced by')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'REFERENC','NFRA-1',
	1				'Local contact')) GOTO 910
C
	IF (.NOT.NSCUMF(OMCA,V_J,'TFIELDS',12,
	1				'Fields per row')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TFORM1','8A',
	1				'Fortran format')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TTYPE1','ANNAME',
	1				'Antenne name')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TUNIT1',' ',
	1				'No units')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TFORM2','3D',
	1				'Fortran format')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TTYPE2','STABXYZ',
	1				'Antenna positions')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TUNIT2','METERS',
	1				'Units')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TFORM3','0D',
	1				'Fortran format')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TTYPE3','ORBPARM',
	1				'Orbital parameters')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TUNIT3',' ',
	1				'No units')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TFORM4','1I',
	1				'Fortran format')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TTYPE4','NOSTA',
	1				'Station number (1...)')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TUNIT4',' ',
	1				'No units')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TFORM5','1I',
	1				'Fortran format')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TTYPE5','MNTSTA',
	1				'Antenna mount:')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TUNIT5',' ',
	1				'Units: 1=equatorial')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TFORM6','1E',
	1				'Fortran format')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TTYPE6','STAXOF',
	1				'Antenna axis ofdfset')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TUNIT6','METERS',
	1				'Units')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TFORM7','1A',
	1				'Fortran format')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TTYPE7','POLTYA',
	1				'Pol. type first dipole')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TUNIT7',' ',
	1				'Units: ''X''=X')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TFORM8','1E',
	1				'Fortran format')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TTYPE8','POLAA',
	1				'P.a. first dipole')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TUNIT8','DEGREES',
	1				'Units')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TFORM9','3E',
	1				'Fortran format')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TTYPE9','POLCALA',
	1			'Pol. cal. param. (see POLTYPE)')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TUNIT9',' ',
	1				'No units')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TFORM10','1A',
	1				'Fortran format')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TTYPE10','POLTYB',
	1				'Second dipole')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TUNIT10',' ',
	1				'No units')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TFORM11','1E',
	1				'Fortran format')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TTYPE11','POLAB',
	1				'Second dipole')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TUNIT11','DEGREES',
	1				'Units')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TFORM12','3E',
	1				'Fortran format')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TTYPE12','POLCALB',
	1				'Second dipole')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'TUNIT12',' ',
	1				'No units')) GOTO 910
C
	IF (.NOT.NSCUMF(OMCA,V_D,'ARRAYX',3828440.6381D0,
	1			'X position array (earth centred')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_D,'ARRAYY',445226.0299D0,
	1			'Y position array (Mk3 system)')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_D,'ARRAYZ',5064923.0797D0,
	1				'Z position array (meters)')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_D,'GSTIA0',360D0*GSTIAT,
	1				'GST at IAT=0 on RDATE')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_D,'DEGPDY',360D0*WNGGD(STH(STH_UTST_1)),
	1			'Earth rotation rate(deg/day)')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_D,'FREQ',
	1		(FRQTB+((NFRQ-1)/2)*STPTB)*1D6,
	1				'Reference frequency (Hz)')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_T,'RDATE',DATB,
	1				'Reference date')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_D,'POLARX',0D0,
	1			'North pole X on RDATE (meters)')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_D,'POLARY',0D0,
	1				'North pole Y on RDATE')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_D,'IATUTC',IATOFF*24D0*3600D0,
	1				'IAT-UTC on RDATE (s)')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_D,'UT1UTC',UT1UTC*24D0*3600D0,
	1				'UT1-UTC on RDATE (s)')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'ARRNAM','WSRT',
	1				'Telescope')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_J,'NUMORB',0,
	1				'# telescopes in orbit')) GOTO 910
	IF (.NOT.NSCUMF(OMCA,V_J,'NOPCAL',3,
	1				'# pol. cal. parameters')) GOTO 910
	IF (.NOT.NSCUMS(OMCA,V_C,'POLTYPE','X-Y LIN',
	1				'Type of polarisation')) GOTO 910
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
	NSCUV1=.FALSE.
 900	CONTINUE
C
	RETURN						!READY
C
C
	END
