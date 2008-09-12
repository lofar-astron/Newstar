C+ NSCPSL.FOR
C  WNB 900810
C
C  Revisions:
C	JPH 960624	Widen Maximum field fro 8 to 10
C	CMV 000928	Add arguments and line for identification
C
	SUBROUTINE NSCPSL(PTYPE,SCH,SNAM,STH,ISCN)
C
C  Show SCN header
C
C  Result:
C
C	CALL NSCPSL ( PTYPE_J:I, SCH_B(0:*):I), 
C	              SNAM_J(0:7):I, STH_B(0:*):I, ISCN_J:I)
C					Show on output PTYPE the scan header
C					SCH using setname SNAM, set header
C					STH en index ISCN for identification.
C					ISCN should be 0 for the first scan
C					in a set.
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'SCH_O_DEF'		!SCAN HEADER
	INCLUDE 'STH_O_DEF'		!SET HEADER
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER PTYPE			!PRINT TYPE (F_P, F_T ETC)
	BYTE SCH(0:*)			!SCAN HEADER
	BYTE STH(0:*)			!SET HEADER
	INTEGER SNAM(0:7)		!SET NAME
	INTEGER ISCN			!SCAN NUMBER IN SET
C
C  Function references:
C
	INTEGER WNGGJ			!GET J VALUE
	REAL WNGGE			!GET E VALUE
	CHARACTER*32 WNTTSG		!SET NAME
C
C  Data declarations:
C
C-
C
C SHOW HEADER
C
	CALL WNCTXT(PTYPE,
	1	'!/Sector !AS(#!UJ) - scan !UJ of !UJ',
	1	WNTTSG(SNAM,0),STH(STH_SETN_1),
	1	ISCN+1,STH(STH_SCN_1))
	CALL WNCTXT(PTYPE,'HA!12C!9$EAF9.4 deg'//
	1		'!29C\Maximum!38C!10$E10.2 W.U.'//
	1		'!56CBits!66C!8$XJ',
	1		SCH(SCH_HA_1),SCH(SCH_MAX_1),SCH(SCH_BITS_1))
	CALL WNCTXT(PTYPE,'Extinction!13C!9$E9.5 '//
	1		'!29C\Refraction!43C!8$E8.5 '//
	1		'!56CFaraday!68C!7$EAR7.1 deg',
	1		1.+WNGGE(SCH(SCH_EXT_1)),
	1		1.+WNGGE(SCH(SCH_REFR_1)),
	1		SCH(SCH_FARAD_1))
	CALL WNCTXT(PTYPE,'Red. noise: !13C!9$4E9.2',
	1		SCH(SCH_REDNS_1))
	CALL WNCTXT(PTYPE,'Align noise: !13C!9$4E9.2!/',
	1		SCH(SCH_ALGNS_1))
C
	RETURN
C
C
	END
