	BLOCK DATA PPDREC_BLOCK
C	Created by BLDDEF from: _$1$DIA3:[TESTDWARF.SRC.DEF]PPDREC.DEF; on 17-OCT-90
C	HjV 921208	Removed all equivalence and add names to common block
C	HjV 930510	Change all INTEGER*2 into INTEGER*4
C       AXC 010709	Linux port - PPDPD$TRAILOR
C
C
	INTEGER*4 PPDPD$LENGTH
	INTEGER*4 PPDPD$TYPE
	CHARACTER*512 PPDPD$DESCR
	INTEGER*4 PPDPD$FORW
	INTEGER*4 PPDPD$EXTEN
	INTEGER*4 PPDPD$LENG
	CHARACTER*16 PPDPD$UNAM
	CHARACTER*16 PPDPD$PNAM
	INTEGER*4 PPDPD$LUNAM
	CHARACTER*1 PPDPD$DTYPE
	CHARACTER*1 PPDPD$IOCD
	CHARACTER*2 PPDPD$DUMMY
	INTEGER*4 PPDPD$PLEN
	INTEGER*4 PPDPD$NVAL
	INTEGER*4 PPDPD$NSETS
	INTEGER*4 PPDPD$MNVAL
	INTEGER*4 PPDPD$MXVAL
	INTEGER*4 PPDPD$CMAS
	INTEGER*4 PPDPD$AMAS
	INTEGER*4 PPDPD$MNOFF
	INTEGER*4 PPDPD$MNLEN
	INTEGER*4 PPDPD$MXOFF
	INTEGER*4 PPDPD$MXLEN
	INTEGER*4 PPDPD$UOFF
	INTEGER*4 PPDPD$ULEN
	INTEGER*4 PPDPD$SOFF
	INTEGER*4 PPDPD$SLEN
	INTEGER*4 PPDPD$DVOFF
	INTEGER*4 PPDPD$DVLEN
	INTEGER*4 PPDPD$OPOFF
	INTEGER*4 PPDPD$OPLEN
	INTEGER*4 PPDPD$PROFF
	INTEGER*4 PPDPD$PRLEN
	INTEGER*4 PPDPD$GOFF
	INTEGER*4 PPDPD$GLEN
	INTEGER*4 PPDPD$HOFF
	INTEGER*4 PPDPD$HLEN
	BYTE PPDPD$TRAILER(360)
C
	COMMON /PPDPD/ PPDPD$FORW, PPDPD$EXTEN, PPDPD$LENG, PPDPD$UNAM,
     *		PPDPD$PNAM, PPDPD$LUNAM, PPDPD$DTYPE, PPDPD$IOCD,
     *		PPDPD$DUMMY, PPDPD$PLEN, PPDPD$NVAL, PPDPD$NSETS,
     *		PPDPD$MNVAL, PPDPD$MXVAL, PPDPD$CMAS, PPDPD$AMAS,
     *		PPDPD$MNOFF, PPDPD$MNLEN, PPDPD$MXOFF, PPDPD$MXLEN,
     *		PPDPD$UOFF, PPDPD$ULEN, PPDPD$SOFF, PPDPD$SLEN,
     *		PPDPD$DVOFF, PPDPD$DVLEN, PPDPD$OPOFF, PPDPD$OPLEN,
     *		PPDPD$PROFF, PPDPD$PRLEN, PPDPD$GOFF, PPDPD$GLEN,
     *		PPDPD$HOFF, PPDPD$HLEN, PPDPD$TRAILER
C
	DATA PPDPD$FORW/0/	!Required by g77
	DATA PPDPD$HLEN/0/
C
	END
C