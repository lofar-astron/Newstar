	BLOCK DATA PPDSTAT_BLOCK
C	Created by BLDDEF from: _$1$DIA3:[TESTDWARF.SRC.DEF]PPDSTAT.DEF; on 17-OCT-90
C	HjV 921208	Removed all equivalence and add names to common block
C	AXC 010709      Linux port - data initialisation
C
C
C
	INTEGER*4 PPS$LENGTH
	INTEGER*4 PPS$TYPE
	INTEGER*4 PPS$MAPB
	INTEGER*4 PPS$MAPE
	INTEGER*4 PPS$INXB
	INTEGER*4 PPS$PARB
	INTEGER*4 PPS$HLPB
	INTEGER*4 PPS$PROTB
	INTEGER*4 PPS$NRINX
	INTEGER*4 PPS$NXTPAR
	INTEGER*4 PPS$NXTPROT
	INTEGER*4 PPS$ENTYP
	INTEGER*4 PPS$NRINXPR
	INTEGER*4 PPS$FABADR
	INTEGER*4 PPS$FABSIZ
C
	COMMON /PPDSTAT/ PPS$MAPB, PPS$MAPE, PPS$INXB, PPS$PARB, PPS$HLPB,
     *		PPS$PROTB, PPS$NRINX, PPS$NXTPAR, PPS$NXTPROT, PPS$ENTYP,
     *		PPS$NRINXPR, PPS$FABADR, PPS$FABSIZ
C
C
	DATA PPS$MAPB /0/
	DATA PPS$MAPE /0/
	DATA PPS$INXB /0/
	DATA PPS$PARB /0/
	DATA PPS$HLPB /0/
	DATA PPS$PROTB /0/
	DATA PPS$NRINX /0/
	DATA PPS$NXTPAR /0/
	DATA PPS$NXTPROT /0/
	DATA PPS$ENTYP /0/
	DATA PPS$NRINXPR /0/
	DATA PPS$FABADR /0/
	DATA PPS$FABSIZ /0/
	END