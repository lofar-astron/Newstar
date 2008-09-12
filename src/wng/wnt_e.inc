/*+ Created from wnt.dsc on 970828 at 17:01:51 at daw18
..  WNT_E.INC
..  WNB 970828
..
..  Revisions:
..
..      WNB 931216      New edit default for D/E unformatted items
..      WNB 930902      Make 32 array indices, 64 field length
..      WNB 930801      Original version
..                                                                            */
/*
..  Result:
..
..  WNT.DSC describes the include files (WNT_O.DEF[.inc],
..  	WNT.DEF [.inc]) for the WNTINC program.
..  	Most WNTI* routines need:
..  	INCLUDE 'WNT_O_DEF' and
..  	INCLUDE 'WNT_DEF'
..                                                                            */
/*
..      Specification of edit tables:
..
.. The character (_EC) table contains:
..      fieldname, pattern, units, special code
.. The integer (_EJ) table contains:
..      offset, #of values, edit (0=allowed), unit length
..                                                                            */
/*
..  WNTB edit definitions:
..                                                                            */
#define WNTBEDL 5                               /* Length table */
#define WNTB__EL 5                              /* Length table */
	static char wntb_ec [5][4][12] = {
                "CCNT","SJ"," "," ",
                "CNT","SJ"," "," ",
                "ELEN","SJ"," "," ",
                "BPTR","SJ"," "," ",
                "JPTR","SJ"," "," "};
	static int wntb_ej [5][4] = {
                0,1,0,4,
                4,1,0,4,
                8,1,0,4,
                12,1,0,4,
                16,1,0,4};
/*
..  WNTI edit definitions:
..                                                                            */
#define WNTIEDL 4                               /* Length table */
#define WNTI__EL 4                              /* Length table */
	static char wnti_ec [4][4][12] = {
                "FTYP","SJ"," "," ",
                "LCOM","SJ"," "," ",
                "PCOM","SJ"," "," ",
                "PFOR","SJ"," "," "};
	static int wnti_ej [4][4] = {
                0,1,0,4,
                4,1,0,4,
                8,1,0,4,
                12,1,0,4};
/*
..  WNTV edit definitions:
..                                                                            */
#define WNTVEDL 4                               /* Length table */
#define WNTV__EL 4                              /* Length table */
	static char wntv_ec [4][4][12] = {
                "NAM","AL"," "," ",
                "TYP","SJ"," "," ",
                "VAL","SJ"," "," ",
                "STR","AL"," "," "};
	static int wntv_ej [4][4] = {
                0,1,0,16,
                16,1,0,4,
                20,1,0,4,
                24,1,0,64};
/*
..  WNTF edit definitions:
..                                                                            */
#define WNTFEDL 16                              /* Length table */
#define WNTF__EL 16                             /* Length table */
	static char wntf_ec [16][4][12] = {
                "BTYP","SJ"," "," ",
                "ALEN","SJ"," "," ",
                "DTP","SJ"," "," ",
                "ULEN","SJ"," "," ",
                "SLEN","SJ"," "," ",
                "DIM","SJ"," "," ",
                "REFP","SJ"," "," ",
                "NINI","SJ"," "," ",
                "INIP","SJ"," "," ",
                "EDIP","SJ"," "," ",
                "TLEN","SJ"," "," ",
                "OFF","SJ"," "," ",
                "ENT","SJ"," "," ",
                "SREF","SJ"," "," ",
                "IND","SJ"," "," ",
                "NAM","AL"," "," "};
	static int wntf_ej [16][4] = {
                0,1,0,4,
                4,1,0,4,
                8,1,0,4,
                12,1,0,4,
                16,1,0,4,
                20,1,0,4,
                24,1,0,4,
                28,1,0,4,
                32,1,0,4,
                36,1,0,4,
                40,1,0,4,
                44,1,0,4,
                48,1,0,4,
                52,1,0,4,
                56,64,0,4,
                312,1,0,64};
/*
..  WNTD edit definitions:
..                                                                            */
#define WNTDEDL 2                               /* Length table */
#define WNTD__EL 2                              /* Length table */
	static char wntd_ec [2][4][12] = {
                "REP","SJ"," "," ",
                "STR","AL"," "," "};
	static int wntd_ej [2][4] = {
                0,1,0,4,
                4,1,0,36};
/*
..  WNTE edit definitions:
..                                                                            */
#define WNTEEDL 4                               /* Length table */
#define WNTE__EL 4                              /* Length table */
	static char wnte_ec [4][4][12] = {
                "EDIT","SJ"," "," ",
                "PAT","AL"," "," ",
                "UNIT","AL"," "," ",
                "SPEC","AL"," "," "};
	static int wnte_ej [4][4] = {
                0,1,0,4,
                4,1,0,12,
                16,1,0,12,
                28,1,0,12};
/*-                                                                           */
