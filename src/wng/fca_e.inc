/*+ Created from fca.dsc on 000922 at 11:09:13 at duw01
..  FCA_E.INC
..  WNB 000922
..
..  Revisions:
..
..      WNB 930811      Add some names; change FIB length; add FAT*
..      WNB 930803      Use WNTINC options
..      WNB 890724      Original version
..      JPH 930415      FCA_x_WRT --> FCA_x_WRTAPE
..                                                                            */
/*
..  Result:
..
..  FCA.DSC defines the FCA (File Control Area)
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
..  FCA edit definitions:
..                                                                            */
#define FCAEDL 40                               /* Length table */
#define FCA__EL 40                              /* Length table */
	static char fca_ec [40][4][12] = {
                "LINK","SJ"," "," ",
                "TID","SJ"," "," ",
                "SIZE","SJ"," "," ",
                "CHAN","SJ"," "," ",
                "IOSB","SJ"," "," ",
                "IOSBI","SI"," "," ",
                "BITS","SJ"," "," ",
                "FIBDES","SJ"," "," ",
                "DID","SJ"," "," ",
                "ATRJ","SJ"," "," ",
                "ATR","UB"," "," ",
                "BQT","SJ"," "," ",
                "BQA","SJ"," "," ",
                "BLEN","SJ"," "," ",
                "BCP","SJ"," "," ",
                "FEA","SJ"," "," ",
                "FEE","SJ"," "," ",
                "FEP","SJ"," "," ",
                "FEL","SJ"," "," ",
                "MCA","SJ"," "," ",
                "HIBLK","SJ"," "," ",
                "EOF","SJ"," "," ",
                "RAD","SJ"," "," ",
                "DAD","SJ"," "," ",
                "BAD","SJ"," "," ",
                "LEN","SJ"," "," ",
                "ACLEN","SJ"," "," ",
                "EF","SJ"," "," ",
                "EFA","SJ"," "," ",
                "ERR","SJ"," "," ",
                "MAP","SJ"," "," ",
                "MAB","SJ"," "," ",
                "MAW","SJ"," "," ",
                "FNAML","SJ"," "," ",
                "FIBJ","SJ"," "," ",
                "FIBI","SI"," "," ",
                "FIB","UB"," "," ",
                "FNAM","AL"," "," ",
                "RECATRJ","SJ"," "," ",
                "RECATR","UB"," "," "};
	static int fca_ej [40][4] = {
                0,1,0,4,
                4,1,0,4,
                8,1,0,4,
                12,1,0,4,
                16,2,0,4,
                16,4,0,2,
                24,1,0,4,
                28,2,0,4,
                36,2,0,4,
                44,3,0,4,
                44,12,0,1,
                56,2,0,4,
                64,2,0,4,
                72,1,0,4,
                76,1,0,4,
                80,2,0,4,
                88,2,0,4,
                96,1,0,4,
                100,1,0,4,
                104,1,0,4,
                108,1,0,4,
                112,1,0,4,
                116,1,0,4,
                120,1,0,4,
                124,1,0,4,
                128,1,0,4,
                132,1,0,4,
                136,1,0,4,
                140,1,0,4,
                144,1,0,4,
                148,1,0,4,
                152,1,0,4,
                156,1,0,4,
                160,1,0,4,
                164,20,0,4,
                164,40,0,2,
                164,80,0,1,
                244,1,0,80,
                324,8,0,4,
                324,32,0,1};
/*-                                                                           */
