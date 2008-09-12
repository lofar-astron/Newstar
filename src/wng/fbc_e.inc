/*+ Created from fbc.dsc on 000922 at 11:09:16 at duw01
..  FBC_E.INC
..  WNB 000922
..
..  Revisions:
..
..      WNB 930803      Rearrange text
..      WNB 890724      Original version
..                                                                            */
/*
..  Result:
..
..  FBC.DSC defines the FBC (File Buffer Controlblock)
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
..  FBC edit definitions:
..                                                                            */
#define FBCEDL 6                                /* Length table */
#define FBC__EL 6                               /* Length table */
	static char fbc_ec [6][4][12] = {
                "BQA","SJ"," "," ",
                "BQT","SJ"," "," ",
                "ADDR","SJ"," "," ",
                "DISK","SJ"," "," ",
                "DISKND","SJ"," "," ",
                "BITS","SJ"," "," "};
	static int fbc_ej [6][4] = {
                0,2,0,4,
                8,2,0,4,
                16,1,0,4,
                20,1,0,4,
                24,1,0,4,
                28,1,0,4};
/*-                                                                           */
