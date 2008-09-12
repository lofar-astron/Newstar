/*+ Created from mca.dsc on 000922 at 11:09:09 at duw01
..  MCA_E.INC
..  WNB 000922
..
..  Revisions:
..
..      WNB 930811      Add some names
..      WNB 930803      Use WNTINC features
..      WNB 890724      Original version
..                                                                            */
/*
..  Result:
..
..  MCA.DSC defines the MCA (Magnetic tape Control Area)
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
..  MCA edit definitions:
..                                                                            */
#define MCAEDL 14                               /* Length table */
#define MCA__EL 14                              /* Length table */
	static char mca_ec [14][4][12] = {
                "LINK","SJ"," "," ",
                "TID","SJ"," "," ",
                "SIZE","SJ"," "," ",
                "CHAN","SJ"," "," ",
                "IOSB","SJ"," "," ",
                "BITS","SJ"," "," ",
                "DENS","SJ"," "," ",
                "UNDES","SJ"," "," ",
                "UNIT","UB"," "," ",
                "FCA","SJ"," "," ",
                "MAGF","SJ"," "," ",
                "VOL","AL"," "," ",
                "HD1","AL"," "," ",
                "HD2","AL"," "," "};
	static int mca_ej [14][4] = {
                0,1,0,4,
                4,1,0,4,
                8,1,0,4,
                12,1,0,4,
                16,2,0,4,
                24,1,0,4,
                28,1,0,4,
                32,2,0,4,
                40,32,0,1,
                72,1,0,4,
                76,1,0,4,
                80,1,0,80,
                160,1,0,80,
                240,1,0,80};
/*-                                                                           */
