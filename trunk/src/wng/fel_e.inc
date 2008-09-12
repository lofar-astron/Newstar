/*+ Created from fel.dsc on 000922 at 11:09:18 at duw01
..  FEL_E.INC
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
..  FEL.DSC defines the FEL (File ELement description)
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
..  FEL edit definitions:
..                                                                            */
#define FELEDL 5                                /* Length table */
#define FEL__EL 5                               /* Length table */
	static char fel_ec [5][4][12] = {
                "LINK","SJ"," "," ",
                "BITS","SJ"," "," ",
                "BUFAD","SJ"," "," ",
                "BUFLEN","SJ"," "," ",
                "DKAD","SJ"," "," "};
	static int fel_ej [5][4] = {
                0,2,0,4,
                8,1,0,4,
                12,1,0,4,
                16,1,0,4,
                20,1,0,4};
/*-                                                                           */
