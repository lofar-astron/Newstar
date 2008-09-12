/*+ Created from fel.dsc on 000922 at 11:09:18 at duw01
..  FEL_T.INC
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
..      Specification of translation tables:
..
..       0= end of table                1= character
..       2= 16 bits integer             3= 32 bits integer
..       4= 32 bits real                5= 64 bits real
..       6= repeat                      7= end repeat
..       8= undefined                   9= byte
..      10= external repeat            11= start union
..      12= start map                  13= end union
..      14= 64 bits complex            15= 128 bits complex
..                                                                            */
	static struct {
/*
..  FEL translation definitions:
..                                                                            */
	short fel_t [2][2] ;
	} fel__t = {
                3, 6,
                0, 1 };
/*-                                                                           */
