/*+ Created from fca.dsc on 000922 at 11:09:12 at duw01
..  FCA_T.INC
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
..  FCA translation definitions:
..                                                                            */
	short fca_t [4][2] ;
	} fca__t = {
                3, 61,
                1, 80,
                3, 8,
                0, 1 };
/*-                                                                           */
