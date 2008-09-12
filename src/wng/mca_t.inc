/*+ Created from mca.dsc on 000922 at 11:09:09 at duw01
..  MCA_T.INC
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
..  MCA translation definitions:
..                                                                            */
	short mca_t [5][2] ;
	} mca__t = {
                3, 10,
                9, 32,
                3, 2,
                1, 240,
                0, 1 };
/*-                                                                           */
