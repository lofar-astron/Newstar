/*+ Created from wnt.dsc on 970828 at 17:01:50 at daw18
..  WNT_T.INC
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
..  WNTB translation definitions:
..                                                                            */
	short wntb_t [3][2] ;
/*
..  WNTI translation definitions:
..                                                                            */
	short wnti_t [2][2] ;
/*
..  WNTV translation definitions:
..                                                                            */
	short wntv_t [4][2] ;
/*
..  WNTF translation definitions:
..                                                                            */
	short wntf_t [3][2] ;
/*
..  WNTD translation definitions:
..                                                                            */
	short wntd_t [3][2] ;
/*
..  WNTE translation definitions:
..                                                                            */
	short wnte_t [3][2] ;
	} wnt__t = {
                3, 5,
                9, 4,
                0, 1,
                3, 4,
                0, 1,
                1, 16,
                3, 2,
                1, 64,
                0, 1,
                3, 78,
                1, 64,
                0, 1,
                3, 1,
                1, 36,
                0, 1,
                3, 1,
                1, 36,
                0, 1 };
/*-                                                                           */
