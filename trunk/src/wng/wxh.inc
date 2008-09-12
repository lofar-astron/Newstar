/*+ Created from wxh.dsc on 970828 at 16:58:05 at daw18
..  WXH.INC
..  WNB 970828
..
..  Revisions:
..
..      WNB 910828      Original version
..                                                                            */
/*
..  Given statements:
..                                                                            */
/*
..  Result:
..
..  WXH.DEF is an INCLUDE file for exit and other handlers.
..  
..                                                                            */
/*
..  Parameters:
..                                                                            */
/*
..  Data declarations:
..                                                                            */
/*
..  WXH common data:
..                                                                            */
struct wxh_com {
	int xhed;                               /* HEAD OF EXIT HANDLER LIST */
	int xhcc[2];                            /* CONTROL C: 0: INHIBIT */
	                                        /* 1: SEEN DURING INHIBIT */
	int xhrs[5];                            /* RESERVED */
};
/*
..  WXH common block:
..                                                                            */
extern struct wxh_com wxh_com_ ;
/*
..  External initialisation:
..                                                                            */
	extern wxh_bd_() ;
/*
..  Given statements:
..                                                                            */
/*-                                                                           */
