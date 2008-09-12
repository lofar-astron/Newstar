/*+ Created from fel.dsc on 000922 at 11:09:18 at duw01
..  FEL_O.INC
..  WNB 000922
..
..  Revisions:
..
..      WNB 930803      Rearrange text
..      WNB 890724      Original version
..                                                                            */
/*
..  Given statements:
..                                                                            */
/*
..  Result:
..
..  FEL.DSC defines the FEL (File ELement description)
..                                                                            */
/*
..  Parameters:
..                                                                            */
#define FEL__NFEL 16                            /* # OF ELEMENTS WANTED */
/*
..  FEL structure definitions:
..                                                                            */
#define FELHDL 24                               /* Length */
#define FELHDV 1                                /* Version */
#define FELHDS 1                                /* System */
#define FEL__L 24                               /* Length */
#define FEL__V 1                                /* Version */
#define FEL__S 1                                /* System */
/*
..  FEL Offsets:
..                                                                            */
struct fel {
	int link[2];                            /* LINK LIST, MUST BE AT 0 */
	int bits;                               /* BITS (BIT0=1: READ, ELSE WRITE) */
	int bufad;                              /* USER BUF ADDRESS */
	int buflen;                             /* USER BUF LENGTH */
	int dkad;                               /* DISK ADDRESS */
};                                              /* END DEFINITION */
/*
..  Given statements:
..                                                                            */
/*-                                                                           */
