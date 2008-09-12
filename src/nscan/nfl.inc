/*+ Created from nfl.dsc on 031229 at 12:46:28 at dop19
..  NFL.INC
..  WNB 031229
..
..  Revisions:
..
..      WNB 930803      Remove .INCLUDE
..      WNB 930618      Original version
..                                                                            */
/*
..  Given statements:
..                                                                            */
/*
..  Result:
..
..  NFL.DEF is an INCLUDE file for the NFLAG program
..  
..                                                                            */
/*
..  Parameters:
..                                                                            */
#define MXNSET  64                              /* MAX. # OF SETS */
#define MXNIFR  120                             /* MAX. # OF INTERFEROMETERS */
/*
..  Data declarations:
..                                                                            */
/*
..  NFL common data:
..                                                                            */
struct nfl_com {
  union {
	char  option[24];                       /* PROGRAM OPTION */
	char  opt[3];
  } option;
	char  nodin[80];                        /* INPUT NODE */
	char  ifile[80];                        /* INPUT FILE NAME */
	int  fcain;                             /* INPUT FCA */
	int  sets[65][8];                       /* SETS TO DO */
};
/*
..  NFL common block:
..                                                                            */
extern struct nfl_com nfl_com_ ;
/*
..  Given statements:
..                                                                            */
/*-                                                                           */
