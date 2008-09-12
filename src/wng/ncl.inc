/*+ Created from ncl.dsc on 970828 at 16:52:59 at daw18
..  NCL.INC
..  WNB 970828
..
..  Revisions:
..
..      HJV 950512      Add DATAFAC
..      WNB 930803      Remove .INCLUDE
..      WNB 921216      Add GRFAC
..      WNB 910809      Original version
..                                                                            */
/*
..  Given statements:
..                                                                            */
/*
..  Result:
..
..  NCL.DEF is an INCLUDE file for the NCLEAN program
..  
..                                                                            */
/*
..  Parameters:
..                                                                            */
#define MXNSET 16                               /* MAX. # OF MAP SETS */
#define MXNAR 32                                /* MAX. # OF AREAS */
#define MNBPAT 3                                /* MIN. BEAM PATCH SIZE */
#define MXBPAT 128                              /* MAX. BEAM PATCH SIZE */
/*
..  Data declarations:
..                                                                            */
/*
..  NCL common data:
..                                                                            */
struct ncl_com {
  union {
	char option[24];                        /* PROGRAM OPTION */
	char opt[3];
  } option;
	int memsiz;                             /* SIZE OF DYNAMIC MEMORY TO USE */
	char nodmap[80];                        /* MAP NODE */
	char filmap[160];                       /* MAP FILE */
	int fcamap;                             /* MAP FCA */
	int msets[17][8];                       /* MAP SETS */
	char nodap[80];                         /* AP NODE */
	char filap[160];                        /* AP FILE */
	int fcaap;                              /* AP FCA */
	int asets[17][8];                       /* AP SETS */
	unsigned int apdcv;                     /* APPLY DECONVOLUTION */
	int cmplog[2];                          /* LOG CODE COMPON_LOG: */
	float cllim;                            /* CLEAN LIMIT */
	float clfac;                            /* CLEAN LOOP FACTOR */
	int srclim;                             /* # OF SOURCES LIMIT */
	int tarea[2][4];                        /* TOTAL AREA */
	int parea[2][32][4];                    /* PARTIAL AREAS */
	int narea;                              /* NUMBER OF AREAS */
	float prhat;                            /* PRUSSIAN HAT VALUE */
	unsigned int resmdl;                    /* OUTPUT RESIDUAL MODEL SWITCH */
	unsigned int rstmdl;                    /* RESTORED OUTPUT SWITCH */
	unsigned int ronmdl;                    /* ONLY RESTORE SWITCH */
	float mpdep;                            /* CYCLE DEPTH */
	float grfac;                            /* GRATING FACTOR */
	float datafac;                          /* DATACLEAN FACTOR */
	float curmax;                           /* CURRENT MAP MAX. */
	int curmxp[2];                          /* POS. CURRENT MAX. */
	int mapnam[8];                          /* CURRENT MAP NAME */
	int apnam[8];                           /* CURRENT AP NAME */
	int bempat;                             /* SIZE BEAM PATCH */
	int mappat;                             /* # OF POINTS IN MAP PATCH */
	float maplim;                           /* MAP DATA LIMIT IN PATCH */
	float clbxlm;                           /* MAX. CORRECTION OUTSIDE PATCH */
	int curpmx;                             /* CURRENT MAX. POINTER */
	float minlim;                           /* MAP INPUT MAXIMUM */
	int cvbfu;                              /* U CONVOLUTION FUNCTION ptr */
	int cvbfv;                              /* V CONVOLUTION FUNCTION ptr */
	float resdl;                            /* RESTORE BEAM L */
	float resdm;                            /* RESTORE BEAM M */
	float resdan;                           /* RESTORE BEAM SKEW ANGLE */
	int mphad;                              /* MAP HISTOGRAM AREA ptr */
	int bmhad;                              /* BEAM HISTOGRAM AREA ptr */
	float mphmxi;                           /* MAX. IN MAP HISTOGRAM */
};
/*
..  NCL common block:
..                                                                            */
extern struct ncl_com ncl_com_ ;
/*
..  Given statements:
..                                                                            */
/*-                                                                           */
