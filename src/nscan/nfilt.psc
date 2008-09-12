!+ NFILT.PSC
!  WNB 940216
!
!  Revisions:
!	HjV 941031	Add MDLNODE_PEF
!	WNB 950704	Add frequency and polynomial selection
!	JPH 960404	Formatting
!
!  Get overall action
!	Ref:	NFIDAT
!
KEYWORD=OPTION
	DATA_TYP=C
	IO=I
	LENGTH=24
	CHECKS=ABBREV_OPTIONS
	SWITCHES=LOOP,NULL_VALUES,WILD_CARDS
	SEARCH=L,P
	OPTIONS=CONTINUUM, QUIT
	PROMPT="Action"
	HELP="
Specify the action to be performed by the program NFILT:
.
  CONTINUUM	make a UV-based estimate of the continuum in line data. The 
		 algoritm used fits a polynomial to the sine and cosine
		 components of the residual corrected data, and stores it as
 		 additive interferometer data. Second order effects could 
		 necessitate an iteration for the cross-polarised channels to 
	 	 properly cater for polarisation correctioons and Faraday 
		 rotation.
		Although the program will handle any combination of input sets,
		 fastest operation is attained if loops are used to select the 
		 fields.
.
  QUIT:	   	leave the program NSCAN
"
!
!  Get polynomial degree
!	Ref:	NFIDAT
!
KEYWORD=POLY_DEGREE
	DATA_TYP=J
	IO=I
	SWITCHES=LOOP,WILD_CARDS,NULL_VALUES
	CHECKS=MAXIMUM,MINIMUM
	MAXIMUM=6
	MINIMUM=0
	SEARCH=L,P
	PROMPT="Polynomial degree"
	HELP="
Specify the degree of the polynomial to solve for: 0=constant; 1=slope
etc.
"
!
!  Get frequency selection
!	Ref:	NFIDAT
!
KEYWORD=FREQ_SELECT
	DATA_TYP=R
	IO=I
	NVALUES=16
	SWITCH=LOOP,WILD_CARDS,NULL_VALUES
	SEARCH=L,P
	PROMPT="Pairs of frequency (MHz) to select solution domain"
	HELP="
  	The actual solution of the continuum radiation will be based on a
subset of the input channels. The channels selected will be based on
pairs of frequencies, indicating a range.
E.g. 325,327,330,332 will use channels with frequnecies in the ranges
325-327 and 330-332 MHz. 
.
  	* indicates all channels.
.
Note that the corrections are saved for all selected input frequency sets.
"
!-
INCLUDE=NGEN_PEF
!-
INCLUDE=SCNNODE_PEF
INCLUDE=SCNSETS_PEF
!-
INCLUDE=SELECT_PEF
!-
INCLUDE=MDLNODE_PEF
!-
INCLUDE=NMODEL_PEF
!-
