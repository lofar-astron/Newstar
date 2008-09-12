!+ NATNF.PSC
!  WNB 920428
!
!  Revisions:
!       WNB 921022      Magtape text
!       WNB 921028      Add SOURCES, BANDS, CHANNELS, INTEGRATION
!       WNB 921123      Add START_OFFSET. CONTINUUM
!       WNB 921211      Make PSC
!       JEN 930312      INCLUDE=NCOMM_PEF
!       JEN 930312      Remove keywords INPUT_UNIT, OUTPUT_SCAN
!       JPH 960403      Format corrections - UNIT_PEF
!
!
!  Get overall action
!       Ref:    NATDAT
!
KEYWORD=OPTION 
        DATA_TYP=C 
        IO=I 
        LENGTH=24 
        CHECKS=ABBREV_OPTIONS 
        SWITCHES=LOOP,NULL_VALUES,WILD_CARDS 
        SEARCH=L,P 
        OPTIONS=LOAD,QUIT 
!       PROMPT="action"
        HELP="
Specify action to perform: 
.
  LOAD    load RPFITS data into scan file 
  QUIT    finish" 
!
!
!  Get input file
!       Ref:    NATDAT
!
KEYWORD=INPUT_FILE 
        DATA_TYP=C 
        IO=I 
        LENGTH=80 
        SWITCHES=LOOP,NULL_VALUES,WILD_CARDS 
        SEARCH=L,P 
        PROMPT="input filename" 
        HELP="
Specify the input filename (without an extension for the LOAD from disk
option). " 
!
!  Get input labels
!       Ref:    NATDAT
!
KEYWORD=INPUT_LABELS 
        DATA_TYP=J 
        IO=I 
        NVALUES=256 
        SWITCHES=LOOP,NULL_VALUES,WILD_CARDS 
        SEARCH=L,P 
        PROMPT="input labels" 
        HELP="
Specify the tape labels to be read. * specifies all labels on the tape " 
!
!  Get input sources
!       Ref:    NATDAT
!
KEYWORD=INPUT_SOURCES 
        DATA_TYP=C 
        IO=I 
        LENGTH=16 
        NVALUES=256 
        SWITCHES=LOOP,NULL_VALUES,WILD_CARDS 
        SEARCH=L,P 
        PROMPT="sources to get" 
        HELP="
Specify the sources to be read. * specifies all sources on the tape" 
!
!  Get input bands
!       Ref:    NATDAT
!
KEYWORD=INPUT_BANDS 
        DATA_TYP=R 
        IO=I 
        NVALUES=16 
        SWITCHES=LOOP,NULL_VALUES,WILD_CARDS 
        SEARCH=L,P 
        PROMPT="input bands in cm" 
        HELP="
Specify the bands to be read (20 etc in octave steps. * specifies all bands on
the tape" 
!
!  Get input channels
!       Ref:    NATDAT
!
KEYWORD=INPUT_CHANNELS 
        DATA_TYP=J 
        IO=I 
        NVALUES=256 
        SWITCHES=LOOP,NULL_VALUES,WILD_CARDS 
        CHECK=MINIMUM 
        MINIMUM=1 
        SEARCH=L,P 
        PROMPT="input channels" 
        HELP="
Specify the channels (1,..) to be read. * specifies all channels on the tape " 
!
!  Get integration time
!       Ref:    NATDAT
!
KEYWORD=INTEGRATION_TIME 
        DATA_TYP=R 
        IO=I 
        SWITCHES=LOOP,NULL_VALUES,WILD_CARDS 
        CHECK=MAXIMUM,MINIMUM 
        MAXIMUM=3600. 
        MINIMUM=10. 
        SEARCH=L,P 
        PROMPT="integration time (sec)" 
        HELP="
Specify the integration time per scan. " 
!
!  Get time offset
!       Ref:    NATDAT
!
KEYWORD=START_OFFSET 
        DATA_TYP=R 
        IO=I 
        SWITCHES=LOOP,NULL_VALUES,WILD_CARDS 
        CHECK=MAXIMUM,MINIMUM 
        MAXIMUM=180. 
        MINIMUM=0. 
        SEARCH=L,P 
        PROMPT="scan start offset (sec)" 
        HELP="
Often the first 10 sec of a scan are bad. By specifying an offset here, the
first n seconds of a scan will be discarded. E.g. specifying an integration of
70 sec and an offset of 10 sec will produce 4 points for a 5 min observation,
discarding the first and last 10 sec. " 
!
!  Continuum definition
!       Ref:    NATDAT
!
KEYWORD=CONTINUUM 
        DATA_TYP=J 
        IO=I 
        NVALUES=16 
        SWITCHES=LOOP,NULL_VALUES,WILD_CARDS 
        SEARCH=L,P 
        PROMPT="define channels for continuum" 
        HELP="
A continuum channel can be made by specifying up to 8 pairs of channel numbers.
The channel values will be averaged to produce a single continuum value. Each
pair specifies a low and high channel number to be included. 
.
Special values: 
  ""      No continuum wanted 
  *       Central 75% of channels used" 
!
!-
INCLUDE=NGEN_PEF 
!-
INCLUDE=SCNNODE_PEF     !
INCLUDE=SCNSETS_PEF     !
INCLUDE=SELECT_PEF      !
INCLUDE=UNIT_PEF 
!-
