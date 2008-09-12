#!/bin/csh
##############################################################
#      .wsrt file for all users of the WSRT offline software #
##############################################################

      if (! $?FILBEC) echo "Setting the WSRT environment ....."
#
#  All 'alias' names in alfabetical order.
#      
      alias ampha '/users/srt/bin/ampha.exe'
      alias buslink '/users/srt/bin/buslink.exe'
      alias buslinkt '/users/srt/onp/buslink/test/buslinkt'
      alias cadis '/users/srt/bin/cadis.exe'
#      alias cadist '/users/srt/bin/cadist.exe'
      alias catap '/users/srt/bin/catap.exe'
      alias cfreq '/users/srt/bin/cfreq.exe'
      alias check '/users/srt/bin/check.exe'
      alias check_cat '/users/srt/bin/check_cat.exe'
      alias cemon '/users/srt/bin/cemon.exe'
      alias cocon '/users/srt/bin/cocon.exe'
      alias cotap '/users/srt/bin/cotap.exe'
      alias decom '/users/srt/bin/decom.exe'
      alias decomt '/users/srt/ofp/decom/test/decomt'
      alias delca '/users/srt/bin/delca.exe'
      alias delfi '/users/srt/bin/delfi.exe'
      alias delof '/users/srt/bin/delof.exe'
      alias dipol '/users/srt/bin/dipol.exe'
      alias dista '/users/srt/bin/dista.exe'
      alias distodat '/users/srt/bin/distodat.exe'      
      alias dumpt '/users/srt/bin/dumpt.exe'
      alias f77dir 'cat *.f | grep "*+"'
      alias femon '/users/srt/bin/femon.exe'
      alias findjm '/users/srt/bin/findjm.exe'
      alias firte '/users/srt/bin/firte.exe'
      alias fixpa '/users/srt/bin/fixpa.exe'
      alias fn1000 'kermit -l /dev/hp1000_a -b 9600 -f '
      alias gt1000 'kermit -l /dev/hp1000_a -b 9600 -g '
      alias hp1000 'kermit -l /dev/hp1000_a -b 9600 -c'
      alias holog '/users/srt/bin/holog.exe'
      alias info 'mosaic /users/srt/mosdoc/info.html'
      alias licom '/users/srt/bin/licom.exe'
      alias linkstat '/users/srt/bin/linkstat.exe'
      alias linktask '/users/srt/bin/linktask.exe'
      alias lists '/users/srt/bin/lists.exe'
      alias lpv '/users/srt/bin/lpv'
      alias makecal '/users/srt/bin/makecal.exe'
      alias marec '/users/srt/bin/marec.exe'
      alias model '/users/srt/bin/model.exe'
      alias mofil '/users/srt/bin/mofil.exe'
      alias mosaic '/disk3/Mosaic/Mosaic-2.0/src/Mosaic'
      alias mospa '/users/srt/bin/mospa.exe'
      alias offline '/users/srt/bin/offline'
      alias parasol '/users/srt/bin/parasol.exe'
      alias plotf '/users/srt/bin/plotf.exe'
      alias plotj '/users/srt/bin/plotj.exe'
      alias pluvo '/users/srt/bin/pluvo.exe'
      alias priad '/users/srt/bin/priad.exe'
      alias prtap '/users/srt/bin/prtap.exe'
      alias quality '/users/srt/bin/quality.exe'
      alias repoi '/users/srt/bin/repoi.exe'
      alias savex '/users/srt/bin/savex.exe'
      alias setup '/users/srt/bin/setup.exe'
      alias spect '/users/srt/bin/spect.exe'
      alias stfma '/users/srt/bin/stfma.exe'
      alias stfmat '/users/srt/bin/stfmat.exe'
      alias stmet '/users/srt/bin/stmet.exe'
      alias sumas '/users/srt/bin/sumas.exe'
      alias sutim '/users/srt/bin/sutim'
      alias tools '/users/srt/bin/tools.exe'
      alias tpdis '/users/srt/bin/tpdis.exe'
      alias tsyst '/users/srt/bin/tsyst.exe'
      alias uvdata '/users/srt/bin/uvdata.exe'
      alias vsyst '/users/srt/bin/vsyst.exe'
      alias zofri '/users/srt/bin/zofri.exe'
      alias nst 'cd /users/srt/nst'
      alias logA '/users/srt/bin/logA.exe'
      alias onp 'cd /users/srt/onp'
      alias ofp 'cd /users/srt/ofp'
      alias view '/users/srt/ofp/viewsched/vw'
      alias wp '/usr/wp/bin/wp'
      alias xfig 'xfig -e ps'
#
#  Environment variables for standard programs in alfabetical order.
#      
setenv      ADM /disk3/obs/adm
setenv      AMPHA /users/srt/bin/ampha.exe
setenv      CATAP /users/srt/bin/catap.exe
setenv      CADIS /users/srt/bin/cadis.exe
setenv      CEMON /users/srt/bin/cemon.exe
setenv      CFREQ /users/srt/bin/cfreq.exe
setenv      COCON /users/srt/bin/cocon.exe
setenv      COTAP /users/srt/bin/cotap.exe
setenv      DECOM /users/srt/bin/decom.exe
setenv      DELCA /users/srt/bin/delca.exe
setenv      DELFI /users/srt/bin/delfi.exe
setenv      DELOF /users/srt/bin/delof.exe
setenv      DISTA /users/srt/bin/dista.exe
setenv      DISTODAT /users/srt/bin/distodat.exe
setenv      DUMPT /users/srt/bin/dumpt.exe
setenv      FINDJM /users/srt/bin/findjm.exe
setenv      FIRTE /users/srt/bin/firte.exe
setenv      FIXPA /users/srt/bin/fixpa.exe
setenv      LICOM /users/srt/bin/licom.exe
setenv      LISTS /users/srt/bin/lists.exe
setenv      LPV /users/srt/bin/lpv
setenv      MAREC /users/srt/bin/marec.exe
setenv      MAKECAL /users/srt/bin/makecal.exe
setenv      MOLOG /users/srt/ofp/molog/molog
setenv      MOPLT /users/srt/ofp/moplt/moplt
setenv      MOSOL /users/srt/ofp/mosys/mosol
setenv      PARASOL /users/srt/bin/parasol.exe
setenv      PLOTF /users/srt/bin/plotf.exe
setenv      PLOTJ /users/srt/bin/plotj.exe
setenv      PLUVO /users/srt/bin/pluvo.exe
setenv      PRIAD /users/srt/bin/priad.exe
setenv      PRTAP /users/srt/bin/prtap.exe
setenv      REPOI /users/srt/bin/repoi.exe
setenv      LKTASK /users/srt/bin/linktask.exe
setenv      STFMA /users/srt/bin/stfma.exe
setenv      STFMAT /users/srt/bin/stfmat.exe
setenv      STMET /users/srt/bin/stmet.exe
setenv      SUMAS /users/srt/bin/sumas.exe
setenv      SUTIM /users/srt/bin/sutim
setenv      TOOLS /users/srt/bin/tools.exe
setenv      TPDIS /users/srt/bin/tpdis.exe
setenv      TSYST /users/srt/bin/tsyst.exe
setenv      UVDATA /users/srt/bin/uvdata.exe
setenv      VSYST /users/srt/bin/vsyst.exe
setenv      ZOFRI /users/srt/bin/zofri.exe
setenv      SPECT /users/srt/bin/spect.exe
setenv      QUALITY /users/srt/bin/quality.exe
#
#  Environment variables for standard data files in alfabetical order.
#
setenv      MON /disk3/mon
setenv      FILBEC $ADM/filbec.txt
setenv      FILCAL $ADM/filcal.txt
setenv      FILCLC $ADM/filclc.txt
setenv      FILCAT $ADM/catalog.d
setenv      FILE01 $ADM/obsad.d
setenv      FILE06 /disk3/obs/data/filuv01.d
setenv      FILE07 /disk3/obs/data/filuv02.d
setenv      FILE08 /disk3/obs/data/filuv03.d
setenv      FILE15 /disk3/obs/data/filuv04.d
setenv      FILE16 /disk3/obs/data/filuv05.d
setenv      FILEX $ADM/srtpar.d
setenv      FILHPD $ADM/filhpd.d
setenv      FILLSI $MON/lsidata.d
setenv      FILMET $ADM/filmet.d
setenv      FILEWN $ADM/filewn.d
setenv      FILNAM $ADM/filnam.d
setenv      FILPRP $ADM/filprp.d
setenv      FILREQ $ADM/filreq.d
setenv      FILVLB $ADM/filvlb.d
setenv      FLCATK /users/srt/ofp/catap/FLCATK
setenv      FLCATL /users/srt/ofp/catap/FLCATL
setenv      FLCEMO $MON/flcemo.d
setenv      FLDIST /users/srt/ofp/dista/FLDIST
setenv      FLMOCA /disk3/obs/vlbi/flmoca.d
setenv      FLMOLO /disk3/obs/vlbi/flmolo.d
setenv      FLPOIN /disk3/obs/pointing/flpoin.d
setenv      FLPOSI $MON/flposi.d
setenv      flsuma /users/srt/flsuma
setenv      FLTEST $MON/fltest.d
setenv      FUNTAB $MON/FUNTAB
setenv      LNKCTL /users/srt/onp/buslink.d
setenv      SCAN01 /disk3/obs/data/scan01.d
setenv      SCAN02 /disk3/obs/data/scan02.d
setenv      SCTP01 /disk3/obs/data/sctp01.d
setenv      LOG /disk3/obs/log
setenv      SRT /users/srt
setenv      XTP /users/srt/xtp
setenv      XTPO /users/srt/xtp/offline
setenv      STATCS /disk3/obs/data/statcs.d
setenv      WNG /users/srt/nst/wng
setenv      WSTASK /users/srt/onp/wstasks.d
setenv      VLBI /disk3/obs/vlbi
#
#     Additional SYSTEM environment variables.
#
      setenv GS_LIB /disk3/csl/ghostscript
#      setenv LPATH /lib:/usr/lib:/users/srt/lib
#
# End of .wsrt



