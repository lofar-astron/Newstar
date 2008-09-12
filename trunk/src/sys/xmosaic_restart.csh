#! /bin/csh -f
#
# xmosaic_restart.csh - (re)start www browser
# arguments:
#	$1	program name (upper or lower case)
#	$2	keyword (must be upper case)
#
# Revisions:
#	HjV 950227	Typo in find target file
#	JPH 951006	Change string for Parameter search. - Sleep 3 --> 1
#	JPH 951121	Replace automatic sync by user verification
#			Add support for netscape, new env. symbol n_www	
#	HjV 960102	Change test for netscape in *netscape*
#	JPH 970305	Remove redundant n_www definition. 
#			Check against recycled pid (which caused the procedure
#			 to hang in its attempt to remotely activate netscape)
#	JPH 970819	Extend check on result of `ps $pid` test for a running 
#			 $n_www (ps does not necessarily return an error status)
#	JPH 980127	Set noglob for processing xm_$DISPLAY file (Solaris)
#	JPH 990111	Bug fix: Preset variable y
#			Display error messages from browser
#	JPH 991117	ps --> ps -p for Solaris

##set echo
	if (! $?n_www) setenv n_www netscape
	set target = ()	##$n_hlp/homepage.html

 	if ($#argv) then
	  set Argv = ( $argv )
	  set argv = (`echo $argv | sed -e 'y:ABCDEFGHIJKLMNOPQRSTUVWXYZ_:abcdefghijklmnopqrstuvwxyz.:' `)
#
# Find the target file
#
	  set target = `grep -l 'Parameter '"$Argv[2]" $n_hlp/${argv[1]}_private_intfc/${argv[1]}_private_intfc.html`
	  if ($#target != 0) then
	    set typ = 'private'
	  else
	    set target = `grep -l 'Parameter '"$Argv[2]" $n_hlp/*_public_intfc/*_public_intfc.html`
	    if ($#target != 0) then
	      set typ = 'public'
	    else
	      echo "No hypertext entry for keyword $Argv[2]"
	      exit
	    endif 
	  endif
	  set target = "${target}#.$argv[2]"
	endif
##echo $target
#
# Check for a running $n_www browser started by us. The pid of the last
#  instantiation from the current terminal is recorded in /tmp/xm-$DISPLAY.
#  If the process has terminated since, the pid is now probably not in use.
#  The rare cases that it is are intercepted by checking the process name.
#
 	set pid = ()
 	while ($#pid == 0)
	  if (-e /tmp/xm-$DISPLAY) then
	    set pid = \
	      `sed < /tmp/xm-$DISPLAY -e 's:.*+ *::' -e 's: .*$::'` 
	    set noglob
	    set x = `cat /tmp/xm-$DISPLAY`
 	    if ("$x" !~ *${n_www}* ) set pid = ()
	    unset noglob
	    set y = ""
	    set p = ''; if ($n_arch == so) set pp = '-p'
	    if ($#pid) set y = `ps $pp $pid`
 	    if ($status || "$y" !~ *${n_www}*) set pid = ()
	    if ("$y" !~ *${n_www}* ) set pid = ()
 	  endif
#
# If none found, start one up with Newstar homepage 
# 
start:
	  if ($#pid == 0) then
 	    echo "	Starting $n_www"
	    if ("$n_www" =~ *netscape*) then
	      $n_www -ncol 48 $n_hlp/homepage.html > /dev/null &
	    else
	      $n_www $n_hlp/homepage.html > /dev/null &
	    endif	  
	    if ($#target) then
	      echo -n \
	"	Hit any key when $n_www is displaying Newstar home page: "
	      set y = $<
	    endif
	    echo -n "" >! /tmp/xm-$DISPLAY
	    while (-z /tmp/xm-$DISPLAY)
	      sleep 1
	      jobs -l >> /tmp/xm-$DISPLAY	
 	    end
	  endif
	  if (! $#target) then
	    echo "	$n_www is running"
	    ps $pp $pid
	    exit
	  endif
	end
	
	if ("$n_www" =~ *mosaic*) then
#
# Create command file
#
	  rm >&/dev/null /tmp/*.$pid
	  set tmp = /tmp/Mosaic.$pid
	  echo 'goto' >! $tmp
	  echo "file:://localhost$target" >> $tmp
	  ln -s $tmp /tmp/xmosaic.$pid
#
# Send a SIGUSR1 signal and wait 3 sec for the command file to be accessed.
# Keep trying until it is accepted
#
	  while (1)
	    $n_exe/signal_and_sync.exe $tmp 3 $pid
	    set sts = $status
echo "status from sinal_and_sync.exe:	$sts"
	    if ($sts < 0) then
	      echo "	failed to send signal to $n_www process $pid"
	      exit $sts
	    endif
	    if (! $sts) exit
	  end
	
	else if ("$n_www" =~ *netscape*) then
	  @ sts = 1
	  while ($sts)
	    $n_www -remote "openURL file:$target" >&/dev/null
	    @ sts = $status
echo "status from remote netscape access:	$sts"
	    if ($sts) then
	      echo "	Something wrong, trying a new start"
	      goto start
	      $n_src/sys/document.csh hyper >&/dev/null	  
##	      echo -n \
##	"	Hit any key when $n_www is displaying Newstar home page: "
##	      set y = $<
	    endif
	  end
	else
	  echo "	ERROR:	Unknown WWW browser $n_www"
	endif
