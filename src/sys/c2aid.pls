#+ C2AID.PLS
#  WNB 940310
#
#  Revisions:
#	WNB 940531	Isolate eq, ne
#	WNB 940621	Some VMS adjustments
#	WNB 940624	Delete some .tmp
#	WNB 940627	Update wc for VMS
#	WNB 940630	chmod for VMS typo
#
# Import the newstar environment to local Perl variables with same name,
#	set a local cwd, and set VMS true if necessary 
#		&ENV_IMPORT()
# Export name:
#		&ENV_EXPORT(name,value)
# Various:
#		&fnp(expr)			Parse filename, no globbing
#		&fn(expr)			Parse filename, possible glob.
#		&fl(expr)			Expand file names
#		&fp(type,file)			Get file:type
#		&ft(type,file)			Do -type test on file
#
#		&vn(string)			$# in variable
#
#		&Pipe(file,...)			Returns contents of last file
#		&doalias(name,key,...)		Execute an alias
#		&dollar(name,in,out)		$name command
#
#		&stdop				select STDIN, STDOUT
#		&stdcl				de-select STDIN STDOUT
#		&stdmk				make STDIN STDOUT
#		&stdcmk				unmake STDIN STDOUT
#
#		&system(string)			do system routine
#
#		&alias(name,value)		alias
#		&ar(type,in,out)
#		&awk(type,program,in,out)	awk creator
#		&cat(type,file)			cat
#		&cd(dir)			Change to dir, set PWD and cwd 
#		&chmod(type,file)		chmod
#		&cmp(in)			cmp files
#		&compress(in,out)
#		&cp(type,infile,outfile)	Copy infile to out
#		&date				Return as Unix date
#		&diff(type,in1,in2,out)		diff
#               &docom(out,in1,...)             do VMS commands
#		&docsh(name,in,out)		do csh
#		&doexe(name,in,out)		do program
#		&domainname
#		&echo(type,text)		Text to screen with nl
#		&elm(type,in,in)
#		&exit
#		&find(type,out)
#		&ftp(type,in,out)		ftp
#		&grep(type,pat,in,out)		grep
#		&ln(type,in)
#		&log(text)			Text to screen and Log
#		&ls(type,in,out)
#		&mem(in)
#		&more(in,out)			more
#		&mv(type,infile,outfile)	Move infile to out
#		&nm(in,out)
#		&peq(left,right)		=~
#		&eq(left,right)			eq (char ==)
#		&pr(type,in,out)
#		&ranlib(in)
#		&rm(type,file)			Remove file (type i or f)
#		&rsh(host,com,in,out)
#		&sed(prog,in,out)
#		&set(out)			set show option
#		&sort(type,in,out)		sort
#		&source(file)
#		&strip(in)
#		&stty(type)
#		&tail(+/-n,in)			tail/head
#		&tar(type,in,out)
#		&tee(type,file)			tee
#		&touch(type,file)		Create file if non-existant
#		&tr(type,pat-in,pat-out,in,out)	tr function
#		&unalias(name)			unalias
#		&uncompress(in,out)
#		&wc(type,in)			wc: type l or c
#		&what(type,in,out)
#		&whoami				Get name
#
# General aids:
# Obtain the newstar environment from Logicals in VMS and set $VMS to indicate
#	VMS environment; and obtain cwd in $n_cwd
#		&VMS_IMPORT()
#		
#		
#-

sub ENV_IMPORT {

  local(@aa);
  local(@list)=('ARR','ARD','AS','CC','FC','NSTAR_DIR','RS'); # special codes
#
# Determine if VMS
#
  &VMS_IMPORT;
#
# Find all n_ ENVironment
#
  @aa=grep(/^n_/i,keys(%ENV));		# get all n_ environment
  for (@aa) { eval ("\$$_='$ENV{$_}'");} # import environment variable
  $n_arch=~tr/A-Z/a-z/;			# make sure lc
#
# Some specials
#
  for (@list) {
    if ($ENV{$_}) {eval ("\$$_=\$ENV{'$_'}");}
  }
#
# Set current directory etc
#
  $HOST=$ENV{'HOST'}; $USER=$ENV{'USER'};
  $cwd=$ENV{'PWD'};
}

sub ENV_EXPORT { local($name,$val)=@_;
#
# Set ENVironment
#
  $ENV{$name}="$val";			# export environment variable
  &system("define/nolog/job $name \"$ENV{$name}\"") if $VMS; # make logical
}

sub fnp { local($name)=@_;

  local(@res);

  if ($VMS) {
    for (split(' ',$name)) { s/\?/%/g;
      if (/\//) {
        s!^/([\w\*\%]+)/?!$1\:!;                # unit
        s!\]/!\]!g;                             # isolated ]/
        s!([\w\*\%]+)(\.DIR)?/!\[\.$1\]!g;      # make [] from /
        s/\.?\]\[\.?/\./g;                      # remove ][
        while (s/(\.[\w\*\%]+)\.([\w\*\%]+)$/$1_$2/) {} # make .a.b into .a_b
        s/\.\]/\]/g; s/\[\./\[/g;}              # no .] or [.
      push(@res,$_);}
    join(' ',@res);}
  else { $name;}
}

sub fn { local($name)=@_;

  local(@res);

  for (split(' ',$name)) {
    if (!defined($noglob) && /[\*\?]/) { push(@res,&fl($_));}
    else {push(@res,$_) if $_;}
  }
  join(' ',@res);
}

sub fl { local($name)=@_;

  local(@aa,*TMP,@l1);

  unlink ("fla$$.tmp") while (-e "fla$$.tmp");
  if ($VMS) {
    open(TMP,">flb$$.tmp"); close(TMP);		# to suppress message
    $name=~s/\?/\%/g;
    $name=&fnp($name);
    &system("directory/nohead/notrail/version=1/col=1/out=fla$$.tmp ".
			"$name,${cwd}flb$$.tmp");
    unlink("flb$$.tmp");}
  else {
    `echo $name > fla$$.tmp`;}
  open (TMP,"fla$$.tmp");
  while (<TMP>) {
    chop; @l1=split(' ',$_);
    for (@l1) {
      if (!/[\%\*\?]/) {if (-e $_) { s/;\d*$//; push(@aa,$_);}}
    }
  }
  close(TMP); unlink "fla$$.tmp" while (-e "fla$$.tmp");
  @aa;
}

sub fp { local($type,$name)=@_;

  local($i,$j);

  if ($VMS) {
    $name=&fnp($name);
    $i=rindex($name,']'); $j=rindex($name,':'); $i=($i>=$j)?$i:$j;
    if ($type eq 't') {
      if ($i == length($name)-1) {$name=~s/\.(\w+)\]$/\]$1/;
        $i=rindex($name,']'); $j=rindex($name,':'); $i=($i>=$j)?$i:$j;}
    }
    elsif ($type eq 'h') {$i++ if $i>=$[;
      if ($i == length($name)) {$name=~s/\.(\w+)\]$/\]$1/;
        $i=rindex($name,']'); $j=rindex($name,':'); $i=($i>=$j)?$i+1:$j+1;}
    }
  }
  else { $i=rindex($name,'/');}
  if ($type eq 't') { ($i>=$[) ? substr($name,$i+1) : $name;}
  elsif ($type eq 'h') { ($i>=$[) ? substr($name,$[,$i) : $name;}
  elsif ($type eq 'e') {
    $j=rindex($name,'.'); ($j>$i) ? substr($name,$j+1) : "";}
  elsif ($type eq 'r') {
    $j=rindex($name,'.'); ($j>$i) ? substr($name,$[,$j) : $name;}
  else {print "Unknown file part type -$type\n"; exit;}
}

sub ft { local($type,$name)=@_;

  if ($VMS) {
    ($name=&fnp($name))=~s/\.\]$/\]/;
    $name=~s/([\[\.])(\w+)\]$/$1$2\.\-\]$2\.DIR/;}
  $type='NO' unless $name;
  if ($type eq 'd') {
    if ($VMS && $name!~/\.DIR$/) { $name.='.DIR';}
    -d $name;}
  elsif ($type eq 'e') {-e $name;}
  elsif ($type eq 'o') {
    if ($VMS) {-w $name;} else {-o $name;}
  }
  elsif ($type eq 'z') {-z $name;}
  elsif ($type eq 'x') {-x $name;}
  elsif ($type eq 'M') {-M $name;}
  elsif ($type eq 'NO') {0;}
  else {print "Unknown file test type -$type\n"; exit;}
}

sub vn { local($str)=@_;

  scalar(split(' ',$str));}

sub Pipe { local(@file)=@_;

  local(*TMP,$t,$f);

  $f=$file[0];
  if ($f && open(TMP,$f)) {
    while (<TMP>) {chop;
      if ($_) {
        if ($t) { $t=join(' ',$t,$_);}
        else { $t=$_;}
      }
    }
    close (TMP); unlink ($f) while -e $f;}
  "$t";
}

sub doalias { local($name,@D_aterm)=@_;

  local(@D_term);

  unless ($C2_alias{$name}) {
    print "Fatal: unknown command(alias) $name\n"; exit;}
  eval($C2_alias{$name});
}

sub doalias_x { local(@val)=@_;

  for (@val) {
    if (/^\!\*$/) { push(@D_term,@D_aterm);}
    elsif (/\!\:(\d+)/) { push(@D_term,$`.$D_aterm[$1-1].$');}
    else { push(@D_term,$_);}
  }
}

sub D_input { local($n)=@_;

  local(@res,$t);

  if ($n eq 'out') {
    if ( @D_term && $D_term[0]=~/^>(.*)$/) { shift(@D_term);
    push(@res,$1.shift(@D_term));}
  }
  elsif ($n eq 'in') {
    while( @D_term && $D_term[0]!~/^>/) {
      if ($D_term[0] eq '<<') { shift(@D_term); shift(@D_term);
        push(@res,$t,"'"."&C2_t${C2_tcnt}_".&fp('r',&fp('t',$C2_in))."'");}
      elsif ($D_term[0] eq '<') { shift(@D_term);
        push(@res,$t,"'".shift(@D_term)."'");}
      else { push(@res,$t,"'".shift(@D_term)."'");}
      $t=".' '.";}
  }
  elsif ($n eq 'int') {
    if( @D_term && $D_term[0]!~/^[<>]/) {
      push(@res,"'".shift(@D_term)."'");}
  }
  elsif ($n eq 'inw') {
   while( @D_term && $D_term[0]!~/^[<>]/) {
      push(@res,$t,"'".shift(@D_term)."'"); $t=".' '.";}
  }
  elsif ($n eq 'inf') {
   while( @D_term && $D_term[0]!~/^[<>]/) {
      push(@res,$t,"'".shift(@D_term)."'"); $t=".' '.";}
  }
  elsif ($n eq 'sw') {
    while (@D_term && $D_term[0]=~/^[\-\+][^\-]/) {
      push(@res,$t,"'".shift(@D_term)."'"); $t=".' '.";}
  }
  if ($#res>0) {eval("@res");}
  elsif (@res) {"@res";}
  else {@res="''";}
}

sub dollar { local($name,$in,$out)=@_;

  local(@in);

  unless (eval("defined \$$name")) {
    print "Fatal: Unknown command variable \$$name\n"; exit;}
  $name=eval("\$$name");
  unless ($VMS) {
    if ($out) {$out=">$out";}
    if ($name eq 'rsh' || $name eq 'remsh') { local(@in)=split(' ',$in);
      local($f)=shift(@in);
      $status=system("$name $f '@in' $out")/256;}
    else {
      $status=&system("$name $in $out");}
  }
  elsif ( $name=~/fortran/i) {
    @in=split(' ',$in); $in='';
    for ($i=0; $i<=$#in; $i++) {
      if ($in[$i]=~/\-o/) { $out=('/OBJECT='.&fnp($in[$i+1])); $i++;}
      elsif ($in[$i]=~/^\//) { $out.=$in[$i];}
      else { $in.=(' '.&fnp($in[$i]));
        $out.=('/LIST='.&fp('t',&fp('r',$in[$i])).'.l');}
    }
    if (-e 'N_SRC:[SYS]N_LINKS.COM') {
      $status=&docom('','@N_SRC:[SYS]N_LINKS.COM',"$name$out $in");}
    else { $status=&system("$name$out $in");}
  }
  elsif ( $name=~/^ar/i) {
    $status=&ar((split(' ',$name))[1],$in,$out);}
}

sub stdop {

  if ($in) { open(S_IN,&fnp($in)); $S_in='S_IN';}
  else { $S_in='STDIN';}
  if ($out) { $out=~s/^([>]?[\&]?)([\!]?)/>\1/; $S_type="$1$2";
    open(S_OUT,&fnp($out)); $S_old=select(S_OUT);}
  else { $S_type='';}
}

sub stdcl {

  if ($in) { close(S_IN);}
  if ($out) { close(S_OUT); select($S_old); $out=$S_type.$out;}
}

sub stdmk { local($l1)=@_;

  $STDMK_OUT=''; $STDMK_TMP='';
  if ($in) { $in='<'.&fnp($in);}
  if ($out) { $out=&fnp($out);
    if ($VMS && $l1 != 1) {
      if ($out=~s/>//) { $STDMK_OUT=$out;
        $out=$STDMK_TMP="st$$.tmp";}
      $out='/OUT='.$out;}
    else { $out='>'.$out;}
  }
}

sub stdcmk { local(*TMPI,*TMPO);

  if ($VMS && $STDMK_OUT) {
    open (TMPI,$STDMK_TMP); open (TMPO,">>$STDMK_OUT");
    while (<TMPI>) { print TMPO $_;}
    close(TMPI); close(TMPO);}
}

sub system { local($str)=@_;

  unless ($VMS) { system("(cd $cwd; $str )")/256;}
  else { system("$str");}
}

sub alias { local($name,$val,$out)=@_;

  local($in);

  unless ($val) { &stdop;
    if ($C2_alias{$name}) { print $C2_alias{$name}."\n";}
    else { print "\n";}
    &stdcl;}
  else { $C2_alias{$name}=$val;}
}

sub ar { local($type,$f,$out)=@_;

  local($in,@in,$lib,$t,$t1);

  &stdmk;
  unless ($VMS) { $status=&system("ar $f $out");}
  else { @in=split(' ',&fnp($f)); $lib=shift(@in);
    if ($type=~/v/) { $t.='/LOG';}
    if (&fp('e',$lib)=~/tlb/i) {$t1='/TEXT';}
    if ($type=~/r/) {
      unless (-e $lib) { &system("LIB/CREATE$t1 $lib");}
      $t="LIB/REPLACE$t$t1 $lib ".join(',',@in);}
    elsif ($type=~/d/) {
      for ($i=0; $i<=$#in; $i++) { $in[$i]=&fp('t',&fp('r',$in[$i]));}
      $t="LIB$t1$t/DELETE=(".join(',',@in).") $lib";}
    else {
      $t="LIB/LIST/FULL$t1 $lib";}
    if ($out) { $status=&docom($out,$t);}
    else { $status=&system($t);}
  }
}

sub awk { local($type,$prog,$in,$out)=@_;

  local($l1,*TMP);

  if (&ft('e',$in)) {
    &stdmk(1);
    if ($VMS) {
      if ($type=~/F(.)/) { $l1="\"-F$1\"";}
      if (length($prog)>100) { open(TMP,">g$$.tmp");
	print TMP $prog."\n"; close(TMP);
	$status=&system("gawk $l1 -f g$$.tmp -- $in $out");
	unlink ("g$$.tmp") while (-e "g$$.tmp");}
      else { $prog=~s/"/""/g;
        $status=&system("gawk $l1 -- \"$prog\" $in $out");}
      }
    else { $status=&system("awk $type '$prog' $in $out");}
  }
  else { &echo('','',$out);}
}

sub cat { local($type,$in,$out)=@_;

  &stdop;
  while(<$S_in>) {print $_;}
  &stdcl;}

sub cd { ($cwd)=@_;

  $cwd=&fnp($cwd);chdir($cwd);$ENV{'PWD'}=$cwd;$PWD=$cwd;}

sub chdir { &cd(@_);}

sub chmod { local($type,$file)=@_;

  local(*l1,$l2,$l3);

  $file=&fnp($file);
  if ($type=~/^[0-7]/) {$l1="0$type";}
  else {
    if ($type=~/([\+\-\=])/) {$l3="\\$1";}
    @l1=split(/$l3/,$type);
    $l1=0;$l2=0;
    if ($l1[0]=~/u/) {$l1=$l1|0100;}
    if ($l1[0]=~/g/) {$l1=$l1|010;}
    if ($l1[0]=~/o/) {$l1=$l1|01;}
    if ($l1[0]=~/a/) {$l1=$l1|0111;}
    if ($l1[1]=~/r/) {$l2=$l2|04;}
    if ($l1[1]=~/w/) {$l2=$l2|02;}
    if ($l1[1]=~/x/) {$l2=$l2|01;}
    $l1=$l1*$l2;
    unless ($l3 eq '=') {
      $l2=(stat($file))[2];
      if ($l3 eq '-') {$l1=$l2&(32767-$l1);}
      else {$l1=$l2|$l1;}
    }
  }
  chmod($l1,$file);
}

sub cmp { local($f,$out)=@_;

  local($in);
  local(*TMP,*TMPI,$l1,$t,$c);
  local($in1,$in2)=split(' ',$f);

  open(TMP,$in1); open(TMPI,$in2);
  &stdop;
  while (<TMP>) {$l1=$_; $c++;
    if (<TMPI>) { if ($_ ne $l1) {$t=1;$c++;last;}}
  }
  if (<TMPI>) {$t=1;}
  close (TMP); close(TMPI);
  if ($t) { print "$in1 $in2 differ: line $c";}
  &stdcl;}

sub compress { local($f,$out)=@_;

  local($in);

  &stdmk;
  unless ($VMS) { $status=&system("compress $in $out");}
}

sub cp { local($type,$file)=@_;

  local(@in,$out);

  @in=split(' ',$file); $out=&fnp(pop(@in));
  if ($VMS && $out=~/\w+$/ && $out!~/\.\w+$/) {
    &ln('-s',"$out. $out"); $out.='.';}
  for (@in) { $_=&fnp($_);
    if ($_ && $_ ne $out) {
      unless ($VMS) { $status=&system("cp $type $_ $out");}
      else { $status=&system ("copy $_ $out");}
    }
  }
}

sub date { local($out)=@_;

  local(@t)=localtime;
  local($in,$t);

  &stdop;
  $t=sprintf("%3s %3s %02d %02d:%02d:%02d LST %4d ",
		(Sun,Mon,Tue,Wed,Thu,Fri,Sat)[$t[6]],
		(Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec)[$t[4]],
		$t[3],
		@t[2,1,0],
		$t[5]+1900);
  if ($out) { print "$t\n";}
  &stdcl; "$t";}

sub diff { local($type,$f,$out)=@_;

  local($in1,$in2)=split(' ',$f);
  local($in,$l1);

  if ($type=~/b/) {
    if ($VMS) {$l1='/IGNORE=(TRAIL,SPACE)';}
    else {$l1='-b';}
  }
  &stdmk;
  if ($VMS) { $in1=&fnp($in1); $in2=&fnp($in2);
    $status=&system("differ$l1$out $in1 $in2"); &stdcmk;}
  else { $status=&system("diff $l1 $in1 $in2 $out");}
}

sub docom { local($out,$in1,$in2,$in3)=@_;

  local(*TMP);

  open (TMP,">com$$.tmp");
  print TMP "\$ set default $cwd\n";
  if ($in1) { print TMP "\$ $in1\n";}
  if ($in2) { print TMP "\$ $in2\n";}
  if ($in3) { print TMP "\$ $in3\n";}
  print TMP "\$ PURGE *$$.TMP*";
  close(TMP);
  $status=&system("\@com$$.tmp$out");
  unlink ("com$$.tmp") while (-e "com$$.tmp");}

sub docsh { local($name,$f,$out)=@_;

  local($l2)=&fp('r',$name);
  local($in,@f);

  $name=&fnp($name);
  unless (&ft('e',$l2.'.pls') && &ft('M',$l2.'.pls') <
			&ft('M',"$n_src/sys/csh2p.pls") &&
			&ft('e',$name) &&
			&ft('M',$l2.'.pls') < &ft('M',$name)) {
    if (&ft('e',$name)) { $status=&system("perl ".
			&fnp("$n_src/sys/csh2p.pls").' -sub '.$name);}
    unless (&ft('e',$l2.'.pls') && &ft('M',$l2.'.pls') <
			&ft('M',"$n_src/sys/csh2p.pls") &&
			&ft('e',$name) &&
			&ft('M',$l2.'.pls') < &ft('M',$name)) {
      print "Fatal: Cannot find/compile $name"; exit;}
  }
  &stdmk;
  if ($VMS) {
    for (split(' ',$f)) {
      if (/^[\-\+]/) { push(@f,$_);}
      else { push(@f,&fnp($_));}
    }
    $f=join(' ',@f); $name=&fnp($name);
    $status=&docom($out,"perl $name $f"); &stdcmk;}
  else { $status=&system("perl $name $f $out");}
}

sub doexe { local($name,$f,$out)=@_;

  local($in,@f);

  &stdmk;
  if ($VMS) {
    for (split(' ',$f)) {
      if (/^[\-\+]/ || $_ eq '/list') { push(@f,$_);}
      else { push(@f,&fnp($_));}
    }
    $f=join(' ',@f);
    if (-e 'N_SRC:[SYS]N_LINKS.COM') {
      $status=&docom($out,'@N_SRC:[SYS]N_LINKS.COM',
		'tmp_exe=="$'.&fnp($name).' "',"tmp_exe $f");}
    else {
      $status=&docom($out,'tmp_exe=="$'.&fnp($name).' "',"tmp_exe $f");}
    $status=1; ## for now
    &stdcmk;}
  else {
    $f=~s/\^/\\\^/g;
    $status=&system("$name $f $out");}
}

sub domainname { local($out)=@_;

  local($in,*TMP);

  unless ($VMS) { &stdmk; $status=&system("domainname $out");}
  else { open(TMP,">".&fnp($out));
    print TMP "$ENV{'DOMAINNAME'}\n"; close (TMP);}
}

sub echo { local ($type,$txt,$out)=@_;

  local($in);

  $txt="$txt\n" unless $type eq '-n';
  &stdop; print "$txt"; &stdcl;}

sub elm { local($type,$sub,$in)=@_;

  local(@in)=split(' ',$sub);

  unless ($in) { $in=pop(@in); $sub=join(' ',@in);}
  $in=~s/^([^<])/<\1/;
  unless ($VMS) { $status=&system("mail $type $sub $in");}
}

sub exit { local($code)=@_;

  "$code"; ##die "Ending $0 with code '$code'" unless $C2_depth;
}

sub find { local($type,$out)=@_;

  local($in);

  &stdmk;
  unless ($VMS) { $status=&system("find $type $out");}
}

sub ftp { local($type,$f,$out)=@_;

  local(@in,$in,*TMP,$l1);

  @in=split(' ',$f);
  unless ($VMS) {
    &stdmk; if ($in[1]) {$in[1]=~s/^([^<])/<\1/;}
    $status=&system("ftp $type $in[0] $in[1] $out");}
  else { $in[1]=&fnp($in[1]);
    if ($type=~/\-v/) {$l1.='/VERBOSE';}
    open (TMP,">>$in[1]"); print TMP "quit\n"; close(TMP);
    $status=&system("ftp/passw=xyx/user=xyz$l1/take_file=$in[1] $in[0]");}
}

sub grep { local($type,$pi,$in,$out)=@_;

  $type=~s/\-//g;
  &stdop;
  while (<$S_in>) {
    if ((grep(/$pi/,$_) && $type!~/v/) ||
		(!grep(/$pi/,$_) && $type=~/v/)) { print $_;}
  }
  &stdcl;}

sub head { local($type,$in,$out)=@_;

  local($t);

  $type=~s/(\d)[a-z]$/\1/; $type=-10 unless $type<0;
  &stdop;
  while (<$S_in>) { $t++;
    if ($t <= -$type) { print;}
  }
  &stdcl;}

sub ln { local($type,$in)=@_;

  local(@in)=split(' ',$in);
  local(*TMP,*TMPI);

  unless ($VMS) {
    if ($type=~/\-s/) { symlink($in[0],$in[1]);}
    else { link($in[0],$in[1]);}
  }
  else {
    unless ($in[0]=~/[\[:\]]/) { $in[0]=$cwd.$in[0];}
    $in[1]=&fp('t',$in[1]);
    if (! -e 'N_SRC:[SYS]N_LINKS.COM') {                # Create link list
      open(TMP,'>N_SRC:[SYS]N_LINKS.COM');
      print TMP '$    !01 N_LINKS.COM created on '."$HOST at $C_Date\n";
      close(TMP);}
    if (open(TMP,">l$$.tmp") && open(TMPI,'N_SRC:[SYS]N_LINKS.COM')) {
      while(<TMPI>) {
        unless (/$in[1]/i) { print TMP $_;}              # Copy
      }
      print TMP '$  ASSIGN/NOLOG "'.$in[0].'" '.$in[1]." ! $C_Date\n";
      close(TMP); close(TMPI);
      &system("SORT l$$.tmp N_SRC:[SYS]N_LINKS.COM");
      unlink ("l$$.tmp") while (-e "l$$.tmp");
      &system("PURGE N_SRC:[SYS]N_LINKS.COM");
    }
  }
}

sub log { local($txt)=@_;

  local(*TMP);

  print "$txt\n";
  if ($Logfile) {
    open (TMP,&fnp(">>$Logfile"));
    print TMP "$txt\n"; close(TMP);}
}

sub ls { local($type,$f,$out)=@_;

  local($in);

  &stdmk;
  unless ($VMS) { $status=&system("ls $type $f $out");}
}

sub mem { local($in)=@_;

  unless ($VMS) { $status=&system("emacs $in");}
}

sub mkdir { local($in)=@_;

  mkdir(&fnp($in),0755);}

sub more { local($type,$f,$out)=@_;

  local($l1)=('');
  local($in);

  &stdmk;
  if ($VMS) {
    if ($out) { $l1=$out;}
    else {$l1="/PAGE";}
    $status=&system("type$l1 $f"); &stdcmk;}
  else { $status=&system("more $f $out");}
}

sub mv { local($type,$infile,$outfile)=@_;

  local(@in);

  @in=split(' ',$infile);
  unless ($outfile) { $outfile=pop(@in);}
  for (@in) {
    if ($_) {
      if ($_ ne $outfile) {
        if ($VMS) { $_=&fnp($_); $outfile=&fnp($outfile);
          $status=&system ("copy $_ $outfile");
          unlink($_) while -e $_;}
        else { $status=&system ("mv $_ $outfile");}
      }
    }
  }
}

sub nm { local($f,$out)=@_;

  local($in);

  &stdmk;
  unless ($VMS) { $status=&system("nm $f $out");}
}

sub peq { local($left,$right)=@_;

  if ($VMS) {
    unless ($right=~/^\[/ || $right=~/[^:]\[/) {
      ($right=&fnp($right))=~s/\%/\?/g;;
      $right=~s/([\[\]])/\\$1/g;}
    $right=~s/\.DIR//;
    $left=&fnp($left);}
  $right=~s/([\/\+\@\$\.])/\\$1/g; $right=~s/\*/\.\*/g;
  $right=~s/\?/\./g;
  if ($VMS) { $left=~/^$right$/i;}
  else { $left=~/^$right$/;}
}

sub eq { local($left,$right)=@_;

  if ($VMS) { ($right=&fnp($right))=~tr/A-Z/a-z/;
    ($left=&fnp($left))=~tr/A-Z/a-z/;}
  $left eq $right;}

sub pr { local($type,$f,$out)=@_;

  local($in);

  &stdmk;
  unless($VMS) { $status=&system("pr $type $f $out");}
  else { $status=&system("TYPE$out ".&fnp((split(' ',$f))[1]));}
}

sub ranlib { local($in)=@_;

  unless ($VMS) { $status=&system("ranlib $in");}
}

sub remsh { local($host,$com,$in,$out)=@_;

  &stdmk;
  unless ($VMS) { $status=system("remsh $host '$com' $in $out")/256;}
}

sub rm { local($type,$file)=@_;

  for (split(' ',$file)) {
    if($_) { $_=&fnp($_) if $VMS;
      if ($type eq '-i') {
        print ("\`$_\'? ");
        if (<STDIN> =~ /^y/i) {unlink($_) while -e $_;}
      }
      else {unlink($_) while -e $_;}
    }
  }
}

sub rsh { local($host,$com,$in,$out)=@_;

  &stdmk;
  unless ($VMS) { $status=system("rsh $host '$com' $in $out")/256;}
}

sub sed { local($sw,$prog,$in,$out)=@_;

  local($l1);

  &stdop;
  while (<$S_in>) {chop; $l1=$_;
    eval("\$l1=~$prog"); print "$l1\n";}
  &stdcl;}

sub set { local($val,$out)=@_;

  local($in,$i);

  if ($val) { local(@t)=split(' ',$val);
    if ($t[0]=~s/\=$//) { splice(@t,0,1,$t[0],'=');}
    if ($t[2] eq '(') { eval("\$$t[0]=''");
      for ($i=3; $i<$#t; $i++) {
        eval("\$$t[0]=join(' ',\$$t[0],\"$t[$i]\")");}
      eval("\$$t[0]=join(' ',split(' ',\$$t[0]))");}
    else {
      for ($i=0; $i<$#t; $i+=3) {
        eval("\$$t[$i]=\"$t[$i+2]\"");}
    }
  }
  else { &stdop;
    for $i (keys(%_main)) {
      if ($i=~/^\w+$/) {
        eval("if (defined(\$$i)) {print \"$i\t\$$i\n\";}");}
    }
    &stdcl;}
}

sub sort { local($type,$f,$out)=@_;

  local($l1,@in);

  if ($type=~/\-o/){
    @in=split(' ',$f); $out=shift(@in); $f=$in[0];}
  if ($VMS) { 
    if ($type=~/-u/) {$l1='/NODUP';}
    $f=&fnp($f); $out=&fnp($out);
    $status=&system("sort$l1 $f $out");}
  else {
    if ($type=~/-u/) {$l1='-u';}
    $status=&system("sort $l1 -o $out $f");}
}

sub source { local($l1)=@_;

  local($l2)=&fp('r',$l1);
  local($l3)=&fp('t',$l2);

  $l1=&fnp($l1);
  unless (&ft('e',$l2.'.pls') && &ft('M',$l2.'.pls') <
			&ft('M',"$n_src/sys/csh2p.pls") &&
			&ft('e',$l1) &&
			&ft('M',$l2.'.pls') < &ft('M',$l1)) {
    if (&ft('e',$l1)) { $status=&system("perl ".
			&fnp("$n_src/sys/csh2p.pls").' -sub '.$l1);}
    unless (&ft('e',$l2.'.pls') && &ft('M',$l2.'.pls') <
			&ft('M',"$n_src/sys/csh2p.pls") &&
			&ft('e',$l1) &&
			&ft('M',$l2.'.pls') < &ft('M',$l1)) {
      print "Fatal: Cannot find/compile $l1"; exit;}
  }
  if (eval("defined &${l3}__pls") && $C2_INC{$l3} eq $l1) {
    eval("&${l3}__pls");}
  else { unshift(@INC,&fp('h',$l1));
    if ($VMS) { &system("COPY $l2.pls $PWD_IN");}
    if (eval("defined &${l3}__pls")) { do "$l3.pls";}
    else {require "$l3.pls";}
    shift(@INC);
    unless (eval("defined &${l3}__pls")) {
      print "Fatal: Cannot load $l3.pls\n"; exit;}
    $C2_INC{$l3}=$l1;}
}

sub strip { local($in)=@_;

  unless ($VMS) { $status=&system("strip $in");}
}

sub stty { local($type)=@_;

  unless ($VMS) { $status=&system("stty $type");}
}

sub tail { local($type,$in,$out)=@_;

  local(@t,$t);

  $type=~s/(\d)[a-z]$/\1/; $type=-10 unless $type;
  &stdop;
  while (<$S_in>) { $t++;
    if ($type>=0) {
      unless ($t < $type) { print;}
    }
    else {
      if ($#t+1 >= -$type) { shift(@t);}
      push(@t,$_);}
  }
  unless ($type>=0) { print @t;}
  &stdcl;}

sub tar { local($type,$f,$out)=@_;

  local($in); &stdmk;

  unless ($VMS) { $status=&system("tar $f $out");}
}

sub tee { local($type,$out,$in)=@_;

  local(@t);

  $out='>'.$out if $out; &stdop;
  while (<$S_in>) { print STDOUT $_; print $_;}
  &stdcl;}

sub touch { local($type,$file)=@_;

  local(*TMP);

  for (split(' ',$file)) {
    if ($_) { $_=&fnp($_) if $VMS;
      unless (-e $_) { 
        unless ($VMS && /\.ppd$/) { open(TMP,">$_"); close(TMP);}
      }
      else { open(TMP,">>$_"); close(TMP);}
    }
  }
}

sub tr { local($type,$pi,$po,$in,$out)=@_;

  $type=~s/\-//g; &stdop;
  if ($VMS && $pi=~s/\\\-//) { $pi.='-';}
  while (<$S_in>) {
    eval("tr/$pi/$po/$type");
    print $_;}
  &stdcl;}

sub unalias { local($name)=@_;

  delete $C2_alias{$name};}

sub uncompress { local($f,$out)=@_;

  local($in); &stdmk;

  unless ($VMS) { $status=&system("uncompress $f $out");}
}

sub wc { local($type,$in,$out)=@_;

  local($t);

  &stdop;
  while (<$S_in>) {
    if ($type eq '-c') {$t+=length($_);}
    else {$t++;}
  }
  print "$t\n";
  &stdcl;}

sub what { local($type,$f,$out)=@_;

  local($in); &stdmk;

  unless ($VMS) { $status=&system("what $type $f $out");}
}

sub whoami { local($out)=@_; local($in);

  local($t)=("$ENV{'USER'}\n");

  &stdop; print "$t"; &stdcl;}

sub VMS_IMPORT {

  local(*ATMP,@aa);
#
# Check if VMS and read all logicals into %ENV
#
  if (! $VMS) {
    if ("$ENV{'SHELL'}") {		# VMS version has no SHELL
      $VMS=0;}				# assume no VMS
    else { $VMS=1;
      open(ATMP,">a$$.tmp");		# get command file
      print ATMP "\$ define/nolog/job n_cwd 'f\$env(\"default\")'\n";
      print ATMP "\$ define/nolog/job n_host 'f\$getsyi(\"nodename\")'\n";
      close(ATMP);
      $status=&system("\@a$$.tmp");
      $status=&system("show log/proc/job/out=a$$.tmp ".
		"n_*,ARR,ARD,NSTAR_DIR,CC,FC,AS,RSH,".
		"DOMAINNAME");		# read n_ log. in tmp file
      open (ATMP,"a$$.tmp");		# convert lines to %ENV
      while (<ATMP>) {
        chop;
        @aa=split(/=/,$_);
        if ($aa[1]) {
          grep(s/^\s*\"//,@aa); grep(s/\"\s*$//,@aa);
	  if ($aa[0]=~/^n_/i) {$aa[0] =~ tr/A-Z/a-z/;}
	  else {$aa[0] =~ tr/a-z/A-Z/;}
          $ENV{$aa[0]}="$aa[1]";
        }
      }
      close(ATMP);
      while (unlink("a$$.tmp")){}	# unlink all versions
      $ENV{'PWD'}=$ENV{'n_cwd'};	# set PWD
      $PWD_IN=$ENV{'PWD'};		# initial PWD
      $ENV{'HOST'}=$ENV{'n_host'};	# set HOST
      $ENV{'HOST'}=~tr/A-Z/a-z/;	# make sure lc
      delete $ENV{'n_cwd'}; delete $ENV{'n_host'};
    }
  }
}

#
# Indicate correct compilation
#
1;
