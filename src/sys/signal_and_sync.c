/* signal_and_sync.c 

optionally send a SIGUSR1 signal to a process;
then wait for that process to respond by accessing a file.

invocation arguments:
	1. filename
	2. number of cycles to wait
	3. if present: pid to send signal to; if absent: do not signal

exit status:
	 0: success
	<0: target pid not found
	>0: timeout: file not accessed after waiting for the specified
	    nr of seconds

	This program has been designed with the purpose of synchonising a Newstar application with xmosaic when the latter is (re)started by the former through PPDHELP.

	The two-argument invocation is used to wait for xmosaic to acces the home page after it has been started. This is used as an indication that it is (almost) ready to accept a 'goto' signal.

	The three-argument invocation is used to send the signal and then check
that xmosaic does indeed access the 'goto' command file; if it fails to, the signal can then be resent. 

   
History:
	JPH 941109
*/

#include <signal.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>


main (argc, argv)
  int argc; char *argv[];
{		
  struct stat buf;
  char *f;
  int d, n, p; long t, tbeg;

  argv++; f=*argv++; n= atoi (*argv++);
  d=0;
  while (d <n){
    if (stat (f, &buf) ==-1) {
      perror(f); return -1;
    } else {
      t= buf.st_atime;
      if (! d ){
	tbeg=t;
        if (argc >3 ){
          p= atoi (*argv);
	  sleep (1);			      /* allow for time quantisation */
          if (kill (p, SIGUSR1)) return -1;   /* process not found */
	}  
      }else{
	if (t != tbeg) return 0;	      /* success */
      }
      d++; sleep (1);
  } }
  return d;				      /* timeout */
}
