/* batch_log.exe - log prompts and replies from program run

	stdin is piped in from the program
	stdout is redirected to the log file
	all input is copied to stderr
*/

#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
 
int main()
{
  char *sp, text[512], *n_psctest;
  int prompt =0;
 
  while (fgets (text, 512, stdin)) {	/* loop over input lines */
    if (*text ==''){
      prompt= 1;
      for (sp=text; *sp && *sp !=' '; sp++);
      *sp= 0;
      fputs (text+1, stdout);
      *sp= ' ';
      *text= ' ';
    }
    if (prompt ){
      for (sp=text; *sp; sp++);	 		/* skip to end of input */
      if (*--sp =='\n' && *--sp =='~'){		/* last chars '~\n' ?	*/
        *sp= 0;					/* truncate before '~\n' */
        prompt= 2;
    } }
    fputs (text, stderr);
    if (prompt ==2){
      fgets (text, 512, stdin);
      if (text[0] ==' ' && text[1] =='#' ){
	fputs (text, stderr);			/* echo EOF */
      }
      fputs (" = ", stdout);
      fputs (text, stdout);
      prompt= 0;
  } }
    return errno;
}

