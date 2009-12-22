/*
 *$Prog$
 *
 * $Id$
 *
 * $Purpose: Standalone client to call upon scissor from Newstar
 *
 * $Usage:   Command scissor has to be defined to run this program
 * 
 * $Log$
 *
 * Revision history:
 *
 *	CMV 941103	Created from client.exa
 *	CMV 950123	Return exit status
 *
 * Environment:
 *
 *    QED1      The url to qed (like //www.astron.nl:8083)
 *    USER      Current user
 *    SCIPWD    The password of this user on qed
 *    QEDDEBUG  Print every line returned by qed?
 *
 *    QEDDEBUG is only tested if a single command is sent, in interactive
 *    mode all output is printed.
 *
 *    This client allows to override the settings for QED1, USER 
 *    and SCIPWD by specifying altenative values on the commandline:
 *        //host:port user password
 *
 *    If the first argument does not start with //, the arguments will
 *    be sent to qed (defined by QED1) as a single command.
 *
 *    Otherwise this client will repeatedly ask for commands to be sent 
 *    to the qed. The extra command "quit" closes the connection.
 *
 *$/Prog$
 */

static char _ID_[]="$Id$";

#include <stdlib.h>
#include <stdio.h>

static void usage(name)

char *name;

{
   fprintf(stderr,"\nSyntax:  %s [server username password]\n",name);
   fprintf(stderr,  "  or:    %s [command]\n",name);
   fprintf(stderr,  "     where server is: //host:port\n");
   fprintf(stderr,  "     and command is a single command for qed\n\n");
   fprintf(stderr,  "If no server, username and password are given,\n");
   fprintf(stderr,  "they are taken from $QED1, $USER and $SCIPWD.\n");
   fprintf(stderr,  "If no command is given, %s enters an interactive\n");
   fprintf(stderr,  "mode, use quit or bye to exit.\n\n");
   exit(1);
}


main(argc,argv)

int argc;
char **argv;

{
   int ld,cmd,st,qeddebug;
   char buf[1024],retbuf[1024];
   
   /*
      If the first argument starts with // it is an URL, 
      else it is a command.
   */
   cmd=(argc>1 && argv[1][0]!='/' && argv[1][1]!='/');

   /*
      No switches are allowed, they all generate the help message
      If an URL is given, user and passwd are required
   */  
   if (( cmd && argv[1][0]=='-') ||
       (!cmd && argc!=1 && argc!=4)) usage(argv[0]);

   /*
      If QEDDEBUG is defined, debugging is turned on.
      Interactive mode always has debugging on.
   */
   qeddebug=(getenv("QEDDEBUG")!=NULL || !cmd);

   /*
       Make a connection. 
       open_socket returns the socket to be used in further calls
       if the connection could not be made, it returns -1
   */
   if ( (cmd || argc==1) && getenv("QED1")==NULL) {
      fprintf(stderr,"No server specified, cannot connect...\n");
      usage(argv[0]);
   } else if (cmd || argc==1) {
      ld=open_socket(getenv("QED1"));
   } else {
      ld=open_socket(argv[1]);
   }

   if (ld<=0) {
      fprintf(stderr,"\nCould not connect to Scissor, aborting\n");
      exit(1);
   }

   /*
      The first command should be an authorisation
      The syntax is hello=user:password
      send_command returns the status code from the server.
      Codes 100,200 etc are returned on successful commands
      In this case, we expect 100 to be returned for validation
   */
   if (cmd || argc==1) {
      if (getenv("SCIPWD")==NULL) {
        sprintf(buf,"HELLO=%s",getenv("USER"));
      } else {
        sprintf(buf,"HELLO=%s:%s",getenv("USER"),getenv("SCIPWD"));
      }    
   } else {
      sprintf(buf,"hello=%s:%s",argv[2],argv[3]);
   }
   
   if (send_command(ld,buf,retbuf,1024,qeddebug)%100 !=0) {
      fprintf(stderr,"\nInvalid password for Scissor...\n");
      exit(1);
   }

   /* 
      If a single command was given, handle it
   */
   if (cmd) {    
      for (cmd=1,buf[0]='\0'; cmd<argc; cmd++) {
      	strcat(buf,argv[cmd]); 
      	strcat(buf," ");
      }
      st=send_command(ld,buf,retbuf,1024,qeddebug);
      if (!qeddebug) printf("%s\n",retbuf);
      
   /*
      Else read commands until the user gives a quit.
      If a line with something on it is given, send the command.
   */
   } else {
      while (strncasecmp(buf,"quit",4) && strncasecmp(buf,"bye",3)) {
        fputs("\nqed> ",stderr);
        if (fgets(buf,1000,stdin)==NULL) {
           strcpy(buf,"bye");
           fprintf(stderr,"%s\n",buf);
        }
        if (*buf!='\n' && strncasecmp(buf,"quit",4) && 
                          strncasecmp(buf,"bye",3)) {
           st=send_command(ld,buf,retbuf,1024,1);
        }
      }
   }

   /*
       Finally close the connection. This is not really necessary.
   */
   close_socket(ld);

   exit((st!=200));
}

/**************  CLIENT SUBROUTINES ARE BELOW *************************/

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <signal.h>
#include <errno.h>
extern int errno;

#define LF 10
#define CR 13

static unsigned int timeout=1200;   /* Default timeout value */

static int put();
static int get();
static int getaline();
static void getline_timed_out();

/*
   Open_socket finds host and port from the url and establishes a connection
*/

int open_socket(url)

char *url;

{
    struct hostent *gethostbyname();
/*    extern char *malloc(); */

    char *p,*buf;
    int  port,sock,st;
    struct hostent     *remote;
    struct sockaddr_in srv;

    sock=socket(AF_INET,SOCK_STREAM,IPPROTO_TCP);
    if (sock == -1) {
       fprintf(stderr,"Cannot create socket\n");
       return(sock);
    } 

    buf=malloc(strlen(url));
    if (buf==NULL) {
       fprintf(stderr,"Cannot allocate buffer\n");
       return(-1);
    }
    strcpy(buf,url+2);

    for (p=buf; *p!='\0' && *p!=':'; p++);
    if (*p=='\0') {
       fprintf(stderr,"No port number in url string, default to 8083\n");
       port=8083;
    } else {
       *p='\0';
       port=atoi(p+1);
    }

    remote=gethostbyname(buf);
    if (remote==NULL) {
       fprintf(stderr,"Cannot get host by name\n");
       st= -1;
    } else {
       srv.sin_family=AF_INET;
       srv.sin_addr.s_addr=htonl(INADDR_ANY);
       srv.sin_port=htons(port);
       srv.sin_addr= *((struct in_addr *) remote->h_addr);

       st=connect(sock,&srv,sizeof(srv));
       if (st== -1) fprintf(stderr,"Cannot connect to server\n");
       else         st=sock;
    }

    free(buf);
    return(st);
}



/*
   Close_socket closes the connection
*/

int close_socket(socket)

int socket;

{
    if (socket>3) close(socket);
    return(1);
}


/*
   Send_command transfers the command string and waits for a respons
   The respons is printed on stdout
   The status code at the beginning of the response is returned
   and retrieve an 8 characters return code.
*/

int send_command(socket,command,retbuf,len,qeddebug)

int  socket,len,qeddebug;
char *command,*retbuf;

{
    char str[2048];
    int js;

    errno=EINTR;  /* Default: error */

/*
   Check if valid socket
*/
    if (socket<3) {
       fprintf(stderr,"Invalid socket\n");
       return(-1);
    }

/*
   Send the string to the socket
*/

/*
    js=write(socket,command,strlen(command));
*/
    strncpy(str,command,2040); str[2040]='\0'; 
    if (str[strlen(str)-1]!='\n') strcat(str,"\n"); 
    js=put(socket,str,strlen(str));

    if (js!=strlen(str)) {
       fprintf(stderr,"Cannot send command on socket\n");
       return(js);
    }
/*
   Wait for reply
*/
    while (js>0) {
      *str='\0';
      js=getaline(str,2000,socket);
      if (js<=0) {
         fprintf(stderr,"Cannot read response from socket\n");
         return(-1);
      }
      if (qeddebug) printf("%s\n",str);
      if (str[3]!='-') js=0;   /* No more lines expected */
    }

/*
   Return status and last line
*/
    strncpy(retbuf,str+4,len);
    retbuf[len-1]='\0';

    errno=0;
    js=atoi(str);
    if (js<0) js= -1;           /* Should set errno as well... */
    return( js );
}


/*
    The trick to catch the EINTR has been shamelessly taken from 
    the Gipsy routine mtiodev.c (KGB, Kapteyn Lab, Univ. of Groningen)
*/

static int put(socket,buf,size)

int  socket,size;
char *buf;

{
   int left,done;

   for (left=size; left; left-=done,buf+=done) {
      while ( (done=write(socket,buf,left)) == -1 && errno == EINTR);
      if (done== -1) return(done);
   }
   return(size);
}

static int get(socket,buf,size)

int  socket,size;
char *buf;

{
   int left,done;

   for (left=size; left; left-=done,buf+=done) {
      while ( (done=read(socket,buf,left)) == -1 && errno == EINTR);
      if (done== -1) return(done);
   }
   return(size);
}


static int getaline(str,n,ld)

char *str;
int n,ld;

{
    int i=0, ret;

    signal(SIGALRM,getline_timed_out);
    alarm(timeout);

    while (1) {
      if ( (ret = read(ld,&str[i],1)) <= 0) {
            /* Mmmmm, Solaris.  */
         if ( (ret == -1) && (errno == EINTR)) continue;
         str[i] = '\0';
         return(i);
      }

      if (str[i] == CR) read(ld,&str[i],1);

      if ( (str[i] == LF) || (i == (n-1))) {
          alarm(0);
          signal(SIGALRM,SIG_IGN);
          str[i] = '\0';
          return(i);
       }
       ++i;
    }
}

static void getline_timed_out() 

{
    fprintf(stderr,"timed out waiting for respons");    
    exit(1);
}


