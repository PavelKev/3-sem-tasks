#include<stdio.h>
#include<string.h>
#include<stdlib.h>
#include<unistd.h>
#include<sys/types.h>
#include<sys/stat.h>
#include<sys/socket.h>
#include<arpa/inet.h>
#include<netdb.h>
#include<signal.h>
#include<fcntl.h>


#define B 1024


#define maxi 100



char *RT;

void start(char *);
void answer(int);
int readf, users[maxi];
void err(char *);




void start(char *P)
{
    struct addrinfo hints, *res, *p;

    // getaddrinfo for host
    memset (&hints, 0, sizeof(hints));

    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_flags = AI_PASSIVE;

    if (getaddrinfo( NULL, P, &hints, &res) != 0)
    {
        perror ("getaddrinfo() err");
        exit(1);
    }
    // socket and bind


    for (p = res; p!=NULL; p=p->ai_next)
    {
        readf = socket (p->ai_family, p->ai_socktype, 0);
        if (readf == -1)
            continue;
        if (bind(readf, p->ai_addr, p->ai_addrlen) == 0)
         break;
    }

    if (p == NULL)
    {
        perror ("socket() or bind()");
        exit(1);
    }

    freeaddrinfo(res);

    // listen for connect
    if ( listen (readf, 1000000) != 0 )
    {
        perror("listen() err");
        exit(1);
    }
}

//client connect
void answer(int n)
{

    char mesg[99999], *reqline[3], data_to_send[B], path[99999];
    int rcvd, fd, b_read;

    memset( (void*)mesg, (int)'\0', 99999 );

    rcvd = recv(users[n], mesg, 99999, 0);

    if (rcvd < 0)
        // err

        fprintf(stderr,("recv() err\n"));
    else if (rcvd == 0)
       // socket closed
        fprintf(stderr,"The client is not available\n");
    else
       // message received
    {
        printf("%s", mesg);

        reqline[0] = strtok (mesg, " \t\n");
        if ( strncmp(reqline[0], "GET\0", 4)==0 )
        {
            reqline[1] = strtok (NULL, " \t");
            reqline[2] = strtok (NULL, " \t\n");

            if ( strncmp( reqline[2], "HTTP/1.0", 8)!=0 && strncmp( reqline[2], "HTTP/1.1", 8)!=0 )
            {
                write(users[n], "HTTP/1.0 400 Bad Request\n", 25);
            }
            else
            {
                if ( strncmp(reqline[1], "/\0", 2)==0 )
                    reqline[1] = "/index.html";

                strcpy(path, RT);
                strcpy(&path[strlen(RT)], reqline[1]);

                printf("file: %s\n", path);

                if ( (fd=open(path, O_RDONLY))!=-1 )    //F FOUND
                {
                    send(users[n], "HTTP/1.0 200 OK\n\n", 17, 0);
                    while ( (b_read=read(fd, data_to_send, B))>0 )
                        write (users[n], data_to_send, b_read);
                }
                else    write(users[n], "HTTP/1.0 404 Not Found\n", 23); //F NOT FOUND
            }
        }
    }

    //Clos SOCKeET
    shutdown (users[n], SHUT_RDWR);
    close(users[n]);
    users[n] = -1;
}




int main(int cc, char* vv[])
{
    struct sockaddr_in claddr
    ;
    socklen_t addrlen;
    char c;


    char P[6];
    RT = getenv("PWD");
    strcpy(P,"10000");

    int sl=0;


    //Parsing
    while ((c = getopt (cc , vv, "p:r:")) != -1)

        switch (c)
        {
            case 'r':
                RT = malloc(strlen(optarg));
                strcpy(RT, optarg);

                break;

            case 'p':
                strcpy(P, optarg);

                break;

            case '?':
                fprintf(stderr, "It is wrong arg\n");
                exit(1);
            default:
                exit(1);
        }

    printf("The server started on port number %s%s%s with directory as %s%s%s\n","\033[92m",P,"\033[0m","\033[92m",RT,"\033[0m");
    // Setting all elements to -1 :)
    int i;

    for (i = 0; i < maxi; i++)
        users[i] = -1;
    start(P);

    // ACCEPT connect

    while (1)
    {
        addrlen = sizeof(claddr);
        users[sl] = accept (readf, (struct sockaddr *) &claddr, &addrlen);

        if (users[sl] < 0)
            err ("accept() err");
        else
        {
            if ( fork() == 0 )
            {
                answer(sl);
                exit(0);
            }
        }

        while (users[sl] != -1)
            sl = (sl + 1) % maxi;
    }

    return 0;
}

