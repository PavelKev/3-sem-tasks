#include <stdio.h>
#include <string.h>
#include <stdlib.h>


int m[3] = {0,0,0};

int program1()
{
    char* s = "What is";
    int len = strlen(s);
    m[1]++;
    printf("Program 1 {%d %s} - %c\n", m[1]*100/len,"%", s[m[1]-1]);

    if (m[1] >= len)
        printf("Program 1 - Complited\n");
    return m[1] >= len;
}

int program2()
{
    char* s = "What is love?";
    int len = strlen(s);
    m[2]++;
    printf("Program 2 {%d %s} - %c\n", m[2]*100/len,"%", s[m[2]-1]);

    if (m[2] >= len)
        printf("Program 2 - Complited\n");
    return m[2] >= len;
}


int program3()
{
    char* s = "What is lifestyle ???";
    int len = strlen(s);
    m[3]++;
    printf("Program 3 {%d %s} - %c\n", m[3]*100/len,"%", s[m[3]-1]);

    if (m[3] >= len)
        printf("Program 3 - Complited\n");
    return m[3] >= len;
}


typedef int (*Prog)();
Prog programs[3] = {program1, program2, program3};

int main()
{
    int a[3] = {0,0,0};
    int f = 1;

    while (f == 1)
        {
            f = 0;
            int i;
            for (i = 0; i < 3; i++)
                if (!a[i] == 1)
                {
                    a[i] = programs[i]();
                    f = 1;
                }
        }

    _getch();
    return 0;
}
