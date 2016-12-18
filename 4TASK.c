#include <stdio.h>
#include <string.h>
#include <sys/queue.h>
#include <stdlib.h>

int i[3] = {0,0,0};

typedef int (*fPtr)();


typedef struct
 {
    int ki;
    fPtr pr;
} tki;



typedef struct
{
    tki program[100];
    int heap_size;

} tHeap;


tki extract(tHeap* A)
{
    if (A -> heap_size < 0)
        printf("error");

    else

    {

    tki maximum = A->program[0];
    A->heap_size--;
    A->program[0] = A->program[A->heap_size];
    heap_s(A,A->heap_size);

    Swap(A);

    return maximum;
    }
}


int Inkey(tHeap* A, int i, tki ki)
 {
    A -> program[i] = ki;

    while ((i > 0)&&(A -> program[i/2].ki < A -> program[i].ki)){
        tki c;
        c = A -> program[i];
        A -> program[i] = A -> program[i/2];
        A -> program[i/2] = c;
    }
    return 0;
}



int Insert(tHeap* A, tki ki)
 {
    A -> program[A -> heap_size].ki = -10000;
    Inkey(A, A -> heap_size, ki);

    (A -> heap_size)++;

    return 0;
}





int maximum (tHeap a, int n, int i, int j, int k)
 {
    int m = i;
    if (j < n && a.program[j].ki > a.program[m].ki) {
        m = j;
    }

    if (k < n && a.program[k].ki > a.program[m].ki) {
        m = k;
    }
    return m;
}


void heap_d  (tHeap* a, int n, int i)
{
    while (1) {
        int j = maximum( (*a), n, i, 2 * i + 1, 2 * i + 2);
        if (j == i) {
            break;
        }
        tki t = a -> program[i];

        a -> program[i] = a -> program[j];
        a -> program[j] = t;
        i = j;
    }
}


void heap_s (tHeap* a, int n)
 {
    int i;

    for (i = (n - 2) / 2; i >= 0; i--) {
        heap_d(a, n, i);
    }

    for (i = 0; i < n; i++) {
        tki t = a->program[n - i - 1];

        a->program[n - i - 1] = a->program[0];
        a->program[0] = t;

        heap_d(a, n - i - 1, 0);
    }
}

void Swap(tHeap* A)
 {
    int i;

    for (i = 0; i < A -> heap_size / 2; i++)
    {
        tki c = A -> program[i];

        A -> program[i] = A -> program[A -> heap_size - i-1];
        A -> program[A -> heap_size - i-1]=c;
    }
}




int program1()
{
    char* s = "What is";
    int len = strlen(s);
    i[1]++;
    printf("Program 1 {%d %s} - %c\n", i[1]*100 / len,"%", s[i[1]-1]);

    if (i[1] >= len)
        printf("Program 1 - Complited\n");

    return i[1] >= len;
}


int program2()
{
    char* s = "What is love?";
    int len = strlen(s);
    i[2]++;
    printf("Program 2 {%d %s} - %c\n", i[2]*100/len,"%", s[i[2]-1]);

    if (i[2] >= len)
        printf("Program 2 - Complited\n");
    return i[2] >= len;
}


int program3()
{
    char* s = "What is lifestyle ???";
    int len = strlen(s);
    i[3]++;
    printf("Program 3 {%d %s} - %c\n", i[3]*100/len,"%", s[i[3]-1]);

    if (i[3] >= len)
        printf("Program 3 - Complited\n");
    return i[3] >= len;
}




int main()
{
    int s;


    printf("1: FIFO\n");
    printf("2: More tasks = More fun\n");
    printf("3: Choose program priority\n");
    printf("4: time1 = time2 = time3... \n");

    scanf("%d",&s);

    switch (s)
    {
        case 1: {
            TAILQ_HEAD(tailhead, entry) head;
            struct entry
            {
                int (*fPt)();
                TAILQ_ENTRY(entry) entries;
            } *n1,*n2;

            TAILQ_INIT(&head);
            n1 = malloc(sizeof(struct entry));
            n1->fPt = program1;

            TAILQ_INSERT_HEAD(&head, n1, entries);
            n1 = malloc(sizeof(struct entry));
            n1->fPt = program2;

            TAILQ_INSERT_HEAD(&head, n1, entries);
            n1 = malloc(sizeof(struct entry));
            n1->fPt = program3;

            TAILQ_INSERT_HEAD(&head, n1, entries);

            while (!TAILQ_EMPTY(&head))
            {
                n1 = TAILQ_FIRST(&head);
                while (!n1 -> fPt());
                TAILQ_REMOVE(&head,  n1,entries);
                free(n1);
            }
            break;

        }
        case 2:
            {
            tHeap A;
            A.heap_size=0;
            tki k;
            k.pr = program1;
            k.ki = 100;
            Insert(&A,k);

            k.pr = program2;
            k.ki = 60;
            Insert(&A,k);

            k.pr = program3;
            k.ki = 80;
            Insert(&A,k);

            while (A.heap_size > 0)
            {
                heap_s(&A,A.heap_size);
                tki k = extract(&A);
                while (!k.pr());
            }
            break;
        }

        case 3:
            {

            tHeap A;
            A.heap_size = 0;
            tki k;
            k.pr = program1;
            k.ki = 128;
            Insert(&A, k);
            k.pr = program2;
            k.ki = 256;
            Insert(&A, k);

            k.pr = program3;
            k.ki = 4;
            Insert(&A,k);

            while (A.heap_size > 0)
            {
                heap_s(&A, A.heap_size);
                Swap(&A);
                tki k = extract(&A);

                while (!k.pr());
            }
            break;
        }

        case 4:
            {

            TAILQ_HEAD(tailhead, entry) head;
            struct entry
            {
                int (*fPt)();
                TAILQ_ENTRY(entry) entries;
            } *n1, *n2;

            TAILQ_INIT(&head);
            n1 = malloc(sizeof (struct entry));
            n1 -> fPt = program1;

            TAILQ_INSERT_HEAD(&head, n1, entries);
            n1 = malloc(sizeof(struct entry));
            n1->fPt = program2;

            TAILQ_INSERT_HEAD(&head, n1, entries);
            n1 = malloc(sizeof(struct entry));
            n1->fPt = program3;

            TAILQ_INSERT_HEAD(&head, n1, entries);

            n1 = TAILQ_FIRST(&head);

            while (!TAILQ_EMPTY(&head))
                {
                n1 = TAILQ_FIRST(&head);
                int i = n1 -> fPt();

                TAILQ_REMOVE(&head, n1, entries);
                if (i) free(n1);
                else TAILQ_INSERT_TAIL(&head, n1, entries);
            }

            break;
        }
    }


    return 0;
}
