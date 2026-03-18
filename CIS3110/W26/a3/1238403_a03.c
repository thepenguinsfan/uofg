#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <semaphore.h>
#include <sys/stat.h>
#include <time.h>

void logStart(char* tID);
void logFinish(char* tID);

void startClock();
long getCurrentTime();
time_t programClock;

typedef struct thread
{
    char tid[4];
    unsigned int startTime;
    int state;
    pthread_t handle;
    int retVal;
} Thread;

/* --- Synchronization globals --- */
static sem_t sem_even;       /* semaphore for threads whose last digit is even */
static sem_t sem_odd;        /* semaphore for threads whose last digit is odd  */
static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

static int last_parity = -1; /* parity of the thread that last ran: 0=even,1=odd,-1=none */
static int finished_count = 0;
static int total_threads = 0;
static int all_started = 0;  /* set to 1 after the last thread has been created */
static int waiting_even = 0; /* threads waiting on sem_even */
static int waiting_odd  = 0; /* threads waiting on sem_odd  */

/* Return the parity (0=even, 1=odd) of the last character of a thread ID */
static int parity_of(const char *tid)
{
    int last_digit = tid[strlen(tid) - 1] - '0';
    return last_digit % 2; /* 0 = even, 1 = odd */
}

void* threadRun(void* t)
{
    Thread *me = (Thread*)t;
    int my_parity = parity_of(me->tid);

    logStart(me->tid);

    /* --- Entry section --- */
    pthread_mutex_lock(&mutex);

    /* Track how many threads of each parity are waiting */
    if (my_parity == 0) waiting_even++;
    else                 waiting_odd++;

    /*
     * A thread may enter the critical section when:
     *   (a) No thread has run yet (first thread goes freely), OR
     *   (b) The last thread had the opposite parity, OR
     *   (c) Starvation avoidance: all threads have been created and there
     *       are no waiting threads of the opposite parity.
     */
    while (1) {
        int opposite_waiting = (my_parity == 0) ? waiting_odd : waiting_even;
        int starvation_ok = all_started && (opposite_waiting == 0);

        if (last_parity == -1 || last_parity != my_parity || starvation_ok) {
            break; /* allowed to proceed */
        }
        /* Must wait — release mutex and block on our semaphore */
        pthread_mutex_unlock(&mutex);
        if (my_parity == 0) sem_wait(&sem_even);
        else                 sem_wait(&sem_odd);
        pthread_mutex_lock(&mutex);
    }

    /* We are now inside the mutex and allowed in */
    if (my_parity == 0) waiting_even--;
    else                 waiting_odd--;

    pthread_mutex_unlock(&mutex);

    /* --- Critical section --- */
    printf("[%ld] Thread %s is in its critical section\n",
           getCurrentTime(), me->tid);

    /* --- Exit section --- */
    pthread_mutex_lock(&mutex);

    last_parity = my_parity;
    finished_count++;

    /*
     * Wake up one thread of the opposite parity if any are waiting.
     * If none of the opposite parity are waiting but some same-parity
     * threads are (starvation case, only after all threads started),
     * wake one of them.
     */
    int opposite_waiting = (my_parity == 0) ? waiting_odd : waiting_even;
    int same_waiting     = (my_parity == 0) ? waiting_even : waiting_odd;

    if (opposite_waiting > 0) {
        if (my_parity == 0) sem_post(&sem_odd);
        else                 sem_post(&sem_even);
    } else if (all_started && same_waiting > 0) {
        /* starvation avoidance: unblock a same-parity thread */
        if (my_parity == 0) sem_post(&sem_even);
        else                 sem_post(&sem_odd);
    }

    pthread_mutex_unlock(&mutex);

    logFinish(me->tid);
    return NULL;
}

int readFile(char* fileName, Thread** threads)
{
    FILE *fp = fopen(fileName, "r");
    if (!fp) {
        fprintf(stderr, "Cannot open file: %s\n", fileName);
        return -1;
    }

    /* Count lines first */
    int count = 0;
    char line[64];
    while (fgets(line, sizeof(line), fp)) {
        if (line[0] != '\n' && line[0] != '\r') count++;
    }
    rewind(fp);

    *threads = (Thread*)malloc(count * sizeof(Thread));
    if (!*threads) { fclose(fp); return -1; }

    int i = 0;
    while (i < count && fgets(line, sizeof(line), fp)) {
        /* Strip \r and \n */
        line[strcspn(line, "\r\n")] = '\0';
        if (strlen(line) == 0) continue;

        /* Format: tID;startTime */
        char *semi = strchr(line, ';');
        if (!semi) continue;
        *semi = '\0';

        strncpy((*threads)[i].tid, line, 3);
        (*threads)[i].tid[3] = '\0';
        (*threads)[i].startTime = (unsigned int)atoi(semi + 1);
        (*threads)[i].state  = 0;
        (*threads)[i].retVal = 0;
        i++;
    }
    fclose(fp);
    return i;
}

/* Comparator: sort threads by startTime ascending */
static int cmp_start(const void *a, const void *b)
{
    const Thread *ta = (const Thread*)a;
    const Thread *tb = (const Thread*)b;
    if (ta->startTime != tb->startTime)
        return (int)ta->startTime - (int)tb->startTime;
    /* Tie-break by tid for determinism */
    return strcmp(ta->tid, tb->tid);
}

int main(int argc, char *argv[])
{
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    Thread* threads = NULL;
    int threadCount = -1;

    threadCount = readFile(argv[1], &threads);
    if (threadCount <= 0) {
        fprintf(stderr, "Failed to read file or no threads.\n");
        return 1;
    }

    total_threads = threadCount;

    /* Sort by start time so we create them in order */
    qsort(threads, threadCount, sizeof(Thread), cmp_start);

    sem_init(&sem_even, 0, 0);
    sem_init(&sem_odd,  0, 0);

    startClock();

    /* Create threads one by one, sleeping until each thread's startTime */
    for (int i = 0; i < threadCount; i++) {
        long now = getCurrentTime();
        long wait = (long)threads[i].startTime - now;
        if (wait > 0) sleep((unsigned int)wait);

        /* Mark all_started before creating the final thread */
        if (i == threadCount - 1) {
            pthread_mutex_lock(&mutex);
            all_started = 1;
            /*
             * If threads are already waiting and are now in starvation
             * (same parity, no opposite waiting), wake one up now so it
             * doesn't block forever.
             */
            int opp = (last_parity == 0) ? waiting_odd : waiting_even;
            int same = (last_parity == 0) ? waiting_even : waiting_odd;
            if (last_parity != -1 && opp == 0 && same > 0) {
                if (last_parity == 0) sem_post(&sem_even);
                else                   sem_post(&sem_odd);
            }
            pthread_mutex_unlock(&mutex);
        }

        pthread_create(&threads[i].handle, NULL, threadRun, &threads[i]);
    }

    /* Join all threads */
    for (int i = 0; i < threadCount; i++) {
        pthread_join(threads[i].handle, NULL);
    }

    sem_destroy(&sem_even);
    sem_destroy(&sem_odd);
    pthread_mutex_destroy(&mutex);
    free(threads);

    return threadCount;
}

void logStart(char* tID)
{
    printf("[%ld] New Thread with ID %s is started.\n", getCurrentTime(), tID);
}

void logFinish(char* tID)
{
    printf("[%ld] Thread with ID %s is finished.\n", getCurrentTime(), tID);
}

void startClock()
{
    programClock = time(NULL);
}

long getCurrentTime()
{
    time_t now;
    now = time(NULL);
    return now - programClock;
}