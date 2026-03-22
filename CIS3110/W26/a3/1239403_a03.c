#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <semaphore.h>
#include <sys/stat.h>
#include <time.h>

void logStart(char* tID);//function to log that a new thread is started
void logFinish(char* tID);//function to log that a thread has finished its time

void startClock();//function to start program clock
long getCurrentTime();//function to check current time since clock was started
time_t programClock;//the global timer/clock for the program

typedef struct thread //represents a single thread, you can add more members if required
{
	char tid[4];//id of the thread as read from file, set in readFile() for you
	unsigned int startTime;//start time of thread as read from file, set in readFile() for you
	int state;//you can use it as per your desire
	pthread_t handle;//you can use it as per your desire
	int retVal;//you can use it as per your desire
} Thread;

//semaphore for threads whose last digit is even
static sem_t semEven;      
//semaphore for threads whose last digit is odd
static sem_t semOdd;    
//mutex for synchronization
static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
//is set to parity of the last thread that ran: 0=even, 1=odd, -1=none
static int lastParity = -1; 
//is set to 1 after the last thread has been created
static int allStarted = 0; 
//number of even threads currently waiting on semEven
static int waitingEven = 0; 
//number of odd threads currently waiting on semOdd
static int waitingOdd = 0;  

void* threadRun(void* t);//the thread function, the code executed by each thread
int readFile(char* fileName, Thread** threads);//function to read the file content and build array of threads

int main(int argc, char *argv[])
{

    //check if command line arguments are valid
	if (argc < 2) {
		printf("Error: Usage: ./Assignment_3 <input_file>\n");
		return 1;
	}

	Thread* threads = NULL;//This is your list of threads, use it in suitable way; remove the comment when ready to use
	int threadCount = -1;

	//input file must be accepted as command line argument. You can write the suitable code here to check
	//command line arguments and read the content of the file using readFile().
	threadCount = readFile(argv[1], &threads);

	//check if file was read successfully
	if (threadCount <= 0) {
		printf("Error: Failed to read file or no threads.\n");
		return 1;
	}
	
	startClock();

    //write some suitable code here to initiate, progress and terminate the threads following the requirements

	//sort threads by startTime ascending so they are created in order
	for (int i = 0; i < threadCount - 1; i++) {
		for (int j = i + 1; j < threadCount; j++) {
			if (threads[j].startTime < threads[i].startTime || (threads[j].startTime == threads[i].startTime && strcmp(threads[j].tid, threads[i].tid) < 0)) {
				Thread temp = threads[i];
				threads[i] = threads[j];
				threads[j] = temp;
			}
		}	
	}
		
	sem_init(&semEven, 0, 0);
	sem_init(&semOdd,  0, 0);

	//create threads, sleeping until each thread's startTime
	for (int i = 0; i < threadCount; i++) {
		//wait until getCurrentTime() reaches the target startTime
		while (getCurrentTime() < (long)threads[i].startTime) {
			sleep(1);
		}

		//before creating last thread, set allStarted to 1
		//wake any stalled same-parity threads that can no longer be unblocked 
		if (i == threadCount - 1) {
			pthread_mutex_lock(&mutex);
			allStarted = 1;
			int opposite  = (lastParity == 0) ? waitingOdd  : waitingEven;
			int same = (lastParity == 0) ? waitingEven : waitingOdd;
			if (lastParity != -1 && opposite == 0 && same > 0) {
				if (lastParity == 0) {
					sem_post(&semEven);
				}
				else {
					sem_post(&semOdd);
				}
			}
			//unlock mutex
			pthread_mutex_unlock(&mutex);
		}
		//create thread
		pthread_create(&threads[i].handle, NULL, threadRun, &threads[i]);
	}

	//join all threads before exiting
	for (int i = 0; i < threadCount; i++) {
		pthread_join(threads[i].handle, NULL);
	}

	//destroy semaphores and mutex
	sem_destroy(&semEven);
	sem_destroy(&semOdd);
	pthread_mutex_destroy(&mutex);
	free(threads);

	return threadCount;
}

int readFile(char* fileName, Thread** threads)//implement this method as per your desire to read the thread information from file
{
	FILE *file = fopen(fileName, "r");
	if (file == NULL) {
		return -1;
	}

	//count non-empty lines to know how many threads to allocate
	int count = 0;
	char line[100];
	while (fgets(line, sizeof(line), file)) {
		if (line[0] != '\n' && line[0] != '\r') count++;
	}
	//reset
	rewind(file);

	*threads = (Thread*)malloc(count * sizeof(Thread));

	//check if threads is NULL
	if (!*threads) {
		fclose(file);
		return -1;
	}

	int i = 0;

	while (i < count && fgets(line, sizeof(line), file)) {

		int len = strlen(line);
		//remove newline and carriage return
		while (len > 0 && (line[len-1] == '\n' || line[len-1] == '\r')){
			line[--len] = '\0';
		}
		//if line is empty, continue
		if (len == 0){
			continue;
		}

		//ensure there is a semicolon
		char *semiColon = strchr(line, ';');
		if (!semiColon){
			continue;
		}
		*semiColon = '\0';

		strncpy((*threads)[i].tid, line, 3);
		(*threads)[i].tid[3] = '\0';
		(*threads)[i].startTime = (unsigned int) atoi (semiColon + 1);
		(*threads)[i].state = 0;
		(*threads)[i].retVal = 0;

		i++;

	}

	//close file
	fclose(file);

	//return number of threads read
	return i;
}

void logStart(char* tID)//do not change this method; you can use this method as per your desire
{
	printf("[%ld] New Thread with ID %s is started.\n", getCurrentTime(), tID);
}

void logFinish(char* tID)//do not change this method; you can use this method as per your desire
{
	printf("[%ld] Thread with ID %s is finished.\n", getCurrentTime(), tID);
}

void* threadRun(void* t)//implement this function in a suitable way
{
	logStart(((Thread*)t)->tid);

	//determine this thread's parity from the last digit of its ID
	int threadParity = (((Thread*)t)->tid[strlen(((Thread*)t)->tid) - 1] - '0') % 2;

	pthread_mutex_lock(&mutex);

	//add this thread to its respective waiting counter
	if (threadParity == 0){
		waitingEven++;
	}
	else{
		waitingOdd++;
	}

	//a thread may enter when: no thread has run yet, the last thread had the
	//opposite parity, or starvation avoidance applies (all threads started and
	//no opposite-parity threads are waiting)
	while (1) {
		int oppositeWaiting = (threadParity == 0) ? waitingOdd : waitingEven;
		int starvationOk = allStarted && (oppositeWaiting == 0);

		if (lastParity == -1 || lastParity != threadParity || starvationOk) {
			//allowed to enter critical section
			break; 
		}

		//release mutex and block on appropriate semaphore
		pthread_mutex_unlock(&mutex);
		if (threadParity == 0){
			sem_wait(&semEven);
		}
		else{
			sem_wait(&semOdd);
		}
		pthread_mutex_lock(&mutex);
	}

	//this thread is no longer waiting, decrement its respective counter
	if (threadParity == 0){
		waitingEven--;
	}
	else{
		waitingOdd--;
	}

	pthread_mutex_unlock(&mutex);

	//critical section starts here, it has only the following printf statement
	printf("[%ld] Thread %s is in its critical section\n",getCurrentTime(), ((Thread*)t)->tid);
	//critical section ends here


	pthread_mutex_lock(&mutex);

	lastParity = threadParity;

	//wake one thread of the opposite parity if any are waiting
	//if no opposite parity threads waiting, wake one of the same parity threads to avoid starvation
	int oppositeWaiting = (threadParity == 0) ? waitingOdd  : waitingEven;
	int sameWaiting = (threadParity == 0) ? waitingEven : waitingOdd;

	if (oppositeWaiting > 0) {
		if (threadParity == 0) {
			sem_post(&semOdd);
		}
		else{
			sem_post(&semEven);
		}
	} 
	else if (allStarted && sameWaiting > 0) {
		if (threadParity == 0) {
			sem_post(&semEven);
		}
		else{
			sem_post(&semOdd);
		}
	}

	pthread_mutex_unlock(&mutex);

	logFinish(((Thread*)t)->tid);
	return NULL;
}

void startClock()//do not change this method
{
	programClock = time(NULL);
}

long getCurrentTime()//invoke this method whenever you want to check how much time units passed
//since you invoked startClock()
{
	time_t now;
	now = time(NULL);
	return now-programClock;
}