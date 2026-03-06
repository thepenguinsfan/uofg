//Name: Luc Levesque
//Student ID: 1238403

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <time.h>

//Datastructure definitions

typedef struct process //represents a single process
{
	char pid[4];
	unsigned int startTime;
	unsigned int lifeTime;
	unsigned int remainingTime;
} Process;

//ready queue (FIFO linked list)
typedef struct nodeQueue{
	int pIndex;
	struct nodeQueue* next;
}NodeQueue;

NodeQueue* queueHead = NULL;
NodeQueue* queueTail = NULL;

//currently running process index (-1 if none)
int runningIndex = -1;
//how many time units the current process has been running in this quantum
unsigned int quantumCounter = 0;


//Global variables
Process* processes = NULL; //the array to store processes read from the file
int processCount = 0; //count of total processes; update it after reading file
unsigned int timeQuantum;//This is the timeQuantum; initialize it based on the command line argument
time_t programClock;//the global timer/clock for the program


//Function prototypes
void logStart(char* pID);//function to log that a new process is started
void logRun(char* pID);//function to log that a process is entered into running state
void logEndQuantum(char* pID);//function to log that a process has finished its time quantum and going back to ready queue
void logFinish(char* pID);//function to log that a process is finished
void startClock();//function to start program clock
long getCurrentTime();//function to check current time since clock was started
int totalTime();//the function that computes total simulation time based on process list
void enqueue(int index);
int dequeue();

int scheduler();//the scheduler function; you have to implement this
int readFile(char* fileName);//function to read the file content and build array of processes; you have to implement this


int main(int argc, char *argv[])
{

    //check if command line arguments are valid
	if (argc < 3){
		printf("Error: Usage: ./Assignment_2 <timeQuantum> <inputFile>\n");
		return 1;
	}
	//set time quantum
	timeQuantum = (unsigned int)atoi(argv[1]);
	//read file
	if(readFile(argv[2]) != 0){
		printf("Error: Usage: ./Assignment_2 <timeQuantum> <inputFile>\n");
		return 1;
	}
    
	startClock();//do not remove this line
	time_t currentTime = -1;//do not remove this line
	
	while(getCurrentTime()<=totalTime())//this loop iterates for the total life of all processes, do not remove this line
	//do not add or change anything in this while loop
	//all you logic must be in the scheduler	
	{
		if(getCurrentTime()==currentTime+1)//this condition simulates the clock ticks and calls scheduler whenever local clock time progresses by 1
		{
			currentTime++;
			scheduler();
		}
	}

	return 0;
}

int readFile(char* fileName)//use this method in a suitable way to read file
{

	FILE *file= fopen(fileName, "r");
	if(file == NULL){
		return 1;
	}
	//count the number of lines in file
	int count = 0;
	char line[100];
	while(fgets(line, sizeof(line), file) != NULL){
		if(line[0] != '\n' && line[0] != '\r'){
			count++;
		}
	}
	//reset pointer
	rewind(file);
	//allocate array for processes
	processes = (Process*)malloc(count * sizeof(Process));
	if(processes == NULL){
		fclose(file);
		return 1;
	}

	int i = 0;

	while(fgets(line, sizeof(line), file) != NULL){
		int len = strlen(line);
		//remove newline and carriage return
		while (len > 0 && (line[len-1] == '\n' || line[len-1] == '\r')){
			line[--len] = '\0';
		}
		//if line is empty, continue
		if (len == 0){
			continue;
		}
		//parse line
		char pid[4];
		unsigned int startT;
		unsigned int lifeT;
		if(sscanf(line, "%[^;];%u;%u", pid, &startT, &lifeT) == 3){
			strncpy(processes[i].pid, pid, 3);
			processes[i].pid[3] = '\0';
			processes[i].startTime = startT;
			processes[i].lifeTime = lifeT;
			processes[i].remainingTime = lifeT;
			i++;
		}
	}
	processCount = i;
	fclose(file);
	
	return 0;
}

void logStart(char* pID)//invoke this method when you start a process
{
	printf("[%ld] New process with ID %s is arrived.\n", getCurrentTime(), pID);
}

void logFinish(char* pID)//invoke this method when a process is over
{
	printf("[%ld] process with ID %s is finished.\n", getCurrentTime(), pID);
}

void logEndQuantum(char* pID)//invoke this method when a process finishes its time quantum
{
	printf("[%ld] Process with ID %s finished its time quantum.\n", getCurrentTime(), pID);
}

void logRun(char* pID)//invoke this method when a process started its time quantum
{
	printf("[%ld] Process with ID %s started its time quantum.\n", getCurrentTime(), pID);
}

int totalTime()
{
	int largestTime = 0;
	for(int k=0; k<processCount; k++)
	{
		if(processes[k].lifeTime+processes[k].startTime > largestTime+processes[k].lifeTime)
			largestTime = processes[k].lifeTime+processes[k].startTime;
		else
			largestTime+=processes[k].lifeTime;
	}
	return largestTime;
}

int scheduler()//implement this function as per the given description
{
	long currentTime = getCurrentTime();
	//if there is a process currently running
	if(runningIndex != -1){
		//if the current process has finished its execution
		if(processes[runningIndex].remainingTime == 0){
			logFinish(processes[runningIndex].pid);
			runningIndex = -1;
			quantumCounter = 0;
			//log the arrival of the new process
			for(int i = 0; i < processCount; i++){
				if(processes[i].startTime == (unsigned int)currentTime){
					logStart(processes[i].pid);
				}
			}
			//enqueue the new process to the ready queue
			for(int i = 0; i < processCount; i++){
				if(processes[i].startTime == (unsigned int)currentTime){
					enqueue(i);
				}
			}
		}
		//if current process exceeded its time quantum
		else if(quantumCounter >= timeQuantum){
			int prevIndex = runningIndex;
			runningIndex = -1;
			quantumCounter = 0;
			//if the process finished exactly at the end of its time quantum
			if(processes[prevIndex].remainingTime == 0){
				logFinish(processes[prevIndex].pid);
				//log the arrival of the new process
				for(int i = 0; i < processCount; i++){
					if(processes[i].startTime == (unsigned int)currentTime){
						logStart(processes[i].pid);
					}
				}
				//enqueue the new process to the ready queue
				for(int i = 0; i < processCount; i++){
					if(processes[i].startTime == (unsigned int)currentTime){
						enqueue(i);
					}
				}
				//the process has not finished but exceeded its time quantum, goes back to ready queue
			} else {
				logEndQuantum(processes[prevIndex].pid);
				//log the start of the new process
				for(int i = 0; i < processCount; i++){
					if(processes[i].startTime == (unsigned int)currentTime){
						logStart(processes[i].pid);
					}
				}
				//enqueue the current process to the ready queue
				enqueue(prevIndex);
				//enqueue the new process to the ready queue
				for(int i = 0; i < processCount; i++){
					if(processes[i].startTime == (unsigned int)currentTime){
						enqueue(i);
					}
				}
			}
		}
		//if current process has not exceeded its time quantum
		else{
			//check if any new process has arrived
			//if so, log the arrival and enqueue the process
			for(int i = 0; i < processCount; i++){
				if(processes[i].startTime == (unsigned int)currentTime){
					logStart(processes[i].pid);
					enqueue(i);
				}
			}
			//running process consumes one unit of time
			quantumCounter++;
			//decrement the remaining time of the running process
			processes[runningIndex].remainingTime--;
			return 0;
		}
	}
	//no process currently running
	else{
		//log the arrival of 
		for(int i = 0; i < processCount; i++){
			if(processes[i].startTime == (unsigned int)currentTime){
				logStart(processes[i].pid);
			}
		}
		//enqueue the new process to the ready queue
		for(int i = 0; i < processCount; i++){
			if(processes[i].startTime == (unsigned int)currentTime){
				enqueue(i);
			}
		}
	}
	//dispatch the next process from the ready queue
	int nextIndex = dequeue();
	if (nextIndex != -1){
		runningIndex = nextIndex;
		//count this as the first unit of the time quantum
		quantumCounter = 1;
		processes[runningIndex].remainingTime--;
		logRun(processes[runningIndex].pid);
	}

	return 0;
}

void startClock()//do not change any code in this fucntions
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

//helper function to enqueue a process into the ready queue
void enqueue(int index){
	NodeQueue* newNode = (NodeQueue*)malloc(sizeof(NodeQueue));
	newNode->pIndex = index;
	newNode->next = NULL;
	if(queueHead == NULL){
		queueHead = newNode;
		queueTail = newNode;
	}else{
		queueTail->next = newNode;
		queueTail = newNode;
	}
}

//helper function to dequeue a process from the ready queue
int dequeue(){
	if(queueHead == NULL){
		return -1;
	}
	int index = queueHead->pIndex;
	NodeQueue* temp = queueHead;
	queueHead = queueHead->next;
	if(queueHead == NULL){
		queueTail = NULL;
	}
	free(temp);
	return index;
}