//Name: Luc Levesque
//Student ID: 1238403
//Due Date: February 8, 2026

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>

#define MAX_LINE_LENGTH 1000
#define MAX_COMMANDS 100
#define PIPE_READ 0
#define PIPE_WRITE 1
//function to write the ouput of commands 
void writeOutput(char* command, char* output) {
	printf("The output of: %s : is\n", command);
	printf(">>>>>>>>>>>>>>>\n%s<<<<<<<<<<<<<<<\n", output);	
}
//function to remove newline characters from the end of the line
void removeNewLine(char* line) {
    size_t length = strlen(line);
    while (length > 0 && (line[length - 1] == '\n' || line[length - 1] == '\r')) {
        line[length - 1] = '\0';
        length--;
    }
}

//function to parse commands
void parse(char* command, char ** args) {
    int i = 0;
    char *token = strtok(command, " ");
    while (token != NULL) {
        args[i++] = token;
        token = strtok(NULL, " ");
    }
    args[i] = NULL;
}


int main(int argc, char *argv[]) {


    if (argc != 2) {
        return 0;
    }

    //open the file with the provided filename
    FILE *file = fopen(argv[1], "r");
    if (file == NULL) {
        return 0;
    }

    //dynamically allocate memory for the commands
    char **commands = malloc(MAX_COMMANDS * sizeof(char*));
    if (commands == NULL) {
        fclose(file);
        return 0;
    }

    //read the commands from the file
    int count = 0;
    char line[MAX_LINE_LENGTH];
    while (fgets(line, sizeof(line), file) != NULL && count < MAX_COMMANDS) {
        //remove the newline character from the end of the line
        removeNewLine(line);

        //if line is nonempty, allocate memory for the command and copy the command to the commands array
        if (strlen(line) > 0) {
            commands[count] = (char*)malloc((strlen(line) + 1) * sizeof(char));
            if (commands[count] == NULL) {
                fclose(file);
                return 0;
            }

            strcpy(commands[count], line);
            count++;
        }
    }
    fclose(file);

    //execute the stored commands
    for (int i = 0; i < count; i ++) { 
        //create a pipe
        int pipefd[2];
        
        //check if the pipe was created successfully
        if(pipe(pipefd) == -1) {
            continue;
        }
        //fork the process
        pid_t pid = fork();

        //check if the fork failed
        if((pid == -1)) {
            close(pipefd[PIPE_READ]);
            close(pipefd[PIPE_WRITE]);
            continue;
        }
        //child process
        else if(pid == 0) {
            //close the read end of the pipe
            close(pipefd[PIPE_READ]);

            //redirect standard output and standard error to the pipe
            dup2(pipefd[PIPE_WRITE], STDOUT_FILENO);
            dup2(pipefd[PIPE_WRITE], STDERR_FILENO);
            close(pipefd[PIPE_WRITE]);

            //parse the command and execute
            char *args[MAX_LINE_LENGTH];
            char commandCopy[MAX_LINE_LENGTH];
            strcpy(commandCopy, commands[i]);
            parse(commandCopy, args);
            
            //replace the current process with the new process
            execvp(args[0], args);

            //if execvp fails
            exit(0);

        }
        //parent process
        else {
            //close the write end of the pipe
            close(pipefd[PIPE_WRITE]);

            //read from the read end of the pipe
            char buffer[MAX_LINE_LENGTH];
            char *output = (char*)malloc(1);
            output[0] = '\0';
            ssize_t bytesRead;
            size_t totalSize = 1;

            while((bytesRead = read(pipefd[PIPE_READ], buffer, sizeof(buffer) - 1)) > 0) {
                buffer[bytesRead] = '\0';
                totalSize += bytesRead;
                output = (char*)realloc(output, totalSize + 1);
                if (output == NULL) {
                    close(pipefd[PIPE_READ]);
                    exit(0);
                }
                strcat(output, buffer);
            }
            

            close(pipefd[PIPE_READ]);
            wait(NULL);
            writeOutput(commands[i], output);
            free(output);
        }

    }

    //free allocated command memory
    for (int i = 0; i < count; i++) {
        free(commands[i]);
    }
    free(commands);



    return 0;
}


