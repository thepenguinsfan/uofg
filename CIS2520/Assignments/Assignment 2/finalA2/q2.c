/*
Name: Luc Levesque
Student ID: 1238403
Assignment 2, Question 2
*/


#include "q2.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
int main(int argc, char** argv){

    //Pointer to the stack
    int i = 0;
    Operand * stack = NULL;
  
    //check if the number of command line arguements is correct
    if(argc != 2){
        printf("Invalid number of arguements\n");
        exit(1);
    }
    //Loop through the second command line arguement, containing the operands and the numbers 
    for(i = 0; i < strlen(argv[1]); ++i){
        char character = argv[1][i];
        //If the character is a digit
        if(isdigit(character)){
            double value = character - '0';
            push(&stack, value);
        }
        //perform the operation based on the character
        else if(character == '+'){
            double value1 = pop(&stack);
            double value2 = pop(&stack);
            push(&stack, value1 + value2);
        }
        else if(character == '-'){
            double value1 = pop(&stack);
            double value2 = pop(&stack);
            push(&stack, value2 - value1);
        }
        else if(character == '*'){
            double value1 = pop(&stack);
            double value2 = pop(&stack);
            push(&stack, value1 * value2);
        }
        else if(character == '/'){
            double value1 = pop(&stack);
            double value2 = pop(&stack);
            if(value1 == 0){
                printf("Cannot divide by zero\n");
                exit(1);
                }
            push(&stack, value2 / value1);
        }
        //exit if the character is invalid
        else{
            printf("Invalid character\n");
            exit(1);
        }
    }
    //print the result
    double result = pop(&stack);
    printf("Result: %.2f\n", result);
  

    return 0;
}