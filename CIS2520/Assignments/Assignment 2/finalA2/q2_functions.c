/*
Name: Luc Levesque
Student ID: 1238403
Assignment 2, Question 2
*/

#include "q2.h"
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
/**
 * Pushes a new value onto the stack.
 * 
 * @param stack Pointer to the pointer of the stack's head.
 * @param value The value to be added to the stack.
 */
void push(Operand **stack, double value){
    Operand *newNode = (Operand *)malloc(sizeof(Operand));
    if(newNode == NULL){
        printf("Memory allocation failed\n");
        exit(1);
    }
    newNode->value = value;
    //link new node to the head of the stack
    newNode->next = *stack;
    //update the head of the stack
    *stack = newNode;
}


/**
 * Pops the top value from the stack and returns it.
 * 
 * @param stack Pointer to the pointer of the stack's head.
 * @return The value that was at the top of the stack.
 */
double pop(Operand **stack){
    if(*stack == NULL){
        printf("Stack is empty\n");
        exit(1);
    }
    
    Operand *temp = *stack;
    double value = temp->value;
    //update the head of the stack
    *stack = (*stack)->next;
    //free popped node
    free(temp);
    return value;
}


/**
 * Prints all values in the stack.
 * 
 * @param stack Pointer to the stack's head.
 */
void print(Operand *stack){
  while(stack != NULL){
    printf("%.2f\n", stack->value);
    stack = stack->next;
  }
}