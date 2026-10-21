#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "a3q1_header.h"


Variable variables[10];
int varCount = 0; 


// The createNode function allocates memory to the tree and creates a new node using the given data before returning the node.
Node* createNode(char *data){
	Node * newNode = (Node*)malloc(sizeof(Node));
	strcpy(newNode->data, data);
	newNode->left = NULL;
	newNode->right = NULL;
	return newNode;
}

// The parseExpression function parses the expression string passed in from command line, stores the information in a new node, and returns the root node of the tree.
Node* parseExpression(char *expr) {
	if(expr == NULL){
		printf("No expression provided.\n");
		return NULL;
	}

	Node* newNode;
	char currentChar = expr[0];
	char operation;
	int count = 0;
	//if the expression is enclosed in parentheses
	if(currentChar == '('){
		for(int i = 1; i < strlen(expr)-1; i++){
			//counting the number of parentheses
			if(expr[i] == '('){
				count++;
			}
			else if(expr[i] == ')'){
				count--;
			}
			//if count is 0, then operation is found
			else{
				if(count == 0){
					//if the character is an operator, set the operation
					if(expr[i] == '+' || expr[i] == '-' || expr[i] == '*' || expr[i] == '/'){
						operation = expr[i];
					}
					//if the character is not an operator, set operation to null
					else{
						operation = '\0';
					}
					//if the operation is not null, create node
					if(operation != '\0'){
						char op[2] = {operation, '\0'};
						newNode = createNode(op);
						//set left node
						int leftLength = i - 1;
						//calculate the length of the left expression
                		char *leftExpression = (char *)malloc(leftLength + 1);
                		strncpy(leftExpression, expr + 1, leftLength);
                		leftExpression[leftLength] = '\0';
						//parse the left expression
						newNode->left = parseExpression(leftExpression);
						//set right node
						int rightLength = strlen(expr) - i - 2;
						//calculate the length of the right expression
                		char *rightExpression = (char *)malloc(rightLength + 1);
                		strncpy(rightExpression, expr + i + 1, rightLength);
                		rightExpression[rightLength] = '\0';
						//parse the right expression
						newNode->right = parseExpression(rightExpression);
						//free the memory
						free(leftExpression);
						free(rightExpression);

						return newNode;
					}
				}
			}
		}
	//if the expression is not enclosed in parentheses
	}else if(currentChar != '\0'){
		//if the expression is a number or a variable
		if(isValidFloat(expr) || isalpha(currentChar)){
			newNode = createNode(expr);
			return newNode;
		}
		else{
			//if the character is not a float or a variable
			printf("Invalid expression.\n");
			exit(1);
		}
	}
	return NULL;
}



// The preOrder function prints tree nodes in preorder traversal.
void preorder(Node *root){
	if(root != NULL){
		printf("%s ", root->data);
		preorder(root->left);
		preorder(root->right);
	}
}


// The inOrder function prints tree nodes in inorder traversal fully parenthesized.
void inorder(Node *root){
    if(root != NULL) {
        printf("(");
        inorder(root->left);
        printf("%s", root->data);
        inorder(root->right);
        printf(")");
    }
}

// The postOrder function prints tree nodes in postorder traversal.
void postorder(Node *root){
	if(root != NULL){
		postorder(root->left);
		postorder(root->right);
		printf("%s ", root->data);
	}
}

// The promptVariables function prompts the user to assign values to each variable found in the expression tree. The values should be stored in the Variables struct.
void promptVariables(Node *root){
	if(root != NULL){
		if(isalpha(root->data[0])){
			int i;
			//check if the variable exists
			for(i = 0; i < varCount; i++){
				if(strcmp(variables[i].varName, root->data) == 0){
					break;
				}
			}
			//add variable if it does not exist
			if(i == varCount){
				char varValue[10];
				strcpy(variables[varCount].varName, root->data);
				printf("Enter value for %s: ", root->data);
				scanf("%s", varValue);
				//validate the value
				if(isValidFloat(varValue)){
					variables[varCount].value = atof(varValue);
				}
				else{
					printf("Invalid value.\n");
					exit(1);
				}
				varCount++;
			}
		}
		//recursively prompt for left and right nodes
		promptVariables(root->left);
		promptVariables(root->right);
	}
}

// The calculate function calculates the expression and returns its result. Division by 0 and/or other error scenarios should be checked.
float calculate(Node *root){
	if(root != NULL){
		//if node is a number
		if(isdigit(root->data[0])){
			return atof(root->data);
		}
		//if node is a variable
		else if(isalpha(root->data[0])){
			for(int i = 0; i < varCount; i++){
				if(strcmp(variables[i].varName, root->data) == 0){
					return variables[i].value;
				}
			}
		}
		else{
			//recursively calculate left and right nodes
			float left = calculate(root->left);
			float right = calculate(root->right);
			if(strcmp(root->data, "+") == 0){
				return left + right;
			}
			else if(strcmp(root->data, "-") == 0){
				return left - right;
			}
			else if(strcmp(root->data, "*") == 0){
				return left * right;
			}
			else if(strcmp(root->data, "/") == 0){
				//check for division by 0
				if(right == 0){
					printf("Error: Division by 0.\n");
					return 0;
				}
				return left / right;
			}
		}
	}
	return 0;
}

// The freeTree function frees the memory allocated for the tree.
void freeTree(Node *root){
	if(root != NULL){
		freeTree(root->left);
		freeTree(root->right);
		free(root);
	}
}


// The isValidFloat function checks if the given string is a valid float number.
int isValidFloat(char *str){
	//check if the string is null
	if(str == NULL){
		return 0;
	}
	//check if the string length is correct
	if (strlen(str) != 4){
		return 0;
	}
	//check if the string is a float
	if(!isdigit(str[0])){
		return 0;
	}
	if(str[1] != '.'){
		return 0;
	}
	if(!isdigit(str[2])){
		return 0;
	}
	if(!isdigit(str[3])){
		return 0;
	}
	return 1;
}