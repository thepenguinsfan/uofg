/*
Name: Luc Levesque
Student ID: 1238403
Assignment 2, Question 1
*/

#include "q1.h"


/* function will display the primary user interface
    This is already done for you*/
void prompt(){
  printf("\n");
  printf("1. Add a new car to the available-for-rent list,\n");
  printf("2. Add a returned car to the available-for-rent list,\n");
  printf("3. Add a returned car to the repair list,\n");
  printf("4. Transfer a car from the repair list to the available-for-rent list,\n");
  printf("5. Rent the first available car,\n");
  printf("6. Print all the lists,\n");
  printf("7. Quit.\n");
  printf("Enter your choice: ");
  printf("\n");
}

/**
 * Inserts a new car into the linked list.
 * @param head Pointer to the head of the list.
 * @param plate Character array containing the car's license plate.
 * @param mileage Integer representing the car's mileage.
 * @param return_date Integer representing the car's return date, you can have -1 represent no return date.
 * @return Pointer to the newly added car in the list.
 */
struct car * insert_to_list(struct car ** head, char plate[], int mileage, int return_date){
    struct car *newCar = (struct car *)malloc(sizeof(struct car));
    if(newCar == NULL){
        printf("Failed to allocate memory for new car\n");
        return NULL;
    }
    //copy the plate number
    strncpy(newCar->plate, plate, sizeof(newCar->plate) - 1);
    //add null terminator
    newCar->plate[sizeof(newCar->plate) - 1] = '\0'; 
    //set the mileage and return date
    newCar->mileage = mileage;
    newCar->return_date = return_date;

    //set the next pointer to NULL
    newCar->next = NULL;

    //if the list is empty
    if(*head == NULL){
        *head = newCar;
    }
    else{
        //traverse to the end of the list
        struct car *temp = *head;
        while (temp->next != NULL) {
            temp = temp->next;
        }
        //insert at the end
        temp->next = newCar;
    }
    return newCar;
}

/**
 * Prints all the cars in the list.
 * @param head Pointer to the head of the list.
 * This function prints out the car details, it should not print return date if it does not exist.
 */
void print_list(struct car *head){
    //if list is empty
    if(head == NULL){
        printf("List is empty\n");
        return;
    }
    struct car *temp = head;
    //traverse the list and print each car
    while(temp != NULL){
        printf("Plate: %s, Mileage: %d", temp->plate, temp->mileage);
        if(temp->return_date != -1){
            date(temp->return_date);
        }
        printf("\n");  
        temp = temp->next;
    }
    return;
}

/**
 * Checks if a car's plate is already in the list.
 * @param head Pointer to the head of the list.
 * @param plate Character array containing the car's license plate.
 * @return Boolean value indicating if the plate is found.
 */
bool is_plate_in_list(struct car * head, char plate[]){
    struct car *temp = head;
    //initialize temp_plate and temp_plate2
    char temp_plate[8] = {0};
    char temp_plate2[8] = {0};

    //convert the plate to lowercase
    for(int i = 0; i < strlen(plate); i++){
        //if a letter, convert to lowercase
        if(isalpha(plate[i])){
            temp_plate[i] = tolower(plate[i]);
        }
        //else keep as is
        else{
            temp_plate[i] = plate[i];
        }
    }

    //traverse the list
    while(temp != NULL){
        //reset temp_plate2
        memset(temp_plate2, 0, sizeof(temp_plate2));
        //convert the current plate to lowercase
        for(int i = 0; i < strlen(temp->plate); i++){
            //if a letter, convert to lowercase
            if(isalpha(temp->plate[i])){
                temp_plate2[i] = tolower(temp->plate[i]);
            }
            //else keep as is
            else{
                temp_plate2[i] = temp->plate[i];
            }
        }

        //compare the lowercase plates
        if(strcmp(temp_plate2, temp_plate) == 0){
            return true;
        }
        temp = temp->next;
    }
    return false;
}

/**
 * Swaps two cars in the list.
 * @param a Pointer to the first car.
 * @param b Pointer to the second car.
 * Swaps the plate, mileage, and return date of two cars.
 */
void swap(struct car *a, struct car *b){
    struct car temp;
    temp = *a;
    *a = *b;
    *b = temp;

    //set new pointers
    temp.next = a->next;
    a->next = b->next;
    b->next = temp.next;
    return;
}

/**
 * Sorts the list of cars based on mileage or return date.
 * @param head Pointer to the head of the list.
 * @param sort_by_mileage Boolean to sort by mileage.
 * @param sort_by_return_date Boolean to sort by return date.
 */
void sort_list(struct car ** head, bool sort_by_mileage, bool sort_by_return_date){
    if(*head == NULL){
        return;
    }
    //bubble sort by mileage
    if(sort_by_mileage){
        struct car *current = *head;
        struct car *index = NULL;
        for(current = *head; current->next != NULL; current = current->next){
            for(index = current->next; index != NULL; index = index->next){
                if(current->mileage > index->mileage){
                    swap(current, index);
                }
            }
        }
    }
    //bubble sort by return date
    else if(sort_by_return_date){
        struct car *current = *head;
        struct car *index = NULL;
        for(current = *head; current->next != NULL; current = current->next){
            for(index = current->next; index != NULL; index = index->next){
                if(current->return_date > index->return_date){
                    swap(current, index);
                }
            }
        }
    }

    
    return;
}

/**
 * Removes a car from the list by its plate number.
 * @param head Pointer to the head of the list.
 * @param plate Character array containing the car's license plate.
 * @return Pointer to the removed car.
 */
struct car * remove_car_from_list(struct car **head, char plate[]){
    struct car *temp = *head;
    struct car *prev = NULL;
    //find the car based on the plate number
    while(temp!= NULL){
        if(strcmp(temp->plate, plate) == 0){
            //if the car is the first car
            if(prev == NULL){
                *head = temp->next;
            } else {
                prev->next = temp->next;
            }
            //return the removed car
            return temp;  
        }
        prev = temp;
        temp = temp->next;
    }
    return NULL;
}

/**
 * Removes the first car from the list.
 * @param head Pointer to the head of the list.
 * @return Pointer to the removed car.
 */
struct car * remove_first_from_list(struct car **head){
    if(*head == NULL){
        printf("List is empty\n");
        return NULL;
    }
    //remove the first car
    struct car *temp = *head;
    *head = (*head)->next;
    //return the removed car
    return temp;
}

/**
 * Calculates the profit based on the mileage difference.
 * @param initial_mileage Integer representing the car's starting mileage.
 * @param final_mileage Integer representing the car's final mileage.
 * @return Double value representing the calculated profit.
 */
double profit_calculator(int initial_mileage, int final_mileage){
    int mileageDifference = final_mileage - initial_mileage;
    //return a fix rate for up to 200 miles
    if(mileageDifference <= 200){
        return 80.00;
    }
    //return a rate of 0.15 for every mile after 200
    else{
        return 80.00 + (mileageDifference - 200) * 0.15;
    }
}

/**
 * Writes the list of cars to a file.
 * The file would be in the format: plate,mileage,return_date
 * @param filename Character array with the file name.
 * @param head Pointer to the head of the list.
 * Writes the details of each car in the list to a file.
 */
void write_list_to_file(char *filename, struct car *head){
    FILE *fptr = fopen(filename, "w");
    if(fptr == NULL){
        return;
    }
    struct car *temp = head;
    //write each car to the file
    while(temp != NULL){
        fprintf(fptr, "%s,%d,%d\n", temp->plate, temp->mileage, temp->return_date);
        temp = temp->next;
    }
    fclose(fptr);
    return;
}


/**
 * Reads car details from a file and adds them to the list.
 * The file would be in the format: plate,mileage,return_date
 * @param filename Character array with the file name.
 * @param head Pointer to the head of the list.
 * Reads data from the file and inserts each car into the list.
 */
void read_file_into_list(char *filename, struct car **head){
    FILE *fptr = fopen(filename, "r");
    if(fptr == NULL){
        return;
    }
    char line[100];
    
    char plate[7];
    int mileage;
    int returnDate;
    //read each line from the file
    while(fgets(line, sizeof(line), fptr) != NULL){
        sscanf(line, "%[^,],%d,%d", plate, &mileage, &returnDate);
        insert_to_list(head, plate, mileage, returnDate);
    }
    fclose(fptr);
    return;
}

/**
 * Prints the date in the format of year/month/day.
 * @param date Integer representing the date in YYMMDD format.
 */
void date(int date){
    int year = date / 10000;
    int month = (date % 10000) / 100;
    int day = date % 100;
    if(year < 10 && month < 10 && day < 10){
        printf(", Return Date: 0%d/0%d/0%d", year, month, day);
    }
    else if(year < 10 && month < 10){
        printf(", Return Date: 0%d/0%d/%d", year, month, day);
    }
    else if(year < 10 && day < 10){
        printf(", Return Date: 0%d/%d/0%d", year, month, day);
    }
    else if(month < 10 && day < 10){
        printf(", Return Date: %d/0%d/0%d", year, month, day);
    }
    else if(year < 10){
        printf(", Return Date: 0%d/%d/%d", year, month, day);
    }
    else if(month < 10){
        printf(", Return Date: %d/0%d/%d", year, month, day);
    }
    else if(day < 10){
        printf(", Return Date: %d/%d/0%d", year, month, day);
    }
    else{
        printf(", Return Date: %d/%d/%d", year, month, day);
    }
    return;
}

/**
 * Frees all the memory allocated for the cars in the list.
 * @param head Pointer to the head of the list.
 * Frees each car node in the list.
 */
void free_list(struct car ** head){
    struct car *temp = *head;
    while(temp != NULL){
        struct car *next = temp->next;
        free(temp);
        temp = next;
    }
    *head = NULL;

    return;
}

//get integer input from the user
int get_int(){
    int num;
    //loop until a valid integer is entered
    while(1){
        if(scanf(" %d", &num) == 1){
            break;
        }
        else{
            printf("Invalid input, please enter an integer: ");
            while(getchar() != '\n');
        }
    }
    return num;
}

//check if plate is valid
bool is_valid_plate(char plate[]){
    //check if the plate is between 2 and 7 characters
    if(strlen(plate) < 2 || strlen(plate) > 7 ){
        return false;
    }
    
    return true;
}

bool is_date_valid(int date){
    if(date > 999999 || date < 0){
        return false;
    }
    int year = date / 10000;
    int month = (date % 10000) / 100;
    int day = date % 100;
    if(year < 0 || year > 99){
        return false;
    }
    if(month < 1 || month > 12){
        return false;
    }
    if(day < 1 || day > 31){
        return false;
    }
    return true;
}