/*
Name: Luc Levesque
Student ID: 1238403
Assignment 2, Question 1
*/
#include "q1.h"

/*
 * A basic menu based structure has been provided
*/

int main(){

    //Our three Linked Lists
    struct car *available_head = NULL;
	struct car *rented_head = NULL;
	struct car *repair_head = NULL;

    char availableFile[] = "available.txt";
    char rentedFile[] = "rented.txt";
    char repairFile[] = "repair.txt";
    int choice;
    read_file_into_list(availableFile, &available_head);
    read_file_into_list(rentedFile, &rented_head);
    read_file_into_list(repairFile, &repair_head);
    
    
    
    do
    {
        prompt();
        choice = get_int();

        switch (choice)
        {
        //add new car to available list
        case 1:
            ;
            char plate[7];
            int mileage1;
            int return_date1 = -1;
            printf("Enter the plate number: ");
            scanf("%s", plate);
            //check if plate is valid
            while(!is_valid_plate(plate)){
                printf("Invalid plate number, please enter a valid plate number: ");
                scanf("%s", plate);
            }
            //check if plate already exists in any list
            if(is_plate_in_list(available_head, plate)){
                printf("Car already in available list\n");
                break;
            }
            if(is_plate_in_list(rented_head, plate)){
                printf("Car already in rented list\n");
                break;
            }
            if(is_plate_in_list(repair_head, plate)){
                printf("Car already in repair list\n");
                break;
            }

            printf("Enter the mileage: ");
            mileage1 = get_int();
            //insert the car and sort the list
            insert_to_list(&available_head, plate, mileage1, return_date1);
            sort_list(&available_head, true, false);
            break;
            
        //add returned car to available list
        case 2:
            ;
            char plate2[7];
            int mileage2;
            int return_date2 = -1;
            double charge2;
            printf("Enter the plate number: ");
            scanf("%s", plate2);
            //check if plate is valid
            while(!is_valid_plate(plate2)){
                printf("Invalid plate number, please enter a valid plate number: ");
                scanf("%s", plate2);
            }
            //check if car is in rented list
            if(!is_plate_in_list(rented_head, plate2)){
                printf("Car not found in rented list\n");
                break;
            }

            printf("Enter the mileage: ");
            mileage2 = get_int();
            
            struct car *temp2 = remove_car_from_list(&rented_head, plate2);
            if(temp2 == NULL){
                printf("Car not found in rented list\n");
                break;
            }
            //check if mileage is logical
            while(mileage2 < temp2->mileage){
                printf("Invalid mileage, must be greater than previous mileage\n");
                printf("Enter the mileage: ");
                mileage2 = get_int();
            }
            //insert the car and sort the lists
            insert_to_list(&available_head, temp2->plate, mileage2, return_date2);
            sort_list(&available_head, true, false);
            sort_list(&rented_head, false, true);
            //calculate the charge
            charge2 = profit_calculator(temp2->mileage, mileage2);
            printf("Charge: %.2f\n", charge2);
            free(temp2);
            break;

        //add returned car to repair list
        case 3:
            ;
            char plate3[7];
            int mileage3;
            int return_date3 = -1;
            double charge3;
            printf("Enter the plate number: ");
            scanf("%s", plate3);
            //check if plate is valid
            while(!is_valid_plate(plate3)){
                printf("Invalid plate number, please enter a valid plate number: ");
                scanf("%s", plate3);
            }
            //check if car is in rented list
            if(!is_plate_in_list(rented_head, plate3)){
                printf("Car not found in rented list\n");
                break;
            }
            printf("Enter the mileage: ");
            mileage3 = get_int();

            struct car *temp3 = remove_car_from_list(&rented_head, plate3);
            if(temp3 == NULL){
                printf("Car not found in rented list\n");
                break;
            }
            //check if mileage is logical
            while(mileage3 < temp3->mileage){
                printf("Invalid mileage, must be greater than previous mileage\n");
                printf("Enter the mileage: ");
                mileage3 = get_int();
            }
            //insert the car 
            insert_to_list(&repair_head, temp3->plate, mileage3, return_date3);
            //calculate the charge
            charge3 = profit_calculator(temp3->mileage, mileage3);
            printf("Charge: %.2f\n", charge3);
            free(temp3);
            break;

        //transfer car from repair to available
        case 4:
            ;
            char plate4[7];
            printf("Enter the plate number: ");
            scanf("%s", plate4);
            //check if plate is valid
            while(!is_valid_plate(plate4)){
                printf("Invalid plate number, please enter a valid plate number: ");
                scanf("%s", plate4);
            }
            //check if car is in repair list
            if(!is_plate_in_list(repair_head, plate4)){
                printf("Car not found in repair list\n");
            }
            struct car *temp4 = remove_car_from_list(&repair_head, plate4);
            if(temp4 == NULL){
                printf("Car not found in repair list\n");
                break;
            }
            //insert the car and sort the list
            insert_to_list(&available_head, temp4->plate, temp4->mileage, temp4->return_date);
            sort_list(&available_head, true, false);
            free(temp4);
            break;

        //rent first available car
        case 5:
            ;
            int return_date5;
            printf("Enter the return date: ");
            return_date5 = get_int();
            //check if date is valid
            while(!is_date_valid(return_date5)){
                printf("Invalid date, please enter a valid date: ");
                return_date5 = get_int();
            }
            struct car *temp5 = remove_first_from_list(&available_head);
            if(temp5 == NULL){
                printf("No cars available\n");
                break;
            }
            //update the return date
            temp5->return_date = return_date5;
            //insert the car and sort the list
            insert_to_list(&rented_head, temp5->plate, temp5->mileage, temp5->return_date);
            sort_list(&rented_head, false, true);
            free(temp5);
            break;

        //print all lists
        case 6:
            printf("Available Cars\n");
            print_list(available_head);
            printf("Rented Cars\n");
            print_list(rented_head);
            printf("Repair Cars\n");
            print_list(repair_head);
            break;

        //quit
        case 7:
            write_list_to_file(availableFile, available_head);
            write_list_to_file(rentedFile, rented_head);
            write_list_to_file(repairFile, repair_head);
            free_list(&available_head);
            free_list(&rented_head);
            free_list(&repair_head);
            printf("Exiting Program\n");
            exit(0);
            break;

        default:
            printf("Invalid choice\n");
            break;
        }
        
    } while (true);
    

    return 0;
}