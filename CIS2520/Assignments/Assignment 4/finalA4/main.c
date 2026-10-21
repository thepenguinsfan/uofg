#include "graph.h"
/*
Name: Luc Levesque
Student ID: 1238403
Assignment 4
*/
/*
 * A basic menu based structure has been provided
 */

int main(int argc, char *argv[])
{

    if (argc != 2)
    {
        fprintf(stderr, "Usage: %s <filename>\n", argv[0]);
        return 1;
    }

    //read graph from file
    Graph *graph = readGraph(argv[1]);
    if (graph == NULL){
        return 1;
    }
    int choice;
    // int startVertex = 1;

    do
    {

        // Display the options
        prompt();

        // Get user input
        printf("Enter a Choice: ");
        scanf("%d", &choice);

        /*
         * You will need to add all the cases here.
         * Call the relevant functions in each case.
         * Make sure to handle invalid inputs or edge cases.
         */

        switch (choice) {
            case 1:
                displayAdjacencyList(graph);
                break;

            case 5:
                freeGraph(graph);
                break;
            default:
                printf("Invalid choice\n");
                break;
        }

    } while (choice != 5);

    return 0;
}
