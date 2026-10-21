#include "graph.h"
/*
Name: Luc Levesque
Student ID: 1238403
Assignment 4
*/
/* function will display the primary user interface
    This is already done for you*/
void prompt(void)
{

    printf("\nMenu:\n");
    printf("1. Display Adjacency List\n");
    printf("2. Perform Breadth-First Search (BFS)\n");
    printf("3. Perform Depth-First Search (DFS)\n");
    printf("4. Find Shortest Path using Dijkstra's Algorithm\n");
    printf("5. Exit\n");
}

/**
 * Reads a graph from a file and constructs the graph structure.
 * @param filename The name of the file containing the adjacency matrix.
 * @return Pointer to the created Graph structure, or NULL if an error occurs.
 */
Graph *readGraph(const char *filename)
{
    //read file
    FILE *file = fopen(filename, "r");
    if (file == NULL)
    {
        printf("Error opening file\n");
        return NULL;
    }
    //allocate memory for graph
    Graph *graph = (Graph *)malloc(sizeof(Graph));
    if (graph == NULL)
    {
        printf("Error allocating memory\n");
        return NULL;
    }

    //determine number of vertices
    int numVertices = 0;
    char line[100];
    while (fgets(line, sizeof(line), file))
    {
        numVertices++;
    }
    //go back to the beginning of the file
    rewind(file);
    //set number of vertices
    graph->numVertices = numVertices;

    //read adjacency matrix
    for(int i = 0; i < numVertices; i++)
    {
        for(int j = 0; j < numVertices; j++){
            //read integer from file
            fscanf(file, "%d", &graph->adjMatrix[i][j]);
        }
        //set adjacency list to NULL
        graph->adjList[i] = NULL;
    }

    fclose(file);   
    createAdjacencyList(graph);

    return graph;
}

/**
 * Creates a new node for the adjacency list.
 * @param vertex The vertex number that this node will represent.
 * @return Pointer to the newly created Node structure.
 */
Node *createNode(int vertex)
{
    //create new node
    Node *newNode = (Node *)malloc(sizeof(Node));
    //if memory allocation fails
    if(newNode == NULL)
    {
        printf("Error allocating memory\n");
        return NULL;
    }
    //set vertex and next pointer
    newNode->vertex = vertex;
    newNode->next = NULL;
    return newNode;
}

/**
 * Displays the adjacency list of the graph.
 * @param graph Pointer to the Graph structure.
 */
void displayAdjacencyList(Graph *graph)
{
    printf("Adjacency List:\n");
    for(int i = 0; i < graph->numVertices; i++){
        printf("Vertex %d: ", i+1);
        //print adjacency list for each vertex
        Node *currentNode = graph->adjList[i];
        while(currentNode != NULL){
            //print vertex and weight
            printf(" -> %d (%d)", currentNode->vertex + 1, graph->adjMatrix[i][currentNode->vertex]);
            currentNode = currentNode->next;
        }
        printf(" NULL\n");
    }
}

/**
 * Converts the adjacency matrix of the graph to an adjacency list.
 * @param graph Pointer to the Graph structure.
 */
void createAdjacencyList(Graph *graph)
{
    for(int i = 0; i < graph->numVertices; i++){
        //create pointer to last node
        Node *lastNode = NULL;
        //iterate through adjacency matrix
        for(int j = 0; j < graph->numVertices; j++){
            //if an edge exists
            if(graph->adjMatrix[i][j] != 0){
                //create new node
                Node *newNode = createNode(j);
                //if first node
                if(graph->adjList[i] == NULL){
                    graph->adjList[i] = newNode;
                }
                //if not first node
                else{
                    lastNode->next = newNode;
                }
                //set last node to new node
                lastNode = newNode;
            }
        }
    }
}

/**
 * Performs Breadth-First Search (BFS) starting from a given vertex.
 * @param graph Pointer to the Graph structure.
 * @param startVertex The vertex from which BFS starts (0-based index).
 */
void bfs(Graph *graph, int startVertex)
{
    // Implement the function logic here
}

/**
 * Performs Depth-First Search (DFS) starting from a given vertex.
 * @param graph Pointer to the Graph structure.
 * @param startVertex The vertex from which DFS starts (0-based index).
 */
void dfs(Graph *graph, int startVertex)
{
    // Implement the function logic here
}

/**
 * Finds the shortest path from the start vertex to all other vertices using Dijkstra's algorithm.
 * @param graph Pointer to the Graph structure.
 * @param startVertex The vertex from which to start the algorithm (0-based index).
 */
void dijkstra(Graph *graph, int startVertex)
{
    // Implement the function logic here
}

/**
 * Frees the memory allocated for the graph, including the adjacency list.
 * @param graph Pointer to the Graph structure to be freed.
 */
void freeGraph(Graph *graph)
{
    //if no graph exists
    if(graph == NULL){
        printf("Nothing to free\n");
    }
    //free adjacency list
    for(int i = 0; i < graph->numVertices; i++){
        //free each node in the adjacency list
        Node *currentNode = graph->adjList[i];
        //travel through the list
        while(currentNode != NULL){
            //set temp node to current node
            Node *temp = currentNode;
            //move to next node
            currentNode = currentNode->next;
            //free current node
            free(temp);
        }
        //set adjacency list to NULL
        graph->adjList[i] = NULL;
    }
    //free graph
    free(graph);
}
