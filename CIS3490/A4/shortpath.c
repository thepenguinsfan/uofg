#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAXX 100
#define MAXY 100

#define WALL '#'
#define PATH '.'
#define START 'S'
#define CHARGER 'C'

int dX[4] = {1, -1, 0, 0};
int dY[4] = {0, 0, -1, 1};

typedef struct {
    int x;
    int y;
    int distance;
} Point;

char grid[MAXX][MAXY];
int distance[MAXX][MAXY];
int rows, columns;

int bfs(int startX, int startY, int chargerX, int chargerY) {
    Point queue[MAXX * MAXY];
    int front = 0;
    int rear = 0;

    //initialize the distance array
    for (int i = 1; i <= rows; i++) {
        for (int j = 1; j <= columns; j++) {
            distance[i][j] = -1;
        }
    }

    //initialize the queue with the starting position
    queue[rear++] = (Point){startX, startY, 0};
    distance[startX][startY] = 0;
    int minSolution = 999999;

    while (front < rear) {
        Point current = queue[front++];

        //return the minimum distance to the charger
        if (current.x == chargerX && current.y == chargerY) {
            if(current.distance < minSolution) {
                minSolution = current.distance;
            }
        }

        //check all four directions
        for (int i = 0; i < 4; i++) {
            int newX = current.x;
            int newY = current.y;
            int tilesMoved = 0;

            //move in a direction until a wall is hit
            while (1) {
                int nextX = newX + dX[i];
                int nextY = newY + dY[i];
                //check the bounds and if the next tile is a wall
                if (nextX <= 1 || nextX >= rows || nextY <= 1 || nextY >= columns || grid[nextX][nextY] == WALL) {
                    break;
                }
                newX = nextX;
                newY = nextY;
                tilesMoved++;
            }
            //if we moved at least one tile, check if we can add the new position to the queue
            if (tilesMoved > 0) {
                int newDistance = current.distance + tilesMoved;
                
                if (distance[newX][newY] == -1 || newDistance < distance[newX][newY]) {
                    distance[newX][newY] = newDistance;
                    queue[rear++] = (Point){newX, newY, newDistance};
                    //printf("Roomba travelled %d tiles %c to (%d,%d), total distance: %d\n", tilesMoved, dir, newX, newY, newDistance);
                }
            }
        }
    }

    return minSolution;
}

int main() {
    FILE *fp = fopen("ROOMS.txt", "r");
    if (fp == NULL) {
        printf("Error opening file\n");
        return 1;
    }

    int testCases = 4;

    for (int t = 0; t < testCases; t++) {
        printf("\n Test Case %d \n", t);
        
        //parse the grid dimensions
        fscanf(fp, "%d %d", &rows, &columns);
        
        //initialize the starting and charger positions
        int startX = -1, startY = -1, chargerX = -1, chargerY = -1;
        
        //parse the grid
        for (int i = 1; i <= rows; i++) {
            fscanf(fp, "%s", &grid[i][1]);
            for (int j = 1; j <= columns; j++) {
                if (grid[i][j] == START) {
                    startX = i;
                    startY = j;
                } else if (grid[i][j] == CHARGER) {
                    chargerX = i;
                    chargerY = j;
                }
            }
        }

        //DEBUG STATEMENTS
        //print grid to make sure it was parsed correctly
        printf("Grid %d (%dx%d):\n", t, rows, columns);
        for (int i = 1; i <= rows; i++) {
            printf("%s\n", &grid[i][1]);
        }
        //print start and charger positions
        printf("Start: (%d,%d), Charger: (%d,%d)\n", startX, startY, chargerX, chargerY);


        //run BFS to find the minimum distance
        int result = bfs(startX, startY, chargerX, chargerY);
        printf("Minimum tiles: %d\n", result);
    }

    fclose(fp);
    return 0;
}