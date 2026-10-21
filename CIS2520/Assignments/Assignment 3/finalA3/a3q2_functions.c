#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include "a3q2_header.h"

/*
 * The downHeap function performs the downheap algorithm.
 * The key at the passed position is swapped with the key of it's child with the smallest value.
 * If the key is smaller, than both of it's children's keys then no swap happens.
 * After swap, the downHeap is called again to the new position of the swapped key and it terminates when it's value is smaller than both children.
*/
void downHeap(int key[20], int pos, int array[20][10]){
    while (isParent(pos)) {
        //left child position 
        int leftChild = 2 * pos + 1;
        //right child position
        int rightChild = 2 * pos + 2;
        //largest position
        int largest = pos;
        //check if the left child is larger than the parent
        if (leftChild < 20 && key[leftChild] > key[largest]) {
            largest = leftChild;
        }
        //check if the right child is larger than the parent
        if (rightChild < 20 && key[rightChild] > key[largest]) {
            largest = rightChild;
        }
        //if the largest is not the parent, swap the parent with the largest child
        if (largest != pos) {
            swap(pos, largest, key, array);
            pos = largest;
        } else {
            break;
        }
    }
}

// The isParent function returns true if the passed position has a child or false if there's no child
bool isParent(int keyPos){
    //true if the parent has a left child
    return (2 * keyPos + 1) < 20;
}

// The swap function swaps 2 rows in the 2D array and the key array. It receives the position of the 2 rows to be swapped, the key array, and the 2D array in its arguements.
void swap(int key1Pos, int key2Pos, int key[20], int array[20][10]){
  int tempKey = key[key1Pos];
  key[key1Pos] = key[key2Pos];
  key[key2Pos] = tempKey;

  for(int i = 0; i < 10; ++i){
    int temp = array[key1Pos][i];
    array[key1Pos][i] = array[key2Pos][i];
    array[key2Pos][i] = temp;
  }
}
