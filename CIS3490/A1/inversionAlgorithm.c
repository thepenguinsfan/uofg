#include <stdio.h>

void merge(int arr[], int left, int middle, int right, int *inversions);
void mergeSort(int arr[], int left, int right, int *inversions);
void printArray(int arr[], int size);


int main(int argc, char *argv[]){
    int inversions = 0;
    int arr[] = {20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1};
    int arr1[] = {4,-11, 19, -1, -4, -16, 20, 3, 5, 6, 14, -15, -8, -6, 7, -9, -20, -18, 0, -19};
    int arr2[] = {-10, 14, 11, -8, 18, 3, 10, 1, -7, -3, 15, -6, -4, 13, 0, 17, 2, 6, 7, -9};

    //get size of first array
    int n = sizeof(arr) / sizeof(arr[0]);
    //call mergeSort 
    mergeSort(arr, 0, n - 1, &inversions);
    //print number of inversions in first array
    printf("Number of inversions: %d\n", inversions);

    inversions = 0;
    //get size of second array
    n = sizeof(arr1) / sizeof(arr1[0]);
    //call mergeSort 
    mergeSort(arr1, 0, n - 1, &inversions);
    //print number of inversions in second array
    printf("Number of inversions: %d\n", inversions);

    inversions = 0;
    //get size of third array
    n = sizeof(arr2) / sizeof(arr2[0]);
    //call mergeSort 
    mergeSort(arr2, 0, n - 1, &inversions);
    //print number of inversions in third array
    printf("Number of inversions: %d\n", inversions);
}

//function to merge two subarrays
void merge(int arr[], int left, int middle, int right, int *inversions) {
    int num1 = middle - left + 1;
    int num2 = right - middle;
    //create temporary arrays
    int leftArr[num1], rightArr[num2];

    //copy data to temporary arrays
    for (int i = 0; i < num1; i++)
        leftArr[i] = arr[left + i];
    for (int j = 0; j < num2; j++)
        rightArr[j] = arr[middle + 1 + j];

    //merge the temporary arrays
    int i = 0, j = 0, k = left;
    while (i < num1 && j < num2) {
        //if left array element is <= right array element, place it in arr
        if (leftArr[i] <= rightArr[j]) {
            arr[k] = leftArr[i];
            i++;
            //else place right array element in arr
        } else {
            arr[k] = rightArr[j];
            //count the number of inversions
            //all remaining elements in leftArr are greater than rightArr[j]
            *inversions += (num1 - i);
            j++;
        }
        k++;
    }
    //copy remaining elements of leftArr and rightArr
    while (i < num1) {
        arr[k] = leftArr[i];
        i++;
        k++;
    }
    while (j < num2) {
        arr[k] = rightArr[j];
        j++;
        k++;
    }
}

//function to implement merge sort
void mergeSort(int arr[], int left, int right, int *inversions) {
    if (left < right) {
        int middle = left + (right - left) / 2;
        //sort first and second halves
        mergeSort(arr, left, middle, inversions);
        mergeSort(arr, middle + 1, right, inversions);
        //merge the sorted halves
        merge(arr, left, middle, right, inversions);
    }
}