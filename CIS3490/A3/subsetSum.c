#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

int subsetSumRecursive(int arr[], int i, int sumTarget);
int subsetSumDP(int arr[], int i, int sumTarget);

int main() {
    //array 
    int arr[] = {1,2,4,5,7,8,11,13,14,15,21,55,99,111,128,200,210,230,300,333};

    int i = sizeof(arr) / sizeof(arr[0]);
    int sumTarget = 444;

    clock_t start, end;
    double cpu_time_used;

    //measure the time needed for recursive approach
    start = clock();
    int recursiveResult = subsetSumRecursive(arr, i, sumTarget);
    end = clock();
    cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
    printf("Recursive approach: %d subsets, Time: %f seconds\n", recursiveResult, cpu_time_used);

    //measure the time needed for DP approach
    start = clock();
    int dpResult = subsetSumDP(arr, i, sumTarget);
    end = clock();
    cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
    printf("DP approach: %d subsets, Time: %f seconds\n", dpResult, cpu_time_used);

    return 0;
}


int subsetSumRecursive(int arr[], int i, int sumTarget){
    //base case: if no elements left then return 0 (no subsets possible)
    if(i == 0){
        return 0;
    }

    //if last element is greater than sumTarget, ignore it and recurse on the rest of the array
    if(arr[i-1] > sumTarget){
        return subsetSumRecursive(arr, i-1, sumTarget);
    }

    //if the last element is equal to the sumTarget, then there is 1 subset, recurse on the rest of the array
    if(arr[i-1] == sumTarget){
        return 1 + subsetSumRecursive(arr, i-1, sumTarget);
    }
    //else count the number of subsets that exclude/include the last element
    return subsetSumRecursive(arr, i-1, sumTarget) + subsetSumRecursive(arr, i-1, sumTarget - arr[i-1]);
}

int subsetSumDP(int arr[], int i, int sumTarget){
    int T[i+1][sumTarget+1];
    //initialize the dp table with 0
    memset(T, 0, sizeof(T));
    
    //if the sum target is 0, there is only 1 subset(empty set)
    for (int j = 0; j <= i; j++) {
        T[j][0] = 1;
    }

    //fill the dp table
    for(int j = 1; j <= i; j++){
        for(int k = 1; k <= sumTarget; k++){
            //if current element is greater than the sum target, ignore it
            if(arr[j-1] > k){
                T[j][k] = T[j-1][k];
            } 
            //else count the number of subsets that exclude/include the current element
            else{
                T[j][k] = T[j-1][k] + T[j-1][k-arr[j-1]];
            }
        }
    }
    return T[i][sumTarget];
}