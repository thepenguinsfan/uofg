#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void maxDiscrepancyIndices(char* B, int n, int* result);

// Main function to test the algorithm
int main() {
    char B[] = "0011011100010101"; // Example binary string
    int n = strlen(B); // Correct length of the string
    int result[2];

    // Find the maximum discrepancy substring
    maxDiscrepancyIndices(B, n, result);

    // Print the result
    printf("Maximum discrepancy substring indices: (%d, %d)\n", result[0], result[1]);

    return 0;
}

// Function to find the maximum discrepancy substring
void maxDiscrepancyIndices(char* B, int n, int* result) {
    int max_discrepancy = 0;       // Stores the maximum discrepancy found so far
    int current_discrepancy = 0;  // Tracks the current running discrepancy
    int start_index = 0;          // Current potential start of substring
    int best_start = 0;           // Best start index of the substring
    int best_end = 0;             // Best end index of the substring

    for (int i = 0; i < n; i++) {
        // Treat '0' as +1 and '1' as -1
        int value = (B[i] == '0') ? 1 : -1;

        // Update current discrepancy
        current_discrepancy += value;

        // Check if the current discrepancy is the maximum found so far
        if (current_discrepancy > max_discrepancy) {
            max_discrepancy = current_discrepancy;
            best_start = start_index;
            best_end = i;
        }

        // Reset the current discrepancy if it goes negative
        if (current_discrepancy < 0) {
            current_discrepancy = 0;
            start_index = i + 1;
        }
    }

    // Store the result as 1-based indices
    result[0] = best_start + 1;
    result[1] = best_end + 1;
}