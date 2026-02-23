! ============================================================================
! Program: rqsort
! Description: Recursive Quicksort implementation
! Author: Luc Levesque
! ID: 1238403
! ============================================================================
    
    program rqsort
        use intIO
        implicit none

        integer, allocatable :: array(:)
        integer :: num
        real :: startTime, endTime, executionTime

        call readUnsorted(array, num)

        call cpu_time(startTime)

        call recursiveQuickSort(array, 1, num)

        call cpu_time(endTime)

        executionTime = endTime - startTime
        write(*, '(A, F10.4)') "Execution time: ", executionTime

        call writeSorted(array, num)

        deallocate(array)

    contains
        !recursive quicksort algorithm
        recursive subroutine recursiveQuickSort(array, left, right)
            integer, intent(inout) :: array(:)
            integer, intent(in) :: left, right
            integer :: i, j, pivot, temp
            
            !if left >= right, subarray is already sorted
            if(left < right) then
                i = left
                j = right
                pivot = array((left + right) / 2)

                !partition the array
                do
                    !find the first element greater than the pivot
                    do while(array(i) < pivot)
                        i = i + 1
                    end do
                    do while(array(j) > pivot)
                        j = j - 1
                    end do
                    !swap elements if necessary
                    if(i <= j) then 
                        temp = array(i)
                        array(i) = array(j)
                        array(j) = temp
                        i = i + 1
                        j = j - 1
                    end if
                    if (i > j) then
                        exit
                    end if
                    end do
                !recursively sort the left and right subarrays
                if (j > left) then
                    call recursiveQuickSort(array, left, j)
                end if
                if (i < right) then
                    call recursiveQuickSort(array, i, right)
                end if
            end if
        end subroutine recursiveQuickSort
    end program rqsort
                




