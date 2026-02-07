! ============================================================================
! Program: iqsort
! Description: Interative Quicksort implementation
! Author: Luc Levesque
! ID: 1238403
! ============================================================================

    program iqsort
        use intIO
        use stackADT
        implicit none

        integer, allocatable :: array(:)
        integer :: num
        real :: startTime, endTime, executionTime

        call readUnsorted(array, num)

        call cpu_time(startTime)

        call iterativeQuickSort(array, num)

        call cpu_time(endTime)
        executionTime = endTime - startTime

        write(*, '(A, F10.4)') "Execution time: ", executionTime

        call writeSorted(array, num)

        deallocate(array)
    

    contains
        !iterative quicksort algorithm
        subroutine iterativeQuickSort(array, num)
            integer, intent(inout) :: array(:)
            integer, intent(in) :: num

            type(Stack) :: stck
            integer :: left, right, i ,j
            integer :: pivot, temp
            integer :: stackCapacity

            !calculate the stack capacity
            stackCapacity = ceiling(log(real(num)) / log(2.0)) + 10

            !initialize the stack
            call stackInit(stck, stackCapacity)

            !push the initial partition(entire array) onto the stack
            call push(stck, 1, num)

            !while the stack is not empty
            do while(.not. isEmpty(stck))

                !pop partition bounds from the stack
                call pop(stck, left, right)

                !process the current partition
                do while(left < right)
                    i = left
                    j = right
                    !calculate the pivot index
                    pivot = array((left + right) / 2)

                    !partition the array
                    do while(i <= j)
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
                    end do
                

                !determine the next partitions to process
                if (j - left < right - i) then
                    if(i < right) then
                        call push(stck, i, right)
                    end if
                    right = j

                else
                    if(j > left) then
                        call push(stck, left, j)
                    end if
                    left = i
                end if
                end do
            end do

            !delete the stack
            call stackDelete(stck)
        end subroutine iterativeQuickSort
    end program iqsort

