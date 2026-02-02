! Luc Levesque
! 1238403
! Recursive Quicksort in Fortran    
    
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

            if(left < right) then
                i = left
                j = right
                pivot = array((left + right) / 2)

                do
                    do while(array(i) < pivot)
                        i = i + 1
                    end do
                    do while(array(j) > pivot)
                        j = j - 1
                    end do
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
                if (j > left) then
                    call recursiveQuickSort(array, left, j)
                end if
                if (i < right) then
                    call recursiveQuickSort(array, i, right)
                end if
            end if
        end subroutine recursiveQuickSort
    end program rqsort
                




