! Integer Input/Output Module

module intIO
    implicit none
    private

    public :: readUnsorted, writeSorted

    contains

    !read unsorted integers from a file
    ! dynamically allocate an array to store the integers

        subroutine readUnsorted(array, num)
            integer, allocatable, intent(out) :: array(:)
            integer, intent(out) :: num

            character(len=256) :: fileName
            integer :: fileUnit, ioStatus, numRead
            integer :: i, temp
            logical :: fileExists

            num = 0

            !prompt user until they enter a valid filename
            do
                write(*, '(A)', advance='no') "Enter filename: "
                read(*, '(A)') fileName

                fileName = trim(adjustl(fileName))
                inquire(file=fileName, exist=fileExists)

                if(fileExists) then
                    exit
                endif
                write(*, '(A)') "File does not exist. Please try again."
            end do

            !open file
            open(newunit=fileUnit, file=fileName, status='old', action='read', iostat=ioStatus)
            if(ioStatus /= 0) then 
                write(*, '(A)') "Error opening file. Please try again."
                return
            endif

            !count number of integers in the file
            numRead = 0
            do 
                read(fileUnit, *, iostat=ioStatus) temp
                if(ioStatus /= 0) exit
                numRead = numRead + 1
            end do

            rewind(fileUnit)

            !allocate array to store integers

            allocate(array(numRead), stat=ioStatus)
            if(ioStatus /= 0) then
                write(*, '(A)') "Error allocating array. Please try again."
                return
            endif

            !read integers from file
            do i = 1, numRead
                read(fileUnit, *, iostat=ioStatus) array(i)
                if(ioStatus /= 0) then
                    write(*, '(A)') "Error reading integers from file. Please try again."
                    deallocate(array)
                    close(fileUnit)
                    return
                endif
            end do

            !close file
            close(fileUnit)

            num = numRead

            write(*, '(A, I0, A)') "Successfully read ", numRead, " integers from file."
        end subroutine readUnsorted

    !write sorted integers to a file
        subroutine writeSorted(array, num)
            integer, intent(in) :: array(:)
            integer, intent(in) :: num

            character(len=256) :: fileName
            character(len=10) :: userChoice
            integer :: fileUnit, ioStatus
            integer :: i
            logical :: fileExists

            fileName = "sortedNUM.txt"
            inquire(file=fileName, exist=fileExists)

            if(fileExists) then    
                write(*, '(A)', advance='no') "File exists. Do you want to overwrite it? (y/n)"
                read(*, '(A)') userChoice 
                userChoice = trim(adjustl(userChoice))
                if(userChoice /= 'y' .and. userChoice /= "Y") then
                    write(*, '(A)') "Write cancelled."
                    return
                endif
            endif

            !open file for writing
            open(newunit=fileUnit, file=fileName, status='replace', action='write', iostat=ioStatus)


            if(ioStatus /= 0) then
                write(*, '(A)') "Error opening file. Please try again."
                return
            endif

            !write integers to file
            do i = 1, num   
                write(fileUnit, '(I0)', iostat=ioStatus) array(i)
                if(ioStatus /= 0) then
                    write(*, '(A)') "Error writing integers to file. Please try again."
                    close(fileUnit)
                    return
                endif
            end do

            !close file
            close(fileUnit)

            write(*, '(A, I0, A)') "Successfully wrote ", num, " integers to file."

        end subroutine writeSorted

end module intIO







