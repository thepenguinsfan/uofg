! ============================================================================
! Module: dynllist
! Description: Dynamic linked list for storing unbounded integers
! Author: Luc Levesque
! ID: 1238403
! ============================================================================

module dynllist
    implicit none
    private

    public :: Node, BigInt, listCreate, listDelete, listPrint, listToString, &
              listFromString, listCopy, listLength, listIsZero, listNegate, &
              listCompareAbs, listTrimLeadingZeros, appendNode

    type :: Node
        integer :: digit
        type(Node), pointer :: next => null()
    end type Node

    !stores sign and pointer to head of digit list (least significant first)
    type :: BigInt
        integer :: sign = 1
        type(Node), pointer :: head => null()
    end type BigInt

    contains

        !create a new BigInt initialized to zero
        subroutine listCreate(num)
            type(BigInt), intent(out) :: num

            num%sign = 1
            nullify(num%head)
            call prependNode(num, 0)
        end subroutine listCreate

        !deallocate all nodes in the list
        subroutine listDelete(num)
            type(BigInt), intent(inout) :: num
            type(Node), pointer :: current, temp

            current => num%head
            do while(associated(current))
                temp => current
                current => current%next
                deallocate(temp)
            end do
            nullify(num%head)
            num%sign = 1
        end subroutine listDelete

        !deep copy src into dst
        subroutine listCopy(dst, src)
            type(BigInt), intent(out) :: dst
            type(BigInt), intent(in) :: src
            type(Node), pointer :: current

            nullify(dst%head)
            dst%sign = src%sign

            current => src%head
            do while(associated(current))
                call appendNode(dst, current%digit)
                current => current%next
            end do
        end subroutine listCopy

        !prepend a digit node at the head (used to build least-significant first)
        subroutine prependNode(num, digit)
            type(BigInt), intent(inout) :: num
            integer, intent(in) :: digit
            type(Node), pointer :: newNode

            allocate(newNode)
            newNode%digit = digit
            newNode%next => num%head
            num%head => newNode
        end subroutine prependNode

        !append a digit node at the tail
        subroutine appendNode(num, digit)
            type(BigInt), intent(inout) :: num
            integer, intent(in) :: digit
            type(Node), pointer :: newNode, current

            allocate(newNode)
            newNode%digit = digit
            nullify(newNode%next)

            if(.not. associated(num%head)) then
                num%head => newNode
            else
                current => num%head
                do while(associated(current%next))
                    current => current%next
                end do
                current%next => newNode
            end if
        end subroutine appendNode

        !count the number of nodes
        integer function listLength(num)
            type(BigInt), intent(in) :: num
            type(Node), pointer :: current

            listLength = 0
            current => num%head
            do while(associated(current))
                listLength = listLength + 1
                current => current%next
            end do
        end function listLength

        !check if the BigInt represents zero
        logical function listIsZero(num)
            type(BigInt), intent(in) :: num
            type(Node), pointer :: current

            listIsZero = .true.
            current => num%head
            do while(associated(current))
                if(current%digit /= 0) then
                    listIsZero = .false.
                    return
                end if
                current => current%next
            end do
        end function listIsZero

        !negate a BigInt
        subroutine listNegate(num)
            type(BigInt), intent(inout) :: num

            if(.not. listIsZero(num)) then
                num%sign = -num%sign
            end if
        end subroutine listNegate

        !remove leading zeros (trailing nodes since list is least-significant first)
        subroutine listTrimLeadingZeros(num)
            type(BigInt), intent(inout) :: num
            type(Node), pointer :: current, prev, last_nonzero
            integer :: len

            len = listLength(num)
            if(len <= 1) return

            !find last non-zero node by traversing and tracking it
            nullify(last_nonzero)
            current => num%head
            do while(associated(current))
                if(current%digit /= 0) then
                    last_nonzero => current
                end if
                current => current%next
            end do

            !if all zeros, keep just one
            if(.not. associated(last_nonzero)) then
                current => num%head%next
                do while(associated(current))
                    prev => current
                    current => current%next
                    deallocate(prev)
                end do
                nullify(num%head%next)
                num%sign = 1
                return
            end if

            !free everything after last_nonzero
            current => last_nonzero%next
            nullify(last_nonzero%next)
            do while(associated(current))
                prev => current
                current => current%next
                deallocate(prev)
            end do
        end subroutine listTrimLeadingZeros

        !compare absolute values: returns -1, 0, or 1
        integer function listCompareAbs(a, b)
            type(BigInt), intent(in) :: a, b
            integer :: lenA, lenB
            type(Node), pointer :: curA, curB
            integer :: i
            integer, allocatable :: digitsA(:), digitsB(:)

            lenA = listLength(a)
            lenB = listLength(b)

            if(lenA > lenB) then
                listCompareAbs = 1
                return
            else if(lenA < lenB) then
                listCompareAbs = -1
                return
            end if

            !same length: compare from most significant digit
            allocate(digitsA(lenA), digitsB(lenB))

            curA => a%head
            do i = 1, lenA
                digitsA(i) = curA%digit
                curA => curA%next
            end do

            curB => b%head
            do i = 1, lenB
                digitsB(i) = curB%digit
                curB => curB%next
            end do

            !compare from last element (most significant) to first
            listCompareAbs = 0
            do i = lenA, 1, -1
                if(digitsA(i) > digitsB(i)) then
                    listCompareAbs = 1
                    deallocate(digitsA, digitsB)
                    return
                else if(digitsA(i) < digitsB(i)) then
                    listCompareAbs = -1
                    deallocate(digitsA, digitsB)
                    return
                end if
            end do

            deallocate(digitsA, digitsB)
        end function listCompareAbs

        !convert a string representation to a BigInt
        !digits are stored least significant first in the linked list
        subroutine listFromString(num, str)
            type(BigInt), intent(out) :: num
            character(len=*), intent(in) :: str
            integer :: i, startPos, d
            character(len=len(str)) :: trimmed

            nullify(num%head)
            num%sign = 1
            trimmed = adjustl(str)

            startPos = 1
            if(trimmed(1:1) == '-') then
                num%sign = -1
                startPos = 2
            else if(trimmed(1:1) == '+') then
                startPos = 2
            end if

            !build list from least significant to most significant
            do i = len_trim(trimmed), startPos, -1
                d = ichar(trimmed(i:i)) - ichar('0')
                call appendNode(num, d)
            end do

            !handle empty input
            if(.not. associated(num%head)) then
                call appendNode(num, 0)
            end if

            call listTrimLeadingZeros(num)

            if(listIsZero(num)) then
                num%sign = 1
            end if
        end subroutine listFromString

        !convert a BigInt to its string representation
        subroutine listToString(num, str)
            type(BigInt), intent(in) :: num
            character(len=*), intent(out) :: str
            integer :: len, pos, i
            type(Node), pointer :: current
            integer, allocatable :: digits(:)

            len = listLength(num)
            allocate(digits(len))

            current => num%head
            do i = 1, len
                digits(i) = current%digit
                current => current%next
            end do

            str = ' '
            pos = 1

            if(num%sign < 0 .and. .not. listIsZero(num)) then
                str(pos:pos) = '-'
                pos = pos + 1
            end if

            !write digits from most significant to least significant
            do i = len, 1, -1
                str(pos:pos) = char(digits(i) + ichar('0'))
                pos = pos + 1
            end do

            deallocate(digits)
        end subroutine listToString

        !print a BigInt to standard output
        subroutine listPrint(num)
            type(BigInt), intent(in) :: num
            character(len=1024) :: str

            call listToString(num, str)
            write(*, '(A)') trim(str)
        end subroutine listPrint

end module dynllist
