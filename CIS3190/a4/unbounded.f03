! ============================================================================
! Program: unbounded
! Description: Unbounded integer arithmetic using dynamic linked lists
! Author: Luc Levesque
! ID: 1238403
! ============================================================================

program unbounded
    use dynllist
    implicit none

    character(len=10) :: operation
    type(BigInt) :: num1, num2, result
    logical :: running

    running = .true.

    do while(running)
        !prompt for operation
        write(*, '(A)', advance='no') "> Enter an operation: + - * / or ! (q to quit): "
        read(*, '(A)') operation
        operation = trim(adjustl(operation))

        select case(operation(1:1))
        case('q', 'Q')
            running = .false.
            cycle

        case('!')
            call handleFactorial()

        case('+', '-', '*', '/')
            call handleArithmetic(operation(1:1))

        case default
            write(*, '(A)') "Invalid operation. Please enter +, -, *, /, !, or q."
        end select
    end do

contains

    !validate that a string represents a valid integer
    logical function isValidNumber(str)
        character(len=*), intent(in) :: str
        integer :: i, startPos
        character(len=len(str)) :: trimmed

        isValidNumber = .false.
        trimmed = trim(adjustl(str))

        if(len_trim(trimmed) == 0) return

        startPos = 1
        if(trimmed(1:1) == '-' .or. trimmed(1:1) == '+') then
            startPos = 2
            if(len_trim(trimmed) < 2) return
        end if

        do i = startPos, len_trim(trimmed)
            if(trimmed(i:i) < '0' .or. trimmed(i:i) > '9') return
        end do

        isValidNumber = .true.
    end function isValidNumber

    !read and validate a number from user input
    subroutine readOperand(prompt, num)
        character(len=*), intent(in) :: prompt
        type(BigInt), intent(out) :: num
        character(len=1024) :: input

        do
            write(*, '(A)', advance='no') prompt
            read(*, '(A)') input
            input = trim(adjustl(input))

            if(isValidNumber(input)) then
                call listFromString(num, input)
                exit
            else
                write(*, '(A)') "Invalid number. Please enter digits only (optional leading +/-)."
            end if
        end do
    end subroutine readOperand

    !handle +, -, *, / operations
    subroutine handleArithmetic(op)
        character(len=1), intent(in) :: op

        call readOperand("> Enter first operand: ", num1)
        call readOperand("> Enter second operand: ", num2)

        select case(op)
        case('+')
            call bigAdd(num1, num2, result)
        case('-')
            call bigSubtract(num1, num2, result)
        case('*')
            call bigMultiply(num1, num2, result)
        case('/')
            if(listIsZero(num2)) then
                write(*, '(A)') "Error: Division by zero."
                call listDelete(num1)
                call listDelete(num2)
                return
            end if
            call bigDivide(num1, num2, result)
        end select

        write(*, '(A)', advance='no') "> The result is: "
        call listPrint(result)
        write(*, *)

        call listDelete(num1)
        call listDelete(num2)
        call listDelete(result)
    end subroutine handleArithmetic

    !handle the factorial operation
    subroutine handleFactorial()
        character(len=1024) :: input
        integer :: n, i, ioStatus
        type(BigInt) :: factResult, multiplier, temp

        write(*, '(A)', advance='no') "> Enter a non-negative integer (up to 42): "
        read(*, '(A)') input
        input = trim(adjustl(input))

        read(input, *, iostat=ioStatus) n
        if(ioStatus /= 0 .or. n < 0 .or. n > 42) then
            write(*, '(A)') "Invalid input. Please enter a non-negative integer up to 42."
            return
        end if

        !0! = 1, 1! = 1
        call listFromString(factResult, "1")

        do i = 2, n
            call bigIntFromInt(multiplier, i)
            call bigMultiply(factResult, multiplier, temp)
            call listDelete(factResult)
            call listCopy(factResult, temp)
            call listDelete(temp)
            call listDelete(multiplier)
        end do

        write(*, '(A)', advance='no') "> The result is: "
        call listPrint(factResult)
        write(*, *)

        call listDelete(factResult)
    end subroutine handleFactorial

    !create a BigInt from a native integer
    subroutine bigIntFromInt(num, val)
        type(BigInt), intent(out) :: num
        integer, intent(in) :: val
        character(len=20) :: str

        write(str, '(I0)') val
        call listFromString(num, trim(str))
    end subroutine bigIntFromInt

    !addition of absolute values (both assumed positive)
    subroutine addAbsolute(a, b, res)
        type(BigInt), intent(in) :: a, b
        type(BigInt), intent(out) :: res
        type(Node), pointer :: curA, curB
        integer :: digitA, digitB, sumDigit, carry

        nullify(res%head)
        res%sign = 1
        carry = 0

        curA => a%head
        curB => b%head

        do while(associated(curA) .or. associated(curB) .or. carry /= 0)
            digitA = 0
            digitB = 0

            if(associated(curA)) then
                digitA = curA%digit
                curA => curA%next
            end if

            if(associated(curB)) then
                digitB = curB%digit
                curB => curB%next
            end if

            sumDigit = digitA + digitB + carry
            carry = sumDigit / 10
            call appendNode(res, mod(sumDigit, 10))
        end do

        call listTrimLeadingZeros(res)
    end subroutine addAbsolute

    !subtraction of absolute values: computes |a| - |b| where |a| >= |b|
    subroutine subtractAbsolute(a, b, res)
        type(BigInt), intent(in) :: a, b
        type(BigInt), intent(out) :: res
        type(Node), pointer :: curA, curB
        integer :: digitA, digitB, diff, borrow

        nullify(res%head)
        res%sign = 1
        borrow = 0

        curA => a%head
        curB => b%head

        do while(associated(curA) .or. associated(curB))
            digitA = 0
            digitB = 0

            if(associated(curA)) then
                digitA = curA%digit
                curA => curA%next
            end if

            if(associated(curB)) then
                digitB = curB%digit
                curB => curB%next
            end if

            diff = digitA - digitB - borrow
            if(diff < 0) then
                diff = diff + 10
                borrow = 1
            else
                borrow = 0
            end if

            call appendNode(res, diff)
        end do

        call listTrimLeadingZeros(res)
    end subroutine subtractAbsolute

    !signed addition
    subroutine bigAdd(a, b, res)
        type(BigInt), intent(in) :: a, b
        type(BigInt), intent(out) :: res
        integer :: cmp

        if(a%sign == b%sign) then
            call addAbsolute(a, b, res)
            res%sign = a%sign
        else
            cmp = listCompareAbs(a, b)
            if(cmp == 0) then
                call listCreate(res)
            else if(cmp > 0) then
                call subtractAbsolute(a, b, res)
                res%sign = a%sign
            else
                call subtractAbsolute(b, a, res)
                res%sign = b%sign
            end if
        end if

        if(listIsZero(res)) res%sign = 1
    end subroutine bigAdd

    !signed subtraction
    subroutine bigSubtract(a, b, res)
        type(BigInt), intent(in) :: a, b
        type(BigInt), intent(out) :: res
        type(BigInt) :: negB

        call listCopy(negB, b)
        call listNegate(negB)
        call bigAdd(a, negB, res)
        call listDelete(negB)
    end subroutine bigSubtract

    !multiply a BigInt by a single digit (0-9)
    subroutine multiplySingleDigit(a, d, res)
        type(BigInt), intent(in) :: a
        integer, intent(in) :: d
        type(BigInt), intent(out) :: res
        type(Node), pointer :: curA
        integer :: prod, carry

        nullify(res%head)
        res%sign = 1
        carry = 0

        curA => a%head
        do while(associated(curA))
            prod = curA%digit * d + carry
            carry = prod / 10
            call appendNode(res, mod(prod, 10))
            curA => curA%next
        end do

        if(carry > 0) then
            call appendNode(res, carry)
        end if

        call listTrimLeadingZeros(res)
    end subroutine multiplySingleDigit

    !shift left by n positions (multiply by 10^n)
    subroutine shiftLeft(num, n)
        type(BigInt), intent(inout) :: num
        integer, intent(in) :: n
        integer :: i
        type(Node), pointer :: newNode

        if(listIsZero(num)) return

        do i = 1, n
            allocate(newNode)
            newNode%digit = 0
            newNode%next => num%head
            num%head => newNode
        end do
    end subroutine shiftLeft

    !signed multiplication using grade-school algorithm
    subroutine bigMultiply(a, b, res)
        type(BigInt), intent(in) :: a, b
        type(BigInt), intent(out) :: res
        type(BigInt) :: partial, newSum
        type(Node), pointer :: curB
        integer :: position

        call listCreate(res)
        position = 0

        curB => b%head
        do while(associated(curB))
            if(curB%digit /= 0) then
                call multiplySingleDigit(a, curB%digit, partial)
                call shiftLeft(partial, position)

                call addAbsolute(res, partial, newSum)
                call listDelete(res)
                call listCopy(res, newSum)
                call listDelete(newSum)
                call listDelete(partial)
            end if

            position = position + 1
            curB => curB%next
        end do

        res%sign = a%sign * b%sign
        if(listIsZero(res)) res%sign = 1
    end subroutine bigMultiply

    !signed integer division (truncated toward zero)
    subroutine bigDivide(a, b, res)
        type(BigInt), intent(in) :: a, b
        type(BigInt), intent(out) :: res
        type(BigInt) :: remainder, absA, absB
        type(BigInt) :: digitTimesB, newRemainder
        integer :: lenA, i
        integer, allocatable :: digitsA(:)
        type(Node), pointer :: curA
        integer :: lo, hi, mid
        type(BigInt) :: midTimesB
        integer :: cmp
        character(len=1024) :: quotientStr
        integer :: qpos

        !handle division of zero
        if(listIsZero(a)) then
            call listCreate(res)
            return
        end if

        call listCopy(absA, a)
        absA%sign = 1
        call listCopy(absB, b)
        absB%sign = 1

        !extract digits of a from most significant to least significant
        lenA = listLength(absA)
        allocate(digitsA(lenA))
        curA => absA%head
        do i = 1, lenA
            digitsA(i) = curA%digit
            curA => curA%next
        end do

        !long division
        call listFromString(remainder, "0")
        quotientStr = ' '
        qpos = 1

        do i = lenA, 1, -1
            !shift remainder left and add next digit
            call shiftLeft(remainder, 1)

            !set the least significant digit to the current digit
            remainder%head%digit = digitsA(i)
            call listTrimLeadingZeros(remainder)

            !binary search for the quotient digit (0-9)
            lo = 0
            hi = 9
            do while(lo < hi)
                mid = (lo + hi + 1) / 2
                call multiplySingleDigit(absB, mid, midTimesB)
                cmp = listCompareAbs(midTimesB, remainder)
                if(cmp <= 0) then
                    lo = mid
                else
                    hi = mid - 1
                end if
                call listDelete(midTimesB)
            end do

            quotientStr(qpos:qpos) = char(lo + ichar('0'))
            qpos = qpos + 1

            !subtract lo * absB from remainder
            if(lo > 0) then
                call multiplySingleDigit(absB, lo, digitTimesB)
                call subtractAbsolute(remainder, digitTimesB, newRemainder)
                call listDelete(remainder)
                call listCopy(remainder, newRemainder)
                call listDelete(newRemainder)
                call listDelete(digitTimesB)
            end if
        end do

        call listFromString(res, trim(quotientStr))
        res%sign = a%sign * b%sign
        if(listIsZero(res)) res%sign = 1

        deallocate(digitsA)
        call listDelete(remainder)
        call listDelete(absA)
        call listDelete(absB)
    end subroutine bigDivide

end program unbounded
