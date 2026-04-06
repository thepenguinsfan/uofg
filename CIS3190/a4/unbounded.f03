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
        !read operation from user
        write(*, '(A)', advance='no') "> Enter an operation: + - * / or ! (q to quit): "
        read(*, '(A)') operation
        operation = trim(adjustl(operation))

        !dispatch to the appropriate handler
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

    !return true if str is a valid integer
    logical function isValidNumber(str)
        character(len=*), intent(in) :: str
        integer :: i, startPos
        character(len=len(str)) :: trimmed

        isValidNumber = .false.
        trimmed = trim(adjustl(str))

        if(len_trim(trimmed) == 0) return

        !skip optional leading sign
        startPos = 1
        if(trimmed(1:1) == '-' .or. trimmed(1:1) == '+') then
            startPos = 2
            if(len_trim(trimmed) < 2) return
        end if

        !reject if any character is not a digit
        do i = startPos, len_trim(trimmed)
            if(trimmed(i:i) < '0' .or. trimmed(i:i) > '9') return
        end do

        isValidNumber = .true.
    end function isValidNumber

    !prompt user until a valid integer is entered
    subroutine readOperand(prompt, num)
        character(len=*), intent(in) :: prompt
        type(BigInt), intent(out) :: num
        character(len=1024) :: input

        !keep prompting until a valid number is entered
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

    !read two operands and perform the given arithmetic operation
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

        !print result and free memory
        write(*, '(A)', advance='no') "> The result is: "
        call listPrint(result)
        write(*, *)

        call listDelete(num1)
        call listDelete(num2)
        call listDelete(result)
    end subroutine handleArithmetic

    !read n and compute n!
    subroutine handleFactorial()
        character(len=1024) :: input
        integer :: n, i, ioStatus
        type(BigInt) :: factResult, multiplier, temp

        write(*, '(A)', advance='no') "> Enter a non-negative integer (up to 42): "
        read(*, '(A)') input
        input = trim(adjustl(input))

        !validate n is a non-negative integer up to 42
        read(input, *, iostat=ioStatus) n
        if(ioStatus /= 0 .or. n < 0 .or. n > 42) then
            write(*, '(A)') "Invalid input. Please enter a non-negative integer up to 42."
            return
        end if

        !start accumulator at 1
        call listFromString(factResult, "1")

        !multiply accumulator by each integer from 2 to n
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

    !convert a native integer to a BigInt
    subroutine bigIntFromInt(num, val)
        type(BigInt), intent(out) :: num
        integer, intent(in) :: val
        character(len=20) :: str

        !format val as a string then parse into a BigInt
        write(str, '(I0)') val
        call listFromString(num, trim(str))
    end subroutine bigIntFromInt

    !add absolute values of two BigInts
    subroutine addAbsolute(a, b, res)
        type(BigInt), intent(in) :: a, b
        type(BigInt), intent(out) :: res
        type(Node), pointer :: nodeA, nodeB
        integer :: digitA, digitB, sumDigit, carry

        !initialize result
        nullify(res%head)
        res%sign = 1
        carry = 0

        nodeA => a%head
        nodeB => b%head

        !add digit pairs from least to most significant
        do while(associated(nodeA) .or. associated(nodeB) .or. carry /= 0)
            digitA = 0
            digitB = 0

            if(associated(nodeA)) then
                digitA = nodeA%digit
                nodeA => nodeA%next
            end if

            if(associated(nodeB)) then
                digitB = nodeB%digit
                nodeB => nodeB%next
            end if

            sumDigit = digitA + digitB + carry
            carry = sumDigit / 10
            call appendNode(res, mod(sumDigit, 10))
        end do

        call listTrimLeadingZeros(res)
    end subroutine addAbsolute

    !subtract absolute values, assumes |a| >= |b|
    subroutine subtractAbsolute(a, b, res)
        type(BigInt), intent(in) :: a, b
        type(BigInt), intent(out) :: res
        type(Node), pointer :: nodeA, nodeB
        integer :: digitA, digitB, diff, borrow

        !initialize result
        nullify(res%head)
        res%sign = 1
        borrow = 0

        nodeA => a%head
        nodeB => b%head

        !subtract digit pairs from least to most significant
        do while(associated(nodeA) .or. associated(nodeB))
            digitA = 0
            digitB = 0

            if(associated(nodeA)) then
                digitA = nodeA%digit
                nodeA => nodeA%next
            end if

            if(associated(nodeB)) then
                digitB = nodeB%digit
                nodeB => nodeB%next
            end if

            diff = digitA - digitB - borrow
            !borrow from the next digit if result is negative
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

    !add two signed BigInts
    subroutine bigAdd(a, b, res)
        type(BigInt), intent(in) :: a, b
        type(BigInt), intent(out) :: res
        integer :: comparison

        !same sign: add magnitudes; opposite sign: subtract the smaller from the larger
        if(a%sign == b%sign) then
            call addAbsolute(a, b, res)
            res%sign = a%sign
        else
            comparison = listCompareAbs(a, b)
            if(comparison == 0) then
                call listCreate(res)
            else if(comparison > 0) then
                call subtractAbsolute(a, b, res)
                res%sign = a%sign
            else
                call subtractAbsolute(b, a, res)
                res%sign = b%sign
            end if
        end if

        if(listIsZero(res)) res%sign = 1
    end subroutine bigAdd

    !subtract two signed BigInts
    subroutine bigSubtract(a, b, res)
        type(BigInt), intent(in) :: a, b
        type(BigInt), intent(out) :: res
        type(BigInt) :: negB

        !negate b and add, reusing the addition logic
        call listCopy(negB, b)
        call listNegate(negB)
        call bigAdd(a, negB, res)
        call listDelete(negB)
    end subroutine bigSubtract

    !multiply a BigInt by a single digit
    subroutine multiplySingleDigit(a, d, res)
        type(BigInt), intent(in) :: a
        integer, intent(in) :: d
        type(BigInt), intent(out) :: res
        type(Node), pointer :: nodeA
        integer :: prod, carry

        nullify(res%head)
        res%sign = 1
        carry = 0

        nodeA => a%head
        do while(associated(nodeA))
            prod = nodeA%digit * d + carry
            carry = prod / 10
            call appendNode(res, mod(prod, 10))
            nodeA => nodeA%next
        end do

        !append any remaining carry
        if(carry > 0) then
            call appendNode(res, carry)
        end if

        call listTrimLeadingZeros(res)
    end subroutine multiplySingleDigit

    !prepend n zero digits to multiply by 10^n
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

    !multiply two signed BigInts 
    subroutine bigMultiply(a, b, res)
        type(BigInt), intent(in) :: a, b
        type(BigInt), intent(out) :: res
        type(BigInt) :: partial, newSum
        type(Node), pointer :: nodeB
        integer :: position

        !accumulate partial products for each digit of b
        call listCreate(res)
        position = 0

        nodeB => b%head
        do while(associated(nodeB))
            if(nodeB%digit /= 0) then
                call multiplySingleDigit(a, nodeB%digit, partial)
                call shiftLeft(partial, position)

                call addAbsolute(res, partial, newSum)
                call listDelete(res)
                call listCopy(res, newSum)
                call listDelete(newSum)
                call listDelete(partial)
            end if

            position = position + 1
            nodeB => nodeB%next
        end do

        !set result sign and handle negative zero
        res%sign = a%sign * b%sign
        if(listIsZero(res)) res%sign = 1
    end subroutine bigMultiply

    !divide two signed BigInts
    subroutine bigDivide(a, b, res)
        type(BigInt), intent(in) :: a, b
        type(BigInt), intent(out) :: res
        type(BigInt) :: remainder, absoluteA, absoluteB
        type(BigInt) :: digitTimesB, newRemainder
        integer :: lengthA, i
        integer, allocatable :: digitsA(:)
        type(Node), pointer :: nodeA
        integer :: low, high, middle
        type(BigInt) :: middleTimesB
        integer :: comparison
        character(len=1024) :: quotientStr
        integer :: quotientPos

        !dividend is zero, return zero
        if(listIsZero(a)) then
            call listCreate(res)
            return
        end if

        !copy a and b to absoluteA and absoluteB and set sign to positive
        call listCopy(absoluteA, a)
        absoluteA%sign = 1
        call listCopy(absoluteB, b)
        absoluteB%sign = 1

        !extract digits of a most significant first
        lengthA = listLength(absoluteA)
        allocate(digitsA(lengthA))
        nodeA => absoluteA%head
        do i = 1, lengthA
            digitsA(i) = nodeA%digit
            nodeA => nodeA%next
        end do

        !perform long division digit by digit
        call listFromString(remainder, "0")
        quotientStr = ' '
        quotientPos = 1

        do i = lengthA, 1, -1
            !bring down the next digit into the remainder
            call shiftLeft(remainder, 1)
            remainder%head%digit = digitsA(i)
            call listTrimLeadingZeros(remainder)

            !binary search for the largest quotient digit that fits
            low = 0
            high = 9
            do while(low < high)
                middle = (low + high + 1) / 2
                call multiplySingleDigit(absoluteB, middle, middleTimesB)
                comparison = listCompareAbs(middleTimesB, remainder)
                if(comparison <= 0) then
                    low = middle
                else
                    high = middle - 1
                end if
                call listDelete(middleTimesB)
            end do

            quotientStr(quotientPos:quotientPos) = char(low + ichar('0'))
            quotientPos = quotientPos + 1

            !subtract the quotient digit times divisor from remainder
            if(low > 0) then
                call multiplySingleDigit(absoluteB, low, digitTimesB)
                call subtractAbsolute(remainder, digitTimesB, newRemainder)
                call listDelete(remainder)
                call listCopy(remainder, newRemainder)
                call listDelete(newRemainder)
                call listDelete(digitTimesB)
            end if
        end do

        !build result from quotient string and apply sign
        call listFromString(res, trim(quotientStr))
        res%sign = a%sign * b%sign
        if(listIsZero(res)) res%sign = 1

        !free memory
        deallocate(digitsA)
        call listDelete(remainder)
        call listDelete(absoluteA)
        call listDelete(absoluteB)
    end subroutine bigDivide

end program unbounded
