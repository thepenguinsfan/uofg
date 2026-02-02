! Stack Abstract Data Type

module stackADT
    implicit none
    private

    public :: Stack, push, pop, isEmpty, isFull, stackInit, stackDelete, stackClear
   
    !left and right end of the stck
    type :: bounds
        integer :: left
        integer :: right
    end type bounds

    type :: Stack   
        type(bounds), allocatable :: elements(:)
        integer :: topElement
        integer :: maxElements
    end type Stack

    contains
        ! Initialize the stck with a given maximum number of elements
        subroutine stackInit(stck, maxElements)
            type (Stack), intent(inout) :: stck
            integer, intent(in) :: maxElements

            stck%maxElements = maxElements
            stck%topElement = 0

            allocate(stck%elements(maxElements))

        end subroutine stackInit

        ! deallocate the stck and all its elements
        subroutine stackDelete(stck)
            type (Stack), intent(inout) :: stck

            if (allocated(stck%elements)) then
                deallocate(stck%elements)
            endif
            stck%topElement = 0
            stck%maxElements = 0
        end subroutine stackDelete

        !clear stck with no deallocation
        subroutine stackClear(stck)
            type (Stack), intent(inout) :: stck
            stck%topElement = 0
        end subroutine stackClear

        !check if the stck is empty
        logical function isEmpty(stck)
            type(Stack), intent(in) :: stck
            isEmpty = (stck%topElement == 0)
        end function isEmpty

        !check if the stck is full
        logical function isFull(stck)
            type(Stack), intent(in) :: stck
            isFull = (stck%topElement == stck%maxElements)
        end function isFull


        !push an element onto the stack
        subroutine push(stck, left, right)
            type(Stack), intent(inout) :: stck
            integer, intent(in) :: left, right
            if (isFull(stck)) then
                error stop "Stack is full"
            endif

            stck%topElement = stck%topElement + 1
            stck%elements(stck%topElement)%left = left
            stck%elements(stck%topElement)%right = right
        end subroutine push


        !pop an element from the stack
        subroutine pop(stck, left, right)
            type(Stack), intent(inout) :: stck
            integer, intent(out) :: left, right

            if(isEmpty(stck)) then
                error stop "Stack is empty"
            endif

            left = stck%elements(stck%topElement)%left
            right = stck%elements(stck%topElement)%right
            stck%topElement = stck%topElement - 1
        end subroutine pop


end module stackADT