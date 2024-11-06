module mod_dictionary
    use, intrinsic :: iso_fortran_env, only: rp=>real32, stdout=>output_unit
    implicit none
    private

    public :: list, node, knode

    character(*), parameter :: fmt_int  = '(I5)'
    character(*), parameter :: fmt_real = '(F5.1)'

    type :: node
        class(*),    pointer :: value => null()
        class(node), pointer :: next  => null()
    contains
        procedure :: str => node_str
        procedure :: print => node_print
        final     :: node_finalize
    end type node
    !
    type, extends(node) :: knode
        character(len=:), allocatable :: key
    contains
        procedure :: print => knode_print
        final     :: knode_finalize
    end type knode
    !
    type :: list
        integer :: size = 0
        class(node), pointer :: head => null()
        class(node), pointer :: tail => null()
    contains
        procedure :: print => list_print
        procedure :: length
        procedure :: first
        procedure :: last
        procedure :: append
        final     :: list_finalize
    end type list
    !
contains
    !
    function node_str(self) result(str)
        class(node), intent(in)   :: self
        character(:), allocatable :: str
        !
        character(len=128) :: str_val
        !
        select type(value=>self%value)
        type is (integer)
            write(str_val,fmt_int) value
        type is (real)
            write(str_val,fmt_real) value
        type is (logical)
            write(str_val,'(L)') value
        type is (character(*))
            write(str_val,'(A)') value
        class default
            str_val = "unknown"
        end select
        !
        str = trim(str_val)
    end function node_str
    !
    subroutine node_print(self)
        class(node), intent(in) :: self
        write(stdout,*) self%str()
    end subroutine node_print
    !
    subroutine knode_print(self)
        class(knode), intent(in) :: self
        write(stdout,100) self%key, self%str()
100 format(A,':',1x,A)
    end subroutine knode_print
    !
    subroutine list_print(self)
        class(list), intent(in) :: self
        class(node), pointer :: current_ptr
        !
        write(stdout,*) "Printing list with ",self%size, " elements"
        current_ptr => self%head
        do while(associated(current_ptr))
            call current_ptr%print
            current_ptr => current_ptr%next
        enddo
    end subroutine list_print
    !
    pure function length(self) result(size)
        class(list), intent(in) :: self
        integer :: size
        size = self%size
    end function length
    !
    function first(self)
        class(list), intent(in) :: self
        class(node), pointer :: first
        first => self%head
    end function first
    !
    function last(self)
        class(list), intent(in) :: self
        class(node), pointer :: last
        last => self%tail
    end function last
    !
    subroutine append(self, node_ptr)
        class(list), intent(inout) :: self
        class(node), pointer, intent(in) :: node_ptr
        !
        self%size = self%size + 1
        if(associated(self%tail)) then
            self%tail%next => node_ptr
        else
            self%head => node_ptr
        endif
        self%tail => node_ptr
    end subroutine append
    !
    !*** Finalize
    !
    subroutine node_finalize(self)
        type(node), intent(inout) :: self
        write(*,*) "+++ delete node"
        if(associated(self%value)) deallocate(self%value)
        nullify(self%next)
    end subroutine node_finalize
    !
    subroutine knode_finalize(self)
        type(knode), intent(inout) :: self
        write(*,*) "+++ delete knode"
        if(allocated(self%key)) deallocate(self%key)
    end subroutine knode_finalize
    !
    subroutine list_finalize(self)
        type(list), intent(inout) :: self
        class(node), pointer :: current_ptr, tmp_ptr
        !
        write(*,*) "+++ delete list"
        current_ptr => self%head
        do while(associated(current_ptr))
            self%size = self%size - 1
            nullify(tmp_ptr)
            tmp_ptr => current_ptr%next
            deallocate(current_ptr)
            current_ptr => tmp_ptr
        enddo
        !
        nullify(self%head)
        nullify(self%tail)
    end subroutine list_finalize
    !
end module mod_dictionary
