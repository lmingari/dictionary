module mod_list
    use, intrinsic :: iso_fortran_env, only: rp=>real32
    implicit none
    private

    public :: list, node

    type :: node
        character(len=:), allocatable :: key
        class(*),         allocatable :: value
        type(node), pointer :: next  => null()
    contains
        procedure :: print => node_print
        final :: node_finalize
    end type node

    type :: list
        integer :: size = 0
        type(node), pointer :: head => null()
        type(node), pointer :: tail => null()
    contains
        procedure :: length
        procedure :: first
        procedure :: last
        procedure :: append
        procedure :: print => list_print
        final     :: list_finalize
    end type list
    !
contains
    !
    subroutine node_print(self)
        class(node), intent(in) :: self
        !
        select type(value=>self%value)
        type is (integer)
            write(*,1) self%key,value
        type is (real)
            write(*,2) self%key,value
        type is (logical)
            write(*,3) self%key,value
        type is (character(*))
            write(*,4) self%key,value
        type is (list)
            write(*,*) "--- ",self%key," ---"
            call value%print
            write(*,*) "----------------------"
        class default
            write(*,*) "unknown"
        end select
        !
1 format(A,':',1x,I5,1x,'integer')
2 format(A,':',1x,F5.1,1x,'real')
3 format(A,':',1x,L,1x,'bool')
4 format(A,':',1x,A,1x,'string')
        !
    end subroutine node_print
    !
    subroutine node_finalize(self)
        type(node), intent(inout) :: self
        
        write(*,*) "+++ deleting node ", self%key, allocated(self%value)
        if(allocated(self%value)) deallocate(self%value)
        if(allocated(self%key))   deallocate(self%key)
        nullify(self%next)
    end subroutine node_finalize
    !
    pure function length(self) result(size)
        class(list), intent(in) :: self
        integer :: size

        size = self%size
    end function length
    !
    function first(self)
        class(list), intent(in) :: self
        type(node), pointer :: first
        !
        first => self%head
    end function first
    !
    function last(self)
        class(list), intent(in) :: self
        type(node), pointer :: last
        !
        last => self%tail
    end function last
    !
    subroutine append(self, key, value)
        class(list), intent(inout) :: self
        character(len=*),   intent(in)    :: key
        class(*),           intent(in)    :: value
        !
        type(node), pointer :: node_ptr
        !
        ! I'm not sure if the target keeps allocated on return ¿?
        ! It seems to work with GNU compiler
        allocate(node_ptr)
!        node_ptr = node(key,value)
        node_ptr%key = key
        node_ptr%value = value
        !
        self%size = self%size + 1
        !
        if(associated(self%tail)) then
            self%tail%next => node_ptr
        else
            self%head => node_ptr
        endif
        self%tail => node_ptr
    end subroutine append
    !
    subroutine list_print(self)
        class(list), intent(in) :: self
        !
        type(node), pointer :: current_ptr
        !
        current_ptr => self%head
        !
        do while(associated(current_ptr))
            call current_ptr%print
            current_ptr => current_ptr%next
        enddo
    end subroutine list_print
    !
    subroutine list_finalize(self)
        type(list), intent(inout) :: self
        !
        type(node), pointer :: current_ptr, tmp_ptr
        !
        current_ptr => self%head
        !
        write(*,*) "+++ deleting list"
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
end module mod_list
