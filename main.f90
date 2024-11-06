program main
    use, intrinsic :: iso_fortran_env, only: rp=>real32, dp=>real64
    use mod_dictionary
    implicit none
    !
    type(list) :: my_list
    class(node), pointer :: n

    write(*,*) "starting"

    block
        !
        allocate(n)
        allocate(n%value, source = .true.)
        call my_list%append(n)
        !
    end block

    write(*,*) "continue"

    block
        class(knode), pointer :: kn
        !
        allocate(kn)
        kn%key = "my_key"
        allocate(kn%value, source = 4.2_rp)
        call my_list%append(kn)

!        allocate(n)
!        n%key = "my_key_integer"
!        allocate(n%value, source = 10)
!        call my_list%append(n)

    end block

    call my_list%print

    !    allocate(my_list,l)
!    !
!    call my_list%append("f1",1.3_rp)
!    call my_list%append("i3",9)
!    call my_list%append("on",.false.)
!    call my_list%append("arr","pepito")
!
!    call l%append("i55555",9)
!    call l%append("test",.true.)
!    !
!    call my_list%append("list",l)
!    !
!    write(*,*) "List of size: ", my_list%length()
!    write(*,*) "----------------"
!    call my_list%print
!    !
!    deallocate(my_list)
    !
end program main
