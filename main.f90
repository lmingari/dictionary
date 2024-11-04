program main
    use, intrinsic :: iso_fortran_env, only: rp=>real32
    use mod_list
    implicit none
    !
    type(list), allocatable :: my_list, l
    !
    allocate(my_list,l)
    !
    call my_list%append("f1",1.3_rp)
    call my_list%append("i3",9)
    call my_list%append("on",.false.)
    call my_list%append("arr","pepito")

    call l%append("i55555",9)
    call l%append("test",.true.)
    !
    call my_list%append("list",l)
    !
    write(*,*) "List of size: ", my_list%length()
    write(*,*) "----------------"
    call my_list%print
    !
    deallocate(my_list)
    !
end program main
