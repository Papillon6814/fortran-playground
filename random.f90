program random
  implicit none

  integer, dimension(4) :: n
  integer i
  integer, parameter::ITERATION_NUM  = 10

  double precision :: rnd
  integer :: arrkey1, arrkey2
  integer :: seedsize
  integer :: pick1, pick2
  integer, allocatable :: seed(:)
  integer :: rand_except
  
  n(1) = 123
  n(2) = 345
  n(3) = 567
  n(4) = 789

  call random_seed(size=seedsize)
  allocate(seed(seedsize))
  
  do i = 1, seedsize
    call system_clock(count=seed(i))
  end do
  call random_seed(put=seed(:))

  do i = 1, ITERATION_NUM
    call random_number(rnd)
    arrkey1 = int(rnd*4) + 1
    arrkey2 = rand_except(arrkey1)

    pick1 = n(arrkey1)
    pick2 = n(arrkey2)

    write (*, '(I3, I3)') pick1, pick2
  end do
end

integer function rand_except(except)
  implicit none

  integer except
  double precision :: rnd
  integer res
  res = except

  do while (res == except)
    call random_number(rnd)
    res = int(rnd*4) + 1
  end do

  rand_except = res
end function rand_except
