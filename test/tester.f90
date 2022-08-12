program tester
	use, intrinsic :: iso_fortran_env, only : error_unit
	use testdrive, only : run_testsuite
	use tests_scanner, only: collect_tests
	implicit none
	integer :: stat
	stat = 0
	print *, new_line('a')," ... scanner tests ... "
	call run_testsuite(collect_tests, error_unit, stat)
	if (stat > 0) then
		write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
		error stop
	end if
end program tester
