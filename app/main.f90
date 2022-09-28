program main

	! NOTE - this is probably broken after I made the change to be more fortran compactable.


	use utils
	use stdlib_string_type
	use stdlib_error, only: error_stop
	use stdlib_io, only: open, getline

	implicit none

	type(cmd)                 :: args
	type(string_type)         :: string
	integer                   :: u, iostat, ln
	character(:), allocatable :: str

	args = get_args()
	u = open(args%fin, iostat=iostat)
	if (iostat.ne.0) call error_stop("error: cannot open " // args%fin)

	ln = 0
	do while (iostat.eq.0)
		call getline(u, string, iostat)
		ln = ln + 1                           ! line number
		string = indent(string, ln, args%spt) ! convert spaces to tabs
		string = space(string, ln)            ! remove double spaces
		str = char(string)                    ! necessary as stdlib strings cannot currently use write with fmt needed to strip the leading space
		write(*,'(a)') str
	end do

	close(u)

end program main
