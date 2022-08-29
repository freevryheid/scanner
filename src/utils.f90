module utils

	use scanner
	use stdlib_string_type
	use stdlib_strings
	use stdlib_error, only : error_stop
	use str2num, only : atoi
	use stdlib_ascii, only: TAB

	implicit none

	type :: cmd
		character(:), allocatable :: fin
		integer :: spt
	end type

	private
	public cmd
	public :: get_args
	public :: indent
	public :: space

	contains

		function get_args() result(args)
		  type(cmd) :: args
			character(:), allocatable :: arg
			integer :: nargs, arglen, iostat
			nargs = command_argument_count()
			if (nargs.ne.2) call error_stop("usage: scanner path_to_file_to_scan spaces_per_tab")
			call get_command_argument(number=1, length=arglen)
			allocate (character(arglen) :: arg)
			call get_command_argument(number=1, value=arg, status=iostat)
			if (iostat.ne.0) call error_stop("error: cannot retrieve first argument")
			args%fin = arg
			call get_command_argument(number=2, length=arglen)
			deallocate(arg)
			allocate (character(arglen) :: arg)
			call get_command_argument(number=2, value=arg, status=iostat)
			if (iostat.ne.0) call error_stop("error: cannot retrieve second argument")
			nargs = atoi(arg)
			if (nargs <= 0) call error_stop("error: second argument must be greater than 0")
			args%spt = nargs
		end function get_args

		function indent(string, ln, spt) result(res)
			type(string_type), intent(in) :: string
			type(string_type) :: res
			integer, intent(in) :: ln, spt ! spaces per tab
			type(textscanner) :: txt
			integer :: code, i, nspaces, ntabs
			call txt%new(string)
			nspaces = 0
			do
				code = txt%next()
				if (code.eq.32) then
					nspaces = nspaces + 1
				else
					exit
				end if
			end do
			res = string
			if (nspaces.gt.0) then
				if (mod(nspaces, spt).ne.0) call error_stop("error: inconsistent number of spaces per tab, line # " // to_string(ln))
				ntabs = nspaces / spt
				res = strip(res)
				do i = 1, ntabs
					res = TAB // res
				end do
			end if
		end function indent

		function space(string, ln) result(res)
			type(string_type), intent(in) :: string
			type(string_type) :: res
			integer, intent(in) :: ln
			type(textscanner) :: txt
			logical :: in_a_double_string, in_a_single_string
			in_a_double_string = .false.
			in_a_single_string = .false.
			res = string
			do
				call txt%new(res)
				do
					select case (txt%next())
						case (-1)
							return
						case (32)
							if (txt%peek().eq.32) then ! found a space following a space
							  if((.not.in_a_double_string).and.(.not.in_a_single_string)) then
									res = char(res, 1, txt%pos -1) // char(res, txt%pos + 1, txt%ilen) ! remove a space
									exit
								end if
							end if
						case (33) ! ignore comments
							return
						case (34) ! ignore spaces in double strings
							if (.not.in_a_single_string) in_a_double_string = .not.in_a_double_string
						case (39) ! ignore spaces in single strings
							if (.not.in_a_double_string) in_a_single_string = .not.in_a_single_string
						case default
							cycle
					end select
				end do
			end do
		end function space

end module utils
