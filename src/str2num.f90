module str2num

	use, intrinsic :: iso_c_binding, only: c_char, c_int, c_double

	implicit none
	private
	public :: atoi, atof

	interface

		function atoi(input) bind(c)
			import :: c_int, c_char
			integer(c_int) :: atoi
			character(c_char) :: input(*)
		end function

		function atof(input) bind(c)
			import c_double, c_char
			real(c_double) :: atof
			character(c_char) :: input(*)
		end function

	end interface

end module str2num
