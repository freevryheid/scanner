module scanner
	!! based on vlang's textscanner
	!! tools to parse strings by character
	!! for small scanners/parsers
	use stdlib_string_type

	implicit none

	private

	type :: textscanner
		type(string_type) :: input
		integer :: ilen
		integer :: pos
		contains
			procedure :: new
			procedure :: skip
			procedure :: skipn
			procedure :: back
			procedure :: backn
			procedure :: first
			procedure :: last
			procedure :: remaining
			procedure :: next
			procedure :: peek
			procedure :: peekn
			procedure :: peek_backn
			procedure :: peek_back
			procedure :: current
	end type

	public :: textscanner

	contains

		subroutine new(self, input)
			!! instantiate a new scanner
			class(textscanner) :: self
			type(string_type), intent(in) :: input
			self%input = input
			self%ilen = len(input)
			self%pos = 0
		end subroutine new

		subroutine skip(self)
			!! skip one character ahead
			class(textscanner) :: self
			if (self%pos + 1 < self%ilen) then
				self%pos = self%pos + 1
			end if
		end subroutine skip

		subroutine skipn(self, n)
			!! skip n characters ahead, stopping at end of input
			class(textscanner) :: self
			integer, intent(in) :: n
			self%pos = self%pos + n
			if (self%pos > self%ilen) then
				self%pos = self%ilen
			end if
		end subroutine skipn

		subroutine back(self)
			!! skip one character behind
			class(textscanner) :: self
			if (self%pos > 0) then
				self%pos = self%pos - 1
			end if
		end subroutine back

		subroutine backn(self, n)
			!! skip n characters behind, stopping at start of input
			class(textscanner) :: self
			integer, intent(in) :: n
			self%pos = self%pos - n
			if (self%pos < 0) then
				self%pos = 0
			end if
			if (self%pos > self%ilen) then
				self%pos = self%ilen
			end if
		end subroutine backn

		subroutine first(self)
			!! move to start of input
			class(textscanner) :: self
			self%pos = 0
		end subroutine first

		subroutine last(self)
			!! move to end of input
			class(textscanner) :: self
			self%pos = self%ilen
		end subroutine last

		function remaining(self) result(res)
			!! returns characters remaining from current position
			class(textscanner) :: self
			integer :: res
			res = self%ilen - self%pos
		end function remaining

		function next(self) result(res)
			!! returns next character from current position, moving to that position
			!! returns -1 if beyond input
			class(textscanner) :: self
			character(len=:), allocatable :: chr
			integer :: res
			if (self%pos < self%ilen) then
				self%pos = self%pos + 1
				chr = char(self%input, self%pos)
				res = ichar(chr)
			else
				res = -1
			end if
		end function next

		function peek(self) result(res)
			!! returns next character code from current position, without moving to that position
			!! returns -1 if beyond input
			class(textscanner) :: self
			character(len=:), allocatable :: chr
			integer :: res
			if (self%pos < self%ilen) then
				chr = char(self%input, self%pos + 1)
				res = ichar(chr)
			else
			  res = -1
			end if
		end function peek

		function peekn(self, n) result(res)
			!! returns character code  positions ahead, without moving to that position
			!! returns -1 if beyond input
			class(textscanner) :: self
			integer, intent(in) :: n
			character(len=:), allocatable :: chr
			integer :: res
			if (self%pos + n < self%ilen) then
				chr = char(self%input, self%pos + n + 1)
				res = ichar(chr)
			else
				res = -1
			end if
		end function peekn

		function peek_backn(self, n) result(res)
			!! returns character code n positions behind, without moving to that position
			!! returns -1 if before input
			class(textscanner) :: self
			integer, intent(in) :: n
			character(len=:), allocatable :: chr
			integer :: res
			if (self%pos > n) then
				chr = char(self%input, self%pos - n)
				res = ichar(chr)
			else
				res = -1
			end if
		end function peek_backn

		function peek_back(self) result(res)
			!! returns previous character code from current position, without moving to that position
			!! returns -1 if before input
			class(textscanner) :: self
			integer :: res
			res = self%peek_backn(1)
		end function peek_back

		function current(self) result(res)
			!! returns character code at current position
			!! returns -1 if before input
			class(textscanner) :: self
			character(len=:), allocatable :: chr
			integer :: res
			if (self%pos > 0) then
				chr = char(self%input, self%pos)
				res = ichar(chr)
			else
				res = -1
			end if
		end function current

end module scanner
