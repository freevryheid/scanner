module scanner
	!! based on vlang's textscanner
	!! tools to parse strings by character
	!! for small scanners/parsers
	!! revised from original in that you cannot
	!! move beyond the start and end of the input
	use stdlib_string_type

	implicit none

	private

	type :: textscanner
		type(string_type)::input
		integer::ilen
		integer::pos
		contains
			procedure::new
			procedure::jump_to
			procedure::jump_from
			procedure::skip
			procedure::skipn
			procedure::back
			procedure::backn
			procedure::first
			procedure::last
			procedure::remaining
			procedure::next
			procedure::prev
			procedure::peek
			procedure::peekn
			procedure::peek_backn
			procedure::peek_back
			procedure::current
	endtype

	public::textscanner

	contains

		subroutine new(self, input)
			!! instantiate a new scanner
			class(textscanner)::self
			type(string_type),intent(in)::input
			self%input=input
			self%ilen=len(input)
			self%pos=1
		endsubroutine new

		subroutine jump_to(self, code)
			!! jumps forward to code or to end of input if code not found
			class(textscanner)::self
			integer,intent(in)::code
			do
				if(self%next().eq.code.or.self%pos.eq.self%ilen) exit
			enddo
		endsubroutine jump_to

		subroutine jump_from(self, code)
			!! jumps backwards to code or to start of input if code not found
			class(textscanner) :: self
			integer,intent(in) :: code
			do
				if(self%prev().eq.code.or.self%pos.eq.1) exit
			enddo
		endsubroutine jump_from

		subroutine skip(self)
			!! skip one character ahead
			class(textscanner)::self
			if(self%pos+1.le.self%ilen)then
				self%pos=self%pos+1
			endif
		endsubroutine skip

		subroutine skipn(self, n)
			!! skip n characters ahead, stopping at end of input
			class(textscanner) :: self
			integer,intent(in) :: n
			self%pos=self%pos+n
			if(self%pos.gt.self%ilen)then
				self%pos=self%ilen
			endif
		endsubroutine skipn

		subroutine back(self)
			!! skip one character behind
			class(textscanner)::self
			if(self%pos.ge.2)then
				self%pos=self%pos-1
			endif
		endsubroutine back

		subroutine backn(self, n)
			!! skip n characters behind, stopping at start of input
			class(textscanner)::self
			integer,intent(in)::n
			self%pos=self%pos-n
			if(self%pos.lt.1)then
				self%pos=1
			endif
		endsubroutine backn

		subroutine first(self)
			!! move to start of input
			class(textscanner)::self
			self%pos=1
		endsubroutine first

		subroutine last(self)
			!! move to end of input
			class(textscanner)::self
			self%pos=self%ilen
		endsubroutine last

		function remaining(self) result(res)
			!! returns characters remaining from current position
			class(textscanner)::self
			integer::res
			res=self%ilen-self%pos
		endfunction remaining

		function next(self) result(res)
			!! returns next character from current position, moving to that position
			!! returns -1 if beyond input
			class(textscanner)::self
			character(len=:),allocatable::chr
			integer::res
			if(self%pos.lt.self%ilen)then
				self%pos=self%pos+1
				chr=char(self%input,self%pos)
				res=ichar(chr)
			else
				res=-1
			endif
		endfunction next

		function prev(self) result(res)
			!! returns previous character from current position, moving to that position
			!! returns -1 if before input
			class(textscanner)::self
			character(len=:),allocatable::chr
			integer::res
			if(self%pos.ge.2)then
				self%pos=self%pos-1
				chr=char(self%input, self%pos)
				res=ichar(chr)
			else
				res=-1
			endif
		endfunction prev

		function peek(self) result(res)
			!! returns next character code from current position, without moving to that position
			!! returns -1 if beyond input
			class(textscanner)::self
			character(len=:),allocatable::chr
			integer::res
			if(self%pos.lt.self%ilen)then
				chr=char(self%input,self%pos+1)
				res=ichar(chr)
			else
				res=-1
			endif
		endfunction peek

		function peekn(self, n) result(res)
			!! returns character code n positions ahead, without moving to that position
			!! returns -1 if beyond input
			class(textscanner)::self
			integer,intent(in)::n
			character(len=:),allocatable::chr
			integer::res
			if(self%pos+n.le.self%ilen)then
				chr=char(self%input,self%pos+n)
				res=ichar(chr)
			else
				res=-1
			endif
		endfunction peekn

		function peek_backn(self,n) result(res)
			!! returns character code n positions behind, without moving to that position
			!! returns -1 if before input
			class(textscanner)::self
			integer,intent(in)::n
			character(len=:),allocatable::chr
			integer::res
			if(self%pos.gt.n)then
				chr=char(self%input,self%pos-n)
				res=ichar(chr)
			else
				res=-1
			endif
		endfunction peek_backn

		function peek_back(self) result(res)
			!! returns previous character code from current position, without moving to that position
			!! returns -1 if before input
			class(textscanner)::self
			integer::res
			res=self%peek_backn(1)
		endfunction peek_back

		function current(self) result(res)
			!! returns character code at current position
			class(textscanner)::self
			character(len=:),allocatable::chr
			integer::res
			chr=char(self%input,self%pos)
			res=ichar(chr)
		endfunction current

end module scanner
