module tests_scanner
	use stdlib_string_type
	use scanner
	use testdrive,only:error_type,unittest_type,new_unittest,check
	implicit none
	private
	public::collect_tests
	contains
		subroutine collect_tests(testsuite)
			type(unittest_type),allocatable,intent(out)::testsuite(:)
			testsuite=[&
			&new_unittest("test remaining",test_remaining),&
			&new_unittest("test next",test_next),&
			&new_unittest("test skip",test_skip),&
			&new_unittest("test skipn",test_skipn),&
			&new_unittest("test peek",test_peek),&
			&new_unittest("test peekn",test_peekn),&
			&new_unittest("test back",test_back),&
			&new_unittest("test backn",test_backn),&
			&new_unittest("test peek_back",test_peek_back),&
			&new_unittest("test peek_backn",test_peek_backn),&
			&new_unittest("test first",test_first),&
			&new_unittest("test current",test_current),&
			&new_unittest("test last",test_last),&
			&new_unittest("test previous",test_previous),&
			&new_unittest("test jumps",test_jumps)]
		endsubroutine collect_tests

		subroutine test_remaining(error)
			type(error_type),allocatable,intent(out)::error
			type(string_type)::string
			type(textscanner)::txt
			string="abc"
			call txt%new(string)
			call check(error,txt%remaining(),2)
			call check(error,txt%current(),ichar("a"))
			call check(error,txt%next(),ichar("b"))
			call check(error,txt%remaining(),1)
			call check(error,txt%next(),ichar("c"))
			call check(error,txt%remaining(),0)
			call check(error,txt%next(),-1)
			call check(error,txt%remaining(),0)
			call check(error,txt%next(),-1)
			call check(error,txt%next(),-1)
			call check(error,txt%remaining(),0)
			call txt%first()
			call check(error,txt%remaining(),2)
			if(allocated(error)) return
		endsubroutine test_remaining

		subroutine test_next(error)
			type(error_type),allocatable,intent(out)::error
			type(string_type)::string
			type(textscanner)::txt
			string="abc"
			call txt%new(string)
			call check(error,txt%next(),ichar("b"))
			call check(error,txt%next(),ichar("c"))
			call check(error,txt%next(),-1)
			call check(error,txt%next(),-1)
			call check(error,txt%next(),-1)
			call check(error,txt%next(),-1)
			if(allocated(error)) return
		endsubroutine test_next

		subroutine test_skip(error)
			type(error_type),allocatable,intent(out)::error
			type(string_type)::string
			type(textscanner)::txt
			string="abc"
			call txt%new(string)
			call check(error,txt%next(),ichar("b"))
			call txt%skip()
			call check(error,txt%next(),-1)
			call txt%skip()
			call check(error,txt%next(),-1)
			if(allocated(error)) return
		endsubroutine test_skip

		subroutine test_skipn(error)
			type(error_type),allocatable,intent(out) :: error
			type(string_type)::string
			type(textscanner)::txt
			string="abc"
			call txt%new(string)
			call txt%skipn(1)
			call check(error,txt%next(),ichar("c"))
			call check(error,txt%next(),-1)
			if(allocated(error)) return
		endsubroutine test_skipn

		subroutine test_peek(error)
			type(error_type),allocatable,intent(out)::error
			type(string_type)::string
			type(textscanner)::txt
			string="abc"
			call txt%new(string)
			call check(error,txt%peek(),ichar("b"))
			call check(error,txt%peek(),ichar("b"))
			call check(error,txt%peek(),ichar("b"))
			call check(error,txt%next(),ichar("b"))
			call check(error,txt%next(),ichar("c"))
			call check(error,txt%next(),-1)
			if(allocated(error)) return
		endsubroutine test_peek

		subroutine test_peekn(error)
			type(error_type),allocatable,intent(out)::error
			type(string_type)::string
			type(textscanner)::txt
			string="abc"
			call txt%new(string)
			call check(error,txt%peekn(0),ichar("a"))
			call check(error,txt%peekn(1),ichar("b"))
			call check(error,txt%peekn(2),ichar("c"))
			call check(error,txt%peekn(3),-1)
			call check(error,txt%peekn(4),-1)
			call check(error,txt%next(),ichar("b"))
			call check(error,txt%next(),ichar("c"))
			call check(error,txt%next(),-1)
			if(allocated(error)) return
		endsubroutine test_peekn

		subroutine test_back(error)
			type(error_type),allocatable,intent(out)::error
			type(string_type)::string
			type(textscanner)::txt
			string="abc"
			call txt%new(string)
			call check(error,txt%next(),ichar("b"))
			call txt%back()
			call check(error,txt%next(),ichar("b"))
			call check(error,txt%next(),ichar("c"))
			call txt%back()
			call check(error,txt%next(),ichar("c"))
			call check(error,txt%next(),-1)
			if(allocated(error)) return
		endsubroutine test_back

		subroutine test_backn(error)
			type(error_type),allocatable,intent(out)::error
			type(string_type)::string
			type(textscanner)::txt
			string="abc"
			call txt%new(string)
			call check(error,txt%next(),ichar("b"))
			call txt%backn(10)
			call check(error,txt%next(),ichar("b"))
			call check(error,txt%next(),ichar("c"))
			call check(error,txt%next(),-1)
			call txt%backn(2)
			call check(error,txt%next(),ichar("b"))
			if(allocated(error)) return
		endsubroutine test_backn

		subroutine test_peek_back(error)
			type(error_type),allocatable,intent(out)::error
			type(string_type)::string
			type(textscanner)::txt
			string="abc"
			call txt%new(string)
			call check(error,txt%next(),ichar("b"))
			call check(error,txt%next(),ichar("c"))
			call check(error,txt%peek_back(),ichar("b"))
			call check(error,txt%peek_back(),ichar("b"))
			call check(error,txt%peek_back(),ichar("b"))
			call check(error,txt%next(),-1)
			call check(error,txt%peek_back(),ichar("b"))
			call txt%first()
			call check(error,txt%peek_back(),-1)
			call txt%last()
			call check(error,txt%peek_back(),ichar("b"))
			if(allocated(error)) return
		endsubroutine test_peek_back

		subroutine test_peek_backn(error)
			type(error_type),allocatable,intent(out)::error
			type(string_type)::string
			type(textscanner)::txt
			string="abc"
			call txt%new(string)
			call txt%last()
			call check(error,txt%peek_backn(0),ichar("c"))
			call check(error,txt%peek_backn(1),ichar("b"))
			call check(error,txt%peek_backn(2),ichar("a"))
			call check(error,txt%peek_backn(3),-1)
			call check(error,txt%peek_backn(4),-1)
			if(allocated(error)) return
		endsubroutine test_peek_backn

		subroutine test_first(error)
			type(error_type),allocatable,intent(out)::error
			type(string_type)::string
			type(textscanner)::txt
			string="abc"
			call txt%new(string)
			call check(error,txt%next(),ichar("b"))
			call check(error,txt%next(),ichar("c"))
			call check(error,txt%next(),-1)
			call txt%first()
			call check(error,txt%next(),ichar("b"))
			if(allocated(error)) return
		endsubroutine test_first

		subroutine test_current(error)
			type(error_type),allocatable,intent(out)::error
			type(string_type)::string
			type(textscanner)::txt
			string="abc"
			call txt%new(string)
			call check(error,txt%current(),ichar("a"))
			call check(error,txt%next(),ichar("b"))
			call check(error,txt%current(),ichar("b"))
			call check(error,txt%current(),ichar("b"))
			call check(error,txt%peek_back(),ichar("a"))
			call check(error,txt%next(),ichar("c"))
			call check(error,txt%current(),ichar("c"))
			call check(error,txt%current(),ichar("c"))
			call check(error,txt%peek_back(),ichar("b"))
			call check(error,txt%next(),-1)
			call check(error,txt%current(),ichar("c"))
			call check(error,txt%next(),-1)
			call check(error,txt%current(),ichar("c"))
			call check(error,txt%next(),-1)
			call check(error,txt%current(),ichar("c"))
			call txt%first()
			call check(error,txt%current(),ichar("a"))
			call check(error,txt%next(),ichar("b"))
			call check(error,txt%current(),ichar("b"))
			if(allocated(error)) return
		endsubroutine test_current

		subroutine test_last(error)
			type(error_type),allocatable,intent(out)::error
			type(string_type)::string
			type(textscanner)::txt
			string="abc"
			call txt%new(string)
			call txt%last()
			call check(error,txt%current(),ichar("c"))
			if(allocated(error)) return
		endsubroutine test_last

		subroutine test_previous(error)
			type(error_type),allocatable,intent(out)::error
			type(string_type)::string
			type(textscanner)::txt
			string="abc"
			call txt%new(string)
			call check(error,txt%next(),ichar("b"))
			call check(error,txt%next(),ichar("c"))
			call check(error,txt%next(),-1)
			call check(error,txt%prev(),ichar("b"))
			call check(error,txt%prev(),ichar("a"))
			call check(error,txt%prev(),-1)
			call check(error,txt%prev(),-1)
			call check(error,txt%prev(),-1)
			if(allocated(error)) return
		endsubroutine test_previous

		subroutine test_jumps(error)
			type(error_type),allocatable,intent(out)::error
			type(string_type)::string
			type(textscanner)::txt
			string="abc"
			call txt%new(string)
			call txt%jump_to(ichar("c"))
			call check(error,txt%next(),-1)
			call txt%jump_from(ichar("a"))
			call check(error,txt%prev(),-1)
			call txt%jump_to(ichar("b"))
			call check(error,txt%next(),ichar("c"))
			call txt%jump_from(ichar("d"))
			call check(error,txt%pos,1)
			if(allocated(error)) return
		endsubroutine test_jumps

endmodule tests_scanner
