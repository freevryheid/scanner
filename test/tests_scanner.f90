module tests_scanner

	use stdlib_string_type
	use scanner
	use testdrive, only: error_type, unittest_type, new_unittest, check

	implicit none

	private
	public :: collect_tests

	contains

		subroutine collect_tests(testsuite)
			type(unittest_type), allocatable, intent(out) :: testsuite(:)
			testsuite = [new_unittest("test remaining", test_remaining), &
			& new_unittest("test next", test_next), &
			& new_unittest("test skip", test_skip), &
			& new_unittest("test skipn", test_skipn), &
			& new_unittest("test peek", test_peek), &
			& new_unittest("test peekn", test_peekn), &
			& new_unittest("test back", test_back), &
			& new_unittest("test backn", test_backn), &
			& new_unittest("test peek_back", test_peek_back), &
			& new_unittest("test peek_backn", test_peek_backn), &
			& new_unittest("test first", test_first), &
			& new_unittest("test current", test_current), &
			& new_unittest("test last", test_last)]
		end subroutine collect_tests

		subroutine test_remaining(error)
			type(error_type), allocatable, intent(out) :: error
			type(string_type) :: string
			type(textscanner) :: txt
			string = "abc"
			call txt%new(string)
			call check(error, txt%remaining(), 3)
			if (allocated(error)) return
			call check(error, txt%next(), ichar("a"))
			if (allocated(error)) return
			call check(error, txt%next(), ichar("b"))
			if (allocated(error)) return
			call check(error, txt%remaining(), 1)
			if (allocated(error)) return
			call check(error, txt%next(), ichar("c"))
			if (allocated(error)) return
			call check(error, txt%remaining(), 0)
			if (allocated(error)) return
			call check(error, txt%next(), -1)
			if (allocated(error)) return
			call check(error, txt%next(), -1)
			if (allocated(error)) return
			call check(error, txt%remaining(), 0)
			if (allocated(error)) return
			call txt%first()
			call check(error, txt%remaining(), 3)
			if (allocated(error)) return
		end subroutine test_remaining

		subroutine test_next(error)
			type(error_type), allocatable, intent(out) :: error
			type(string_type) :: string
			type(textscanner) :: txt
			string = "abc"
			call txt%new(string)
			call check(error, txt%next(), ichar("a"))
			if (allocated(error)) return
			call check(error, txt%next(), ichar("b"))
			if (allocated(error)) return
			call check(error, txt%next(), ichar("c"))
			if (allocated(error)) return
			call check(error, txt%next(), -1)
			if (allocated(error)) return
			call check(error, txt%next(), -1)
			if (allocated(error)) return
			call check(error, txt%next(), -1)
			if (allocated(error)) return
		end subroutine test_next

		subroutine test_skip(error)
			type(error_type), allocatable, intent(out) :: error
			type(string_type) :: string
			type(textscanner) :: txt
			string = "abc"
			call txt%new(string)
			call check(error, txt%next(), ichar("a"))
			if (allocated(error)) return
			call txt%skip()
			call check(error, txt%next(), ichar("c"))
			if (allocated(error)) return
			call check(error, txt%next(), -1)
			if (allocated(error)) return
		end subroutine test_skip

		subroutine test_skipn(error)
			type(error_type), allocatable, intent(out) :: error
			type(string_type) :: string
			type(textscanner) :: txt
			string = "abc"
			call txt%new(string)
			call txt%skipn(2)
			call check(error, txt%next(), ichar("c"))
			if (allocated(error)) return
			call check(error, txt%next(), -1)
			if (allocated(error)) return
		end subroutine test_skipn

		subroutine test_peek(error)
			type(error_type), allocatable, intent(out) :: error
			type(string_type) :: string
			type(textscanner) :: txt
			string = "abc"
			call txt%new(string)
			call check(error, txt%peek(), ichar("a"))
			if (allocated(error)) return
			call check(error, txt%peek(), ichar("a"))
			if (allocated(error)) return
			call check(error, txt%peek(), ichar("a"))
			if (allocated(error)) return
			call check(error, txt%next(), ichar("a"))
			if (allocated(error)) return
			call check(error, txt%next(), ichar("b"))
			if (allocated(error)) return
			call check(error, txt%next(), ichar("c"))
			if (allocated(error)) return
			call check(error, txt%next(), -1)
		end subroutine test_peek

		subroutine test_peekn(error)
			type(error_type), allocatable, intent(out) :: error
			type(string_type) :: string
			type(textscanner) :: txt
			string = "abc"
			call txt%new(string)
			call check(error, txt%peekn(0), ichar("a"))
			if (allocated(error)) return
			call check(error, txt%peekn(1), ichar("b"))
			if (allocated(error)) return
			call check(error, txt%peekn(2), ichar("c"))
			if (allocated(error)) return
			call check(error, txt%peekn(3), -1)
			if (allocated(error)) return
			call check(error, txt%peekn(4), -1)
			if (allocated(error)) return
			call check(error, txt%next(), ichar("a"))
			if (allocated(error)) return
			call check(error, txt%next(), ichar("b"))
			if (allocated(error)) return
			call check(error, txt%next(), ichar("c"))
			if (allocated(error)) return
			call check(error, txt%next(), -1)
			if (allocated(error)) return
		end subroutine test_peekn

		subroutine test_back(error)
			type(error_type), allocatable, intent(out) :: error
			type(string_type) :: string
			type(textscanner) :: txt
			string = "abc"
			call txt%new(string)
			call check(error, txt%next(), ichar("a"))
			if (allocated(error)) return
			call txt%back()
			call check(error, txt%next(), ichar("a"))
			if (allocated(error)) return
			call check(error, txt%next(), ichar("b"))
			if (allocated(error)) return
			call txt%back()
			call check(error, txt%next(), ichar("b"))
			if (allocated(error)) return
			call check(error, txt%next(), ichar("c"))
			if (allocated(error)) return
			call check(error, txt%next(), -1)
			if (allocated(error)) return
		end subroutine test_back

		subroutine test_backn(error)
			type(error_type), allocatable, intent(out) :: error
			type(string_type) :: string
			type(textscanner) :: txt
			string = "abc"
			call txt%new(string)
			call check(error, txt%next(), ichar("a"))
			if (allocated(error)) return
			call txt%backn(10)
			call check(error, txt%next(), ichar("a"))
			if (allocated(error)) return
			call check(error, txt%next(), ichar("b"))
			if (allocated(error)) return
			call check(error, txt%next(), ichar("c"))
			if (allocated(error)) return
			call txt%backn(2)
			call check(error, txt%next(), ichar("b"))
			if (allocated(error)) return
		end subroutine test_backn

		subroutine test_peek_back(error)
			type(error_type), allocatable, intent(out) :: error
			type(string_type) :: string
			type(textscanner) :: txt
			string = "abc"
			call txt%new(string)
			call check(error, txt%next(), ichar("a"))
			if (allocated(error)) return
			call check(error, txt%next(), ichar("b"))
			if (allocated(error)) return
			call check(error, txt%peek_back(), ichar("a"))
			if (allocated(error)) return
			call check(error, txt%peek_back(), ichar("a"))
			if (allocated(error)) return
			call check(error, txt%peek_back(), ichar("a"))
			if (allocated(error)) return
			call check(error, txt%next(), ichar("c"))
			if (allocated(error)) return
			call check(error, txt%peek_back(), ichar("b"))
			if (allocated(error)) return
			call txt%first()
			call check(error, txt%peek_back(), -1)
			if (allocated(error)) return
			call txt%last()
			call check(error, txt%peek_back(), ichar("b"))
			if (allocated(error)) return
		end subroutine test_peek_back

		subroutine test_peek_backn(error)
			type(error_type), allocatable, intent(out) :: error
			type(string_type) :: string
			type(textscanner) :: txt
			string = "abc"
			call txt%new(string)
			call txt%last()
			call check(error, txt%peek_backn(0), ichar("c"))
			if (allocated(error)) return
			call check(error, txt%peek_backn(1), ichar("b"))
			if (allocated(error)) return
			call check(error, txt%peek_backn(2), ichar("a"))
			if (allocated(error)) return
			call check(error, txt%peek_backn(3), -1)
			if (allocated(error)) return
			call check(error, txt%peek_backn(4), -1)
			if (allocated(error)) return
		end subroutine test_peek_backn

		subroutine test_first(error)
			type(error_type), allocatable, intent(out) :: error
			type(string_type) :: string
			type(textscanner) :: txt
			string = "abc"
			call txt%new(string)
			call check(error, txt%next(), ichar("a"))
			if (allocated(error)) return
			call check(error, txt%next(), ichar("b"))
			if (allocated(error)) return
			call check(error, txt%next(), ichar("c"))
			if (allocated(error)) return
			call check(error, txt%next(), -1)
			if (allocated(error)) return
			call txt%first()
			call check(error, txt%next(), ichar("a"))
			if (allocated(error)) return
		end subroutine test_first

		subroutine test_current(error)
			type(error_type), allocatable, intent(out) :: error
			type(string_type) :: string
			type(textscanner) :: txt
			string = "abc"
			call txt%new(string)
			call check(error, txt%current(), -1)
			if (allocated(error)) return
			call check(error, txt%next(), ichar("a"))
			if (allocated(error)) return
			call check(error, txt%current(), ichar("a"))
			if (allocated(error)) return
			call check(error, txt%current(), ichar("a"))
			if (allocated(error)) return
			call check(error, txt%peek_back(), -1)
			if (allocated(error)) return
			call check(error, txt%next(), ichar("b"))
			if (allocated(error)) return
			call check(error, txt%current(), ichar("b"))
			if (allocated(error)) return
			call check(error, txt%current(), ichar("b"))
			if (allocated(error)) return
			call check(error, txt%peek_back(), ichar("a"))
			if (allocated(error)) return
			call check(error, txt%next(), ichar("c"))
			if (allocated(error)) return
			call check(error, txt%current(), ichar("c"))
			if (allocated(error)) return
			call check(error, txt%next(), -1)
			if (allocated(error)) return
			call check(error, txt%current(), ichar("c"))
			if (allocated(error)) return
			call check(error, txt%next(), -1)
			if (allocated(error)) return
			call check(error, txt%current(), ichar("c"))
			if (allocated(error)) return
			call txt%first()
			call check(error, txt%current(), -1)
			if (allocated(error)) return
			call check(error, txt%next(), ichar("a"))
			if (allocated(error)) return
			call check(error, txt%current(), ichar("a"))
			if (allocated(error)) return
		end subroutine test_current

		subroutine test_last(error)
			type(error_type), allocatable, intent(out) :: error
			type(string_type) :: string
			type(textscanner) :: txt
			string = "abc"
			call txt%new(string)
			call txt%last()
			call check(error, txt%current(), ichar("c"))
			if (allocated(error)) return
		end subroutine test_last

end module tests_scanner
