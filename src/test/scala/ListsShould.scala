package com.austinsalonen

import org.scalatest.{FunSpec, GivenWhenThen}
import org.scalatest.matchers.ShouldMatchers

class ListsShould extends FunSpec with ShouldMatchers with GivenWhenThen {
	val numbers = List(1,1,2,3,5,8)
	val letters = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

	describe ("P01: Finding elements of a list") {
		it ("can find the last element"){
			when ("querying for the last item")
			val n = Lists99.last(numbers)

			then ("it should be 8")
			n should be(8)
		}

		it ("should handle Nil") {
			when ("given Nil") 
			val f = Lists99.last(Nil)

			then ("it should be Nil")
			f should be(Nil)
		}
	}

	describe  ("P02: Finding the last but one element of a list") {
		it ("can find the last-1 element") {
			when ("querying for the penultimate item")
			val n = Lists99.penultimate(numbers)

			then ("it should be 5")
			n should be(5)
		}

		it ("should handle Nil") {
			when ("given Nil") 
			val f = Lists99.penultimate(Nil)

			then ("it should be Nil")
			f should be(Nil)
		}
	}

	describe ("P03: Find the Kth element of a list") {
		it ("can find the Kth element") {
			when ("querying for the Kth element")
			val n = Lists99.nth(2, numbers)

			then ("it should be 2")
			n should be(2)
		}

		it ("should handle a negative index") {
			when ("given a negative number") 
			val f = Lists99.nth(-1, numbers)

			then ("it should be Nil")
			f should be(Nil)
		}

		it ("should handle Nil") {
			when ("given Nil") 
			val f = Lists99.nth(0, Nil)

			then ("it should be Nil")
			f should be(Nil)
		}
	}

	describe ("P04: Finding the lenght of a list") {
		it ("can find the length") {
			when ("querying for the length") 
			val len = Lists99.length(numbers)

			then ("it should be 6")
			len should be(6)
		}

		it ("should handle Nil") {
			when ("given Nil") 
			val f = Lists99.length(Nil)

			then ("it should be Nil")
			f should be(0)
		}
	}

	describe ("P05: Reversing a list") {
		it ("can be reversed") {
			when ("reversing a list")
			val rev = Lists99.reverse(numbers)

			then ("it should be reversed") 
			rev should be(List(8,5,3,2,1,1))
		}

		it ("should handle Nil") {
			when ("given Nil") 
			val f = Lists99.reverse(Nil)

			then ("it should be Nil")
			f should be(Nil)
		}
	}

	describe ("P06: Finding out whether a list is a palindrome") {
		it ("is not a palindrome") {
			when ("not using a palindrome") 
			val p = Lists99.isPalindrome(numbers)

			then ("it should be false") 
			p should be(false)
		}

		it ("is a palindrome") {
			when ("using a known palindrome") 
			val p = Lists99.isPalindrome(List(1, 2, 3, 2, 1))

			then ("it should be true") 
			p should be(true)
		}

		it ("should handle Nil") {
			when ("given Nil") 
			val f = Lists99.isPalindrome(Nil)

			then ("it should be Nil")
			f should be(false)
		}
	}

	describe ("P07: Flattening a nested list structure") {
		it ("should be flattened") {
			when ("given a jagged set")
			val f = Lists99.flatten(List(List(1, 1), 2, List(3, List(5, 8))))

			then ("it should be flat")
			f should be(numbers)
		}

		it ("should handle Nil") {
			when ("given Nil") 
			val f = Lists99.flatten(Nil)

			then ("it should be Nil")
			f should be(Nil)
		}
	}

	describe ("P08: Eliminate consecutive duplicates of alist elements") {
		it ("should be compressed") {
			when ("given a set")
			val f = Lists99.compress(letters)

			then ("it should be compressed")
			f should be(List('a, 'b, 'c, 'a, 'd, 'e))
		}

		it ("should handle Nil") {
			when ("given Nil") 
			val f = Lists99.compress(Nil)

			then ("it should be Nil")
			f should be(Nil)
		}
	}

	describe ("P09: Pack consecutive duplicates of list elements") {
		it ("should create a list of lists") {
			when ("given a set to pack") 
			val f = Lists99.pack(letters)

			then ("it should be a list of lists")
			f should be(List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
		}

		it ("should handle Nil") {
			when ("given Nil") 
			val f = Lists99.pack(Nil)

			then ("it should be Nil")
			f should be(Nil)
		}
	}

	describe ("P10: Run-length encoding of a list.") {
		it ("should RLE a list") {
			when ("RL encoding the list") 
			val rle = Lists99.encode(letters)

			then ("it should be tuples")
			rle should be(List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
		}

		it ("should handle Nil") {
			when ("given Nil") 
			val rle = Lists99.encode(Nil)

			then ("it should be Nil")
			rle should be(Nil)
		}
	}

	describe ("P11: Modified run-length encoding.") {
		it ("should RLE a list") {
			when ("RL encoding the list") 
			val rle = Lists99.encodeModified(letters)

			then ("it should be tuples")
			rle should be(List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)))
		}	

		it ("should handle Nil") {
			when ("given Nil") 
			val rle = Lists99.encodeModified(Nil)

			then ("it should be Nil")
			rle should be(Nil)
		}
	}

	describe ("P12: Decode a run-length encoded list.") { 
		it ("should decode a RLE'd list") {
			when ("decoding the RLE'd list")
			val dec = Lists99.decode(List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))

			then ("it should be the original list") 
			dec should be(letters)
		}

		it ("should handle Nil") {
			when ("given Nil")
			val dec = Lists99.decode(Nil)

			then ("it should be Nil")
			dec should be(Nil)
		}
	}

	describe ("P13: Run-length encoding of a list (direct solution).") {
		it ("should RLE a list without supporting methods") {
			when ("RL encoding the list")
			val rle = Lists99.encodeDirect(letters)

			then ("it should be tuples")
			rle should be(List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
		}

		it ("should yield Nil when given Nil") {
			when ("given Nil")
			val rle = Lists99.encodeDirect(Nil)

			then ("it should be Nil")
			rle should be(Nil)
		}
	}
}