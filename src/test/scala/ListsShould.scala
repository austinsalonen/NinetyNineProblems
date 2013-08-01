package com.austinsalonen

import org.scalatest.{FunSpec, GivenWhenThen}
import org.scalatest.matchers.ShouldMatchers

class ListsShould extends FunSpec with ShouldMatchers with GivenWhenThen {
	import Lists99._
	val numbers = List(1,1,2,3,5,8)
	val letters = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

	describe ("P01: Finding elements of a list") {
		it ("can find the last element"){
			when ("querying for the last item")
			val n = last(numbers)

			then ("it should be 8")
			n should be(8)
		}

		it ("should handle Nil") {
			when ("given Nil") 
			val f = last(Nil)

			then ("it should be Nil")
			f should be(Nil)
		}
	}

	describe  ("P02: Finding the last but one element of a list") {
		it ("can find the last-1 element") {
			when ("querying for the penultimate item")
			val n = penultimate(numbers)

			then ("it should be 5")
			n should be(5)
		}

		it ("should handle Nil") {
			when ("given Nil") 
			val f = penultimate(Nil)

			then ("it should be Nil")
			f should be(Nil)
		}
	}

	describe ("P03: Find the Kth element of a list") {
		it ("can find the Kth element") {
			when ("querying for the Kth element")
			val n = nth(2, numbers)

			then ("it should be 2")
			n should be(2)
		}

		it ("should handle a negative index") {
			when ("given a negative number") 
			val f = nth(-1, numbers)

			then ("it should be Nil")
			f should be(Nil)
		}

		it ("should handle a too large index") {
			when ("given a large number") 
			val f = nth(100, numbers)

			then ("it should be Nil")
			f should be(Nil)
		}

		it ("should handle Nil") {
			when ("given Nil") 
			val f = nth(0, Nil)

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
			val rev = reverse(numbers)

			then ("it should be reversed") 
			rev should be(List(8,5,3,2,1,1))
		}

		it ("should handle Nil") {
			when ("given Nil") 
			val f = reverse(Nil)

			then ("it should be Nil")
			f should be(Nil)
		}
	}

	describe ("P06: Finding out whether a list is a palindrome") {
		it ("is not a palindrome") {
			when ("not using a palindrome") 
			val p = isPalindrome(numbers)

			then ("it should be false") 
			p should be(false)
		}

		it ("is a palindrome") {
			when ("using a known palindrome") 
			val p = isPalindrome(List(1, 2, 3, 2, 1))

			then ("it should be true") 
			p should be(true)
		}

		it ("should handle Nil") {
			when ("given Nil") 
			val f = isPalindrome(Nil)

			then ("it should be Nil")
			f should be(false)
		}
	}

	describe ("P07: Flattening a nested list structure") {
		it ("should be flattened") {
			when ("given a jagged set")
			val f = flatten(List(List(1, 1), 2, List(3, List(5, 8))))

			then ("it should be flat")
			f should be(numbers)
		}

		it ("should handle Nil") {
			when ("given Nil") 
			val f = flatten(Nil)

			then ("it should be Nil")
			f should be(Nil)
		}
	}

	describe ("P08: Eliminate consecutive duplicates of alist elements") {
		it ("should be compressed") {
			when ("given a set")
			val f = compress(letters)

			then ("it should be compressed")
			f should be(List('a, 'b, 'c, 'a, 'd, 'e))
		}

		it ("should handle Nil") {
			when ("given Nil") 
			val f = compress(Nil)

			then ("it should be Nil")
			f should be(Nil)
		}
	}

	describe ("P09: Pack consecutive duplicates of list elements") {
		it ("should create a list of lists") {
			when ("given a set to pack") 
			val f = pack(letters)

			then ("it should be a list of lists")
			f should be(List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
		}

		it ("should handle Nil") {
			when ("given Nil") 
			val f = pack(Nil)

			then ("it should be Nil")
			f should be(Nil)
		}
	}

	describe ("P10: Run-length encoding of a list.") {
		it ("should RLE a list") {
			when ("RL encoding the list") 
			val rle = encode(letters)

			then ("it should be tuples")
			rle should be(List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
		}

		it ("should handle Nil") {
			when ("given Nil") 
			val rle = encode(Nil)

			then ("it should be Nil")
			rle should be(Nil)
		}
	}

	describe ("P11: Modified run-length encoding.") {
		it ("should RLE a list") {
			when ("RL encoding the list") 
			val rle = encodeModified(letters)

			then ("it should be tuples")
			rle should be(List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)))
		}	

		it ("should handle Nil") {
			when ("given Nil") 
			val rle = encodeModified(Nil)

			then ("it should be Nil")
			rle should be(Nil)
		}
	}

	describe ("P12: Decode a run-length encoded list.") { 
		it ("should decode a RLE'd list") {
			when ("decoding the RLE'd list")
			val dec = decode(List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))

			then ("it should be the original list") 
			dec should be(letters)
		}

		it ("should handle Nil") {
			when ("given Nil")
			val dec = decode(Nil)

			then ("it should be Nil")
			dec should be(Nil)
		}
	}

	describe ("P13: Run-length encoding of a list (direct solution).") {
		it ("should RLE a list without supporting methods") {
			when ("RL encoding the list")
			val rle = encodeDirect(letters)

			then ("it should be tuples")
			rle should be(List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
		}

		it ("should yield Nil when given Nil") {
			when ("given Nil")
			val rle = encodeDirect(Nil)

			then ("it should be Nil")
			rle should be(Nil)
		}
	}

	describe ("P14: Duplicate the elements of a list.") {
		it ("should duplicate a given list") {
			when ("duplicating a list") 
			val dup = duplicate(List('a, 'b, 'c, 'c, 'd))

			then ("it should be doubled up")
			dup should be(List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
		}

		it ("should handle Nil") {
			when ("given Nil") 
			val dup = duplicate(Nil)

			then ("it should be Nil")
			dup should be(Nil)
		}
	}

	describe ("P15: Duplicate the elements of a list a given number of times.") {
		it ("should duplicate a given list") {
			when ("duplicating a list") 
			val dup = duplicateN(3, List('a, 'b, 'c, 'c, 'd))

			then ("it should be tripled up")
			dup should be(List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
		}

		it ("should handle Nil") {
			when ("given Nil") 
			val dup = duplicateN(4, Nil)

			then ("it should be Nil")
			dup should be(Nil)
		}
	}

	describe ("P16: Drop every Nth element from a list.") {
		it ("should drop every 3rd item of a given list") {
			when ("dropping items in a list") 
			val dup = drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

			then ("it should be without itmes")
			dup should be(List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
		}

		it ("should handle Nil") {
			when ("given Nil") 
			val f = drop(4, Nil)

			then ("it should be Nil")
			f should be(Nil)
		}
	}

	describe ("P17: Split a list into two parts.") {
		it ("should split a list") {
			when ("splitting a list") 
			val parts = split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

			then ("there should be two lists")
			parts should be((List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
		}

		it ("should handle Nil") {
			when ("given Nil") 
			val f = split(4, Nil)

			then ("it should be Nil")
			f should be(Tuple2(Nil,Nil))
		}
	}

	describe ("P18: Extract a slice from a list") {
		it ("should slice the list") {
			when ("slicing a list") 
			val f = slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

			then ("it should be expected")
			f should be(List('d, 'e, 'f, 'g))
		}

		it ("should handle Nil") {
			when ("given Nil") 
			val f = slice(4, 6, Nil)

			then ("it should be Nil")
			f should be(Nil)
		}
	}

	describe ("P19: Rotate a list N places to the left") {
		it ("should rotate left") {
			when ("rotating left")
			val f = rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

			then ("it should be rotated")
			f should be (List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
		}

		it ("should rotate right") {
			when ("rotating left")
			val f = rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

			then ("it should be rotated")
			f should be (List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
		}

		it ("should handle Nil") {
			when ("given Nil") 
			val f = rotate(4, Nil)

			then ("it should be Nil")
			f should be(Nil)
		}
	}

	describe ("P20: Remove the Kth element from a list") {
		it ("should remove the element") {
			when ("removing at a position") 
			val f = removeAt(1, List('a, 'b, 'c, 'd))

			then ("it should yield a shortened list and the value")
			f should be((List('a, 'c, 'd),'b))
		}

		it ("should handle Nil") {
			when ("given Nil") 
			val f = removeAt(4, Nil)

			then ("it should be Nil")
			f should be(Tuple2(Nil,Nil))
		}
	}

	describe ("P21: Insert an element at a given position into a list") {
		it ("should insert an item") {
			when ("inserting an item") 
			val f = insertAt('new, 1, List('a, 'b, 'c, 'd))

			then ("there should be a new list")
			f should be(List('a, 'new, 'b, 'c, 'd))
		}
	}

	describe ("P22: Create a list containing all integers within a given range") {
		it ("should yield a range") {
			when ("creating a range")
			val f = range(4,9)

			then ("it should yield an inclusive range") 
			f should be(List(4,5,6,7,8,9))
		}
	}

	describe ("P23: Extract a given number of randomly selected elements from a list.") {
		it ("should extract n values") {
			when ("values are extracted")
			val f = randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))

			then ("it should have n items")
			f.length should be(3)
		}
	}
}