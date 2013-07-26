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
	}

	describe  ("P02: Finding the last but one element of a list") {
		it ("can find the last-1 element") {
			when ("querying for the penultimate item")
			val n = Lists99.penultimate(numbers)

			then ("it should be 5")
			n should be(5)
		}
	}

	describe ("P03: Find the Kth element of a list") {
		it ("can find the Kth element") {
			when ("querying for the Kth element")
			val n = Lists99.nth(2, numbers)

			then ("it should be 2")
			n should be(2)
		}
	}

	describe ("P04: Finding the lenght of a list") {
		it ("can find the length") {
			when ("querying for the length") 
			val len = Lists99.length(numbers)

			then ("it should be 6")
			len should be(6)
		}
	}

	describe ("P05: Reversing a list") {
		it ("can be reversed") {
			when ("reversing a list")
			val rev = Lists99.reverse(numbers)

			then ("it should be reversed") 
			rev should be(List(8,5,3,2,1,1))
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
	}

	describe ("P07: Flattening a nested list structure") {
		it ("should be flattened") {
			when ("given a jagged set")
			val f = Lists99.flatten(List(List(1, 1), 2, List(3, List(5, 8))))

			then ("it should be flat")
			f should be(numbers)
		}
	}

	describe ("P08: Eliminate consecutive duplicates of alist elements") {
		it ("should be compressed") {
			when ("given a set")
			val f = Lists99.compress(letters)

			then ("it should be compressed")
			f should be(List('a, 'b, 'c, 'a, 'd, 'e))
		}
	}
}