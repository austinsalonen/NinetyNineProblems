package com.austinsalonen

import org.scalatest.{FunSpec, GivenWhenThen}
import org.scalatest.matchers.ShouldMatchers

class ListsShould extends FunSpec with ShouldMatchers with GivenWhenThen {
	val numbers = List(1,1,2,3,5,8)

	def last(given: List[Int]): Int = given match {
		case h :: Nil => h
		case h :: t => last(t)
	}

	def nth(index: Int, given: List[Int]): Int = index match {
		case 0 => given.head
		case _ => nth(index - 1, given.tail)
	}

	def length(given: List[Int]): Int = {
		// let's ignore the existance of .length
		def _length(acc: Int, lst: List[Int]): Int = lst match {
			case h :: t => _length(acc + 1, lst.tail)
			case Nil => acc		
		}

		_length(0, given)
	}

	def penultimate(given: List[Int]): Int = nth(given.length - 2, given)

	def reverse(given: List[Int]): List[Int] = {
		// let's ignore the existance of .reverse
		def _reverse(head: List[Int], tail: List[Int]): List[Int] = tail match {
			case h :: t => _reverse(h +: head, tail.tail)
			case Nil => head
		}

		_reverse(Nil, given)
	}


	describe ("Finding elements of a list") {
		it ("can find the last element"){
			when ("querying for the last item")
			val n = last(numbers)

			then ("it should be 8")
			n should be(8)
		}
	}

	describe  ("Finding the last but one element of a list") {
		it ("can find the last-1 element") {
			when ("querying for the penultimate item")
			val n = penultimate(numbers)

			then ("it should be 5")
			n should be(5)
		}
	}

	describe ("Find the Kth element of a list") {
		it ("can find the Kth element") {
			when ("querying for the Kth element")
			val n = nth(2, numbers)

			then ("it should be 2")
			n should be(2)
		}
	}

	describe ("Finding the lenght of a list") {
		it ("can find the length") {
			when ("querying for the length") 
			val len = length(numbers)

			then ("it should be 6")
			len should be(6)
		}
	}

	describe ("Reversing a list") {
		it ("can be reversed") {
			when ("reversing a list")
			var rev = reverse(numbers)

			then ("it should be reversed") 
			rev should be(List(8,5,3,2,1,1))
		}
	}
}