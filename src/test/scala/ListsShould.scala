package com.austinsalonen

import org.scalatest.{FunSpec, GivenWhenThen}
import org.scalatest.matchers.ShouldMatchers

class ListsShould extends FunSpec with ShouldMatchers with GivenWhenThen {
	val numbers = List(1,1,2,3,5,8)

	def last(given: List[Int]): Int = given match {
		case h :: Nil => h
		case h :: t => last(t)
	}

	def penultimate(given: List[Int]): Int = given match {
		case h :: p :: Nil => h
		case h :: p :: t => penultimate(given.tail)
	}

	def nth(index: Int, given: List[Int]): Int = index match {
		case 0 => given.head
		case _ => nth(index - 1, given.tail)
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
}