package com.austinsalonen

import org.scalatest.{FunSpec, GivenWhenThen}
import org.scalatest.matchers.ShouldMatchers

class ListsShould extends FunSpec with ShouldMatchers with GivenWhenThen {
	val numbers = List(1,1,2,3,5,8)

	def last(given: List[Int]): Int = given match {
		case h :: Nil => h
		case h :: t => last(t)
	}

	describe ("Finding elements of a list") {
		it ("can find the last element"){
			when ("querying for the last item")
			val n = last(numbers)

			then ("it should be 8")
			n should be(8)
		}
	}
}