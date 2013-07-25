package com.austinsalonen

object Lists99 {
	def last(given: List[Int]): Int = given match {
		case h :: Nil => h
		case h :: t => last(t)
	}

	def penultimate(given: List[Int]): Int = nth(given.length - 2, given)

	def nth(index: Int, given: List[Int]): Int = index match {
		case 0 => given.head
		case _ => nth(index - 1, given.tail)
	}

	def length(given: List[Int]): Int = {
		// let's ignore the existance of .length
		def _length(acc: Int, lst: List[Int]): Int = lst match {
			case h :: t => _length(acc + 1, t)
			case Nil => acc		
		}

		_length(0, given)
	}

	def reverse(given: List[Int]): List[Int] = {
		// let's ignore the existance of .reverse
		def _reverse(head: List[Int], tail: List[Int]): List[Int] = tail match {
			case h :: t => _reverse(h +: head, t)
			case Nil => head
		}

		_reverse(Nil, given)
	}

	def isPalindrome(given: List[Int]): Boolean = given == reverse(given)

	def flatten(given: List[Any]): List[Any] = given flatMap {
		case i: List[_] => flatten(i)
		case n => List(n)
	}

	//def compress(given: List[Any]) : List[Any] = Nil
}