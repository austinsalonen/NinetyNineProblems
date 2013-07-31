package com.austinsalonen

object Lists99 {
	def last(given: List[Any]): Any = given match {
		case Nil => Nil
		case h :: Nil => h
		case h :: t => last(t)
	}

	def penultimate(given: List[Any]): Any = nth(given.length - 2, given)

	def nth(index: Int, given: List[Any]): Any = { 
		if (given.isEmpty) Nil
		else {
			index match {
				case neg if neg < 0 => Nil
				case 0 => given.head
				case _ => nth(index - 1, given.tail)
			}
		}
	}


	def length(given: List[Any]): Int = {
		def _length(acc: Int, lst: List[Any]): Int = lst match {
			case h :: t => _length(acc + 1, t)
			case Nil => acc		
		}

		_length(0, given)
	}

	def reverse(given: List[Any]): List[Any] = {
		// let's ignore the existance of .reverse
		def _reverse(head: List[Any], tail: List[Any]): List[Any] = tail match {
			case h :: t => _reverse(h +: head, t)
			case Nil => head
		}

		_reverse(Nil, given)
	}

	def isPalindrome(given: List[Any]): Boolean = !(given.isEmpty) && given == reverse(given)

	def flatten(given: List[Any]): List[Any] = given flatMap {
		case i: List[_] => flatten(i)
		case n => List(n)
	}

	def compress(given: List[Any]) : List[Any] = {
		def _compress(acc: List[Any], rest: List[Any]): List[Any] = rest match {
			case Nil => acc
			case h :: t => _compress(acc :+ h, t dropWhile (_ == h))
		}

		_compress(Nil, given)
	}

	def pack(given: List[Any]) : List[List[Any]] = {
		def _pack(acc: List[List[Any]], rest: List[Any]) : List[List[Any]] = rest match {
			case Nil => acc
			case h :: t => { 
				val (leading,trailing) = rest span {_ == h}
				_pack(acc :+ leading, trailing)
			}
		}

		_pack(Nil, given)
	}

	def encode(given: List[Any]) : List[Any] = pack(given) map (lst => (lst.length, lst.head))

	def encodeModified(given: List[Any]) : List[Any] = {
		def _modified(itm: Any) : Any = itm match {
			case (1, x) => x
			case tup => tup
		}

		encode(given) map _modified
	}

	def decode(encoded: List[Tuple2[Int,Any]]) : List[Any] = {
		def _expand(itm: Tuple2[Int,Any]) : List[Any] = List.fill(itm._1)(itm._2)
		encoded flatMap _expand
	}

	def encodeDirect(given: List[Any]) : List[Tuple2[Int, Any]] = {
		def rle(lst: List[Any]): Tuple2[Int,Any] = (lst.length, lst.head)

		if (given.isEmpty) Nil
		else {
			val (leading, trailing) = given span {_ == given.head}
			rle(leading) :: encodeDirect(trailing)	
		}
	}
}