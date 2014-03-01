package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("weight of leaf") {
    new TestTrees {
      assert(weight( new Leaf('a', 5)) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("chars of leaf") {
    new TestTrees {
      assert( chars(new Leaf('a', 5)) === List('a'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times") {
    assert( times( string2Chars("Hello world")) === List(('H', 1),('e', 1),('l', 3),('o', 2),(' ', 1), ('w', 1),('r', 1),('d', 1)))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  /*
  test("makeOrderedLeafList - to be or not to be") {
    assert(makeOrderedLeafList( times( string2Chars("to be or not to be")) ) === List(Leaf('n',1), Leaf('r',1), Leaf('e',2), Leaf('b',2), Leaf('t',3), Leaf('o',4), Leaf(' ',5)) )
  }
  */

  test("optimal encoding test") {
    val charslist = string2Chars("to be or not to be")
    assert(encode(createCodeTree(charslist))(charslist).length===47)
  }

  test("singleton ") {
    assert( !singleton(Leaf('a',2) :: Leaf('b',3) :: List()))
    assert( !singleton(List()))
    assert( singleton(Leaf('a',2) :: List()))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("createCodeTree") {
    new TestTrees {
      assert(createCodeTree("ababb".toList) == t1)
    }
  }

  test("secret") {
    print (decodedSecret)
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and encode a much longer piece") {
    new TestTrees {
      val tree = createCodeTree("Hello world this is".toList)
      assert(decode(tree, encode(tree)("Hello world this is".toList)) === "Hello world this is".toList)
    }
  }

  test("codebits") {
    val aBits: List[Int] = 0 :: 1 :: 0 :: List()
    val codeTable = ('a', aBits) :: ('b', 1::1::1::List()) :: List()
    assert(codeBits(codeTable)('a') === aBits)
  }

  test("convert") {
    new TestTrees {
      val aBits: List[Int] = 0 :: 0 :: List()
      val bBits: List[Int] = 0 :: 1 :: List()
      val dBits: List[Int] = 1 :: List()

      val codeTable = ('a', aBits) :: ('b', bBits) :: ('d', dBits) :: List()
      assert( convert(t2) === codeTable)
    }
  }

  test("decode and quick encode a much longer piece") {
    new TestTrees {
      val tree = createCodeTree("Hello world this is".toList)
      assert(decode(tree, quickEncode(tree)("Hello world this is".toList)) === "Hello world this is".toList)
    }
  }
}
