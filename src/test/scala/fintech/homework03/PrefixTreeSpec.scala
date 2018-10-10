package fintech.homework03
import org.scalatest.{FlatSpec, Matchers}

class PrefixTreeSpec extends FlatSpec with Matchers {

  it should "work well with any types" in {
    val withInt: PrefixTree[Char, Int] = PrefixTree.put("qw", 15)
    val withIntAndDouble: PrefixTree[Char, AnyVal] = withInt.put("abcde", 13.0)
    val withAny: PrefixTree[Char, Any] = withIntAndDouble.put("a", 89 -> "klaRa")
    println(withAny)
  }

  it should "function equals" in {
    val tree1 = PrefixTree.put("abcd", 34).put("acn", 54)
    val tree2 = PrefixTree.put("abcd", 34).put("acn", 54)

    tree1 shouldEqual tree2

    val tree3 = PrefixTree.put("abcd", 34).put("aer", 54)
    val tree4 = PrefixTree.put("abcd", 34).put("acn", 55)

    tree1 should not equal tree3
    tree1 should not equal tree4

  }

  it should "function sub and put" in {
    val tree = PrefixTree.put("abcd", 34).put("acn", 54)

    tree.sub("abcd").get shouldEqual 34
    tree.sub("acn").get shouldEqual 54
    tree.sub("a").option shouldEqual None
    assertThrows[NoSuchElementException]{
      tree.sub("unknown path")
    }
  }

}
