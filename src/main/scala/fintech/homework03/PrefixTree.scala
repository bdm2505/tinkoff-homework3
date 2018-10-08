package fintech.homework03


// Реализовать интерфейс PrefixTree
// Интерфейс позволяет складывать объекты произвольного класса V по заданному "пути" Seq[K] в дерево
// и изымать их используя комбинацию методов sub и get

// Например, можно на каждом "уровне" дерева хранить Option[V] и Map[K, PrefixTree[K, V]]

trait PrefixTree[K, +V] {
  def put[U >: V](path: Seq[K], value: U): PrefixTree[K, U]

  def sub(path: Seq[K]): PrefixTree[K, V]

  def get: V

  def apply(): Option[V]
}

class MapPrefixTree[K, +V](val value: Option[V], val map: Map[K, MapPrefixTree[K, V]]) extends PrefixTree[K, V] {

  override def sub(path: Seq[K]): PrefixTree[K, V] = {
    def fun(path: Seq[K], tree: MapPrefixTree[K, V]): PrefixTree[K, V] = {
      if (path.isEmpty)
        tree
      else
        fun(path.tail, tree.map(path.head))
    }

    fun(path, this)
  }

  override def put[U >: V](path: Seq[K], value: U): PrefixTree[K, U] = ???

  override def toString: String = {
    val sb = new StringBuilder(s"PrefixTree( value=$value\n")

    def fun(i: Int, tree: MapPrefixTree[K, V]): Unit = {
      tree.map.foreach { case (k, v) =>
        sb.append(" " * i).append(k).append(" -> ").append(if (v().isDefined) v.get else "None").append("\n")
        fun(i + 2, v)
      }
    }

    fun(0, this)
    sb.append(")").toString
  }

  override def get: V = value.get

  override def apply(): Option[V] = value
}

object PrefixTree {

  def empty[K, V] = new MapPrefixTree[K, V](None, Map.empty)

  def apply[K, V](value: Option[V], map: Map[K, MapPrefixTree[K, V]]): MapPrefixTree[K, V] =
    new MapPrefixTree(value, map)

  implicit def toPrefixTree[K, V](elem: V): MapPrefixTree[K, V] = {
    apply[K, V](Some(elem), Map.empty[K, MapPrefixTree[K, V]])
  }

}
