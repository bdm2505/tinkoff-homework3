package fintech.homework03


// Реализовать интерфейс PrefixTree
// Интерфейс позволяет складывать объекты произвольного класса V по заданному "пути" Seq[K] в дерево
// и изымать их используя комбинацию методов sub и get

// Например, можно на каждом "уровне" дерева хранить Option[V] и Map[K, PrefixTree[K, V]]

trait PrefixTree[K, +V] {
  def put[U >: V](path: Seq[K], value: U): PrefixTree[K, U]

  def sub(path: Seq[K]): PrefixTree[K, V]

  def get: V

  def option: Option[V]
}

class MapPrefixTree[K, +V](val value: Option[V], val map: Map[K, MapPrefixTree[K, V]]) extends PrefixTree[K, V] {

  override def sub(path: Seq[K]): PrefixTree[K, V] = {
    def fun(path: Seq[K], tree: MapPrefixTree[K, V]): PrefixTree[K, V] = {
      if (path.isEmpty)
        tree
      else if (tree.map.contains(path.head))
        fun(path.tail, tree.map(path.head))
      else
        PrefixTree.empty
    }

    fun(path, this)
  }

  override def put[U >: V](path: Seq[K], value: U): MapPrefixTree[K, U] = {

    def getTree(path: Seq[K], currentTree: MapPrefixTree[K, U]): MapPrefixTree[K, U] = {
      if (path.isEmpty)
        PrefixTree(Some(value), currentTree.map)
      else {
        val tree =
          if (currentTree.map.contains(path.head))
            getTree(path.tail, currentTree.map(path.head))
          else
            getTree(path.tail, PrefixTree.empty)
        PrefixTree(currentTree.value, currentTree.map + (path.head -> tree))
      }
    }

    getTree(path, this)
  }

  override def toString: String = {
    val sb = new StringBuilder(s"PrefixTree( value=$value\n")

    def fun(i: Int, tree: MapPrefixTree[K, V]): Unit = {
      tree.map.foreach { case (k, v) =>
        sb.append(" " * i).append(k).append(" -> ").append(if (v.option.isDefined) v.get else "None").append("\n")
        fun(i + 2, v)
      }
    }

    fun(0, this)
    sb.append(")").toString
  }

  override def get: V = value.get

  override def option: Option[V] = value


  override def equals(other: Any): Boolean = other match {
    case that: MapPrefixTree[K, V] =>
      value == that.value && map == that.map
    case _ => false
  }

  override def hashCode(): Int = {
    31 * value.hashCode() + map.hashCode()
  }
}

object PrefixTree {

  def empty[K, V] = new MapPrefixTree[K, V](None, Map.empty)

  def apply[K, V](value: Option[V], map: Map[K, MapPrefixTree[K, V]]): MapPrefixTree[K, V] =
    new MapPrefixTree(value, map)

  def put[K, V](path: Seq[K], value: V): MapPrefixTree[K, V] = empty.put(path, value)

}
