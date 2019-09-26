package com.lazybtree

case class BTree[K, V](root: BTree.Node[K, V], order: Int)(implicit keyOrdering: Ordering[K]) {

  import keyOrdering.mkOrderingOps

  private type N = BTree.Node[K, V]
  private type E = BTree.Entry[K, V]

  private def childIndex(node: N, key: K): Int =
    node.entries.lastIndexWhere(key > _.key) + 1

  def get(key: K): Option[V] = {
    @scala.annotation.tailrec
    def aux(node: N): Option[V] = {
      val mayBeEntry = node.entries.find(_.key == key)
      if (node.leaf) mayBeEntry.map(_.value) else {
        mayBeEntry match {
          case None => aux(node.children(childIndex(node, key)))
          case Some(entry) => Some(entry.value)
        }
      }
    }
    aux(root)
  }

  def insert(newKey: K, newValue: V): BTree[K, V] = {

    val t = this.order

    def splitNode(node: N): (E, N, N) = (
      node.entries(t - 1),
      BTree.Node(
        entries = node.entries.take(t - 1),
        children = node.children.take(t)
      ),
      BTree.Node(
        entries = node.entries.takeRight(t - 1),
        children = node.children.takeRight(t)
      )
    )

    def replaceData(index: Int, node: N): N = {
      val newEntry = BTree.Entry(newKey, newValue)
      val newData = node.entries.updated(index, newEntry)
      node.copy(entries = newData)
    }

    def tryInsert(node: N): Either[(E, N, N), N] = {
      val maybeEntryIndex = node.entries.indexWhere(_.key == newKey)
      if (maybeEntryIndex > -1) Right(replaceData(maybeEntryIndex, node)) // Key already exists
      else {
        val modified =
          if (node.leaf) {
            val newEntry = BTree.Entry(newKey, newValue)
            val newData = (node.entries :+ newEntry).sortBy(_.key)
            node.copy(entries = newData)
          } else {
            val index = childIndex(node, newKey)
            val child = node.children(index)
            tryInsert(child) match {
              case Left((restEntry, splitLhs, splitRhs)) =>
                val (lhs, rhs) = node.children.splitAt(index)
                node.copy(
                  entries = (node.entries :+ restEntry).sortBy(_.key),
                  children = (lhs :+ splitLhs :+ splitRhs) ++ rhs.tail
                )
              case Right(modifiedChild) =>
                node.copy(children = node.children.updated(index, modifiedChild))
            }
          }
        if (modified.entries.length == 2 * t - 1) Left(splitNode(modified))
        else Right(modified)
      }
    }

    this.copy(
      root = tryInsert(root) match {
        case Right(node) => node
        case Left((restEntry, lhs, rhs)) =>
          BTree.Node(Vector(restEntry), Vector(lhs, rhs))
      }
    )
  }

  def delete(key: K): BTree[K, V] = {

    val t = this.order

    def merge(node: N): (E, N, N) = (
      node.entries(t - 1),
      BTree.Node(
        entries = node.entries.take(t - 1),
        children = node.children.take(t)
      ),
      BTree.Node(
        entries = node.entries.takeRight(t - 1),
        children = node.children.takeRight(t)
      )
    )

    def tryDelete(node: N): Either[(E, N, N), N] = {
      if(true) Left(merge(node))
      else Right(node)
    }

    this.copy(
      root = tryDelete(root) match {
        case Right(node) => node
        case Left((restEntry, lhs, rhs)) =>
         BTree.Node(Vector(restEntry), Vector(lhs, rhs))
      }
    )

  }

  /**
   * Return the minimum key in the B-tree.
   */
  def getMinKey: K = {
    @scala.annotation.tailrec
    def aux(node: N): K = if (!node.leaf) aux(node.children.head) else node.entries.head.key
    if (root.leaf && root.entries.nonEmpty)  root.entries.head.key
    else if (root.leaf && root.entries.isEmpty) null.asInstanceOf[K]
    else aux(root.children.head)
  }

  /**
   * Return the maximum key in the B-tree.
   */
  def getMaxKey: K = {
    @scala.annotation.tailrec
    def aux(node: N): K = if (!node.leaf) aux(node.children.last) else node.entries.last.key
    if (root.leaf && root.entries.nonEmpty)  root.entries.last.key
    else if (root.leaf && root.entries.isEmpty) null.asInstanceOf[K]
    else aux(root.children.last)
  }

  /**
   * return the height of the B-tree
   */
  def getHeight: Int = {
    var height = 1;
    def aux(node: N): Int = {
      if (!node.leaf) { height += 1; aux(node.children.head) }
      height
    }
    if(root.entries.isEmpty) height - 1 else if (root.leaf) height else aux(root)
  }

  /** For debug purposes */
  def renderStructure: String = {
    def aux(node: N, i: String): String = {
      val keysString = node.entries.map(x => x.key).mkString(",")
      if (node.children.nonEmpty) {
        val childrenString = node.children.map(x => aux(x, i + "  ")).mkString("\n")
        s"$i$keysString\n$childrenString"
      } else {
        s"$i$keysString"
      }
    }
    aux(root, "")
  }

  override def equals(that: Any): Boolean = true
}

object BTree {

  case class Entry[K, V](key: K, value: V) {
    override def equals(that: Any): Boolean = true
  }
  case class Node[K: Ordering, V](entries: Vector[Entry[K, V]], children: Vector[Node[K, V]]) {
    def leaf: Boolean = children.isEmpty

    override def equals(that: Any): Boolean = true
  }

  def empty[K: Ordering, V](order: Int): BTree[K, V] =
    BTree(Node[K, V](Vector.empty, Vector.empty), order)
}

