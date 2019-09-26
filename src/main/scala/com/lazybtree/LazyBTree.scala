package com.lazybtree

import com.lazybtree.BTree.Entry

case class LazyBTree[K, V](var buckets: LazyBTree.Buckets[MutableDLL[K]], btree: BTree[K,V], order: Int)(implicit keyOrdering: Ordering[K]) {
  private type B = LazyBTree.Buckets[MutableDLL[K]]
  buckets.
  def viewBuckets(): Unit = {
    for (i <- 0 to buckets.length) println(s"Bucket($i): $buckets(i)")
  }

  def splitBucket: Boolean = true
  def fuseBuckets: Boolean = false
}

object LazyBTree {
  case class Buckets[MutableDLL[K]](size: Int)
  def empty[K: Ordering, V](order: Int): LazyBTree[K, V] = {
    val initBtree: BTree[K,V] = BTree.empty[K, V](2)
    println(initBtree.getHeight)
    LazyBTree[K, V](Buckets[MutableDLL[K]](5),initBtree, order)
  }
}
