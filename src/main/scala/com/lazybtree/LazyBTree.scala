package com.lazybtree

case class LazyBTree[K](var bPlusTree: BPlusTree[K], order: Int)(implicit keyOrdering: Ordering[K]) {

  //def listBuckets(): Unit =
   // println(this.btree.buckets)
  // BTree ~ methods
  def insertAtBPlusTree(key: K): Unit = {
    val newBPlusTree: BPlusTree[K] = this.bPlusTree.insert(key)
    this.bPlusTree = newBPlusTree
  }
  def getBPlusTreeKey(key: K): Option[K] = this.bPlusTree.get(key)
  def getBPlusTreeMinKey: K = this.bPlusTree.getMinKey
  def getBPlusTreeMaxKey: K = this.bPlusTree.getMaxKey
  def getBPlusTreeHeight: Int = this.bPlusTree.getHeight
  def renderBPlusTree: String = this.bPlusTree.renderStructure

}

object LazyBTree {
  class Lij[K] extends MutableDLL[K] {
    def LTB(x: K): Int = 1
  }
  class Li[Lij] extends MutableDLL[Lij]
  class L[Li] extends MutableDLL[Li]
  def empty[K: Ordering](order: Int): LazyBTree[K] = {
    val initBtree = BPlusTree.empty[K](order);
    LazyBTree[K](initBtree, order)
  }
}
