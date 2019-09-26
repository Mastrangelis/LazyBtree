package com.lazybtree

object Main extends App {

  val lazyBtree = LazyBTree.empty[Int, Int](2)
  lazyBtree.listBuckets()
  lazyBtree.insert(1, 1);
  lazyBtree.insert(2, 2)
  lazyBtree.insert(3, 3)
  lazyBtree.insert(4, 4)
  lazyBtree.insert(5, 5)
  lazyBtree.insert(6, 6)
  lazyBtree.insert(7, 7)
  lazyBtree.insert(8, 8)
  println(lazyBtree)
  println(lazyBtree.btree)
  println(lazyBtree.buckets)
  println(lazyBtree.getKey(5))
  println(lazyBtree.getKey(7))
  println("Height: " + lazyBtree.getBTreeHeight)
  println("Min-key: " + lazyBtree.getMinKey)
  println("Max-key: " + lazyBtree.getMaxKey)
  println(lazyBtree.renderBTree)

}
