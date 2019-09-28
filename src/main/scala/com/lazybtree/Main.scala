package com.lazybtree

object Main extends App {

  val lazyBtree = LazyBTree.empty[Int](4)
  println(lazyBtree.bPlusTree.buckets)
  lazyBtree.insertAtBPlusTree(1)
  lazyBtree.insertAtBPlusTree(4)
  lazyBtree.insertAtBPlusTree(7)
  lazyBtree.insertAtBPlusTree(10)
  lazyBtree.insertAtBPlusTree(17)
  lazyBtree.insertAtBPlusTree(21)
  lazyBtree.insertAtBPlusTree(31)
  lazyBtree.insertAtBPlusTree(25)
  lazyBtree.insertAtBPlusTree(19)
  lazyBtree.insertAtBPlusTree(20)
  lazyBtree.insertAtBPlusTree(28)
  lazyBtree.insertAtBPlusTree(42)
  lazyBtree.insertAtBPlusTree(51)
  lazyBtree.insertAtBPlusTree(61)
  lazyBtree.insertAtBPlusTree(71)
  lazyBtree.insertAtBPlusTree(81)


  println(lazyBtree.getBPlusTreeKey(1))
  println(lazyBtree.getBPlusTreeKey(7))
  println("Height: " + lazyBtree.getBPlusTreeHeight)
  println("Min-key: " + lazyBtree.getBPlusTreeMinKey)
  println("Max-key: " + lazyBtree.getBPlusTreeMaxKey)
  println(lazyBtree.renderBPlusTree)
  println(lazyBtree.bPlusTree.buckets)

}
