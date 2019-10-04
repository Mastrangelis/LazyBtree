package com.lazybtree

object Main extends App {

  val lazyBtree = LazyBTree.empty[Int](4)
  println(lazyBtree.bPlusTree.buckets)
  //for (i <- 0 to 19) lazyBtree.insertAtBPlusTree(i)
  lazyBtree.insertAtBPlusTree(1)
  lazyBtree.insertAtBPlusTree(4)
  lazyBtree.insertAtBPlusTree(7)
  lazyBtree.insertAtBPlusTree(10)
  lazyBtree.insertAtBPlusTree(17)
  lazyBtree.insertAtBPlusTree(21)
  lazyBtree.insertAtBPlusTree(31)

  lazyBtree.insertAtBPlusTree(19)


  println(lazyBtree.getBPlusTreeKey(1))
  println(lazyBtree.getBPlusTreeKey(7))
  println("Height: " + lazyBtree.getBPlusTreeHeight)
  println("Min-key: " + lazyBtree.getBPlusTreeMinKey)
  println("Max-key: " + lazyBtree.getBPlusTreeMaxKey)
  println(lazyBtree.renderBPlusTree)
  println(lazyBtree.bPlusTree.buckets)

}
