package com.lazybtree

object Main extends App {

  // Starting with an empty tree
  var btree = {
    BTree.empty[Int, Int](2)
  }
  //Insert some values on the tree
  for (i <- 1 to 15) {
    btree = btree.insert(i, i);
  }
  // Find the height of the tree
  val height = {
    val h = btree.getHeight
    if (h == 0) "Empty tree. Height: 0"
    else s"It's height is: $h"
  }
  // Find the maximum key
  val maxKey = btree.getMaxKey
  // Fund the minimum Key
  val minKey = btree.getMinKey
  // Render b-tree structure
  val rendering = btree.renderStructure
  // Print info
  println("\n\n*********************** STRUCTURE ***********************\n" + btree)
  println("\n\n*********************** RENDERING ***********************\n" + rendering)
  println("\n\n***********************  HEIGHT  ***********************\n" + height)
  println("\n\n*********************** MAX-KEY  ***********************\n" + maxKey)
  println("\n\n*********************** MIN-KEY  ***********************\n" + minKey)


 val lazyBtree = LazyBTree.empty[Int, Int](2)
 lazyBtree.viewBuckets()

}
