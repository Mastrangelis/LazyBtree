import com.lazybtree.{BTree, MutableDLL}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object main extends App {

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

  var buckets = new ArrayBuffer[MutableDLL[MutableDLL[Int]]](5)
  for(i <- buckets.indices) buckets(i) = new MutableDLL[MutableDLL[Int]]
  var ll2 = new MutableDLL[Int]
  var ll3 = new MutableDLL[Int]
  ll2 += 100; ll2 += 101; ll2.insertAll(2, List(103, 104, 105))
  ll3 += 200; ll3 += 201; ll3.insertAll(2, List(203, 204, 205))

  for(e <- buckets.indices) buckets(e).insertAll(0, List(ll2))
  for(j <- buckets.indices) println(buckets(j))
  var ll = new MutableDLL[MutableDLL[Int]]
  ll += ll2
  println(ll)
  ll += ll3
  println(ll)
}
