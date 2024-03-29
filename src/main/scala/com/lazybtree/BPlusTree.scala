package com.lazybtree

import com.lazybtree.BPlusTree.{Bucket, BucketData}

import scala.collection.mutable.ArrayBuffer

case class BPlusTree[K](root: BPlusTree.Node[K], buckets: BPlusTree.Buckets[BPlusTree.Bucket[BPlusTree.BucketData[K]]], order: Int)(implicit keyOrdering: Ordering[K]) {

  import keyOrdering.mkOrderingOps

  private type N = BPlusTree.Node[K]
  private type E = BPlusTree.Entry[K]

  private type BiData = BPlusTree.BucketData[K]
  private type Bi = BPlusTree.Bucket[BiData]
  private type B = BPlusTree.Buckets[Bi]


  private def childIndex(node: N, key: K): Int =
    node.keys.lastIndexWhere(key > _.key) + 1

  def get(key: K): Option[K] = {
    @scala.annotation.tailrec
    def aux(node: N): Option[K] = {
      val mayBeEntry = node.keys.find(_.key == key)
      if (node.leaf) mayBeEntry.map(_.key) else {
        mayBeEntry match {
          case None => aux(node.children(childIndex(node, key)))
          case Some(entry) => Some(entry.key)
        }
      }
    }
    aux(root)
  }

  def insert(newKey: K): BPlusTree[K] = {

    val m = this.order
    var bucketData = null.asInstanceOf[BiData]

    def splitNode(node: N): (E, N, N) = (
      node.keys(math.floor(m / 2).toInt),
      BPlusTree.Node(keys = node.keys.take(math.floor(m / 2).toInt), children = node.children.take(math.floor(m / 2).toInt + 1)),
      BPlusTree.Node(keys = node.keys.takeRight(m - math.floor(m / 2).toInt), children = node.children.takeRight((m - math.floor(m / 2).toInt) + 1))
    )

    def replaceData(index: Int, node: N): N = {
      val newEntry = BPlusTree.Entry(newKey)
      val newData = node.keys.updated(index, newEntry)
      node.copy(keys = newData)
    }

    def tryInsert(node: N): Either[(E, N, N), N] = {
      val maybeEntryIndex = node.keys.indexWhere(_.key == newKey)
      if (maybeEntryIndex > -1) Right(replaceData(maybeEntryIndex, node)) // Key already exists
      else {
        val modified =
          if (node.leaf) {
            val newEntry = BPlusTree.Entry(newKey);
            val newData = (node.keys :+ newEntry).sortBy(_.key)
            bucketData = BPlusTree.BucketData(newEntry.key)
            node.copy(keys = newData)
          } else {
            val index = childIndex(node, newKey)
            val child = node.children(index)
            tryInsert(child) match {
              case Left((restEntry, splitLhs, splitRhs)) =>
                val newRhs = removeDuplicateIndexOnInternalNodes(splitRhs) match {
                  case Right(duplicateAtInternalNode) => duplicateAtInternalNode
                  case Left(duplicateInLeaf) => duplicateInLeaf
                }
                val (lhs, rhs) = node.children.splitAt(index)
                node.copy(
                  keys = (node.keys :+ restEntry).sortBy(_.key),
                  children = (lhs :+ splitLhs :+ newRhs) ++ rhs.tail
                )
              case Right(modifiedChild) =>
                node.copy(children = node.children.updated(index, modifiedChild))
            }
          }
        if (modified.keys.length == m) Left(splitNode(modified))
        else Right(modified)
      }
    }

    def removeDuplicateIndexOnInternalNodes(rhs: N): Either[N, N] = {
      if (!rhs.leaf && rhs != root) {
        val newRhsKeys = rhs.keys.drop(1)
        val newRhsChildren = rhs.children.drop(1)
        Right(BPlusTree.Node(newRhsKeys, newRhsChildren))
      } else Left(rhs)
    }

    /*def addDataToBuckets(bucketData: BiData): Option[Bi] = {
      @scala.annotation.tailrec
      def aux(bucket: Bi): Option[Bi] = {
        val mayBeBucket = buckets.find(_.length < m - 1)
        mayBeBucket match {
          case None =>
            val newBucket = new Bi(null.asInstanceOf[BiData])
            newBucket.setBucketSize(order); buckets.addOne(newBucket)
            aux(newBucket)
          case Some(bucket) =>
            bucket.addOne(bucketData)
            bucket.sortBy(_.key)
            Some(bucket)
        }
      }
      aux(buckets.head)
    }
    *
    */
    def addToBucket(bucket: Bi, bidata: BiData): Unit = {
      @scala.annotation.tailrec
      def aux(bucket: Bi): Unit = {
        if (bucket.length == 0) {
          bucket.addOne(bidata)
        } else {
          val index = bucket.lastIndexWhere(bucketData.key > _.key)
          if (index != -1) {
            if (index != m - 2) {
              bucket.addOne(bidata)
              if (bucket.length == m) {
                println("Wrong")
              }
            } else {
              val newBucket = new Bi(Vector.empty)
              val bucketIndex = buckets.indexOf(bucket)
              buckets.insert(bucketIndex - 1, newBucket)
              aux(newBucket)
            }
          } else {
            val bucketIndex = buckets.indexOf(bucket)
            val previousBucket = buckets(bucketIndex - 1)
            aux(previousBucket)
          }
        }
      }
      aux(bucket)
    }
    def addDataToBuckets(bucketData: BiData): Unit = {
      @scala.annotation.tailrec
      def aux(): Unit = {
        val mayBeEntry = buckets.find(_.length < m - 1)
        mayBeEntry match {
          case Some(bucket) =>
            addToBucket(bucket, bucketData)
          case None =>
            val newBucket = {
              new Bi(Vector.empty)
            }
            buckets.addOne(newBucket)
            aux()
        }
      }
      aux()
    }

    this.copy(
      root = tryInsert(root) match {
        case  Right(node) =>
          addDataToBuckets(bucketData)
          node
        case Left((restEntry, lhs, rhs)) =>
          val newRhs = removeDuplicateIndexOnInternalNodes(rhs) match {
            case Right(duplicateAtInternalNode) => duplicateAtInternalNode
            case Left(duplicateInLeaf) => duplicateInLeaf
          }
          addDataToBuckets(bucketData)
          BPlusTree.Node(Vector(restEntry), Vector(lhs, newRhs))
      }
    )
  }

  def delete(key: K): BPlusTree[K] = {

    val t = this.order

    def merge(node: N): (E, N, N) = (
      node.keys(t - 1),
      BPlusTree.Node(
        keys = node.keys.take(t - 1),
        children = node.children.take(t)
      ),
      BPlusTree.Node(
        keys = node.keys.takeRight(t - 1),
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
          BPlusTree.Node(Vector(restEntry), Vector(lhs, rhs))
      }
    )

  }

  def getMinKey: K = {
    @scala.annotation.tailrec
    def aux(node: N): K = if (!node.leaf) aux(node.children.head) else node.keys.head.key
    if (root.leaf && root.keys.nonEmpty)  root.keys.head.key
    else if (root.leaf && root.keys.isEmpty) null.asInstanceOf[K]
    else aux(root.children.head)
  }

  def getMaxKey: K = {
    @scala.annotation.tailrec
    def aux(node: N): K = if (!node.leaf) aux(node.children.last) else node.keys.last.key
    if (root.leaf && root.keys.nonEmpty)  root.keys.last.key
    else if (root.leaf && root.keys.isEmpty) null.asInstanceOf[K]
    else aux(root.children.last)
  }

  def getHeight: Int = {
    var height = 1;
    @scala.annotation.tailrec
    def aux(node: N): Int = {
      if (node.leaf) height else {
        height += 1
        aux(node.children.head)
      }
    }
    if(root.keys.isEmpty) height - 1 else if (root.leaf) height else aux(root)
  }

  def renderStructure: String = {
    def aux(node: N, i: String): String = {
      val keysString = node.keys.map(x => x.key).mkString("|")
      val formattedKeysString = s"|$keysString|"
      if (node.children.nonEmpty) {
        val childrenString = node.children.map(x => aux(x, i + "  ")).mkString("\n")
        s"$i$formattedKeysString\n$childrenString"
      } else {
        s"$i$formattedKeysString"
      }
    }
    aux(root, "")
  }
}

object BPlusTree {

  case class BucketData[K](key: K)
  case class Bucket[K](data: Vector[BucketData[K]]) extends MutableDLL[K] {
    var bucketSize: Int = 0
    def setBucketSize(order: Int): Unit = bucketSize = order - 1
    def getBucketSize: Int = bucketSize
    def full(order: Int): Boolean = this.getBucketSize == order - 1
    def sortByKey(s1: BucketData[K], s2: BucketData[K]) = {
      println("comparing %s and %s".format(s1, s2))
      s1.key.asInstanceOf[Int] > s2.key.asInstanceOf[Int]
    }
  }
  case class Buckets[Bucket](buckets: Vector[Bucket]) extends MutableDLL[Bucket]
  case class Entry[K](key: K)
  case class Node[K: Ordering](keys: Vector[Entry[K]], children: Vector[Node[K]]) {
    def leaf: Boolean = children.isEmpty
  }
  def empty[K: Ordering](order: Int): BPlusTree[K] = {
    val buckets = Buckets[Bucket[BucketData[K]]](Vector.empty)
    BPlusTree(Node[K](Vector.empty, Vector.empty), buckets, order)
  }
}



/**

 *
 * // Declare _buckets_ variable
 * var buckets: B = _
 *
 * // Buckets ~ methods
 * private def initBuckets() : Unit = {
 * buckets = new B()
 * val size = btree.getHeight
 * for (i <- 0 to size + 3) {
 * val bucket = new Bi()
 * for (j <- 0 to size + 3) {
 * val bucketData = new BiData()
 *         bucket.addOne(bucketData)
 * }
 *       buckets.addOne(bucket)
 * }
 * }
 *
 * private def addOnBucket(bucket: Bi, key: K): Unit = {
 * if (bucket.getBucketLength == bucket.bucketSize) splitBucket(bucket) else {
 *       bucket.criticallity += 1
 *       bucket.head.addOne(key) // add to L1,1
 *       bucket.setBucketLength(bucket.head.length)
 * }
 * }
 * private def splitBucket(bucket: Bi): Unit = {
 * val indexOfBucket = buckets.indexOf(bucket)
 * val newBucket = new Bi(); buckets.insert(indexOfBucket, newBucket)
 * val (newBucketData, restData) =  bucket.splitAt(bucket.length / 2)
 * for { x <- newBucketData } newBucket.addOne(x)
 *     bucket.clear(); for { x <- restData } bucket.addOne(x)
 * }
 * private def fuseBuckets: Boolean = false
 * def add(key: K): Unit = addOnBucket(buckets(2), key)
 * def getBucket(idx: Int): Bi = {
 * require( idx >= 0 && idx < buckets.length );
 *     buckets.apply(idx)
 * }
 * def listBuckets(): Unit = {
 * for (i <- 0 until buckets.length) {
 * val bucket = buckets(i)
 * println(s"Bucket($i): $bucket")
 * }
 * }
 *
 *
// Initialize buckets
  this.initBuckets()
 */
