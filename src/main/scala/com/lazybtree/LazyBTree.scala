package com.lazybtree

case class LazyBTree[K, V](var btree: BTree[K,V], order: Int)(implicit keyOrdering: Ordering[K]) {
  private type BiData = LazyBTree.BucketData[K];
  private type Bi = LazyBTree.Bucket[BiData];
  private type B = LazyBTree.Buckets[Bi];

  // Declare _buckets_ variable
  var buckets: B = _

  // Buckets ~ methods
  private def initBuckets() : Unit = {
    buckets = new B()
    val size = btree.getHeight
    for (i <- 0 to size + 5) {
      val bucket = new Bi()
      buckets.addOne(bucket)
    }
  }

  def listBuckets(): Unit = {
    for (i <- 0 until buckets.length) {
      val bucket = buckets(i)
      println(s"Bucket($i): $bucket")
    }
  }
  def insert(key: K, value: V): Unit = {
    val newBtree: BTree[K, V] = this.btree.insert(key, value)
    this.btree = newBtree
  }
  def insertAtBucket(key: K, value: V): Boolean = false
  def removeFromBucket(key: K): Boolean = true
  def getBucket(idx: Int): Bi = {
    require( idx >= 0 && idx < buckets.length )
    buckets(idx)
  }
  def splitBucket: Boolean = true
  def fuseBuckets: Boolean = false

  // BTree ~ methods
  def getKey(key: K): Option[V] = this.btree.get(key)
  def getMinKey: K = this.btree.getMinKey
  def getMaxKey: K = this.btree.getMaxKey
  def getBTreeHeight: Int = this.btree.getHeight
  def renderBTree:String = this.btree.renderStructure

  // Initialize buckets
  this.initBuckets()
}

object LazyBTree {
  class BucketData[K] extends MutableDLL[K]
  class Bucket[BucketData] extends MutableDLL[BucketData] {
    def criticallity: Boolean = true
  }
  class Buckets[Bucket] extends MutableDLL[Bucket]
  def empty[K: Ordering, V](order: Int): LazyBTree[K, V] = {
    val initBtree = BTree.empty[K, V](2);
    LazyBTree[K, V](initBtree, order)
  }
}
