package com.lazybtree

import scala.collection.mutable

class MutableDLL[A] extends mutable.Buffer[A] {
  private var default: A = _
  private class Node(var data: A, var prev: Node, var next: Node)
  private val end: Node = new Node(default, null, null)
  end.prev = end
  end.next = end
  private var numElems = 0

  // append an element to the list
  def += (elem: A): MutableDLL.this.type = {
    val newNode = new Node(elem, end.prev, end)
    end.prev.next = newNode
    end.prev = newNode
    numElems += 1
    this
  }

  // prepend an element to the list
  def +=: (elem: A): MutableDLL.this.type = {
    val newNode = new Node(elem, end, end.next)
    end.next.prev = newNode
    end.next = newNode
    numElems += 1
    this
  }

  // getting lists length
  def length: Int = numElems

  // clearing lists
  def clear(): Unit = {
    end.next = end
    end.prev = end
    numElems = 0
  }
  // applying
  def apply(index: Int): A = {
    require( index >= 0 && index < numElems )
    var node = end.next
    for (i <- 1 to index ) node = node.next
    node.data
  }

  def update(index: Int, newelem: A): Unit = {
    require( index >= 0 && index < numElems )
    var node = end.next
    for (i <- 1 to index ) node = node.next
    node.data = newelem
  }

  def iterator: Iterator[A] = new Iterator[A] {
    var node: Node = end.next
    // stop when node is equal to sentinel
    def hasNext: Boolean = node != end
    def next(): A = {
      val ret = node.data
      node = node.next
      ret
    }
  }
  //insertAll
  def insertAll(index: Int, elems: collection.Traversable[A]): Unit = {
    require(index >= 0 && index < numElems + 1)
    if (elems.nonEmpty) {
      var node = end.next
      for (i <- 0 until index) node = node.next
      for (e <- elems) {
        val newNode = new Node(e, node.prev, node)
        node.prev.next = newNode
        node.prev = newNode
        numElems += 1
      }
    }

  }
  // removing
  def remove(index: Int): A = {
    require(index >= 0 && index < numElems)
    numElems -= 1
    var node = end.next
    for (i <- 0 until index) node = node.next
    val ret = node.data
    node.prev.next = node.next
    node.next.prev = node.prev
    ret
  }
}