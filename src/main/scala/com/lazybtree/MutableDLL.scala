package com.lazybtree

import scala.collection.mutable

class MutableDLL[A] extends mutable.Buffer[A] {
  private var default: A = _
  private var numElems = 0

  // Declare Node class and the initial node
  private class Node(var data: A, var prev: Node, var next: Node)
  private val end: Node = new Node(default, null, null)
  // Init end and prev pointers for initial node
  end.prev = end
  end.next = end

  // Get the length of the list
  def length: Int = numElems

  // Clear the list
  def clear(): Unit = {
    end.next = end
    end.prev = end
    numElems = 0
  }

  // Get the data of the element at the given index
  def apply(index: Int): A = {
    require( index >= 0 && index < numElems )
    var node = end.next
    for (i <- 1 to index ) node = node.next
    node.data
  }

  // Iterating through the nodes
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

  // Add an element at the end of the list
  def prepend(elem: A): MutableDLL.this.type = {
    val newNode = new Node(elem, end, end.next)
    end.next.prev = newNode
    end.next = newNode
    numElems += 1
    this
  }

  // Add an element ot the beginning of the list ~ append
  def addOne(elem: A): MutableDLL.this.type = {
    val newNode = new Node(elem, end.prev, end)
    end.prev.next = newNode
    end.prev = newNode
    numElems += 1
    this
  }

  // Add an element on the list at a given index
  def insert(idx: Int, elem: A): Unit = {
    require(idx >= 0 && idx < numElems + 1)
    numElems += 1
    var node = end.next
    for (i <- 0 until idx) node = node.next
    val newNode = new Node(elem, node.prev, node)
    node.prev.next = newNode
    node.prev = newNode
  }

  // Add multiple elements as a sequence at a given index on the list
  def insertAll(idx: Int, elems: Traversable[A]): Unit = {
    require(idx >= 0 && idx < numElems + 1)
    if (elems.nonEmpty) {
      var node = end.next
      for (i <- 0 until idx) node = node.next
      for (e <- elems) {
        val newNode = new Node(e, node.prev, node)
        node.prev.next = newNode
        node.prev = newNode
        numElems += 1
      }
    }
  }

  // Update the data of a node in the list
  def update(index: Int, newelem: A): Unit = {
    require( index >= 0 && index < numElems )
    var node = end.next
    for (i <- 1 to index ) node = node.next
    node.data = newelem
  }

  // Update an element at a given index
  def patchInPlace(from: Int, patch: Traversable[A], replaced: Int): MutableDLL[A] = {
    this
  }

  // Remove the element at the given index from the list
  def remove(idx: Int): A = {
    require(idx >= 0 && idx < numElems)
    numElems -= 1
    var node = end.next
    for (i <- 0 until idx) node = node.next
    val ret = node.data
    node.prev.next = node.next
    node.next.prev = node.prev
    ret
  }

  // Removing multiple elements from the list
  override def remove(idx: Int, count: Int): Unit = {

  }

  override def +=(elem: A): MutableDLL.this.type = this
  override def +=:(elem: A): MutableDLL.this.type = this
}