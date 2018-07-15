import scala.collection.mutable

class OrderedHashMap[S, T](capacity: Int)
{
  case class Element(key: S, var value: T, var prev: Element, var next: Element)

  var head: Element = null
  var last: Element = null
  var cache: mutable.Map[S, Element] = new mutable.HashMap[S, Element]()
  var size: Int = 0

  def apply(key: S): T = {
    val value = cache(key).value
    remove(key)
    insert(key, value)
    value
  }

  def insert(key: S, value: T) = {
    val elem = new Element(key, value, null, null)
    cache += elem.key -> elem
    if (head == null) {
      head = elem
      last = elem
      elem.prev = null
      elem.next = null
    } else {
      head.prev = elem
      elem.next = head
      head = elem
      elem.prev = head
    }
    size += 1
    if (size > capacity) {
      remove(last.key)
    }
  }

  def contains(key: S) = { cache contains key }

  def update(key: S, value: T): Boolean = {
    val elem = cache.getOrElse(key, null)
    if (elem == null) {
      false
    } else {
      remove(key)
      elem.value = value
      insert(key, value)
      true
    }
  }

  def remove(key: S) = {
    val elem = cache.getOrElse(key, null)
    if (elem != null) {
      if (head == elem && last == elem) {
        head = null
        last = null
      } else if (head == elem && last != elem) {
        head = elem.next
        head.prev = null
      } else if (head != elem && last == elem) {
        last = elem.prev
        last.next = null
      } else {
        elem.next.prev = elem.prev
        elem.prev.next = elem.next
      }
      elem.next = null
      elem.prev = null
      cache -= elem.key
      size -= 1
    }
  }
}

class LRUCache(_capacity: Int) {
  case class Element(key: Int, var value: Int, var prev: Element, var next: Element)
  val ordered_cache = new OrderedHashMap[Int, Int](_capacity)

  def get(key: Int): Int = {
    if (ordered_cache contains key) {
      ordered_cache(key)
    } else {
      -1
    }
  }

  def put(key: Int, value: Int) {
    if (ordered_cache contains key) {
      ordered_cache.update(key, value)
    } else {
      ordered_cache.insert(key, value)
    }
  }
}
