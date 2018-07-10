import scala.collection.mutable

class LRUCache(_capacity: Int) {
  case class Element(key: Int, var value: Int, var prev: Element, var next: Element)
  var head: Element = null
  var last: Element = null
  var cache: mutable.Map[Int, Element] = new mutable.HashMap[Int, Element]()
  var size: Int = 0

  println("")
  println("new cache")

  private def prepend(elem: Element) = {
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
  }

  private def remove(elem: Element) = {
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

  def get(key: Int): Int = {
    var elem = cache.getOrElse(key, null)
    if (elem != null) {
      remove(elem)
      prepend(elem)
      elem.value
    } else {
      -1
    }
  }

  def put(key: Int, value: Int) {
    if (cache contains key) {
      var elem = cache(key)
      remove(elem)
      elem.value = value
      prepend(elem)
    } else {
      var elem = new Element(key, value, null, null)
      prepend(elem)
      if (size > _capacity) {
        remove(last)
      }
    }
  }
}
