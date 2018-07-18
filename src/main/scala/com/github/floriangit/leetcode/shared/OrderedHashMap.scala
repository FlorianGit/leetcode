import scala.collection.mutable

class OrderedHashMap[S, T](capacity: Int, less_than: (T, T) => Boolean, are_equal: (T, T) => Boolean, on_access: T => T)
{
  case class Element(key: S, var value: T, var prev: Element, var next: Element)

  var head: Element = null
  var last: Element = null
  var cache: mutable.Map[S, Element] = new mutable.HashMap[S, Element]()
  var size: Int = 0

  def apply(key: S): T = {
    var value = cache(key).value
    remove(key)
    value = on_access(value)
    insert(key, value)
    value
  }

  def insert(key: S, value: T) = {
    if (size == capacity) {
      remove(last.key)
    }

    val elem = new Element(key, value, null, null)
    cache += elem.key -> elem
    if (head == null) {
      head = elem
      last = elem
      elem.prev = null
      elem.next = null
    } else if (less_than(value, head.value) || are_equal(value, head.value)) {
      head.prev = elem
      elem.next = head
      head = elem
      elem.prev = head
    } else {
      val insert_after = getLastSmaller(value, less_than)
      if (insert_after.next == null) {
        last = elem
      } else {
        insert_after.next.prev = elem
      }
      elem.prev = insert_after

      elem.next = insert_after.next
      insert_after.next = elem
    }

    size += 1
  }

  private def getLastSmaller(value: T, less_than: (T, T) => Boolean) = {
    var current = head
    while (current.next != null && less_than(current.next.value, value)) {
      current = current.next
    }
    current
  }

  def contains(key: S) = { cache contains key }

  def update(key: S, value: T): Boolean = {
    val elem = cache.getOrElse(key, null)
    if (elem == null) {
      println("update not found")
      false
    } else {
      remove(key)
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

  override def toString(): String = head match {
    case null => "empty"
    case _ => {
      var result = ""
      var current = head
      while (current != null) {
        result += current.value.toString + " "
        current = current.next
      }
      result
    }
  }
}
