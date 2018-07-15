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
