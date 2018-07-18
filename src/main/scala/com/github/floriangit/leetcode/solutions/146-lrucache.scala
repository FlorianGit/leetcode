class LRUCache(_capacity: Int) {
  val ordered_cache = new OrderedHashMap[Int, Int](_capacity, (_, _) => true, (_, _) => false, x => x)

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
