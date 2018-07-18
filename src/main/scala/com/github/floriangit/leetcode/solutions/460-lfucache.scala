class LFUCache(_capacity: Int) {
  case class Value(var value: Int, var freq: Int)

  val ordered_cache = new OrderedHashMap[Int, Value](_capacity, compare(_, _), are_equal(_, _),  on_access _)

  def compare(a: Value, b: Value): Boolean = {
    if (a.freq > b.freq) {
      true
    } else {
      false
    }
  }

  def are_equal(a: Value, b: Value): Boolean = {
    a.freq == b.freq
  }

  def on_access(a: Value): Value = {
    a.freq += 1
    a
  }

  def get(key: Int): Int = {
    if (ordered_cache contains key) {
      ordered_cache(key).value
    } else {
      -1
    }
  }

  def put(key: Int, value: Int) {
    if (ordered_cache contains key) {
      val tmp = ordered_cache(key)
      tmp.value = value
      ordered_cache.update(key, tmp)
    } else {
      val tmp = new Value(value, 1)
      ordered_cache.insert(key, tmp)
    }
  }

  override def toString(): String = {
    ordered_cache toString
  }
}
