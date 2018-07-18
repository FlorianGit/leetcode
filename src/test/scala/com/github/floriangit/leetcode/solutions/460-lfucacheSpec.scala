import org.scalatest.FlatSpec

class LFUCacheSpec extends FlatSpec {
  "An LFUCache" should "return value added to it" in {
    val cache = new LFUCache(1)
    cache.put(1, 1)
    assert(cache.get(1) == 1)
  }

  "An LFUCache" should "return -1 when requesting unknown value" in {
    val cache = new LFUCache(1)
    assert(cache.get(1) == -1)
  }

  "An LFUCache" should "enable updating of values" in {
    val cache = new LFUCache(1)
    cache.put(1, 1)
    cache.put(1, 2)
    assert (cache.get(1) == 2)
  }

  "An LFUCache" should "forget least frequently used value when capacity is exceeded" in {
    val cache = new LFUCache(4)
    cache.put(1, 1)
    cache.put(2, 2)
    cache.put(3, 3)
    cache.put(4, 4)
    assert(cache.get(1) == 1)
    assert(cache.get(3) == 3)
    assert(cache.get(3) == 3)
    assert(cache.get(4) == 4)
    cache.put(5, 5)
    assert(cache.get(2) == -1)
  }

  "An LFUCache" should "forget least recently used value in case of tie" in {
    val cache = new LFUCache(3)
    cache.put(1, 1)
    cache.put(2, 2)
    cache.put(3, 3)
    assert(cache.get(2) == 2)
    assert(cache.get(3) == 3)
    assert(cache.get(1) == 1)
    cache.put(4, 4)
    assert(cache.get(2) == -1)
  }
}
