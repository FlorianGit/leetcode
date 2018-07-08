import org.scalatest.FlatSpec

class LRUCacheSpec extends FlatSpec {
  "An LRUCache" should "return value added to it" in {
    val cache = new LRUCache(1)
    cache.put(1, 1)
    assert(cache.get(1) == 1)
  }

  "An LRUCache" should "return -1 when requesting unknown value" in {
    val cache = new LRUCache(1)
    assert(cache.get(1) == -1)
  }

  "An LRUCache" should "forget least recently used value above capacity" in {
    val cache = new LRUCache(2)
    cache.put(1, 2)
    cache.put(2, 1)
    cache.get(1)
    cache.put(3, 3)
    assert(cache.get(2) == -1)
  }

  "An LRUCache" should "enable updating of values" in {
    val cache = new LRUCache(1)
    cache.put(1, 1)
    cache.put(1, 2)
    assert (cache.get(1) == 2)
  }

  "An LRUCache" should "pass simple leetcode test case" in {
    val cache = new LRUCache(2)
    cache.put(1, 1)
    cache.put(2, 2)
    assert(cache.get(1) == 1)
    cache.put(3, 3)
    assert(cache.get(2) == -1)
    cache.put(4, 4)
    assert(cache.get(1) == -1)
    assert(cache.get(3) == 3)
    assert(cache.get(4) == 4)
  }

  "An LRUCache" should "pass second leetcode test case" in {
    val cache = new LRUCache(2)
    cache.put(2, 1)
    cache.put(1, 1)
    assert(cache.get(2) == 1)
    cache.put(4, 1)
    assert(cache.get(1) == -1)
    assert(cache.get(2) == 1)
  }

}
