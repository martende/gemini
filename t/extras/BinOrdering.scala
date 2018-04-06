package ms.webmaster.binordering

import java.nio.ByteBuffer

// converts T to/from binary form preserving Ordering[T]
// it may be not the best binary form; its main feature is to preserve Ordering[T]
trait BinOrdering[T] extends Ordering[T] { // todo: how related to TableKey[T] ?
//final    def fromByteArray(bin: Array[Byte], off: Int, len: Int): T           = fromBuffer(ByteBuffer.wrap(bin, off, len))
           def fromBuffer(bb: ByteBuffer):     T           // = fromByteArray(bb.array, bb.position, bb.remaining); bb.position(bb.position+...)
           // append to the buffer (useful when a lot of data is put into the same buffer)
           def toBuffer(t: T, bb: ByteBuffer): ByteBuffer  // = bb put toByteArray(t)
           // simpler in use, no need to take care on buffer free space
           def toByteArray(t: T):              Array[Byte] // = toBuffer(ByteBuffer allocate ...).array

           // serialization implies ordering, as in serialized T can be compared.
           // here is the default INEFFECTIVE implementation to compare T based on this order
           // it can be overriden for the particular T
           def compare(a: T, b: T): Int = UByteArrayOrdering.compare(toByteArray(a), toByteArray(b))

}

object BinOrdering {
  def memcmp(bs1: Array[Byte], off1: Int, len1: Int,
             bs2: Array[Byte], off2: Int, len2: Int): Int = { // bytes compared as unsigned
    val minLen = Math.min(len1, len2)
    var i = off1
    while (i < minLen) {
      val a = bs1(off1+i) & 0xff
      val b = bs2(off2+i) & 0xff
      if (a != b)
        return a - b
      i += 1
    }
    return len1 - len2
  }
}

object UByteArrayOrdering extends Ordering[Array[Byte]] { def compare(bs1: Array[Byte], bs2: Array[Byte]): Int = BinOrdering.memcmp(bs1, 0, bs1.length, bs2, 0, bs2.length) }
object UByteBufferOrdering extends Ordering[ByteBuffer] { def compare(bb1: ByteBuffer,  bb2: ByteBuffer):  Int = BinOrdering.memcmp(bb1.array, bb1.position, bb1.remaining, bb2.array, bb2.position, bb2.remaining) }

object UByteOrdering  extends Ordering[Byte]  { def compare(a: Byte,  b: Byte ): Int = Integer.compare(a&0xFF,       b&0xFF)       }
object UShortOrdering extends Ordering[Short] { def compare(a: Short, b: Short): Int = Integer.compare(a&0xFFFF,     b&0xFFFF)     }
object UIntOrdering   extends Ordering[Int]   { def compare(a: Int,   b: Int  ): Int = Integer.compare(a^0x80000000, b^0x80000000) }
object ULongOrdering  extends Ordering[Long]  { def compare(a: Long,  b: Long ): Int = java.lang.Long.compare(a^0x8000000000000000L, b^0x8000000000000000L) }