package scalismo.io

import java.io.RandomAccessFile
import java.nio.ByteBuffer

object FileReader {
  def readFileToByteBuffer(file: String): ByteBuffer = {
    val raf = new RandomAccessFile(file, "r")
    val channel = raf.getChannel
    val buffer = ByteBuffer.allocate(channel.size.toInt)
    channel.read(buffer)
    buffer.rewind()
    buffer
  }
}
