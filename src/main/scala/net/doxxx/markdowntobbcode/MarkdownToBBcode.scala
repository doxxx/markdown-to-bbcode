package net.doxxx.markdowntobbcode

import org.pegdown.PegDownProcessor
import io.Source
import java.io.FileWriter

/**
 * MarkdownToBBcode app.
 * @author Gordon Tyler
 */
object MarkdownToBBcode extends App {
  val file = Source.fromFile(args(0))
  val processor = new PegDownProcessor()
  val root = processor.parseMarkdown(file.toArray)
  val generator = new BBcodeGenerator
  val writer = new FileWriter("out.txt")
  writer.write(generator.toBBcode(root))
  writer.close()
}
