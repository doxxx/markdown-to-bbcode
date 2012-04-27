package net.doxxx.markdowntobbcode

import io.Source
import java.io.{OutputStreamWriter, FileWriter}
import org.pegdown.PegDownProcessor
import org.pegdown.Extensions._

/**
 * MarkdownToBBcode app.
 * @author Gordon Tyler
 */
object MarkdownToBBcode extends App {
  if (args.isEmpty) {
    Console.println("Usage: MarkdownToBBcode <filename> [<outputfilename>]")
    System.exit(255)
  }
  val file = Source.fromFile(args(0))
  try {
    val processor = new PegDownProcessor(AUTOLINKS | WIKILINKS)
    val root = processor.parseMarkdown(file.toArray)
    val generator = new BBcodeGenerator
    val writer =
      if (args.size == 2) new FileWriter(args(1))
      else new OutputStreamWriter(System.out)
    try {
      writer.write(generator.toBBcode(root))
    }
    finally {
      writer.close()
    }
  }
  finally {
    file.close()
  }
}
