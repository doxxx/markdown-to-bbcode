package net.doxxx.markdowntobbcode

import java.io.{File, OutputStreamWriter, FileWriter}

/**
 * MarkdownToBBcode app.
 * @author Gordon Tyler
 */
object MarkdownToBBcode extends App {
  if (args.isEmpty) {
    Console.println("Usage: md2bbcode <filename> [<outputfilename>]")
    System.exit(255)
  }
  val file = new File(args(0))
  val generator = BBcodeGenerator(file)
  val writer =
    if (args.size == 2) {
      val outFile = new File(args(1))
      if (outFile.exists()) {
        Console.println("error: output file already exists: " + outFile)
        System.exit(255)
      }
      new FileWriter(args(1))
    }
    else new OutputStreamWriter(System.out)
  try {
    writer.write(generator.toString)
  }
  finally {
    writer.close()
  }
}
