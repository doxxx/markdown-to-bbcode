package net.doxxx.markdowntobbcode

import org.pegdown.ast._
import scala.collection.JavaConversions._

class BBcodeGenerator extends Visitor {
  val sb = new StringBuilder

  def toBBcode(rootNode: RootNode): String = {
    rootNode.accept(this)
    sb.toString()
  }

  def visit(node: RootNode) {
    // TODO: references and abbreviations
    visitChildren(node)
  }

  def visit(node: AbbreviationNode) {}

  def visit(node: AutoLinkNode) {}

  def visit(node: BlockQuoteNode) {}

  def visit(node: BulletListNode) {
    Console.println(node.getClass.getName + ": " + node)
    openTag("ul")
    newline()
    visitChildren(node)
    closeTag("ul")
    newline()
    newline()
  }

  def visit(node: CodeNode) {
    Console.println(node.getClass.getName + ": " + node)
    openTag("code")
    newline()
    text(node.getText)
    closeTag("code")
    newline()
  }

  def visit(node: DefinitionListNode) {}

  def visit(node: DefinitionNode) {}

  def visit(node: DefinitionTermNode) {}

  def visit(node: EmphNode) {
    Console.println(node.getClass.getName + ": " + node)
    openTag("i")
    visitChildren(node)
    closeTag("i")
  }

  def visit(node: ExpImageNode) {}

  def visit(node: ExpLinkNode) {}

  def visit(node: HeaderNode) {
    Console.println(node.getClass.getName + ": " + node)
    openTag("size=" + (6 - node.getLevel))
    openTag("b")
    visitChildren(node)
    closeTag("b")
    closeTag("size")
    newline()
    newline()
  }

  def visit(node: HtmlBlockNode) {}

  def visit(node: InlineHtmlNode) {}

  def visit(node: ListItemNode) {
    Console.println(node.getClass.getName + ": " + node)
    openTag("*")
    visitChildren(node)
    newline()
  }

  def visit(node: MailLinkNode) {}

  def visit(node: OrderedListNode) {
    Console.println(node.getClass.getName + ": " + node)
    openTag("ol")
    newline()
    visitChildren(node)
    closeTag("ol")
    newline()
    newline()
  }

  def visit(node: ParaNode) {
    Console.println(node.getClass.getName + ": " + node)
    visitChildren(node)
    newline()
    newline()
  }

  def visit(node: QuotedNode) {
    Console.println(node.getClass.getName + ": " + node)
    node.getType match {
      case QuotedNode.Type.Double => {
        text("\"")
        visitChildren(node)
        text("\"")
      }
      case QuotedNode.Type.DoubleAngle => {
        text("\"")
        visitChildren(node)
        text("\"")
      }
      case QuotedNode.Type.Single => {
        text("'")
        visitChildren(node)
        text("'")
      }
    }
  }

  def visit(node: ReferenceNode) {}

  def visit(node: RefImageNode) {}

  def visit(node: RefLinkNode) {}

  def visit(node: SimpleNode) {
    Console.println(node.getClass.getName + ": " + node)
    node.getType match {
      case SimpleNode.Type.Apostrophe => text("&rsquo;")
      case SimpleNode.Type.Ellipsis => text("&hellip;")
      case SimpleNode.Type.Emdash => text("&mdash;")
      case SimpleNode.Type.Endash => text("&ndash;")
      case SimpleNode.Type.HRule => { newline(); text("---") }
      case SimpleNode.Type.Linebreak => newline()
      case SimpleNode.Type.Nbsp => text(" ")
    }
  }

  def visit(node: SpecialTextNode) {
    Console.println(node.getClass.getName + ": " + node)
    text(node.getText)
  }

  def visit(node: StrongNode) {
    Console.println(node.getClass.getName + ": " + node)
    openTag("b")
    visitChildren(node)
    closeTag("b")
  }

  def visit(node: TableBodyNode) {}

  def visit(node: TableCellNode) {}

  def visit(node: TableColumnNode) {}

  def visit(node: TableHeaderNode) {}

  def visit(node: TableNode) {}

  def visit(node: TableRowNode) {}

  def visit(node: VerbatimNode) {
    Console.println(node.getClass.getName + ": " + node)
    openTag("pre")
    text(node.getText)
    closeTag("pre")
  }

  def visit(node: WikiLinkNode) {}

  def visit(node: TextNode) {
    Console.println(node.getClass.getName + ": " + node)
    text(node.getText)
  }

  def visit(node: SuperNode) {
    Console.println(node.getClass.getName + ": " + node)
    visitChildren(node)
  }

  def visit(node: Node) {
    throw new RuntimeException("not implemented")
  }

  private def visitChildren(node: SuperNode) {
    node.getChildren.foreach(_.accept(this))
  }

  private def openTag(tag: String) {
    sb.append('[').append(tag).append(']')
  }

  private def closeTag(tag: String) {
    sb.append("[/").append(tag).append(']')
  }

  private def text(text: String) {
    sb.append(text)
  }

  private def newline() {
    sb.append('\n')
  }
}
