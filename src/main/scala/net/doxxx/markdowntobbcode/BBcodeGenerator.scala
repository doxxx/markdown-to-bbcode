package net.doxxx.markdowntobbcode

import org.pegdown.ast._
import scala.collection.JavaConversions._
import org.pegdown.{PegDownProcessor, LinkRenderer}
import collection.mutable.Map
import java.io.File
import org.pegdown.Extensions._
import scala.io.Source

object BBcodeGenerator {
  def apply(file: File): BBcodeGenerator = {
    val contents = Source.fromFile(file)
    try {
      apply(contents.toArray)
    }
    finally {
      contents.close()
    }
  }

  def apply(contents: String): BBcodeGenerator = {
    apply(contents.toCharArray)
  }

  def apply(contents: Array[Char]): BBcodeGenerator = {
    val processor = new PegDownProcessor(AUTOLINKS | WIKILINKS)
    val root = processor.parseMarkdown(contents)
    new BBcodeGenerator(root)
  }
}

class BBcodeGenerator private (rootNode: RootNode) extends Visitor {
  val sb = new StringBuilder
  val renderer = new LinkRenderer()
  val references: Map[String,ReferenceNode] = Map.empty

  rootNode.accept(this)

  override def toString: String = {
    sb.toString()
  }

  def visit(node: RootNode) {
    node.getReferences.foreach { refNode =>
      references.put(normalizeRefKey(childrenToString(refNode)), refNode)
    }
    visitChildren(node)
  }

  def visit(node: AbbreviationNode) {
    warn("not implemented yet: " + node)
  }

  def visit(node: AutoLinkNode) {
    link(renderer.render(node))
  }

  def visit(node: BlockQuoteNode) {
    openTag("quote")
    visitChildren(node)
    closeTag("quote")
    newline()
    newline()
  }

  def visit(node: BulletListNode) {
    openTag("list")
    newline()
    visitChildren(node)
    closeTag("list")
    newline()
    newline()
  }

  def visit(node: CodeNode) {
    openTag("code")
    newline()
    text(node.getText)
    closeTag("code")
    newline()
  }

  def visit(node: DefinitionListNode) {
    warn("not implemented yet: " + node)
  }

  def visit(node: DefinitionNode) {
    warn("not implemented yet: " + node)
  }

  def visit(node: DefinitionTermNode) {
    warn("not implemented yet: " + node)
  }

  def visit(node: EmphNode) {
    openTag("i")
    visitChildren(node)
    closeTag("i")
  }

  def visit(node: ExpImageNode) {
    openTag("img")
    text(node.url)
    closeTag("img")
  }

  def visit(node: ExpLinkNode) {
    link(renderer.render(node, childrenToString(node)))
  }

  def visit(node: HeaderNode) {
    openTag("size=" + (6 - node.getLevel))
    openTag("b")
    visitChildren(node)
    closeTag("b")
    closeTag("size")
    newline()
    newline()
  }

  def visit(node: HtmlBlockNode) {
    warn("not implemented yet: " + node)
  }

  def visit(node: InlineHtmlNode) {
    warn("not implemented yet: " + node)
  }

  def visit(node: ListItemNode) {
    openTag("*")
    visitChildren(node)
    newline()
  }

  def visit(node: MailLinkNode) {
    openTag("email")
    text(node.getText)
    closeTag("email")
  }

  def visit(node: OrderedListNode) {
    openTag("list=1")
    newline()
    visitChildren(node)
    closeTag("list")
    newline()
    newline()
  }

  def visit(node: ParaNode) {
    visitChildren(node)
    newline()
    newline()
  }

  def visit(node: QuotedNode) {
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

  def visit(node: ReferenceNode) {
    // do nothing
  }

  def visit(node: RefImageNode) {
    val nodeText = childrenToString(node)
    val key = if (node.referenceKey != null) childrenToString(node.referenceKey) else nodeText
    references.get(normalizeRefKey(key)) match {
      case Some(refNode) => {
        openTag("img")
        text(refNode.getUrl)
        closeTag("img")
      }
      case _ => warn("unknown reference: " + key)
    }
  }

  def visit(node: RefLinkNode) {
    val text = childrenToString(node)
    val key = if (node.referenceKey != null) childrenToString(node.referenceKey) else text
    references.get(normalizeRefKey(key)) match {
      case Some(refNode) => link(renderer.render(node, refNode.getUrl, refNode.getTitle, text))
      case _ => warn("unknown reference: " + key)
    }
  }

  def visit(node: SimpleNode) {
    node.getType match {
      case SimpleNode.Type.Apostrophe => text("'")
      case SimpleNode.Type.Ellipsis => text("...")
      case SimpleNode.Type.Emdash => text("--")
      case SimpleNode.Type.Endash => text("-")
      case SimpleNode.Type.HRule => { newline(); text("---"); newline() }
      case SimpleNode.Type.Linebreak => newline()
      case SimpleNode.Type.Nbsp => text(" ")
    }
  }

  def visit(node: SpecialTextNode) {
    text(node.getText)
  }

  def visit(node: StrongNode) {
    openTag("b")
    visitChildren(node)
    closeTag("b")
  }

  def visit(node: TableBodyNode) {
    warn("not implemented yet: " + node)
  }

  def visit(node: TableCellNode) {
    warn("not implemented yet: " + node)
  }

  def visit(node: TableColumnNode) {
    warn("not implemented yet: " + node)
  }

  def visit(node: TableHeaderNode) {
    warn("not implemented yet: " + node)
  }

  def visit(node: TableNode) {
    warn("not implemented yet: " + node)
  }

  def visit(node: TableRowNode) {
    warn("not implemented yet: " + node)
  }

  def visit(node: VerbatimNode) {
    openTag("code")
    newline()
    text(node.getText)
    closeTag("code")
    newline()
    newline()
  }

  def visit(node: WikiLinkNode) {
    link(renderer.render(node))
  }

  def visit(node: TextNode) {
    text(node.getText)
  }

  def visit(node: SuperNode) {
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

  private def childrenToString(node: SuperNode): String = {
    val originalString = sb.toString()
    sb.clear()
    visitChildren(node)
    val result = sb.toString()
    sb.clear()
    sb.append(originalString)
    result
  }

  private def link(renderedLink: LinkRenderer.Rendering) {
    openTag("url=" + renderedLink.href)
    text(renderedLink.text)
    closeTag("url")
  }

  private def normalizeRefKey(name: String): String = {
    name.filter(!" \n\t".contains(_)).toLowerCase
  }

  private def warn(msg: String) {
    Console.err.print("WARN: ")
    Console.err.println(msg)
  }
}
