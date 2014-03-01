import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import patmat.Huffman._


@RunWith(classOf[JUnitRunner])
class VisualGraph extends FunSuite{

// Create a graphviz string representation
def show(tree: CodeTree): String = {
  def showTree(parentLabel: String, tree: CodeTree): String = tree match {
    case Fork(left, right, chars, weight) => {
      val label = '"' + "(" + chars.mkString(",") + ") " + weight + '"'
      parentLabel + " -> " + label + "\n" + showTree(label, left) + "\n" + showTree(label, right)
    }
    case Leaf(char, weight) => parentLabel + " -> " + '"' + "(" + char + ") " + weight + '"'
  }
  """digraph codetree {
        size="6,6";
        node [color=lightblue2, style=filled];
  """ + showTree("root", tree) + "\n}"
}

  test("print graph of 'to be or not to be'") {
    print(show(createCodeTree( string2Chars("to be or not to be")) ))
  }


}