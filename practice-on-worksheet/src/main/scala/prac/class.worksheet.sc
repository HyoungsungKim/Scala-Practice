abstract class Element:
    def contents: Vector[String]
    def height: Int = contents.length
    def width: Int = if height == 0 then 0 else contents(0).length
    def above(that: Element): Element = 
        VectorElement(this.contents ++ that.contents)
    def beside(that: Element): Element = 
        VectorElement(
            for (line1, line2) <- this.contents.zip(that.contents)
            yield line1 + line2
        )
    override def toString = contents.mkString("\n")
end Element
val a = List(1,2,3)

//for (i <- a) yield i * 2
//for (i <- a) do i * 2

class VectorElement(val contents: Vector[String]) extends Element
class LineElement(s: String) extends Element:
    val contents = Vector(s)
    override def width = s.length
    override def height = 1

class UniformElement(
    ch: Char,
    override val width: Int,
    override val height: Int,
) extends Element:
    private val line = ch.toString * width
    def contents = Vector.fill(height)(line)

val e1: Element = VectorElement(Vector("Hello", "world"))