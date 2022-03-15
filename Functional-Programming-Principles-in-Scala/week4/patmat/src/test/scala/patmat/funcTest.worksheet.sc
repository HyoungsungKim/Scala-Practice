trait HuffmanInterface:
  def weight(tree: CodeTree): Int  
  def chars(tree: CodeTree): List[Char]
  def times(chars: List[Char]): List[(Char, Int)]
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf]
  def singleton(trees: List[CodeTree]): Boolean
  def combine(trees: List[CodeTree]): List[CodeTree]
  def until(done: List[CodeTree] => Boolean, merge: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree]
  def createCodeTree(chars: List[Char]): CodeTree
  def decode(tree: CodeTree, bits: List[Int]): List[Char]

  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)
  type Bit = Int
  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)
  def decodedSecret: List[Char] = decode(frenchCode, secret)

  def encode(tree: CodeTree)(text: List[Char]): List[Int]
  
  def convert(tree: CodeTree): List[(Char, List[Int])]
  /*
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Int]
  def frenchCode: CodeTree
  def secret: List[Int]
  */
end HuffmanInterface

abstract class CodeTree
case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
case class Leaf(char: Char, weight: Int) extends CodeTree

trait Huffman extends HuffmanInterface:
    def weight(tree: CodeTree): Int = tree match
        case Fork(_, _, _, weight) => weight
        case Leaf(_, weight) => weight

    def chars(tree: CodeTree): List[Char] = tree match
        case Fork(_, _, chars, _) => chars
        case Leaf(char, _) => List(char)

    def makeCodeTree(left: CodeTree, right: CodeTree): CodeTree =
        Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

    def string2Chars(str: String): List[Char] = str.toList

    def times(chars: List[Char]): List[(Char, Int)] = chars match
        case Nil => Nil
        case List(ch) => List((ch, 1))
        case head :: tail =>
            val (target, remain) = tail.partition(ch => ch == head)
            List((head, target.length + 1)) ++ times(remain)

    def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] =
        freqs.sortBy(_._2).map(freq => Leaf(freq._1, freq._2))

    def singleton(trees: List[CodeTree]): Boolean = 
        trees.length == 1

    def combine(trees: List[CodeTree]): List[CodeTree] = trees match
        case Nil => trees
        case head1 :: head2 :: tail =>
            (List(makeCodeTree(head1, head2)) ++ combine(tail)).sortBy(elem => weight(elem))
        case _ => trees

    def until(done: List[CodeTree] => Boolean, merge: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] =
        if done(trees) == true || trees.length < 1 then trees        
        else until(done, merge)(merge(trees))

    def createCodeTree(chars: List[Char]): CodeTree = 
        val freq = times(chars)
        val leaf = makeOrderedLeafList(freq)
        until(singleton, combine)(leaf)(0)

    def decode(tree: CodeTree, bits: List[Int]): List[Char] = 
        def traverse(subtree:CodeTree, subBits:List[Bit]):List[Char] = subtree match
            case Leaf(char, _) => 
                if subBits.isEmpty then List(char)
                else List(char) ++ traverse(tree, subBits)
            case Fork(left, right, _, _) => 
                if subBits.head == 0 then traverse(left, subBits.tail)
                else traverse(right, subBits.tail)

        traverse(tree, bits)

    def encode(tree: CodeTree)(text: List[Char]): List[Bit] =         
        def traverse(tree: CodeTree, char:Char):List[Bit] = tree match
            case Fork(left, right, _, _) => 
                if chars(left).contains(char) then List(0) ++ traverse(left, char)
                else List(1) ++ traverse(right, char)
            case Leaf(_, _) => List()
        
        //text.map(ch => traverse(tree, ch, List[Bit]())).flatten        
        text.flatMap(ch => traverse(tree, ch))

    /*
        def lookup(tree:  CodeTree, c: Char): List[Bit] = tree match 
            case Leaf(_, _) => List()
            case Fork(left, right, _, _) if chars(left).contains(c) => List(0) ++ lookup(left, c)
            case Fork(left, right, _, _) => List(1) ++ lookup(right, c)

        text.flatMap(ch =>lookup(tree, ch))
    */
    type CodeTable = List[(Char, List[Bit])]
    def codeBits(table: CodeTable)(char: Char): List[Bit] = table match
        case Nil => Nil
        case x :: xs =>
            if x._1 == char then x._2
            else codeBits(xs)(char)

    def convert(tree: CodeTree): CodeTable = tree match
        case Fork(_, _, chars, _) => 
            chars.map(ch => 
                (ch, encode(tree)(List(ch)))
            )
        case Leaf(ch, _) => List((ch, encode(tree)(List(ch))))

end Huffman

List(1) ++ List[Int]() 

object Huffman extends Huffman

val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)

Huffman.weight(t1)
Huffman.weight(t2)

Huffman.chars(t1)
Huffman.chars(t2)

Huffman.string2Chars("Hello world")

Huffman.times(List[Char]('a', 's', 'd', 'a','s','s'))
Huffman.times(List('a', 'b', 'a'))
Huffman.makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3)))

val leaflist = List(Leaf('A', 9), Leaf('B', 3), Leaf('C', 1), Leaf('D', 1), Leaf('E', 1), Leaf('F', 1), Leaf('G', 1), Leaf('h', 1))
val leaflist2 = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))

Huffman.combine(leaflist)

// Expected ans: List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4))
Huffman.combine(leaflist2)

//val test = Huffman.until(Huffman.singleton, Huffman.combine)(leaflist)
val test = Huffman.createCodeTree("AAAAAAAABBBCDEGH".toList)
Huffman.encode(test)("A".toList)
Huffman.encode(test)("B".toList)
Huffman.encode(test)("C".toList)
Huffman.encode(test)("D".toList)
Huffman.encode(test)("E".toList)
Huffman.encode(test)("F".toList)
Huffman.encode(test)("G".toList)
Huffman.encode(test)("H".toList)

val helloworld = Huffman.createCodeTree("hello world".toList)

val enhw = Huffman.encode(helloworld)("hello world".toList)
Huffman.decode(helloworld, enhw)

val encoding = Huffman.encode(t2)("aabbbdddd".toList)
Huffman.encode(t2)("a".toList)
Huffman.encode(t2)("b".toList)
Huffman.encode(t2)("d".toList)
Huffman.decode(t2, encoding)

List(1,2) ++ List(3,4)

(Set(1, 2) ++ Set(2, 3)).toList
(Set(1, 2) ++ Set(2, 3)).toList.toSet