import scala.io.{ Codec, Source }

trait AnagramsInterface:
  def wordOccurrences(w: String): List[(Char, Int)]
  def sentenceOccurrences(s: List[String]): List[(Char, Int)]
  def dictionaryByOccurrences: Map[List[(Char, Int)], List[String]]
  def wordAnagrams(word: String): List[String]
  def combinations(occurrences: List[(Char, Int)]): List[List[(Char, Int)]]
  def subtract(x: List[(Char, Int)], y: List[(Char, Int)]): List[(Char, Int)]   
  def sentenceAnagrams(sentence: List[String]): List[List[String]]
  

object Anagrams extends AnagramsInterface:
    type Word = String
    type Sentence = List[Word]
    type Occurrences = List[(Char, Int)]

    val dictionary: List[Word] = Dictionary.loadDictionary

    def wordOccurrences(w: Word): Occurrences = 
      val lowerWord = w.toLowerCase
      val characterSet = lowerWord.toSet
      val group = lowerWord.toList.groupBy(_.toString)

      characterSet.map(ch => (ch, group(ch.toString).length)).toList.sortBy(_._1)

    def sentenceOccurrences(s: List[String]): List[(Char, Int)] = 
      wordOccurrences(s.mkString)

    lazy val dictionaryByOccurrences: Map[List[(Char, Int)], List[String]] =
      dictionary.groupBy(w => wordOccurrences(w))

    def wordAnagrams(word: Word): List[Word] = 
      dictionaryByOccurrences.getOrElse(wordOccurrences(word), Nil)

    def combinations(occurrences: List[(Char, Int)]): List[Occurrences] = 
      var extendedOcc = occurrences
      var subSetList:List[Occurrences] = List[Occurrences]()
      var filtedSubSetList:List[Occurrences] = List[Occurrences]()

      for o <- occurrences do
        for i <- 0 until o._2 do
          extendedOcc = extendedOcc ++ List(Tuple2(o._1, i))

      for i <- 0 to occurrences.length do
        subSetList = subSetList ++ extendedOcc.combinations(i).toList

      for subSet <- subSetList do
          filtedSubSetList = filtedSubSetList ++ List(subSet.filter(_._2 != 0).toMap.map(kv => Tuple2(kv._1, kv._2)).toList)
      
      filtedSubSetList.toSet.toList.map(occ => occ.sortBy(_._1))

    def subtract(x: List[(Char, Int)], y: List[(Char, Int)]): List[(Char, Int)] =
      //x.filterNot(y.toSet)
      var subtractedList:List[(Char, Int)] = List[(Char, Int)]()
      val mapX = x.toMap
      val mapY = y.toMap

      //x.toMap.filter(kv => !mapY.contains(kv._1)).toList
      x.toMap.map(kv =>
        if mapY.contains(kv._1) then (kv._1, kv._2 - mapY(kv._1))
        else (kv._1, kv._2)
      ).toList.filter((k, v) => v != 0).sortBy(_._1)


    def sentenceAnagrams(sentence: Sentence): List[Sentence] = 
      def iter(occ: Occurrences): List[Sentence] = 
        if occ.isEmpty then List(Nil)
        else 
          for
            comb <- combinations(occ)
            word <- dictionaryByOccurrences.getOrElse(comb, Nil)
            rest <- iter(subtract(occ, wordOccurrences(word)))
          yield word :: rest

      iter(sentenceOccurrences(sentence))



val a = List(("a", 2), ("b", 2))
a.toMap
for i <- 1 until 5 yield i
//Anagrams.sentenceAnagrams(List("player"))

List(1,2,3) == List(1,2,3)
Set(1, 2, 3) - Set(1, 2)

List(1,2,3).filterNot(List(1, 2).toSet)


val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
val r = List(('r', 1))

val jimmy = List(('j', 1), ('i', 1), ('m', 2), ('y', 1))
val my = List(('m', 1), ('y', 1))

val assessment = Anagrams.wordOccurrences("assessment")
val assess = Anagrams.wordOccurrences("assess")
val ment = Anagrams.wordOccurrences("ment")

Anagrams.subtract(jimmy, my)
Anagrams.subtract(assessment, assess)

Anagrams.wordOccurrences("Robert")
Anagrams.combinations(List(('a', 2), ('b', 2))).toSet

Anagrams.sentenceAnagrams(List("Linux", "rulez"))
val testing = List(
    List(),
    List(('a', 1)),
    List(('a', 2)),
    List(('b', 1)),
    List(('a', 1), ('b', 1)),
    List(('a', 2), ('b', 1)),
    List(('b', 2)),
    List(('a', 1), ('b', 2)),
    List(('a', 2), ('b', 2))
  ).toSet
val word = "heloworld"
val wordList = word.toList
val chSet = word.toSet

val occ = Anagrams.wordOccurrences("aabb")
occ.length
val extended = occ ++ List(Tuple2('a', 0)) ++ List(Tuple2('a', 1)) ++ List(Tuple2('b', 0)) ++ List(Tuple2('b', 1))
extended.toSet.subsets.map(_.toList).toList
extended.combinations(2).toList(1)
extended.combinations(2).toList(1).filter(_._2 != 0)

for i <- 0 until 5 do i
for i <- 0 until 5 yield i

def makeSubset(occ: Anagrams.Occurrences): List[List[(Char, Int)]] = //List[Anagrams.Occurrences] = 
  var extendedOcc = occ
  var subSetList:List[Anagrams.Occurrences] = List[Anagrams.Occurrences]()
  var filtedSubSetList:List[Anagrams.Occurrences] = List[Anagrams.Occurrences]()

  for o <- occ do
    for i <- 0 until o._2 do
      extendedOcc = extendedOcc ++ List(Tuple2(o._1, i))

  for i <- 0 to occ.length do
    subSetList = subSetList ++ extendedOcc.combinations(i).toList

  for subSet <- subSetList do
    //println(subSet.filter(_._2 != 0).toMap.map(kv => Tuple2(kv._1, kv._2)).toList)
    filtedSubSetList = filtedSubSetList ++ List(subSet.filter(_._2 != 0).toMap.map(kv => Tuple2(kv._1, kv._2)).toList)

  
  filtedSubSetList.toSet.toList

/*

makeSubset(occ)
Anagrams.combinations(occ)

val occ2 = Anagrams.sentenceOccurrences(List("Linux", "rulez"))
Anagrams.combinations(Anagrams.wordOccurrences("linux"))
Anagrams.combinations(occ2)


for
  i <- 1 until 3
  j <- 1 until 5
yield Tuple2(i, j)


val group = wordList.groupBy(_.toString)
group("l")
'c'.toString
chSet.map(ch => (ch, group(ch.toString).length))
Anagrams.wordOccurrences("Robert")
"Hello" + "world"

List("hello", "hello", "world").fold("")

Anagrams.sentenceOccurrences(List("hello", "hello", "world"))
Anagrams.dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet)
Anagrams.wordOccurrences("eat")
Anagrams.dictionary.groupBy(w => Anagrams.wordOccurrences(w))
Anagrams.dictionary.groupBy(Anagrams.wordOccurrences(_)).get(List(('a', 1), ('e', 1), ('t', 1)))
Anagrams.dictionary.groupBy(Anagrams.wordOccurrences(_)).get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet)
Anagrams.dictionary.groupBy(Anagrams.wordOccurrences(_)).get(List(('a', 1), ('e', 1), ('t', 1)))
Anagrams.wordAnagrams("eat")
*/

val m = Map(1 -> 1, 2 -> 2, 3 -> 3)
m.getOrElse(1, 0)
m.getOrElse(2, 0)
m.getOrElse(3, 0)
m.getOrElse(4, 0)

Anagrams.wordOccurrences("aabb")

object Dictionary:
  def loadDictionary: List[String] =
    val wordstream = Option {
      getClass.getResourceAsStream(List("forcomp", "linuxwords.txt").mkString("/", "/", ""))
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try
      val s = Source.fromInputStream(wordstream)(Codec.UTF8)
      s.getLines().toList
    catch
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    finally
      wordstream.close()
