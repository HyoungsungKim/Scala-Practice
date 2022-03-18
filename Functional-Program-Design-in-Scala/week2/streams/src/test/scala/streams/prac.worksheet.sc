List(List(1,2,3), List(4,5,6)).indexOf(1)
List(1,2,3).indexOf(1)
List(1,2,3).indexWhere(_ == 1)

val a = List(List(1,2,3), List(4,5,6))
a.zipWithIndex

val filtedA = a.zipWithIndex.map((row, idx) => if row.contains(1) then List(idx, row) else Nil)
filtedA
filtedA.flatten


List(1,2,3).contains(1)
List(1,2,3).filter(_==2)

a.zipWithIndex.filter(_._1.contains(1)).map(_._2)
a.zipWithIndex.filter(_._1.contains(4))
a.zipWithIndex.filter((row, _) => row.contains(4))
a.zipWithIndex.filter((row, idx) => row.contains(1))
a.zipWithIndex.filter((row, idx) => row.contains(1)).head

/*
a.zipWithIndex.flatMap{
    case (row, idx) if row.contains(1) => (idx, row)
}
*/

a.zipWithIndex.map((row, idx) => row.filter((col) => col == 4))
a.zipWithIndex.filter((row, idx) => row.contains(1))

for (row, idx) <- a.zipWithIndex if row.contains(2); col <- row  yield (col, row)
List(1, 2, 3).indexOf(2)

a.find(row => row.contains(1))
a.exists(row => row.contains(7))
List(1,2,3).exists(elem => elem == 1)
List(1,2,3).contains(1)
a.zipWithIndex.map((row, idx) => (row.filter((col) => col == 4), idx))
a.map(row => if row.contains(1) then row else Nil)
a.zipWithIndex
a.zipWithIndex.map((row, idx) => (row.filter(_==1), idx))
/*
def findRow(matrix: List[List[Int]], target:Int): Tuple2[List[Int], Int] = 
    matrix.zipWithIndex.map((row, idx) => row.filter(_ == target))
*/

enum Move:
    case Left, Right, Up, Down

Move.fromOrdinal(1)
Move.values.foreach(println(_))
Move.values.flatMap(List() ++ List(_)).toList
Move.values.flatMap(List() ++ List(_)).head
Move.values.map(List() ++ List(_)).tail
val ll = LazyList() #:: LazyList(4,5,6)
ll.toList

List((1, 1),(2, 2), (3, 3)).to(LazyList).map((l, r) => (l*2, r)).toList

List(1) ++ List(1,2)
//1 ++ List(1,2)
1 :: List(1,2)

LazyList(1,2,3).filter(_!=2).toList