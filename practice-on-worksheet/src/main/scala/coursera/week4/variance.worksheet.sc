// Roughly speaking, a type that accepts mutations of its elements should not be covariant
// But immutable types can be covariant, if some conditions on methods are met

trait Fruit
class Apple extends Fruit
class Orange extends Fruit

type FtoO = Fruit => Orange // True: Orange is Fruit
type AtoF = Apple => Fruit // False: Fruit is Apple

trait Function1[-T, +U]:
    def apply(x: T): U

/* Error
trait Function2[-T, +U]:
    def apply(x: U): T
*/
