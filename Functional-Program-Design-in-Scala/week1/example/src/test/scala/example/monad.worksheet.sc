abstract class Try[+T]
case class Success[+T](x: T)        extends Try[T]
case class Failure(ex: Exception)   extends Try[Nothing]