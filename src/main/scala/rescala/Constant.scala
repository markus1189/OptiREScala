package rescala

case class Constant[A](v: A) extends Signal[A] {
  def get = v
  def apply() = v
  def map[B](f: A => B) = Constant(f(v))
  def reEvaluate = v
}
