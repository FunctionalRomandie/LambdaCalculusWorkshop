import org.scalatest.FunSpec

sealed trait Exp
case class Lit(i: Int) extends Exp
case class App(op: Op, e1: Exp, e2: Exp) extends Exp
case class Var(name: String) extends Exp
case class Let(bind: Bind, exp: Exp) extends Exp

case class Bind(name: String, exp: Exp)

sealed trait Op
case object Add extends Op
case object Sub extends Op

object Interpreter {
  def apply(data: Map[String, Exp], e: Exp): Int = e match {
    case Lit(i) ⇒ i
    case App(Add, l, r) ⇒ apply(data, l) + apply(data, r)
    case App(Sub, l, r) ⇒ apply(data, l) - apply(data, r)
    case Var(name) ⇒ apply(data, data(name))
    case Let(b, exp) ⇒ apply(data + (b.name -> b.exp), exp)
  }
  def apply(exp: Exp): Int = apply(Map.empty[String, Exp], exp)
}

class MainSpec extends FunSpec {
  describe("Interpreter") {
    it("should work") {
      val program = App(Add, Lit(21), Lit(21))
      assert(Interpreter(program) === 42)
    }

    it("should work too") {
      val program = App(Add, Lit(21), Lit(21))
      val lambda = App(Add, Var("x"), App(Sub, program, Lit(2)))
      assert(Interpreter(Map("x" -> Lit(42)), lambda) === 82)
    }
    it("should work too too") {
      val program = Let(Bind("x", Lit(21)), App(Add, Var("x"), Lit(21)))
      assert(Interpreter(program) === 42)
    }
  }
}
