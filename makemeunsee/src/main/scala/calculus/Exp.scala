package calculus

/**
 * Created by markus on 16/07/2015.
 */
sealed trait Exp {
  def interpret( context: Map[String, Exp] = Map.empty ): Int
}

case class Lit( i: Int ) extends Exp {
  def interpret( context: Map[String, Exp] ): Int = i
}

case class App( exp: Exp, args: Seq[Exp] ) extends Exp {
  def interpret( context: Map[String, Exp] ): Int = exp match {
    // reserved keyword
    case Var( "add" ) => args( 0 ).interpret( context ) + args( 1 ).interpret( context )
    case Var( "sub" ) => args( 0 ).interpret( context ) - args( 1 ).interpret( context )
    // apply lambda
    case Lambda( varName, exp_ ) => exp_.interpret( context + ( ( varName, args( 0 ) ) ) )
    // turn var to expr from context
    case Var( str ) => App( context( str ), args ).interpret( context )
    // just interpret
    case _ => exp.interpret( context )
  }
}

case class Var( name: String ) extends Exp {
  def interpret( context: Map[String, Exp] ): Int = context( name ).interpret( context )
}

case class Let( name: String, value: Exp, continuation: Exp ) extends Exp {
  def interpret( context: Map[String, Exp] ): Int =
    continuation.interpret( context + ( ( name, value ) ) )
}

case class Lambda( varName: String, exp: Exp ) extends Exp {
  def interpret( context: Map[String, Exp] ): Int = throw new Error( "not interpretable" )
}