import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import rationalFancyOperators.Rational

object RationlaProps extends Properties("String") {

  property("unary_negation_works") = forAll { (n:Int, d:Int) =>
    if (d != 0)
    new Rational(n,d) == - (- new Rational(n,d) )
    else
      true
  }

  /*
  property("concatenate") = forAll { (a: String, b: String) =>
    (a+b).length > a.length && (a+b).length > b.length
  }

  property("substring") = forAll { (a: String, b: String, c: String) =>
    (a+b+c).substring(a.length, a.length+b.length) == b
  }

*/
}