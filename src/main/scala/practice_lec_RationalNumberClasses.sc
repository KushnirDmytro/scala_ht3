class Rational (n:Int, d:Int){
  require(d != 0, "Zero denominator is not allowed!")

  private def gcd(a: Int,b: Int): Int = {
    if(b ==0) a else gcd(b, a%b)
  }

  val numer: Int = n
  val denom: Int = d

  //opers
  def add(that:Rational): Rational =
    new Rational( this.numer * that.denom + this.denom * that.numer,
      this.denom * that.denom)

  def sub(that:Rational): Rational =
  add(that.neg())

  def mul(that:Rational): Rational =
    new Rational( this.numer * that.numer,
      this.denom * that.denom)

  def div(that:Rational): Rational =
    new Rational( this.numer * that.denom,
      this.denom * that.numer)

  def neg():Rational=
  new Rational(-this.numer, this.denom)

  def equals_?(that:Rational): Boolean ={
    this.numer * that.denom == this.denom * that.numer
  }
/*
  def equal_?(r1:Rational, r2:Rational): Boolean =
    numer(r1)*denom(r2) ==  denom(r1) * numer(r2)

*/

  override def toString: String = numer/gcd(n,d) + "/" + denom/gcd(n,d)
}


//def toStr(r:Rational) = numer(r) + "/" + denom(r)


/*
val one_half = new Rational(1,2)


val one_third = new Rational(1,3)
var rez_add = one_third.add( one_third)
println(rez_add)
rez_add.equals_?(new Rational(2,3))

var rez_sub = one_half.sub( one_third)
println(rez_sub)

var rez_mul = one_half.mul( one_third)
println(rez_mul)

var rez_div = one_half.div( one_third)
println(rez_div)

var check = one_half.sub(one_half).sub(one_third)
println(check)
*/
//var test_wierd = new Rational(1,0)
//test_wierd.add(test_wierd)
/*
one_half add one_third
*/