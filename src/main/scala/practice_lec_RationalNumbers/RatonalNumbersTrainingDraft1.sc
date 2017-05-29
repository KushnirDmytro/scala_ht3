type Rational = ( (Int, Int) => Int ) => Int


def gcd(a: Int,b: Int): Int = {
  if(b ==0) a else gcd(b, a%b)
}

//setters
def newRational(n:Int, d:Int) :Rational =
  (f: (Int, Int)=> Int ) => f(n/gcd(n,d),d/ gcd(n,d))

//getters
def numer (r:Rational) :Int = r((n,d) => n)
def denom (r:Rational) :Int = r((n,d) => d)


def toStr(r:Rational) = numer(r) + "/" + denom(r)

//opers
def add(r1:Rational, r2:Rational): Rational =
newRational( numer(r1)*denom(r2) + numer(r2)*denom(r1),
  denom(r1) * denom(r2))


def sub(r1:Rational, r2:Rational): Rational =
  newRational( numer(r1)*denom(r2) - numer(r2)*denom(r1),
    denom(r1) * denom(r2))

def mul(r1:Rational, r2:Rational): Rational =
  newRational( numer(r1)*numer(r2),
    denom(r1) * denom(r2))

def div(r1:Rational, r2:Rational): Rational =
  newRational( numer(r1)*denom(r2),
    denom(r1) * numer(r2))

def equal_?(r1:Rational, r2:Rational): Boolean =
   numer(r1)*denom(r2) ==  denom(r1) * numer(r2)


val one_half = newRational(1,2)
val one_third = newRational(1,3)
var rez_add = add(one_third , one_third)
println(toStr(rez_add))

val rez_sub = sub(one_half, one_third)
println(toStr(rez_sub))

val rez_mul = mul (one_half, one_third)
println(toStr(rez_mul))

val rez_div = div(one_half, one_third)
println(toStr(rez_div))

equal_?(rez_div, newRational(3,2))


numer(one_half)
denom(one_half)

toStr(one_half)

println(toStr(one_half))