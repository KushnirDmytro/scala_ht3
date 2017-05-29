//Object oriented decomposition session

trait Expr {
  def eval: Int
}

class Number(n: Int) extends Expr {
  def eval = n
}

class Sum(lOp: Expr, rOp: Expr) extends Expr {
  def eval = lOp.eval + rOp.eval
}

new Sum(new Number(5), new Number(4))

new Sum(new Number(5), new Number(4)).eval

new Sum(new Number(5),
  new Sum(new Number(4), new Number(-3))).eval