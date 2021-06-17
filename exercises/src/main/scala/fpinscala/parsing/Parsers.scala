package fpinscala.parsing

import language.higherKinds
import scala.util.matching.Regex

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))

  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  // From book TODO: check if it needs to be abstract after implementing map
  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)

  def slice[A](p: Parser[A]): Parser[String]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List())
    else map2(p, listOfN(n-1, p))(_ :: _)

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  implicit def regex(r: Regex): Parser[String]

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    flatMap(p)(a => map(p2)(b => (a, b)))

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    for {a <- p; b <- p2} yield f(a, b)

  //book implementation of map2
    //product(p, p2) map (f.tupled)

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")

  /* Parser that recognizes zero or more repetitions of the character 'a' and returns the number of character it has seen
   */
  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = {
    flatMap(p)(a => succeed(f(a)))
    //flatMap(p)(f andThen succeed)
  }

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]


  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]

  def skipL[B](p: Parser[Any], p2: => Parser[B]): Parser[B] =
    map2(slice(p), p2)((_, b) => b)

  def skipR[A](p: Parser[A], p2: => Parser[Any]): Parser[A] =
    map2(p, slice(p2))((a, _) => a)

  def whitespace: Parser[String] = "\\s*".r

  def token[A](p: Parser[A]): Parser[A] =
    attempt(p) <* whitespace

  def sep[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] = // use `Parser[Any]` since don't care about result type of separator
    sep1(p,p2) or succeed(List())

  def sep1[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] =
    map2(p, many(p2 *> p))(_ :: _)

  def surround[A](start: Parser[Any], stop: Parser[Any])(p: => Parser[A]) =
    start *> p <* stop

  def eof: Parser[String] =
    regex("\\z".r).label("unexpected trailing characters")

  def root[A](p: Parser[A]): Parser[A] =
    p <* eof


  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def many = self.many(p)

    def slice: Parser[String] = self.slice(p)

    def **[B](p2: => Parser[B]): Parser[(A,B)] =
      self.product(p,p2)
    def product[B](p2: => Parser[B]): Parser[(A,B)] =
      self.product(p,p2)

    def flatMap[B](f: A => Parser[B]): Parser[B] =
      self.flatMap(p)(f)

    def label(msg: String): Parser[A] = self.label(msg)(p)

    def *>[B](p2: => Parser[B]) = self.skipL(p, p2)
    def <*(p2: => Parser[Any]) = self.skipR(p, p2)
    def token = self.token(p)
    def sep(separator: Parser[Any]) = self.sep(p, separator)
    def sep1(separator: Parser[Any]) = self.sep1(p, separator)

  }

  object Laws {
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}