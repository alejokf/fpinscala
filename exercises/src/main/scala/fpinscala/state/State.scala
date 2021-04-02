package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    val nexValue = fromOldRangeToNewRange(i, Int.MinValue, Int.MaxValue, 0, Int.MaxValue)
    (nexValue, r)
  }

  def fromOldRangeToNewRange(oldValue: Int, oldMin: Int, oldMax: Int, newMin: Int, newMax: Int): Int = {
    val oldRange = oldMax - oldMin
    val newRange = newMax - newMin
    (((oldValue - oldMin) * newRange) / oldRange) + newMin
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d, r1) = double(rng)
    val (i, r2) = r1.nextInt
    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(c: Int, acc: List[Int], nextRng: RNG): (List[Int], RNG) = {
      if (c <= 0)
        (acc, nextRng)
      else {
        val (i, r) = nextRng.nextInt
        go(c - 1, acc :+ i, r)
      }
    }
    go(count, List(), rng)
  }

  // From the book
  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(_ - 1 % 2)

  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  // From the book
  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  // From the book
  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  // From the book
  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((r, acc) => map2(r, acc)(_ :: _))

  def intsViaSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r1) = f(rng)
      g(a)(r1)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }

  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)
}

import fpinscala.state.State.{get, modify, sequence, unit}

case class State[S,+A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })
}

object State {
  type Rand[+A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List()))((s, acc) => s.map2(acc)(_ :: _))

  // From the book
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  // From the book
  def get[S]: State[S, S] = State(s => (s, s))

  // From the book
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  //Given a function Input => Machine, it returns another Machine with the new state in the Machine
  def update: Input => Machine => Machine =
    input => machine =>
      // rules from the book described on a comment for each case
      (input, machine) match {
        // A machine that's out of candy ignores all inputs. This case must be the first one to avoid matching on other cases
        case (_, Machine(_, 0, _)) => machine
        // Inserting a coin into a locked machine will cause it to unlock if there is any candy left
        case (Coin, Machine(true, candies, coins)) => Machine(false, candies, coins + 1)
        // Turning the knob on a unlocked machine will cause it to dispense candy and become locked
        case (Turn, Machine(false, candies, coins)) => Machine(true, candies - 1, coins)
        // Inserting a coin into a unlocked machine does nothing
        case (Coin, Machine(false, _, _)) => machine
        // Turning the knob on a locked machine does nothing
        case (Turn, Machine(true, _, _)) => machine
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- sequence(inputs.map(modify[Machine] _ compose update))
      //_ <- sequence(inputs.map(modify[Machine].compose(update)))
      s <- get
    } yield (s.coins, s.candies)
}

object CandySimulator extends App {
  val machine = Machine(true, 10, 2)
  println("Initial Machine state: " + machine)
  private val inputs = List(Coin, Turn, Coin, Turn)
  println("Moves: " + inputs.mkString(", "))
  val s = Candy.simulateMachine(inputs)
  println("New Machine state" + s.run(machine)._2) //this should be Machine(true,8,4)
}
