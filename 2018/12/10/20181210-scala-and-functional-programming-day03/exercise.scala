// 연습문제 4.1
sealed trait MyOption[+A] {
  def map[B](f: A => B): MyOption[B] = this match {
    case MySome(a) => MySome(f(a))
    case MyNone => MyNone
  }

  def flatMap[B](f: A => MyOption[B]): MyOption[B] = this match {
    case MySome(a) => f(a)
    case MyNone => MyNone
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case MySome(a) => a
    case MyNone => default
  }

  def getOrElse2[B >: A](default: B): B = this match {
    case MySome(a) => a
    case MyNone => default
  }

  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] = this match {
    case MySome(a) => MySome(a)
    case MyNone => ob
  }

  def filter(f: A => Boolean): MyOption[A] = this match {
    case MySome(a) if (f(a)) => MySome(a)
    case MyNone => MyNone
  }
}

case class MySome[+A](get: A) extends MyOption[A]

case object MyNone extends MyOption[Nothing]

case class Employee(name: String, dept: String, houseCount: MyOption[Int])

println(MySome(1).map(a => a + "AAA"))
println(MySome(Employee("Lee", "Datapart", MyNone)).flatMap(a => a.houseCount))
println(MyNone.map(_ => "BBB"))
println(MySome(1).getOrElse(throw new RuntimeException("엄서욤!")))
//println(MyNone.getOrElse(throw new RuntimeException("엄서욤!")))
println(MyNone.orElse(MySome("Empty")))
println(MySome("Lee").orElse(MySome("Empty")))
println(MySome("Lee").filter(_.length > 2))


//def flatMap2[B](f: A => MyOption[B]): MyOption[B] =
//  if (this.isInstanceOf[MyNone.type]) {
//    MyNone
//  }
//  else {
//    f(this.asInstanceOf[MySome[A]].get)
//  }


// 연습문제 4.2


// 연습문제 4.3
def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = age * numberOfSpeedingTickets * 0.1

def map2[A, B, C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] = {
  a match {
    case MySome(ax) => {
      b match {
        case MySome(bx) => MySome(f(ax, bx))
        case MyNone => MyNone
      }
    }
    case MyNone => MyNone
  }
}

def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): MyOption[Double] = {
  val optAge: MyOption[Int] = MyTry(age.toInt)
  val optTickets: MyOption[Int] = MyTry(numberOfSpeedingTickets.toInt)

  map2(optAge, optTickets)(insuranceRateQuote)
}

def MyTry[A](a: => A): MyOption[A] =
  try MySome(a)
  catch {
    case e: Exception => MyNone
  }

println(parseInsuranceRateQuote("20", "2"))


// 연습문제 4.4
def sequence[A](a: List[MyOption[A]]): MyOption[List[A]] =
  MyTry(for (element <- a) yield {
    element match {
      case MySome(v) => v
    }
  })

def parseInts(a: List[String]): MyOption[List[Int]] = sequence(a map (i => MyTry(i.toInt)))

println(parseInts(List("1", "T", "3", "4", "5")))
println(parseInts(List("1", "2", "3", "4", "5")))


// 연습문제 4.5
def traverse[A, B](a: List[A])(f: A => MyOption[B]): MyOption[List[B]] = {
  MyTry(for (element <- a) yield {
    f(element) match {
      case MySome(v) => v
    }
  })
}

def parseInts2(a: List[String]): MyOption[List[Int]] = traverse(a)(i => MyTry(i.toInt))

println(parseInts2(List("1", "T", "3", "4", "5")))
println(parseInts2(List("1", "2", "3", "4", "5")))


// 연습문제 4.6
sealed trait MyEither[+E, +A]
case class MyLeft[+E](value: E) extends MyEither[E, Nothing]
case class MyRight[+A](value: A) extends MyEither[Nothing, A]

def mean3(xs: IndexedSeq[Double]): MyEither[String, Double] =
  if (xs.isEmpty)
    MyLeft("mean of empty list!")
  else
    MyRight(xs.sum / xs.length)

def MyTry2[A](a: => A): MyEither[Exception, A] =
  try MyRight(a)
  catch { case e: Exception => MyLeft(e) }
