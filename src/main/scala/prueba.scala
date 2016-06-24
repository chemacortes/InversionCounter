object prueba extends App {

  import scala.io.Source
  import inversionCounter._

  val l = OrderedList(List(1, 3, 4, 2, 5, 6, 8, 7))

  println(l)

  val s = OrderedList("Hello, world!".toList)
  println(s)

  val array = Source.fromFile("IntegerArray.txt").getLines map (_.toInt)

  val res = OrderedList(array.toList)

  println(res.numInv) //--> 2407905288

}