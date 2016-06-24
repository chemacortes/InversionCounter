/*
 * Ejercicio de contaje de inversiones en una lista. (Generic type class)
 *
 * Este código usa tipos genéricos que puedan convertirse en `Ordered`: T <% Ordered[T]
 *
 */

import scala.annotation.tailrec

package object inversionCounter {

  case class OrderedList[T <% Ordered[T]](list: List[T], numInv: BigInt) {
    def ++(aList: OrderedList[T]) =
      merge(list, aList.list, Nil, numInv + aList.numInv)
  }

  object OrderedList {
    def apply[T <% Ordered[T]](list: List[T]) = sort(list)
  }

  /*
   * Por claridad, están aquí separadas las dos funciones del método *DIVIDE&CONQUER*
   *
   * 	sort  -- divide la lista en dos partes
   * 	merge -- mezcla dos listas ordenadas
   *
   */

  private def sort[T <% Ordered[T]](list: List[T]): OrderedList[T] = {
    val n = list.length
    if (n == 1)
      OrderedList(list, 0) // Base case
    else {
      val (left, right) = list.splitAt(n / 2)
      sort(left) ++ sort(right)
    }
  }

  @tailrec
  private def merge[T <% Ordered[T]](left: List[T],
                                     right: List[T],
                                     total: List[T],
                                     numInv: BigInt): OrderedList[T] =
    (left, right) match {
      case (Nil, _) => OrderedList(total ++ right, numInv)
      case (_, Nil) => OrderedList(total ++ left, numInv)
      case (b :: btail, c :: ctail) =>
        if (b <= c)
          merge(btail, right, total :+ b, numInv)
        else
          merge(left, ctail, total :+ c, numInv + left.length)
    }

}