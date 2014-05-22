package object InversionCounter {

  case class OrderedList[T <: Ordered[T]](list: List[T], numInv: BigInt) {
    def merge(withList: OrderedList[T]) =
      OrderedList.merge(list, withList.list, Nil, 0)
  }

  object OrderedList {
    def apply[T <: Ordered[T]](list: List[T]) = sort(list)

    private def sort[T](list: List[T]): OrderedList[T] = {
      val n = list.length
      if (n == 1)
        new OrderedList(list, 0)
      else {
        val (left, right) = list.splitAt(n / 2)
        OrderedList(left) merge OrderedList(right)
      }
    }

    private def merge[T <: Ordered[T]](left: List[T], right: List[T], total: List[T], numInv: BigInt): OrderedList[T] =
      (left, right) match {
        case (Nil, _) => new OrderedList(right, numInv)
        case (_, Nil) => new OrderedList(left, numInv)
        case (b :: btail, c :: ctail) =>
          if (b <= c)
            merge(btail, right, total :+ b, numInv)
          else
            merge(left, ctail, total :+ c, numInv + left.length)
      }
  }

}