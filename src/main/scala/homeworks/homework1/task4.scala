package homeworks.homework1


object task4 extends App {
  /**
   * Реализуйте метод optionPairMax, возвращающий максимум двух Option.
   * Если оба Option определены, нужно вернуть тот, значеие которого больше.
   * Если определен только один Option, нужно вернуть его.
   * Если ни один не определен, нужно вернуть None.
   */
  def optionPairMax(option1: Option[Int], option2: Option[Int]): Option[Int] = (option1, option2) match {
    case (first, second) if first.isEmpty => second
    case (first, second) if second.isEmpty => first
    case (Some(first), Some(second)) if first > second => Some(first)
    case (_, second) => second
  }

  println(optionPairMax(Some(3), Some(5)))
  // Some(5)
  println(optionPairMax(Some(1), Some(2)))
  // Some(2)
  println(optionPairMax(Some(6), None))
  // Some(6)
  println(optionPairMax(None, Some(8)))
  // Some(8)
  println(optionPairMax(None, None))
  // None
}
