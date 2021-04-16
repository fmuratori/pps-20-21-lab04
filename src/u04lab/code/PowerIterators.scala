package u04lab.code

import Optionals._
import Lists._
import u04lab.code.Lists.List.{Cons, Nil, append, drop, reverse}

trait PowerIterator[A] {
  def next(): Option[A]
  def allSoFar(): List[A]
  def reversed(): PowerIterator[A]
}

trait PowerIteratorsFactory {

  def incremental(start: Int, successive: Int => Int): PowerIterator[Int]
  def fromList[A](list: List[A])
  def randomBooleans(size: Int): PowerIterator[Boolean]
}

class PowerIteratorsFactoryImpl extends PowerIteratorsFactory {

  override def incremental(start: Int, successive: Int => Int): PowerIterator[Int] = new IncrementalPowerIterator(start, successive)

  override def fromList[A](list: List[A]): Unit = new FromListPowerIterator(list)

  override def randomBooleans(size: Int): PowerIterator[Boolean] = new BooleansPowerIterator()
}

class BooleansPowerIterator extends PowerIterator[Boolean]  {
  var allSoFarList:List[Boolean] = Nil()

  override def next(): Option[Boolean] = {
    val nextValue:Boolean = math.random < 0.25
    allSoFarList = append(allSoFarList, Cons(nextValue, Nil()))
    Option.of(nextValue)
  }

  override def allSoFar(): List[Boolean] = allSoFarList

  override def reversed(): PowerIterator[Boolean] = new FromListPowerIterator[Boolean](reverse(allSoFarList))
}

class FromListPowerIterator[A] (l: List[A]) extends PowerIterator[A] {
  var currentList: List[A] = l
  var allSoFarList:List[A] = Nil()

  override def next(): Option[A] = {

    currentList match {
      case Cons(h, t) => {
        currentList = drop(currentList, 1)
        allSoFarList = append(allSoFarList, Cons(h, Nil()))
        Option.of(h)
      }
      case Nil() => Option.empty
    }
  }

  override def allSoFar(): List[A] = allSoFarList

  override def reversed(): PowerIterator[A] = new FromListPowerIterator(reverse(allSoFarList))
}


class IncrementalPowerIterator[Int] (startValue:Int, incFunction:(Int => Int)) extends PowerIterator[Int] {
  var currentValue:Int = startValue
  var allSoFarList:List[Int] = Nil()

  override def next(): Option[Int] = {
    val outputValue = currentValue
    allSoFarList = append(allSoFarList, Cons(outputValue, Nil()))
    currentValue = incFunction(currentValue)
    Option.of(outputValue)
  }

  override def allSoFar(): List[Int] = allSoFarList

  override def reversed(): PowerIterator[Int] = new FromListPowerIterator(reverse(allSoFarList))
}
