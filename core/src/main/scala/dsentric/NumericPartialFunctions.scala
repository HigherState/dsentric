package dsentric

import java.text.NumberFormat

object NumericPartialFunctions {

  private val numericRegex         = "^[\\+-]{0,1}(\\d{1,3}[\\.', ](\\d{3}[\\.', ])*\\d{3}([\\.,]\\d*)?|\\d*([\\.,]\\d*)?)$".r
  private val numberFormat         = NumberFormat.getInstance()
  private def isNumeric(s: String) =
    !s.isEmpty && numericRegex.pattern.matcher(s).matches() && s != "-" && s != "+" && s != "." && s != ","

  def byte: PartialFunction[Any, Byte] = {
    case n: Byte                                              => n
    case n: Short if n <= Byte.MaxValue && n >= Byte.MinValue => n.toByte
    case n: Int if n <= Byte.MaxValue && n >= Byte.MinValue   => n.toByte
    case n: Long if n <= Byte.MaxValue && n >= Byte.MinValue  => n.toByte
    case n: Double if n % 1 == 0 && n <= Byte.MaxValue && n >= Byte.MinValue => n.toByte
    case n: Float if n  % 1 == 0 && n <= Byte.MaxValue && n >= Byte.MinValue => n.toByte
  }

  def short: PartialFunction[Any, Short] = {
    case n: Byte                                               => n.toShort
    case n: Short                                              => n
    case n: Int if n <= Short.MaxValue && n >= Short.MinValue  => n.toShort
    case n: Long if n <= Short.MaxValue && n >= Short.MinValue => n.toShort
    case n: Double if n % 1 == 0 && n <= Short.MaxValue && n >= Short.MinValue => n.toShort
    case n: Float if n  % 1 == 0 && n <= Short.MaxValue && n >= Short.MinValue => n.toShort
  }

  def int: PartialFunction[Any, Int] = {
    case n: Int                                            => n
    case n: Long if n <= Int.MaxValue && n >= Int.MinValue => n.toInt
    case n: Double if n % 1 == 0 && n <= Int.MaxValue && n >= Int.MinValue => n.toInt
    case n: Float if n  % 1 == 0 && n <= Int.MaxValue && n >= Int.MinValue => n.toInt
    case n: Short => n.toInt
    case n: Byte  => n.toInt
  }

  def long: PartialFunction[Any, Long] = {
    case n: Int  => n.toLong
    case n: Long => n
    case n: Double if n % 1 == 0 && n <= Long.MaxValue && n >= Long.MinValue => n.toLong
    case n: Float if n  % 1 == 0 && n <= Long.MaxValue && n >= Long.MinValue => n.toLong
    case n: Short => n.toLong
    case n: Byte  => n.toLong
  }

  def float: PartialFunction[Any, Float] = {
    case n: Float                                                => n
    case n: Int                                                  => n.toFloat
    case n: Long                                                 => n.toFloat
    case n: Double if n <= Float.MaxValue && n >= Float.MinValue => n.toFloat
    case n: Short                                                => n.toFloat
    case n: Byte                                                 => n.toFloat
  }

  def double: PartialFunction[Any, Double] = {
    case n: Double => n
    case n: Long   => n.toDouble
    case n: Float  => n.toDouble
    case n: Int    => n.toDouble
    case n: Short  => n.toDouble
    case n: Byte   => n.toDouble
  }

  def number: PartialFunction[Any, Number] = {
    case n: Double => n
    case n: Long   => n
    case n: Float  => n
    case n: Int    => n
    case n: Short  => n
    case n: Byte   => n
  }

  def stringDouble: PartialFunction[Any, Double] = {
    case s: String if isNumeric(s) =>
      numberFormat.parse(s).doubleValue()
  }
}
