import scala.io.StdIn

case class MenuOption[A](text: String, value: A)

trait Ui:
  def print(text: String): Unit
  def println(text: String): Unit
  def readLine(prompt: String): Option[String]
  def choose[A](prompt: String, options: Seq[MenuOption[A]]): Option[A]

object ConsoleUi extends Ui:
  def print(text: String): Unit =
    Predef.print(text)

  def println(text: String): Unit =
    Predef.println(text)

  def readLine(prompt: String): Option[String] =
    Predef.print(prompt)
    Option(StdIn.readLine())
  
  def choose[A](prompt: String, options: Seq[MenuOption[A]]): Option[A] =
    // zipWithIndex превращает пункты в пары (пункт, индекс); индекс начинается с 0.
    options.zipWithIndex.foreach { case (option, index) =>
      // Пользователю показываем нумерацию с 1, поэтому index + 1
      Predef.println(s"${index + 1}) ${option.text}")
    }
    // flatMap: если строки нет, дальше не идём; иначе получаем raw: String.
    readLine(prompt).flatMap { raw =>
      raw.trim.toIntOption
        // Переводим номер с 1 на индекс с 0; lift безопасно проверяет диапазон
        .flatMap(number => options.lift(number - 1))
        // Достаём значение пункта; map, потому что value - обычное значение, а не Option.
        .map(_.value)
    }
