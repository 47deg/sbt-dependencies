package dependencies

object TablePrinter {

  def format(table: List[List[String]]): String = table match {
    case Nil => ""
    case _ =>
      val sizes = table map {
        _.map { cell =>
          Option(cell) map (_.length) getOrElse 0
        }
      }
      val colSizes = sizes.transpose map (_.max)
      val rows = table.zipWithIndex.map {
        case (row, index) => formatRow(row, colSizes, index == 0)
      }
      formatRows(rowSeparator(colSizes), rows)
  }

  def formatRows(rowSeparator: String, rows: List[String]): String =
    (rowSeparator ::
      rows.head ::
      rowSeparator ::
      rows.tail :::
      rowSeparator ::
      List()).mkString("\n")

  def formatRow(row: List[String], colSizes: List[Int], header: Boolean): String = {
    val cells = row.zip(colSizes) map {
      case (item, size) if size == 0 => ""
      case (item, size)              => ("%" + size + "s").format(item)
    }
    cells.mkString("|", "|", "|")
  }

  def rowSeparator(colSizes: List[Int]): String =
    colSizes map { "-" * _ } mkString ("+", "+", "+")

}
