/*
 * Copyright 2017-2020 47 Degrees, LLC. <http://www.47deg.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package dependencies

object TablePrinter {

  def format(table: List[List[String]]): String = table match {
    case Nil => ""
    case _ =>
      val sizes = table map {
        _.map(cell => Option(cell) map (_.length) getOrElse 0)
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
