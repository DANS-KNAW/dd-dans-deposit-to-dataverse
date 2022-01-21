/*
 * Copyright (C) 2020 DANS - Data Archiving and Networked Services (info@dans.knaw.nl)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package nl.knaw.dans.easy.dd2d.mapping

import scala.xml.Node

object Amd {

  def toDateOfDeposit(depositOrigin: String)(node: Node): Option[String] = {
    getFirstChangeToState(node, "SUBMITTED", depositOrigin)
      .orElse {
        getFirstChangeToState(node, "PUBLISHED", depositOrigin)
      }
  }

  def getFirstChangeToState(amd: Node, state: String, depositOrigin: String): Option[String] = {
    if (depositOrigin == "VAULT")
      DateTypeElement.toYearMonthDayFormat((amd \ "lastStateChange").head)
    else
      (amd \ "stateChangeDates" \ "stateChangeDate")
        .filter(sc => (sc \ "toState").text == state)
        .toList
        .map(_ \ "changeDate")
        .map(_.head)
        .map(DateTypeElement.toYearMonthDayFormat)
        .map(_.get)
        .sorted
        .headOption
  }
}
