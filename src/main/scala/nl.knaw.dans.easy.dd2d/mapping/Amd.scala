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

import org.apache.commons.lang.StringUtils

import scala.xml.Node

object Amd {

  def toDateOfDeposit(node: Node): Option[String] = {
    getFirstChangeToState(node, "SUBMITTED")
      .orElse {
        toPublicationDate(node: Node): Option[String]
      }
  }

  def toPublicationDate(node: Node): Option[String] = {
    getFirstChangeToState(node, "PUBLISHED")
      .orElse {
        (node \ "lastStateChange")
          .map(DateTypeElement.toYearMonthDayFormat)
          .headOption.flatten
      }
  }

  private def getFirstChangeToState(amd: Node, state: String): Option[String] = {
    (amd \ "stateChangeDates" \ "stateChangeDate")
      .filter(sc => (sc \ "toState").text == state)
      .map(n => (n \ "changeDate").headOption)
      .filter(_.isDefined)
      .map(_.get)
      .filter(n => StringUtils.isNotBlank(n.text))
      .map(DateTypeElement.toYearMonthDayFormat)
      .filter(_.isDefined)
      .map(_.get)
      .sorted
      .headOption
  }
}
