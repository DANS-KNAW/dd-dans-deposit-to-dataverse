/**
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

import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.xml.Node

object Language extends BlockCitation with DebugEnhancedLogging {
  def isIsoLanguage(node: Node): Boolean = {
    hasXsiType(node, "ISO639-2") || node.attribute("encodingScheme").flatMap(_.headOption.map(_.text == "ISO639-2")).getOrElse(false)
  }

  def toCitationBlockLanguage(isoToDataverse: Map[String, String])(node: Node): Option[String] = {
    if (isIsoLanguage(node)) node.attribute("code").flatMap(_.headOption.flatMap(a => isoToDataverse.get(a.text)))
    else Option.empty[String]
  }

  def langAttributeToMetadataLanguage(isoToDataverse: Map[String, String])(node: Node): Option[String] = {
    node.attribute(XML_NAMESPACE_URI, "lang").flatMap(_.headOption.flatMap(a => isoToDataverse.get(a.text)))
  }

  def toKeywordValue(node: Node): JsonObject = {
    val m = FieldMap()
    m.addPrimitiveField(KEYWORD_VALUE, node.text)
    m.addPrimitiveField(KEYWORD_VOCABULARY, "")
    m.addPrimitiveField(KEYWORD_VOCABULARY_URI, "")
    m.toJsonObject
  }
}
