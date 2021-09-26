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

object Identifier extends BlockCitation with BlockArchaeologySpecific {
  val archisNumberTypeToCvItem = Map(
    "ARCHIS-ONDERZOEK" -> "onderzoek",
    "ARCHIS-VONDSTMELDING" -> "vondstmelding",
    "ARCHIS-MONUMENT" -> "monument",
    "ARCHIS-WAARNEMING" -> "waarneming"
  )

  def canBeMappedToOtherId(node: Node): Boolean = {
    hasXsiType(node, "EASY2") || !attributeExists(node, XML_SCHEMA_INSTANCE_URI, "type")
  }

  def toOtherIdValue(node: Node): JsonObject = {
    val m = FieldMap()
    if (hasXsiType(node, "EASY2")) {
      m.addPrimitiveField(OTHER_ID_AGENCY, "DANS-KNAW")
      m.addPrimitiveField(OTHER_ID_VALUE, node.text)
    }
    else if (!attributeExists(node, XML_SCHEMA_INSTANCE_URI, "type")) {
      m.addPrimitiveField(OTHER_ID_AGENCY, "")
      m.addPrimitiveField(OTHER_ID_VALUE, node.text)
    }
    m.toJsonObject
  }

  def isRelatedPublication(node: Node): Boolean = {
    hasXsiType(node, "ISBN") || hasXsiType(node, "ISSN")
  }

  def toRelatedPublicationValue(node: Node): JsonObject = {
    val m = FieldMap()
    m.addPrimitiveField(PUBLICATION_CITATION, "")
    m.addPrimitiveField(PUBLICATION_ID_NUMBER, node.text)
    m.addCvField(PUBLICATION_ID_TYPE, getIdType(node))
    m.addPrimitiveField(PUBLICATION_URL, "")
    m.toJsonObject
  }

  private def getIdType(node: Node): String = {
    node.attribute(XML_SCHEMA_INSTANCE_URI, "type").map(_.text.toLowerCase).map(s => s.substring(s.indexOf(':') + 1)).getOrElse("")
  }

  def isNwoGrantNumber(node: Node): Boolean = {
    hasXsiType(node, "NWO-PROJECTNR")
  }

  def toNwoGrantNumberValue(node: Node): JsonObject = {
    val m = FieldMap()
    m.addPrimitiveField(GRANT_NUMBER_AGENCY, "NWO")
    m.addPrimitiveField(GRANT_NUMBER_VALUE, node.text)
    m.toJsonObject
  }

  def isArchisZaakId(node: Node): Boolean = {
    hasXsiType(node, "ARCHIS-ZAAK-IDENTIFICATIE")
  }

  def toArchisZaakId(node: Node): Option[String] = {
    if (hasXsiType(node, "ARCHIS-ZAAK-IDENTIFICATIE")) Some(node.text)
    else Option.empty
  }

  def isArchisNumber(node: Node): Boolean = {
    hasXsiType(node, "ARCHIS-ONDERZOEK") ||
      hasXsiType(node, "ARCHIS-VONDSTMELDING") ||
      hasXsiType(node, "ARCHIS-MONUMENT") ||
      hasXsiType(node, "ARCHIS-WAARNEMING")
  }

  def toArchisNumberValue(node: Node): JsonObject = {
    val m = FieldMap()
    val optArchisNumberType = node.attribute(XML_SCHEMA_INSTANCE_URI, "type").map(_.text)
    val optArchisNumberId = node.text

    if (optArchisNumberType.isEmpty) throw new IllegalArgumentException("Archis number without type")
    if (optArchisNumberId.trim.isEmpty) throw new IllegalArgumentException("Archis number without ID")
    val archisNumberType = optArchisNumberType.get

    m.addCvField(ARCHIS_NUMBER_TYPE, archisNumberTypeToCvItem(archisNumberType.substring(archisNumberType.indexOf(":") + 1)))
    m.addPrimitiveField(ARCHIS_NUMBER_ID, optArchisNumberId)
    m.toJsonObject
  }
}
