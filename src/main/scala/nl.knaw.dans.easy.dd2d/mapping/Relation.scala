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

object Relation extends BlockRelation {
  private val labelToType = Map(
    "relation" -> "relation",
    "conformsTo" -> "conforms to",
    "hasFormat" -> "has format",
    "hasPart" -> "has part",
    "references" -> "references",
    "replaces" -> "replaces",
    "requires" -> "requires",
    "hasVersion" -> "has version",
    "isFormatOf" -> "is format of",
    "isPartOf" -> "is part of",
    "isReferencedBy" -> "is referenced by",
    "isReplacedBy" -> "is replaced by",
    "isRequiredBy" -> "is required by",
    "isVersionOf" -> "is version of")

  def toRelationValueObject(node: Node): JsonObject = {
    val m = FieldMap()
    m.addCvField(RELATION_TYPE, labelToType.getOrElse(node.label, "relation"))
    m.addPrimitiveField(RELATION_URI, node.attribute("href").map(_.text).getOrElse(""))
    m.addPrimitiveField(RELATION_TEXT, node.text)
    m.toJsonObject
  }

  def isRelation(node: Node): Boolean = {
    labelToType.keySet contains node.label
  }
}
