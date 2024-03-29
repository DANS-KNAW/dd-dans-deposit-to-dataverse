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

import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.xml.Node

object DatesOfCollection extends BlockCitation with DebugEnhancedLogging {
  private val rangePattern = """^(.*)/(.*)$""".r(DATE_OF_COLLECTION_START, DATE_OF_COLLECTION_END)

  def toDateOfCollectionValue(node: Node): JsonObject = {
    val m = FieldMap()
    val matchIterator = rangePattern.findAllIn(node.text.trim)
    m.addPrimitiveField(DATE_OF_COLLECTION_START, matchIterator.group(DATE_OF_COLLECTION_START))
    m.addPrimitiveField(DATE_OF_COLLECTION_END, matchIterator.group(DATE_OF_COLLECTION_END))
    m.toJsonObject
  }
}
