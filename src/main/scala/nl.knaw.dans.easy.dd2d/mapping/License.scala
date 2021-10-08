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

import java.net.URI
import scala.util.Try
import scala.xml.Node

object License {

  def isLicenseUri(node: Node): Boolean = {
    if (node.label != "license") false
    else if (node.namespace != DCTERMS_NAMESPACE_URI) false
         else if (!hasXsiType(node, "URI")) false
              else isValidUri(node.text)
  }

  private def isValidUri(s: String): Boolean = {
    Try {
      new URI(s)
    }.isSuccess
  }

  def getLicenseUri(node: Node): URI = {
    if (isLicenseUri(node)) new URI(node.text)
    else throw new IllegalArgumentException("Not a valid license node")
  }
}
