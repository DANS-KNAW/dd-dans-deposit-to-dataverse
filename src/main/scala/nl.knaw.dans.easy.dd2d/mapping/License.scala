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

  def getLicenseUri(supportedLicenses: List[URI])(variantToNormalized: Map[String, String])(node: Node): URI = {
    if (isLicenseUri(node)) {
      val licenseUriStr = normalizeVariants(variantToNormalized)(removeTrailingSlash(node.text))
      val licenseUri = new URI(licenseUriStr)
      normalizeScheme(supportedLicenses)(licenseUri).getOrElse(throw new IllegalArgumentException(s"Unsupported license: ${ licenseUri.toASCIIString }"))
    }
    else throw new IllegalArgumentException("Not a valid license node")
  }

  private def removeTrailingSlash(uri: String): String = {
    if (uri.endsWith("/")) uri.substring(0, uri.length - 1)
    else uri
  }

  private def normalizeVariants(variantToNormalized: Map[String, String])(s: String): String = {
    variantToNormalized.getOrElse(s, s)
  }

  def normalizeScheme(supportedLicenses: List[URI])(uri: URI): Option[URI] = {
    supportedLicenses.find {
      sl =>
        uri.getHost == sl.getHost &&
          uri.getPath == sl.getPath &&
          uri.getPort == sl.getPort &&
          uri.getQuery == sl.getQuery
    }.filter { uri => uri.getScheme == "http" || uri.getScheme == "https" }
  }
}
