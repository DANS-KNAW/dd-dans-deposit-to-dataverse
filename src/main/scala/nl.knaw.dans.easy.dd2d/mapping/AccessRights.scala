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

object AccessRights {
  private val accessRightsToDefaultRestrict = Map(
    "OPEN_ACCESS" -> false,
    "OPEN_ACCESS_FOR_REGISTERED_USERS" -> true,
    "REQUEST_PERMISSION" -> true,
    "NO_ACCESS" -> true)

  def toDefaultRestrict(node: Node): Boolean = {
    accessRightsToDefaultRestrict.getOrElse(node.text, true)
  }

  def isEnableRequests(accessRightsNode: Node, filesNode: Node): Boolean = {
    val explicitAccessibleToValues = filesNode \ "file" \ "accessibleToRights"
    val numberOfFiles = (filesNode \ "file").size

    def isExplicitlyDefinedNoAccessFilePresent = {
      explicitAccessibleToValues.map(_.text).contains("NONE")
    }

    def isImplicitlyDefinedNoAccessFilePresent = {
      numberOfFiles > explicitAccessibleToValues.size && accessRightsNode.text == "NO_ACCESS"
    }

    /*
     * If one or more files are explicitly fully closed, the complete dataset must be not allow permission requests.
     * If there are implicitly defined accessibleTo values the access category must not be NONE, because that means
     * the implicit accessibleTo is NO_ACCESS. See: https://dans-knaw.github.io/dans-bagit-profile/versions/0.0.0/#4-bag-sequence-requirements
     */
    !isExplicitlyDefinedNoAccessFilePresent && !isImplicitlyDefinedNoAccessFilePresent
  }
}
