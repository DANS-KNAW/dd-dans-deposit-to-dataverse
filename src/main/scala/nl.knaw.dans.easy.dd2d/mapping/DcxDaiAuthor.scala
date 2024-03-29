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

object DcxDaiAuthor extends Contributor with BlockCitation {
  // TODO: handle case where a DcxDaiOrganization is specified

  private case class Author(titles: Option[String],
                            initials: Option[String],
                            insertions: Option[String],
                            surname: Option[String],
                            dai: Option[String],
                            isni: Option[String],
                            orcid: Option[String],
                            role: Option[String],
                            organization: Option[String])

  private def parseAuthor(authorElement: Node) = Author(
    titles = (authorElement \ "titles").map(_.text).headOption,
    initials = (authorElement \ "initials").map(_.text).headOption,
    insertions = (authorElement \ "insertions").map(_.text).headOption,
    surname = (authorElement \ "surname").map(_.text).headOption,
    dai = (authorElement \ "DAI").map(_.text).headOption,
    isni = (authorElement \ "ISNI").map(_.text).headOption,
    orcid = (authorElement \ "ORCID").map(_.text).headOption,
    role = (authorElement \ "role").map(_.text).headOption,
    organization = (authorElement \ "organization" \ "name").map(_.text).headOption)

  def toAuthorValueObject(node: Node): JsonObject = {
    val m = FieldMap()
    val author = parseAuthor(node)
    val name = formatName(author)

    if (StringUtils.isNotBlank(name)) {
      m.addPrimitiveField(AUTHOR_NAME, name)
    }

    if (author.orcid.isDefined) {
      addIdentifier(m, "ORCID", author.orcid.get)
    }
    else if (author.isni.isDefined) {
      addIdentifier(m, "ISNI", author.isni.get)
    }
         else if (author.dai.isDefined) {
           addIdentifier(m, "DAI", author.dai.get)
         }

    if (author.organization.isDefined) {
      m.addPrimitiveField(AUTHOR_AFFILIATION, author.organization.get)
    }
    m.toJsonObject
  }

  def toContributorValueObject(node: Node): JsonObject = {
    val m = FieldMap()
    val author = parseAuthor(node)
    val name = formatName(author)
    if (StringUtils.isNotBlank(name)) {
      m.addPrimitiveField(CONTRIBUTOR_NAME, name + (if (author.organization.isDefined) s" (${ author.organization.get })"
                                                    else ""))
    }
    else if (author.organization.isDefined) {
      m.addPrimitiveField(CONTRIBUTOR_NAME, author.organization.get)
    }
    if (author.role.isDefined) {
      m.addCvField(CONTRIBUTOR_TYPE, author.role.map(contributoreRoleToContributorType.getOrElse(_, "Other")).getOrElse("Other"))
    }
    m.toJsonObject
  }

  def toRightsHolder(node: Node): Option[String] = {
    val author = parseAuthor(node)
    Option(formatRightsHolder(author))
  }

  def isRightsHolder(node: Node): Boolean = {
    val author = parseAuthor(node)
    author.role.contains("RightsHolder")
  }

  private def formatName(author: Author): String = {
    List(author.initials.getOrElse(""),
      author.insertions.getOrElse(""),
      author.surname.getOrElse(""))
      .mkString(" ").trim().replaceAll("\\s+", " ")
  }

  private def formatRightsHolder(rightsHolder: Author): String = {
    if (rightsHolder.surname.isEmpty)
      rightsHolder.organization.getOrElse("")
    else
      List(rightsHolder.titles.getOrElse(""),
        rightsHolder.initials.getOrElse(""),
        rightsHolder.insertions.getOrElse(""),
        rightsHolder.surname.getOrElse(""),
        rightsHolder.organization.map(o => s"($o)").getOrElse(""))
        .mkString(" ").trim().replaceAll("\\s+", " ")
  }

  private def addIdentifier(m: FieldMap, scheme: String, value: String): Unit = {
    if (StringUtils.isNotBlank(value)) {
      m.addCvField(AUTHOR_IDENTIFIER_SCHEME, scheme)
      m.addPrimitiveField(AUTHOR_IDENTIFIER, value)
    }
  }
}
