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

import nl.knaw.dans.easy.dd2d.TestSupportFixture
import org.json4s.native.Serialization
import org.json4s.{ DefaultFormats, Formats }

class IdentifierSpec extends TestSupportFixture with BlockCitation {
  private implicit val jsonFormats: Formats = DefaultFormats

  "toOtherIdValue" should "create OtherId Json object without agency for identifier without type attribute" in {
    val result = Serialization.writePretty(Identifier toOtherIdValue (<identifier xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">123</identifier>))
    getPathAsString(result, "$.otherIdAgency.value") shouldBe ""
    getPathAsString(result, "$.otherIdValue.value") shouldBe "123"
  }

  it should "create OtherId Json object DANS-KNAW for identifier with EASY2 type attribute" in {
    val result = Serialization.writePretty(Identifier toOtherIdValue (<identifier xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="id-type:EASY2">easy-dataset:1234</identifier>))
    getPathAsString(result, "$.otherIdAgency.value") shouldBe "DANS-KNAW"
    getPathAsString(result, "$.otherIdValue.value") shouldBe "easy-dataset:1234"
  }

  "canBeMappedToOtherId" should "return true if EASY2 is xsi:type" in {
    Identifier canBeMappedToOtherId <identifier xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="id-type:EASY2">easy-dataset:1234</identifier> shouldBe true
  }

  it should "return true if no xsi:type is present" in {
    Identifier canBeMappedToOtherId <identifier xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">1234</identifier> shouldBe true
  }

  it should "return false for identifiers with other xsi:types" in {
    Identifier canBeMappedToOtherId <identifier xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="id-type:DOI">10.5072/test</identifier> shouldBe false
  }

  "isRelatedPublication" should "return true of xsi:type is id-type ISBN" in {
    Identifier isRelatedPublication <identifier xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="id-type:ISBN">12345</identifier> shouldBe true
  }

  it should "return true if id-type prefix is absent for ISBN" in {
    Identifier isRelatedPublication <identifier xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="ISBN">12345</identifier> shouldBe true
  }

  it should "return true of xsi:type is id-type ISSN" in {
    Identifier isRelatedPublication <identifier xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="id-type:ISSN">12345</identifier> shouldBe true
  }

  it should "return true if id-type prefix is absent for ISSN" in {
    Identifier isRelatedPublication <identifier xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="ISSN">12345</identifier> shouldBe true
  }

  it should "return false if id-type is DOI" in {
    Identifier isRelatedPublication <identifier xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="id-type:DOI">10.1234/12345</identifier> shouldBe false
  }

  it should "return if there is not xsi:type" in {
    Identifier isRelatedPublication <identifier xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">whatever</identifier> shouldBe false
  }

  "toRelatedPublicationValue" should "map xsi:type to ID type and node text to ID number" in {
    val result = Serialization.writePretty(Identifier toRelatedPublicationValue <identifier xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="ISSN">12345</identifier>)
    getPathAsString(result, "$.publicationCitation.value") shouldBe ""
    getPathAsString(result, "$.publicationIDType.value") shouldBe "issn"
    getPathAsString(result, "$.publicationIDNumber.value") shouldBe "12345"
    getPathAsString(result, "$.publicationURL.value") shouldBe ""
  }

  "isNwoGrantNumber" should "return true if xsi:type is NWO-PROJECTNR" in {
    Identifier isNwoGrantNumber <identifier xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="id-type:NWO-PROJECTNR">12345</identifier> shouldBe true
  }

  "toNwoGrantNumber" should "fill in subfields correctly" in {
    val result = Serialization.writePretty(Identifier toNwoGrantNumberValue <identifier xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="id-type:NWO-PROJECTNR">12345</identifier>)
    getPathAsString(result, "$.grantNumberAgency.value") shouldBe "NWO"
    getPathAsString(result, "$.grantNumberValue.value") shouldBe "12345"
  }
}
