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

class DepositPropertiesVaultMetadataSpec extends TestSupportFixture with BlockCitation {
  private implicit val jsonFormats: Formats = DefaultFormats

  "toOtherIdValue" should "create OtherId Json object for other ID if specified in correct format" in {
    val result = Serialization.writePretty(DepositPropertiesVaultMetadata.toOtherIdValue("PAN:123"))
    getPathAsString(result, "$.otherIdAgency.value") shouldBe "PAN"
    getPathAsString(result, "$.otherIdValue.value") shouldBe "123"
  }

  it should "create a None if input is blank" in {
    DepositPropertiesVaultMetadata.toOtherIdValue("") shouldBe Option.empty
  }

  it should "create a None if input is blank with spaces" in {
    DepositPropertiesVaultMetadata.toOtherIdValue("  ") shouldBe Option.empty
  }

  it should "create a None if input is null" in {
    DepositPropertiesVaultMetadata.toOtherIdValue(null) shouldBe Option.empty
  }

  it should "throw an IllegalArgumentException if the value does not have prefix:suffix format (slash instead of colon)" in {
    an[IllegalArgumentException] should be thrownBy {
      DepositPropertiesVaultMetadata.toOtherIdValue("in/valid")
    }
  }

  it should "throw an IllegalArgumentException if the value does not have prefix:suffix format (two instead of one colon)" in {
    an[IllegalArgumentException] should be thrownBy {
      DepositPropertiesVaultMetadata.toOtherIdValue("in:va:lid")
    }
  }

  it should "throw an IllegalArgumentException if the value does not have prefix:suffix format (no colon)" in {
    an[IllegalArgumentException] should be thrownBy {
      DepositPropertiesVaultMetadata.toOtherIdValue("123")
    }
  }

  it should "throw an IllegalArgumentException if the value does not have prefix:suffix (internal spaces)" in {
    an[IllegalArgumentException] should be thrownBy {
      DepositPropertiesVaultMetadata.toOtherIdValue("12:1 2 3")
    }
  }
}
