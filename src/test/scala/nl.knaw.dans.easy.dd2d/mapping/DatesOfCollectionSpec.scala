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

class DatesOfCollectionSpec extends TestSupportFixture with BlockCitation {
  private implicit val jsonFormats: Formats = DefaultFormats

  "toDateOfCollectionValue" should "split correctly formatted date range in start and end subfields" in {
    val datesOfCollection = <ddm:datesOfCollection>2022-01-01/2022-02-01</ddm:datesOfCollection>
    val result = Serialization.writePretty(DatesOfCollection.toDateOfCollectionValue(datesOfCollection))
    findString(result, s"$DATE_OF_COLLECTION_START.value") shouldBe "2022-01-01"
    findString(result, s"$DATE_OF_COLLECTION_END.value") shouldBe "2022-02-01"
  }

  it should "handle ranges without start" in {
    val datesOfCollection = <ddm:datesOfCollection>/2022-02-01</ddm:datesOfCollection>
    val result = Serialization.writePretty(DatesOfCollection.toDateOfCollectionValue(datesOfCollection))
    findString(result, s"$DATE_OF_COLLECTION_START.value") shouldBe ""
    findString(result, s"$DATE_OF_COLLECTION_END.value") shouldBe "2022-02-01"
  }

  it should "handle ranges without end" in {
    val datesOfCollection = <ddm:datesOfCollection>2022-01-01/</ddm:datesOfCollection>
    val result = Serialization.writePretty(DatesOfCollection.toDateOfCollectionValue(datesOfCollection))
    findString(result, s"$DATE_OF_COLLECTION_START.value") shouldBe "2022-01-01"
    findString(result, s"$DATE_OF_COLLECTION_END.value") shouldBe ""
  }

  it should "handle whitespace at beginnen and end of text" in {
    val datesOfCollection = <ddm:datesOfCollection>
      2022-01-01/2022-02-01
    </ddm:datesOfCollection>
    val result = Serialization.writePretty(DatesOfCollection.toDateOfCollectionValue(datesOfCollection))
    findString(result, s"$DATE_OF_COLLECTION_START.value") shouldBe "2022-01-01"
    findString(result, s"$DATE_OF_COLLECTION_END.value") shouldBe "2022-02-01"
  }

}
