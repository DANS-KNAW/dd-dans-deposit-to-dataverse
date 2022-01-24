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

class TemporalAbrSpec extends TestSupportFixture with BlockArchaeologySpecific with AbrScheme {

  "isAbrPeriod" should "return true if schemeURI, subjectScheme for ABR Periods is used" in {
    val xml =  <ddm:temporal
      schemeURI={ SCHEME_URI_ABR_PERIOD }
      subjectScheme={ SCHEME_ABR_PERIOD }
      valueURI="https://data.cultureelerfgoed.nl/term/id/abr/c6858173-5ca2-4319-b242-f828ec53d52d" xml:lang="nl">Nieuwe Tijd</ddm:temporal>

    TemporalAbr.isAbrPeriod(xml) shouldBe true
  }

  it should "return false if schemeURI does not match" in {
    val xml =  <ddm:temporal
      schemeURI="https://data.cultureelerfgoed.nl/term/id/rn/NO-MATCH"
      subjectScheme={ SCHEME_ABR_PERIOD }
      valueURI="https://data.cultureelerfgoed.nl/term/id/abr/c6858173-5ca2-4319-b242-f828ec53d52d" xml:lang="nl">Nieuwe Tijd</ddm:temporal>

    TemporalAbr.isAbrPeriod(xml) shouldBe false
  }

  it should "return false if subjectScheme does not match" in {
    val xml =  <ddm:temporal
      schemeURI={ SCHEME_URI_ABR_PERIOD }
      subjectScheme="NO MATCH"
      valueURI="https://data.cultureelerfgoed.nl/term/id/abr/c6858173-5ca2-4319-b242-f828ec53d52d" xml:lang="nl">Nieuwe Tijd</ddm:temporal>

    TemporalAbr.isAbrPeriod(xml) shouldBe false
  }

  it should "return true if schemeURI, subjectScheme for ABR+ is used" in {
    val xml =  <ddm:temporal
      schemeURI={ SCHEME_URI_ABR_PLUS }
      subjectScheme={ SCHEME_ABR_PLUS }
      valueURI="https://data.cultureelerfgoed.nl/term/id/abr/c6858173-5ca2-4319-b242-f828ec53d52d" xml:lang="nl">Nieuwe Tijd</ddm:temporal>

    TemporalAbr.isAbrPeriod(xml) shouldBe true
  }

  it should "return false if schemeURI of ABR+ does not match" in {
    val xml =  <ddm:temporal
      schemeURI="https://data.cultureelerfgoed.nl/term/id/rn/NO-MATCH"
      subjectScheme={ SCHEME_ABR_PLUS }
      valueURI="https://data.cultureelerfgoed.nl/term/id/abr/c6858173-5ca2-4319-b242-f828ec53d52d" xml:lang="nl">Nieuwe Tijd</ddm:temporal>

    TemporalAbr.isAbrPeriod(xml) shouldBe false
  }

  it should "return false if subjectScheme of ABR+ does not match" in {
    val xml =  <ddm:temporal
      schemeURI={ SCHEME_URI_ABR_PLUS }
      subjectScheme="NO MATCH"
      valueURI="https://data.cultureelerfgoed.nl/term/id/abr/c6858173-5ca2-4319-b242-f828ec53d52d" xml:lang="nl">Nieuwe Tijd</ddm:temporal>

    TemporalAbr.isAbrPeriod(xml) shouldBe false
  }

}
