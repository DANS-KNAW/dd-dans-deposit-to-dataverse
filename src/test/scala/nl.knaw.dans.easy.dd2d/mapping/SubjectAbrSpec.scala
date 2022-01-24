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

class SubjectAbrSpec extends TestSupportFixture with BlockArchaeologySpecific with AbrScheme {

  "isOldAbr" should "return true if schemeURI of old ABR is used and subjectScheme matches name" in {
    val xml =  <ddm:subject
      schemeURI={ SCHEME_URI_ABR_OLD}
      subjectScheme={ SCHEME_ABR_OLD }
      valueURI="https://data.cultureelerfgoed.nl/term/id/rn/ea77d56e-1475-4e4c-94f5-489bd3d9a3e7" xml:lang="nl">Broader Match: knoop</ddm:subject>

    SubjectAbr.isOldAbr(xml) shouldBe true
  }

  it should "return false if schemeURI does not match" in {
    val xml =  <ddm:subject
      schemeURI="https://data.cultureelerfgoed.nl/term/id/rn/NO-MATCH"
      subjectScheme={ SCHEME_ABR_OLD }
      valueURI="https://data.cultureelerfgoed.nl/term/id/rn/ea77d56e-1475-4e4c-94f5-489bd3d9a3e7" xml:lang="nl">Broader Match: knoop</ddm:subject>

    SubjectAbr.isOldAbr(xml) shouldBe false
  }

  it should "return false if subjectScheme does not match" in {
    val xml =  <ddm:subject
      schemeURI={ SCHEME_URI_ABR_OLD}
      subjectScheme="NO MATCH"
      valueURI="https://data.cultureelerfgoed.nl/term/id/rn/ea77d56e-1475-4e4c-94f5-489bd3d9a3e7" xml:lang="nl">Broader Match: knoop</ddm:subject>

    SubjectAbr.isOldAbr(xml) shouldBe false
  }

  "fromAbrOldToAbrArtifact" should "create artifact termURI from legacy URI by using the UUID from the legacy URI" in {
    val abrBaseUrl = "https://data.cultureelerfgoed.nl/term/id/abr"
    val termUuid = "ea77d56e-1475-4e4c-94f5-489bd3d9a3e7"
    val valueUri = s"https://data.cultureelerfgoed.nl/term/id/rn/$termUuid"
    val xml =  <ddm:subject
      schemeURI={ SCHEME_URI_ABR_OLD }
      subjectScheme={ SCHEME_ABR_OLD }
      valueURI={ valueUri } xml:lang="nl">Broader Match: knoop</ddm:subject>

    SubjectAbr.fromAbrOldToAbrArtifact(xml) shouldBe Some(s"$abrBaseUrl/$termUuid")
  }

  it should "return None for an element that is not an old ABR element" in {
    val abrBaseUrl = "https://data.cultureelerfgoed.nl/term/id/abr"
    val termUuid = "ea77d56e-1475-4e4c-94f5-489bd3d9a3e7"
    val valueUri = s"https://data.cultureelerfgoed.nl/term/id/rn/$termUuid"
    val xml =  <ddm:subject
      schemeURI="https://data.cultureelerfgoed.nl/term/id/rn/NOT-OLD-ABR"
      subjectScheme={ SCHEME_ABR_OLD }
      valueURI={ valueUri } xml:lang="nl">Broader Match: knoop</ddm:subject>

    SubjectAbr.fromAbrOldToAbrArtifact(xml) shouldBe None
  }

  "isAbrArtifact" should "return true for subject element matching schemeURI and subjectScheme attributes" in {
    val xml = <ddm:subject
      schemeURI={ SCHEME_URI_ABR_ARTIFACT }
      subjectScheme={ SCHEME_ABR_ARTIFACT }
      valueURI="https://data.cultureelerfgoed.nl/term/id/abr/6f6b36ae-f1cb-4fe6-82f0-20d2bdb75506" xml:lang="nl">speelgoed</ddm:subject>
    SubjectAbr.isAbrArtifact(xml) shouldBe true
  }
}
