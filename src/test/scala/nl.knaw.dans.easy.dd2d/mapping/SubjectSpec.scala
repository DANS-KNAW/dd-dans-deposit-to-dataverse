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
import org.json4s.{ DefaultFormats, Formats }

class SubjectSpec extends TestSupportFixture with BlockCitation with UnsupportedSubjectSchemes {
  private implicit val jsonFormats: Formats = DefaultFormats

  "hasNoCvAttributes" should "return false for ABR subjects" in {
    val node = <ddm:subject  xml:lang="nl" valueURI="https://data.cultureelerfgoed.nl/term/id/abr/2179d872-888f-4807-a6d5-5e5afaa616c4" subjectScheme="ABR Complextypen" schemeURI="https://data.cultureelerfgoed.nl/term/id/abr/e9546020-4b28-4819-b0c2-29e7c864c5c0">
                bewoning (inclusief verdediging)
               </ddm:subject>
    Subject hasNoCvAttributes node shouldBe false
  }

  it should "return true for a subject with no subjectScheme and schemeURI attributes" in {
    val node = <ddm:subject>Test</ddm:subject>
    Subject hasNoCvAttributes node shouldBe true
  }

  "isPanTerm" should "return true if both schemeURI and subjectScheme match" in {
    val xml =
      <ddm:subject schemeURI={ SCHEME_URI_PAN }
        subjectScheme={ SCHEME_PAN }
        valueURI="https://data.cultureelerfgoed.nl/term/id/pan/08-01-01" xml:lang="en">
        button with solid head and loop-shaped shank</ddm:subject>
    Subject.isPanTerm(xml) shouldBe true
  }

  it should "return false if schemeURI does not match" in {
    val xml =
      <ddm:subject schemeURI="https://data.cultureelerfgoed.nl/term/id/pan/NOT-PAN"
        subjectScheme={ SCHEME_PAN }
        valueURI="https://data.cultureelerfgoed.nl/term/id/pan/08-01-01" xml:lang="en">
        button with solid head and loop-shaped shank</ddm:subject>
    Subject.isPanTerm(xml) shouldBe false
  }

  it should "return false if subjectScheme does not match" in {
    val xml =
      <ddm:subject schemeURI={ SCHEME_URI_PAN }
        subjectScheme="NOT PAN"
        valueURI="https://data.cultureelerfgoed.nl/term/id/pan/08-01-01" xml:lang="en">
        button with solid head and loop-shaped shank</ddm:subject>
    Subject.isPanTerm(xml) shouldBe false
  }

  "isAatTerm" should "return true if both schemeURI and subjectScheme match" in {
    val xml =
      <ddm:subject schemeURI={ SCHEME_URI_AAT }
        subjectScheme={ SCHEME_AAT }
        valueURI="http://vocab.getty.edu/aat/300239261" xml:lang="en">
        Broader Match: buttons (fasteners)</ddm:subject>
    Subject.isAatTerm(xml) shouldBe true
  }

  it should "return false if schemeURI does not match" in {
    val xml =
      <ddm:subject schemeURI="http://vocab.getty.edu/NOT-AAT/"
        subjectScheme={ SCHEME_AAT }
        valueURI="http://vocab.getty.edu/aat/300239261" xml:lang="en">
        Broader Match: buttons (fasteners)</ddm:subject>
    Subject.isAatTerm(xml) shouldBe false
  }

  it should "return false if subjectScheme does not match" in {
    val xml =
      <ddm:subject schemeURI={ SCHEME_URI_AAT }
        subjectScheme="NOT AAT"
        valueURI="http://vocab.getty.edu/aat/300239261" xml:lang="en">
        Broader Match: buttons (fasteners)</ddm:subject>
    Subject.isAatTerm(xml) shouldBe false
  }

  "removeMatchPrefix" should "remove Broader Match: before a term" in {
    val xml =
      <ddm:subject schemeURI={ SCHEME_URI_AAT }
        subjectScheme={ SCHEME_AAT }
        valueURI="http://vocab.getty.edu/aat/300239261" xml:lang="en">Broader Match: buttons (fasteners)</ddm:subject>
    Subject.removeMatchPrefix(xml) shouldBe "buttons (fasteners)"
  }

  it should "remove Close Match: before a term" in {
    val xml =
      <ddm:subject schemeURI={ SCHEME_URI_AAT }
        subjectScheme={ SCHEME_AAT }
        valueURI="http://vocab.getty.edu/aat/300239261" xml:lang="en">Close Match: buttons (fasteners)</ddm:subject>
    Subject.removeMatchPrefix(xml) shouldBe "buttons (fasteners)"
  }

  it should "term unchanged if it has no prefix" in {
    val xml =
      <ddm:subject schemeURI={ SCHEME_URI_AAT }
        subjectScheme={ SCHEME_AAT }
        valueURI="http://vocab.getty.edu/aat/300239261" xml:lang="en">buttons (fasteners)</ddm:subject>
    Subject.removeMatchPrefix(xml) shouldBe "buttons (fasteners)"
  }



}
