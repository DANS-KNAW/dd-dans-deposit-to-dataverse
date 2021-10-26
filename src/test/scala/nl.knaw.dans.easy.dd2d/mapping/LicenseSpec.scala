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

import better.files.File
import nl.knaw.dans.easy.dd2d.{ TestSupportFixture, loadCsvToMap, loadTxtToList }

import java.net.URI
import java.nio.file.Paths

class LicenseSpec extends TestSupportFixture {
  private val variantToLicense = loadCsvToMap(File(Paths.get("src/main/assembly/dist/install/license-uri-variants.csv")),
    keyColumn = "Variant",
    valueColumn = "Normalized").get
  private val supportedLicenses = loadTxtToList(File(Paths.get("src/main/assembly/dist/install/supported-licenses.txt"))).get.map(s => new URI(s))

  "isLicense" should "return true if license element is found and has proper attribute" in {
    val lic = <dct:license xmlns:dct="http://purl.org/dc/terms/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="dcterms:URI">http://creativecommons.org/licenses/by-sa/4.0/</dct:license>
    License.isLicenseUri(lic) shouldBe true
  }

  it should "return false if attribute is not present" in {
    val lic = <dct:license xmlns:dct="http://purl.org/dc/terms/">http://creativecommons.org/licenses/by-sa/4.0/</dct:license>
    License.isLicenseUri(lic) shouldBe false
  }

  it should "return false if attribute has non-URI value" in {
    val lic = <dct:license xmlns:dct="http://purl.org/dc/terms/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="dcterms:URI">not a uri</dct:license>
    License.isLicenseUri(lic) shouldBe false
  }

  it should "return false if element is not dcterms:license" in {
    val lic = <dct:rights xmlns:dct="http://purl.org/dc/terms/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="dcterms:URI">http://creativecommons.org/licenses/by-sa/4.0/</dct:rights>
    License.isLicenseUri(lic) shouldBe false
  }

  "getLicense" should "return URI with license value for license element without trailing slash" in {
    val s = "http://creativecommons.org/licenses/by-sa/4.0"
    val trailingSlash = "/"
    val lic = <dct:license xmlns:dct="http://purl.org/dc/terms/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="dcterms:URI">{s}{trailingSlash}</dct:license>
    License.getLicenseUri(supportedLicenses)(variantToLicense)(lic) shouldBe new URI(s)
  }

  it should "throw an IllegalArgumentException if isLicense returns false" in {
    val lic = <dct:rights xmlns:dct="http://purl.org/dc/terms/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="dcterms:URI">http://creativecommons.org/licenses/by-sa/4.0/</dct:rights>
    an [IllegalArgumentException] shouldBe thrownBy(License.getLicenseUri(supportedLicenses)(variantToLicense)(lic))
  }
}
