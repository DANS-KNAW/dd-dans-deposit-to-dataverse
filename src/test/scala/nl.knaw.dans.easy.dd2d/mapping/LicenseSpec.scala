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

import java.net.URI

class LicenseSpec extends TestSupportFixture {

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

  "getLicense" should "return URI with license value for license element " in {
    val s = "http://creativecommons.org/licenses/by-sa/4.0/"
    val lic = <dct:license xmlns:dct="http://purl.org/dc/terms/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="dcterms:URI">{s}</dct:license>
    License.getLicenseUri(lic) shouldBe new URI(s)
  }

  it should "throw an IllegalArgumentException if isLicense returns false" in {
    val lic = <dct:rights xmlns:dct="http://purl.org/dc/terms/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="dcterms:URI">http://creativecommons.org/licenses/by-sa/4.0/</dct:rights>
    an [IllegalArgumentException] shouldBe thrownBy(License.getLicenseUri(lic))
  }
}
