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
import nl.knaw.dans.lib.dataverse.model.file.FileMeta

class FileElementSpec extends TestSupportFixture {
  "toFileMetadata" should "strip data/ prefix from path to get directoryLabel" in {
    val xml =
      <file filepath="data/this/is/the/directoryLabel/filename.txt">
      </file>
    val result = FileElement.toFileMeta(xml, defaultRestrict = true)
    result shouldBe FileMeta(
      directoryLabel = Option("this/is/the/directoryLabel"),
      label = Option("filename.txt"),
      restrict = Option(true)
    )
  }

  it should "represent key-value pairs in the description, for keys on the fixed keys list" in {
    val xml =
      <file filepath="data/test.txt">
        <othmat_codebook>the code book</othmat_codebook>
      </file>
    val result = FileElement.toFileMeta(xml, defaultRestrict = true)
    result shouldBe FileMeta(
      directoryLabel = None,
      label = Option("test.txt"),
      description = Option("""othmat_codebook: "the code book""""),
      restrict = Option(true))
  }

  // TODO: write more unit tests
}
