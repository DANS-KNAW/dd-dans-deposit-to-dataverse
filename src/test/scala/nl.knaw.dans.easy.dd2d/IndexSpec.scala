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
package nl.knaw.dans.easy.dd2d

import better.files.File

import java.io.ByteArrayOutputStream

class IndexSpec extends TestSupportFixture {
  private val indexFile = File("docs/index.md")

  private val configuration = Configuration(
    version = "my-version",
    inboxDir = null,
    outboxDir = null,
    optFileExclusionPattern = null,
    zipFileHandler = null,
    depositorRole = null,
    validatorServiceUrl = null,
    validatorConnectionTimeoutMs = 1000,
    validatorReadTimeoutMs = 1000,
    dataverse = null,
    migrationInfo = null,
    publishAwaitUnlockMaxNumberOfRetries = 3,
    publishAwaitUnlockMillisecondsBetweenRetries = 500,
    narcisClassification = null,
    iso1ToDataverseLanguage = null,
    iso2ToDataverseLanguage = null,
    reportIdToTerm = null,
    variantToLicense = null,
    supportedLicenses = null
  )
  private val clo = new CommandLineOptions(Array[String](), configuration) {
    // avoids System.exit() in case of invalid arguments or "--help"
    override def verify(): Unit = {}
  }

  private val helpInfo = {
    val mockedStdOut = new ByteArrayOutputStream()
    Console.withOut(mockedStdOut) {
      clo.printHelp()
    }
    mockedStdOut.toString
  }

  "options in help info" should "be part of index.md" in {
    val lineSeparators = s"(${ System.lineSeparator() })+"
    val options = helpInfo.split(s"${ lineSeparators }Options:$lineSeparators")(1)
    options.trim should not be empty
    indexFile should containTrimmed(options)
  }

  "synopsis in help info" should "be part of index.md" in {
    indexFile should containTrimmed(clo.synopsis)
  }

  "description line(s) in help info" should "be part of README.md and pom.xml" in {
    indexFile should containTrimmed(clo.description)
    File("pom.xml") should containTrimmed(clo.description)
  }
}