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
import nl.knaw.dans.easy.dd2d.dansbag.DansBagValidator
import nl.knaw.dans.easy.dd2d.migrationinfo.MigrationInfo
import nl.knaw.dans.lib.dataverse.DataverseInstance

import java.net.URI
import java.util.regex.Pattern
import scala.xml.Elem

/**
 * Factory for creating ingest tasks.
 *
 * @param isMigrated                                   is this a migrated dataset?
 * @param prestagedFiles                               are prestaged files included?
 * @param activeMetadataBlocks                         the metadata blocks enabled in the target dataverse
 * @param optDansBagValidator                          interface to the easy-validate-dans-bag service
 * @param instance                                     interface to the target Dataverse instance
 * @param migrationInfo                                optional interface to a migration info service
 * @param publishAwaitUnlockMaxNumberOfRetries         maximum number of times to poll for unlock after publish is called after ingest of the deposit
 * @param publishAwaitUnlockMillisecondsBetweenRetries number of milliseconds to wait between retries of unlock polling after publish
 * @param narcisClassification                         root element of the NARCIS SKOS file
 * @param iso2ToDataverseLanguage                      mapping of ISO639-2 to Dataverse language term
 * @param reportIdToTerm                               mapping of ABR report ID to term
 * @param outboxDir                                    outbox
 */
class DepositIngestTaskFactory(isMigrated: Boolean = false,
                               prestagedFiles: Boolean,
                               optFileExclusionPattern: Option[Pattern],
                               depositorRole: String,
                               deduplicateService: Boolean,
                               deduplicateImport: Boolean,
                               activeMetadataBlocks: List[String],
                               optDansBagValidator: Option[DansBagValidator],
                               instance: DataverseInstance,
                               migrationInfo: Option[MigrationInfo],
                               publishAwaitUnlockMaxNumberOfRetries: Int,
                               publishAwaitUnlockMillisecondsBetweenRetries: Int,
                               narcisClassification: Elem,
                               iso1ToDataverseLanguage: Map[String, String],
                               iso2ToDataverseLanguage: Map[String, String],
                               variantToLicense: Map[String, String],
                               supportedLicenses: List[URI],

                               reportIdToTerm: Map[String, String],
                               outboxDir: File) {

  def createDepositIngestTask(deposit: Deposit): DepositIngestTask = {
    if (isMigrated)
      new DepositMigrationTask(deposit,
        prestagedFiles,
        optFileExclusionPattern,
        depositorRole,
        deduplicateImport,
        activeMetadataBlocks,
        optDansBagValidator,
        instance,
        migrationInfo,
        publishAwaitUnlockMaxNumberOfRetries,
        publishAwaitUnlockMillisecondsBetweenRetries,
        narcisClassification,
        iso1ToDataverseLanguage,
        iso2ToDataverseLanguage,
        variantToLicense,
        supportedLicenses,
        reportIdToTerm,
        outboxDir: File)
    else
      DepositIngestTask(
        deposit,
        prestagedFiles,
        optFileExclusionPattern,
        depositorRole,
        deduplicateService,
        activeMetadataBlocks,
        optDansBagValidator,
        instance,
        Option.empty,
        publishAwaitUnlockMaxNumberOfRetries,
        publishAwaitUnlockMillisecondsBetweenRetries,
        narcisClassification,
        iso1ToDataverseLanguage,
        iso2ToDataverseLanguage,
        variantToLicense,
        supportedLicenses,
        reportIdToTerm,
        outboxDir: File)
  }
}
