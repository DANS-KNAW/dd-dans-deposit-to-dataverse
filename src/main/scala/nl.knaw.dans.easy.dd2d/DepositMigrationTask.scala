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
import nl.knaw.dans.easy.dd2d.mapping.Amd
import nl.knaw.dans.easy.dd2d.migrationinfo.MigrationInfo
import nl.knaw.dans.lib.dataverse.DataverseInstance
import nl.knaw.dans.lib.dataverse.model.dataset.Dataset

import java.net.URI
import java.util.regex.Pattern
import scala.language.postfixOps
import scala.util.{ Failure, Success, Try }
import scala.xml.{ Elem, Node }

class DepositMigrationTask(deposit: Deposit,
                           optFileExclusionPattern: Option[Pattern],
                           depositorRole: String,
                           deduplicate: Boolean,
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
                           repordIdToTerm: Map[String, String],
                           outboxDir: File)
  extends DepositIngestTask(deposit,
    optFileExclusionPattern,
    depositorRole,
    deduplicate,
    activeMetadataBlocks,
    optDansBagValidator,
    instance,
    migrationInfo: Option[MigrationInfo],
    publishAwaitUnlockMaxNumberOfRetries,
    publishAwaitUnlockMillisecondsBetweenRetries,
    narcisClassification,
    iso1ToDataverseLanguage,
    iso2ToDataverseLanguage,
    variantToLicense,
    supportedLicenses,
    repordIdToTerm,
    outboxDir) {

  override protected def checkDepositType(): Try[Unit] = {
    for {
      _ <- if (deposit.doi.isEmpty) Failure(new IllegalArgumentException("Deposit for migrated dataset MUST have deposit property identifier.doi set"))
           else Success(())
      _ <- deposit.vaultMetadata.checkMinimumFieldsForImport()
    } yield ()
  }

  override def newDatasetUpdater(dataverseDataset: Dataset): DatasetUpdater = {
    new DatasetUpdater(deposit, optFileExclusionPattern, isMigration = true, dataverseDataset.datasetVersion.metadataBlocks, variantToLicense, supportedLicenses, instance, migrationInfo)
  }

  override def newDatasetCreator(dataverseDataset: Dataset, depositorRole: String): DatasetCreator = {
    new DatasetCreator(deposit, optFileExclusionPattern, depositorRole, isMigration = true, dataverseDataset, variantToLicense, supportedLicenses, instance, migrationInfo)
  }

  override protected def getDateOfDeposit: Try[Option[String]] = {
    for {
      optAmd <- deposit.tryOptAmd
      optDate = optAmd.flatMap(Amd toDateOfDeposit(deposit.getDepositOrigin))
    } yield optDate
  }

  override protected def publishDataset(persistentId: String): Try[Unit] = {
    trace(persistentId)
    for {
      optAmd <- deposit.tryOptAmd
      amd = optAmd.getOrElse(throw new Exception(s"no AMD found for $persistentId"))
      optPublicationDate <- getJsonLdPublicationdate(amd, deposit.getDepositOrigin)
      publicationDate = optPublicationDate.getOrElse(throw new IllegalArgumentException(s"no publication date found in AMD for $persistentId"))
      _ <- instance.dataset(persistentId).releaseMigrated(publicationDate)
      _ <- instance.dataset(persistentId).awaitUnlock(
        maxNumberOfRetries = publishAwaitUnlockMaxNumberOfRetries,
        waitTimeInMilliseconds = publishAwaitUnlockMillisecondsBetweenRetries)
    } yield ()
  }

  private def getJsonLdPublicationdate(amd: Node, depositOrigin: String): Try[Option[String]] = Try {
    Amd.getFirstChangeToState(amd, "PUBLISHED", depositOrigin)
      .map(d => s"""{"http://schema.org/datePublished": "$d"}""")
  }

  override protected def postPublication(persistentId: String): Try[Unit] = {
    trace(persistentId)
    Success(())
  }
}
