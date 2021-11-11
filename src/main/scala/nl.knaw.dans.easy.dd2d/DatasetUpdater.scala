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

import nl.knaw.dans.easy.dd2d.migrationinfo.{ BasicFileMeta, MigrationInfo }
import nl.knaw.dans.lib.dataverse.model.dataset.MetadataBlocks
import nl.knaw.dans.lib.dataverse.model.file.FileMeta
import nl.knaw.dans.lib.dataverse.model.search.DatasetResultItem
import nl.knaw.dans.lib.dataverse.{ DatasetApi, DataverseInstance, FileApi, Version }
import nl.knaw.dans.lib.error.{ TraversableTryExtensions, TryExtensions }
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import java.net.URI
import java.nio.file.{ Path, Paths }
import java.util.regex.Pattern
import scala.util.control.NonFatal
import scala.util.{ Failure, Success, Try }

class DatasetUpdater(deposit: Deposit,
                     optFileExclusionPattern: Option[Pattern],
                     isMigration: Boolean = false,
                     metadataBlocks: MetadataBlocks,
                     variantToLicense: Map[String, String],
                     supportedLicenses: List[URI],
                     instance: DataverseInstance,
                     optMigrationInfoService: Option[MigrationInfo]) extends DatasetEditor(instance, optFileExclusionPattern) with DebugEnhancedLogging {
  trace(deposit)

  override def performEdit(): Try[PersistendId] = {
    {
      for {
        doi <- if (isMigration) Try { deposit.dataversePid }
               else getDoiBySwordToken
      } yield doi
    } match {
      case Failure(e) => Failure(FailedDepositException(deposit, "Could not find persistentId of existing dataset", e))
      case Success(doi) => {
        for {
          dataset <- Try { instance.dataset(doi) }
          _ <- dataset.awaitUnlock()
          /*
           * Temporary fix. If we do not wait a couple of seconds here, the first version never gets properly published, and the second version
           * just overwrites it, becoming V1.
           */
          - <- Try { Thread.sleep(3000) }
          _ <- dataset.awaitUnlock()
          _ <- checkDatasetInPublishedState(dataset)
          _ <- dataset.updateMetadata(metadataBlocks)
          _ <- dataset.awaitUnlock()

          _ <- setLicense(supportedLicenses)(variantToLicense)(deposit, dataset)
          _ <- dataset.awaitUnlock()
          pathToFileInfo <- getPathToFileInfo(deposit)
          _ = debug(s"pathToFileInfo = $pathToFileInfo")
          pathToFileMetaInLatestVersion <- getFilesInLatestVersion(dataset)
          _ = debug(s"pathToFileMetaInLatestVersion = $pathToFileMetaInLatestVersion")
          _ <- validateFileMetas(pathToFileMetaInLatestVersion.values.toList)

          numPub <- getNumberOfPublishedVersions(dataset)
          _ = debug(s"Number of published versions so far: $numPub")
          prestagedFiles <- optMigrationInfoService.map(_.getPrestagedDataFilesFor(doi, numPub + 1)).getOrElse(Success(Set.empty[BasicFileMeta]))
          filesToReplace <- getFilesToReplace(pathToFileInfo, pathToFileMetaInLatestVersion)
          fileReplacements <- replaceFiles(dataset, filesToReplace, prestagedFiles)
          _ = debug(s"fileReplacements = $fileReplacements")

          oldToNewPathMovedFiles <- getOldToNewPathOfFilesToMove(pathToFileMetaInLatestVersion, pathToFileInfo)
          fileMovements = oldToNewPathMovedFiles.map { case (old, newPath) => (pathToFileMetaInLatestVersion(old).dataFile.get.id, pathToFileInfo(newPath).metadata) }
          // Movement will be realized by updating label and directoryLabel attributes of the file; there is no separate "move-file" API endpoint.
          _ = debug(s"fileMovements = $fileMovements")

          pathsToDelete = pathToFileMetaInLatestVersion.keySet diff pathToFileInfo.keySet diff oldToNewPathMovedFiles.keySet
          fileDeletions <- getFileDeletions(pathsToDelete, pathToFileMetaInLatestVersion)
          _ = debug(s"fileDeletions = $fileDeletions")
          _ <- deleteFiles(dataset, fileDeletions.toList)

          pathsToAdd = pathToFileInfo.keySet diff pathToFileMetaInLatestVersion.keySet diff oldToNewPathMovedFiles.values.toSet
          filesToAdd = pathsToAdd.map(pathToFileInfo).toList
          fileAdditions <- addFiles(doi, filesToAdd, prestagedFiles).map(_.mapValues(_.metadata))

          // TODO: what happens with file that only got a new description? Their MD will not be updated ??!!
          // TODO: probably just change this to: update the file md of all the files that are in the new version. Will DV show "null-replacements" in the differences view??
          _ <- updateFileMetadata(fileReplacements ++ fileMovements ++ fileAdditions)
          _ <- dataset.awaitUnlock()

          dateAvailable <- deposit.getDateAvailable
          _ <- if (isEmbargo(dateAvailable)) {
            val fileIdsToEmbargo = (fileReplacements ++ fileAdditions).filter(f => f._2.directoryLabel.getOrElse("") != "easy-migration").keys
            logger.info(s"Embargoing new files until $dateAvailable")
            embargoFiles(doi, dateAvailable, fileIdsToEmbargo.toList)
          }
               else {
                 logger.debug(s"Date available in the past, no embargo: $dateAvailable }")
                 Success(())
               }
          /*
           * Cannot enable requests if they were disallowed because of closed files in a previous version. However disabling is possible because a the update may add a closed file.
           */
          _ <- configureEnableAccessRequests(deposit, doi, canEnable = false)
        } yield doi
      }.doIfFailure {
        case e: CannotUpdateDraftDatasetException => // Don't delete the draft that caused the failure
        case NonFatal(e) =>
          logger.error("Dataset update failed, deleting draft", e)
          deleteDraftIfExists(doi)
      }
    }
  }

  private def checkDatasetInPublishedState(datasetApi: DatasetApi): Try[Unit] = {
    for {
      r <- datasetApi.viewLatestVersion()
      v <- r.data
      _ <- if (v.latestVersion.versionState.contains("DRAFT")) Failure(CannotUpdateDraftDatasetException(deposit))
           else Success(())
    } yield ()
  }

  private def getDoiBySwordToken: Try[String] = {
    trace(())
    debug(s"dansSwordToken = ${ deposit.vaultMetadata.dataverseSwordToken }")
    for {
      r <- instance.search().find(s"""dansSwordToken:"${ deposit.vaultMetadata.dataverseSwordToken }"""")
      searchResult <- r.data
      items = searchResult.items
      _ = if (items.size != 1) throw FailedDepositException(deposit, s"Deposit is update of ${ items.size } datasets; should always be 1!")
      doi = items.head.asInstanceOf[DatasetResultItem].globalId
      _ = debug(s"Deposit is update of dataset $doi")
    } yield doi
  }

  private def getFilesInLatestVersion(dataset: DatasetApi): Try[Map[Path, FileMeta]] = {
    for {
      response <- dataset.listFiles(Version.LATEST_PUBLISHED) // N.B. If LATEST_PUBLISHED is not specified, it almost works, but the directoryLabel is not picked up somehow.
      files <- response.data
      pathToFileMeta = files.map(f => (getPathFromFileMeta(f), f)).toMap
    } yield pathToFileMeta
  }

  private def getPathFromFileMeta(fileMeta: FileMeta): Path = {
    Paths.get(fileMeta.directoryLabel.getOrElse(""), fileMeta.label.getOrElse(""))
  }

  private def validateFileMetas(files: List[FileMeta]): Try[Unit] = {
    if (files.map(_.dataFile).exists(_.isEmpty)) Failure(new IllegalArgumentException("Found file metadata without dataFile element"))
    else if (files.map(_.dataFile.get).exists(_.checksum.`type` != "SHA-1")) Failure(new IllegalArgumentException("Not all file checksums are of type SHA-1"))
         else Success(())
  }

  private def getNumberOfPublishedVersions(datasetApi: DatasetApi): Try[Int] = {
    for {
      r <- datasetApi.viewAllVersions()
      vs <- r.data
    } yield vs.count(v => v.versionState.isDefined && v.versionState.get == "RELEASED")
  }

  private def getFilesToReplace(pathToFileInfo: Map[Path, FileInfo], pathToFileMetaInLatestVersion: Map[Path, FileMeta]): Try[Map[Int, FileInfo]] = Try {
    trace(())
    val intersection = pathToFileInfo.keySet intersect pathToFileMetaInLatestVersion.keySet
    debug(s"The following files are in both deposit and latest published version: ${ intersection.mkString(", ") }")
    val checksumsDiffer = intersection.filter(p => pathToFileInfo(p).checksum != pathToFileMetaInLatestVersion(p).dataFile.get.checksum.value) // TODO: validate filemetas first
    debug(s"The following files are in both deposit and latest published version AND have a different checksum: ${ checksumsDiffer.mkString(", ") }")
    checksumsDiffer.map(p => (pathToFileMetaInLatestVersion(p).dataFile.get.id, pathToFileInfo(p))).toMap
  }

  /**
   * Creatings a mapping for moving files to a new location. To determine this, the file needs to be unique in the old and the new version, because
   * its checksum is used to locate it. Files that occur multiple times in either the old or the new version cannot be moved in this way. They
   * will appear to have been deleted in the old version and added in the new. This has the same net result, except that the "Changes" overview in
   * Dataverse does not record that the file was effectively moved.
   *
   * @param pathToFileMetaInLatestVersion map from path to file metadata in the old version
   * @param pathToFileInfo                map from path to file info in the new version (i.e. the deposit).
   * @return
   */
  private def getOldToNewPathOfFilesToMove(pathToFileMetaInLatestVersion: Map[Path, FileMeta], pathToFileInfo: Map[Path, FileInfo]): Try[Map[Path, Path]] = {
    for {
      checksumsToPathNonDuplicatedFilesInDeposit <- getChecksumsToPathOfNonDuplicateFiles(pathToFileInfo.mapValues(_.checksum))
      checksumsToPathNonDuplicatedFilesInLatestVersion <- getChecksumsToPathOfNonDuplicateFiles(pathToFileMetaInLatestVersion.mapValues(_.dataFile.get.checksum.value))
      checksumsOfPotentiallyMovedFiles = checksumsToPathNonDuplicatedFilesInDeposit.keySet intersect checksumsToPathNonDuplicatedFilesInLatestVersion.keySet
      oldToNewPathMovedFiles = checksumsOfPotentiallyMovedFiles
        .map(c => (checksumsToPathNonDuplicatedFilesInLatestVersion(c), checksumsToPathNonDuplicatedFilesInDeposit(c)))
      /*
       * Work-around for a bug in Dataverse. The API seems to lose the directoryLabel when the draft of a second version is started. For now, we therefore don't filter
       * away files that have kept the same path. They will be "moved" in place, making sure the directoryLabel is reconfirmed.
       *
       * For files with duplicates in the same dataset this will not work, because those are not collected above.
       */
      //        .filter { case (pathInLatestVersion, pathInDeposit) => pathInLatestVersion != pathInDeposit }
    } yield oldToNewPathMovedFiles.toMap
  }

  private def getChecksumsToPathOfNonDuplicateFiles(pathToChecksum: Map[Path, String]): Try[Map[String, Path]] = Try {
    pathToChecksum
      .groupBy { case (_, c) => c }
      .filter { case (_, pathToFileInfoMappings) => pathToFileInfoMappings.size == 1 }
      .map { case (c, m) => (c, m.head._1) }
  }

  private def getFileDeletions(paths: Set[Path], pathToFileMeta: Map[Path, FileMeta]): Try[Set[Int]] = Try {
    paths.map(path => pathToFileMeta(path).dataFile.get.id)
  }

  private def deleteFiles(dataset: DatasetApi, databaseIds: List[DatabaseId]): Try[Unit] = {
    databaseIds.map(id => {
      debug(s"Deleting file, databaseId = $id")
      instance.sword().deleteFile(id)
      dataset.awaitUnlock()
    }).collectResults.map(_ => ())
  }

  private def replaceFiles(dataset: DatasetApi, databaseIdToNewFile: Map[Int, FileInfo], prestagedFiles: Set[BasicFileMeta] = Set.empty): Try[Map[Int, FileMeta]] = {
    trace(databaseIdToNewFile, prestagedFiles)
    databaseIdToNewFile.map {
      case (id, fileInfo) =>
        val fileApi = instance.file(id)

        for {
          (replacementId, replacementMeta) <- replaceFile(fileApi, fileInfo, prestagedFiles)
          _ <- dataset.awaitUnlock()
        } yield (replacementId, replacementMeta)
    }.toList.collectResults.map(_.toMap)
  }

  private def replaceFile(fileApi: FileApi, fileInfo: FileInfo, prestagedFiles: Set[BasicFileMeta]): Try[(Int, FileMeta)] = {
    /*
     * Note, forceReplace = true is used, so that the action does not fail if the replacement has a different MIME-type than
     * the replaced file. The only way to pass forceReplace is through the FileMeta. This means we are deleting any existing
     * metadata with the below call. This is not a problem, because the metadata will be made up-to-date at the end of the
     * update process.
     */
    for {
      r <- getPrestagedFileFor(fileInfo, prestagedFiles).map { prestagedFile =>
        debug(s"Replacing with prestaged file: $fileInfo")
        fileApi.replaceWithPrestagedFile(prestagedFile.copy(forceReplace = true))
      }.getOrElse {
        debug(s"Uploading replacement file: $fileInfo")
        fileApi.replace(Option(fileInfo.file), Option(FileMeta(forceReplace = true)))
      }
      fileList <- r.data
      id = fileList.files.head.dataFile.map(_.id).getOrElse(throw new IllegalStateException("Could not get ID of replacement file after replace action"))
    } yield (id, fileInfo.metadata)
  }
}
