/**
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

import nl.knaw.dans.easy.dd2d.mapping.AccessRights
import nl.knaw.dans.lib.dataverse.DataverseInstance
import nl.knaw.dans.lib.dataverse.model.file.FileMeta
import nl.knaw.dans.lib.error.TraversableTryExtensions
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.util.{ Success, Try }

/**
 * Object that edits a dataset, a new draft.
 */
abstract class DatasetEditor(instance: DataverseInstance) extends DebugEnhancedLogging {
  type PersistendId = String
  type DatasetId = Int

  /**
   * Performs the task.
   *
   * @return the persistentId of the dataset created or modified
   */
  def performEdit(): Try[PersistendId]

  protected def addFiles(persistentId: String, files: List[FileInfo]): Try[Map[Int, FileInfo]] = {
    trace(persistentId, files)
    files
      .map(f => {
        debug(s"Adding file, directoryLabel = ${ f.metadata.directoryLabel }, label = ${ f.metadata.label }")
        for {
          id <- addFile(persistentId, f)
          _ <- instance.dataset(persistentId).awaitUnlock()
        } yield (id -> f)
      }).collectResults.map(_.toMap)
  }

  private def addFile(doi: String, fileInfo: FileInfo): Try[Int] = {
    val result = for {
      r <- instance.dataset(doi).addFile(Option(fileInfo.file))
      files <- r.data
      id = files.files.headOption.flatMap(_.dataFile.map(_.id))
      _ <- instance.dataset(doi).awaitUnlock()
    } yield id
    result.map(_.getOrElse(throw new IllegalStateException("Could not get DataFile ID from response")))
  }

  protected def updateFileMetadata(databaseIdToFileInfo: Map[Int, FileMeta]): Try[Unit] = {
    trace(databaseIdToFileInfo)
    databaseIdToFileInfo.map { case (id, fileMeta) => instance.file(id).updateMetadata(fileMeta) }.collectResults.map(_ => ())
  }

  protected def configureEnableAccessRequests(deposit: Deposit, persistendId: PersistendId, canEnable: Boolean): Try[Unit] = {
    for {
      ddm <- deposit.tryDdm
      files <- deposit.tryFilesXml
      enable = AccessRights.isEnableRequests((ddm \ "profile" \ "accessRights").head, files)
      _ <- if (enable && canEnable) instance.accessRequests(persistendId).enable()
           else Success(())
      _ <- if (!enable) instance.accessRequests(persistendId).disable()
           else Success(())
    } yield ()
  }
}
