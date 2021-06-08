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
import nl.knaw.dans.lib.dataverse.model.dataset.{ Dataset, DatasetCreationResult }
import nl.knaw.dans.lib.dataverse.model.{ DefaultRole, RoleAssignment }
import nl.knaw.dans.lib.dataverse.{ DataverseInstance, DataverseResponse }
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.util.Try

class DatasetCreator(deposit: Deposit, isMigration: Boolean = false, dataverseDataset: Dataset, instance: DataverseInstance) extends DatasetEditor(instance) with DebugEnhancedLogging {
  trace(deposit)

  override def performEdit(): Try[PersistendId] = {
    for {
      // autoPublish is false, because it seems there is a bug with it in Dataverse (most of the time?)
      response <- if (isMigration)
                    instance
                      .dataverse("root")
                      .importDataset(dataverseDataset, Some(s"doi:${ deposit.doi }"), autoPublish = false)
                  else instance.dataverse("root").createDataset(dataverseDataset)
      persistentId <- getPersistentId(response)
      fileInfos <- deposit.getPathToFileInfo
      databaseIdsToFileInfo <- addFiles(persistentId, fileInfos.values.toList)
      _ <- updateFileMetadata(databaseIdsToFileInfo.mapValues(_.metadata))
      _ <- instance.dataset(persistentId).awaitUnlock()
      _ <- configureEnableAccessRequests(deposit, persistentId, canEnable = true)
      _ <- instance.dataset(persistentId).awaitUnlock()
      _ = debug(s"Assigning curator role to ${deposit.depositorUserId}")
      _ <- instance.dataset(persistentId).assignRole(RoleAssignment(s"@${ deposit.depositorUserId }", DefaultRole.curator.toString))
      _ <- instance.dataset(persistentId).awaitUnlock()
    } yield persistentId
  }

  private def getPersistentId(response: DataverseResponse[DatasetCreationResult]): Try[String] = {
    response.data.map(_.persistentId)
  }
}
