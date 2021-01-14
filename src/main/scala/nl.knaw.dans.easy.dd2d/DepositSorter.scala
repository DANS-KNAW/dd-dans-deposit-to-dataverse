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

import java.io.FileInputStream
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.Properties

import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import nl.knaw.dans.lib.taskqueue.{ Task, TaskSorter }

class DepositSorter extends TaskSorter[Deposit] with DebugEnhancedLogging {

  private case class DepositsSortInfo(name: String, timeStamp: LocalDateTime, isVersionOf: Option[String], depositTask: DepositIngestTask)

  private type baseIdToAllVersions = Map[String, List[DepositsSortInfo]]
  private val BAG_INFO_FILE = "bag-info.txt"
  private val IS_VERSION_OF = "Is-Version-Of"
  private val CREATED = "Created"

  /**
   * sorts the (DepositIngest)Tasks by dataset and per dataset by version
   *
   * @param tasks list of DepositIngestTasks
   * @return the sorted list of deposit directories
   */
  override def sort(tasks: List[Task[Deposit]]): List[Task[Deposit]] = {
    val sortInfoList = tasks.map(d => getDepositSortInfo(d.asInstanceOf[DepositIngestTask]))
    val depositSequences = getDepositSequences(sortInfoList)
    sortByVersion(depositSequences)
  }

  private def getDepositSortInfo(depositIngestTask: DepositIngestTask): DepositsSortInfo = {
    val bagInfoFile = depositIngestTask.deposit.dir.list(_.isRegularFile, 2).filter(_.name == BAG_INFO_FILE).toList.head
    val properties = new Properties()
    properties.load(new FileInputStream(bagInfoFile.pathAsString));
    val timeStamp = parseTimestamp(properties.getProperty(CREATED))
    val isVersionOf: Option[String] = properties.getProperty(IS_VERSION_OF) match {
      case null => None
      case uuid: String => Some(uuid)
    }
    DepositsSortInfo(s"urn:uuid:${ depositIngestTask.deposit.dir.name }", timeStamp, isVersionOf, depositIngestTask)
  }

  private def getDepositSequences(depositsSortInfoList: List[DepositsSortInfo]): baseIdToAllVersions = {
    val firstVersions = depositsSortInfoList
      .filter(_.isVersionOf.isEmpty)
      .map(v => v.name -> List(v)).toMap

    val laterVersions =
      depositsSortInfoList
        .filter(_.isVersionOf.isDefined)
        .groupBy(_.isVersionOf.get)

    val laterVersionsUpdated = removeDatasetsWithoutFirstVersion(firstVersions, laterVersions)

    (firstVersions.keySet ++ laterVersionsUpdated.keySet)
      .map(k => {
        if (laterVersionsUpdated.contains(k))
          k -> (firstVersions(k) ++ laterVersionsUpdated(k))
        else
          k -> firstVersions(k)
      })
      .toMap
  }

  private def sortByVersion(groupedDeposits: baseIdToAllVersions): List[DepositIngestTask] = {
    groupedDeposits.mapValues(sortByTimestamp)
      .flatMap(_._2)
      .map(_.depositTask)
      .toList
  }

  private def removeDatasetsWithoutFirstVersion(firstVersions: baseIdToAllVersions, laterVersions: baseIdToAllVersions): baseIdToAllVersions = {
    val datasetsWithoutFirstVersions = laterVersions.filterNot(k => firstVersions.contains(k._1))
    datasetsWithoutFirstVersions.foreach(v => logger.error(s"No first version was found for dataset ${ v._1 }. The dataset was not imported into Dataverse"))
    val correctVersions = for (k <- laterVersions if firstVersions.keySet.contains(k._1)) yield k
    correctVersions
  }

  private def parseTimestamp(timestampString: String): LocalDateTime = {
    LocalDateTime.parse(timestampString, DateTimeFormatter.ISO_OFFSET_DATE_TIME)
  }

  private def sortByTimestamp(sortInfoList: List[DepositsSortInfo]): List[DepositsSortInfo] = {
    implicit def ordered: Ordering[LocalDateTime] = (x: LocalDateTime, y: LocalDateTime) => x compareTo y

    sortInfoList.sortBy(_.timeStamp)
  }
}
