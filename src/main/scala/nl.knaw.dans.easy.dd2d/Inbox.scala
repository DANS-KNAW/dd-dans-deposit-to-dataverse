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

import better.files.{ File, FileMonitor }
import nl.knaw.dans.easy.dd2d.dansbag.DansBagValidator
import nl.knaw.dans.easy.dd2d.dataverse.DataverseInstance
import nl.knaw.dans.easy.dd2d.queue.TaskQueue
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.json4s.{ DefaultFormats, Formats }

/**
 * The inbox directory containing deposit directories. It can enqueue DepositIngestTasks in
 * on a TaskQueue in several ways.
 *
 * @param dir the file system directory
 * @param dataverse the DataverseInstance to use for the DepositIngestTasks
 */
class Inbox(dir: File, dansBagValidator: DansBagValidator, dataverse: DataverseInstance, autoPublish: Boolean = true) extends DebugEnhancedLogging {
  private implicit val jsonFormats: Formats = new DefaultFormats {}
  private val dirs = dir.list(_.isDirectory, maxDepth = 1).filterNot(_ == dir).toList

  /**
   * Directly enqueue the deposit directories currently present as deposits on the queue
   *
   * @param q the TaskQueue to put the DepositIngestTasks on
   */
  def enqueue(q: TaskQueue): Unit = {
    dirs.foreach {
      d => {
        debug(s"Adding $d")
        q.add(DepositIngestTask(Deposit(d), dansBagValidator, dataverse, publish = autoPublish))
      }
    }
  }

  /**
   * Creates and returns a FileMonitor that enqueues new deposits as they appear in
   * the inbox directory. Note that the caller is responsible for starting the FileMonitor
   * in the ExecutionContext of its choice.
   *
   * @param q the TaskQueue to put the DepositIngestTasks on
   * @return the FileMonitor
   */
  def createFileMonitor(q: TaskQueue): FileMonitor = {
    new FileMonitor(dir, maxDepth = 1) {
      override def onCreate(d: File, count: Int): Unit = {
        trace(d, count)
        if (d.isDirectory) {
          logger.debug(s"Detected new subdirectory in inbox. Adding $d")
          q.add(DepositIngestTask(Deposit(d), dansBagValidator, dataverse, publish = autoPublish))
        }
      }
    }
  }
}
