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
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import nl.knaw.dans.lib.taskqueue.AbstractInbox

/**
 * The inbox directory containing deposit directories. It can enqueue DepositIngestTasks in
 * on a TaskQueue in several ways.
 *
 * @param dir                      the file system directory
 * @param depositIngestTaskFactory factory object that creates DepositIngestTask objects for deposits
 */
class Inbox(dir: File, depositIngestTaskFactory: DepositIngestTaskFactory) extends AbstractInbox[Deposit](dir) with DebugEnhancedLogging {
  override def createTask(f: File): Option[DepositIngestTask] = {
    try {
      Some(depositIngestTaskFactory.createDepositIngestTask(Deposit(f)))
    }
    catch {
      case e: InvalidDepositException =>
        logger.warn(e.getMessage)
        None
    }
  }
}
