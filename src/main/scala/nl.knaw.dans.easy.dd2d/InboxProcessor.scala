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

import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import nl.knaw.dans.lib.taskqueue.PassiveTaskQueue

import scala.util.Try

class InboxProcessor(inbox: Inbox) extends DebugEnhancedLogging {

  def process(): Try[Unit] = {
    trace(())
    for {
      ingestTasks <- Try { new PassiveTaskQueue[Deposit]() }
      _ = logger.info("Enqueuing deposits found in inbox...")
      _ <- inbox.enqueue(ingestTasks, Some(new DepositSorter))
      _ = logger.info("Processing queue...")
      _ <- ingestTasks.process()
      _ = logger.info("Done processing.")
    } yield ()
  }
}
