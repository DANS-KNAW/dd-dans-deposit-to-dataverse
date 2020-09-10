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

import java.nio.file.{ Path, Paths }

import better.files.File
import nl.knaw.dans.easy.dd2d.dataverse.DataverseInstance
import nl.knaw.dans.easy.dd2d.queue.Task
import nl.knaw.dans.easy.s2d.{ FailedDepositException, RejectedDepositException, ValidateBag }
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.json4s.native.JsonMethods._
import org.json4s.native.Serialization
import org.json4s.{ Formats, _ }

import scala.util.{ Failure, Success, Try }

/**
 * Checks one deposit and then ingests it into Dataverse.
 *
 * @param deposit     the deposit to ingest
 * @param dataverse   the Dataverse instance to ingest in
 * @param jsonFormats implicit necessary for pretty-printing JSON
 */
case class DepositIngestTask(deposit: Deposit, dataverse: DataverseInstance)(implicit jsonFormats: Formats) extends Task with ValidateBag with DebugEnhancedLogging {
  trace(deposit, dataverse)

  val mapper = new DdmToDataverseMapper()
  //todo find a more robust solution
  private val rootToInboxPath = "data/inbox/valid-easy-submitted/example-bag-medium/"

  override def run(): Try[Unit] = Try {
    trace(())
    debug(s"Ingesting $deposit into Dataverse")

//    val validation = validateDansBag(deposit.bagDir.path)

//    validation match {
//      case Success(v) => {
        deposit.tryDdm match {
          case Success(ddm) => {
            mapper.mapToJson(ddm) match {
              case Success(json) => {
                dataverse.dataverse("root")
                  .createDataset(json) match {
                  case Success(responseJson) => {
                    val dvId = readIdFromResponse(responseJson)
                    uploadFilesToDataset(dvId)
                  }
                  case Failure(exception) => {
                    logger.info("Creating dataset failed: " + exception.getMessage)
                  }
                }
              }
              case Failure(exception) => {
                logger.info("Mapping DDM to Dataverse Json failed: " + exception.getMessage)
                Failure(exception)
              }
            }
          }
          case Failure(exception) => {
            logger.info(exception.getMessage)
            Failure(exception)
          }
        }
      }
//      case Failure(exception) => {
//        logger.info(exception.getMessage)
//        Failure(exception)
//      }
//    }
//  }

  private def uploadFilesToDataset(dvId: String): Try[Unit] = {
    deposit.tryFilesXml match {
      case Success(filesXml) => Try {
        mapper.mapFilesToJson(filesXml).foreach(fileMetadata => {
          val path = rootToInboxPath + fileMetadata.directoryLabel.getOrElse("")

          // Dataverse uses a "File Path" indicating which folder the file should be uploaded to within the dataset.
          // The filepath attribute (excluding the filename) of the Bags files.xml is used for this purpose.
          val fileMetadataUpdated = fileMetadata.copy(directoryLabel = getDirPath(fileMetadata.directoryLabel))
          dataverse.dataverse(dvId)
            .uploadFileToDataset(dvId, File(path), Some(Serialization.writePretty(fileMetadataUpdated)))
        })
      }
      case Failure(exception) => Failure(exception)
    }
  }

  private def readIdFromResponse(responseJson: String): String = {
    (parse(responseJson) \\ "persistentId")
      .extract[String]
  }

  private def getDirPath(fullPath: Option[String]): Option[String] = {
    fullPath.map(p => Paths.get(p).getParent.toString)
  }

  private def validateDansBag(bagDir: Path): Try[Unit] = {
    validateBag(bagDir) match {
      case Success(validationResult) =>
        if (validationResult.isCompliant) {
          debug(s"Validation result: $validationResult")
          logger.info("Success!")
          Success(())
        }
        else
          depositRejected(
            s"""
               |Bag was not valid according to Profile Version ${ validationResult.profileVersion }.
               |Violations:
               |${ validationResult.ruleViolations.map(_.map(formatViolation).mkString("\n")).getOrElse("") }
          """.stripMargin)
      case Failure(f) => {
        depositFailed(s"Problem calling easy-validate-dans-bag: ${ f.getMessage }", f)
      }
    }
  }

  protected def depositRejected[T](message: => String, t: Throwable = null): Try[T] = {
    Failure(RejectedDepositException(deposit, message, t))
  }

  protected def depositFailed[T](message: => String, t: Throwable = null): Failure[T] = {
    Failure(FailedDepositException(deposit, message, t))
  }

  private def formatViolation(v: (String, String)): String = v match {
    case (nr, msg) => s" - [$nr] $msg"
  }
}