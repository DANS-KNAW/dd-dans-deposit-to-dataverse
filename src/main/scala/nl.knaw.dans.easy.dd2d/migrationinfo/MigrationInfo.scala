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
package nl.knaw.dans.easy.dd2d.migrationinfo

import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.json4s.native.JsonMethods
import org.json4s.{ DefaultFormats, Formats }
import scalaj.http.Http

import java.net.URI
import scala.util.Try

class MigrationInfo(config: MigrationInfoConfig) extends DebugEnhancedLogging {
  private implicit val jsonFormats: Formats = DefaultFormats

  def checkConnection(): Try[Unit] = {
    logger.info(s"Checking if migration-info service can be reached at ${ config.baseUrl.toASCIIString }")
    Try {
      Http(config.baseUrl.toASCIIString)
        .timeout(connTimeoutMs = config.connectionTimeout, readTimeoutMs = config.readTimeout)
        .asBytes
    } map {
      case r if r.code == 200 =>
        logger.info("OK: migration-info service is reachable.")
        ()
      case r => throw new RuntimeException(s"Connection to migration-info service could not be established. Status: ${ r.code }")
    }
  }

  def getPrestagedDataFilesFor(doi: String, seqNr: Int): Try[Set[BasicFileMeta]] = {
    trace(doi, seqNr)
    val url = (new URI(config.baseUrl + "/") resolve s"datasets/:persistentId/seq/$seqNr/basic-file-metas").toASCIIString
    debug(s"Retrieving pre-staged files for $doi from $url")
    Try {
      Http(url)
        .params("persistentId" -> doi)
        .timeout(connTimeoutMs = config.connectionTimeout, readTimeoutMs = config.readTimeout)
        .asString
    } map {
      case r if r.code == 200 =>
        val json = JsonMethods.parse(r.body)
        json.extract[List[BasicFileMeta]].toSet
      case r if r.code == 404 =>
        logger.warn(s"No pre-staged files could be found for dataset $doi. Returning empty result.")
        Set.empty
      case r => throw new RuntimeException(s"Could not retrieve pre-staged files. Status: ${ r.code }, Message: ${ r.body }")
    }
  }
}
