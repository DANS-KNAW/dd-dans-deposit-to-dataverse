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
import org.apache.tika.Tika

import java.util.zip.Deflater

class ZipFileHandler(tempDir: File) extends DebugEnhancedLogging {
  private val tika = new Tika()
  /*
   * The file types that Dataverse wants unpackage or repackage.
   */
  private val needToBeZipWrapped = List(
    "application/zip",
    "application/zipped-shapefile",
    "application/fits-gzipped"
  )

  def wrapIfZipFile(file: File): Option[File] = {
    if (needsToBeWrapped(file)) {
      logger.info("ZIP file found: {}. Creating ZIP-wrapper around it...", file)
      val wrapper = file.zipTo(File.newTemporaryFile("zip-wrapped-", ".zip", Some(tempDir)), Deflater.NO_COMPRESSION)
      logger.info("Wrapper created at {}", wrapper)
      Option(wrapper)
    }
    else Option.empty
  }

  private def needsToBeWrapped(file: File): Boolean = {
    file.name.toLowerCase.endsWith(".zip") ||
      needToBeZipWrapped.contains(tika.detect(file.toJava))
  }
}
