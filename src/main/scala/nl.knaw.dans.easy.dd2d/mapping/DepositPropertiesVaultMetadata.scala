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
package nl.knaw.dans.easy.dd2d.mapping

import org.apache.commons.lang.StringUtils

object DepositPropertiesVaultMetadata extends BlockCitation {
  def toOtherIdValue(dansVaultMetadataOtherId: String): Option[JsonObject] = {
    if (StringUtils.isBlank(dansVaultMetadataOtherId)) Option.empty
    else {
      val m = FieldMap()
      val trimmedOtherId = dansVaultMetadataOtherId.trim
      if (trimmedOtherId.exists(_.isWhitespace)) throw new IllegalArgumentException("Identifier must not contain whitespace")
      val parts = trimmedOtherId.split(":")
      if (parts.size != 2) throw new IllegalArgumentException("Other ID value has invalid format. It should be '<prefix>:<suffix>'")
      m.addPrimitiveField(OTHER_ID_AGENCY, parts(0))
      m.addPrimitiveField(OTHER_ID_VALUE, parts(1))
      Option(m.toJsonObject)
    }
  }
}
