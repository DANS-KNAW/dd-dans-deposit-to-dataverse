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

import scala.xml.Node

object SpatialPoint extends Spatial with BlockTemporalAndSpatial {
  def toEasyTsmSpatialPointValueObject(spatial: Node): JsonObject = {
    val isRD = isRd(spatial) // TODO: improve error handling
    // TODO: Only one Point expected here, but should be more robust
    val pointElem = (spatial \ "Point").head
    // Passing in spatial to work around the fact that the point element does not have an srsName itself.
    val p = getPoint(isRD)(pointElem)
    val m = FieldMap()

    m.addCvField(SPATIAL_POINT_SCHEME, if (isRD) RD_SCHEME
                                       else LONLAT_SCHEME)
    m.addPrimitiveField(SPATIAL_POINT_X, p.x)
    m.addPrimitiveField(SPATIAL_POINT_Y, p.y)
    m.toJsonObject
  }
}
