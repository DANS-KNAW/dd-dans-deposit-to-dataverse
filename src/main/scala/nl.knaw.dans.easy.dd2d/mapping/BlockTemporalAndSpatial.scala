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

trait BlockTemporalAndSpatial {
  val TEMPORAL_COVERAGE = "dansTemporalCoverage"
  val SPATIAL_POINT = "dansSpatialPoint"
  val SPATIAL_POINT_SCHEME = "dansSpatialPointScheme"
  val SPATIAL_POINT_X = "dansSpatialPointX"
  val SPATIAL_POINT_Y = "dansSpatialPointY"
  val SPATIAL_BOX = "dansSpatialBox"
  val SPATIAL_BOX_SCHEME = "dansSpatialBoxScheme"
  val SPATIAL_BOX_NORTH = "dansSpatialBoxNorth"
  val SPATIAL_BOX_EAST = "dansSpatialBoxEast"
  val SPATIAL_BOX_SOUTH = "dansSpatialBoxSouth"
  val SPATIAL_BOX_WEST = "dansSpatialBoxWest"
  val SPATIAL_COVERAGE_CONTROLLED = "dansSpatialCoverageControlled"
  val SPATIAL_COVERAGE_UNCONTROLLED = "dansSpatialCoverageText"
}
