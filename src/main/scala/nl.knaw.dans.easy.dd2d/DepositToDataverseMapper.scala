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

import nl.knaw.dans.easy.dd2d.mapping._
import nl.knaw.dans.lib.dataverse.model.dataset.{ CompoundField, ControlledMultipleValueField, Dataset, DatasetVersion, MetadataBlock, MetadataField, PrimitiveMultipleValueField, PrimitiveSingleValueField }

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps
import scala.util.Try
import scala.xml.{ Elem, Node, NodeSeq }

/**
 * Maps DANS Dataset Metadata to Dataverse JSON.
 */
// TODO: Rename if we also need to take elements from EMD
class DepositToDataverseMapper(narcisClassification: Elem, isoToDataverseLanguage: Map[String, String]) extends BlockCitation
  with BlockArchaeologySpecific
  with BlockTemporalAndSpatial
  with BlockRights
  with BlockDataVaultMetadata {
  lazy val citationFields = new ListBuffer[MetadataField]
  lazy val archaeologySpecificFields = new ListBuffer[MetadataField]
  lazy val temporalSpatialFields = new ListBuffer[MetadataField]
  lazy val rightsFields = new ListBuffer[MetadataField]
  lazy val dataVaultFields = new ListBuffer[MetadataField]

  def toDataverseDataset(ddm: Node, contactData: CompoundField, vaultMetadata: VaultMetadata): Try[Dataset] = Try {
    // Please, keep ordered by order in Dataverse UI as much as possible (note, if display-on-create is not set for all fields, some may be hidden initally)

    val titles = ddm \ "profile" \ "title"
    if (titles.isEmpty) throw MissingRequiredFieldException("title")

    val alternativeTitles = (ddm \ "dcmiMetadata" \ "title") ++ (ddm \ "dcmiMetadata" \ "alternative")

    // Citation
    addPrimitiveFieldSingleValue(citationFields, TITLE, titles.head)
    addPrimitiveFieldSingleValue(citationFields, ALTERNATIVE_TITLE, alternativeTitles)
    addCompoundFieldMultipleValues(citationFields, OTHER_ID, ddm \ "dcmiMetadata" \ "isFormatOf", IsFormatOf toOtherIdValueObject)
    addCompoundFieldMultipleValues(citationFields, AUTHOR, ddm \ "profile" \ "creatorDetails" \ "author", DcxDaiAuthor toAuthorValueObject)
    addCompoundFieldMultipleValues(citationFields, AUTHOR, ddm \ "profile" \ "creatorDetails" \ "organization", DcxDaiOrganization toAuthorValueObject)
    addCompoundFieldMultipleValues(citationFields, AUTHOR, ddm \ "profile" \ "creator", Creator toAuthorValueObject)
    citationFields.append(contactData)
    addCompoundFieldMultipleValues(citationFields, DESCRIPTION, ddm \ "profile" \ "description", Description toDescriptionValueObject)
    addCompoundFieldMultipleValues(citationFields, DESCRIPTION, if (alternativeTitles.isEmpty) NodeSeq.Empty
                                                                else alternativeTitles.tail, Description toDescriptionValueObject)
    // TODO: add languages that cannot be mapped to Dataverse language terms.

    val audience = ddm \ "profile" \ "audience"
    if (audience.isEmpty) throw MissingRequiredFieldException(SUBJECT)

    addCvFieldMultipleValues(citationFields, SUBJECT, ddm \ "profile" \ "audience", Audience toCitationBlockSubject)
    addCvFieldMultipleValues(citationFields, LANGUAGE, ddm \ "dcmiMetadata" \ "language", Language.toCitationBlockLanguage(isoToDataverseLanguage))
    addPrimitiveFieldSingleValue(citationFields, PRODUCTION_DATE, ddm \ "profile" \ "created", DateTypeElement toYearMonthDayFormat)
    addCompoundFieldMultipleValues(citationFields, CONTRIBUTOR, ddm \ "dcmiMetadata" \ "contributorDetails" \ "author", DcxDaiAuthor toContributorValueObject)
    addCompoundFieldMultipleValues(citationFields, CONTRIBUTOR, ddm \ "dcmiMetadata" \ "contributorDetails" \ "organization", DcxDaiOrganization toContributorValueObject)
    addPrimitiveFieldSingleValue(citationFields, DISTRIBUTION_DATE, ddm \ "profile" \ "available", DateTypeElement toYearMonthDayFormat)
    addPrimitiveFieldMultipleValues(citationFields, DATA_SOURCES, ddm \ "dcmiMetadata" \ "source")

    // Archaeology specific
    addPrimitiveFieldMultipleValues(archaeologySpecificFields, ARCHIS_ZAAK_ID, ddm \ "dcmiMetadata" \ "identifier", IsFormatOf toArchisZaakId)
    addCompoundFieldWithControlledVocabulary(archaeologySpecificFields, ABR_RAPPORT_TYPE, (ddm \ "dcmiMetadata" \ "reportNumber").filter(AbrReportType isAbrReportType), AbrReportType toAbrRapportType)
    addPrimitiveFieldMultipleValues(archaeologySpecificFields, ABR_RAPPORT_NUMMER, ddm \ "dcmiMetadata" \ "reportNumber")
    addCompoundFieldWithControlledVocabulary(archaeologySpecificFields, ABR_VERWERVINGSWIJZE, (ddm \ "dcmiMetadata" \ "acquisitionMethod").filter(AbrAcquisitionMethod isAbrVerwervingswijze), AbrAcquisitionMethod toVerwervingswijze)
    addCompoundFieldWithControlledVocabulary(archaeologySpecificFields, ABR_COMPLEX, (ddm \ "dcmiMetadata" \ "subject").filter(SubjectAbr isAbrComplex), SubjectAbr toAbrComplex)
    addCompoundFieldWithControlledVocabulary(archaeologySpecificFields, ABR_PERIOD, (ddm \ "dcmiMetadata" \ "temporal").filter(TemporalAbr isAbrPeriod), TemporalAbr toAbrPeriod)

    // Temporal and spatial coverage
    addCompoundFieldMultipleValues(temporalSpatialFields, SPATIAL_POINT, ddm \ "dcmiMetadata" \ "spatial" \ "Point", SpatialPoint toEasyTsmSpatialPointValueObject)
    addCompoundFieldMultipleValues(temporalSpatialFields, SPATIAL_BOX, ddm \ "dcmiMetadata" \ "spatial" \ "boundedBy", SpatialBox toEasyTsmSpatialBoxValueObject)

    // Rights
    val rightsHolder = ddm \ "dcmiMetadata" \ "rightsHolder"
    if (rightsHolder.isEmpty) throw MissingRequiredFieldException(RIGHTS_HOLDER)
    addPrimitiveFieldMultipleValues(rightsFields, RIGHTS_HOLDER, ddm \ "dcmiMetadata" \ "rightsHolder", AnyElement toText)

    // Data vault
    addVaultValue(dataVaultFields, BAG_ID, vaultMetadata.dataverseBagId)
    addVaultValue(dataVaultFields, NBN, vaultMetadata.dataverseNbn)
    addVaultValue(dataVaultFields, DANS_OTHER_ID, vaultMetadata.dataverseOtherId)
    addVaultValue(dataVaultFields, DANS_OTHER_ID_VERSION, vaultMetadata.dataverseOtherIdVersion)
    addVaultValue(dataVaultFields, SWORD_TOKEN, vaultMetadata.dataverseSwordToken)

    assembleDataverseDataset()
  }

  private def assembleDataverseDataset(): Dataset = {
    val versionMap = mutable.Map[String, MetadataBlock]()
    addMetadataBlock(versionMap, "citation", "Citation Metadata", citationFields)
    addMetadataBlock(versionMap, "archaeologyMetadata", "Archaeology-Specific Metadata", archaeologySpecificFields)
    addMetadataBlock(versionMap, "temporal-spatial", "Temporal and Spatial Coverage", temporalSpatialFields)
    addMetadataBlock(versionMap, "dansRights", "Rights Metadata", rightsFields)
    addMetadataBlock(versionMap, "dataVault", "Data Vault Metadata", dataVaultFields)
    val datasetVersion = DatasetVersion(metadataBlocks = versionMap.toMap)
    Dataset(datasetVersion)
  }

  private def addPrimitiveFieldSingleValue(metadataBlockFields: ListBuffer[MetadataField], name: String, sourceNodes: NodeSeq, nodeTransformer: Node => Option[String] = AnyElement toText): Unit = {
    sourceNodes
      .map(nodeTransformer)
      .filter(_.isDefined)
      .map(_.get)
      .take(1)
      .foreach(v => metadataBlockFields += PrimitiveSingleValueField(name, v))
  }

  private def addPrimitiveFieldMultipleValues(metadataBlockFields: ListBuffer[MetadataField], name: String, sourceNodes: NodeSeq, nodeTransformer: Node => Option[String] = AnyElement toText): Unit = {
    val values = sourceNodes.map(nodeTransformer).filter(_.isDefined).map(_.get).toList
    if (values.nonEmpty) {
      metadataBlockFields += PrimitiveMultipleValueField(name, values)
    }
  }

  private def addCvFieldMultipleValues(metadataBlockFields: ListBuffer[MetadataField], name: String, sourceNodes: NodeSeq, nodeTransformer: Node => Option[String]): Unit = {
    val values = sourceNodes.map(nodeTransformer).filter(_.isDefined).map(_.get).toList
    if (values.nonEmpty) {
      metadataBlockFields += ControlledMultipleValueField(name, values)
    }
  }

  private def addCompoundFieldMultipleValues(metadataBlockFields: ListBuffer[MetadataField], name: String, sourceNodes: NodeSeq, nodeTransformer: Node => JsonObject): Unit = {
    val valueObjects = new ListBuffer[JsonObject]()
    sourceNodes.foreach(e => valueObjects += nodeTransformer(e))
    if (valueObjects.nonEmpty) {
      metadataBlockFields += CompoundField(name, valueObjects.toList)
    }
  }

  private def addCompoundFieldWithControlledVocabulary(metadataBlockFields: ListBuffer[MetadataField], name: String, sourceNodes: NodeSeq, nodeTransformer: Node => Option[JsonObject]): Unit = {
    val valueObjects = new ListBuffer[JsonObject]()
    sourceNodes.foreach({
      e =>
        nodeTransformer(e) match {
          case Some(jsonObject: JsonObject) => valueObjects += jsonObject
          case None => valueObjects
        }
    })
    if (valueObjects.nonEmpty) {
      metadataBlockFields += CompoundField(name, valueObjects.toList)
    }
  }

  private def addVaultValue(metadataBlockFields: ListBuffer[MetadataField], name: String, value: String): Unit = {
    if (value.nonEmpty) {
      metadataBlockFields += PrimitiveSingleValueField(name, value)
    }
  }

  private def addMetadataBlock(versionMap: mutable.Map[String, MetadataBlock], blockId: String, blockDisplayName: String, fields: ListBuffer[MetadataField]): Unit = {
    if (fields.nonEmpty) {
      versionMap.put(blockId, MetadataBlock(blockDisplayName, fields.toList))
    }
  }
}
