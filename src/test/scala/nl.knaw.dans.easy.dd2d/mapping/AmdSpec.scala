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

import nl.knaw.dans.easy.dd2d.TestSupportFixture

class AmdSpec extends TestSupportFixture with BlockCitation {

  "toDateOfDeposit" should "use date of first change to SUBMITTED state" in {
    val xml =
    <damd:administrative-md version="0.1" xmlns:damd="http://easy.dans.knaw.nl/easy/dataset-administrative-metadata/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:foxml="info:fedora/fedora-system:def/foxml#">
      <datasetState>PUBLISHED</datasetState>
      <previousState>MAINTENANCE</previousState>
      <depositorId>rutgerelsma</depositorId>
      <stateChangeDates>
        <damd:stateChangeDate>
          <fromState>DRAFT</fromState>
          <toState>SUBMITTED</toState>
          <changeDate>2017-04-13T11:03:05.000+02:00</changeDate>
        </damd:stateChangeDate>
        <damd:stateChangeDate>
          <fromState>SUBMITTED</fromState>
          <toState>DRAFT</toState>
          <changeDate>2017-04-14T11:03:05.000+02:00</changeDate>
        </damd:stateChangeDate>
        <damd:stateChangeDate>
          <fromState>DRAFT</fromState>
          <toState>SUBMITTED</toState>
          <changeDate>2017-04-15T11:03:05.000+02:00</changeDate>
        </damd:stateChangeDate>
        <damd:stateChangeDate>
          <fromState>SUBMITTED</fromState>
          <toState>PUBLISHED</toState>
          <changeDate>2017-04-13T14:35:11.281+02:00</changeDate>
        </damd:stateChangeDate>
        <damd:stateChangeDate>
          <fromState>PUBLISHED</fromState>
          <toState>MAINTENANCE</toState>
          <changeDate>2017-08-08T16:51:19.886+02:00</changeDate>
        </damd:stateChangeDate>
        <damd:stateChangeDate>
          <fromState>MAINTENANCE</fromState>
          <toState>PUBLISHED</toState>
          <changeDate>2017-08-08T16:52:20.914+02:00</changeDate>
        </damd:stateChangeDate>
      </stateChangeDates>
      <groupIds/>
      <damd:workflowData version="0.1" />
    </damd:administrative-md>

    Amd.toDateOfDeposit(xml) shouldBe Some("2017-04-13")
  }

  it should "use date of first change to PUBLISHED state if no change to SUBMITTED state is found" in {
    val xml = <damd:administrative-md xmlns:damd="http://easy.dans.knaw.nl/easy/dataset-administrative-metadata/" version="0.1">
                <datasetState>PUBLISHED</datasetState>
                <previousState>DRAFT</previousState>
                <lastStateChange>2019-04-25T12:20:01.636+02:00</lastStateChange>
                <depositorId>PANVU</depositorId>
                <stateChangeDates>
                  <damd:stateChangeDate>
                    <fromState>DRAFT</fromState>
                    <toState>PUBLISHED</toState>
                    <changeDate>2019-04-25T12:20:01.636+02:00</changeDate>
                  </damd:stateChangeDate>
                </stateChangeDates>
                <groupIds></groupIds>
                <damd:workflowData version="0.1" />
              </damd:administrative-md>
    Amd.toDateOfDeposit(xml) shouldBe Some("2019-04-25")
  }
}
