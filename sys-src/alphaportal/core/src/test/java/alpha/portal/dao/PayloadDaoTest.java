/**************************************************************************
 * alpha-Portal: A web portal, for managing knowledge-driven 
 * ad-hoc processes, in form of case files.
 * ==============================================
 * Copyright (C) 2011-2012 by 
 *   - Christoph P. Neumann (http://www.chr15t0ph.de)
 *   - and the SWAT 2011 team
 **************************************************************************
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *     http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software 
 * distributed under the License is distributed on an "AS IS" BASIS, 
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 **************************************************************************
 * $Id$
 *************************************************************************/
package alpha.portal.dao;

import java.util.List;

import org.appfuse.dao.BaseDaoTestCase;
import org.junit.Assert;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCase;
import alpha.portal.model.Payload;
import alpha.portal.model.PayloadIdentifier;
import alpha.portal.service.AlphaCardManager;
import alpha.portal.service.impl.CaseManagerImpl;

/**
 * The Class PayloadDaoTest.
 */
public class PayloadDaoTest extends BaseDaoTestCase {

	/** The payload dao. */
	@Autowired
	private PayloadDao payloadDao;

	/** The alpha card manager. */
	@Autowired
	private AlphaCardManager alphaCardManager;

	/** The case manager. */
	@Autowired
	private CaseManagerImpl caseManager;

	/**
	 * Test get all versions.
	 */
	@Test
	public void testGetAllVersions() {
		AlphaCase aCase = new AlphaCase();
		aCase = this.caseManager.save(aCase);

		// AlphaCard aCard =
		// alphaCardManager.createAlphaCard(aCase.getCaseId());
		AlphaCard aCard = new AlphaCard(aCase);

		byte[] content = "payload data1".getBytes();
		Payload payload = new Payload();
		payload.setFilename("filename");
		payload.setMimeType("text/plain");
		payload.setContent(content);
		payload = this.payloadDao.save(payload); // first payload version
		this.flush();

		List<Payload> payloads = this.payloadDao.getAllVersions(payload);
		Assert.assertEquals(1, payloads.size());

		aCard.setPayload(payload);
		aCard = this.alphaCardManager.save(aCard);
		this.flush();

		aCard.setPayload(null);
		aCard = this.alphaCardManager.save(aCard);

		payloads = this.payloadDao.getAllVersions(payload);
		Assert.assertEquals(1, payloads.size());

		content = "payload data2".getBytes();
		Payload payload2 = new Payload();
		payload2.setPayloadIdentifier(new PayloadIdentifier(payload
				.getPayloadIdentifier().getPayloadId(), 0));
		payload2.setFilename("filename");
		payload2.setMimeType("text/plain");
		payload2.setContent(content);
		payload2 = this.payloadDao.save(payload2); // second payload version
		this.flush();

		aCard.setPayload(payload2);
		aCard = this.alphaCardManager.save(aCard);

		payloads = this.payloadDao.getAllVersions(payload2);
		Assert.assertEquals(2, payloads.size());

	}

	/**
	 * Test get version.
	 */
	@Test
	public void testGetVersion() {

		AlphaCase aCase = new AlphaCase();

		aCase = this.caseManager.save(aCase);

		// AlphaCard aCard =
		// alphaCardManager.createAlphaCard(aCase.getCaseId());
		AlphaCard aCard = new AlphaCard(aCase);

		byte[] content = "payload data1".getBytes();
		Payload payload = new Payload();
		payload.setFilename("filename");
		payload.setMimeType("text/plain");
		payload.setContent(content);
		payload = this.payloadDao.save(payload);
		this.flush();

		aCard.setPayload(payload);

		aCard = this.alphaCardManager.save(aCard);
		this.flush();
		aCard.setPayload(null);
		aCard = this.alphaCardManager.save(aCard);

		content = "payload data2".getBytes();
		Payload payload2 = new Payload();
		payload2.setFilename("filename");
		payload2.setMimeType("text/plain");
		payload2.setContent(content);
		payload2 = this.payloadDao.save(payload2);
		this.flush();

		aCard.setPayload(payload2);

		aCard = this.alphaCardManager.save(aCard);

		final Payload backPayload = this.payloadDao.getVersion(payload2
				.getPayloadIdentifier());

		Assert.assertTrue(backPayload.getPayloadIdentifier()
				.getSequenceNumber() == payload2.getPayloadIdentifier()
				.getSequenceNumber());

	}

	/**
	 * Test save.
	 */
	@Test
	public void testSave() {

		AlphaCase aCase = new AlphaCase();

		aCase = this.caseManager.save(aCase);

		// AlphaCard aCard =
		// alphaCardManager.createAlphaCard(aCase.getCaseId());
		AlphaCard aCard = new AlphaCard(aCase);

		final byte[] content = "payload data1".getBytes();
		Payload payload = new Payload();
		payload.setFilename("filename");
		payload.setMimeType("text/plain");
		payload.setContent(content);
		payload = this.payloadDao.save(payload);
		this.flush();

		final Long payloadId = payload.getPayloadIdentifier().getPayloadId();

		aCard.setPayload(payload);

		aCard = this.alphaCardManager.save(aCard);
		this.flush();

		Assert.assertTrue(payloadId == this.payloadDao
				.getVersion(payload.getPayloadIdentifier())
				.getPayloadIdentifier().getPayloadId());

		final int oldVersionCount = this.payloadDao.getAllVersions(payload)
				.size();
		final byte[] content2 = "payload data2".getBytes();
		payload.setContent(content2);
		payload = this.payloadDao.save(payload);
		this.flush();

		Assert.assertFalse(oldVersionCount == this.payloadDao.getAllVersions(
				payload).size());
	}
}
