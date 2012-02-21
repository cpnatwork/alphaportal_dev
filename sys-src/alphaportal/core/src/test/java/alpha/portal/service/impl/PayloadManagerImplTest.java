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
package alpha.portal.service.impl;

import java.util.List;

import org.appfuse.service.impl.BaseManagerMockTestCase;
import org.hibernate.criterion.Criterion;
import org.jmock.Expectations;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import alpha.portal.dao.AlphaCardDao;
import alpha.portal.dao.PayloadDao;
import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCardIdentifier;
import alpha.portal.model.AlphaCase;
import alpha.portal.model.Payload;
import alpha.portal.model.PayloadIdentifier;
import alpha.portal.service.AlphaCardManager;

/**
 * The Class PayloadManagerImplTest.
 */
public class PayloadManagerImplTest extends BaseManagerMockTestCase {

	/** The card manager. */
	private AlphaCardManagerImpl cardManager = null;

	/** The manager. */
	private PayloadManagerImpl manager = null;

	/** The dao. */
	private PayloadDao dao = null;

	/** The card dao. */
	private AlphaCardDao cardDao = null;

	/** The payload2. */
	Payload payload2 = new Payload();

	/**
	 * Sets the up.
	 */
	@Before
	public void setUp() {
		this.dao = this.context.mock(PayloadDao.class);
		this.cardDao = this.context.mock(AlphaCardDao.class);
		this.manager = new PayloadManagerImpl(this.dao);
		this.cardManager = new AlphaCardManagerImpl(this.cardDao);
	}

	/**
	 * Tear down.
	 */
	@After
	public void tearDown() {
		this.manager = null;
	}

	/**
	 * Test get all versions.
	 */
	@Test
	public void testGetAllVersions() {

		final String testCaseId = "myCaseId";
		final AlphaCard aCard = this.cardManager.createAlphaCard(testCaseId);

		// aCard = alphaCardDao.save(aCard);

		this.context.checking(new Expectations() {
			{
				this.one(PayloadManagerImplTest.this.dao).getAllVersions(
						this.with(Expectations.equal(aCard.getPayload())));
			}
		});

		this.manager.getAllVersions(aCard.getPayload());

	}

	/**
	 * Test get version.
	 */
	@Test
	public void testGetVersion() {

		final String testCaseId = "myCaseId";
		final AlphaCard aCard = this.cardManager.createAlphaCard(testCaseId);
		final long version = 1;

		final byte[] content = "payload data1".getBytes();
		final Payload payload = new Payload();
		payload.setPayloadIdentifier(new PayloadIdentifier(1, 1));
		payload.setFilename("filename");
		payload.setMimeType("text/plain");
		payload.setContent(content);
		aCard.setPayload(payload);

		Assert.assertTrue(aCard.getPayload().getPayloadIdentifier()
				.getSequenceNumber() == version);

		this.context.checking(new Expectations() {
			{
				this.one(PayloadManagerImplTest.this.dao).getVersion(
						this.with(Expectations.same(payload
								.getPayloadIdentifier())));
				this.will(Expectations.returnValue(payload));
			}
		});

		final Payload payload2 = this.manager.getVersion(payload
				.getPayloadIdentifier());
		Assert.assertEquals(payload, payload2);
	}

	/**
	 * Test save new payload.
	 */
	@Test
	public void testSaveNewPayload() {
		final String testCaseId = "myCaseId";
		final AlphaCard aCard = this.cardManager.createAlphaCard(testCaseId);
		final byte[] content = "payload data1".getBytes();

		this.payload2.setFilename("filename");
		this.payload2.setMimeType("text/plain");
		this.payload2.setContent(content);

		final AlphaCardManager cardManager = new AlphaCardManager() {

			public List<AlphaCard> search(final String searchTerm,
					final Class clazz) {
				return null;
			}

			public AlphaCard save(final AlphaCard object) {
				return aCard;
			}

			public void remove(final AlphaCardIdentifier id) {

			}

			public List<AlphaCard> getAll() {
				return null;
			}

			public AlphaCard get(final AlphaCardIdentifier id) {
				return null;
			}

			public boolean exists(final AlphaCardIdentifier id) {
				return true;
			}

			public AlphaCard getVersion(final AlphaCardIdentifier id) {
				return null;
			}

			public List<AlphaCard> getAllVersions(final String caseId) {
				return null;
			}

			public AlphaCard createAlphaCard(final String caseId) {
				return null;
			}

			public List<AlphaCard> listAlphaCardsByCriterion(
					final Criterion[] criterions) {
				return null;
			}

			public List<AlphaCard> listAlphaCardsByCriterion(
					final String caseId, final Criterion... criterions) {
				return null;
			}

			public List<AlphaCard> listDashBoardCards(
					final List<AlphaCase> caseList) {
				return null;
			}
		};

		this.manager.setAlphaCardManager(cardManager);

		this.context.checking(new Expectations() {
			{
				this.one(PayloadManagerImplTest.this.dao).save(
						this.with(Expectations
								.equal(PayloadManagerImplTest.this.payload2)));
				this.will(Expectations
						.returnValue(PayloadManagerImplTest.this.payload2));
			}
		});

		this.manager.saveNewPayload(this.payload2, aCard);
	}
}
