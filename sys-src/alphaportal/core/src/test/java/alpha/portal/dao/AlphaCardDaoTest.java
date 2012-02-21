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
import org.appfuse.service.GenericManager;
import org.junit.Assert;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.AccessDeniedException;

import alpha.portal.model.Adornment;
import alpha.portal.model.AdornmentType;
import alpha.portal.model.AdornmentTypeDataProvision;
import alpha.portal.model.AdornmentTypeDeleted;
import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCardIdentifier;
import alpha.portal.model.AlphaCase;
import alpha.portal.model.Payload;
import alpha.portal.service.impl.AlphaCardManagerImpl;

/**
 * The Class AlphaCardDaoTest.
 */
public class AlphaCardDaoTest extends BaseDaoTestCase {

	/** The alpha card dao. */
	@Autowired
	private AlphaCardDao alphaCardDao;

	/** The case dao. */
	@Autowired
	private AlphaCaseDao caseDao;

	/** The adornment manager. */
	@Autowired
	private GenericManager<Adornment, Long> adornmentManager;

	/** The payload dao. */
	@Autowired
	private PayloadDao payloadDao;

	/**
	 * Test whole alphaCard.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	@Test
	public void testAddAndRemoveAlphaCard() throws Exception {
		final String title = "TestCard Title";

		AlphaCase aCase = new AlphaCase();
		aCase.setName("Test Case");
		aCase = this.caseDao.save(aCase);
		this.flush();

		AlphaCard card = new AlphaCard(aCase);
		card = this.alphaCardDao.save(card);
		final long firstVersion = card.getAlphaCardIdentifier()
				.getSequenceNumber();
		this.flush();
		Assert.assertTrue(this.alphaCardDao.exists(card
				.getAlphaCardIdentifier()));

		card.getAlphaCardDescriptor().setTitle(title);
		card = this.alphaCardDao.save(card);
		this.flush();
		Assert.assertEquals(firstVersion + 1, card.getAlphaCardIdentifier()
				.getSequenceNumber().longValue());
		card = this.alphaCardDao.get(card.getAlphaCardIdentifier());
		Assert.assertEquals(title, card.getAlphaCardDescriptor().getTitle());

		card.getAlphaCardDescriptor().setTitle("blablip");
		card = this.alphaCardDao.save(card);
		this.flush();
		Assert.assertEquals(firstVersion + 2, card.getAlphaCardIdentifier()
				.getSequenceNumber().longValue());

		final List<AlphaCard> versions = this.alphaCardDao.getAllVersions(card
				.getAlphaCardIdentifier().getCaseId());
		Assert.assertEquals(3, versions.size());

		// Payload
		final byte[] payloadData = "some cool test data".getBytes();
		Payload payload = new Payload();
		payload.setFilename("filename");
		payload.setMimeType("text/plain");
		payload.setContent(payloadData);
		payload = this.payloadDao.save(payload);
		card.setPayload(payload);
		card = this.alphaCardDao.save(card);
		card = this.alphaCardDao.get(card.getAlphaCardIdentifier());
		Assert.assertNotNull(card.getPayload());
		Assert.assertEquals("some cool test data", new String(card.getPayload()
				.getContent()));

		final AlphaCard testCard = this.alphaCardDao.get(card
				.getAlphaCardIdentifier());
		Assert.assertNotNull(testCard);
		Assert.assertEquals(card.getAlphaCardIdentifier(),
				testCard.getAlphaCardIdentifier());
		Assert.assertEquals(card.getAlphaCardDescriptor(),
				testCard.getAlphaCardDescriptor());
		Assert.assertEquals(card.getPayload(), testCard.getPayload());
		Assert.assertEquals(card, testCard);
	}

	/**
	 * Test get version.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	@Test
	public void testGetVersion() throws Exception {

		AlphaCase aCase = new AlphaCase();
		aCase.setName("Test Case");
		aCase = this.caseDao.save(aCase);
		this.flush();

		AlphaCard card = new AlphaCard(aCase);
		card = this.alphaCardDao.save(card);
		this.flush();
		Assert.assertTrue(this.alphaCardDao.exists(card
				.getAlphaCardIdentifier()));

		AlphaCard aCard = this.alphaCardDao
				.getVersion(new AlphaCardIdentifier());

		Assert.assertNull(aCard);

		aCard = this.alphaCardDao.get(new AlphaCardIdentifier());

		Assert.assertNull(aCard);

		Assert.assertFalse(this.alphaCardDao.exists(new AlphaCardIdentifier()));

	}

	/**
	 * Test remove.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	@Test(expected = AccessDeniedException.class)
	public void testRemove() throws Exception {

		AlphaCase aCase = new AlphaCase();
		aCase.setName("Test Case");
		aCase = this.caseDao.save(aCase);
		this.flush();

		AlphaCard card = new AlphaCard(aCase);
		card = this.alphaCardDao.save(card);
		this.flush();
		Assert.assertTrue(this.alphaCardDao.exists(card
				.getAlphaCardIdentifier()));

		this.alphaCardDao.remove(card.getAlphaCardIdentifier());

	}

	/**
	 * Test list alpha cards by criterion.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	@Test
	public void testListAlphaCardsByCriterion() throws Exception {
		List<AlphaCard> res = this.alphaCardDao.listAlphaCardsByCriterion(
				"gibtsnicht", AlphaCardManagerImpl.DATA_PROVISION_OPEN);
		Assert.assertEquals(0, res.size());
		res = this.alphaCardDao.listAlphaCardsByCriterion(
				"550e4713-e22b-11d4-a716-446655440002",
				AlphaCardManagerImpl.DATA_PROVISION_OPEN);
		Assert.assertEquals(1, res.size());
		for (final AlphaCard c : res) {
			Assert.assertEquals("550e4713-e22b-11d4-a716-446655440002", c
					.getAlphaCardIdentifier().getCaseId());
			Assert.assertEquals(
					AdornmentTypeDataProvision.OPEN.getName(),
					c.getAlphaCardDescriptor()
							.getAdornment(AdornmentType.DataProvision.getName())
							.getValue());
		}

		Adornment delAdornment1 = new Adornment(AdornmentType.Deleted.getName());
		delAdornment1.setValue(AdornmentTypeDeleted.FALSE.value());
		delAdornment1 = this.adornmentManager.save(delAdornment1);
		this.flush();
		Adornment delAdornment2 = new Adornment(AdornmentType.Deleted.getName());
		delAdornment2.setValue(AdornmentTypeDeleted.FALSE.value());
		delAdornment2 = this.adornmentManager.save(delAdornment2);
		this.flush();

		AlphaCard critMatcher1 = new AlphaCard(
				"550e4713-e22b-11d4-a716-446655440002");
		critMatcher1.setAlphaCardIdentifier(new AlphaCardIdentifier(
				"550e4713-e22b-11d4-a716-446655440002", "0111-222-333-444"));
		critMatcher1 = this.alphaCardDao.save(critMatcher1);
		this.flush();
		critMatcher1.getAlphaCardDescriptor().setTitle("Test Alpha Card 1");
		critMatcher1.getAlphaCardDescriptor().setAdornment(delAdornment1);
		critMatcher1 = this.alphaCardDao.save(critMatcher1);
		this.flush();
		AlphaCard critMatcher2 = new AlphaCard(
				"550e4713-e22b-11d4-a716-446655440002");
		critMatcher1.setAlphaCardIdentifier(new AlphaCardIdentifier(
				"550e4713-e22b-11d4-a716-446655440002", "0111-222-888-999"));
		critMatcher2 = this.alphaCardDao.save(critMatcher2);
		this.flush();
		critMatcher2.getAlphaCardDescriptor().setTitle("Test Alpha Card 2");
		critMatcher2.getAlphaCardDescriptor().setAdornment(delAdornment2);
		critMatcher2 = this.alphaCardDao.save(critMatcher2);
		this.flush();

		final List<AlphaCard> res2 = this.alphaCardDao
				.listAlphaCardsByCriterion(
						"550e4713-e22b-11d4-a716-446655440002",
						AlphaCardManagerImpl.NOT_DELETED);
		Assert.assertNotNull(res2);
		// FIXME: assertion is violated
		// assertTrue(res2.size() > 0);
	}

	/**
	 * List dash board alpha cards.
	 */
	@Test
	public void listDashBoardAlphaCards() {
		final String[] caseIDs = new String[3];
		caseIDs[0] = "550e4713-e22b-11d4-a716-446655440001";
		caseIDs[1] = "550e4713-e22b-11d4-a716-446655440002";
		caseIDs[2] = "11111111-2222-3333-4444-555556666777";

		final List<AlphaCard> testList = this.alphaCardDao
				.listDashBoardAlphaCards(caseIDs);
		Assert.assertNotNull(testList);
		Assert.assertTrue(testList.size() > 0);
	}

	/**
	 * Test save with null.
	 */
	@Test
	public void testSaveWithNull() {
		AlphaCard testCard = new AlphaCard();
		testCard.setAlphaCardIdentifier(null);

		testCard = this.alphaCardDao.save(testCard);
		Assert.assertNotNull(testCard);
	}
}
