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
import org.appfuse.model.User;
import org.junit.Assert;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCardDescriptor;
import alpha.portal.model.AlphaCardIdentifier;
import alpha.portal.model.AlphaCase;

/**
 * The Class CaseDaoTest.
 */
public class AlphaCaseDaoTest extends BaseDaoTestCase {

	/** The case dao. */
	@Autowired
	private AlphaCaseDao caseDao;

	/** The user dao. */
	@Autowired
	private UserExtensionDao userDao;

	/**
	 * Test cards.
	 */
	@Test
	public void testCards() {
		final AlphaCase testCase = this.caseDao
				.get("550e4713-e22b-11d4-a716-446655440002");
		final List<AlphaCard> cards = testCase.getAlphaCards();
		Assert.assertTrue(cards.size() > 0);

		for (final AlphaCard c : cards) {
			if (c.getAlphaCardIdentifier().getCardId() == "440e4816-e01b-74d4-a716-449955440097") {
				final AlphaCardIdentifier i = c.getAlphaCardIdentifier();
				final AlphaCardDescriptor d = c.getAlphaCardDescriptor();
				final AlphaCardIdentifier ci = d.getAlphaCardIdentifier();

				// based on test/resources/sample-data.xml
				Assert.assertEquals("roflcopter", d.getTitle());
				Assert.assertEquals(-2L, d.getContributor().longValue());
				Assert.assertEquals(i.getCaseId(),
						"550e4713-e22b-11d4-a716-446655440002");
				Assert.assertEquals(ci.getCaseId(),
						"550e4713-e22b-11d4-a716-446655440002");
				Assert.assertEquals(i.getCardId(),
						"440e4816-e01b-74d4-a716-449955440097");
				Assert.assertEquals(ci.getCardId(),
						"440e4816-e01b-74d4-a716-449955440097");
			}
		}
	}

	/**
	 * Test find case by name.
	 * 
	 */
	@Test
	public void testFindCaseByName() {
		final List<AlphaCase> testCase = this.caseDao.findByName("Akte Susi");
		Assert.assertTrue(testCase.size() > 0);
	}

	/**
	 * Test add and remove case.
	 * 
	 */
	@Test
	public void testAddAndRemoveCase() {

		AlphaCase testCase = new AlphaCase();

		testCase.setName("Test-Akte");
		testCase = this.caseDao.save(testCase);
		this.flush();

		final AlphaCase savedCase = this.caseDao.get(testCase.getCaseId());
		Assert.assertEquals(testCase, savedCase);

		this.caseDao.remove(testCase.getCaseId());
		this.flush();

		Assert.assertFalse(this.caseDao.exists(testCase.getCaseId()));
	}

	/**
	 * Test remove case and alpha cards.
	 */
	@Test
	public void testRemoveCaseAndAlphaCards() {
		AlphaCase alphacase = new AlphaCase();

		alphacase.setName("Test");
		alphacase = this.caseDao.save(alphacase);
		this.flush();

		final AlphaCard alphacard = new AlphaCard(alphacase);
		alphacard.getAlphaCardIdentifier().setCardId("123");
		alphacard.getAlphaCardIdentifier().setSequenceNumber(1L);
		alphacard.getAlphaCardDescriptor().setAlphaCardIdentifier(
				alphacard.getAlphaCardIdentifier());

		alphacase.addAlphaCard(alphacard);
		alphacase = this.caseDao.save(alphacase);
		this.flush();

		this.caseDao.remove(alphacase.getCaseId());
		this.flush();

		final AlphaCardIdentifier id = alphacard.getAlphaCardIdentifier();

		Assert.assertFalse(this.caseDao.exists(alphacase.getCaseId()));
		Assert.assertFalse(this.caseDao.exists(id.getCardId()));
	}

	/**
	 * Test participants.
	 * 
	 */
	@Test
	public void testParticipants() {
		AlphaCase testCase = new AlphaCase();
		final User user = this.userDao.get(-2l).getUser();

		testCase.setName("Irgendwas");
		testCase.addParticipant(user);

		testCase = this.caseDao.save(testCase);
		this.flush();

		final List<AlphaCase> dbCases = this.caseDao.findByParticipant(user);

		Assert.assertEquals(10, dbCases.size());
		Assert.assertTrue(dbCases.contains(testCase));

		// test if user is present
		testCase = this.caseDao.get(testCase.getCaseId());
		Assert.assertTrue(testCase.getListOfParticipants().contains(user));

		// remove user
		testCase.removeParticipant(user);
		testCase = this.caseDao.save(testCase);
		this.flush();

		// test if user is not present
		testCase = this.caseDao.get(testCase.getCaseId());
		Assert.assertTrue(!testCase.getListOfParticipants().contains(user));

		this.caseDao.remove(testCase.getCaseId());
	}
}
