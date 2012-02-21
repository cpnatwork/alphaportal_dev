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
package alpha.portal.model;

import org.appfuse.model.User;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * The Class AlphaCaseTest.
 */
public class AlphaCaseTest {

	/** The alpha case. */
	AlphaCase alphaCase = new AlphaCase();

	/** The test id. */
	private final String testId = "4711";

	/** The name. */
	private final String name = "testCase";

	/** The alpha card1. */
	AlphaCard alphaCard1 = new AlphaCard();

	/** The alpha card2. */
	AlphaCard alphaCard2 = new AlphaCard();

	/** The alpha card3. */
	AlphaCard alphaCard3 = new AlphaCard();

	/** The testidentifier1. */
	private final AlphaCardIdentifier testidentifier1 = new AlphaCardIdentifier(
			this.testId, "4811");

	/** The testidentifier2. */
	private final AlphaCardIdentifier testidentifier2 = new AlphaCardIdentifier(
			this.testId, "4812");

	/** The testidentifier3. */
	private final AlphaCardIdentifier testidentifier3 = new AlphaCardIdentifier(
			this.testId, "4813");

	/** The participant. */
	User participant = new User();

	/**
	 * Sets the up.
	 */
	@Before
	public void setUp() {
		this.alphaCase.setCaseId(this.testId);
		this.alphaCase.setName(this.name);
		this.alphaCard1.setAlphaCardIdentifier(this.testidentifier1);
		this.alphaCard2.setAlphaCardIdentifier(this.testidentifier2);
		this.alphaCard3.setAlphaCardIdentifier(this.testidentifier3);
		this.participant.setId(5L);
	}

	/**
	 * Test get case id.
	 */
	@Test
	public void testGetCaseId() {
		Assert.assertTrue(this.alphaCase.getCaseId().equals(this.testId));
	}

	/**
	 * Test set case id.
	 */
	@Test
	public void testSetCaseId() {
		this.alphaCase.setCaseId("4911");
		Assert.assertTrue(this.alphaCase.getCaseId() == "4911");
	}

	/**
	 * Test get name.
	 */
	@Test
	public void testGetName() {
		Assert.assertTrue(this.alphaCase.getName().equals(this.name));
	}

	/**
	 * Test set name.
	 */
	@Test
	public void testSetName() {
		this.alphaCase.setName("Hugo");
		Assert.assertTrue(this.alphaCase.getName() == "Hugo");
	}

	/**
	 * Test participants.
	 */
	@Test
	public void testParticipants() {
		Assert.assertEquals(0, this.alphaCase.getListOfParticipants().size());

		this.alphaCase.addParticipant(this.participant);
		Assert.assertEquals(1, this.alphaCase.getListOfParticipants().size());

		this.alphaCase.getListOfParticipants().contains(this.participant);

		this.alphaCase.removeParticipant(this.participant);
		Assert.assertEquals(0, this.alphaCase.getListOfParticipants().size());

		Assert.assertTrue(this.alphaCase.getParticipantsCRA() != null);
	}

	/**
	 * Test alpha card.
	 */
	@Test
	public void testAlphaCard() {
		Assert.assertEquals(0, this.alphaCase.getAlphaCards().size());

		this.alphaCase.addAlphaCard(this.alphaCard1);
		Assert.assertEquals(1, this.alphaCase.getAlphaCards().size());

		this.alphaCase.getAlphaCards().contains(this.alphaCard1);
	}

	/**
	 * Test add alpha card.
	 */
	@Test
	public void testAddAlphaCard() {

		this.alphaCase.addAlphaCard(this.alphaCard1);

		this.alphaCase.addAlphaCard(this.alphaCard2);

		this.alphaCase.addAlphaCard(this.alphaCard3, 0);

		Assert.assertTrue(this.alphaCase.getAlphaCards().get(0)
				.equals(this.alphaCard3));
		Assert.assertTrue(this.alphaCase.getAlphaCards().get(1)
				.equals(this.alphaCard1));
		Assert.assertTrue(this.alphaCase.getAlphaCards().get(2)
				.equals(this.alphaCard2));

	}

	/**
	 * Test move alpha card.
	 */
	@Test
	public void testMoveAlphaCard() {

		this.alphaCase.addAlphaCard(this.alphaCard1);

		this.alphaCase.addAlphaCard(this.alphaCard2);

		this.alphaCase.addAlphaCard(this.alphaCard3);

		this.alphaCase.moveAlphaCard(this.alphaCard1, 1);

		Assert.assertTrue(this.alphaCase.getAlphaCards().get(1)
				.equals(this.alphaCard1));
		Assert.assertTrue(this.alphaCase.getAlphaCards().get(0)
				.equals(this.alphaCard2));
		Assert.assertTrue(this.alphaCase.getAlphaCards().get(2)
				.equals(this.alphaCard3));

	}

	/**
	 * Test remove alpha card.
	 */
	@Test
	public void testRemoveAlphaCard() {

		this.alphaCase.addAlphaCard(this.alphaCard1);

		Assert.assertTrue(this.alphaCase.getAlphaCards().get(0)
				.equals(this.alphaCard1));
		Assert.assertFalse(this.alphaCase.getAlphaCards().isEmpty());

		this.alphaCase.removeAlphaCard(this.alphaCard1);

		Assert.assertTrue(this.alphaCase.getAlphaCards().isEmpty());

		Assert.assertTrue(this.alphaCase.getAlphaCasePSA() != null);
	}

	/**
	 * Test equals object.
	 */
	@Test
	public void testEqualsObject() {
		final AlphaCase alphaCase2 = new AlphaCase();
		alphaCase2.setCaseId(this.testId);
		alphaCase2.setName(this.name);

		Assert.assertTrue(this.alphaCase.equals(alphaCase2));

		Assert.assertFalse(this.alphaCase.equals(new AlphaCard()));
	}

	/**
	 * Test to string.
	 */
	@Test
	public void testToString() {
		this.alphaCase.toString();
	}
}
