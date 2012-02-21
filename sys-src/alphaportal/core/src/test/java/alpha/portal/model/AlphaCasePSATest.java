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

import java.util.LinkedList;
import java.util.List;

import org.junit.Assert;
import org.junit.Test;

/**
 * The Class AlphaCasePSATest.
 */
public class AlphaCasePSATest {

	/**
	 * Test basics.
	 */
	@Test
	public void testBasics() {
		final AlphaCasePSA psa = new AlphaCasePSA();
		Assert.assertNotNull(psa.getListOfAlphaCards());
		Assert.assertEquals(0, psa.getListOfAlphaCards().size());

		final int hash = psa.hashCode();
		Assert.assertTrue(psa.equals(new AlphaCasePSA()));
		Assert.assertEquals(hash, (new AlphaCasePSA()).hashCode());

		final String to = psa.toString();

		final AlphaCard aCard = new AlphaCard();
		psa.addAlphaCard(aCard, 0);
		Assert.assertEquals(1, psa.getListOfAlphaCards().size());
		Assert.assertTrue(psa.getListOfAlphaCards().contains(aCard));

		final AlphaCasePSA psa2 = new AlphaCasePSA();
		psa2.addAlphaCard(aCard, 0);

		Assert.assertEquals(psa, psa2);

		psa.removeAlphaCard(aCard);
		Assert.assertEquals(0, psa.getListOfAlphaCards().size());
		Assert.assertEquals(to, psa.toString());
		Assert.assertEquals(hash, psa.hashCode());
	}

	/**
	 * Test clear alpha cards.
	 */
	@Test
	public void testClearAlphaCards() {

		final AlphaCasePSA psa = new AlphaCasePSA();
		psa.addAlphaCard(new AlphaCard(), 0);
		psa.clearAlphaCards();
		Assert.assertTrue(psa.getListOfAlphaCards().size() == 0);

	}

	/**
	 * Test set alpha cards.
	 */
	@Test
	public void testSetAlphaCards() {

		final AlphaCasePSA psa = new AlphaCasePSA();
		final List<AlphaCard> cards = new LinkedList<AlphaCard>();
		cards.add(new AlphaCard());
		psa.setAlphaCards(cards);
		Assert.assertEquals(cards, psa.getListOfAlphaCards());

	}

	/**
	 * Test get alpha card by card id.
	 */
	@Test
	public void testGetAlphaCardByCardId() {
		final AlphaCasePSA psa = new AlphaCasePSA();
		final AlphaCard c = new AlphaCard();
		c.getAlphaCardIdentifier().setCardId("123");
		psa.addAlphaCard(c, 0);
		Assert.assertEquals(c, psa.getAlphaCardByCardId("123"));
	}

	/**
	 * Test add alpha card.
	 */
	@Test
	public void testAddAlphaCard() {

		final AlphaCasePSA psa = new AlphaCasePSA();
		final AlphaCard aCard1 = new AlphaCard();
		final AlphaCard aCard2 = new AlphaCard();
		final AlphaCard aCard3 = new AlphaCard();

		final AlphaCasePSA psa2 = psa;

		Assert.assertFalse(psa.equals(aCard1));
		Assert.assertTrue(psa.equals(psa2));

		Assert.assertTrue(psa.addAlphaCard(aCard1, 0));
		Assert.assertTrue(psa.addAlphaCard(aCard2, 1));
		Assert.assertTrue(psa.addAlphaCard(aCard3, 2));
		Assert.assertTrue(psa.getListOfAlphaCards().size() == 3);
		Assert.assertTrue(psa.moveAlphaCard(aCard1, 1));
		Assert.assertEquals(aCard2, psa.getListOfAlphaCards().get(0));
		Assert.assertEquals(aCard1, psa.getListOfAlphaCards().get(1));
		Assert.assertEquals(aCard3, psa.getListOfAlphaCards().get(2));
	}
}
