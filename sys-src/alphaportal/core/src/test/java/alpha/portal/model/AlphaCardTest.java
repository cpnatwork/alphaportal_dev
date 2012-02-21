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

import org.junit.Assert;
import org.junit.Test;

/**
 * The Class AlphaCardTest.
 */
public class AlphaCardTest {

	/**
	 * Test alpha card alpha card descriptor null exception.
	 */
	@Test(expected = IllegalStateException.class)
	public void testAlphaCardAlphaCardDescriptorNullException() {
		final AlphaCardDescriptor testDescriptor = new AlphaCardDescriptor();
		testDescriptor.setAlphaCardIdentifier(null);
		@SuppressWarnings("unused")
		final AlphaCard testAlphaCard = new AlphaCard(testDescriptor);
	}

	/**
	 * Test alpha card alpha card descriptor card id empty exception.
	 */
	@Test(expected = IllegalStateException.class)
	public void testAlphaCardAlphaCardDescriptorCardIdEmptyException() {
		final AlphaCardDescriptor testDescriptor = new AlphaCardDescriptor();
		testDescriptor.setAlphaCardIdentifier(new AlphaCardIdentifier("test",
				""));
		@SuppressWarnings("unused")
		final AlphaCard testAlphaCard = new AlphaCard(testDescriptor);
	}

	/**
	 * Test alpha card alpha card descriptor case id empty exception.
	 */
	@Test(expected = IllegalStateException.class)
	public void testAlphaCardAlphaCardDescriptorCaseIdEmptyException() {
		final AlphaCardDescriptor testDescriptor = new AlphaCardDescriptor();
		testDescriptor.setAlphaCardIdentifier(new AlphaCardIdentifier("",
				"test"));
		@SuppressWarnings("unused")
		final AlphaCard testAlphaCard = new AlphaCard(testDescriptor);
	}

	/**
	 * Test alpha card alpha card descriptor.
	 */
	@Test
	public void testAlphaCardAlphaCardDescriptor() {
		final String testCaseId = "myTestCaseId";
		final String testCardId = "myTestCardId";

		final AlphaCardDescriptor testDescriptor = new AlphaCardDescriptor();
		testDescriptor.setAlphaCardIdentifier(new AlphaCardIdentifier(
				testCaseId, testCardId));

		final AlphaCard testAlphaCard = new AlphaCard(testDescriptor);

		Assert.assertTrue(testAlphaCard.getAlphaCardDescriptor().equals(
				testDescriptor));
	}

	/**
	 * Test alpha card alpha case exception.
	 */
	@Test(expected = IllegalStateException.class)
	public void testAlphaCardAlphaCaseException() {
		final AlphaCase testAlphaCase = new AlphaCase();
		@SuppressWarnings("unused")
		final AlphaCard testAlphaCard = new AlphaCard(testAlphaCase);
	}

	/**
	 * Test set alpha card identifier.
	 */
	@Test
	public void testSetAlphaCardIdentifier() {
		final String testCaseId = "myTestCaseId";
		final String testCardId = "myTestCardId";

		final AlphaCardDescriptor testDescriptor = new AlphaCardDescriptor();
		final AlphaCardIdentifier testAlphaCardIdentifier = new AlphaCardIdentifier(
				testCaseId, testCardId);
		testDescriptor.setAlphaCardIdentifier(testAlphaCardIdentifier);

		final AlphaCard testAlphaCard = new AlphaCard();
		testAlphaCard.setAlphaCardIdentifier(testAlphaCardIdentifier);

		Assert.assertTrue(testAlphaCard.getAlphaCardIdentifier().equals(
				testAlphaCardIdentifier));
	}

	/**
	 * Test set alpha card descriptor.
	 */
	@Test
	public void testSetAlphaCardDescriptor() {
		final String testCaseId = "myTestCaseId";
		final String testCardId = "myTestCardId";

		final AlphaCard testAlphaCard = new AlphaCard("myTestCaseId");

		final AlphaCardDescriptor testDescriptor = new AlphaCardDescriptor();
		final AlphaCardIdentifier testAlphaCardIdentifier = new AlphaCardIdentifier(
				testCaseId, testCardId);
		testDescriptor.setAlphaCardIdentifier(testAlphaCardIdentifier);

		testAlphaCard.setAlphaCardIdentifier(null);
		testAlphaCard.setAlphaCardDescriptor(testDescriptor);

		Assert.assertTrue(testAlphaCard.getAlphaCardIdentifier().equals(
				testDescriptor.getAlphaCardIdentifier()));
	}

	/**
	 * Test set alpha card descriptor second condition.
	 */
	@Test
	public void testSetAlphaCardDescriptorSecondCondition() {
		final String testCaseId = "myTestCaseId";
		final String testCardId = "myTestCardId";

		final AlphaCard testAlphaCard = new AlphaCard(testCaseId);

		final AlphaCardDescriptor testDescriptor = new AlphaCardDescriptor();
		final AlphaCardIdentifier testAlphaCardIdentifier = new AlphaCardIdentifier(
				testCaseId, testCardId);
		testDescriptor.setAlphaCardIdentifier(testAlphaCardIdentifier);

		testAlphaCard.setAlphaCardIdentifier(null);
		testAlphaCard.getAlphaCardDescriptor().setAlphaCardIdentifier(null);
		testAlphaCard.setAlphaCardDescriptor(testDescriptor);

		Assert.assertTrue(testAlphaCard.getAlphaCardIdentifier().equals(
				testDescriptor.getAlphaCardIdentifier()));

		Assert.assertFalse(!testAlphaCard.getAlphaCardIdentifier().equals(
				testDescriptor.getAlphaCardIdentifier()));
	}

	/**
	 * Test equals object.
	 */
	@Test
	public void testEqualsObject() {
		final AlphaCard testAlphaCard = new AlphaCard();
		Assert.assertFalse(testAlphaCard.equals(new AlphaCase()));
	}

	/**
	 * Test to string.
	 */
	@Test
	public void testToString() {
		final AlphaCard testAlphaCard = new AlphaCard();
		testAlphaCard.setAlphaCardDescriptor(new AlphaCardDescriptor());
		testAlphaCard.setAlphaCardIdentifier(new AlphaCardIdentifier());
		testAlphaCard.setAlphaCase(new AlphaCase());
		testAlphaCard.setPayload(new Payload());

		Assert.assertTrue(testAlphaCard.toString().length() > 0);
	}

	/**
	 * Test get alpha case.
	 */
	@Test
	public void testGetAlphaCase() {
		final AlphaCard testAlphaCard = new AlphaCard();
		final AlphaCase testAlphaCase = new AlphaCase();

		testAlphaCard.setAlphaCase(testAlphaCase);

		Assert.assertTrue(testAlphaCard.getAlphaCase().equals(testAlphaCase));

		final AlphaCard testAlphaCard2 = new AlphaCard();
		Assert.assertFalse(testAlphaCard2.getAlphaCase() != null);
	}

	/**
	 * Test get alpha card descriptor.
	 */
	@Test
	public void testGetAlphaCardDescriptor() {
		final AlphaCard testAlphaCard = new AlphaCard();
		final AlphaCardDescriptor testAlphaCardDescriptor = new AlphaCardDescriptor();

		testAlphaCard.setAlphaCardDescriptor(testAlphaCardDescriptor);

		Assert.assertTrue(testAlphaCard.getAlphaCardDescriptor().equals(
				testAlphaCardDescriptor));
	}

}
