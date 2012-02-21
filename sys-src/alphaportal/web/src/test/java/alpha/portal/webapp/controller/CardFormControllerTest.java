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
package alpha.portal.webapp.controller;

import java.io.IOException;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.validation.BindingResult;
import org.springframework.validation.DataBinder;

import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCardIdentifier;
import alpha.portal.service.AlphaCardManager;

/**
 * The Class CardFormControllerTest.
 */
public class CardFormControllerTest extends BaseControllerTestCase {

	/** The Constant CASE_ID. */
	private static final String CASE_ID = "550e4713-e22b-11d4-a716-446655440000";

	/** The Constant CARD_ID. */
	private static final String CARD_ID = "440e4816-e01b-74d4-a716-449955440092";

	/** The c. */
	@Autowired
	private CardFormController c;

	/** The alpha card manager. */
	@Autowired
	private AlphaCardManager alphaCardManager;

	/** The alpha card. */
	private AlphaCard alphaCard;

	/**
	 * Sets the up.
	 */
	@Before
	public void setUp() {
		final AlphaCardIdentifier identifier = new AlphaCardIdentifier(
				CardFormControllerTest.CASE_ID, CardFormControllerTest.CARD_ID);
		this.alphaCard = new AlphaCard();
		this.alphaCard.setAlphaCardIdentifier(identifier);
	}

	/**
	 * Test show form.
	 */
	@Test
	public void testShowForm() {
		final MockHttpServletRequest request = this.newGet("/caseform");
		request.addParameter("case", CardFormControllerTest.CASE_ID);
		request.addParameter("card", CardFormControllerTest.CARD_ID);
		this.c.showForm(request);
	}

	/**
	 * Test on submit.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	@Test
	public void testOnSubmit() throws Exception {
		final MockHttpServletRequest request = this.newPost("/cardform");
		final String view = this.c.onSubmit(this.alphaCard, null, request,
				new MockHttpServletResponse());
		Assert.assertEquals("redirect:/caseform?caseId="
				+ CardFormControllerTest.CASE_ID, view);
	}

	/**
	 * Test save card.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	@Test
	public void testSaveCard() throws Exception {
		final MockHttpServletRequest request = this.newPost("/cardform");
		request.setRemoteUser("admin");
		final BindingResult errors = new DataBinder(this.alphaCard)
				.getBindingResult();
		final String view = this.c.saveCard(this.alphaCard, errors, request,
				new MockHttpServletResponse());
		Assert.assertEquals("redirect:/caseform?activeCardId="
				+ CardFormControllerTest.CARD_ID + "&caseId="
				+ CardFormControllerTest.CASE_ID, view);

		final MockHttpServletRequest request2 = this.newPost("/cardform");
		request2.setRemoteUser("admin");
		final AlphaCardIdentifier identifier = new AlphaCardIdentifier(
				CardFormControllerTest.CASE_ID);
		final AlphaCard alphaCard2 = new AlphaCard();
		alphaCard2.setAlphaCardIdentifier(identifier);

		this.c.saveCard(alphaCard2, null, request2,
				new MockHttpServletResponse());
		// unable to verify cardId since the controller doesn't return the saved
		// card to us :/
	}

	/**
	 * Test cancel card.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	@Test
	public void testCancelCard() throws Exception {
		final MockHttpServletRequest request = this.newPost("/cardform");
		final String view = this.c.cancelCard(this.alphaCard, null, request);
		Assert.assertEquals("redirect:/caseform?caseId="
				+ CardFormControllerTest.CASE_ID, view);
	}

	/**
	 * Test assign card.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	@Test
	public void testAssignCard() throws Exception {
		final MockHttpServletRequest request = this.newPost("/cardform");
		request.setRemoteUser("admin");
		final String view = this.c.assignCard(this.alphaCard, null, request);
		Assert.assertEquals("redirect:/caseform?caseId="
				+ CardFormControllerTest.CASE_ID + "&activeCardId="
				+ CardFormControllerTest.CARD_ID, view);
	}

	/**
	 * Test unassign card.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	@Test
	public void testUnassignCard() throws Exception {
		final MockHttpServletRequest request = this.newPost("/cardform");
		final String view = this.c.unassignCard(this.alphaCard, null, request);
		Assert.assertEquals("redirect:/caseform?caseId="
				+ CardFormControllerTest.CASE_ID + "&activeCardId="
				+ CardFormControllerTest.CARD_ID, view);
	}

	/**
	 * Test get payload.
	 * 
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 */
	@Test
	public void testGetPayload() throws IOException {
		final String view = this.c.getPayload(this.alphaCard,
				new MockHttpServletResponse());
		Assert.assertEquals("redirect:/caseform?caseId="
				+ CardFormControllerTest.CASE_ID + "&activeCardId="
				+ CardFormControllerTest.CARD_ID, view);
	}

	/**
	 * Test delete payload.
	 * 
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 */
	@Test
	public void testDeletePayload() throws IOException {
		final MockHttpServletRequest request = this.newPost("/cardform");
		final String view = this.c.deletePayload(this.alphaCard, request);
		Assert.assertEquals("redirect:/caseform?caseId="
				+ CardFormControllerTest.CASE_ID + "&activeCardId="
				+ CardFormControllerTest.CARD_ID, view);
	}
}