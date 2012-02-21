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

import org.appfuse.service.GenericManager;
import org.aspectj.lang.annotation.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import alpha.portal.model.Adornment;
import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCardIdentifier;
import alpha.portal.service.AlphaCardManager;
import alpha.portal.service.ContributorRoleManager;
import alpha.portal.service.UserExtensionManager;

/**
 * The Class AdornmentFormControllerTest.
 */
public class AdornmentFormControllerTest {

	/** The Constant CASE_ID. */
	private static final String CASE_ID = "550e4713-e22b-11d4-a716-446655440002";

	/** The Constant CARD_ID. */
	private static final String CARD_ID = "440e4816-e01b-74d4-a716-449955440097";

	/** The alpha card manager. */
	@Autowired
	private AlphaCardManager alphaCardManager;

	/** The contributor role manager. */
	@Autowired
	private ContributorRoleManager contributorRoleManager;

	/** The adornment manager. */
	@Autowired
	private GenericManager<Adornment, Long> adornmentManager;

	/** The user extension manager. */
	@Autowired
	private UserExtensionManager userExtensionManager;

	/** The alpha card. */
	private AlphaCard alphaCard;

	/** The adornment. */
	private Adornment adornment;

	/**
	 * Sets the up.
	 */
	@Before(value = "")
	public void setUp() {
		final AlphaCardIdentifier identifier = new AlphaCardIdentifier(
				AdornmentFormControllerTest.CASE_ID,
				AdornmentFormControllerTest.CARD_ID);
		this.alphaCard = new AlphaCard();
		this.alphaCard.setAlphaCardIdentifier(identifier);
	}

	/**
	 * Test setup adornment types.
	 */
	@Test
	public void testSetupAdornmentTypes() {

	}

	/**
	 * Test show form.
	 */
	@Test
	public void testShowForm() {

	}

	/**
	 * On submit form.
	 */
	@Test
	public void onSubmitForm() {

	}
}
