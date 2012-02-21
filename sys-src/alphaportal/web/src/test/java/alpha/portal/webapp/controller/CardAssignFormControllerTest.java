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

import java.util.LinkedList;
import java.util.List;

import org.appfuse.service.GenericManager;
import org.junit.Assert;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.ui.ExtendedModelMap;
import org.springframework.ui.Model;

import alpha.portal.model.ContributorRequest;
import alpha.portal.model.UserExtension;
import alpha.portal.service.UserExtensionManager;

/**
 * The Class CardAssignFormControllerTest.
 */
public class CardAssignFormControllerTest extends BaseControllerTestCase {

	/** The form. */
	@Autowired
	private CardAssignFormController form;

	/** The contr req manager. */
	@Autowired
	private GenericManager<ContributorRequest, Long> contrReqManager;

	/** The user extension manager. */
	@Autowired
	private UserExtensionManager userExtensionManager;

	/**
	 * Test show form.
	 */
	@Test
	public void testShowForm() {
		final MockHttpServletRequest request = this.newGet("/cardassignform");
		request.setRemoteUser("admin");
		request.addParameter("card", "");
		request.addParameter("case", "");

		Model m = new ExtendedModelMap();
		this.form.showForm(request, m);
		Assert.assertFalse(m.containsAttribute("users"));

		request.setParameter("card", "440e4816-e01b-74d4-a716-449955440092");
		m = new ExtendedModelMap();
		this.form.showForm(request, m);
		Assert.assertFalse(m.containsAttribute("users"));

		request.setParameter("case", "atjaerhe");
		m = new ExtendedModelMap();
		this.form.showForm(request, m);
		Assert.assertFalse(m.containsAttribute("users"));

		request.setParameter("case", "550e4713-e22b-11d4-a716-446655440000");
		m = new ExtendedModelMap();
		this.form.showForm(request, m);

		final List<UserExtension> l = new LinkedList<UserExtension>();
		l.add(this.userExtensionManager.get(-5L));
		l.add(this.userExtensionManager.get(-4L));
		l.add(this.userExtensionManager.get(-2L));

		// FIXME the sample-data.xml change concerning the contributorrole
		// values has broken this
		// ModelAndViewAssert.assertModelAttributeValue(new ModelAndView("",
		// m.asMap()), "users", l);
	}

	/**
	 * Test on submit.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	@Test
	public void testOnSubmit() throws Exception {
		final String cardId = "440e4816-e01b-74d4-a716-449955440092";
		final String caseId = "550e4713-e22b-11d4-a716-446655440000";

		final MockHttpServletRequest request = this.newPost("/cardassignform");
		request.setRemoteUser("admin");
		request.setParameter("card", cardId);
		request.setParameter("case", caseId);
		MockHttpServletResponse response = new MockHttpServletResponse();

		request.setParameter("cancel", "");
		this.form.onSubmit(request, response);

		response = new MockHttpServletResponse();
		request.removeParameter("cancel");
		request.setParameter("user", "-1");
		this.form.onSubmit(request, response);

		boolean found = false;
		for (final ContributorRequest r : this.contrReqManager.getAll()) {
			if (r.getAcceptingUser().getId().equals(-1L)
					&& r.getAlphaCard().getAlphaCardIdentifier().getCardId()
							.equals(cardId)
					&& r.getRequestingUser().getId().equals(-2L)) {
				found = true;
				break;
			}
		}
		Assert.assertTrue(found);
	}
}
