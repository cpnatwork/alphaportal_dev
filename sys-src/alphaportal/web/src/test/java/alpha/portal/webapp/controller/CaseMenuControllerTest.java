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

import java.util.List;

import org.appfuse.model.User;
import org.appfuse.service.UserManager;
import org.junit.Assert;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.web.servlet.ModelAndView;

import alpha.portal.model.AlphaCase;
import alpha.portal.service.CaseManager;

/**
 * The Class CaseMenuControllerTest.
 */

public class CaseMenuControllerTest extends BaseControllerTestCase {

	/** The ctrl. */
	@Autowired
	private CaseMenuController ctrl;

	/** The user manager. */
	@Autowired
	private UserManager userManager;

	/** The case manager. */
	@Autowired
	private CaseManager caseManager;

	/**
	 * Test handle request.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	@SuppressWarnings("unchecked")
	@Test
	public void testHandleRequest() throws Exception {
		final MockHttpServletRequest request = this.newGet("/caseMenu");

		User u = new User("ichbineintollertesterdenesnochnichtgibt");
		u.setEmail("l@m.d");
		u.setFirstName("l");
		u.setLastName("m");
		u.setPassword("123");
		u = this.userManager.save(u);

		AlphaCase c = new AlphaCase();
		c.setName("blablabla");
		c.addParticipant(u);
		c = this.caseManager.save(c);

		request.setRemoteUser("ichbineintollertesterdenesnochnichtgibt");

		ModelAndView result = this.ctrl.handleRequest(request);

		List<AlphaCase> lCases = (List<AlphaCase>) result.getModel().get(
				"caseList");
		Assert.assertEquals(1, lCases.size());
		final AlphaCase c2 = lCases.get(0);
		Assert.assertEquals(c, c2);

		this.caseManager.remove(c.getCaseId());
		result = this.ctrl.handleRequest(request);
		lCases = (List<AlphaCase>) result.getModel().get("caseList");
		Assert.assertTrue((lCases == null) || lCases.isEmpty());

		this.userManager.remove(u.getId());
	}

}
