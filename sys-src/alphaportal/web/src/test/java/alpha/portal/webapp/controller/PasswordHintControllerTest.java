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

import org.junit.Assert;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mock.web.MockHttpServletRequest;
import org.subethamail.wiser.Wiser;

/**
 * The Class PasswordHintControllerTest.
 */
public class PasswordHintControllerTest extends BaseControllerTestCase {

	/** The c. */
	@Autowired
	private final PasswordHintController c = null;

	/**
	 * Test execute.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	@Test
	public void testExecute() throws Exception {
		final MockHttpServletRequest request = this
				.newGet("/passwordHint.html");
		request.addParameter("username", "user");

		// start SMTP Server
		final Wiser wiser = new Wiser();
		wiser.setPort(this.getSmtpPort());
		wiser.start();

		this.c.handleRequest(request);

		// verify an account information e-mail was sent
		wiser.stop();
		Assert.assertTrue(wiser.getMessages().size() == 1);

		// verify that success messages are in the session
		Assert.assertNotNull(request.getSession().getAttribute(
				BaseFormController.MESSAGES_KEY));
	}
}
