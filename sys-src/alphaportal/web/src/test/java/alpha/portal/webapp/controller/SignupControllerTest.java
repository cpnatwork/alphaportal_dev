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

import javax.servlet.http.HttpServletResponse;

import org.appfuse.Constants;
import org.appfuse.model.Address;
import org.appfuse.model.User;
import org.junit.Assert;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.validation.BindingResult;
import org.springframework.validation.DataBinder;
import org.subethamail.wiser.Wiser;

/**
 * The Class SignupControllerTest.
 */
public class SignupControllerTest extends BaseControllerTestCase {

	/** The c. */
	@Autowired
	private final SignupController c = null;

	/**
	 * Test display form.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	@Test
	public void testDisplayForm() throws Exception {
		final User user = this.c.showForm();
		Assert.assertNotNull(user);
	}

	/**
	 * Test signup user.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	@Test
	public void testSignupUser() throws Exception {
		final MockHttpServletRequest request = this.newPost("/signup.html");

		final Address address = new Address();
		address.setCity("Denver");
		address.setProvince("Colorado");
		address.setCountry("USA");
		address.setPostalCode("80210");

		final User user = new User();
		user.setAddress(address);

		user.setUsername("self-registered");
		user.setPassword("Password1");
		user.setConfirmPassword("Password1");
		user.setFirstName("First");
		user.setLastName("Last");
		user.setEmail("self-registered@raibledesigns.com");
		user.setWebsite("http://raibledesigns.com");
		user.setPasswordHint("Password is one with you.");

		final HttpServletResponse response = new MockHttpServletResponse();

		// start SMTP Server
		final Wiser wiser = new Wiser();
		wiser.setPort(this.getSmtpPort());
		wiser.start();

		final BindingResult errors = new DataBinder(user).getBindingResult();
		this.c.onSubmit(user, errors, request, response);
		Assert.assertFalse("errors returned in model", errors.hasErrors());

		// verify an account information e-mail was sent
		wiser.stop();
		Assert.assertTrue(wiser.getMessages().size() == 1);

		// verify that success messages are in the request
		Assert.assertNotNull(request.getSession().getAttribute(
				"successMessages"));
		Assert.assertNotNull(request.getSession().getAttribute(
				Constants.REGISTERED));

		SecurityContextHolder.getContext().setAuthentication(null);
	}
}
