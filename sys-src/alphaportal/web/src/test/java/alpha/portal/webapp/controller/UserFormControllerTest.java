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

import java.util.ArrayList;
import java.util.Map;

import org.appfuse.Constants;
import org.appfuse.model.User;
import org.appfuse.service.UserManager;
import org.junit.Assert;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.ui.ExtendedModelMap;
import org.springframework.validation.BindingResult;
import org.springframework.validation.DataBinder;

import alpha.portal.model.ContributorRole;
import alpha.portal.model.UserExtension;

/**
 * The Class UserFormControllerTest.
 */
public class UserFormControllerTest extends BaseControllerTestCase {

	/** The c. */
	@Autowired
	private final UserFormController c = null;

	/** The request. */
	private MockHttpServletRequest request;

	/** The user. */
	private User user;

	/**
	 * Test add.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	@Test
	public void testAdd() throws Exception {
		this.log.debug("testing add new user...");
		this.request = this.newGet("/userform.html");
		this.request.addParameter("method", "Add");
		this.request.addUserRole(Constants.ADMIN_ROLE);

		final Map<String, Object> model = this.c.showForm(this.request,
				new MockHttpServletResponse()).getModel();
		final Object object = model.get("user");
		Assert.assertTrue(object instanceof User);
		this.user = (User) object;
		Assert.assertNull(this.user.getUsername());
	}

	/**
	 * Test add without permission.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	@Test
	public void testAddWithoutPermission() throws Exception {
		this.log.debug("testing add new user...");
		this.request = this.newGet("/userform.html");
		this.request.addParameter("method", "Add");

		try {
			this.c.showForm(this.request, new MockHttpServletResponse());
			Assert.fail("AccessDeniedException not thrown...");
		} catch (final AccessDeniedException ade) {
			Assert.assertNotNull(ade.getMessage());
		}
	}

	/**
	 * Test cancel.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	@Test
	public void testCancel() throws Exception {
		this.log.debug("testing cancel...");
		this.request = this.newPost("/userform.html");
		this.request.addParameter("cancel", "");

		final BindingResult errors = new DataBinder(this.user)
				.getBindingResult();
		final String view = this.c.onSubmit(this.user, errors, this.request,
				new MockHttpServletResponse(), null);

		Assert.assertEquals("redirect:/mainMenu", view);
	}

	/**
	 * Test edit.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	@Test
	public void testEdit() throws Exception {
		this.log.debug("testing edit...");
		this.request = this.newGet("/userform.html");
		this.request.addParameter("id", "-1");
		this.request.addUserRole(Constants.ADMIN_ROLE);

		final Map<String, Object> model = this.c.showForm(this.request,
				new MockHttpServletResponse()).getModel();
		final Object object = model.get("user");
		Assert.assertTrue(object instanceof User);
		this.user = (User) object;
		Assert.assertEquals("Tomcat User", this.user.getFullName());
	}

	/**
	 * Test edit without permission.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	@Test
	public void testEditWithoutPermission() throws Exception {
		this.log.debug("testing edit...");
		this.request = this.newGet("/userform.html");
		this.request.addParameter("id", "-1"); // regular user

		try {
			this.c.showForm(this.request, new MockHttpServletResponse());
			Assert.fail("AccessDeniedException not thrown...");
		} catch (final AccessDeniedException ade) {
			Assert.assertNotNull(ade.getMessage());
		}
	}

	/**
	 * Test edit profile.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	@Test
	public void testEditProfile() throws Exception {
		this.log.debug("testing edit profile...");
		this.request = this.newGet("/userform.html");
		this.request.setRemoteUser("user");

		final Map<String, Object> model = this.c.showForm(this.request,
				new MockHttpServletResponse()).getModel();
		final Object object = model.get("user");
		Assert.assertTrue(object instanceof User);
		this.user = (User) object;
		Assert.assertEquals("Tomcat User", this.user.getFullName());
	}

	/**
	 * Test save.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	@Test
	public void testSave() throws Exception {
		this.request = this.newPost("/userform.html");
		// set updated properties first since adding them later will
		// result in multiple parameters with the same name getting sent
		final User user = ((UserManager) this.applicationContext
				.getBean("userManager")).getUser("-1");
		user.setConfirmPassword(user.getPassword());
		user.setLastName("Updated Last Name");

		final BindingResult errors = new DataBinder(user).getBindingResult();
		this.c.onSubmit(user, errors, this.request,
				new MockHttpServletResponse(), null);

		Assert.assertFalse(errors.hasErrors());
		Assert.assertNotNull(this.request.getSession().getAttribute(
				"successMessages"));
	}

	/**
	 * Test add with missing fields.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	@Test
	public void testAddWithMissingFields() throws Exception {
		this.request = this.newPost("/userform.html");
		this.user = new User();
		this.user.setFirstName("Jack");
		this.request.setRemoteUser("user");

		final BindingResult errors = new DataBinder(this.user)
				.getBindingResult();
		this.c.onSubmit(this.user, errors, this.request,
				new MockHttpServletResponse(), new ExtendedModelMap());

		Assert.assertTrue(errors.getAllErrors().size() == 10);
	}

	/**
	 * Test remove.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	@Test
	public void testRemove() throws Exception {
		this.request = this.newPost("/userform.html");
		this.request.addParameter("delete", "");
		this.user = new User();
		this.user.setId(-2L);

		final BindingResult errors = new DataBinder(this.user)
				.getBindingResult();
		this.c.onSubmit(this.user, errors, this.request,
				new MockHttpServletResponse(), null);

		Assert.assertNotNull(this.request.getSession().getAttribute(
				"successMessages"));
	}

	/**
	 * Test contibutor role stuff.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	@Test
	public void testContibutorRoleStuff() throws Exception {
		this.request = this.newGet("/userform.html");
		this.request.setRemoteUser("admin");

		final Map<String, Object> model = this.c.showForm(this.request,
				new MockHttpServletResponse()).getModel();
		Object object = model.get("userExtension");
		Assert.assertNotNull(object);
		Assert.assertTrue(object instanceof UserExtension);
		object = model.get("contributorRoles");
		Assert.assertNotNull(object);
		Assert.assertTrue(object instanceof ArrayList<?>);
		final ArrayList<?> objList = (ArrayList<?>) object;
		if (objList.size() > 0) {
			Assert.assertTrue(objList.get(0) instanceof ContributorRole);
		}
	}
}
