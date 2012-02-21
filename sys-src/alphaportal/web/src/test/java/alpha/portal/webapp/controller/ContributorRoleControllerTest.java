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
import java.util.Map;

import org.apache.commons.lang.StringUtils;
import org.appfuse.model.User;
import org.appfuse.service.UserManager;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.orm.ObjectRetrievalFailureException;
import org.springframework.web.servlet.ModelAndView;

import alpha.portal.model.ContributorRole;
import alpha.portal.service.ContributorRoleManager;
import alpha.portal.service.UserExtensionManager;

/**
 * Test for ContributorRoleController.
 * 
 * @see ContributorRoleController ContributorRoleController
 */
public class ContributorRoleControllerTest extends BaseControllerTestCase {

	/** User name of test user. */
	private final String testUserName = "ichbineintollertesterdenesnochnichtgibt";

	/** Name of test contributor role. */
	private final String testcontribRoleName = "Test Contributor Role";

	/** Test user object. */
	private User u;

	/** Test contributor role. */
	private ContributorRole contribRole;

	/** The controller. */
	@Autowired
	private ContributorRoleController ctrl;

	/** The ContributorRoleManager. */
	@Autowired
	private ContributorRoleManager contributorRoleManager;

	/** The user manager. */
	@Autowired
	private UserManager userManager;

	/** The UserExtensionManager. */
	@Autowired
	private UserExtensionManager UserExtensionManager;

	/**
	 * - Prepare and save test user - Create test contributorRole.
	 */
	@Before
	public void beforePrepare() {
		this.u = new User(this.testUserName);
		this.u.setEmail("l@m.d");
		this.u.setFirstName("l");
		this.u.setLastName("m");
		this.u.setPassword("123");
		this.u = this.userManager.save(this.u);

		this.contribRole = new ContributorRole(this.testcontribRoleName);
		this.contribRole = this.contributorRoleManager.save(this.contribRole);
	}

	/**
	 * Test basic page call.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	@SuppressWarnings("unchecked")
	@Test
	public void testBasicPage() throws Exception {
		final MockHttpServletRequest request = this.newGet("/contributorRole");
		request.setRemoteUser(this.testUserName);

		final int numberOfRoles = this.contributorRoleManager.getAll().size();
		/**
		 * Only test precondition!
		 */
		Assert.assertTrue("No roles to test!", numberOfRoles > 0);

		final ModelAndView result = this.ctrl.showPage(request);
		final Map<String, Object> resModel = result.getModel();

		Assert.assertTrue(resModel.containsKey("contributorRolesList"));
		final Object contribListObj = resModel.get("contributorRolesList");
		final List<ContributorRole> contribList = (List<ContributorRole>) contribListObj;
		Assert.assertEquals(numberOfRoles, contribList.size());
	}

	/**
	 * V-Test (save, show, edit, delete).
	 * 
	 * @throws Exception
	 *             the exception
	 */
	@SuppressWarnings("unchecked")
	@Test
	public void testVTest() throws Exception {
		final String newCRName = "New Test Contributor Role";

		final int numberOfRolesBeforeAdd = this.contributorRoleManager.getAll()
				.size();

		/**
		 * Add
		 */
		MockHttpServletRequest request = this.newPost("/contributorRole");
		request.setRemoteUser(this.testUserName);
		request.addParameter("save_new", "button.save");
		request.addParameter("newContributorRole", newCRName);

		MockHttpServletResponse response = new MockHttpServletResponse();

		this.ctrl.saveNew(request, response);
		Assert.assertTrue(StringUtils.isBlank(response.getErrorMessage()));

		/**
		 * Show
		 */
		request = this.newGet("/contributorRole");
		request.setRemoteUser(this.testUserName);

		ModelAndView result = this.ctrl.showPage(request);
		Map<String, Object> resModel = result.getModel();

		final Object contribListObj = resModel.get("contributorRolesList");
		final List<ContributorRole> contribList = (List<ContributorRole>) contribListObj;
		Assert.assertEquals((numberOfRolesBeforeAdd + 1), contribList.size());
		ContributorRole newCR = this.contributorRoleManager
				.getContributorRoleByName(newCRName);
		boolean contains = false;
		for (int c = 0; (c < contribList.size()) && (contains == false); c++) {
			if (contribList.get(c).getName().equals(newCRName)) {
				contains = true;
			}
		}
		Assert.assertTrue("New contrib.role in roles list", contains);

		/**
		 * Edit-Page
		 */
		request = this.newGet("/contributorRole");
		request.setRemoteUser(this.testUserName);
		request.addParameter("edit", newCR.getContributorRoleId().toString());

		result = this.ctrl.showPage(request);
		resModel = result.getModel();

		Assert.assertTrue(resModel.containsKey("showEditingForm"));
		Assert.assertTrue(resModel.containsKey("roleToEditId"));
		Assert.assertTrue(resModel.containsKey("roleToEdit"));
		Assert.assertFalse(resModel.containsKey("messageId"));

		Assert.assertEquals(newCR.getContributorRoleId().toString(),
				resModel.get("roleToEditId"));
		Assert.assertEquals(newCR.getName(), resModel.get("roleToEdit"));

		/**
		 * Edit-Post
		 */
		request = this.newPost("/contributorRole");
		request.setRemoteUser(this.testUserName);
		request.addParameter("save_edit", "button.save");
		request.addParameter("newContributorRole", newCRName + " - Changed");
		request.addParameter("oldContribRoleId", newCR.getContributorRoleId()
				.toString());

		response = new MockHttpServletResponse();

		this.ctrl.saveEdit(request, response);
		Assert.assertTrue(StringUtils.isBlank(response.getErrorMessage()));

		newCR = this.contributorRoleManager.get(newCR.getContributorRoleId());
		Assert.assertEquals(newCRName + " - Changed", newCR.getName());

		/**
		 * Delete
		 */
		request = this.newPost("/contributorRole");
		request.setRemoteUser(this.testUserName);
		request.addParameter("delete", newCR.getContributorRoleId().toString());

		result = this.ctrl.showPage(request);
		resModel = result.getModel();

		Assert.assertTrue(resModel.containsKey("contributorRolesList"));
		Assert.assertTrue(StringUtils.isBlank(response.getErrorMessage()));

		boolean isException = false;
		try {
			newCR = this.contributorRoleManager.get(newCR
					.getContributorRoleId());
		} catch (final ObjectRetrievalFailureException e) {
			isException = true;
		}
		Assert.assertTrue("role not deleted", isException);
	}

	/**
	 * "Umlaut"-Test.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	@SuppressWarnings("unchecked")
	@Test
	public void testUmlaute() throws Exception {
		final String newCRName = "äöü'";

		MockHttpServletRequest request = this.newPost("/contributorRole");
		request.setRemoteUser(this.testUserName);
		request.addParameter("save_new", "button.save");
		request.addParameter("newContributorRole", newCRName);
		final MockHttpServletResponse response = new MockHttpServletResponse();
		this.ctrl.saveNew(request, response);
		Assert.assertTrue(StringUtils.isBlank(response.getErrorMessage()));

		request = this.newGet("/contributorRole");
		request.setRemoteUser(this.testUserName);
		final ModelAndView result = this.ctrl.showPage(request);
		final Map<String, Object> resModel = result.getModel();
		final Object contribListObj = resModel.get("contributorRolesList");
		final List<ContributorRole> contribList = (List<ContributorRole>) contribListObj;
		boolean contains = false;
		for (int c = 0; (c < contribList.size()) && (contains == false); c++) {
			if (contribList.get(c).getName().equals(newCRName)) {
				contains = true;
			}
		}
		Assert.assertTrue("Umlaut-Test failed", contains);
	}

	/**
	 * Test for get error redirects.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	@Test
	public void testGetErrors() throws Exception {
		MockHttpServletRequest request = this.newPost("/contributorRole");
		request.setRemoteUser(this.testUserName);
		ModelAndView result = this.ctrl.showPage(request);
		Map<String, Object> resModel = result.getModel();
		Assert.assertTrue(resModel.containsKey("contributorRolesList"));

		request = this.newPost("/contributorRole");
		request.setRemoteUser(this.testUserName);
		request.addParameter("delete", "1234567890");
		result = this.ctrl.showPage(request);
		resModel = result.getModel();
		Assert.assertTrue(resModel.containsKey("contributorRolesList"));

		request = this.newPost("/contributorRole");
		request.setRemoteUser(this.testUserName);
		request.addParameter("delete", this.contributorRoleManager
				.getContributorRoleByName("Radiologe").getContributorRoleId()
				.toString());
		request.addParameter("edit", "1234567890");
		result = this.ctrl.showPage(request);
		resModel = result.getModel();

		request = this.newPost("/contributorRole");
		request.setRemoteUser(this.testUserName);
		request.addParameter("edit", "1234567890");
		result = this.ctrl.showPage(request);
		resModel = result.getModel();
		Assert.assertFalse(resModel.containsKey("showEditingForm"));
		Assert.assertFalse(resModel.containsKey("roleToEditId"));
		Assert.assertFalse(resModel.containsKey("roleToEdit"));
	}

	/**
	 * Test for post error redirects.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	@Test
	public void testPostErrors() throws Exception {
		MockHttpServletRequest request = this.newPost("/contributorRole");
		request.setRemoteUser(this.testUserName);
		MockHttpServletResponse response = new MockHttpServletResponse();
		String redirect = this.ctrl.doPost(request, response);
		Assert.assertEquals("redirect:/contributorRole", redirect);

		request = this.newPost("/contributorRole");
		request.setRemoteUser(this.testUserName);
		request.addParameter("save_new", "button.save");
		response = new MockHttpServletResponse();
		redirect = this.ctrl.saveNew(request, response);
		Assert.assertEquals("redirect:/contributorRole", redirect);

		request = this.newPost("/contributorRole");
		request.setRemoteUser(this.testUserName);
		request.addParameter("save_new", "button.save");
		request.addParameter("netContributorRole", "button.save");
		response = new MockHttpServletResponse();
		redirect = this.ctrl.saveNew(request, response);
		Assert.assertEquals("redirect:/contributorRole", redirect);

		request = this.newPost("/contributorRole");
		request.setRemoteUser(this.testUserName);
		request.addParameter("save_new", "button.save");
		request.addParameter("newContributorRole", this.contribRole.getName());
		response = new MockHttpServletResponse();
		redirect = this.ctrl.saveNew(request, response);
		Assert.assertEquals("redirect:/contributorRole", redirect);

		request = this.newPost("/contributorRole");
		request.setRemoteUser(this.testUserName);
		request.addParameter("save_edit", "button.save");
		response = new MockHttpServletResponse();
		redirect = this.ctrl.saveNew(request, response);
		Assert.assertEquals("redirect:/contributorRole", redirect);

		request = this.newPost("/contributorRole");
		request.setRemoteUser(this.testUserName);
		request.addParameter("newContributorRole", "");
		request.addParameter("save_edit", "button.save");
		response = new MockHttpServletResponse();
		redirect = this.ctrl.saveNew(request, response);
		Assert.assertEquals("redirect:/contributorRole", redirect);

		request = this.newPost("/contributorRole");
		request.setRemoteUser(this.testUserName);
		request.addParameter("newContributorRole", "TESTTESTTEST");
		request.addParameter("oldContribRoleId", "");
		request.addParameter("save_edit", "button.save");
		response = new MockHttpServletResponse();
		redirect = this.ctrl.saveNew(request, response);
		Assert.assertEquals("redirect:/contributorRole", redirect);

		request = this.newPost("/contributorRole");
		request.setRemoteUser(this.testUserName);
		request.addParameter("oldContribRoleId", "1");
		request.addParameter("save_edit", "button.save");
		response = new MockHttpServletResponse();
		redirect = this.ctrl.saveEdit(request, response);
		Assert.assertEquals("redirect:/contributorRole?edit=1", redirect);

		request = this.newPost("/contributorRole");
		request.setRemoteUser(this.testUserName);
		request.addParameter("newContributorRole", this.contribRole.getName());
		request.addParameter("oldContribRoleId", "1");
		request.addParameter("save_edit", "button.save");
		response = new MockHttpServletResponse();
		redirect = this.ctrl.saveEdit(request, response);
		Assert.assertEquals("redirect:/contributorRole?edit=1", redirect);

		request = this.newPost("/contributorRole");
		request.setRemoteUser(this.testUserName);
		request.addParameter("newContributorRole", "TESTTESTTEST");
		request.addParameter("oldContribRoleId", "12345667890");
		request.addParameter("save_edit", "button.save");
		response = new MockHttpServletResponse();
		redirect = this.ctrl.saveEdit(request, response);
		Assert.assertEquals("redirect:/contributorRole?edit=12345667890",
				redirect);

		request = this.newPost("/contributorRole");
		request.setRemoteUser(this.testUserName);
		request.addParameter("newContributorRole", "TESTTESTTEST");
		request.addParameter("oldContribRoleId", "NotA-Long");
		request.addParameter("save_edit", "button.save");
		response = new MockHttpServletResponse();
		redirect = this.ctrl.saveEdit(request, response);
		Assert.assertEquals("redirect:/contributorRole?edit=NotA-Long",
				redirect);
	}

	/**
	 * - delete test user - delete test contributor role.
	 */
	@After
	public void afterCleanup() {
		this.userManager.remove(this.u.getId());

		if (this.contribRole != null) {
			this.contributorRoleManager.remove(this.contribRole
					.getContributorRoleId());
		}
	}

}
