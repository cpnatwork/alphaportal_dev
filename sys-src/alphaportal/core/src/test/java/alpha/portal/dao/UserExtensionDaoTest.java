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
package alpha.portal.dao;

import java.util.List;

import org.appfuse.dao.BaseDaoTestCase;
import org.junit.Assert;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import alpha.portal.model.ContributorRole;
import alpha.portal.model.UserExtension;
import alpha.portal.service.ContributorRoleManager;

/**
 * The Class UserExtensionDaoTest.
 */
public class UserExtensionDaoTest extends BaseDaoTestCase {

	/** The user extension dao. */
	@Autowired
	private UserExtensionDao userExtensionDao;

	/** The contributor role manager. */
	@Autowired
	private ContributorRoleManager contributorRoleManager;

	/**
	 * Test get user extensions by contributor role.
	 */
	@Test
	public void testGetUserExtensionsByContributorRole() {
		ContributorRole cRole = this.contributorRoleManager
				.getContributorRoleByName("Radiologe");
		List<UserExtension> ueList = this.userExtensionDao
				.getUserExtensionsByContributorRole(cRole);

		Assert.assertNotNull(cRole);
		Assert.assertTrue(ueList.size() == 4);

		cRole = this.contributorRoleManager
				.getContributorRoleByName("Somethingthatdoesnotexist");
		ueList = this.userExtensionDao
				.getUserExtensionsByContributorRole(cRole);

		Assert.assertTrue(ueList.size() == 0);
	}
}
