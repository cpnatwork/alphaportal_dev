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

import org.appfuse.dao.BaseDaoTestCase;
import org.junit.Assert;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import alpha.portal.model.ContributorRole;

/**
 * The Class ContributorRoleDaoTest.
 */
public class ContributorRoleDaoTest extends BaseDaoTestCase {

	/** The user extension dao. */
	@Autowired
	private ContributorRoleDao contributorRoleDao;

	/**
	 * Test get contributor role by name.
	 */
	@Test
	public void testGetContributorRoleByName() {

		final String name = "SomeContributorRoleName";

		ContributorRole cRole = new ContributorRole(name);

		this.contributorRoleDao.save(cRole);

		cRole = this.contributorRoleDao.getContributorRoleByName(name);
		Assert.assertNotNull(cRole);
		Assert.assertEquals(name, cRole.getName());
	}
}
