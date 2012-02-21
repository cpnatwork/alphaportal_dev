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
package alpha.portal.service.impl;

import org.appfuse.service.impl.BaseManagerMockTestCase;
import org.jmock.Expectations;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import alpha.portal.dao.UserExtensionDao;
import alpha.portal.model.ContributorRole;

/**
 * The Class UserExtensionManagerImplTest.
 */
public class UserExtensionManagerImplTest extends BaseManagerMockTestCase {

	/** The manager. */
	private UserExtensionManagerImpl manager = null;

	/** The dao. */
	private UserExtensionDao dao = null;

	/**
	 * Sets the up.
	 */
	@Before
	public void setUp() {
		this.dao = this.context.mock(UserExtensionDao.class);
		this.manager = new UserExtensionManagerImpl(this.dao);
	}

	/**
	 * Tear down.
	 */
	@After
	public void tearDown() {
		this.manager = null;
	}

	/**
	 * Test get user extensions by contributor role.
	 */
	@Test
	public void testGetUserExtensionsByContributorRole() {

		final ContributorRole role = new ContributorRole("Psychiater");

		this.context.checking(new Expectations() {
			{
				this.one(UserExtensionManagerImplTest.this.dao)
						.getUserExtensionsByContributorRole(
								this.with(Expectations.equal(role)));
			}
		});

		this.manager.getUserExtensionsByContributorRole(role);

	}
}
