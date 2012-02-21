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
package alpha.portal.model;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * The Class ContributorRoleTest.
 */
public class ContributorRoleTest {

	/** The test contributor role. */
	private ContributorRole testContributorRole = new ContributorRole();

	/** The test name. */
	private final String testName = "mytestname";

	/** The contributor role id. */
	private final long contributorRoleId = 47;

	/**
	 * Sets the up.
	 */
	@Before
	public void setUp() {
		this.testContributorRole.setName(this.testName);
		this.testContributorRole.setContributorRoleId(this.contributorRoleId);
	}

	/**
	 * Tear down.
	 */
	@After
	public void tearDown() {
		this.testContributorRole = null;
	}

	/**
	 * Test setter and getter.
	 */
	@Test
	public void testSetterAndGetter() {
		Assert.assertTrue(this.testContributorRole.getName().equals(
				this.testName));
		Assert.assertTrue(this.testContributorRole.getContributorRoleId()
				.equals(this.contributorRoleId));
	}

	/**
	 * Test equals object.
	 */
	@Test
	public void testEqualsObject() {
		final ContributorRole role2 = new ContributorRole();
		role2.setName(this.testName);
		role2.setContributorRoleId(this.contributorRoleId);

		Assert.assertTrue(this.testContributorRole.equals(role2));

		Assert.assertFalse(this.testContributorRole
				.equals(new ContributorRole()));
	}

	/**
	 * Test to string.
	 */
	@Test
	public void testToString() {
		final ContributorRole role3 = new ContributorRole();
		role3.setName(this.testName);

		Assert.assertTrue(role3.toString().length() > 0);
	}
}
