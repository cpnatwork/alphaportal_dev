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

import java.util.HashSet;

import org.appfuse.model.User;
import org.junit.Assert;
import org.junit.Test;

/**
 * The Class UserExtensionTest.
 */
public class UserExtensionTest {

	/**
	 * Test add and has role.
	 */
	@Test
	public void testAddAndHasRole() {
		final UserExtension ue = new UserExtension();
		final ContributorRole r = new ContributorRole("test");
		Assert.assertEquals(0, ue.getRoles().size());
		ue.addRole(r);
		Assert.assertFalse(ue.addRole(r));
		Assert.assertEquals(1, ue.getRoles().size());
		Assert.assertTrue(ue.hasRole(r));
		final ContributorRole r2 = new ContributorRole("test2");
		Assert.assertFalse(ue.hasRole(r2));
	}

	/**
	 * Test constructor.
	 */
	@Test
	public void testConstructor() {
		final User u = new User();
		u.setId(1L);
		final UserExtension ue = new UserExtension(u);
		Assert.assertEquals(u, ue.getUser());
		Assert.assertEquals(u.getId(), ue.getUserId());
	}

	/**
	 * Test hash code equals.
	 */
	@Test
	public void testHashCodeEquals() {
		final ContributorRole r = new ContributorRole("test");
		final UserExtension ue = new UserExtension();
		ue.setUserId(1L);
		ue.addRole(r);
		final UserExtension ue2 = new UserExtension();
		ue2.setUserId(1L);
		ue2.addRole(r);
		Assert.assertEquals(ue, ue2);
		Assert.assertEquals(ue.hashCode(), ue2.hashCode());
		Assert.assertNotSame(ue, new User());
		ue.toString();
		ue.setUser(new User());
		ue.setRoles(new HashSet<ContributorRole>());
		Assert.assertFalse(ue.equals(new String(
				"I am not an User Extension! Please don´t shoot!")));
	}

}
