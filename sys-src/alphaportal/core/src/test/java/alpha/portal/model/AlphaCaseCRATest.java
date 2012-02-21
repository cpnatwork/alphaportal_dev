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

import org.appfuse.model.User;
import org.junit.Assert;
import org.junit.Test;

/**
 * The Class AlphaCaseCRATest.
 */
public class AlphaCaseCRATest {

	/**
	 * Test basics.
	 */
	@Test
	public void testBasics() {
		final AlphaCaseCRA cra = new AlphaCaseCRA();
		Assert.assertNotNull(cra.getListOfParticipants());
		Assert.assertEquals(0, cra.getListOfParticipants().size());

		final int hash = cra.hashCode();
		Assert.assertTrue(cra.equals(new AlphaCaseCRA()));
		Assert.assertEquals(hash, (new AlphaCaseCRA()).hashCode());

		final String to = cra.toString();

		final User u = new User();
		u.setId(123L);
		cra.addUserToListOfParticipants(u);
		Assert.assertEquals(1, cra.getListOfParticipants().size());
		Assert.assertTrue(cra.getListOfParticipants().contains(u));

		final AlphaCaseCRA cra2 = new AlphaCaseCRA();
		cra2.addUserToListOfParticipants(u);
		Assert.assertEquals(cra, cra2);

		cra.removeUserFromListOfParticipants(u);
		Assert.assertEquals(0, cra.getListOfParticipants().size());
		Assert.assertEquals(to, cra.toString());
		Assert.assertEquals(hash, cra.hashCode());
	}

	/**
	 * Test equals.
	 */
	@Test
	public void testEquals() {
		final AlphaCaseCRA cra = new AlphaCaseCRA();

		Assert.assertFalse(cra.equals(new AlphaCard()));

	}
}
