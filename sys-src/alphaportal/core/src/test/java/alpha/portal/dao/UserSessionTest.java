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
import org.appfuse.dao.hibernate.GenericDaoHibernate;
import org.appfuse.model.User;
import org.appfuse.service.UserManager;
import org.hibernate.SessionFactory;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import alpha.portal.model.UserSession;

/**
 * The Class UserSessionTest.
 */
public class UserSessionTest extends BaseDaoTestCase {

	/** The session factory. */
	@Autowired
	SessionFactory sessionFactory;

	/** The user man. */
	UserManager userMan;

	/** The test user. */
	private User testUser;

	/** The user session dao. */
	private GenericDaoHibernate<UserSession, Long> userSessionDao;

	/**
	 * Sets the up.
	 */
	@Before
	public void setUp() {
		this.userSessionDao = new GenericDaoHibernate<UserSession, Long>(
				UserSession.class);
		this.userSessionDao.setSessionFactory(this.sessionFactory);

		// GET User
		this.testUser = new User("testUser");
		this.testUser.setId(42L);

		return;
	}

	/**
	 * Test all.
	 */
	@Test
	public void testAll() {
		final String lastViewedCaseId = "11111-22222-33333-44444-55555";

		UserSession uutUserSession = new UserSession();
		Assert.assertNotNull("could not create UserSession", uutUserSession);

		uutUserSession.setUserId(this.testUser.getId());
		uutUserSession = this.userSessionDao.save(uutUserSession);
		this.flush();

		Assert.assertEquals(this.testUser.getId(), uutUserSession.getUserId());

		uutUserSession.setLastViewedCaseId(lastViewedCaseId);
		uutUserSession = this.userSessionDao.save(uutUserSession);
		this.flush();

		Assert.assertEquals(lastViewedCaseId,
				uutUserSession.getLastViewedCaseId());

		this.userSessionDao.remove(uutUserSession.getUserId());

		return;
	}

	/**
	 * Clean up.
	 */
	@After
	public void cleanUp() {
		this.testUser = null;

		return;
	}

}
