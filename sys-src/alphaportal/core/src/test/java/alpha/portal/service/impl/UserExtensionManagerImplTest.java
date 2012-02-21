package alpha.portal.service.impl;

import org.appfuse.service.impl.BaseManagerMockTestCase;
import org.jmock.Expectations;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import alpha.portal.dao.UserExtensionDao;
import alpha.portal.model.ContributorRole;

public class UserExtensionManagerImplTest extends BaseManagerMockTestCase {

	private UserExtensionManagerImpl manager = null;
	private UserExtensionDao dao = null;

	@Before
	public void setUp() {
		this.dao = this.context.mock(UserExtensionDao.class);
		this.manager = new UserExtensionManagerImpl(this.dao);
	}

	@After
	public void tearDown() {
		this.manager = null;
	}

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
