package alpha.portal.dao;

import java.util.List;

import org.appfuse.dao.BaseDaoTestCase;
import org.junit.Assert;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import alpha.portal.model.ContributorRole;
import alpha.portal.model.UserExtension;
import alpha.portal.service.ContributorRoleManager;

public class UserExtensionDaoTest extends BaseDaoTestCase {

	/** The user extension dao. */
	@Autowired
	private UserExtensionDao userExtensionDao;

	@Autowired
	private ContributorRoleManager contributorRoleManager;

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
