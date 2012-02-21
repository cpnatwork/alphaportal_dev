package alpha.portal.dao;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import org.appfuse.dao.BaseDaoTestCase;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import alpha.portal.model.ContributorRole;

public class ContributorRoleDaoTest extends BaseDaoTestCase {

    /** The user extension dao. */
    @Autowired
    private ContributorRoleDao contributorRoleDao;

    @Test
    public void testGetContributorRoleByName() {

        String name = "SomeContributorRoleName";

        ContributorRole cRole = new ContributorRole(name);

        contributorRoleDao.save(cRole);

        cRole = contributorRoleDao.getContributorRoleByName(name);
        assertNotNull(cRole);
        assertEquals(name, cRole.getName());
    }
}
