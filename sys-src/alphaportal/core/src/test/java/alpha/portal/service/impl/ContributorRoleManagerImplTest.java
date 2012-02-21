package alpha.portal.service.impl;

import org.appfuse.service.impl.BaseManagerMockTestCase;
import org.jmock.Expectations;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import alpha.portal.dao.ContributorRoleDao;

public class ContributorRoleManagerImplTest extends BaseManagerMockTestCase {

    private ContributorRoleManagerImpl manager = null;
    private ContributorRoleDao dao = null;

    @Before
    public void setUp() {
        dao = context.mock(ContributorRoleDao.class);
        manager = new ContributorRoleManagerImpl(dao);
    }

    @After
    public void tearDown() {
        manager = null;
    }

    @Test
    public void testGetContributorRoleByName() {

        final String name = "some text";

        context.checking(new Expectations() {
            {
                one(dao).getContributorRoleByName(with(equal(name)));
            }
        });

        manager.getContributorRoleByName(name);

    }
}
