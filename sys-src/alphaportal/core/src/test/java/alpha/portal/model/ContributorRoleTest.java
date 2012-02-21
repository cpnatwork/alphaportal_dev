package alpha.portal.model;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class ContributorRoleTest {

    private ContributorRole testContributorRole = new ContributorRole();

    private String testName = "mytestname";
    private long contributorRoleId = 47;

    @Before
    public void setUp() {
        testContributorRole.setName(testName);
        testContributorRole.setContributorRoleId(contributorRoleId);
    }

    @After
    public void tearDown() {
        testContributorRole = null;
    }

    @Test
    public void testSetterAndGetter() {
        assertTrue(testContributorRole.getName().equals(testName));
        assertTrue(testContributorRole.getContributorRoleId().equals(contributorRoleId));
    }

    @Test
    public void testEqualsObject() {
        ContributorRole role2 = new ContributorRole();
        role2.setName(testName);
        role2.setContributorRoleId(contributorRoleId);

        assertTrue(testContributorRole.equals(role2));

        assertFalse(testContributorRole.equals(new ContributorRole()));
    }

    @Test
    public void testToString() {
        ContributorRole role3 = new ContributorRole();
        role3.setName(testName);

        assertTrue(role3.toString().length() > 0);
    }
}
