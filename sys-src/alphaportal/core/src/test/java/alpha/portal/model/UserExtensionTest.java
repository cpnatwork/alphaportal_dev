package alpha.portal.model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertTrue;

import java.util.HashSet;

import org.appfuse.model.User;
import org.junit.Test;

public class UserExtensionTest {

    @Test
    public void testAddAndHasRole() {
        UserExtension ue = new UserExtension();
        ContributorRole r = new ContributorRole("test");
        assertEquals(0, ue.getRoles().size());
        ue.addRole(r);
        assertFalse(ue.addRole(r));
        assertEquals(1, ue.getRoles().size());
        assertTrue(ue.hasRole(r));
        ContributorRole r2 = new ContributorRole("test2");
        assertFalse(ue.hasRole(r2));
    }

    @Test
    public void testConstructor() {
        User u = new User();
        u.setId(1L);
        UserExtension ue = new UserExtension(u);
        assertEquals(u, ue.getUser());
        assertEquals(u.getId(), ue.getUserId());
    }

    @Test
    public void testHashCodeEquals() {
        ContributorRole r = new ContributorRole("test");
        UserExtension ue = new UserExtension();
        ue.setUserId(1L);
        ue.addRole(r);
        UserExtension ue2 = new UserExtension();
        ue2.setUserId(1L);
        ue2.addRole(r);
        assertEquals(ue, ue2);
        assertEquals(ue.hashCode(), ue2.hashCode());
        assertNotSame(ue, new User());
        ue.toString();
        ue.setUser(new User());
        ue.setRoles(new HashSet<ContributorRole>());
        assertFalse(ue.equals(new String("I am not an User Extension! Please don´t shoot!")));
    }

}
