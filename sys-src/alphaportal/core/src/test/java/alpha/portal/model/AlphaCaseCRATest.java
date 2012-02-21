package alpha.portal.model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.appfuse.model.User;
import org.junit.Test;

public class AlphaCaseCRATest {
    @Test
    public void testBasics() {
	AlphaCaseCRA cra = new AlphaCaseCRA();
	assertNotNull(cra.getListOfParticipants());
	assertEquals(0, cra.getListOfParticipants().size());

	int hash = cra.hashCode();
	assertTrue(cra.equals(new AlphaCaseCRA()));
	assertEquals(hash, (new AlphaCaseCRA()).hashCode());

	String to = cra.toString();

	User u = new User();
	u.setId(123L);
	cra.addUserToListOfParticipants(u);
	assertEquals(1, cra.getListOfParticipants().size());
	assertTrue(cra.getListOfParticipants().contains(u));

	AlphaCaseCRA cra2 = new AlphaCaseCRA();
	cra2.addUserToListOfParticipants(u);
	assertEquals(cra, cra2);

	cra.removeUserFromListOfParticipants(u);
	assertEquals(0, cra.getListOfParticipants().size());
	assertEquals(to, cra.toString());
	assertEquals(hash, cra.hashCode());
    }

    @Test
    public void testEquals() {
	AlphaCaseCRA cra = new AlphaCaseCRA();

	assertFalse(cra.equals(new AlphaCard()));

    }
}
