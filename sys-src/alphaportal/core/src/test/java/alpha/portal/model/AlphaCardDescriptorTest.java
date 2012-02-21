package alpha.portal.model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.junit.Test;

public class AlphaCardDescriptorTest {
    @Test
    public void testBasics() {
        AlphaCard c = new AlphaCard("123-abc");
        AlphaCardIdentifier id = c.getAlphaCardIdentifier();
        id.setCardId("987-zxy");

        AlphaCardDescriptor d = new AlphaCardDescriptor();
        d.setAlphaCard(c);
        assertEquals(c, d.getAlphaCard());
        d.setAlphaCardIdentifier(id);
        assertEquals(id, d.getAlphaCardIdentifier());

        int hash = d.hashCode();

        AlphaCardDescriptor d2 = new AlphaCardDescriptor(c);
        assertEquals(d, d2);
        AlphaCardDescriptor d3 = new AlphaCardDescriptor(id.getCaseId(), id.getCardId());
        d3.setAlphaCard(c);
        assertEquals(d, d3);

        assertEquals(hash, d3.hashCode());

        AlphaCard f = new AlphaCard("Test-123");
        AlphaCard g = new AlphaCard("Test-1234");
        AlphaCardDescriptor acD = new AlphaCardDescriptor();
        assertNotNull(f.getAlphaCardIdentifier());
        acD.setAlphaCard(f);
        assertEquals(f.getAlphaCardIdentifier(), acD.getAlphaCardIdentifier());
        g.setAlphaCardIdentifier(null);
        acD.setAlphaCard(g);
        assertEquals(f.getAlphaCardIdentifier(), acD.getAlphaCardIdentifier());
        acD.setAlphaCardIdentifier(null);
        g.setAlphaCardIdentifier(null);
        acD.setAlphaCard(g);
    }

    @Test
    public void testAdornments() throws Exception {
        AlphaCardDescriptor d = new AlphaCardDescriptor();
        d.setAlphaCardIdentifier(new AlphaCardIdentifier("123", "987"));
        assertNotNull(d.getAllAdornments());
        assertEquals(0, d.getAllAdornments().size());

        assertFalse(d.isAdornmentsChanged());
        d.setAdornment("test", "value");
        assertTrue(d.isAdornmentsChanged());
        assertNotNull(d.getAdornment("test"));
        assertEquals("test", d.getAdornment("test").getName());
        assertEquals("value", d.getAdornment("test").getValue());
        assertEquals(1, d.getAllAdornments().size());

        assertNull(d.getContributor());
        d.setContributor(123L);
        assertEquals(123L, d.getContributor().longValue());
        assertEquals(2, d.getAllAdornments().size());

        d.setAdornment(AdornmentType.Contributor.getName(), "");
        assertEquals(null, d.getContributor());
        d.setContributor(123L);

        assertEquals("No name", d.getTitle());
        d.setTitle("test");
        assertEquals("test", d.getTitle());

        d.setTitle("test2");
        assertEquals("test2", d.getTitle());
        assertEquals(3, d.getAllAdornments().size());

        d.deleteAdornment("test");
        assertNull(d.getAdornment("test"));

        assertFalse(d.deleteAdornment("geistadornment"));
    }

    @Test
    public void testEquals() {
        AlphaCardDescriptor desc = new AlphaCardDescriptor();
        desc.setAlphaCardIdentifier(new AlphaCardIdentifier("123", "987"));

        assertFalse(desc.equals(new AlphaCard()));

        desc.setAdornment("", "wow");

        try {
            desc.setTitle("");
        } catch (Exception e) {
            fail();
        }
        assertEquals("No name", desc.getTitle());

    }
}
