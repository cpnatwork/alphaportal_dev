package alpha.portal.model;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

public class AdornmentTest {

    @Autowired
    private Adornment testAdornment = new Adornment();

    private String testName = "mytestname";
    private long testId = 999l;
    private String testValue = "mytestadornmentvalue";

    @Before
    public void setUp() {
        testAdornment.setName(testName);
        testAdornment.setAdornmentId(testId);
        testAdornment.setValue(testValue);
    }

    @After
    public void tearDown() {
        testAdornment = null;
    }

    @Test
    public void testSetName() {
        assertTrue(testAdornment.getName().equals(testName));
    }

    @Test
    public void setAdornmentId() {
        testAdornment.setAdornmentId(999l);
        assertTrue(testAdornment.getAdornmentId() == 999l);
        testAdornment.setAdornmentId(testId);
    }

    @Test
    public void getAdornmentId() {
        assertTrue(testAdornment.getAdornmentId() == testId);
    }

    @Test
    public void testEqualsObject() {
        Adornment ad2 = new Adornment();
        ad2.setAdornmentId(testId);
        ad2.setName(testName);
        ad2.setValue(testValue);

        assertTrue(testAdornment.equals(ad2));

        assertFalse(testAdornment.equals(new AlphaCard()));
    }

    @Test
    public void testToString() {
        Adornment ad3 = new Adornment();
        ad3.setAdornmentId(testId);
        ad3.setName(testName);
        ad3.setValue(testValue);

        assertTrue(ad3.toString().length() > 0);
    }

    @Test
    public void testClone() {
        Adornment a = new Adornment();
        a.setName("test");
        a.setValue("lol");
        Adornment a2 = a.clone();
        assertEquals(a, a2);
    }
}
