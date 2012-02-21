package alpha.portal.model;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class AdornmentTypeRangeTest {

    @Test
    public void testConstructorInt() {
        int min = 1;
        int max = 10;
        AdornmentTypeRange atr = new AdornmentTypeRange(min, max);

        assertTrue(atr.getMinInteger() == min);
        assertTrue(atr.getMaxInteger() == max);
    }

    @Test
    public void testConstructorFloat() {
        float min = 1.0f;
        float max = 10.0f;
        AdornmentTypeRange atr = new AdornmentTypeRange(min, max);

        assertTrue(atr.getMinFloat() == min);
        assertTrue(atr.getMaxFloat() == max);
    }

    @Test
    public void testIsValid() {
        AdornmentTypeRange atrInt = new AdornmentTypeRange(1, 10);
        AdornmentTypeRange atrFloat = new AdornmentTypeRange(1.0f, 10.0f);
        AdornmentTypeRange atrString = new AdornmentTypeRange(new String[] {
                "Valid", "Invalid" });

        assertTrue(atrInt.isValid(5));
        assertTrue(atrFloat.isValid(5.0f));
        assertTrue(atrString.isValid("Valid"));

        assertFalse(atrInt.isValid(100));
        assertFalse(atrInt.isValid(-1));
        assertFalse(atrFloat.isValid(100.0f));
        assertFalse(atrFloat.isValid(0.1f));
        assertFalse(atrString.isValid("$%&/()"));

        atrInt.setMaxInteger(5);
        atrInt.setMinInteger(0);
        atrFloat.setMaxFloat(5.0f);
        atrFloat.setMinFloat(0.0f);
    }

}
