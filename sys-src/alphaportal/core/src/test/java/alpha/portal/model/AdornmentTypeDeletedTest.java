package alpha.portal.model;

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class AdornmentTypeDeletedTest {

    @Test
    public void testFromValueException() {
        assertNull(AdornmentTypeDeleted.fromValue("nosuchvalue_$%&/()"));
    }

    @Test
    public void testFromValue() {
        assertTrue(AdornmentTypeDeleted.fromValue(AdornmentTypeDeleted.TRUE.value()) != null);
        assertTrue(AdornmentTypeDeleted.fromValue(AdornmentTypeDeleted.FALSE.value()) != null);
    }

}
