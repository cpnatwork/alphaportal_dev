package alpha.portal.model;

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class AdornmentTypeVisibilityTest {

	@Test
	public void testFromValueException() {
		assertNull(AdornmentTypeVisibility.fromValue("nosuchvalue_$%&/()"));
	}

	@Test
	public void testFromValue() {
		assertTrue(AdornmentTypeVisibility
				.fromValue(AdornmentTypeVisibility.PRIVATE.value()) != null);
		assertTrue(AdornmentTypeVisibility
				.fromValue(AdornmentTypeVisibility.PUBLIC.value()) != null);
	}

}
