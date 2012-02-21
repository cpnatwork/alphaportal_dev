package alpha.portal.model;

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class AdornmentTypeValidityTest {

	@Test
	public void testFromValueException() {
		assertNull(AdornmentTypeValidity.fromValue("nosuchvalue_$%&/()"));
	}

	@Test
	public void testFromValue() {
		assertTrue(AdornmentTypeValidity.fromValue(AdornmentTypeValidity.VALID
				.value()) != null);
		assertTrue(AdornmentTypeValidity
				.fromValue(AdornmentTypeValidity.INVALID.value()) != null);
	}

}
