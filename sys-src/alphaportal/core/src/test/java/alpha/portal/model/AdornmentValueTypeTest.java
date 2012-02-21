package alpha.portal.model;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class AdornmentValueTypeTest {
	@Test
	public void testValueTypeConstructor() {
		AdornmentValueType myAdType = AdornmentValueType.Float;
		assertTrue(myAdType.equals(AdornmentValueType.Float));

		assertFalse(myAdType.equals(AdornmentValueType.String));
	}
}
