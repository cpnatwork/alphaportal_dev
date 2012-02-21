package alpha.portal.model;

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class AdornmentTypeDataProvisionTest {

	@Test
	public void testFromValueException() {
		assertNull(AdornmentTypeDataProvision.fromValue("nosuchvalue_$%&/()"));
	}

	@Test
	public void testFromValue() {
		assertTrue(AdornmentTypeDataProvision
				.fromValue(AdornmentTypeDataProvision.OPEN.value()) != null);
		assertTrue(AdornmentTypeDataProvision
				.fromValue(AdornmentTypeDataProvision.INPROGRESS.value()) != null);
		assertTrue(AdornmentTypeDataProvision
				.fromValue(AdornmentTypeDataProvision.FULLFILLED.value()) != null);
	}

}
