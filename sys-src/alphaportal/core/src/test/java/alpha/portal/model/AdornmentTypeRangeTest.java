/**************************************************************************
 * alpha-Portal: A web portal, for managing knowledge-driven 
 * ad-hoc processes, in form of case files.
 * ==============================================
 * Copyright (C) 2011-2012 by 
 *   - Christoph P. Neumann (http://www.chr15t0ph.de)
 *   - and the SWAT 2011 team
 **************************************************************************
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *     http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software 
 * distributed under the License is distributed on an "AS IS" BASIS, 
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 **************************************************************************
 * $Id$
 *************************************************************************/
package alpha.portal.model;

import org.junit.Assert;
import org.junit.Test;

/**
 * The Class AdornmentTypeRangeTest.
 */
public class AdornmentTypeRangeTest {

	/**
	 * Test constructor int.
	 */
	@Test
	public void testConstructorInt() {
		final int min = 1;
		final int max = 10;
		final AdornmentTypeRange atr = new AdornmentTypeRange(min, max);

		Assert.assertTrue(atr.getMinInteger() == min);
		Assert.assertTrue(atr.getMaxInteger() == max);
	}

	/**
	 * Test constructor float.
	 */
	@Test
	public void testConstructorFloat() {
		final float min = 1.0f;
		final float max = 10.0f;
		final AdornmentTypeRange atr = new AdornmentTypeRange(min, max);

		Assert.assertTrue(atr.getMinFloat() == min);
		Assert.assertTrue(atr.getMaxFloat() == max);
	}

	/**
	 * Test is valid.
	 */
	@Test
	public void testIsValid() {
		final AdornmentTypeRange atrInt = new AdornmentTypeRange(1, 10);
		final AdornmentTypeRange atrFloat = new AdornmentTypeRange(1.0f, 10.0f);
		final AdornmentTypeRange atrString = new AdornmentTypeRange(
				new String[] { "Valid", "Invalid" });

		Assert.assertTrue(atrInt.isValid(5));
		Assert.assertTrue(atrFloat.isValid(5.0f));
		Assert.assertTrue(atrString.isValid("Valid"));

		Assert.assertFalse(atrInt.isValid(100));
		Assert.assertFalse(atrInt.isValid(-1));
		Assert.assertFalse(atrFloat.isValid(100.0f));
		Assert.assertFalse(atrFloat.isValid(0.1f));
		Assert.assertFalse(atrString.isValid("$%&/()"));

		atrInt.setMaxInteger(5);
		atrInt.setMinInteger(0);
		atrFloat.setMaxFloat(5.0f);
		atrFloat.setMinFloat(0.0f);
	}

}
