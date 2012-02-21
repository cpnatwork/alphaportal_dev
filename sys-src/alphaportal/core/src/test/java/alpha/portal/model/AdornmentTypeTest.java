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

import org.apache.commons.lang.ArrayUtils;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * The Class AdornmentTypeTest.
 */
public class AdornmentTypeTest {

	/** The type. */
	private AdornmentType type;

	/** The name. */
	private String name;

	/** The value type. */
	private AdornmentValueType valueType;

	/** The range. */
	private AdornmentTypeRange range;

	/** The default value. */
	private String defaultValue;

	/**
	 * Sets the up.
	 */
	@Before
	public void setUp() {
		this.defaultValue = AdornmentTypeVisibility.PRIVATE.value();
		this.valueType = AdornmentValueType.Enum;
		this.range = new AdornmentTypeRange(new String[] {
				AdornmentTypeVisibility.PRIVATE.value(),
				AdornmentTypeVisibility.PUBLIC.value() });

		this.type = AdornmentType.Visibility;
		this.type.setValueType(this.valueType);
		this.type.setValueRange(this.range);

		this.name = this.type.getName();
	}

	/**
	 * Test from name exception.
	 */
	@Test
	public void testFromNameException() {
		Assert.assertNull(AdornmentType.fromName("nosuchvalue_$%&/()"));
	}

	/**
	 * Test from name.
	 */
	@Test
	public void testFromName() {
		Assert.assertTrue(AdornmentType.fromName(AdornmentType.Contributor
				.getName()) == AdornmentType.Contributor);
	}

	/**
	 * Test value type.
	 */
	@Test
	public void testValueType() {
		Assert.assertTrue(this.valueType == this.type.getValueType());

		this.type.setValueType(AdornmentValueType.String);
		Assert.assertFalse(this.valueType == this.type.getValueType());
	}

	/**
	 * Test value range.
	 */
	@Test
	public void testValueRange() {
		Assert.assertTrue(this.range == this.type.getValueRange());

		this.type.setValueRange(new AdornmentTypeRange(
				new String[] { "falsches Array" }));
		Assert.assertFalse(this.range == this.type.getValueRange());
	}

	/**
	 * Test value default.
	 */
	@Test
	public void testValueDefault() {
		Assert.assertTrue(this.defaultValue == this.type.getValueDefault());

		this.type.setValueDefault(AdornmentTypeVisibility.PUBLIC.value());
		Assert.assertFalse(this.defaultValue == this.type.getValueDefault());
	}

	/**
	 * Test validate.
	 */
	@Test
	public void testValidate() {
		Assert.assertTrue(AdornmentType.Title.validate("bla"));
		Assert.assertFalse(AdornmentType.Visibility.validate("neeeein"));
		Assert.assertTrue(AdornmentType.Visibility
				.validate(AdornmentTypeVisibility.PRIVATE.value()));

		Assert.assertFalse(AdornmentType.PayloadVersionNumber.validate("abc"));
	}

	/**
	 * Test valid strings.
	 */
	@Test
	public void testValidStrings() {
		final String[] t = AdornmentType.Visibility.getValueRange()
				.getValidStrings();
		Assert.assertEquals(2, t.length);
		Assert.assertTrue(ArrayUtils.contains(t,
				AdornmentTypeVisibility.PRIVATE.value()));
		Assert.assertTrue(ArrayUtils.contains(t,
				AdornmentTypeVisibility.PUBLIC.value()));
	}
}
