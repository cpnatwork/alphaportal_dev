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

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * The Class AdornmentTest.
 */
public class AdornmentTest {

	/** The test adornment. */
	@Autowired
	private Adornment testAdornment = new Adornment();

	/** The test name. */
	private final String testName = "mytestname";

	/** The test id. */
	private final long testId = 999l;

	/** The test value. */
	private final String testValue = "mytestadornmentvalue";

	/**
	 * Sets the up.
	 */
	@Before
	public void setUp() {
		this.testAdornment.setName(this.testName);
		this.testAdornment.setAdornmentId(this.testId);
		this.testAdornment.setValue(this.testValue);
	}

	/**
	 * Tear down.
	 */
	@After
	public void tearDown() {
		this.testAdornment = null;
	}

	/**
	 * Test set name.
	 */
	@Test
	public void testSetName() {
		Assert.assertTrue(this.testAdornment.getName().equals(this.testName));
	}

	/**
	 * Sets the adornment id.
	 */
	@Test
	public void setAdornmentId() {
		this.testAdornment.setAdornmentId(999l);
		Assert.assertTrue(this.testAdornment.getAdornmentId() == 999l);
		this.testAdornment.setAdornmentId(this.testId);
	}

	/**
	 * Gets the adornment id.
	 * 
	 * @return the adornment id
	 */
	@Test
	public void getAdornmentId() {
		Assert.assertTrue(this.testAdornment.getAdornmentId() == this.testId);
	}

	/**
	 * Test equals object.
	 */
	@Test
	public void testEqualsObject() {
		final Adornment ad2 = new Adornment();
		ad2.setAdornmentId(this.testId);
		ad2.setName(this.testName);
		ad2.setValue(this.testValue);

		Assert.assertTrue(this.testAdornment.equals(ad2));

		Assert.assertFalse(this.testAdornment.equals(new AlphaCard()));
	}

	/**
	 * Test to string.
	 */
	@Test
	public void testToString() {
		final Adornment ad3 = new Adornment();
		ad3.setAdornmentId(this.testId);
		ad3.setName(this.testName);
		ad3.setValue(this.testValue);

		Assert.assertTrue(ad3.toString().length() > 0);
	}

	/**
	 * Test clone.
	 */
	@Test
	public void testClone() {
		final Adornment a = new Adornment();
		a.setName("test");
		a.setValue("lol");
		final Adornment a2 = a.clone();
		Assert.assertEquals(a, a2);
	}
}
