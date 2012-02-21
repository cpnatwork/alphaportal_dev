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
 * The Class AlphaCardDescriptorTest.
 */
public class AlphaCardDescriptorTest {

	/**
	 * Test basics.
	 */
	@Test
	public void testBasics() {
		final AlphaCard c = new AlphaCard("123-abc");
		final AlphaCardIdentifier id = c.getAlphaCardIdentifier();
		id.setCardId("987-zxy");

		final AlphaCardDescriptor d = new AlphaCardDescriptor();
		d.setAlphaCard(c);
		Assert.assertEquals(c, d.getAlphaCard());
		d.setAlphaCardIdentifier(id);
		Assert.assertEquals(id, d.getAlphaCardIdentifier());

		final int hash = d.hashCode();

		final AlphaCardDescriptor d2 = new AlphaCardDescriptor(c);
		Assert.assertEquals(d, d2);
		final AlphaCardDescriptor d3 = new AlphaCardDescriptor(id.getCaseId(),
				id.getCardId());
		d3.setAlphaCard(c);
		Assert.assertEquals(d, d3);

		Assert.assertEquals(hash, d3.hashCode());

		final AlphaCard f = new AlphaCard("Test-123");
		final AlphaCard g = new AlphaCard("Test-1234");
		final AlphaCardDescriptor acD = new AlphaCardDescriptor();
		Assert.assertNotNull(f.getAlphaCardIdentifier());
		acD.setAlphaCard(f);
		Assert.assertEquals(f.getAlphaCardIdentifier(),
				acD.getAlphaCardIdentifier());
		g.setAlphaCardIdentifier(null);
		acD.setAlphaCard(g);
		Assert.assertEquals(f.getAlphaCardIdentifier(),
				acD.getAlphaCardIdentifier());
		acD.setAlphaCardIdentifier(null);
		g.setAlphaCardIdentifier(null);
		acD.setAlphaCard(g);
	}

	/**
	 * Test adornments.
	 * 
	 * @throws Exception
	 *             the exception
	 */
	@Test
	public void testAdornments() throws Exception {
		final AlphaCardDescriptor d = new AlphaCardDescriptor();
		d.setAlphaCardIdentifier(new AlphaCardIdentifier("123", "987"));
		Assert.assertNotNull(d.getAllAdornments());
		Assert.assertEquals(0, d.getAllAdornments().size());

		Assert.assertFalse(d.isAdornmentsChanged());
		d.setAdornment("test", "value");
		Assert.assertTrue(d.isAdornmentsChanged());
		Assert.assertNotNull(d.getAdornment("test"));
		Assert.assertEquals("test", d.getAdornment("test").getName());
		Assert.assertEquals("value", d.getAdornment("test").getValue());
		Assert.assertEquals(1, d.getAllAdornments().size());

		Assert.assertNull(d.getContributor());
		d.setContributor(123L);
		Assert.assertEquals(123L, d.getContributor().longValue());
		Assert.assertEquals(2, d.getAllAdornments().size());

		d.setAdornment(AdornmentType.Contributor.getName(), "");
		Assert.assertEquals(null, d.getContributor());
		d.setContributor(123L);

		Assert.assertEquals("No name", d.getTitle());
		d.setTitle("test");
		Assert.assertEquals("test", d.getTitle());

		d.setTitle("test2");
		Assert.assertEquals("test2", d.getTitle());
		Assert.assertEquals(3, d.getAllAdornments().size());

		d.deleteAdornment("test");
		Assert.assertNull(d.getAdornment("test"));

		Assert.assertFalse(d.deleteAdornment("geistadornment"));
	}

	/**
	 * Test equals.
	 */
	@Test
	public void testEquals() {
		final AlphaCardDescriptor desc = new AlphaCardDescriptor();
		desc.setAlphaCardIdentifier(new AlphaCardIdentifier("123", "987"));

		Assert.assertFalse(desc.equals(new AlphaCard()));

		desc.setAdornment("", "wow");

		try {
			desc.setTitle("");
		} catch (final Exception e) {
			Assert.fail();
		}
		Assert.assertEquals("No name", desc.getTitle());

	}
}
