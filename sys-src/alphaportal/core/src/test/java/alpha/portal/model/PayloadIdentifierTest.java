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
 * The Class PayloadIdentifierTest.
 */
public class PayloadIdentifierTest {

	/**
	 * Test equals.
	 */
	@Test
	public void testEquals() {
		final Long payloadId = 1L;
		final Long seqNr = 1L;

		final PayloadIdentifier id = new PayloadIdentifier();
		id.setPayloadId(payloadId);
		id.setSequenceNumber(seqNr);

		Assert.assertFalse(id.equals(new AlphaCard()));

		Assert.assertFalse(id.equals(new PayloadIdentifier()));

	}

	/**
	 * Test basics.
	 */
	@Test
	public void testBasics() {
		final Long payloadId = 1L;
		final Long seqNr = 1L;

		final PayloadIdentifier id = new PayloadIdentifier();
		id.setPayloadId(payloadId);
		Assert.assertTrue(payloadId == id.getPayloadId());
		id.setSequenceNumber(seqNr);
		Assert.assertTrue(seqNr == id.getSequenceNumber());
		final int hash = id.hashCode();

	}
}
