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
 * The Class AlphaCardIdentifierTest.
 */
public class AlphaCardIdentifierTest {

	/**
	 * Test basics.
	 */
	@Test
	public void testBasics() {
		final String cardId = "lol";
		final String caseId = "rofl";

		final AlphaCardIdentifier id = new AlphaCardIdentifier();
		id.setCardId(cardId);
		Assert.assertEquals(cardId, id.getCardId());
		id.setCaseId(caseId);
		Assert.assertEquals(caseId, id.getCaseId());
		final int hash = id.hashCode();

		final AlphaCardIdentifier id2 = new AlphaCardIdentifier(caseId);
		id2.setCardId(cardId);
		Assert.assertEquals(id, id2);
		Assert.assertEquals(hash, id2.hashCode());

		final AlphaCardIdentifier id3 = new AlphaCardIdentifier(caseId, cardId);
		Assert.assertEquals(id, id3);
		Assert.assertEquals(hash, id3.hashCode());
	}

	/**
	 * Test equals.
	 */
	@Test
	public void testEquals() {
		final String cardId = "lol";
		final String caseId = "123";
		final Long sequenceNumber = 1L;

		final AlphaCardIdentifier id = new AlphaCardIdentifier();
		id.setCardId(cardId);
		id.setCaseId(caseId);
		id.setSequenceNumber(sequenceNumber);

		Assert.assertFalse(id.equals(new AlphaCard()));

		final AlphaCardIdentifier id2 = id.clone();
		Assert.assertEquals(id, id2);
	}
}
