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
package alpha.portal.service;

import java.util.List;

import org.appfuse.service.GenericManager;

import alpha.portal.model.AlphaCard;
import alpha.portal.model.Payload;
import alpha.portal.model.PayloadIdentifier;

/**
 * Interface PayloadManager, which extends from the GenericManager with
 * following objects: Payload and Long.
 */
public interface PayloadManager extends
		GenericManager<Payload, PayloadIdentifier> {

	/**
	 * Gets all payload versions from the database.
	 * 
	 * @param payload
	 *            Payload
	 * @return List with Payloads
	 */
	public List<Payload> getAllVersions(Payload payload);

	/**
	 * Gets the given version of the payload.
	 * 
	 * @param id
	 *            PayloadIdentifier
	 * @return the Payload with the given version
	 */
	public Payload getVersion(PayloadIdentifier id);

	/**
	 * Saves a new payload in to the given AlphaCard. Generates a payloadId if
	 * there is none.
	 * 
	 * @param payload
	 *            the payload
	 * @param card
	 *            the card
	 * @return the saved payload (with its new sequenceNumber)
	 */
	public Payload saveNewPayload(final Payload payload, final AlphaCard card);
}
