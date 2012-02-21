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
package alpha.portal.dao;

import java.util.List;

import org.appfuse.dao.GenericDao;

import alpha.portal.model.Payload;
import alpha.portal.model.PayloadIdentifier;

/**
 * The PayloadDAO is the Data Access Object of the Payload Class. It extends
 * from the GenericDao with the following objects: Payload and Long.
 */
public interface PayloadDao extends GenericDao<Payload, PayloadIdentifier> {

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
	 * @return he Payload with the given version
	 */
	public Payload getVersion(PayloadIdentifier id);
}
