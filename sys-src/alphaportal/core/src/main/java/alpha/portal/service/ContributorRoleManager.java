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

import org.appfuse.service.GenericManager;

import alpha.portal.model.ContributorRole;

/**
 * Interface ContributorRoleManager, which extends from the GenericManager with
 * following objects: UserExtension and Long.
 */
public interface ContributorRoleManager extends
		GenericManager<ContributorRole, Long> {

	/**
	 * Get a ContributorRole by name.
	 * 
	 * @param name
	 *            the name of the ContributorRole
	 * @return the ContributorRole or null
	 */
	public ContributorRole getContributorRoleByName(String name);
}
