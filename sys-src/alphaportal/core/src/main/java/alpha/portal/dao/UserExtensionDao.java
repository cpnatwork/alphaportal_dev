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

import alpha.portal.model.ContributorRole;
import alpha.portal.model.UserExtension;

/**
 * The UserExtensionDAO is the Data Access Object of the User Extension Class.
 * It extends from the GenericDao with the following objects: UserExtension and
 * Long.
 */
public interface UserExtensionDao extends GenericDao<UserExtension, Long> {

	/**
	 * Get all users that have the specific contributor role.
	 * 
	 * @param role
	 *            the given contributor role
	 * @return List with UserExtensions
	 */
	public List<UserExtension> getUserExtensionsByContributorRole(
			ContributorRole role);
}
