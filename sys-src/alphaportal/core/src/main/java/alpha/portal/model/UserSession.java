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

import javax.persistence.Entity;
import javax.persistence.Id;

/**
 * The user session is the object, where we save across sessions attributes. It
 * is used for user specific settings over sessions.
 */
@Entity(name = "usersession")
public class UserSession {

	/**
	 * the Id of the user.
	 */
	@Id
	private Long userId;

	/**
	 * the case, which the user saw last.
	 */
	private String lastViewedCaseId;

	/**
	 * gets the userId.
	 * 
	 * @return userId
	 */
	public Long getUserId() {
		return this.userId;
	}

	/**
	 * sets the userId.
	 * 
	 * @param userId
	 *            the new Id of the user
	 */
	public void setUserId(final Long userId) {
		this.userId = userId;
	}

	/**
	 * gets the last case, the user viewed.
	 * 
	 * @return the caseId of the last viewed case
	 */
	public String getLastViewedCaseId() {
		return this.lastViewedCaseId;
	}

	/**
	 * sets the Id of the case, the user last viewed.
	 * 
	 * @param lastViewedCaseId
	 *            the caseId
	 */
	public void setLastViewedCaseId(final String lastViewedCaseId) {
		this.lastViewedCaseId = lastViewedCaseId;
	}

}
