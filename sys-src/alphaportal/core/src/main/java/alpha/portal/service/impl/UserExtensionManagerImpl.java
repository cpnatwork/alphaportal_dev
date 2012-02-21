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
package alpha.portal.service.impl;

import java.util.List;

import org.appfuse.service.impl.GenericManagerImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import alpha.portal.dao.UserExtensionDao;
import alpha.portal.model.ContributorRole;
import alpha.portal.model.UserExtension;
import alpha.portal.service.UserExtensionManager;

/**
 * implements the interface UserExtensionManager.
 * 
 * @see UserExtensionManager User Extension Manager
 */
@Service("userExtensionManager")
public class UserExtensionManagerImpl extends
		GenericManagerImpl<UserExtension, Long> implements UserExtensionManager {

	/** The user extension dao. */
	private final UserExtensionDao userExtensionDao;

	/**
	 * Instantiates a new user extension manager impl.
	 * 
	 * @param ueDao
	 *            the ue dao
	 */
	@Autowired
	public UserExtensionManagerImpl(final UserExtensionDao ueDao) {
		super(ueDao);
		this.userExtensionDao = ueDao;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * alpha.portal.service.UserExtensionManager#getUserExtensionsByContributorRole
	 * (alpha.portal.model.ContributorRole)
	 */
	public List<UserExtension> getUserExtensionsByContributorRole(
			final ContributorRole role) {
		return this.userExtensionDao.getUserExtensionsByContributorRole(role);
	}

}
