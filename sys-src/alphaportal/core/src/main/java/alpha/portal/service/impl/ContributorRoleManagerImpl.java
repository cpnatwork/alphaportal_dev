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

import org.appfuse.service.impl.GenericManagerImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import alpha.portal.dao.ContributorRoleDao;
import alpha.portal.model.ContributorRole;
import alpha.portal.service.ContributorRoleManager;

/**
 * The Class ContributorRoleManagerImpl.
 */
@Service("contributorRoleManager")
public class ContributorRoleManagerImpl extends
		GenericManagerImpl<ContributorRole, Long> implements
		ContributorRoleManager {

	/** The contributor role dao. */
	private final ContributorRoleDao contributorRoleDao;

	/**
	 * Instantiates a new contributor role manager impl.
	 * 
	 * @param contributorRoleDao
	 *            the contributor role dao
	 */
	@Autowired
	public ContributorRoleManagerImpl(
			final ContributorRoleDao contributorRoleDao) {
		super(contributorRoleDao);
		this.contributorRoleDao = contributorRoleDao;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * alpha.portal.service.ContributorRoleManager#getContributorRoleByName(
	 * java.lang.String)
	 */
	public ContributorRole getContributorRoleByName(final String name) {
		return this.contributorRoleDao.getContributorRoleByName(name);
	}
}
