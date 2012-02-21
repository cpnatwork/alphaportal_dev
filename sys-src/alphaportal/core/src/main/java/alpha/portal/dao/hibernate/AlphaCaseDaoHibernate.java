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
package alpha.portal.dao.hibernate;

import java.util.List;

import org.appfuse.dao.hibernate.GenericDaoHibernate;
import org.appfuse.model.User;
import org.springframework.stereotype.Repository;

import alpha.portal.dao.AlphaCaseDao;
import alpha.portal.model.AlphaCase;

/**
 * The AlphaCaseDaoHibernate implements the interface AlphaCaseDao.
 * 
 * @see AlphaCaseDao AlphaCaseDAO
 */
@Repository("caseDao")
public class AlphaCaseDaoHibernate extends
		GenericDaoHibernate<AlphaCase, String> implements AlphaCaseDao {

	/**
	 * Instantiates a new case dao hibernate.
	 */
	public AlphaCaseDaoHibernate() {
		super(AlphaCase.class);
	}

	/**
	 * Find by name.
	 * 
	 * @param name
	 *            the name
	 * @return the list
	 * @see alpha.portal.dao.AlphaCaseDao#findByName(java.lang.String)
	 */
	@SuppressWarnings("unchecked")
	public List<AlphaCase> findByName(final String name) {
		return this.getHibernateTemplate().find("from alphacase where name=?",
				name);
	}

	/**
	 * Find by participant.
	 * 
	 * @param user
	 *            the user
	 * @return the list
	 * @see alpha.portal.dao.AlphaCaseDao#findByParticipant(java.lang.String)
	 */
	@SuppressWarnings("unchecked")
	public List<AlphaCase> findByParticipant(final User user) {
		return this
				.getHibernateTemplate()
				.find("from alphacase where ? = some elements(participantsCRA.participants)",
						user);
	}
}
