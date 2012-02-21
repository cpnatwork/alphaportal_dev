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
import org.hibernate.criterion.Criterion;

import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCardIdentifier;

/**
 * The AlphaCardDAO is the Data Access Object of the α-Card. It extends from the
 * GenericDao with the following objects: α-Card and α-Card-Identifier.
 */
public interface AlphaCardDao extends
		GenericDao<AlphaCard, AlphaCardIdentifier> {

	/**
	 * Loads all versions of a specific AlphaCard.
	 * 
	 * @param caseId
	 *            the case id
	 * @return the all versions
	 */
	List<AlphaCard> getAllVersions(String caseId);

	/**
	 * Loads a specific version of an AlphaCard.
	 * 
	 * @param id
	 *            identifier with all properties != null.
	 * @return the version
	 */
	AlphaCard getVersion(AlphaCardIdentifier id);

	/**
	 * Loads all AlphaCards matching a given Criterion.
	 * 
	 * @param caseId
	 *            the case id
	 * @param criterions
	 *            can be fetched statically from AlphaCardManagerImpl
	 * @return the list
	 */
	List<AlphaCard> listAlphaCardsByCriterion(String caseId,
			Criterion... criterions);

	/**
	 * List the last 20 changes of AlphaCards in AlphaCases where I take part.
	 * 
	 * @param caseIDs
	 *            A String array with the caseIDs of the cases where I take part
	 * 
	 * @return A list with AlphaCards
	 */
	List<AlphaCard> listDashBoardAlphaCards(String[] caseIDs);

}
