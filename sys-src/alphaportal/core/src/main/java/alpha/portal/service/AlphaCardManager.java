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
import org.hibernate.criterion.Criterion;

import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCardIdentifier;
import alpha.portal.model.AlphaCase;
import alpha.portal.service.impl.AlphaCardManagerImpl;

/**
 * Interface AlphaCardManager, which extends from the GenericManager with
 * following objects: α-Card and α-Card-Identifier.
 */
public interface AlphaCardManager extends
		GenericManager<AlphaCard, AlphaCardIdentifier> {

	/**
	 * Creates a new AlphaCard object with all available adornments set to their
	 * default values.
	 * 
	 * @param caseId
	 *            the case id
	 * @return the alpha card
	 */
	public AlphaCard createAlphaCard(String caseId);

	/**
	 * Loads a specific AlphaCard version.
	 * 
	 * @param id
	 *            AlphaCardIdentifier with all attributes set
	 * @return the version
	 */
	public AlphaCard getVersion(final AlphaCardIdentifier id);

	/**
	 * Loads all versions of a specific AlphaCard.
	 * 
	 * @param caseId
	 *            the case id
	 * @return the all versions
	 */
	public List<AlphaCard> getAllVersions(final String caseId);

	/**
	 * Lists alphaCards filtered by Adornment-Properties. All given
	 * {@link Criterion}s will be joined by {@code and}.
	 * 
	 * @param caseId
	 *            the case id
	 * @param criterions
	 *            a array of {@link Criterion}.
	 * @return The intersection of all criterion as a List of {@link AlphaCard}s
	 *         or null if an error occurs
	 * @see AlphaCardManagerImpl for predifined {@link Criterion}
	 */
	public List<AlphaCard> listAlphaCardsByCriterion(String caseId,
			Criterion... criterions);

	/**
	 * Get the last 20 changes of AlphaCards for the Dashboard.
	 * 
	 * @param caseList
	 *            A list with the cases where I take part.
	 * 
	 * @return A list with AlphaCards
	 */
	public List<AlphaCard> listDashBoardCards(List<AlphaCase> caseList);

}
