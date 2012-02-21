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

import org.appfuse.model.User;
import org.appfuse.service.GenericManager;

import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCase;

/**
 * Interface CaseManager, which extends from the GenericManager with following
 * objects: α-Case and String.
 */
public interface CaseManager extends GenericManager<AlphaCase, String> {

	/**
	 * Find by name.
	 * 
	 * @param name
	 *            the name
	 * @return the list
	 */
	public List<AlphaCase> findByName(String name);

	/**
	 * Find by participant.
	 * 
	 * @param user
	 *            the user
	 * @return the list
	 */
	public List<AlphaCase> findByParticipant(User user);

	/**
	 * Builds a new AlphaCard list the alphaCase and saves it.
	 * 
	 * @param alphaCase
	 *            the alpha case
	 * @param cardIds
	 *            list of cardId
	 */
	public void updateCardOrder(AlphaCase alphaCase, List<String> cardIds);

	/**
	 * Removes the AlphaCard from the case and saves the case (to persist
	 * card-order).
	 * 
	 * @param card
	 *            the card
	 */
	public void removeAlphaCard(AlphaCard card);

}
