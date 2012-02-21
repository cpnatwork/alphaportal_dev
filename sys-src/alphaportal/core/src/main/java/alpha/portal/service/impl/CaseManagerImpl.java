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

import java.util.LinkedList;
import java.util.List;

import org.appfuse.model.User;
import org.appfuse.service.impl.GenericManagerImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import alpha.portal.dao.AlphaCaseDao;
import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCase;
import alpha.portal.service.CaseManager;
import alpha.portal.service.CaseService;

/**
 * implements the interface AlphaCaseManager.
 * 
 * @see AlphaCaseManager α-Case Manager
 */
@Service("caseManager")
public class CaseManagerImpl extends GenericManagerImpl<AlphaCase, String>
		implements CaseManager, CaseService {

	/** The case dao. */
	private final AlphaCaseDao caseDao;

	/**
	 * Instantiates a new case manager impl.
	 * 
	 * @param caseDao
	 *            the case dao
	 */
	@Autowired
	public CaseManagerImpl(final AlphaCaseDao caseDao) {
		super(caseDao);
		this.caseDao = caseDao;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see alpha.portal.service.CaseManager#findByName(java.lang.String)
	 */
	public List<AlphaCase> findByName(final String name) {
		return this.caseDao.findByName(name);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * alpha.portal.service.CaseManager#findByParticipant(org.appfuse.model.
	 * User)
	 */
	public List<AlphaCase> findByParticipant(final User user) {
		return this.caseDao.findByParticipant(user);
	}

	/**
	 * Update card order.
	 * 
	 * @param alphaCase
	 *            the alpha case
	 * @param cardIds
	 *            the card ids
	 * @see alpha.portal.service.CaseManager#updateCardOrder(alpha.portal.model.AlphaCase,
	 *      java.util.List)
	 */
	public void updateCardOrder(final AlphaCase alphaCase,
			final List<String> cardIds) {
		if (cardIds.size() < alphaCase.getAlphaCards().size())
			throw new IllegalArgumentException(
					"Not all AlphaCards are in this order-set!");
		final List<AlphaCard> newCards = new LinkedList<AlphaCard>();
		for (final String cardId : cardIds) {
			newCards.add(alphaCase.getAlphaCasePSA().getAlphaCardByCardId(
					cardId));
		}
		alphaCase.getAlphaCasePSA().setAlphaCards(newCards);
		this.save(alphaCase);
	}

	/**
	 * Removes the alpha card.
	 * 
	 * @param card
	 *            the card
	 * @see alpha.portal.service.CaseManager#removeAlphaCard(alpha.portal.model.AlphaCard)
	 */
	public void removeAlphaCard(final AlphaCard card) {
		final AlphaCase aCase = card.getAlphaCase();
		aCase.removeAlphaCard(card);
		this.save(aCase);
	}

	/**
	 * {@inheritDoc}
	 * 
	 * @see CaseService#getCase(String)
	 */
	public AlphaCase getCase(final String caseId) {
		return this.get(caseId);
	}

	/**
	 * {@inheritDoc}
	 * 
	 * @see CaseService#getCases()
	 */
	public List<AlphaCase> getCases() {
		return this.getAll();
	}

	/**
	 * {@inheritDoc}
	 * 
	 * @see CaseService#removeCase(String)
	 */
	public void removeCase(final String caseId) {
		this.remove(caseId);
	}

	/**
	 * {@inheritDoc}
	 * 
	 * @see CaseService#saveCase(AlphaCase)
	 */
	public AlphaCase saveCase(final AlphaCase alphaCase) {
		return this.save(alphaCase);
	}
}