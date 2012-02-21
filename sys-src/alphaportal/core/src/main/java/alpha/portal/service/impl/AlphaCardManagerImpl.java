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

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.appfuse.service.impl.GenericManagerImpl;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.Restrictions;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import alpha.portal.dao.AlphaCardDao;
import alpha.portal.model.Adornment;
import alpha.portal.model.AdornmentType;
import alpha.portal.model.AdornmentTypeDataProvision;
import alpha.portal.model.AdornmentTypeDeleted;
import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCardDescriptor;
import alpha.portal.model.AlphaCardIdentifier;
import alpha.portal.model.AlphaCase;
import alpha.portal.model.Payload;
import alpha.portal.service.AlphaCardManager;
import alpha.portal.service.AlphaCardService;
import alpha.portal.service.CaseManager;
import alpha.portal.service.PayloadManager;

/**
 * implements the interface AlphaCardManager.
 * 
 * @see AlphaCardManager α-Card Manager
 */
@Service("alphaCardManager")
public class AlphaCardManagerImpl extends
		GenericManagerImpl<AlphaCard, AlphaCardIdentifier> implements
		AlphaCardManager, AlphaCardService {

	/** The Constant _PROVISION. */
	public static final Criterion _PROVISION = Restrictions.eq("ad.name",
			AdornmentType.DataProvision.getName());

	/** The Constant _PROVISION_OPEN. */
	public static final Criterion _PROVISION_OPEN = Restrictions.eq("ad.value",
			AdornmentTypeDataProvision.OPEN.value());

	/** The Constant _PROVISION_INPROGRESS. */
	public static final Criterion _PROVISION_INPROGRESS = Restrictions.eq(
			"ad.value", AdornmentTypeDataProvision.INPROGRESS.value());

	/** The Constant _PROVISION_OPEN_INPROGRESS. */
	public static final Criterion _PROVISION_OPEN_INPROGRESS = Restrictions.or(
			AlphaCardManagerImpl._PROVISION_OPEN,
			AlphaCardManagerImpl._PROVISION_INPROGRESS);

	/** The Constant _PROVISION_FULFILLED. */
	public static final Criterion _PROVISION_FULFILLED = Restrictions.eq(
			"ad.value", AdornmentTypeDataProvision.FULLFILLED.value());

	/** The Constant _CONTRIBUTOR. */
	public static final Criterion _CONTRIBUTOR = Restrictions.eq("ad.name",
			AdornmentType.Contributor.getName());

	/** The Constant _CONTRIBUTORROLE. */
	public static final Criterion _CONTRIBUTORROLE = Restrictions.eq("ad.name",
			AdornmentType.ContributorRole.getName());

	/** The Constant _DELETE_STATUS. */
	private static final Criterion _DELETE_STATUS = Restrictions.eq("ad.name",
			AdornmentType.Deleted.getName());

	/** The Constant _NOT_DELETED. */
	private static final Criterion _NOT_DELETED = Restrictions.eq("ad.value",
			AdornmentTypeDeleted.FALSE.value());

	/** The Constant _DELETED. */
	private static final Criterion _DELETED = Restrictions.eq("ad.value",
			AdornmentTypeDeleted.TRUE.value());

	/** Restriction for the filter element "open". */
	public static final Criterion DATA_PROVISION_OPEN = Restrictions.and(
			AlphaCardManagerImpl._PROVISION,
			AlphaCardManagerImpl._PROVISION_OPEN);

	/** Restriction for the filter element "open or inprogress". */
	public static final Criterion DATA_PROVISION_OPEN_INPROGRESS = Restrictions
			.and(AlphaCardManagerImpl._PROVISION,
					AlphaCardManagerImpl._PROVISION_OPEN_INPROGRESS);

	/** Restriction for the filter element "inprogress". */
	public static final Criterion DATA_PROVISION_INPROGRESS = Restrictions.and(
			AlphaCardManagerImpl._PROVISION,
			AlphaCardManagerImpl._PROVISION_INPROGRESS);

	/** Restriction for the filter element "fulfilled". */
	public static final Criterion DATA_PROVISION_FULFILLED = Restrictions.and(
			AlphaCardManagerImpl._PROVISION,
			AlphaCardManagerImpl._PROVISION_FULFILLED);

	/** Deleted-Restriction: Restriction for the filter element "notDeleted". */
	public static final Criterion NOT_DELETED = Restrictions.and(
			AlphaCardManagerImpl._DELETE_STATUS,
			AlphaCardManagerImpl._NOT_DELETED);

	/** Deleted-Restriction: Restriction for the filter element "deleted". */
	public static final Criterion DELETED = Restrictions.and(
			AlphaCardManagerImpl._DELETE_STATUS, AlphaCardManagerImpl._DELETED);

	/** The payload manager. */
	@Autowired
	PayloadManager payloadManager;

	/** The case manager. */
	@Autowired
	CaseManager caseManager;

	/**
	 * Instantiates a new alpha card manager impl.
	 * 
	 * @param alphaCardDao
	 *            the alpha card dao
	 */
	@Autowired
	public AlphaCardManagerImpl(final AlphaCardDao alphaCardDao) {
		super(alphaCardDao);
	}

	/**
	 * Sets the payload manager.
	 * 
	 * @param payloadManager
	 *            the new payload manager
	 */
	public void setPayloadManager(final PayloadManager payloadManager) {
		this.payloadManager = payloadManager;
	}

	/**
	 * Creates the alpha card.
	 * 
	 * @param caseId
	 *            the case id
	 * @return the alpha card
	 * @see alpha.portal.service.AlphaCardManager#createAlphaCard(java.lang.String)
	 */
	public AlphaCard createAlphaCard(final String caseId) {
		final AlphaCard alphaCard = new AlphaCard(caseId);
		final AlphaCardDescriptor descriptor = alphaCard
				.getAlphaCardDescriptor();

		for (final AdornmentType type : AdornmentType.values()) {
			descriptor.setAdornment(type.getName(), type.getValueDefault());
		}
		return alphaCard;
	}

	/**
	 * Gets the version.
	 * 
	 * @param id
	 *            the id
	 * @return the version
	 * @see alpha.portal.service.AlphaCardManager#getVersion(alpha.portal.model.AlphaCardIdentifier)
	 */
	public AlphaCard getVersion(final AlphaCardIdentifier id) {
		return ((AlphaCardDao) this.dao).getVersion(id);
	}

	/**
	 * Gets the all versions.
	 * 
	 * @param caseId
	 *            the case id
	 * @return the all versions
	 * @see alpha.portal.service.AlphaCardManager#getAllVersions(java.lang.String,
	 *      java.lang.String)
	 */
	public List<AlphaCard> getAllVersions(final String caseId) {
		return ((AlphaCardDao) this.dao).getAllVersions(caseId);
	}

	/**
	 * Overridden to save the AlphaCase, too, to connect it with the newest
	 * version.
	 * 
	 * @param card
	 *            the card
	 * @return the alpha card
	 */
	@Override
	public AlphaCard save(AlphaCard card) {
		final AlphaCase aCase = card.getAlphaCase();
		final int pos = aCase.getAlphaCards().indexOf(card);
		if (pos > -1) {
			card.getAlphaCase().removeAlphaCard(card);
		}
		card = ((AlphaCardDao) this.dao).save(card);
		if (pos > -1) {
			aCase.addAlphaCard(card, pos);
		} else {
			aCase.addAlphaCard(card);
		}
		card.setAlphaCase(this.caseManager.save(aCase));
		return card;
	}

	/**
	 * {@inheritDoc}
	 * 
	 * @see AlphaCardService#getCard(String, String)
	 */
	public AlphaCard getCard(final String cardId, final String caseId) {
		return this.get(new AlphaCardIdentifier(caseId, cardId));
	}

	/**
	 * {@inheritDoc}
	 * 
	 * @see AlphaCardService#addCard(AlphaCard)
	 */
	public AlphaCard addCard(final AlphaCard alphaCard) {
		return this.saveCard(alphaCard);
	}

	/**
	 * {@inheritDoc}
	 * 
	 * @see AlphaCardService#saveCard(AlphaCard)
	 */
	public AlphaCard saveCard(final AlphaCard alphaCard) {
		return this.save(alphaCard);
	}

	/**
	 * {@inheritDoc}
	 * 
	 * @see AlphaCardService#deleteCard(String, String)
	 */
	public void deleteCard(final String cardId, final String caseId) {
		this.remove(new AlphaCardIdentifier(caseId, cardId));
	}

	/**
	 * {@inheritDoc}
	 * 
	 * @see AlphaCardService#getAdornment(String, String, String)
	 */
	public Adornment getAdornment(final String cardId, final String caseId,
			final String adornmentName) {
		final AlphaCard alphaCard = this.getCard(cardId, caseId);
		if (alphaCard != null) {
			final AlphaCardDescriptor descriptor = alphaCard
					.getAlphaCardDescriptor();
			return descriptor.getAdornment(adornmentName);
		}
		return null;
	}

	/**
	 * {@inheritDoc}
	 * 
	 * @see AlphaCardService#addAdornment(String, String, Adornment)
	 */
	public Adornment addAdornment(final String cardId, final String caseId,
			final Adornment adornment) {
		return this.saveAdornment(cardId, caseId, adornment);
	}

	/**
	 * {@inheritDoc}
	 * 
	 * @see AlphaCardService#saveAdornment(String, String, Adornment)
	 */
	public Adornment saveAdornment(final String cardId, final String caseId,
			Adornment adornment) {
		AlphaCard alphaCard = this.getCard(cardId, caseId);
		if (alphaCard != null) {
			final AlphaCardDescriptor descriptor = alphaCard
					.getAlphaCardDescriptor();
			descriptor.setAdornment(adornment);
			alphaCard = this.save(alphaCard);
			adornment = alphaCard.getAlphaCardDescriptor().getAdornment(
					adornment.getName());
		}
		return adornment;
	}

	/**
	 * {@inheritDoc}
	 * 
	 * @see AlphaCardService#deleteAdornment(String, String, String)
	 */
	public void deleteAdornment(final String cardId, final String caseId,
			final String adornmentName) {
		final AlphaCard alphaCard = this.getCard(cardId, caseId);
		if (alphaCard != null) {
			final AlphaCardDescriptor descriptor = alphaCard
					.getAlphaCardDescriptor();
			if (descriptor.deleteAdornment(adornmentName)) {
				this.save(alphaCard);
			}
		}
	}

	/**
	 * {@inheritDoc}
	 * 
	 * @see AlphaCardService#getPayload(String, String)
	 */
	public Payload getPayload(final String cardId, final String caseId) {
		final AlphaCard alphaCard = this.getCard(cardId, caseId);
		if (alphaCard != null)
			return alphaCard.getPayload();
		return null;
	}

	/**
	 * {@inheritDoc}
	 * 
	 * @see AlphaCardService#setPayload(String, String, Payload)
	 */
	public Payload setPayload(final String cardId, final String caseId,
			final Payload payload) {
		final AlphaCard alphaCard = this.getCard(cardId, caseId);
		if ((alphaCard != null) && (payload != null))
			return this.payloadManager.saveNewPayload(payload, alphaCard);
		return null;
	}

	/**
	 * {@inheritDoc}
	 * 
	 * @see alpha.portal.service.AlphaCardManager#listAlphaCardsByCriterion(org.hibernate.criterion.Criterion[])
	 */
	public List<AlphaCard> listAlphaCardsByCriterion(final String caseId,
			final Criterion... criterions) {
		if (StringUtils.isBlank(caseId))
			return null;

		return ((AlphaCardDao) this.dao).listAlphaCardsByCriterion(caseId,
				criterions);
	}

	/**
	 * Returns a {@link Criterion} to get all Adornments with the given userId
	 * as contributor.
	 * 
	 * @param userId
	 *            the users id
	 * @return a {@link Criterion}
	 */
	public static Criterion getContributorCriterionOwn(final String userId) {
		return Restrictions.and(AlphaCardManagerImpl._CONTRIBUTOR,
				Restrictions.eq("ad.value", userId));
	}

	/**
	 * Returns a {@link Criterion} to get all Adornments where the given userId
	 * is not the contributor.
	 * 
	 * @param userId
	 *            the users id
	 * @return a {@link Criterion}
	 */
	public static Criterion getContributorCriterionOthers(final String userId) {
		return Restrictions.and(AlphaCardManagerImpl._CONTRIBUTOR,
				Restrictions.ne("ad.value", userId));
	}

	/**
	 * Returns a {@link Criterion} to get all Adornments where the
	 * contributorrole is one of roles.
	 * 
	 * @param roles
	 *            the roles
	 * @return a {@link Criterion}
	 */
	public static Criterion getContributorRoleCriterionOwn(
			final String... roles) {
		return Restrictions.and(AlphaCardManagerImpl._CONTRIBUTORROLE,
				Restrictions.in("ad.value", roles));
	}

	/**
	 * Returns a {@link Criterion} to get all Adornments where the
	 * contributorrole is not in roles.
	 * 
	 * @param roles
	 *            the roles
	 * @return a {@link Criterion}
	 */
	public static Criterion getContributorRoleCriterionOthers(
			final String... roles) {
		return Restrictions.and(AlphaCardManagerImpl._CONTRIBUTORROLE,
				Restrictions.not(Restrictions.in("ad.value", roles)));
	}

	/**
	 * Get the last 20 changes of AlphaCards for the Dashboard.
	 * 
	 * @param caseList
	 *            A list with the cases where I take part.
	 * 
	 * @return A list with AlphaCards
	 */
	public List<AlphaCard> listDashBoardCards(final List<AlphaCase> caseList) {
		final List<AlphaCard> result = new ArrayList<AlphaCard>();

		final String[] caseIDs = new String[caseList.size()];
		int i = 0;
		for (final AlphaCase ac : caseList) {
			caseIDs[i] = ac.getCaseId();
			i++;
		}

		return ((AlphaCardDao) this.dao).listDashBoardAlphaCards(caseIDs);
	}

	/**
	 * Gets the case manager.
	 * 
	 * @return the case manager
	 */
	public CaseManager getCaseManager() {
		return this.caseManager;
	}

	/**
	 * Sets the case manager.
	 * 
	 * @param caseManager
	 *            the new case manager
	 */
	public void setCaseManager(final CaseManager caseManager) {
		this.caseManager = caseManager;
	}
}
