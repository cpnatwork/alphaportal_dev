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

import java.util.LinkedList;
import java.util.List;
import java.util.UUID;

import org.appfuse.dao.hibernate.GenericDaoHibernate;
import org.hibernate.Criteria;
import org.hibernate.Query;
import org.hibernate.classic.Session;
import org.hibernate.criterion.CriteriaSpecification;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Property;
import org.hibernate.criterion.Restrictions;
import org.hibernate.criterion.Subqueries;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.stereotype.Repository;

import alpha.portal.dao.AlphaCardDao;
import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCardDescriptor;
import alpha.portal.model.AlphaCardIdentifier;

/**
 * The AlphaCardDaoHibernate implements the interface AlphaCardDao.
 * 
 * @see AlphaCardDao AlphaCardDAO
 */
@Repository("alphaCardDao")
public class AlphaCardDaoHibernate extends
		GenericDaoHibernate<AlphaCard, AlphaCardIdentifier> implements
		AlphaCardDao {
	/**
	 * Instantiates a new alpha card dao hibernate.
	 */
	public AlphaCardDaoHibernate() {
		super(AlphaCard.class);
	}

	/**
	 * Gets the all versions.
	 * 
	 * @param caseId
	 *            the case id
	 * @return the all versions
	 * @see alpha.portal.dao.AlphaCardDao#getAllVersions(java.lang.String,
	 *      java.lang.String)
	 */
	@SuppressWarnings("unchecked")
	public List<AlphaCard> getAllVersions(final String caseId) {
		return this
				.getHibernateTemplate()
				.find("from alphacard where alphaCardIdentifier.caseId = ? order by alphaCardIdentifier.sequenceNumber desc",
						caseId);
	}

	/**
	 * Gets the version.
	 * 
	 * @param id
	 *            the id
	 * @return the version
	 * @see alpha.portal.dao.AlphaCardDao#getVersion(alpha.portal.model.AlphaCardIdentifier)
	 */
	public AlphaCard getVersion(final AlphaCardIdentifier id) {
		final List<AlphaCard> cards = this
				.getHibernateTemplate()
				.find("from alphacard where caseid = ? and cardid = ? and sequenceNumber = ?",
						id.getCaseId(), id.getCardId(), id.getSequenceNumber());
		if (cards.size() > 0)
			return cards.get(0);
		else
			return null;
	}

	/**
	 * List alpha cards by criterion.
	 * 
	 * @param caseId
	 *            the case id
	 * @param criteriaArray
	 *            the criteria array
	 * @return the list
	 * @see alpha.portal.dao.AlphaCardDao#listAlphaCardsByCriterion(org.hibernate.criterion.Criterion)
	 */
	public List<AlphaCard> listAlphaCardsByCriterion(final String caseId,
			final Criterion... criteriaArray) {
		Session session;
		boolean sessionOwn = false;
		try {
			session = this.getSessionFactory().getCurrentSession();
		} catch (final Exception e) {
			session = this.getSessionFactory().openSession();
			sessionOwn = true;
		}

		// get newest sequenceNumber for each cardId
		final DetachedCriteria version = DetachedCriteria
				.forClass(AlphaCard.class, "cardVersion")
				.add(Property.forName("card.alphaCardIdentifier.cardId")
						.eqProperty("cardVersion.alphaCardIdentifier.cardId"))
				.setProjection(
						Projections
								.projectionList()
								.add(Projections
										.max("alphaCardIdentifier.sequenceNumber")));

		final Criteria crit = session.createCriteria(AlphaCard.class, "card");
		for (final Criterion c : criteriaArray) {
			// Create the subquery (somehow we need to use the Descriptor or
			// else the subquery-JOIN is not done)
			final DetachedCriteria subcrit = DetachedCriteria.forClass(
					AlphaCardDescriptor.class, "crit");

			// Join the adornments
			subcrit.createAlias("crit.adornmentList", "ad");

			// Add adornment condition
			subcrit.add(c);

			// Map the subquery back to the outer query
			subcrit.add(Restrictions.eqProperty("card.alphaCardIdentifier",
					"crit.alphaCardIdentifier"));

			// Narrow down subquery or else we get a NullPointer-Exception
			subcrit.setProjection(Projections
					.property("crit.alphaCardIdentifier.cardId"));

			// Add this subquery to the outer query.
			crit.add(Subqueries.exists(subcrit));
		}
		crit.setResultTransformer(CriteriaSpecification.DISTINCT_ROOT_ENTITY)
				.add(Property.forName("alphaCardIdentifier.sequenceNumber").eq(
						version)).createAlias("alphaCase", "case")
				.add(Restrictions.eq("case.caseId", caseId));

		List<AlphaCard> list = crit.list();
		if (list.size() > 1) {
			final List<AlphaCard> order = (list.get(0)).getAlphaCase()
					.getAlphaCards();
			final List<AlphaCard> orderedList = new LinkedList<AlphaCard>();
			for (final AlphaCard cc : order) {
				for (final AlphaCard c : list) {
					if (c.getAlphaCardIdentifier().equals(
							cc.getAlphaCardIdentifier())) {
						orderedList.add(c);
						break;
					}
				}
			}
			list = orderedList;
		}

		if (sessionOwn) {
			session.close();
		}

		return list;
	}

	/**
	 * Gets the latest version of the given id (ignoring its sequenceNumber).
	 * 
	 * @param id
	 *            the id
	 * @return the alpha card
	 * @see org.appfuse.dao.hibernate.GenericDaoHibernate#get(java.io.Serializable)
	 */
	@Override
	public AlphaCard get(final AlphaCardIdentifier id) {
		// load latest version
		final List res = this
				.getHibernateTemplate()
				.find("select max(cast(sequenceNumber, integer)) from alphacard where caseid = ? and cardid = ?",
						id.getCaseId(), id.getCardId());
		// if there is none return null
		if (res.size() == 0)
			return null;
		final Integer sequenceNumber = (Integer) res.get(0);
		// load card of that version
		final List cards = this
				.getHibernateTemplate()
				.find("from alphacard where caseid = ? and cardid = ? and sequenceNumber = ?",
						id.getCaseId(), id.getCardId(), sequenceNumber);
		if (cards.size() > 0)
			return (AlphaCard) cards.get(0);
		else
			return null;
	}

	/**
	 * Overridden to use our get() function.
	 * 
	 * @param id
	 *            the id
	 * @return true, if successful
	 * @see org.appfuse.dao.hibernate.GenericDaoHibernate#exists(java.io.Serializable)
	 */
	@Override
	public boolean exists(final AlphaCardIdentifier id) {
		final AlphaCard entity = id.getSequenceNumber() == null ? this.get(id)
				: this.getVersion(id);
		return entity != null;
	}

	/**
	 * Overridden to deny deleting of AlphaCards.
	 * 
	 * @param id
	 *            the id
	 * @see org.appfuse.dao.hibernate.GenericDaoHibernate#remove(java.io.Serializable)
	 */
	@Override
	public void remove(final AlphaCardIdentifier id) {
		throw new AccessDeniedException("AlphaCards cannot be deleted");
	}

	/**
	 * Overridden to generate UUID and for versioning. Clones the descriptor and
	 * increases the sequenceNumber before saving.
	 * 
	 * @param card
	 *            the card
	 * @return the alpha card
	 * @see org.appfuse.dao.hibernate.GenericDaoHibernate#save(java.lang.Object)
	 */
	@Override
	public AlphaCard save(final AlphaCard card) {
		if (card.getAlphaCardIdentifier() != null) {
			if (card.getAlphaCardIdentifier().getCardId() == null) {
				card.getAlphaCardIdentifier().setCardId(
						UUID.randomUUID().toString());
			} else {
				this.getHibernateTemplate().flush();
				this.getHibernateTemplate().evict(card);
				this.getHibernateTemplate().clear();
			}
			card.setAlphaCardDescriptor(card.getAlphaCardDescriptor().clone());

			final Long newSequenceNumber = this.getLastValue() + 1L;
			card.getAlphaCardIdentifier().setSequenceNumber(newSequenceNumber);
			card.getAlphaCardDescriptor().getAlphaCardIdentifier()
					.setSequenceNumber(newSequenceNumber);
		}
		return super.save(card);
	}

	/**
	 * Internal function to load the highest sequenceNumber from the AlphaCard
	 * table.
	 * 
	 * @return 0 if no record is found
	 */
	private Long getLastValue() {
		try {
			final List list = this
					.getHibernateTemplate()
					.find("select max(c.alphaCardIdentifier.sequenceNumber) from alphacard c");
			Long value = (Long) list.get(0);
			if (value == null) {
				value = 0L;
			}
			return value.longValue();
		} catch (final Exception e) {
			e.printStackTrace();
			this.log.error("Failed to get last value of sequenceNumber!");
			return 0L;
		}
	}

	/**
	 * List the last 20 changes of AlphaCards in AlphaCases where I take part.
	 * 
	 * @param caseIDs
	 *            A String array with the caseIDs of the cases where I take part
	 * 
	 * @return A list with AlphaCards
	 */
	public List<AlphaCard> listDashBoardAlphaCards(final String[] caseIDs) {
		Session session;
		boolean sessionOwn = false;
		try {
			session = this.getSessionFactory().getCurrentSession();
		} catch (final Exception e) {
			session = this.getSessionFactory().openSession();
			sessionOwn = true;
		}

		String queryStr = "from alphacard where";
		// add all caseIDs in 'where' with OR clause
		for (int i = 0; i < caseIDs.length; i++) {
			queryStr = queryStr.concat(" alphaCardIdentifier.caseId = " + "'"
					+ caseIDs[i] + "'");
			if (i < (caseIDs.length - 1)) {
				queryStr = queryStr.concat(" or");
			}
		}

		// order the result descending
		queryStr = queryStr
				.concat(" order by alphaCardIdentifier.sequenceNumber DESC");

		final Query query = session.createQuery(queryStr);
		// only the top 20 results
		query.setMaxResults(20);

		return query.list();
	}
}
