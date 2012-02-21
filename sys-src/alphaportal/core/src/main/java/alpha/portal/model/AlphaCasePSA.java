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

import java.io.Serializable;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Embeddable;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.JoinTable;
import javax.persistence.ManyToMany;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.apache.commons.lang.builder.ToStringBuilder;
import org.hibernate.annotations.IndexColumn;

/**
 * It is an attribute of an α-Case, containing the list of alpha cards. PSA
 * means "Process Structure Artifact".
 */
@Embeddable
public class AlphaCasePSA implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -6579707319743185225L;

	/**
	 * List of alpha cards. Uses @ManyToMany (instead of @OneToMany) to remove
	 * "unique" constraint on inverseJoinColumns to avoid "duplicate entry"
	 * error on update (reordering uses update on inverseJoinColumns :O). Bug
	 * was fixed in 2010 but hibernate3-maven-plugin was last published on
	 * 21.01.2009
	 * 
	 * @see https://hibernate.onjira.com/browse/HHH-3609
	 */
	@ManyToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER)
	@JoinTable(joinColumns = @JoinColumn(name = "alphacase_caseId", referencedColumnName = "caseId", updatable = false), inverseJoinColumns = {
			@JoinColumn(name = "alphacard_caseId", referencedColumnName = "caseId", updatable = false),
			@JoinColumn(name = "alphacard_cardId", referencedColumnName = "cardId", updatable = false),
			@JoinColumn(name = "alphacard_sequenceNumber", referencedColumnName = "sequenceNumber", updatable = false) })
	@IndexColumn(name = "priority")
	private List<AlphaCard> alphaCards = new LinkedList<AlphaCard>();

	/**
	 * Gets the list of AlphaCards.
	 * 
	 * @return list of AlphaCards as <b>unmodifiable</b> set
	 */
	public List<AlphaCard> getListOfAlphaCards() {
		return Collections.unmodifiableList(this.alphaCards);
	}

	/**
	 * Clear the list of AlphaCards.
	 */
	public void clearAlphaCards() {
		this.alphaCards.clear();
	}

	/**
	 * Sets the whole new List alphaCards.
	 * 
	 * @param alphaCards
	 *            the new list of alpha cards
	 */
	public void setAlphaCards(final List<AlphaCard> alphaCards) {
		this.alphaCards = alphaCards;
	}

	/**
	 * Searches alphaCards for the given cardId. Returns null on failure.
	 * 
	 * @param cardId
	 *            the card id
	 * @return the alpha card by card id
	 */
	public AlphaCard getAlphaCardByCardId(final String cardId) {
		for (final AlphaCard c : this.alphaCards) {
			if (c.getAlphaCardIdentifier().getCardId().equals(cardId))
				return c;
		}
		return null;
	}

	/**
	 * Add a AlphaCard to AlphaCards list to a specific position.
	 * 
	 * @param card
	 *            the card
	 * @param priority
	 *            the priority
	 * @return true, if successful
	 */
	public boolean addAlphaCard(final AlphaCard card, final int priority) {
		this.alphaCards.add(priority, card);
		return true;
	}

	/**
	 * Moves an AlphaCard within the list to a specific position.
	 * 
	 * @param card
	 *            the AlphaCard
	 * @param newPriority
	 *            new priority
	 * @return true, if successful
	 */
	public boolean moveAlphaCard(final AlphaCard card, final int newPriority) {
		this.alphaCards.remove(card);
		this.addAlphaCard(card, newPriority);
		return true;
	}

	/**
	 * Removes the AlphaCard from list of AlphaCards.
	 * 
	 * @param card
	 *            the card
	 * @return true if present and successfully removed
	 */
	public boolean removeAlphaCard(final AlphaCard card) {
		return this.alphaCards.remove(card);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(final Object other) {
		if (!(other instanceof AlphaCasePSA))
			return false;
		final AlphaCasePSA castOther = (AlphaCasePSA) other;
		return new EqualsBuilder().append(
				this.alphaCards.toArray(new AlphaCard[] {}),
				castOther.alphaCards.toArray(new AlphaCard[] {})).isEquals();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return new HashCodeBuilder(-1638945841, 1946728535).append(
				this.alphaCards).toHashCode();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return new ToStringBuilder(this).append("alphacards", this.alphaCards)
				.toString();
	}

}
