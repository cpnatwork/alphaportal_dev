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

import java.util.Collections;
import java.util.List;
import java.util.Set;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.xml.bind.annotation.XmlTransient;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.apache.commons.lang.builder.ToStringBuilder;
import org.appfuse.model.User;
import org.codehaus.jackson.annotate.JsonIgnore;
import org.hibernate.annotations.GenericGenerator;

/**
 * This is a POJO for the entity α-Case.
 * 
 * It is used to coordinate cooperating parties. Using this α-Case does not
 * require any preinstalled system components, so true adhoc information
 * interchange is enabled.
 */
@Entity(name = "alphacase")
public class AlphaCase {

	/**
	 * The identifier of the α-Case.
	 */
	@Id
	@GeneratedValue(generator = "uuid")
	@GenericGenerator(name = "uuid", strategy = "org.hibernate.id.UUIDGenerator")
	private String caseId;

	/**
	 * The name of α-Case.
	 */
	@Column(name = "name", length = 50)
	private String name;

	/**
	 * the alphaCasePSA.getListOfAlphaCards() of this α-Case.
	 */
	@XmlTransient
	@JsonIgnore
	private final AlphaCasePSA alphaCasePSA;

	/**
	 * the participants of this α-Case.
	 */
	@XmlTransient
	@JsonIgnore
	private final AlphaCaseCRA participantsCRA;

	/**
	 * Instantiates a new alpha case.
	 */
	public AlphaCase() {
		this.alphaCasePSA = new AlphaCasePSA();
		this.participantsCRA = new AlphaCaseCRA();
	}

	/**
	 * Gets the name.
	 * 
	 * @return name
	 */
	public String getName() {
		return this.name;
	}

	/**
	 * Sets the name.
	 * 
	 * @param name
	 *            new name of the α-Case
	 */
	public void setName(final String name) {
		this.name = name;
	}

	/**
	 * Gets the id.
	 * 
	 * @return id
	 */
	public String getCaseId() {
		return this.caseId;
	}

	/**
	 * Sets the id.
	 * 
	 * @param id
	 *            new id of the α-Case.
	 */
	public void setCaseId(final String id) {
		this.caseId = id;
	}

	/**
	 * Gets the list of participants.
	 * 
	 * @return list of participants as <b>unmodifiable</b> set
	 */
	public Set<User> getListOfParticipants() {
		return this.participantsCRA.getListOfParticipants();
	}

	/**
	 * Add a participant to the case.
	 * 
	 * @param participant
	 *            participant, who should be added
	 */
	public void addParticipant(final User participant) {
		this.participantsCRA.addUserToListOfParticipants(participant);
	}

	/**
	 * Removes the participant.
	 * 
	 * @param participant
	 *            participant of the α-Case.
	 * @return true, if present and successfully removed
	 */
	public boolean removeParticipant(final User participant) {
		return this.participantsCRA
				.removeUserFromListOfParticipants(participant);
	}

	/**
	 * Gets the α-Cards as <b>unmodifiable</b> Set.
	 * 
	 * @return α-Card-List
	 */
	public List<AlphaCard> getAlphaCards() {
		return Collections.unmodifiableList(this.alphaCasePSA
				.getListOfAlphaCards());
	}

	/**
	 * adds a α-Card.
	 * 
	 * @param card
	 *            α-Card, which will be added.
	 * @param newPriority
	 *            the new priority
	 */
	public void addAlphaCard(final AlphaCard card, final int newPriority) {
		if (card.getAlphaCase() == null) {
			card.setAlphaCase(this);
		}
		if (!this.alphaCasePSA.getListOfAlphaCards().contains(card)) {
			this.alphaCasePSA.addAlphaCard(card, newPriority);
		}
	}

	/**
	 * Adds an AlphaCard to the list with a specific position.
	 * 
	 * @param card
	 *            the AlphaCard
	 */
	public void addAlphaCard(final AlphaCard card) {
		if (card.getAlphaCase() == null) {
			card.setAlphaCase(this);
		}
		if (!this.alphaCasePSA.getListOfAlphaCards().contains(card)) {
			this.alphaCasePSA.addAlphaCard(card, this.alphaCasePSA
					.getListOfAlphaCards().size());
		}
	}

	/**
	 * Moves an AlphaCard within the list to a specific position.
	 * 
	 * @param card
	 *            the AlphaCard
	 * @param newPriority
	 *            new priority
	 */
	public void moveAlphaCard(final AlphaCard card, final int newPriority) {
		this.alphaCasePSA.moveAlphaCard(card, newPriority);
	}

	/**
	 * Removes the AlphaCard.
	 * 
	 * @param aCard
	 *            the a card
	 * @return true, if present and successfully removed
	 */
	public boolean removeAlphaCard(final AlphaCard aCard) {
		return this.alphaCasePSA.removeAlphaCard(aCard);
	}

	/**
	 * Gets the alphaCasePSA.
	 * 
	 * @return the AlphaCase PSA
	 */
	@XmlTransient
	@JsonIgnore
	public AlphaCasePSA getAlphaCasePSA() {
		return this.alphaCasePSA;
	}

	/**
	 * Gets the participants of this α-Case.
	 * 
	 * @return the participants CRA
	 */
	@XmlTransient
	@JsonIgnore
	public AlphaCaseCRA getParticipantsCRA() {
		return this.participantsCRA;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(final Object other) {
		if (!(other instanceof AlphaCase))
			return false;
		final AlphaCase castOther = (AlphaCase) other;
		return new EqualsBuilder().append(this.caseId, castOther.caseId)
				.append(this.name, castOther.name)
				.append(this.alphaCasePSA, castOther.alphaCasePSA)
				.append(this.participantsCRA, castOther.participantsCRA)
				.isEquals();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return new HashCodeBuilder(1171060467, -2122474045).append(this.caseId)
				.append(this.name).append(this.participantsCRA).toHashCode();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return new ToStringBuilder(this).append("id", this.caseId)
				.append("name", this.name)
				.append("alphaCasePSA", this.alphaCasePSA)
				.append("participantsCRA", this.participantsCRA).toString();
	}

}