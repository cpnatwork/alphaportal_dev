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

import javax.persistence.CascadeType;
import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.PrimaryKeyJoinColumn;
import javax.xml.bind.annotation.XmlTransient;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.apache.commons.lang.builder.ToStringBuilder;
import org.codehaus.jackson.annotate.JsonIgnore;

/**
 * An α-Card is a document of a α-Case.It should only contain e.g. one
 * diagnostic finding, piece of clinical evidence, therapeutic measure or
 * prescriptionic address information for the other participants. An α-Card is
 * build up of an α-Card-Identifier, α-Card-Descriptor,Payload.
 */
@Entity(name = "alphacard")
public final class AlphaCard {

	/**
	 * the α-Card-Identifier, which identifies the α-Card.
	 * 
	 * @see AlphaCardIdentifier α-Card-Identifier
	 */
	@EmbeddedId
	private AlphaCardIdentifier alphaCardIdentifier;

	/**
	 * the α-Card-Descriptor, which describes the α-Card.
	 * 
	 * @see AlphaCardDescriptor α-Card-Descriptor
	 */
	@OneToOne(cascade = CascadeType.ALL, fetch = FetchType.EAGER, orphanRemoval = true)
	@PrimaryKeyJoinColumn
	private AlphaCardDescriptor alphaCardDescriptor;

	/**
	 * reference to the the α-Case, the α-Card belongs to.
	 * 
	 * @see AlphaCase α-Case
	 */
	@XmlTransient
	@JsonIgnore
	@ManyToOne(cascade = CascadeType.ALL, fetch = FetchType.EAGER, optional = false)
	@JoinColumn(name = "caseId", referencedColumnName = "caseId", insertable = false, updatable = false)
	private AlphaCase alphaCase;

	/**
	 * the payload of the the α-Card.
	 * 
	 * @see Payload Payload
	 */
	@OneToOne(fetch = FetchType.EAGER, optional = true)
	private Payload payload;

	/**
	 * Default constructor which automatically links this object to a new
	 * descriptor and identifier.
	 */
	public AlphaCard() {
		this.setAlphaCardIdentifier(new AlphaCardIdentifier());
		this.setAlphaCardDescriptor(new AlphaCardDescriptor(this));
	}

	/**
	 * Constructor which sets the caseId of its new identifier and also hands it
	 * to a new descriptor.
	 * 
	 * @param caseId
	 *            the case id
	 */
	public AlphaCard(final String caseId) {
		this.setAlphaCardIdentifier(new AlphaCardIdentifier(caseId));
		this.setAlphaCardDescriptor(new AlphaCardDescriptor(this));
	}

	/**
	 * Constructor with a fully identified (caseId + cardId) descriptor.
	 * 
	 * @param desc
	 *            Fully identified, which means caseId and cardId are already
	 *            set!
	 */
	public AlphaCard(final AlphaCardDescriptor desc) {
		if ((desc.getAlphaCardIdentifier() == null)
				|| StringUtils.isEmpty(desc.getAlphaCardIdentifier()
						.getCardId())
				|| StringUtils.isEmpty(desc.getAlphaCardIdentifier()
						.getCaseId()))
			throw new IllegalStateException(
					"Creating a card with an empty descriptor is not intended. "
							+ "AlphaCard is the stronger entity so please use "
							+ "new AlphaCard([caseId]) which automatically "
							+ "creates a new descriptor.");
		this.setAlphaCardDescriptor(desc); // automatically takes desc's
											// Identifier into the card
	}

	/**
	 * Constructor which takes the id of a case.
	 * 
	 * @param aCase
	 *            Needs to be already saved (have an id).
	 */
	public AlphaCard(final AlphaCase aCase) {
		if (StringUtils.isEmpty(aCase.getCaseId()))
			throw new IllegalStateException(
					"AlphaCard cannot be initialized with an unsaved case!");
		this.setAlphaCardIdentifier(new AlphaCardIdentifier(aCase.getCaseId()));
		this.setAlphaCardDescriptor(new AlphaCardDescriptor(this));
		this.setAlphaCase(aCase);
	}

	/**
	 * gets the α-Case.
	 * 
	 * @return alphaCase
	 */
	@XmlTransient
	@JsonIgnore
	public AlphaCase getAlphaCase() {
		return this.alphaCase;
	}

	/**
	 * Links the α-Card with a α-Case. This method will also add this card to
	 * the list of cards of the given case and take the case's caseId.
	 * 
	 * @param alphaCase
	 *            the new reference to the the α-Case, the α-Card belongs to
	 */
	public void setAlphaCase(final AlphaCase alphaCase) {
		this.alphaCase = alphaCase;
		if ((this.alphaCardIdentifier != null)
				&& (this.alphaCardIdentifier.getCaseId() == null)
				&& (alphaCase != null) && (alphaCase.getCaseId() != null)) {
			this.alphaCardIdentifier.setCaseId(alphaCase.getCaseId());
		}
	}

	/**
	 * gets the identifier of the α-Card.
	 * 
	 * @return α-Card-Identifier
	 */
	public AlphaCardIdentifier getAlphaCardIdentifier() {
		return this.alphaCardIdentifier;
	}

	/**
	 * Adds the identifier and also hands it to the connected descriptor since
	 * those always need to be equal.
	 * 
	 * @param alphaCardId
	 *            α-Card-identifier, which is to be set.
	 */
	public void setAlphaCardIdentifier(final AlphaCardIdentifier alphaCardId) {
		this.alphaCardIdentifier = alphaCardId;
		if (this.alphaCardDescriptor != null) {
			this.alphaCardDescriptor.setAlphaCardIdentifier(alphaCardId);
		}
	}

	/**
	 * gets the α-Card-Descriptor.
	 * 
	 * @return alphaCardDescriptor
	 */
	public AlphaCardDescriptor getAlphaCardDescriptor() {
		return this.alphaCardDescriptor;
	}

	/**
	 * Links the α-Card with a α-Card-Descriptor and also takes the identifier
	 * of the α-Card-Descriptor, if it is "better" than this α-Card's
	 * identifier.
	 * 
	 * @param alphaCardDescriptor
	 *            alphaCardDescriptor, which has to be set.
	 */
	public void setAlphaCardDescriptor(
			final AlphaCardDescriptor alphaCardDescriptor) {

		this.alphaCardDescriptor = alphaCardDescriptor;
		if (this.alphaCardDescriptor == null)
			return;

		// set this alphaCard on the descriptor
		if (this.alphaCardDescriptor.getAlphaCard() == null) {
			this.alphaCardDescriptor.setAlphaCard(this);
		}

		// get the alphaCardIdentifier from the descriptor and set it too
		if ((this.alphaCardIdentifier == null)
				&& (this.alphaCardDescriptor.getAlphaCardIdentifier() != null)) {
			this.alphaCardIdentifier = this.alphaCardDescriptor
					.getAlphaCardIdentifier();
		}

	}

	/**
	 * gets the payload.
	 * 
	 * @return the payload
	 */
	public Payload getPayload() {
		return this.payload;
	}

	/**
	 * sets the payload.
	 * 
	 * @param payload
	 *            payload, which has to be set
	 */
	public void setPayload(final Payload payload) {
		this.payload = payload;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(final Object other) {
		if (!(other instanceof AlphaCard))
			return false;
		final AlphaCard castOther = (AlphaCard) other;
		return new EqualsBuilder()
				.append(this.alphaCardIdentifier, castOther.alphaCardIdentifier)
				.append(this.alphaCardDescriptor, castOther.alphaCardDescriptor)
				.append(this.payload, castOther.payload).isEquals();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(this.alphaCardIdentifier)
				.append(this.alphaCardDescriptor).append(this.payload)
				.toHashCode();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("alphaCardIdentifier", this.alphaCardIdentifier)
				.append("alphaCardDescriptor", this.alphaCardDescriptor)
				.append("payload", this.payload).toString();
	}

}
