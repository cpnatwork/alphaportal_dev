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

import javax.persistence.Embeddable;
import javax.xml.bind.annotation.XmlTransient;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.apache.commons.lang.builder.ToStringBuilder;
import org.codehaus.jackson.annotate.JsonIgnore;

/**
 * This class defines the α-Card-Identifier. It is composed of the
 * α-Episode(a.k.a α-Document)-ID, the Institution-ID, the Doctor-ID and a
 * unique DHT-value.
 * 
 */

@Embeddable
public class AlphaCardIdentifier implements Serializable, Cloneable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 7088062976516558752L;

	/**
	 * The id of the α-Case.
	 */
	private String caseId;

	/**
	 * The id of the α-Card.
	 */
	private String cardId;

	/**
	 * The sequence number of this version.
	 */
	private Long sequenceNumber;

	/**
	 * Default constructor.
	 */
	public AlphaCardIdentifier() {
	}

	/**
	 * Constructor with 1 parameter, the caseId since cardId is usually
	 * generated.
	 * 
	 * @param caseId
	 *            The id of the α-Case.
	 */
	public AlphaCardIdentifier(final String caseId) {
		this(caseId, null, null);
	}

	/**
	 * Constructor with both arguments.
	 * 
	 * @param caseId
	 *            The id of the α-Case.
	 * @param cardId
	 *            The id of the α-Card.
	 */
	public AlphaCardIdentifier(final String caseId, final String cardId) {
		this(caseId, cardId, null);
	}

	/**
	 * Constructor with sequence number.
	 * 
	 * @param caseId
	 *            the case id
	 * @param cardId
	 *            the card id
	 * @param sequenceNumber
	 *            the sequence number
	 */
	public AlphaCardIdentifier(final String caseId, final String cardId,
			final Long sequenceNumber) {
		this.caseId = caseId;
		this.cardId = cardId;
		this.sequenceNumber = sequenceNumber;
	}

	/**
	 * gets the id of the α-Case.
	 * 
	 * @return caseId
	 */
	public String getCaseId() {
		return this.caseId;
	}

	/**
	 * sets the caseId
	 * 
	 * @param caseId
	 *            the new id of the α-Case
	 */
	public void setCaseId(final String caseId) {
		this.caseId = caseId;
	}

	/**
	 * gets the id of the α-Card.
	 * 
	 * @return cardId
	 */
	public String getCardId() {
		return this.cardId;
	}

	/**
	 * sets the cardId
	 * 
	 * @param cardId
	 *            the new id of the α-Card
	 */
	public void setCardId(final String cardId) {
		this.cardId = cardId;
	}

	/**
	 * Gets the sequence number of this version.
	 * 
	 * @return the sequenceNumber
	 */
	public Long getSequenceNumber() {
		return this.sequenceNumber;
	}

	/**
	 * Sets the sequence number of this version.
	 * 
	 * @param sequenceNumber
	 *            the sequenceNumber to set
	 */
	public void setSequenceNumber(final Long sequenceNumber) {
		this.sequenceNumber = sequenceNumber;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return new ToStringBuilder(this).append("caseId", this.caseId)
				.append("cardId", this.cardId)
				.append("sequenceNumber", this.sequenceNumber).toString();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(final Object other) {
		if (!(other instanceof AlphaCardIdentifier))
			return false;
		final AlphaCardIdentifier castOther = (AlphaCardIdentifier) other;
		return new EqualsBuilder().append(this.caseId, castOther.caseId)
				.append(this.cardId, castOther.cardId)
				.append(this.sequenceNumber, castOther.sequenceNumber)
				.isEquals();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(this.caseId).append(this.cardId)
				.append(this.sequenceNumber).toHashCode();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#clone()
	 */
	@Override
	@XmlTransient
	@JsonIgnore
	public AlphaCardIdentifier clone() {
		return new AlphaCardIdentifier(this.getCaseId(), this.getCardId(),
				this.getSequenceNumber());
	}
}
