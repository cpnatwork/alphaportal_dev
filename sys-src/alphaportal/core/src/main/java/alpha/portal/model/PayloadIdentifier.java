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

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;

/**
 * The Class PayloadIdentifier.
 */
@Embeddable
public class PayloadIdentifier implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1L;

	/** The payload id. */
	private long payloadId;

	/** The sequence number. */
	private long sequenceNumber;

	/**
	 * Instantiates a new payload identifier.
	 */
	public PayloadIdentifier() {
	}

	/**
	 * Instantiates a new payload identifier.
	 * 
	 * @param payloadId
	 *            the payload id
	 * @param sequenceNUmber
	 *            the sequence n umber
	 */
	public PayloadIdentifier(final long payloadId, final long sequenceNUmber) {
		this.payloadId = payloadId;
		this.sequenceNumber = sequenceNUmber;
	}

	/**
	 * Gets the payload id.
	 * 
	 * @return the payload id
	 */
	public long getPayloadId() {
		return this.payloadId;
	}

	/**
	 * Sets the payload id.
	 * 
	 * @param payloadId
	 *            the new payload id
	 */
	public void setPayloadId(final long payloadId) {
		this.payloadId = payloadId;
	}

	/**
	 * Gets the sequence number.
	 * 
	 * @return the sequence number
	 */
	public long getSequenceNumber() {
		return this.sequenceNumber;
	}

	/**
	 * Sets the sequence number.
	 * 
	 * @param sequenceNumber
	 *            the new sequence number
	 */
	public void setSequenceNumber(final long sequenceNumber) {
		this.sequenceNumber = sequenceNumber;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return new HashCodeBuilder(1947995799, 551833291)
				.append(this.payloadId).append(this.sequenceNumber)
				.toHashCode();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(final Object other) {
		if (!(other instanceof PayloadIdentifier))
			return false;
		final PayloadIdentifier castOther = (PayloadIdentifier) other;
		return new EqualsBuilder().append(this.payloadId, castOther.payloadId)
				.append(this.sequenceNumber, castOther.sequenceNumber)
				.isEquals();
	}

}
