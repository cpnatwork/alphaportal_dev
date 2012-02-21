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

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.OneToOne;

import org.appfuse.model.User;

/**
 * 
 * Represents a request to become a contributor to an AlphaCard.
 * 
 */
@Entity(name = "contributorrequest")
public class ContributorRequest {

	/** The contributor request id. */
	@Id
	@GeneratedValue
	private Long contributorRequestID;

	/** The requesting user. */
	@OneToOne(optional = false, fetch = FetchType.EAGER)
	User requestingUser;

	/** The accepting user. */
	@OneToOne(optional = false, fetch = FetchType.EAGER)
	User acceptingUser;

	/** The alpha card. */
	@OneToOne(optional = false, fetch = FetchType.EAGER)
	AlphaCard alphaCard;

	/** The date. */
	@Column
	Date date;

	/**
	 * Instantiates a new contributor request.
	 */
	public ContributorRequest() {
		this.date = new Date();
	}

	/**
	 * Instantiates a new contributor request.
	 * 
	 * @param reqUser
	 *            the req user
	 * @param acceptUser
	 *            the accept user
	 * @param aCard
	 *            the a card
	 */
	public ContributorRequest(final User reqUser, final User acceptUser,
			final AlphaCard aCard) {
		this.requestingUser = reqUser;
		this.acceptingUser = acceptUser;
		this.alphaCard = aCard;
		this.date = new Date();
	}

	/**
	 * Instantiates a new contributor request.
	 * 
	 * @param acceptUser
	 *            the accept user
	 * @param aCard
	 *            the a card
	 */
	public ContributorRequest(final User acceptUser, final AlphaCard aCard) {
		this.acceptingUser = acceptUser;
		this.alphaCard = aCard;
		this.date = new Date();
	}

	/**
	 * Gets the contributor request id.
	 * 
	 * @return the contributor request id
	 */
	public Long getContributorRequestID() {
		return this.contributorRequestID;
	}

	/**
	 * Sets the contributor request id.
	 * 
	 * @param contributorRequestID
	 *            the new contributor request id
	 */
	public void setContributorRequestID(final Long contributorRequestID) {
		this.contributorRequestID = contributorRequestID;
	}

	/**
	 * Gets the requesting user.
	 * 
	 * @return the requesting user
	 */
	public User getRequestingUser() {
		return this.requestingUser;
	}

	/**
	 * Sets the requesting user.
	 * 
	 * @param requestingUser
	 *            the new requesting user
	 */
	public void setRequestingUser(final User requestingUser) {
		this.requestingUser = requestingUser;
	}

	/**
	 * Gets the accepting user.
	 * 
	 * @return the accepting user
	 */
	public User getAcceptingUser() {
		return this.acceptingUser;
	}

	/**
	 * Sets the accepting user.
	 * 
	 * @param acceptingUser
	 *            the new accepting user
	 */
	public void setAcceptingUser(final User acceptingUser) {
		this.acceptingUser = acceptingUser;
	}

	/**
	 * Gets the alpha card.
	 * 
	 * @return the alpha card
	 */
	public AlphaCard getAlphaCard() {
		return this.alphaCard;
	}

	/**
	 * Sets the alpha card.
	 * 
	 * @param alphaCard
	 *            the new alpha card
	 */
	public void setAlphaCard(final AlphaCard alphaCard) {
		this.alphaCard = alphaCard;
	}

}
