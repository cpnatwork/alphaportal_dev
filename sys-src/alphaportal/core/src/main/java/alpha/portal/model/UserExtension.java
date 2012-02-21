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

import java.util.HashSet;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.ManyToMany;
import javax.persistence.OneToOne;
import javax.persistence.PrimaryKeyJoinColumn;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.apache.commons.lang.builder.ToStringBuilder;
import org.appfuse.model.User;

/**
 * The Class UserExtension.
 */
@Entity(name = "userextension")
public class UserExtension {

	/** The user id. */
	@Id
	private Long userId;

	/** The user. */
	@OneToOne(fetch = FetchType.EAGER, cascade = CascadeType.ALL)
	@PrimaryKeyJoinColumn
	private User user;

	/** The roles. */
	@ManyToMany(fetch = FetchType.EAGER, cascade = CascadeType.ALL)
	private Set<ContributorRole> roles;

	/**
	 * Default constructor (initializes empty HashSet).
	 */
	public UserExtension() {
		this.roles = new HashSet<ContributorRole>();
	}

	/**
	 * Constructor which sets this extensions user.
	 * 
	 * @param u
	 *            the u
	 */
	public UserExtension(final User u) {
		this();
		this.user = u;
		this.userId = u.getId();
	}

	/**
	 * Gets the user id.
	 * 
	 * @return the userId
	 */
	public Long getUserId() {
		return this.userId;
	}

	/**
	 * Sets the user id.
	 * 
	 * @param userId
	 *            the userId to set
	 */
	public void setUserId(final Long userId) {
		this.userId = userId;
	}

	/**
	 * Gets the user.
	 * 
	 * @return the user
	 */
	public User getUser() {
		return this.user;
	}

	/**
	 * Sets the user.
	 * 
	 * @param user
	 *            the user to set
	 */
	public void setUser(final User user) {
		this.user = user;
	}

	/**
	 * Gets the roles.
	 * 
	 * @return the roles
	 */
	public Set<ContributorRole> getRoles() {
		return this.roles;
	}

	/**
	 * Sets the roles.
	 * 
	 * @param roles
	 *            the roles to set
	 */
	public void setRoles(final Set<ContributorRole> roles) {
		this.roles = roles;
	}

	/**
	 * Checks whether the user has a specific ContributorRole.
	 * 
	 * @param role
	 *            the role
	 * @return true/false
	 */
	public boolean hasRole(final ContributorRole role) {
		for (final ContributorRole r : this.roles) {
			if (r.equals(role))
				return true;
		}
		return false;
	}

	/**
	 * Adds a specific role to this user.
	 * 
	 * @param role
	 *            the role
	 * @return returns true if it was successfully added, else (e.g. already in
	 *         the list) false
	 */
	public boolean addRole(final ContributorRole role) {
		if (!this.hasRole(role))
			return this.roles.add(role);
		return false;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(final Object other) {
		if (!(other instanceof UserExtension))
			return false;
		final UserExtension castOther = (UserExtension) other;
		return new EqualsBuilder().append(this.userId, castOther.userId)
				.append(this.roles, castOther.roles).isEquals();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return new HashCodeBuilder(1947995791, 551833295).append(this.userId)
				.append(this.roles).toHashCode();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return new ToStringBuilder(this).append("userId", this.userId)
				.append("roles", this.roles).toString();
	}
}
