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

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.apache.commons.lang.builder.ToStringBuilder;

/**
 * Role of the contributor (for Example: gynecologist).
 */

@Entity(name = "contributorrole")
public class ContributorRole implements Serializable {

	/** Serial. */
	private static final long serialVersionUID = 2281689896037198533L;

	/** The identifier of the user role. */
	@Id
	@GeneratedValue
	private Long contributorRoleId;

	/**
	 * The name of the contributor role.
	 */
	@Column(unique = true)
	private String name;

	/**
	 * Default constructor for Hibernate.
	 */
	public ContributorRole() {
	}

	/**
	 * Instantiates a new contributor role.
	 * 
	 * @param name
	 *            the name of the contributor role
	 */
	public ContributorRole(final String name) {
		this.name = name;
	}

	/**
	 * gets the name of the contributor role.
	 * 
	 * @return the name
	 */
	public String getName() {
		return this.name;
	}

	/**
	 * sets the name of the contributor role.
	 * 
	 * @param name
	 *            the name to set
	 */
	public void setName(final String name) {
		this.name = name;
	}

	/**
	 * Gets the identifier of the user role.
	 * 
	 * @return the contributor role id
	 */
	public Long getContributorRoleId() {
		return this.contributorRoleId;
	}

	/**
	 * Sets the identifier of the user role.
	 * 
	 * @param contributorRoleId
	 *            the contributorRoleId to set
	 */
	public void setContributorRoleId(final Long contributorRoleId) {
		this.contributorRoleId = contributorRoleId;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(final Object other) {
		if (!(other instanceof ContributorRole))
			return false;
		final ContributorRole castOther = (ContributorRole) other;
		return new EqualsBuilder()
				.append(this.contributorRoleId, castOther.contributorRoleId)
				.append(this.name, castOther.name).isEquals();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return new HashCodeBuilder(526235407, 1637708351)
				.append(this.contributorRoleId).append(this.name).toHashCode();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("contributorRoleId", this.contributorRoleId)
				.append("name", this.name).toString();
	}
}
