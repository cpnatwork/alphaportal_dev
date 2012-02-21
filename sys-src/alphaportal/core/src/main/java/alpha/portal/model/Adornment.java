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
 * An adornment is a coordination-relevant attribute as meta-information about
 * an α-Card. The set of all adornments constitutes the α-Card descriptor.
 * 
 * @see AdornmentType default adornments
 */
@Entity(name = "adornment")
public class Adornment implements Serializable, Cloneable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -3355106215767986096L;

	/**
	 * The identifier of the single adornment.
	 */
	@Id
	@GeneratedValue
	private Long adornmentId;

	/**
	 * The name of the single adornment.
	 * 
	 * @see AdornmentType adornment types
	 */
	@Column
	private String name;

	/**
	 * The value of the single adornment.
	 */
	@Column
	private String value;

	/**
	 * Default constructor for hibernate.
	 */
	public Adornment() {
	}

	/**
	 * Instantiates a new adornment.
	 * 
	 * @param name
	 *            the name of the adornment
	 */
	public Adornment(final String name) {
		this.name = name;
	}

	/**
	 * gets the name of the adornment.
	 * 
	 * @return the name
	 */
	public String getName() {
		return this.name;
	}

	/**
	 * sets the name of the adornment.
	 * 
	 * @param name
	 *            the name to set
	 */
	public void setName(final String name) {
		this.name = name;
	}

	/**
	 * gets the value of the adornment.
	 * 
	 * @return the value
	 */
	public String getValue() {
		return this.value;
	}

	/**
	 * sets the value of the adornment.
	 * 
	 * @param value
	 *            the value to set
	 */
	public void setValue(final String value) {
		this.value = value;
	}

	/**
	 * gets the identifier of the adornment.
	 * 
	 * @return adornmentId
	 */
	public Long getAdornmentId() {
		return this.adornmentId;
	}

	/**
	 * sets the id
	 * 
	 * @param id
	 *            the new id
	 */
	public void setAdornmentId(final long id) {
		this.adornmentId = id;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(final Object other) {
		if (!(other instanceof Adornment))
			return false;
		final Adornment castOther = (Adornment) other;
		return new EqualsBuilder()
				.append(this.adornmentId, castOther.adornmentId)
				.append(this.name, castOther.name)
				.append(this.value, castOther.value).isEquals();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return new HashCodeBuilder(-891151943, 520022095)
				.append(this.adornmentId).append(this.name).append(this.value)
				.toHashCode();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return new ToStringBuilder(this).append("id", this.adornmentId)
				.append("name", this.name).append("value", this.value)
				.toString();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#clone()
	 */
	@Override
	public Adornment clone() {
		final Adornment a = new Adornment();
		a.setName(this.getName());
		a.setValue(this.getValue());
		return a;
	}
}
