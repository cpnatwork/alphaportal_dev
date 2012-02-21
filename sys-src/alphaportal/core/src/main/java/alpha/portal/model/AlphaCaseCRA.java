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
import java.util.HashSet;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.Embeddable;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.JoinTable;
import javax.persistence.ManyToMany;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.apache.commons.lang.builder.ToStringBuilder;
import org.appfuse.model.User;

/**
 * It is an attribute of an α-Case, containing the list of participants. CRA
 * means "Collaboration Resource Artifact".
 */
@Embeddable
public class AlphaCaseCRA implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -3750497200648290571L;

	/**
	 * List of participants, at the moment from Appfuse Users.
	 */
	@ManyToMany(fetch = FetchType.EAGER, cascade = { CascadeType.PERSIST,
			CascadeType.MERGE })
	@JoinTable(name = "alphacase_participants", joinColumns = @JoinColumn(name = "alphacase_caseId"), inverseJoinColumns = @JoinColumn(name = "app_user_id"))
	private final Set<User> participants = new HashSet<User>();

	/**
	 * Gets the list of participants.
	 * 
	 * @return list of participants as <b>unmodifiable</b> set
	 */
	public Set<User> getListOfParticipants() {
		return Collections.unmodifiableSet(this.participants);
	}

	/**
	 * Add a User to participants list.
	 * 
	 * @param participant
	 *            new participant
	 * @return list of participants if the participant was successfully added
	 *         (will return {@code false} if already present)
	 */
	public boolean addUserToListOfParticipants(final User participant) {
		return this.participants.add(participant);
	}

	/**
	 * Removes the user from list of participants.
	 * 
	 * @param participant
	 *            participant
	 * @return true if present and successfully removed
	 */
	public boolean removeUserFromListOfParticipants(final User participant) {
		return this.participants.remove(participant);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(final Object other) {
		if (!(other instanceof AlphaCaseCRA))
			return false;
		final AlphaCaseCRA castOther = (AlphaCaseCRA) other;
		return new EqualsBuilder().append(
				this.participants.toArray(new User[] {}),
				castOther.participants.toArray(new User[] {})).isEquals();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return new HashCodeBuilder(-1081601783, 2063236229).append(
				this.participants).toHashCode();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return new ToStringBuilder(this).append("participants",
				this.participants).toString();
	}

}
