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
 * It is an attribute of an α-Case, containing the list of participants.
 * CRA means "Collaboration Resource Artifact". 
 */
@Embeddable
public class AlphaCaseCRA implements Serializable {

    private static final long serialVersionUID = -3750497200648290571L;

    /** 
     * List of participants, at the moment from Appfuse Users.
     */
    @ManyToMany(fetch = FetchType.EAGER, cascade = { CascadeType.PERSIST, CascadeType.MERGE })
    @JoinTable(name = "alphacase_participants", joinColumns = @JoinColumn(name = "alphacase_caseId"), inverseJoinColumns = @JoinColumn(name = "app_user_id"))
    private final Set<User> participants = new HashSet<User>();

    /**
     * Gets the list of participants.
     * 
     * @return list of participants as <b>unmodifiable</b> set
     */
    public Set<User> getListOfParticipants() {
	return Collections.unmodifiableSet(participants);
    }

    /**
     * Add a User to participants list
     * 
     * @param participant
     *            new participant
     * @return list of participants if the participant was successfully added (will return {@code false} if already present)
     **/
    public boolean addUserToListOfParticipants(final User participant) {
	return participants.add(participant);
    }

    /**
     * Removes the user from list of participants.
     * 
     * @param participant
     *            participant
     * @return true if present and successfully removed
     */
    public boolean removeUserFromListOfParticipants(final User participant) {
	return participants.remove(participant);
    }

    @Override
    public boolean equals(final Object other) {
	if (!(other instanceof AlphaCaseCRA)) {
	    return false;
	}
	final AlphaCaseCRA castOther = (AlphaCaseCRA) other;
	return new EqualsBuilder().append(participants.toArray(new User[] {}), castOther.participants.toArray(new User[] {})).isEquals();
    }

    @Override
    public int hashCode() {
	return new HashCodeBuilder(-1081601783, 2063236229).append(participants).toHashCode();
    }

    @Override
    public String toString() {
	return new ToStringBuilder(this).append("participants", participants).toString();
    }

}
