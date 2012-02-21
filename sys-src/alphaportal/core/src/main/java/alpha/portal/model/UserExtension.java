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

@Entity(name = "userextension")
public class UserExtension {
    @Id
    private Long userId;

    @OneToOne(fetch = FetchType.EAGER, cascade = CascadeType.ALL)
    @PrimaryKeyJoinColumn
    private User user;

    @ManyToMany(fetch = FetchType.EAGER, cascade = CascadeType.ALL)
    private Set<ContributorRole> roles;

    /**
     * Default constructor (initializes empty HashSet)
     */
    public UserExtension() {
        roles = new HashSet<ContributorRole>();
    }

    /**
     * Constructor which sets this extensions user.
     * 
     * @param u
     */
    public UserExtension(final User u) {
        this();
        user = u;
        userId = u.getId();
    }

    /**
     * @return the userId
     */
    public Long getUserId() {
        return userId;
    }

    /**
     * @param userId
     *            the userId to set
     */
    public void setUserId(final Long userId) {
        this.userId = userId;
    }

    /**
     * @return the user
     */
    public User getUser() {
        return user;
    }

    /**
     * @param user
     *            the user to set
     */
    public void setUser(final User user) {
        this.user = user;
    }

    /**
     * @return the roles
     */
    public Set<ContributorRole> getRoles() {
        return roles;
    }

    /**
     * @param roles
     *            the roles to set
     */
    public void setRoles(final Set<ContributorRole> roles) {
        this.roles = roles;
    }

    /**
     * Checks whether the user has a specific ContributorRole.
     * 
     * @return true/false
     */
    public boolean hasRole(final ContributorRole role) {
        for (ContributorRole r : roles) {
            if (r.equals(role))
                return true;
        }
        return false;
    }

    /**
     * Adds a specific role to this user.
     * 
     * @return returns true if it was successfully added, else (e.g. already in the list) false
     */
    public boolean addRole(final ContributorRole role) {
        if (!hasRole(role))
            return roles.add(role);
        return false;
    }

    @Override
    public boolean equals(final Object other) {
        if (!(other instanceof UserExtension))
            return false;
        UserExtension castOther = (UserExtension) other;
        return new EqualsBuilder().append(userId, castOther.userId).append(roles, castOther.roles).isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder(1947995791, 551833295).append(userId).append(roles).toHashCode();
    }

    @Override
    public String toString() {
        return new ToStringBuilder(this).append("userId", userId).append("roles", roles).toString();
    }
}
