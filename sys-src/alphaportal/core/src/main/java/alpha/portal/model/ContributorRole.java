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
 * Role of the contributor (for Example: gynecologist)
 */

@Entity(name = "contributorrole")
public class ContributorRole implements Serializable {

    /**
     * Serial
     */
    private static final long serialVersionUID = 2281689896037198533L;

    /**
     * The identifier of the user role
     */
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
        return name;
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
     * @return the contributor role id
     */
    public Long getContributorRoleId() {
        return contributorRoleId;
    }

    /**
     * @param contributorRoleId
     *            the contributorRoleId to set
     */
    public void setContributorRoleId(final Long contributorRoleId) {
        this.contributorRoleId = contributorRoleId;
    }

    @Override
    public boolean equals(final Object other) {
        if (!(other instanceof ContributorRole))
            return false;
        ContributorRole castOther = (ContributorRole) other;
        return new EqualsBuilder().append(contributorRoleId, castOther.contributorRoleId).append(name, castOther.name)
                .isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder(526235407, 1637708351).append(contributorRoleId).append(name).toHashCode();
    }

    @Override
    public String toString() {
        return new ToStringBuilder(this).append("contributorRoleId", contributorRoleId).append("name", name).toString();
    }
}
