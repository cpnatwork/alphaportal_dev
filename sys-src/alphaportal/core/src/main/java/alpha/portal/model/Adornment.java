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
 * An adornment is a coordination-relevant attribute as meta-information about an α-Card. The set of all adornments
 * constitutes the α-Card descriptor.
 * 
 * @see AdornmentType default adornments
 */
@Entity(name = "adornment")
public class Adornment implements Serializable, Cloneable {

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
        return name;
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
        return value;
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
        return adornmentId;
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

    @Override
    public boolean equals(final Object other) {
        if (!(other instanceof Adornment)) {
            return false;
        }
        Adornment castOther = (Adornment) other;
        return new EqualsBuilder().append(adornmentId, castOther.adornmentId).append(name, castOther.name).append(
                value, castOther.value).isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder(-891151943, 520022095).append(adornmentId).append(name).append(value).toHashCode();
    }

    @Override
    public String toString() {
        return new ToStringBuilder(this).append("id", adornmentId).append("name", name).append("value", value)
                .toString();
    }

    @Override
    public Adornment clone() {
        Adornment a = new Adornment();
        a.setName(this.getName());
        a.setValue(this.getValue());
        return a;
    }
}
