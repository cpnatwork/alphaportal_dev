package alpha.portal.model;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.OrderBy;
import javax.persistence.PrimaryKeyJoinColumn;
import javax.persistence.Transient;
import javax.xml.bind.annotation.XmlTransient;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.apache.commons.lang.builder.ToStringBuilder;
import org.codehaus.jackson.annotate.JsonIgnore;
import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Parameter;

/**
 * The α-Card-Descriptor includes the α-Card-Identifier and a set of adornments.
 */
@Entity(name = "alphacarddescriptor")
public final class AlphaCardDescriptor implements Cloneable {

    /**
     * the α-Card-Identifier, which identifies the α-Card.
     * 
     * @see AlphaCardIdentifier α-Card-Identifier
     */
    @XmlTransient
    @JsonIgnore
    @EmbeddedId
    @GenericGenerator(name = "generator", strategy = "foreign", parameters = @Parameter(name = "property", value = "alphaCard"))
    @GeneratedValue(generator = "generator")
    private AlphaCardIdentifier alphaCardIdentifier;

    /**
     * reference to the the α-Case, the α-Card belongs to.
     * 
     * @see AlphaCase α-Case
     */
    @XmlTransient
    @JsonIgnore
    @OneToOne(cascade = CascadeType.ALL, fetch = FetchType.EAGER, mappedBy = "alphaCardDescriptor")
    @PrimaryKeyJoinColumn
    private AlphaCard alphaCard;

    /**
     * A list of adornments belonging to this α-Card.
     */
    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER, orphanRemoval = true)
    @OrderBy(value = "name")
    private final Set<Adornment> adornmentList = new HashSet<Adornment>();

    /**
     * Flag to determine adornment changes.
     */
    @XmlTransient
    @Transient
    private boolean adornmentsChanged;

    /**
     * Default constructor. Since α-Card is the stronger entity this does NOT create any α-Card or α-Card-Identifier.
     */
    public AlphaCardDescriptor() {

    }

    /**
     * Constructor, which automatically links to an AlphaCard.
     * 
     * @param card
     *            α-Card
     */
    public AlphaCardDescriptor(final AlphaCard card) {
        setAlphaCard(card);
    }

    /**
     * Constructor, which also creates a (full!) α-Card-Identifier so this α-Card-Descriptor can be used for new
     * AlphaCard(descriptor). Does NOT initialize an α-Card object!
     * 
     * @param caseId
     *            case id
     * @param cardId
     *            card id
     */
    public AlphaCardDescriptor(final String caseId, final String cardId) {
        setAlphaCardIdentifier(new AlphaCardIdentifier(caseId, cardId));
    }

    /**
     * Gets the α-Card.
     * 
     * @return α-Card
     */
    @XmlTransient
    @JsonIgnore
    public AlphaCard getAlphaCard() {
        return alphaCard;
    }

    /**
     * Links this α-Card-Descriptor with an α-Card and also takes its α-Card-Identifier, if it is "better" than this
     * α-Card-Descriptor's identifier.
     * 
     * @param alphaCard
     *            the new α-Card
     */
    public void setAlphaCard(final AlphaCard alphaCard) {
        this.alphaCard = alphaCard;
        // sets this descriptor to the alphaCard

        if (alphaCard != null) {
            if (alphaCard.getAlphaCardDescriptor() == null) {
                alphaCard.setAlphaCardDescriptor(this);
            }

            // sets the alphaCardIdentifier the cards alphaCardIdentifier
            if (alphaCardIdentifier == null && alphaCard.getAlphaCardIdentifier() != null) {
                alphaCardIdentifier = alphaCard.getAlphaCardIdentifier();
            }
        }
    }

    /**
     * Sets the title.
     * 
     * @param title
     *            the new title of the adronment of the type Title.
     * @throws Exception
     *             exception
     */
    public void setTitle(final String title) throws Exception {
        setAdornment(AdornmentType.Title.getName(), title);
    }

    /**
     * Gets the title.
     * 
     * @return the title
     */
    public String getTitle() {
        Adornment a = getAdornment(AdornmentType.Title.getName());
        if (a == null)
            return "No name"; // throw new
        // NoSuchFieldException("Adornment Title not set");
        if (a.getValue().isEmpty())
            return "No name";

        return a.getValue();
    }

    /**
     * Sets the responsible person of this α-Card and takes the Appfuse user ID, to do so.
     * 
     * @param userId
     *            the new person in charge for the adornment "Contributor"
     * @throws Exception
     *             exception
     */
    public void setContributor(final Long userId) throws Exception {
        setAdornment(AdornmentType.Contributor.getName(), userId.toString());
    }

    /**
     * Gets the AppFuse user ID of the person in charge.
     * 
     * @return person in charge
     */
    public Long getContributor() {
        Adornment a = getAdornment(AdornmentType.Contributor.getName());
        if (a == null || StringUtils.isEmpty(a.getValue()))
            return null;
        return Long.parseLong(a.getValue());
    }

    /**
     * Gets the Contributor role configured for this card.
     * 
     * @return name of ContributorRole
     */
    public String getContributorRole() {
        Adornment a = getAdornment(AdornmentType.ContributorRole.getName());
        if (a == null || StringUtils.isEmpty(a.getValue()))
            return null;
        return a.getValue();
    }

    /**
     * Returns the adornment with name key. If the adornment does not exist, null is returned.
     * 
     * @param key
     *            name of the adornment
     * @return found adornment or null
     */
    public Adornment getAdornment(final String key) {

        for (Adornment a : adornmentList) {
            if (a.getName().equalsIgnoreCase(key))
                // Adornment found
                return a;
        }

        // Adornment not found
        return null;
    }

    /**
     * Adds a new adornment to the list or changes the value of a existing one.
     * 
     * @param key
     *            name of the adornment
     * @param value
     *            value of the adornment
     */
    public void setAdornment(final String key, final String value) {
        Adornment newAdornment = getAdornment(key);

        if (newAdornment == null) {
            setAdornmentsChanged(true);
            // add new adornment
            newAdornment = new Adornment(key);
            newAdornment.setValue(value);

            adornmentList.add(newAdornment);
        } else {
            if ((value == null && StringUtils.isNotBlank(newAdornment.getValue()))
                    || (value != null && !value.equals(newAdornment.getValue()))) {
                setAdornmentsChanged(true);
                // change existing adornment
                newAdornment.setValue(value);
            }
        }
    }

    public void setAdornment(final Adornment a) {
        setAdornment(a.getName(), a.getValue());
    }

    /**
     * Deletes the adornment with the given name.
     * 
     * @param name
     *            name of adornment
     * @return true, if successful
     */
    public boolean deleteAdornment(final String name) {
        for (Adornment adornment : adornmentList) {
            if (adornment.getName().equals(name)) {
                setAdornmentsChanged(true);
                return adornmentList.remove(adornment);
            }
        }
        return false;
    }

    /**
     * Gets all adornments of the list.
     * 
     * @return all adornments as <b>unmodifiable</b> set
     */
    public Set<Adornment> getAllAdornments() {
        return Collections.unmodifiableSet(adornmentList);
    }

    /**
     * Gets the α-Card-Identifier.
     * 
     * @return α-Card
     */
    @XmlTransient
    @JsonIgnore
    public AlphaCardIdentifier getAlphaCardIdentifier() {
        return alphaCardIdentifier;
    }

    /**
     * Sets the α-Card-Identifier.
     * 
     * @param alphaCardId
     *            α-Card-Identifier, which has to be set
     */
    public void setAlphaCardIdentifier(final AlphaCardIdentifier alphaCardId) {
        this.alphaCardIdentifier = alphaCardId;
    }

    @Override
    public boolean equals(final Object other) {
        if (!(other instanceof AlphaCardDescriptor))
            return false;
        final AlphaCardDescriptor castOther = (AlphaCardDescriptor) other;
        return new EqualsBuilder().append(alphaCardIdentifier, castOther.alphaCardIdentifier).append(adornmentList,
                castOther.adornmentList).isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder().append(alphaCardIdentifier).append(adornmentList).toHashCode();
    }

    @Override
    public String toString() {
        return new ToStringBuilder(this).append("alphaCardIdentifier", alphaCardIdentifier).append("adornmentList",
                adornmentList).toString();
    }

    @Override
    @XmlTransient
    @JsonIgnore
    public AlphaCardDescriptor clone() {
        AlphaCardDescriptor desc = new AlphaCardDescriptor();
        desc.setAlphaCardIdentifier(this.getAlphaCardIdentifier());
        for (Adornment a : getAllAdornments()) {
            desc.setAdornment(a.getName(), a.getValue());

        }
        return desc;
    }

    /**
     * @return the adornmentsChanged
     */
    @XmlTransient
    @JsonIgnore
    public boolean isAdornmentsChanged() {
        return adornmentsChanged;
    }

    /**
     * @param adornmentsChanged
     *            the adornmentsChanged to set
     */
    public void setAdornmentsChanged(final boolean adornmentsChanged) {
        this.adornmentsChanged = adornmentsChanged;
    }
}
