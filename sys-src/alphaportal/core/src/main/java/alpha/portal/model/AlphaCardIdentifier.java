package alpha.portal.model;

import java.io.Serializable;

import javax.persistence.Embeddable;
import javax.xml.bind.annotation.XmlTransient;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.apache.commons.lang.builder.ToStringBuilder;
import org.codehaus.jackson.annotate.JsonIgnore;

/**
 * This class defines the α-Card-Identifier. It is composed of the α-Episode(a.k.a α-Document)-ID, the Institution-ID,
 * the Doctor-ID and a unique DHT-value.
 * 
 */

@Embeddable
public class AlphaCardIdentifier implements Serializable, Cloneable {

    private static final long serialVersionUID = 7088062976516558752L;

    /**
     * The id of the α-Case.
     */
    private String caseId;

    /**
     * The id of the α-Card.
     */
    private String cardId;

    /**
     * The sequence number of this version.
     */
    private Long sequenceNumber;

    /**
     * Default constructor
     */
    public AlphaCardIdentifier() {
    }

    /**
     * Constructor with 1 parameter, the caseId since cardId is usually generated.
     * 
     * @param caseId
     *            The id of the α-Case.
     */
    public AlphaCardIdentifier(final String caseId) {
        this(caseId, null, null);
    }

    /**
     * Constructor with both arguments
     * 
     * @param caseId
     *            The id of the α-Case.
     * 
     * @param cardId
     *            The id of the α-Card.
     */
    public AlphaCardIdentifier(final String caseId, final String cardId) {
        this(caseId, cardId, null);
    }

    /**
     * Constructor with sequence number.
     */
    public AlphaCardIdentifier(final String caseId, final String cardId, final Long sequenceNumber) {
        this.caseId = caseId;
        this.cardId = cardId;
        this.sequenceNumber = sequenceNumber;
    }

    /**
     * gets the id of the α-Case.
     * 
     * @return caseId
     */
    public String getCaseId() {
        return caseId;
    }

    /**
     * sets the caseId
     * 
     * @param caseId
     */
    public void setCaseId(final String caseId) {
        this.caseId = caseId;
    }

    /**
     * gets the id of the α-Card.
     * 
     * @return cardId
     */
    public String getCardId() {
        return cardId;
    }

    /**
     * sets the cardId
     * 
     * @param cardId
     */
    public void setCardId(final String cardId) {
        this.cardId = cardId;
    }

    /**
     * @return the sequenceNumber
     */
    public Long getSequenceNumber() {
        return sequenceNumber;
    }

    /**
     * @param sequenceNumber
     *            the sequenceNumber to set
     */
    public void setSequenceNumber(final Long sequenceNumber) {
        this.sequenceNumber = sequenceNumber;
    }

    @Override
    public String toString() {
        return new ToStringBuilder(this).append("caseId", caseId).append("cardId", cardId).append("sequenceNumber",
                sequenceNumber).toString();
    }

    @Override
    public boolean equals(final Object other) {
        if (!(other instanceof AlphaCardIdentifier))
            return false;
        AlphaCardIdentifier castOther = (AlphaCardIdentifier) other;
        return new EqualsBuilder().append(caseId, castOther.caseId).append(cardId, castOther.cardId).append(
                sequenceNumber, castOther.sequenceNumber).isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder().append(caseId).append(cardId).append(sequenceNumber).toHashCode();
    }

    @Override
    @XmlTransient
    @JsonIgnore
    public AlphaCardIdentifier clone() {
        return new AlphaCardIdentifier(this.getCaseId(), this.getCardId(), this.getSequenceNumber());
    }
}
