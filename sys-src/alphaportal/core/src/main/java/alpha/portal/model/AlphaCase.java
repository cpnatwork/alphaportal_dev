package alpha.portal.model;

import java.util.Collections;
import java.util.List;
import java.util.Set;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.xml.bind.annotation.XmlTransient;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.apache.commons.lang.builder.ToStringBuilder;
import org.appfuse.model.User;
import org.codehaus.jackson.annotate.JsonIgnore;
import org.hibernate.annotations.GenericGenerator;

/**
 * This is a POJO for the entity α-Case.
 * 
 * It is used to coordinate cooperating parties. Using this α-Case does not require any preinstalled system components,
 * so true adhoc information interchange is enabled.
 */
@Entity(name = "alphacase")
public class AlphaCase {

    /**
     * The identifier of the α-Case.
     */
    @Id
    @GeneratedValue(generator = "uuid")
    @GenericGenerator(name = "uuid", strategy = "org.hibernate.id.UUIDGenerator")
    private String caseId;

    /**
     * The name of α-Case.
     */
    @Column(name = "name", length = 50)
    private String name;

    /**
     * the alphaCasePSA.getListOfAlphaCards() of this α-Case.
     */
    @XmlTransient
    @JsonIgnore
    private final AlphaCasePSA alphaCasePSA;

    /**
     * the participants of this α-Case.
     */
    @XmlTransient
    @JsonIgnore
    private final AlphaCaseCRA participantsCRA;

    public AlphaCase() {
        alphaCasePSA = new AlphaCasePSA();
        participantsCRA = new AlphaCaseCRA();
    }

    /**
     * Gets the name.
     * 
     * @return name
     */
    public String getName() {
        return name;
    }

    /**
     * Sets the name.
     * 
     * @param name
     *            new name of the α-Case
     */
    public void setName(final String name) {
        this.name = name;
    }

    /**
     * Gets the id.
     * 
     * @return id
     */
    public String getCaseId() {
        return caseId;
    }

    /**
     * Sets the id.
     * 
     * @param id
     *            new id of the α-Case.
     */
    public void setCaseId(final String id) {
        this.caseId = id;
    }

    /**
     * Gets the list of participants.
     * 
     * @return list of participants as <b>unmodifiable</b> set
     */
    public Set<User> getListOfParticipants() {
        return participantsCRA.getListOfParticipants();
    }

    /**
     * Add a participant to the case
     * 
     * @param participant
     *            participant, who should be added
     */
    public void addParticipant(final User participant) {
        participantsCRA.addUserToListOfParticipants(participant);
    }

    /**
     * Removes the participant.
     * 
     * @param participant
     *            participant of the α-Case.
     * @return true, if present and successfully removed
     */
    public boolean removeParticipant(final User participant) {
        return participantsCRA.removeUserFromListOfParticipants(participant);
    }

    /**
     * Gets the α-Cards as <b>unmodifiable</b> Set.
     * 
     * @return α-Card-List
     */
    public List<AlphaCard> getAlphaCards() {
        return Collections.unmodifiableList(alphaCasePSA.getListOfAlphaCards());
    }

    /**
     * adds a α-Card.
     * 
     * @param card
     *            α-Card, which will be added.
     */
    public void addAlphaCard(final AlphaCard card, final int newPriority) {
        if (card.getAlphaCase() == null) {
            card.setAlphaCase(this);
        }
        if (!alphaCasePSA.getListOfAlphaCards().contains(card)) {
            alphaCasePSA.addAlphaCard(card, newPriority);
        }
    }

    /**
     * Adds an AlphaCard to the list with a specific position.
     * 
     * @param card
     *            the AlphaCard
     */
    public void addAlphaCard(final AlphaCard card) {
        if (card.getAlphaCase() == null) {
            card.setAlphaCase(this);
        }
        if (!alphaCasePSA.getListOfAlphaCards().contains(card)) {
            alphaCasePSA.addAlphaCard(card, alphaCasePSA.getListOfAlphaCards().size());
        }
    }

    /**
     * Moves an AlphaCard within the list to a specific position.
     * 
     * @param card
     *            the AlphaCard
     * @param newPriority
     *            new priority
     */
    public void moveAlphaCard(final AlphaCard card, final int newPriority) {
        alphaCasePSA.moveAlphaCard(card, newPriority);
    }

    /**
     * Removes the AlphaCard.
     * 
     * @param participant
     *            participant of the α-Case.
     * @return true, if present and successfully removed
     */
    public boolean removeAlphaCard(final AlphaCard aCard) {
        return alphaCasePSA.removeAlphaCard(aCard);
    }

    /**
     * @return the AlphaCase PSA
     */
    @XmlTransient
    @JsonIgnore
    public AlphaCasePSA getAlphaCasePSA() {
        return alphaCasePSA;
    }

    /**
     * @return the participants CRA
     */
    @XmlTransient
    @JsonIgnore
    public AlphaCaseCRA getParticipantsCRA() {
        return participantsCRA;
    }

    @Override
    public boolean equals(final Object other) {
        if (!(other instanceof AlphaCase)) {
            return false;
        }
        final AlphaCase castOther = (AlphaCase) other;
        return new EqualsBuilder().append(caseId, castOther.caseId).append(name, castOther.name).append(alphaCasePSA,
                castOther.alphaCasePSA).append(participantsCRA, castOther.participantsCRA).isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder(1171060467, -2122474045).append(caseId).append(name).append(participantsCRA)
                .toHashCode();
    }

    @Override
    public String toString() {
        return new ToStringBuilder(this).append("id", caseId).append("name", name).append("alphaCasePSA", alphaCasePSA)
                .append("participantsCRA", participantsCRA).toString();
    }

}