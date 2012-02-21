package alpha.portal.model;

import java.io.Serializable;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Embeddable;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.JoinTable;
import javax.persistence.ManyToMany;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.apache.commons.lang.builder.ToStringBuilder;
import org.hibernate.annotations.IndexColumn;

/**
 * It is an attribute of an α-Case, containing the list of alpha cards. PSA means "Process Structure Artifact".
 */
@Embeddable
public class AlphaCasePSA implements Serializable {

    private static final long serialVersionUID = -6579707319743185225L;

    /**
     * List of alpha cards. Uses @ManyToMany (instead of @OneToMany) to remove "unique" constraint on inverseJoinColumns
     * to avoid "duplicate entry" error on update (reordering uses update on inverseJoinColumns :O). Bug was fixed in
     * 2010 but hibernate3-maven-plugin was last published on 21.01.2009
     * 
     * @see https://hibernate.onjira.com/browse/HHH-3609
     */
    @ManyToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER)
    @JoinTable(joinColumns = @JoinColumn(name = "alphacase_caseId", referencedColumnName = "caseId", updatable = false), inverseJoinColumns = {
            @JoinColumn(name = "alphacard_caseId", referencedColumnName = "caseId", updatable = false),
            @JoinColumn(name = "alphacard_cardId", referencedColumnName = "cardId", updatable = false),
            @JoinColumn(name = "alphacard_sequenceNumber", referencedColumnName = "sequenceNumber", updatable = false) })
    @IndexColumn(name = "priority")
    private List<AlphaCard> alphaCards = new LinkedList<AlphaCard>();

    /**
     * Gets the list of AlphaCards.
     * 
     * @return list of AlphaCards as <b>unmodifiable</b> set
     */
    public List<AlphaCard> getListOfAlphaCards() {
        return Collections.unmodifiableList(alphaCards);
    }

    /**
     * Clear the list of AlphaCards.
     */
    public void clearAlphaCards() {
        alphaCards.clear();
    }

    /**
     * Sets the whole new List alphaCards.
     */
    public void setAlphaCards(final List<AlphaCard> alphaCards) {
        this.alphaCards = alphaCards;
    }

    /**
     * Searches alphaCards for the given cardId. Returns null on failure.
     */
    public AlphaCard getAlphaCardByCardId(final String cardId) {
        for (AlphaCard c : alphaCards) {
            if (c.getAlphaCardIdentifier().getCardId().equals(cardId)) {
                return c;
            }
        }
        return null;
    }

    /**
     * Add a AlphaCard to AlphaCards list to a specific position.
     * 
     * @param AlphaCard
     *            new AlphaCard
     **/
    public boolean addAlphaCard(final AlphaCard card, final int priority) {
        alphaCards.add(priority, card);
        return true;
    }

    /**
     * Moves an AlphaCard within the list to a specific position.
     * 
     * @param card
     *            the AlphaCard
     * @param newPriority
     *            new priority
     */
    public boolean moveAlphaCard(final AlphaCard card, final int newPriority) {
        alphaCards.remove(card);
        addAlphaCard(card, newPriority);
        return true;
    }

    /**
     * Removes the AlphaCard from list of AlphaCards.
     * 
     * @param AlphaCard
     *            AlphaCard
     * @return true if present and successfully removed
     */
    public boolean removeAlphaCard(final AlphaCard card) {
        return alphaCards.remove(card);
    }

    @Override
    public boolean equals(final Object other) {
        if (!(other instanceof AlphaCasePSA)) {
            return false;
        }
        final AlphaCasePSA castOther = (AlphaCasePSA) other;
        return new EqualsBuilder().append(alphaCards.toArray(new AlphaCard[] {}),
                castOther.alphaCards.toArray(new AlphaCard[] {})).isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder(-1638945841, 1946728535).append(alphaCards).toHashCode();
    }

    @Override
    public String toString() {
        return new ToStringBuilder(this).append("alphacards", alphaCards).toString();
    }

}
