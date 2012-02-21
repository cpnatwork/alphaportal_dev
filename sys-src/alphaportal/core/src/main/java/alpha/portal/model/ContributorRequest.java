package alpha.portal.model;

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.OneToOne;

import org.appfuse.model.User;

/**
 * 
 * Represents a request to become a contributor to an AlphaCard.
 * 
 */
@Entity(name = "contributorrequest")
public class ContributorRequest {

    @Id
    @GeneratedValue
    private Long contributorRequestID;

    @OneToOne(optional = false, fetch = FetchType.EAGER)
    User requestingUser;

    @OneToOne(optional = false, fetch = FetchType.EAGER)
    User acceptingUser;

    @OneToOne(optional = false, fetch = FetchType.EAGER)
    AlphaCard alphaCard;

    @Column
    Date date;

    public ContributorRequest() {
        date = new Date();
    }

    public ContributorRequest(final User reqUser, final User acceptUser, final AlphaCard aCard) {
        this.requestingUser = reqUser;
        this.acceptingUser = acceptUser;
        this.alphaCard = aCard;
        date = new Date();
    }

    public ContributorRequest(final User acceptUser, final AlphaCard aCard) {
        this.acceptingUser = acceptUser;
        this.alphaCard = aCard;
        date = new Date();
    }

    public Long getContributorRequestID() {
        return contributorRequestID;
    }

    public void setContributorRequestID(final Long contributorRequestID) {
        this.contributorRequestID = contributorRequestID;
    }

    public User getRequestingUser() {
        return requestingUser;
    }

    public void setRequestingUser(final User requestingUser) {
        this.requestingUser = requestingUser;
    }

    public User getAcceptingUser() {
        return acceptingUser;
    }

    public void setAcceptingUser(final User acceptingUser) {
        this.acceptingUser = acceptingUser;
    }

    public AlphaCard getAlphaCard() {
        return alphaCard;
    }

    public void setAlphaCard(final AlphaCard alphaCard) {
        this.alphaCard = alphaCard;
    }

}
