package alpha.portal.model;

import javax.persistence.Entity;
import javax.persistence.Id;

/**
 * The user session is the object, where we save across sessions attributes.
 * It is used for user specific settings over sessions. 
 */
@Entity(name = "usersession")
public class UserSession {

    /**
     * the Id of the user.
     */
    @Id
    private Long userId;

    /**
     * the case, which the user saw last.
     */
    private String lastViewedCaseId;

    /**
     * gets the userId.
     * 
     * @return userId
     */
    public Long getUserId() {
	return userId;
    }

    /**
     * sets the userId.
     * 
     * @param userId
     */
    public void setUserId(final Long userId) {
	this.userId = userId;
    }

    /**
     * gets the last case, the user viewed.
     * 
     * @return the caseId of the last viewed case
     */
    public String getLastViewedCaseId() {
	return lastViewedCaseId;
    }

    /**
     * sets the Id of the case, the user last viewed.
     * 
     * @param lastViewedCaseId
     *            the caseId
     */
    public void setLastViewedCaseId(final String lastViewedCaseId) {
	this.lastViewedCaseId = lastViewedCaseId;
    }

}
