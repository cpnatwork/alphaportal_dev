package alpha.portal.service;

import java.util.List;

import org.appfuse.model.User;
import org.appfuse.service.GenericManager;

import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCase;

/**
 * Interface CaseManager, which extends from the GenericManager with following
 * objects: α-Case and String.
 */
public interface CaseManager extends GenericManager<AlphaCase, String> {
    
    
     /**
     * Find by name.
     * 
     * @param name
     *            the name
     * @return the list
     */
    public List<AlphaCase> findByName(String name);

    /**
     * Find by participant.
     * 
     * @param user
     *            the user
     * @return the list
     */
    public List<AlphaCase> findByParticipant(User user);

    /**
     * Builds a new AlphaCard list the alphaCase and saves it.
     * 
     * @param alphaCase
     * @param cardIds
     *            list of cardId
     */
    public void updateCardOrder(AlphaCase alphaCase, List<String> cardIds);

    /**
     * Removes the AlphaCard from the case and saves the case (to persist
     * card-order).
     */
    public void removeAlphaCard(AlphaCard card);

}
