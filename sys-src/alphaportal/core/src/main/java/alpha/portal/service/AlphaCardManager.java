/**
 * 
 */
package alpha.portal.service;

import java.util.List;

import org.appfuse.service.GenericManager;
import org.hibernate.criterion.Criterion;

import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCardIdentifier;
import alpha.portal.model.AlphaCase;
import alpha.portal.service.impl.AlphaCardManagerImpl;

/**
 * Interface AlphaCardManager, which extends from the GenericManager with following objects: α-Card and
 * α-Card-Identifier.
 */
public interface AlphaCardManager extends GenericManager<AlphaCard, AlphaCardIdentifier> {

    /**
     * Creates a new AlphaCard object with all available adornments set to their default values.
     * 
     * @param caseId
     *            the case id
     * @return the alpha card
     */
    public AlphaCard createAlphaCard(String caseId);

    /**
     * Loads a specific AlphaCard version.
     * 
     * @param id
     *            AlphaCardIdentifier with all attributes set
     */
    public AlphaCard getVersion(final AlphaCardIdentifier id);

    /**
     * Loads all versions of a specific AlphaCard.
     */
    public List<AlphaCard> getAllVersions(final String caseId);

    /**
     * Lists alphaCards filtered by Adornment-Properties. All given {@link Criterion}s will be joined by {@code and}.
     * 
     * @param criterions
     *            a array of {@link Criterion}.
     * @return The intersection of all criterion as a List of {@link AlphaCard}s or null if an error occurs
     * @see AlphaCardManagerImpl for predifined {@link Criterion}
     */
    public List<AlphaCard> listAlphaCardsByCriterion(String caseId, Criterion... criterions);

    /**
     * Get the last 20 changes of AlphaCards for the Dashboard.
     * 
     * @param caseList
     *            A list with the cases where I take part.
     * 
     * @return A list with AlphaCards
     */
    public List<AlphaCard> listDashBoardCards(List<AlphaCase> caseList);

}
