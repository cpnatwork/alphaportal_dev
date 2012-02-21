package alpha.portal.dao;

import java.util.List;

import org.appfuse.dao.GenericDao;
import org.hibernate.criterion.Criterion;

import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCardIdentifier;

/**
 * The AlphaCardDAO is the Data Access Object of the α-Card. It extends from the GenericDao with the following objects:
 * α-Card and α-Card-Identifier.
 */
public interface AlphaCardDao extends GenericDao<AlphaCard, AlphaCardIdentifier> {
    /**
     * Loads all versions of a specific AlphaCard.
     */
    List<AlphaCard> getAllVersions(String caseId);

    /**
     * Loads a specific version of an AlphaCard.
     * 
     * @param id
     *            identifier with all properties != null.
     */
    AlphaCard getVersion(AlphaCardIdentifier id);

    /**
     * Loads all AlphaCards matching a given Criterion.
     * 
     * @param criterions
     *            can be fetched statically from AlphaCardManagerImpl
     * @return
     */
    List<AlphaCard> listAlphaCardsByCriterion(String caseId, Criterion... criterions);

    /**
     * List the last 20 changes of AlphaCards in AlphaCases where I take part.
     * 
     * @param caseIDs
     *            A String array with the caseIDs of the cases where I take part
     * 
     * @return A list with AlphaCards
     */
    List<AlphaCard> listDashBoardAlphaCards(String[] caseIDs);

}
