package alpha.portal.dao;

import org.appfuse.dao.GenericDao;
import org.appfuse.model.User;

import alpha.portal.model.AlphaCase;

import java.util.List;

/**
 * The AlphaCaseDAO is the Data Access Object of the α-Case.
 * It extends from the GenericDao with the following objects: α-Case and String.
 */
public interface AlphaCaseDao extends GenericDao<AlphaCase, String> {
    
    /**
     * Find by name.
     *
     * @param name name
     * @return list of α-Cases.
     */
    public List<AlphaCase> findByName(String name);
    
    /**
     * Find by participant.
     *
     * @param user user
     * @return list of α-Cases
     */
    public List<AlphaCase> findByParticipant(User user);
}