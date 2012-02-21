package alpha.portal.dao.hibernate;

import java.util.List;

import org.appfuse.dao.hibernate.GenericDaoHibernate;
import org.appfuse.model.User;
import org.springframework.stereotype.Repository;

import alpha.portal.dao.AlphaCaseDao;
import alpha.portal.model.AlphaCase;

/**
 * The AlphaCaseDaoHibernate implements the interface AlphaCaseDao.
 * 
 * @see AlphaCaseDao AlphaCaseDAO
 */
@Repository("caseDao")
public class AlphaCaseDaoHibernate extends GenericDaoHibernate<AlphaCase, String> implements AlphaCaseDao {

    /**
     * Instantiates a new case dao hibernate.
     */
    public AlphaCaseDaoHibernate() {
        super(AlphaCase.class);
    }

    /**
     * @see alpha.portal.dao.AlphaCaseDao#findByName(java.lang.String)
     */
    @SuppressWarnings("unchecked")
    public List<AlphaCase> findByName(final String name) {
        return getHibernateTemplate().find("from alphacase where name=?", name);
    }

    /**
     * @see alpha.portal.dao.AlphaCaseDao#findByParticipant(java.lang.String)
     */
    @SuppressWarnings("unchecked")
    public List<AlphaCase> findByParticipant(final User user) {
        return getHibernateTemplate()
                .find("from alphacase where ? = some elements(participantsCRA.participants)", user);
    }
}
