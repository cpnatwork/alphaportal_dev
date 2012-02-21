package alpha.portal.dao.hibernate;

import java.util.List;

import org.appfuse.dao.hibernate.GenericDaoHibernate;
import org.springframework.stereotype.Repository;

import alpha.portal.dao.UserExtensionDao;
import alpha.portal.model.ContributorRole;
import alpha.portal.model.UserExtension;

/**
 * The UserExtensionDaoHibernate implements the interface UserExtensionDao.
 * 
 * @see UserExtensionDao UserExtensionDAO
 */
@Repository("userExtensionDao")
public class UserExtensionDaoHibernate extends GenericDaoHibernate<UserExtension, Long> implements UserExtensionDao {

    /**
     * Instantiates a new user extension dao hibernate.
     */
    public UserExtensionDaoHibernate() {
        super(UserExtension.class);
    }

    /**
     * @see alpha.portal.dao.UserExtensionDao#getUserExtensionsByContributorRole(alpha.portal.model.ContributorRole)
     */
    @SuppressWarnings("unchecked")
    public List<UserExtension> getUserExtensionsByContributorRole(final ContributorRole role) {
        return getHibernateTemplate().find("from userextension where ? = some elements(roles)", role);
    }
}
