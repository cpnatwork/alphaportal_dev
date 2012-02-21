package alpha.portal.service.impl;

import java.util.List;

import org.appfuse.service.impl.GenericManagerImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import alpha.portal.dao.UserExtensionDao;
import alpha.portal.model.ContributorRole;
import alpha.portal.model.UserExtension;
import alpha.portal.service.UserExtensionManager;

/**
 * implements the interface UserExtensionManager.
 * 
 * @see UserExtensionManager User Extension Manager
 */
@Service("userExtensionManager")
public class UserExtensionManagerImpl extends GenericManagerImpl<UserExtension, Long> implements UserExtensionManager {

    private UserExtensionDao userExtensionDao;

    @Autowired
    public UserExtensionManagerImpl(final UserExtensionDao ueDao) {
        super(ueDao);
        this.userExtensionDao = ueDao;
    }

    public List<UserExtension> getUserExtensionsByContributorRole(ContributorRole role) {
        return userExtensionDao.getUserExtensionsByContributorRole(role);
    }

}
